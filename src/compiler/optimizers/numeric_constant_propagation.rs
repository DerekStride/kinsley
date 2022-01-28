use crate::{
    object::{Integer, Primitive},
    compiler::{
        optimizer::{self, Optimizer},
        code::{
            Instruction,
            Instruction::*,
            Register
        },
    },
};

#[derive(Debug)]
struct Change {
    pub instructions_to_replace: Vec<(usize, Instruction)>,
    pub instructions_to_remove: Vec<usize>,

    pub constants_to_replace: Vec<(usize, Primitive)>,
    pub constants_to_remove: Vec<usize>,
}

pub struct NumericConstantPropagation {
    instructions_to_remove: Vec<usize>,
    constants_to_remove: Vec<usize>,
}

impl NumericConstantPropagation {
    pub fn new() -> Self {
        Self {
            instructions_to_remove: Vec::new(),
            constants_to_remove: Vec::new(),
        }
    }
}

impl Optimizer for NumericConstantPropagation {
    fn optimize(&mut self, current_index: usize, instructions: &mut [Instruction], constants: &mut [Primitive]) {
        let change = match instructions[current_index] {
            Add { dest, a, b } => try_add(constants, &instructions[..current_index], dest, a, b),
            Sub { dest, a, b } => try_sub(constants, &instructions[..current_index], dest, a, b),
            Mul { dest, a, b } => try_mul(constants, &instructions[..current_index], dest, a, b),
            Div { dest, a, b } => try_div(constants, &instructions[..current_index], dest, a, b),
            Neg { dest, src } => try_neg(constants, &instructions[..current_index], dest, src),
            _ => return,
        };

        if change.is_none() { return; };
        let mut change = change.unwrap();

        while let Some((pos, ins)) = change.instructions_to_replace.pop() {
            instructions[pos] = ins;
        };
        while let Some((pos, c)) = change.constants_to_replace.pop() {
            constants[pos] = c;
        };

        self.instructions_to_remove.append(&mut change.instructions_to_remove);
        self.constants_to_remove.append(&mut change.constants_to_remove);
    }

    fn finalize(&mut self, instructions: &mut Vec<Instruction>, constants: &mut Vec<Primitive>) {
        self.instructions_to_remove.sort();//ins_to_remove.sort();
        self.constants_to_remove.sort();//con_to_remove.sort();

        for idx in self.instructions_to_remove.iter().rev() {
            instructions.remove(*idx);
        };

        optimizer::remap_constants(instructions, constants, &self.constants_to_remove);
    }
}

fn try_arithmetic_op(op: &str, constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    let (ins_a, idx_a) = find_source(&instructions, a)?;
    let (ins_b, idx_b) = find_source(&instructions, b)?;

    let (int_a, const_a) = maybe_int(constants, ins_a)?;
    let (int_b, const_b) = maybe_int(constants, ins_b)?;

    let value = match op {
        "+" => int_a + int_b,
        "-" => int_a - int_b,
        "*" => int_a * int_b,
        "/" => int_a / int_b,
        _ => panic!("op was: {}", op),
    };

    Some(
        Change {
            instructions_to_replace: vec![(instructions.len(), load!(dest, const_a))],
            instructions_to_remove: vec![idx_a, idx_b],
            constants_to_replace: vec![(const_a as usize, kint!(value))], // No need to remove const_a because we're using the location for the new value.
            constants_to_remove: vec![const_b as usize], // No need to remove const_a because we're using the location for the new value.
        }
    )
}

fn try_add(constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    try_arithmetic_op("+", constants, instructions, dest, a, b)
}

fn try_sub(constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    try_arithmetic_op("-", constants, instructions, dest, a, b)
}

fn try_mul(constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    try_arithmetic_op("*", constants, instructions, dest, a, b)
}

fn try_div(constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    try_arithmetic_op("/", constants, instructions, dest, a, b)
}

fn try_neg(constants: &[Primitive], instructions: &[Instruction], dest: Register, src: Register) -> Option<Change> {
    let (ins, idx) = find_source(&instructions, src)?;
    let (int, const_a) = maybe_int(constants, ins)?;

    Some(
        Change {
            instructions_to_replace: vec![(instructions.len(), load!(dest, const_a))],
            instructions_to_remove: vec![idx],
            constants_to_replace: vec![(const_a as usize, kint!(-int))], // No need to remove const_a because we're using the location for the new value.
            constants_to_remove: Vec::new(), // No need to remove const_a because we're using the location for the new value.
        }
    )
}

fn find_source(instructions: &[Instruction], register: Register) -> Option<(Instruction, usize)> {
    for (index, ins) in instructions.iter().enumerate().rev() {
        match ins {
            load @ Load { dest, .. } =>  if register == *dest { return Some((*load, index)) },
            _ => {},
        };
    };

    None
}

fn maybe_int(constants: &[Primitive], ins: Instruction) -> Option<(i128, u16)> {
    match ins {
        Load { constant, .. } => {
            match constants.get(constant as usize) {
                Some(Primitive::Int(Integer { value })) => Some((*value, constant)),
                _ => None,
            }
        },
        _ => None,
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        error::*,
        utils::*,
        ast::Ast,
        compiler::Compiler,
    };

    struct TestCase {
        input: String,
        expected_instructions: Vec<Instruction>,
        expected_constants: Vec<Primitive>,
    }

    fn run_optimizer_tests(tests: Vec<TestCase>) -> Result<()> {
        for tt in tests {
            let program = parse(tt.input)?;
            let mut compiler = Compiler::new();
            compiler.compile(Ast::Prog(program))?;

            compiler.optimize(&mut NumericConstantPropagation::new())?;

            let bytecode = compiler.bytecode();

            test_instructions(&tt.expected_instructions, &bytecode.instructions);
            test_constants(&tt.expected_constants, &bytecode.constants);
        };

        Ok(())
    }

    #[test]
    fn test_precomputing_contants() -> Result<()> {
        let tests = vec![
            TestCase {
                input: r#"
                    let foo = 1 + 3;
                "#.to_string(),
                expected_constants: vec![kint!(4)],
                expected_instructions: vec![
                    load!(2, 0),
                    set_global!(0, 2),
                ],
            },
            TestCase {
                input: r#"
                    let foo = 1 - 3;
                "#.to_string(),
                expected_constants: vec![kint!(-2)],
                expected_instructions: vec![
                    load!(2, 0),
                    set_global!(0, 2),
                ],
            },
            TestCase {
                input: r#"
                    let foo = 2 * 3;
                "#.to_string(),
                expected_constants: vec![kint!(6)],
                expected_instructions: vec![
                    load!(2, 0),
                    set_global!(0, 2),
                ],
            },
            TestCase {
                input: r#"
                    let foo = 4 / 2;
                "#.to_string(),
                expected_constants: vec![kint!(2)],
                expected_instructions: vec![
                    load!(2, 0),
                    set_global!(0, 2),
                ],
            },
            TestCase {
                input: r#"
                    let foo = 1 + 3 * 4;
                "#.to_string(),
                expected_constants: vec![kint!(13)],
                expected_instructions: vec![
                    load!(4, 0),
                    set_global!(0, 4),
                ],
            },
            TestCase {
                input: r#"
                    let foo = -1 + 3 * -4;
                "#.to_string(),
                expected_constants: vec![kint!(-13)],
                expected_instructions: vec![
                    load!(6, 0),
                    set_global!(0, 6),
                ],
            },
            TestCase {
                input: r#"
                    let pos = 1 + 3 * 4;
                    let neg = -1 + 3 * -4;
                "#.to_string(),
                expected_constants: vec![kint!(13), kint!(-13)],
                expected_instructions: vec![
                    load!(4, 0),
                    set_global!(0, 4),
                    load!(11, 1),
                    set_global!(1, 11),
                ],
            },
        ];

        run_optimizer_tests(tests)
    }

    #[test]
    fn test_precomputing_negative_numbers() -> Result<()> {
        let tests = vec![
            TestCase {
                input: r#"
                    let foo = -1;
                "#.to_string(),
                expected_constants: vec![kint!(-1)],
                expected_instructions: vec![
                    load!(1, 0),
                    set_global!(0, 1),
                ],
            },
            TestCase {
                input: r#"
                    let foo = -123;
                    let bar = -42;
                "#.to_string(),
                expected_constants: vec![kint!(-123), kint!(-42)],
                expected_instructions: vec![
                    load!(1, 0),
                    set_global!(0, 1),
                    load!(3, 1),
                    set_global!(1, 3),
                ],
            },
        ];

        run_optimizer_tests(tests)
    }
}
