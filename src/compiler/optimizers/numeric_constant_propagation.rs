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

#[derive(Clone, Debug)]
pub struct NumericChange {
    pub instructions_to_replace: Vec<(usize, Instruction)>,
    pub instructions_to_remove: Vec<usize>,

    pub constants_to_replace: Vec<(usize, Primitive)>,
    pub constants_to_remove: Vec<usize>,
}

pub struct NumericConstantPropagation {}

impl NumericConstantPropagation {
    pub fn new() -> Self {
        Self {}
    }
}

impl Optimizer for NumericConstantPropagation {
    type Change = NumericChange;

    fn optimize(&mut self, current_index: usize, instructions: &[Instruction], constants: &[Primitive]) -> Option<Self::Change> {
        match instructions[current_index] {
            Add { dest, a, b } => try_add(constants, &instructions[..current_index], dest, a, b),
            Sub { dest, a, b } => try_sub(constants, &instructions[..current_index], dest, a, b),
            Mul { dest, a, b } => try_mul(constants, &instructions[..current_index], dest, a, b),
            Div { dest, a, b } => try_div(constants, &instructions[..current_index], dest, a, b),
            Neg { dest, src } => try_neg(constants, &instructions[..current_index], dest, src),
            _ => None,
        }
    }

    fn apply_change(&mut self, change: &Self::Change, instructions: &mut [Instruction], constants: &mut [Primitive]) {
        for (pos, ins) in change.instructions_to_replace.iter().rev() {
            instructions[*pos] = *ins;
        };
        for (pos, c) in change.constants_to_replace.iter().rev() {
            constants[*pos] = c.clone();
        };
    }

    fn finalize_changes(&mut self, changes: &mut [Self::Change], instructions: &mut Vec<Instruction>, constants: &mut Vec<Primitive>) {
        let mut ins_to_remove = Vec::new();
        let mut con_to_remove = Vec::new();

        for change in changes {
            ins_to_remove.append(&mut change.instructions_to_remove);
            con_to_remove.append(&mut change.constants_to_remove);
        };

        ins_to_remove.sort();
        con_to_remove.sort();
        for idx in ins_to_remove.iter().rev() {
            instructions.remove(*idx);
        };

        optimizer::remap_constants(instructions, constants, &con_to_remove);
    }
}

fn try_arithmetic_op(op: &str, constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<NumericChange> {
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

    // reuse the location of one of the existing constants.
    let new_constant = kint!(value);
    // constants[const_a as usize] = new_constant.clone();

    Some(
        NumericChange {
            instructions_to_replace: vec![(instructions.len(), load!(dest, const_a))],
            instructions_to_remove: vec![idx_a, idx_b],
            constants_to_replace: vec![(const_a as usize, new_constant)], // No need to remove const_a because we're using the location for the new value.
            constants_to_remove: vec![const_b as usize], // No need to remove const_a because we're using the location for the new value.
        }
    )
}

fn try_add(constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<NumericChange> {
    try_arithmetic_op("+", constants, instructions, dest, a, b)
}

fn try_sub(constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<NumericChange> {
    try_arithmetic_op("-", constants, instructions, dest, a, b)
}

fn try_mul(constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<NumericChange> {
    try_arithmetic_op("*", constants, instructions, dest, a, b)
}

fn try_div(constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<NumericChange> {
    try_arithmetic_op("/", constants, instructions, dest, a, b)
}

fn try_neg(constants: &[Primitive], instructions: &[Instruction], dest: Register, src: Register) -> Option<NumericChange> {
    let (ins, idx) = find_source(&instructions, src)?;
    let (int, const_a) = maybe_int(constants, ins)?;

    // reuse the location of one of the existing constants.
    let new_constant = kint!(-int);
    // constants[const_a as usize] = new_constant.clone();

    Some(
        NumericChange {
            instructions_to_replace: vec![(instructions.len(), load!(dest, const_a))],
            instructions_to_remove: vec![idx],
            constants_to_replace: vec![(const_a as usize, new_constant)], // No need to remove const_a because we're using the location for the new value.
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
