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
    // pub instructions_to_remove: Vec<usize>,
    // pub constants_to_remove: Vec<usize>,
}

pub struct NumericConstantPropagation {
    constants_to_add: Vec<Primitive>,
}

impl NumericConstantPropagation {
    pub fn new() -> Self {
        Self {
            constants_to_add: Vec::new(),
        }
    }

    fn try_arithmetic_op(&mut self, op: &str, constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
        let ins_a = find_source(&instructions, a)?;
        let ins_b = find_source(&instructions, b)?;

        let int_a = maybe_int(constants, ins_a)?;
        let int_b = maybe_int(constants, ins_b)?;

        let value = match op {
            "+" => int_a + int_b,
            "-" => int_a - int_b,
            "*" => int_a * int_b,
            "/" => int_a / int_b,
            _ => panic!("op was: {}", op),
        };

        self.constants_to_add.push(kint!(value));

        Some(
            Change {
                instructions_to_replace: vec![(instructions.len(), load!(dest, (constants.len() + self.constants_to_add.len() - 1) as u16))],
            }
        )
    }

    fn try_add(&mut self, constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
        self.try_arithmetic_op("+", constants, instructions, dest, a, b)
    }

    fn try_sub(&mut self, constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
        self.try_arithmetic_op("-", constants, instructions, dest, a, b)
    }

    fn try_mul(&mut self, constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
        self.try_arithmetic_op("*", constants, instructions, dest, a, b)
    }

    fn try_div(&mut self, constants: &[Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
        self.try_arithmetic_op("/", constants, instructions, dest, a, b)
    }

    fn try_neg(&mut self, constants: &[Primitive], instructions: &[Instruction], dest: Register, src: Register) -> Option<Change> {
        let ins = find_source(&instructions, src)?;
        let int = maybe_int(constants, ins)?;

        self.constants_to_add.push(kint!(-int));

        Some(
            Change {
                instructions_to_replace: vec![(instructions.len(), load!(dest, (constants.len() + self.constants_to_add.len() - 1) as u16))],
            }
        )
    }
}

impl Optimizer for NumericConstantPropagation {
    fn optimize(&mut self, current_index: usize, instructions: &mut [Instruction], constants: &mut Vec<Primitive>) {
        let change = match instructions[current_index] {
            Add { dest, a, b } => self.try_add(constants, &instructions[..current_index], dest, a, b),
            Sub { dest, a, b } => self.try_sub(constants, &instructions[..current_index], dest, a, b),
            Mul { dest, a, b } => self.try_mul(constants, &instructions[..current_index], dest, a, b),
            Div { dest, a, b } => self.try_div(constants, &instructions[..current_index], dest, a, b),
            Neg { dest, src } => self.try_neg(constants, &instructions[..current_index], dest, src),
            _ => return,
        };

        if change.is_none() { return; };
        let mut change = change.unwrap();

        while let Some((pos, ins)) = change.instructions_to_replace.pop() {
            instructions[pos] = ins;
        };

        self.constants_to_add.reverse();

        while let Some(c) = self.constants_to_add.pop() {
            constants.push(c);
        };
    }

    fn finalize(&mut self, _instructions: &mut Vec<Instruction>, constants: &mut Vec<Primitive>) {
    }
}

fn find_source(instructions: &[Instruction], register: Register) -> Option<Instruction> {
    for ins in instructions.iter().rev() {
        match ins {
            load @ Load { dest, .. } =>  if register == *dest { return Some(*load) },
            _ => {},
        };
    };

    None
}

fn maybe_int(constants: &[Primitive], ins: Instruction) -> Option<i128> {
    match ins {
        Load { constant, .. } => {
            match constants.get(constant as usize) {
                Some(Primitive::Int(Integer { value })) => Some(*value),
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
                expected_constants: vec![kint!(1), kint!(3), kint!(4)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    load!(2, 2),
                    set_global!(0, 2),
                ],
            },
            TestCase {
                input: r#"
                    let foo = 1 - 3;
                "#.to_string(),
                expected_constants: vec![kint!(1), kint!(3), kint!(-2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    load!(2, 2),
                    set_global!(0, 2),
                ],
            },
            TestCase {
                input: r#"
                    let foo = 2 * 3;
                "#.to_string(),
                expected_constants: vec![kint!(2), kint!(3), kint!(6)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    load!(2, 2),
                    set_global!(0, 2),
                ],
            },
            TestCase {
                input: r#"
                    let foo = 4 / 2;
                "#.to_string(),
                expected_constants: vec![kint!(4), kint!(2), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    load!(2, 2),
                    set_global!(0, 2),
                ],
            },
            TestCase {
                input: r#"
                    let foo = 1 + 3 * 4;
                "#.to_string(),
                expected_constants: vec![kint!(1), kint!(3), kint!(4), kint!(12), kint!(13)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    load!(2, 2),
                    load!(3, 3),
                    load!(4, 4),
                    set_global!(0, 4),
                ],
            },
            TestCase {
                input: r#"
                    let foo = -1 + 3 * -4;
                "#.to_string(),
                expected_constants: vec![kint!(1), kint!(3), kint!(4), kint!(-1), kint!(-4), kint!(-12), kint!(-13)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 3),
                    load!(2, 1),
                    load!(3, 2),
                    load!(4, 4),
                    load!(5, 5),
                    load!(6, 6),
                    set_global!(0, 6),
                ],
            },
            TestCase {
                input: r#"
                    let pos = 1 + 3 * 4;
                    let neg = -1 + 3 * -4;
                "#.to_string(),
                expected_constants: vec![kint!(1), kint!(3), kint!(4), kint!(1), kint!(3), kint!(4), kint!(12), kint!(13), kint!(-1), kint!(-4), kint!(-12), kint!(-13)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    load!(2, 2),
                    load!(3, 6),
                    load!(4, 7),
                    set_global!(0, 4),
                    load!(5, 3),
                    load!(6, 8),
                    load!(7, 4),
                    load!(8, 5),
                    load!(9, 9),
                    load!(10, 10),
                    load!(11, 11),
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
                expected_constants: vec![kint!(1), kint!(-1)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    set_global!(0, 1),
                ],
            },
            TestCase {
                input: r#"
                    let foo = -123;
                    let bar = -42;
                "#.to_string(),
                expected_constants: vec![kint!(123), kint!(42), kint!(-123), kint!(-42)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 2),
                    set_global!(0, 1),
                    load!(2, 1),
                    load!(3, 3),
                    set_global!(1, 3),
                ],
            },
        ];

        run_optimizer_tests(tests)
    }

    #[test]
    fn test_multiple_bindings() -> Result<()> {
        let tests = vec![
            TestCase {
                input: r#"
                    let a = 0;
                    let b = 1;
                    a + b;
                    let c = 2;
                    let d = 3;
                    b + c;
                    c + d;
                "#.to_string(),
                expected_constants: vec![kint!(0), kint!(1), kint!(2), kint!(3), kint!(1), kint!(3), kint!(5)],
                expected_instructions: vec![
                    load!(0, 0),
                    set_global!(0, 0),
                    load!(1, 1),
                    set_global!(1, 1),
                    load!(2, 4),
                    load!(3, 2),
                    set_global!(2, 3),
                    load!(4, 3),
                    set_global!(3, 4),
                    load!(5, 5),
                    load!(6, 6),
                ],
            },
        ];

        run_optimizer_tests(tests)
    }
}
