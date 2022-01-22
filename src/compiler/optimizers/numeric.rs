use crate::{
    object::{Integer, Primitive},
    compiler::{
        optimizer::*,
        code::{
            Instruction,
            Instruction::*,
            Register
        },
    },
};

pub struct Numeric {}

impl Numeric {
    pub fn new() -> Self {
        Self {}
    }
}

impl Optimizer for Numeric {
    fn optimize(&mut self, current_index: usize, instructions: &[Instruction], constants: &[Primitive]) -> Option<Change> {
        match instructions[current_index] {
            Add { dest, a, b } => try_add(constants, &instructions[..current_index], dest, a, b),
            Sub { dest, a, b } => try_sub(constants, &instructions[..current_index], dest, a, b),
            Mul { dest, a, b } => try_mul(constants, &instructions[..current_index], dest, a, b),
            Div { dest, a, b } => try_div(constants, &instructions[..current_index], dest, a, b),
            Neg { dest, src } => try_neg(constants, &instructions[..current_index], dest, src),
            _ => None,
        }
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

    // reuse the location of one of the existing constants.
    let new_constant = kint!(value);
    // constants[const_a as usize] = new_constant.clone();

    Some(
        Change {
            instructions_to_replace: vec![(instructions.len(), load!(dest, const_a))],
            instructions_to_remove: vec![idx_a, idx_b],
            constants_to_replace: vec![(const_a as usize, new_constant)], // No need to remove const_a because we're using the location for the new value.
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

    // reuse the location of one of the existing constants.
    let new_constant = kint!(-int);
    // constants[const_a as usize] = new_constant.clone();

    Some(
        Change {
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
        test_utils::parse,
        ast::Ast,
        object::Primitive,
        compiler::{
            Compiler,
            Bytecode,
            code::Instruction,
        },
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

            compiler.optimize(&mut Numeric::new())?;

            let bytecode = compiler.bytecode();

            test_instructions(tt.expected_instructions, bytecode.instructions);
            test_constants(tt.expected_constants, bytecode.constants);
        };

        Ok(())
    }

    fn test_instructions(expected: Vec<Instruction>, actual: Vec<Instruction>) {
        assert_eq!(
            expected,
            actual,
            "\n\nInstructions:\nwant:\n{}\ngot:\n{}\n",
            Bytecode::format_instructions(&expected),
            Bytecode::format_instructions(&actual),
        );
    }

    fn test_constants(expected: Vec<Primitive>, actual: Vec<Primitive>) {
        assert_eq!(
            expected,
            actual,
            "\n\nConstants:\nwant:\n{}\ngot:\n{}\n",
            Bytecode::format_constants(&expected),
            Bytecode::format_constants(&actual),
        );
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
