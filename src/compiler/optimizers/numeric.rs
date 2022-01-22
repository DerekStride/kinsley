use crate::{
    error::*,
    object::{Integer, Primitive},
    compiler::{
        Compiler,
        optimizer::Optimizer,
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
    fn optimize(&mut self, compiler: &mut Compiler) -> Result<()> {
        let scope = compiler.scopes.last_mut().unwrap();
        let instructions = &mut scope.instructions;
        let constants = &mut compiler.constants;

        let mut index = 0;
        let mut ins_to_remove = Vec::new();
        let mut con_to_remove = Vec::new();

        while index < instructions.len() {
            let pos = index;
            index += 1;

            match instructions[pos] {
                Add { dest, a, b } => {
                    let mut change = match try_add(constants, &instructions[..pos], dest, a, b) {
                        Some(x) => x,
                        None => panic!("try_add failed"),
                    };

                    ins_to_remove.append(&mut change.instruction_to_remove);
                    con_to_remove.append(&mut change.constant_to_remove);
                    instructions[change.pos] = change.instruction;
                },
                Sub { dest, a, b } => {
                    let mut change = match try_sub(constants, &instructions[..pos], dest, a, b) {
                        Some(x) => x,
                        None => panic!("try_sub failed"),
                    };

                    ins_to_remove.append(&mut change.instruction_to_remove);
                    con_to_remove.append(&mut change.constant_to_remove);
                    instructions[change.pos] = change.instruction;
                },
                Mul { dest, a, b } => {
                    let mut change = match try_mul(constants, &instructions[..pos], dest, a, b) {
                        Some(x) => x,
                        None => panic!("try_mul failed"),
                    };

                    ins_to_remove.append(&mut change.instruction_to_remove);
                    con_to_remove.append(&mut change.constant_to_remove);
                    instructions[change.pos] = change.instruction;
                },
                Div { dest, a, b } => {
                    let mut change = match try_div(constants, &instructions[..pos], dest, a, b) {
                        Some(x) => x,
                        None => panic!("try_div failed"),
                    };

                    ins_to_remove.append(&mut change.instruction_to_remove);
                    con_to_remove.append(&mut change.constant_to_remove);
                    instructions[change.pos] = change.instruction;
                },
                Neg { dest, src } => {
                    let mut change = match try_neg(constants, &instructions[..pos], dest, src) {
                        Some(x) => x,
                        None => panic!("try_div failed"),
                    };

                    instructions[pos] = change.instruction;
                    ins_to_remove.append(&mut change.instruction_to_remove);
                }
                _ => {},
            };
        };

        ins_to_remove.sort();
        con_to_remove.sort();
        for idx in ins_to_remove.iter().rev() {
            instructions.remove(*idx);
        };
        for idx in con_to_remove.iter().rev() {
            constants.remove(*idx);
        };

        Ok(())
    }
}

struct Change {
    pos: usize,
    instruction: Instruction,
    instruction_to_remove: Vec<usize>,
    constant_to_remove: Vec<usize>,
}

fn try_arithmetic_op(op: &str, constants: &mut [Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
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
    constants[const_a as usize] = new_constant;

    Some(
        Change {
            pos: instructions.len(),
            instruction: load!(dest, const_a),
            instruction_to_remove: vec![idx_a, idx_b],
            constant_to_remove: vec![const_b as usize], // No need to remove const_a because we're using the location for the new value.
        }
    )
}

fn try_add(constants: &mut [Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    try_arithmetic_op("+", constants, instructions, dest, a, b)
}

fn try_sub(constants: &mut [Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    try_arithmetic_op("-", constants, instructions, dest, a, b)
}

fn try_mul(constants: &mut [Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    try_arithmetic_op("*", constants, instructions, dest, a, b)
}

fn try_div(constants: &mut [Primitive], instructions: &[Instruction], dest: Register, a: Register, b: Register) -> Option<Change> {
    try_arithmetic_op("/", constants, instructions, dest, a, b)
}

fn try_neg(constants: &mut [Primitive], instructions: &[Instruction], dest: Register, src: Register) -> Option<Change> {
    let (ins, idx) = find_source(&instructions, src)?;
    let (int, const_a) = maybe_int(constants, ins)?;

    // reuse the location of one of the existing constants.
    let new_constant = kint!(-int);
    constants[const_a as usize] = new_constant;

    Some(
        Change {
            pos: instructions.len(),
            instruction: load!(dest, const_a),
            instruction_to_remove: vec![idx],
            constant_to_remove: Vec::new(), // No need to remove const_a because we're using the location for the new value.
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
        test_utils::parse,
        ast::Ast,
        object::Primitive,
        compiler::{
            code::Instruction,
            Bytecode,
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
