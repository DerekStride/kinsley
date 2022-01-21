use crate::{
    error::*,
    compiler::{
        Compiler,
        optimizer::Optimizer,
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
        // let scope = compiler.leave_scope();
        // let mut instructions = scope.instructions;
        // let mut constants = compiler.constants;

        Ok(())
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
                    let one = 1 + 3;
                "#.to_string(),
                expected_constants: vec![kint!(4)],
                expected_instructions: vec![
                    load!(0, 0),
                    set_global!(0, 0),
                ],
            },
        ];

        run_optimizer_tests(tests)
    }
}
