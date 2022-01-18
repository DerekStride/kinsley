use std::fmt;

use crate::{
    error::*,
    ast::*,
    object::*,
    compiler::{
        code::*,
        code::Opcode::*,
        compilation_scope::CompilationScope,
    },
};

pub mod code;
mod compilation_scope;
mod emitted_instruction;

pub struct Bytecode {
    instructions: Vec<Instruction>,
    constants: Vec<Primitive>,
}

pub struct Compiler {
    constants: Vec<Primitive>,
    scopes: Vec<CompilationScope>,

    code: Code,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            scopes: vec![CompilationScope::new()],
            code: Code::new(),
        }
    }

    pub fn bytecode(&self) -> Bytecode {
        let instructions = self
            .current_scope()
            .instructions
            .clone();

        Bytecode {
            instructions,
            constants: self.constants.clone(),
        }
    }

    pub fn compile(&mut self, node: KNode) -> Result<()> {
        match node {
            KNode::Prog(prog) => {
                for expr in prog.exprs { self.compile(expr)?; };
            },
            KNode::Int(int) => {
                let literal = kint!(int.value);
                self.constants.push(literal);
                let reg = self.next_register();
                self.emit(Load, Opcode::load(reg, (self.constants.len() - 1) as u16));
            },
            KNode::In(infix) => {
                self.compile(*infix.left)?;
                self.compile(*infix.right)?;
                let b = 255;
                let c = 255;
                let reg = self.next_register();

                match infix.operator.as_str() {
                    "+" => self.emit(Add, [reg, b, c]),
                    "-" => self.emit(Sub, [reg, b, c]),
                    "*" => self.emit(Mul, [reg, b, c]),
                    "/" => self.emit(Div, [reg, b, c]),
                    _ => return Err(Error::new(format!("unknown operator: {}", infix.operator))),
                };
            },
            x => return Err(Error::new(format!("Compilation not implemented for node: {:?}", x))),
        };

        Ok(())
    }

    fn emit(&mut self, op: Opcode, operands: Operands) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.emit(&self.code, op, operands);
        };
    }

    fn next_register(&mut self) -> Register {
        if let Some(scope) = self.scopes.last_mut() {
            scope.next_register()
        } else {
            unreachable!();
        }
    }

    fn enter_scope(&mut self, scope: CompilationScope) {
        // self.symbols = SymbolTable::enclose(self.symbols.clone());
        self.scopes.push(scope);
    }

    fn leave_scope(&mut self) -> CompilationScope {
        // self.symbols = self.symbols.outer().unwrap();
        self.scopes.pop().unwrap()
    }

    fn current_scope(&self) -> &CompilationScope {
        self.scopes.last().unwrap()
    }

    fn current_instructions(&self) -> &[Instruction] {
        &self.current_scope().instructions
    }
}

impl fmt::Display for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let scope = self.current_scope();
        write!(
            f,
            "Compiler {{\n\tinstructions:\n\t\t{}\n\tconstants:\n\t\t{}\n\tprev_instruction:\n\t\t{}\n\tlast_instruction:\n\t\t{}\n}}",
            self.code.format(self.current_instructions())
                .lines()
                .map(|l| l.to_string())
                .collect::<Vec<String>>()
                .join("\n\t\t"),
            self.constants
                .iter()
                .enumerate()
                .map(|(i, o)| format!("{}: {}", i, o))
                .collect::<Vec<String>>()
                .join("\n\t\t"),
            if let Some(x) = &scope.prev_emitted_instruction { format!("{}", x) } else { "None".to_string() },
            if let Some(x) = &scope.last_emitted_instruction { format!("{}", x) } else { "None".to_string() },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_utils::parse;

    struct TestCase {
        input: String,
        expected_instructions: Vec<Instruction>,
        expected_constants: Vec<Primitive>,
    }

    fn run_compiler_tests(tests: Vec<TestCase>) -> Result<()> {
        for tt in tests {
            let program = parse(tt.input)?;
            let mut compiler = Compiler::new();
            compiler.compile(KNode::Prog(program))?;

            let bytecode = compiler.bytecode();

            test_instructions(tt.expected_instructions, bytecode.instructions);
            test_constants(tt.expected_constants, bytecode.constants);
        };

        Ok(())
    }

    fn test_instructions(expected: Vec<Instruction>, actual: Vec<Instruction>) {
        let code = Code::new();
        assert_eq!(expected, actual, "\n\nInstructions:\nwant:\n{}\ngot:\n{}\n", code.format(&expected), code.format(&actual));
    }

    fn test_constants(expected: Vec<Primitive>, actual: Vec<Primitive>) {
        assert_eq!(
            expected,
            actual,
            "\n\nConstants:\nwant:\n{}\ngot:\n{}\n",
            expected
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join("\n"),
            actual
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join("\n"),
        );
    }

    #[test]
    fn test_compiler() {
        let mut c = Compiler::new();
        assert!(c.compile(KNode::Prog(Program { exprs: Vec::new() })).is_ok());
    }

    #[test]
    fn test_int_expr() -> Result<()> {
        let code = Code::new();
        let tests = vec![
            TestCase {
                input: "5; 2;".to_string(),
                expected_constants: vec![kint!(5), kint!(2)],
                expected_instructions: vec![
                    code.make(Load, &Opcode::load(0, 0)),
                    code.make(Load, &Opcode::load(1, 1)),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_integer_arithmetic() -> Result<()> {
        let code = Code::new();
        let tests = vec![
            TestCase {
                input: "1 + 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    code.make(Load, &Opcode::load(0, 0)),
                    code.make(Load, &Opcode::load(1, 1)),
                    code.make(Add, &[2, 0, 1]),
                ],
            },
            TestCase {
                input: "1 - 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    code.make(Load, &Opcode::load(0, 0)),
                    code.make(Load, &Opcode::load(1, 1)),
                    code.make(Sub, &[2, 0, 1]),
                ],
            },
            TestCase {
                input: "1 * 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    code.make(Load, &Opcode::load(0, 0)),
                    code.make(Load, &Opcode::load(1, 1)),
                    code.make(Mul, &[2, 0, 1]),
                ],
            },
            TestCase {
                input: "1 / 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    code.make(Load, &Opcode::load(0, 0)),
                    code.make(Load, &Opcode::load(1, 1)),
                    code.make(Div, &[2, 0, 1]),
                ],
            },
            TestCase {
                input: "-1 - -2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    code.make(Load, &Opcode::load(0, 0)),
                    code.make(Load, &Opcode::load(1, 1)),
                    code.make(Sub, &[2, 0, 1]),
                    // code.make(&OP_CONSTANT, &vec![0]),
                    // code.make(&OP_MINUS, &vec![]),
                    // code.make(&OP_CONSTANT, &vec![1]),
                    // code.make(&OP_MINUS, &vec![]),
                    // code.make(&OP_SUB, &vec![]),
                    // code.make(&OP_POP, &vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }
}
