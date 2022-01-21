use std::fmt;

use crate::{
    error::*,
    ast::*,
    object::*,
    compiler::{
        code::*,
        optimizer::Optimizer,
        code::Instruction::*,
    },
};

#[macro_use]
mod code;
mod symbol_table;
mod bytecode;
mod compilation_scope;
mod emitted_instruction;
mod optimizer;
mod optimizers;

pub type Bytecode = bytecode::Bytecode;
pub type SymbolTable = symbol_table::SymbolTable;
pub type Scope = symbol_table::Scope;
pub type Symbol = symbol_table::Symbol;
pub type CompilationScope = compilation_scope::CompilationScope;

pub struct Compiler {
    constants: Vec<Primitive>,
    scopes: Vec<CompilationScope>,
    symbols: SymbolTable,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            scopes: vec![CompilationScope::new()],
            symbols: SymbolTable::new(),
        }
    }

    pub fn with_state(symbols: SymbolTable, constants: Vec<Primitive>) -> Self {
        Self {
            constants,
            symbols,
            scopes: vec![CompilationScope::new()],
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

    pub fn symbol_table(&self) -> SymbolTable {
        self.symbols.clone()
    }

    pub fn compile(&mut self, node: Ast) -> Result<()> {
        match node {
            Ast::Prog(prog) => {
                for expr in prog.exprs { self.compile(expr)?; };
            },
            Ast::Int(int) => {
                let literal = kint!(int.value);
                self.constants.push(literal);
                let dest = self.next_register();

                self.emit(load!(dest, (self.constants.len() - 1) as u16));
            },
            Ast::Bool(boolean) => {
                let dest = self.next_register();
                if boolean.value {
                    self.emit(load_true!(dest));
                } else {
                    self.emit(load_false!(dest));
                };
            },
            Ast::In(infix) => {
                self.compile(*infix.left)?;
                let a = self.last_dest_reg();

                self.compile(*infix.right)?;
                let b = self.last_dest_reg();

                let dest = self.next_register();

                match infix.operator.as_str() {
                    "+" => self.emit(Add { dest, a, b }),
                    "-" => self.emit(Sub { dest, a, b }),
                    "*" => self.emit(Mul { dest, a, b }),
                    "/" => self.emit(Div { dest, a, b }),
                    "<" => self.emit(Lt { dest, a, b }),
                    "<=" => self.emit(Le { dest, a, b }),
                    ">" => self.emit(Lt { dest, a: b, b: a }),
                    ">=" => self.emit(Le { dest, a: b, b: a }),
                    "==" => self.emit(Eq { dest, a, b }),
                    "!=" => self.emit(NotEq { dest, a, b }),
                    _ => return Err(Error::new(format!("unknown operator: {}", infix.operator))),
                };
            },
            Ast::Pre(prefix) => {
                self.compile(*prefix.right)?;
                let a = self.next_register();
                let b = self.last_dest_reg();

                match prefix.operator.as_str() {
                    "-" => self.emit(neg!(a, b)),
                    "!" => self.emit(neg!(a, b)),
                    _ => return Err(Error::new(format!("unknown operator: {}", prefix.operator))),
                };
            },
            Ast::Let(let_expr) => {
                let symbol = self.symbols.define(let_expr.name.value);
                let index = symbol.index as u16;

                self.compile(*let_expr.value)?;
                let src = self.last_dest_reg();

                self.emit(set_global!(index, src));
            },
            Ast::Ident(identifier) => {
                let (index, scope) = match self.symbols.resolve(&identifier.value) {
                    Some(sym) => (sym.index, sym.scope),
                    None => return Err(Error::new(format!("Identifier not found: {}", identifier))),
                };

                self.load_symbol(index, scope);
            },
            x => return Err(Error::new(format!("Compilation not implemented for node: {:?}", x))),
        };

        Ok(())
    }

    pub fn optimize<T: Optimizer>(&mut self, optimizer: &mut T) -> Result<()> {
        optimizer.optimize(self)
    }

    fn emit(&mut self, ins: Instruction) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.emit(ins);
        };
    }

    fn next_register(&mut self) -> Register {
        if let Some(scope) = self.scopes.last_mut() {
            scope.next_register()
        } else {
            unreachable!();
        }
    }

    fn load_symbol(&mut self, index: usize, scope: Scope) {
        let index = index as u16;
        let reg = self.next_register();

        let ins = match scope {
            Scope::Global => get_global!(reg, index),
            Scope::Local => panic!("Scope not implemented!"),
            Scope::Builtin => panic!("Scope not implemented!"),
            Scope::Free => panic!("Scope not implemented!"),
            Scope::Function => panic!("Scope not implemented!"),
        };

        self.emit(ins);
    }

    fn last_dest_reg(&mut self) -> Register {
        self.current_scope().last_dest_register()
    }

    fn enter_scope(&mut self, scope: CompilationScope) {
        self.symbols = SymbolTable::enclose(self.symbols.clone());
        self.scopes.push(scope);
    }

    fn leave_scope(&mut self) -> CompilationScope {
        self.symbols = self.symbols.outer().unwrap();
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
            self.current_instructions()
                .iter()
                .map(ToString::to_string)
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
            compiler.compile(Ast::Prog(program))?;

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
    fn test_compiler() {
        let mut c = Compiler::new();
        assert!(c.compile(Ast::Prog(Program { exprs: Vec::new() })).is_ok());
    }

    #[test]
    fn test_compiler_scopes() {
        let mut compiler = Compiler::new();
        assert_eq!(1, compiler.scopes.len());

        let global_symbol_table = compiler.symbol_table();
        compiler.emit(mul!(0, 1, 2));

        compiler.enter_scope(CompilationScope::new());
        assert_eq!(2, compiler.scopes.len());

        compiler.emit(sub!(3, 4, 5));

        let last = compiler.scopes.last().unwrap().last_emitted_instruction.as_ref().unwrap();

        assert_eq!(sub!(3, 4, 5), last.instruction);
        assert_ne!(compiler.symbol_table(), global_symbol_table);

        compiler.leave_scope();
        assert_eq!(1, compiler.scopes.len());
        assert_eq!(compiler.symbol_table(), global_symbol_table);

        compiler.emit(add!(6, 7, 8));
        let last = compiler.scopes.last().unwrap().last_emitted_instruction.as_ref().unwrap();
        let prev = compiler.scopes.last().unwrap().prev_emitted_instruction.as_ref().unwrap();

        assert_eq!(add!(6, 7, 8), last.instruction);
        assert_eq!(mul!(0, 1, 2), prev.instruction);
    }

    #[test]
    fn test_int_expr() -> Result<()> {
        let tests = vec![
            TestCase {
                input: "5; 2;".to_string(),
                expected_constants: vec![kint!(5), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_integer_arithmetic() -> Result<()> {
        let tests = vec![
            TestCase {
                input: "1 + 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    add!(2, 0, 1),
                ],
            },
            TestCase {
                input: "1 - 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    sub!(2, 0, 1),
                ],
            },
            TestCase {
                input: "1 * 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    mul!(2, 0, 1),
                ],
            },
            TestCase {
                input: "1 / 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    div!(2, 0, 1),
                ],
            },
            TestCase {
                input: "-1 - -2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    neg!(1, 0),
                    load!(2, 1),
                    neg!(3, 2),
                    sub!(4, 1, 3),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_boolean_expressions() -> Result<()> {
        let tests = vec![
            TestCase {
                input: "true;".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    load_true!(0),
                ],
            },
            TestCase {
                input: "false;".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    load_false!(0),
                ],
            },
            TestCase {
                input: "!false;".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    load_false!(0),
                    neg!(1, 0),
                ],
            },
            TestCase {
                input: "1 < 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    lt!(2, 0, 1),
                ],
            },
            TestCase {
                input: "1 > 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    lt!(2, 1, 0),
                ],
            },
            TestCase {
                input: "1 >= 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    le!(2, 1, 0),
                ],
            },
            TestCase {
                input: "1 == 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    eq!(2, 0, 1),
                ],
            },
            TestCase {
                input: "1 != 2;".to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    load!(1, 1),
                    not_eq!(2, 0, 1),
                ],
            },
            TestCase {
                input: "true == false;".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    load_true!(0),
                    load_false!(1),
                    eq!(2, 0, 1),
                ],
            },
            TestCase {
                input: "true != false;".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    load_true!(0),
                    load_false!(1),
                    not_eq!(2, 0, 1),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_let_statements() -> Result<()> {
        let tests = vec![
            TestCase {
                input: r#"
                    let one = 1;
                    let two = 2;
                "#.to_string(),
                expected_constants: vec![kint!(1), kint!(2)],
                expected_instructions: vec![
                    load!(0, 0),
                    set_global!(0, 0),
                    load!(1, 1),
                    set_global!(1, 1),
                ],
            },
            TestCase {
                input: "let one = 1; one;".to_string(),
                expected_constants: vec![kint!(1)],
                expected_instructions: vec![
                    load!(0, 0),
                    set_global!(0, 0),
                    get_global!(1, 0),
                ],
            },
        ];

        run_compiler_tests(tests)
    }
}
