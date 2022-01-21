use std::io::{
    self,
    Read,
    Write,
    BufRead,
    BufReader
};

use crate::{
    lexer::Lexer,
    parser::Parser,
    object::Primitive,
    error::Result,
    compiler::{
        Compiler,
        Bytecode,
        SymbolTable,
    },
    ast::Ast,
};

#[derive(Debug)]
struct State {
    constants: Vec<Primitive>,
    symbols: SymbolTable,
}

impl State {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            symbols: SymbolTable::new(),
        }
    }
}

pub struct Engine {
    runner: fn(Ast, &mut State) -> Result<Bytecode>,
    env: State,
}

impl Engine {
    pub fn vm() -> Self {
        Self {
            runner: Self::vm_runner,
            env: State::new(),
        }
    }

    pub fn run(&mut self, node: Ast) -> Result<Bytecode> {
        (self.runner)(node, &mut self.env)
    }

    fn vm_runner(node: Ast, state: &mut State) -> Result<Bytecode> {
        let mut compiler = Compiler::with_state(state.symbols.clone(), state.constants.clone());
        compiler.compile(node)?;

        Ok(compiler.bytecode())
    }
}

const PROMPT: &[u8; 4] = b">>> ";

pub fn start<I: Read, O: Write>(input: I, output: &mut O, engine: &mut Engine) -> Result<()> {
    let mut bufio = BufReader::new(input);
    let mut buf = String::new();

    loop {
        output.write_all(PROMPT)?;
        output.flush()?;
        bufio.read_line(&mut buf)?;

        let src = buf
            .bytes()
            .peekable();
        let lex = &mut Lexer::new(src)?;
        let tokens = lex
            .map(Result::unwrap)
            .peekable();
        let mut parser = Parser::new(tokens)?;
        let program = parser.parse()?;

        if let Err(_) = print_parser_errors(output, parser.errors()) {
            continue;
        };

        let evaluated = engine.run(Ast::Prog(program))?;

        output.write_all(format!("{}\n", evaluated).as_bytes())?;
        output.flush()?;
        buf.clear()
    }
}

fn print_parser_errors<O: Write>(output: &mut O, errors: Vec<String>) -> io::Result<()> {
    if errors.is_empty() {
        return Ok(())
    }

    output.write_all(b"Parser errors:\n")?;
    for e in errors {
        output.write_all(format!("\t{}\n", e).as_bytes())?;
    }

    output.flush()
}
