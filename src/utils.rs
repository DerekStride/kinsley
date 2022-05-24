use std::{
    iter::Peekable,
    fs::File,
    io::{
        Read,
        BufReader,
    },
};

use crate::{
    ast::*,
    object::Primitive,
    error::{Result, Error},
    lexer::{
        Lexer,
        token::Token,
    },
    parser::Parser,
    compiler::{
        Bytecode,
        Instruction,
    },
};

pub fn test_instructions(expected: &[Instruction], actual: &[Instruction]) {
    assert_eq!(
        expected,
        actual,
        "\n\nInstructions:\nwant:\n{}\ngot:\n{}\n",
        Bytecode::format_instructions(&expected),
        Bytecode::format_instructions(&actual),
        );
}

pub fn test_constants(expected: &[Primitive], actual: &[Primitive]) {
    assert_eq!(
        expected,
        actual,
        "\n\nConstants:\nwant:\n{}\ngot:\n{}\n",
        Bytecode::format_constants(&expected),
        Bytecode::format_constants(&actual),
        );
}

pub fn parse(input: String) -> Result<Program> {
    let stream = input
        .as_bytes()
        .to_vec()
        .into_iter()
        .peekable();
    parse_stream(stream)
}

pub fn parse_stream<I: Iterator<Item = u8>>(stream: Peekable<I>) -> Result<Program> {
    let lexer = Lexer::new(stream)?;

    let tokens = lexer
        .map(Result::unwrap)
        .peekable();
    let mut parser = Parser::new(tokens)?;
    let program = parser.parse()?;

    check_parser_errors(parser)?;

    Ok(program)
}

pub fn parse_file(filepath: &String) -> Result<Ast> {
    let file = File::open(filepath)?;
    let buf_reader = BufReader::new(file);
    let stream = buf_reader
        .bytes()
        .map(std::result::Result::unwrap)
        .peekable();

    let program = parse_stream(stream)?;

    Ok(Ast::Prog(program))
}

pub fn check_parser_errors<I: Iterator<Item = Token>>(p: Parser<I>) -> Result<()> {
    let errors = p.errors();

    if errors.is_empty() { return Ok(()); };

    let mut msg = format!("The Parser had {} errors:\n", errors.len());

    for e in errors {
        msg.push_str(&e);
        msg.push('\n');
    }

    Err(Error::new(msg))
}

