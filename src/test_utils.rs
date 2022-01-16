use crate::{
    ast::*,
    error::{Result, Error},
    lexer::{
        Lexer,
        token::Token,
    },
    parser::Parser,
};

pub fn parse(input: String) -> Result<Program> {
    let stream = input
        .as_bytes()
        .to_vec()
        .into_iter()
        .peekable();
    let lexer = Lexer::new(stream)?;

    let tokens = lexer
        .map(Result::unwrap)
        .peekable();
    let mut parser = Parser::new(tokens)?;
    let program = parser.parse()?;

    check_parser_errors(parser)?;

    Ok(program)
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

