use std::collections::HashMap;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    String,
    Ident,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Percent,
    Dot,

    Lt,
    Le,
    Gt,
    Ge,

    Eq,
    NotEq,

    // Delimiters
    Comma,
    SemiColon,
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Pub,
    Class,
    Constructor,
    Instance,
}

pub fn compute_keyword_map(map: &mut HashMap<&'static str, TokenType>) {
    let keywords = [
        ("fn", TokenType::Function),
        ("let", TokenType::Let),
        ("true", TokenType::True),
        ("false", TokenType::False),
        ("if", TokenType::If),
        ("else", TokenType::Else),
        ("return", TokenType::Return),
        ("pub", TokenType::Pub),
        ("class", TokenType::Class),
        ("constructor", TokenType::Constructor),
        ("@", TokenType::Instance),
    ];

    for t in keywords {
        map.insert(t.0, t.1);
    };
}

