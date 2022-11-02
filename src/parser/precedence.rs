use std::collections::HashMap;

use crate::lexer::token_type::TokenType;

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Dot,       // myArray[0]
    Call,        // myFunction(X)
    Index,       // myArray[0]
}

pub fn compute_priority_map(map: &mut HashMap<TokenType, Precedence>) {
    let precedences = [
        (TokenType::Eq,         Precedence::Equals),
        (TokenType::NotEq,      Precedence::Equals),
        (TokenType::Lt,         Precedence::LessGreater),
        (TokenType::Gt,         Precedence::LessGreater),
        (TokenType::Le,         Precedence::LessGreater),
        (TokenType::Ge,         Precedence::LessGreater),
        (TokenType::Plus,       Precedence::Sum),
        (TokenType::Minus,      Precedence::Sum),
        (TokenType::Slash,      Precedence::Product),
        (TokenType::Percent,      Precedence::Product),
        (TokenType::Asterisk,   Precedence::Product),
        (TokenType::Dot,        Precedence::Dot),
        (TokenType::LParen,     Precedence::Call),
        (TokenType::LBracket,   Precedence::Index),
    ];

    for t in precedences {
        map.insert(t.0, t.1);
    };
}

