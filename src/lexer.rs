use std:: str;
use std::iter::Peekable;
use std::collections::HashMap;

use crate::error::Result;
use crate::lexer::token::Token;
use crate::lexer::token_type::TokenType;

pub mod token;
pub mod token_type;

pub struct Lexer<I: Iterator<Item = u8>> {
    input: Peekable<I>,
    ch: u8,
    keyword_map: HashMap<&'static str, TokenType>,
}

impl<I: Iterator<Item = u8>> Iterator for Lexer<I> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token { token_type: TokenType::Eof, .. }) => None,
            Ok(tok) => Some(Ok(tok)),
            Err(e) =>  Some(Err(e)),
        }
    }
}

impl<I: Iterator<Item = u8>> Lexer<I> {
    pub fn new(mut input: Peekable<I>) -> Result<Self> {
        let ch = input.next().unwrap_or(0);

        let lex = Self {
            input,
            ch,
            keyword_map: HashMap::new()
        };

        Ok(lex)
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.eat_whitespace()?;
        let ch = self.ch;

        let tok = match ch {
            b'=' => {
                let peeked = self.peek_char()?;
                if peeked == b'=' {
                    self.next_char()?;
                    new_token(TokenType::Eq, &[ch, peeked])?
                } else {
                    new_token(TokenType::Assign, &[ch])?
                }
            },
            b';' => new_token(TokenType::SemiColon, &[ch])?,
            b'(' => new_token(TokenType::LParen, &[ch])?,
            b')' => new_token(TokenType::RParen, &[ch])?,
            b'[' => new_token(TokenType::LBracket, &[ch])?,
            b']' => new_token(TokenType::RBracket, &[ch])?,
            b',' => new_token(TokenType::Comma, &[ch])?,
            b'.' => new_token(TokenType::Dot, &[ch])?,
            b'@' => new_token(TokenType::Instance, &[ch])?,
            b'+' => new_token(TokenType::Plus, &[ch])?,
            b'-' => new_token(TokenType::Minus, &[ch])?,
            b'*' => new_token(TokenType::Asterisk, &[ch])?,
            b'/' => new_token(TokenType::Slash, &[ch])?,
            b'<' => new_token(TokenType::Lt, &[ch])?,
            b'>' => new_token(TokenType::Gt, &[ch])?,
            b':' => new_token(TokenType::Colon, &[ch])?,
            b'!' => {
                let peeked = self.peek_char()?;
                if peeked == b'=' {
                    self.next_char()?;
                    new_token(TokenType::NotEq, &[ch, peeked])?
                } else {
                    new_token(TokenType::Bang, &[ch])?
                }
            },
            b'{' => new_token(TokenType::LBrace, &[ch])?,
            b'}' => new_token(TokenType::RBrace, &[ch])?,
            b'"' => {
                let string_lit = self.read_string()?;
                Token::new(TokenType::String, string_lit)
            },
            0 => new_token(TokenType::Eof, &[])?,
            _ => {
                if is_letter(ch) {
                    let ident = self.read_identifier()?;
                    let tok_type = self.lookup_ident(&ident);

                    Token::new(tok_type, ident)
                } else if is_digit(ch) {
                    let num = self.read_number()?;

                    Token::new(TokenType::Int, num)
                } else {
                    new_token(TokenType::Illegal, &[])?
                }
            },
        };

        self.next_char()?;

        Ok(tok)
    }

    fn eat_whitespace(&mut self) -> Result<()> {
        let mut ch = self.ch;

        while ch == b' ' || ch == b'\t' || ch == b'\n' || ch == b'\r' {
            ch = self.next_char()?;
        }

        Ok(())
    }

    fn next_char(&mut self) -> Result<u8> {
        self.ch = match self.input.next() {
            Some(ch) => ch,
            None => 0,
        };
        Ok(self.ch)
    }

    fn peek_char(&mut self) -> Result<u8> {
        match self.input.peek() {
            Some(&peeked) => Ok(peeked),
            None => Ok(0),
        }
    }

    fn read_identifier(&mut self) -> Result<String> {
        let mut ident = Vec::new();
        let mut ch = self.peek_char()?;
        ident.push(self.ch);

        while is_letter(ch) {
            self.next_char()?;
            ident.push(ch);
            ch = self.peek_char()?;
        }

        Ok(String::from_utf8(ident)?)
    }

    fn read_number(&mut self) -> Result<String> {
        let ident = &mut Vec::<u8>::new();
        let mut ch = self.peek_char()?;
        ident.push(self.ch);

        while is_digit(ch) {
            self.next_char()?;
            ident.push(ch);
            ch = self.peek_char()?;
        }

        Ok(String::from_utf8(ident.to_vec())?)
    }

    fn read_string(&mut self) -> Result<String> {
        self.next_char()?;
        let mut string_lit = Vec::new();

        while self.ch != b'"' {
            string_lit.push(self.ch);
            self.next_char()?;
        }

        Ok(String::from_utf8(string_lit)?)
    }

    fn lookup_ident(&mut self, ident: &str) -> TokenType {
        if self.keyword_map.is_empty() {
            crate::lexer::token_type::compute_keyword_map(&mut self.keyword_map);
        }

        match self.keyword_map.get(ident) {
            Some(&tok) => tok,
            None => TokenType::Ident,
        }
    }
}

#[inline]
fn new_token(t: TokenType, literal: &[u8]) -> Result<Token> {
    Ok(Token::new(t, str::from_utf8(literal)?.to_string()))
}

#[inline]
fn is_letter(ch: u8) -> bool {
    b'a' <= ch && ch <= b'z' ||
        b'A' <= ch && ch <= b'Z' ||
        ch == b'_'
}

#[inline]
fn is_digit(ch: u8) -> bool {
    b'0' <= ch && ch <= b'9'
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Expected {
        expected_type: TokenType,
        expected_literal: String,
    }

    fn lex<I: Iterator<Item = u8>>(input: I) -> Lexer<I> {
        Lexer::new(input.peekable()).unwrap()
    }

    fn assert_tokens<I: Iterator<Item = u8>>(expected: Vec<Expected>, lex: &mut Lexer<I>) {
        for t in expected {
            let tok = lex.next_token().unwrap();
            assert_eq!(t.expected_type, tok.token_type);
            assert_eq!(t.expected_literal, tok.literal);
        }
    }

    #[test]
    fn test_next_token() {
        let input = b"=+(){},;".to_vec();
        let l = &mut lex(input.into_iter());

        let tests = vec![
            Expected { expected_type: TokenType::Assign, expected_literal: "=".to_string() },
            Expected { expected_type: TokenType::Plus, expected_literal: "+".to_string() },
            Expected { expected_type: TokenType::LParen, expected_literal: "(".to_string() },
            Expected { expected_type: TokenType::RParen, expected_literal: ")".to_string() },
            Expected { expected_type: TokenType::LBrace, expected_literal: "{".to_string() },
            Expected { expected_type: TokenType::RBrace, expected_literal: "}".to_string() },
            Expected { expected_type: TokenType::Comma, expected_literal: ",".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::Eof, expected_literal: "".to_string() },
        ];

        assert_tokens(tests, l);
    }

    #[test]
    fn test_monkey_program() {
        let input = br###"
            let five = 5;
            let ten = 10;

            class Adder {
                constructor(z) {
                    @z = z;
                }

                pub fn add(x, y) {
                    x + y;
                }
            }

            let result = Adder.add(five, ten);
        "###.to_vec();
        let l = &mut lex(input.into_iter());

        let tests = vec![
            Expected { expected_type: TokenType::Let, expected_literal: "let".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "five".to_string() },
            Expected { expected_type: TokenType::Assign, expected_literal: "=".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "5".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::Let, expected_literal: "let".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "ten".to_string() },
            Expected { expected_type: TokenType::Assign, expected_literal: "=".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "10".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::Class, expected_literal: "class".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "Adder".to_string() },
            Expected { expected_type: TokenType::LBrace, expected_literal: "{".to_string() },
            Expected { expected_type: TokenType::Constructor, expected_literal: "constructor".to_string() },
            Expected { expected_type: TokenType::LParen, expected_literal: "(".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "z".to_string() },
            Expected { expected_type: TokenType::RParen, expected_literal: ")".to_string() },
            Expected { expected_type: TokenType::LBrace, expected_literal: "{".to_string() },
            Expected { expected_type: TokenType::Instance, expected_literal: "@".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "z".to_string() },
            Expected { expected_type: TokenType::Assign, expected_literal: "=".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "z".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::RBrace, expected_literal: "}".to_string() },
            Expected { expected_type: TokenType::Pub, expected_literal: "pub".to_string() },
            Expected { expected_type: TokenType::Function, expected_literal: "fn".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "add".to_string() },
            Expected { expected_type: TokenType::LParen, expected_literal: "(".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "x".to_string() },
            Expected { expected_type: TokenType::Comma, expected_literal: ",".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "y".to_string() },
            Expected { expected_type: TokenType::RParen, expected_literal: ")".to_string() },
            Expected { expected_type: TokenType::LBrace, expected_literal: "{".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "x".to_string() },
            Expected { expected_type: TokenType::Plus, expected_literal: "+".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "y".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::RBrace, expected_literal: "}".to_string() },
            Expected { expected_type: TokenType::RBrace, expected_literal: "}".to_string() },
            Expected { expected_type: TokenType::Let, expected_literal: "let".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "result".to_string() },
            Expected { expected_type: TokenType::Assign, expected_literal: "=".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "Adder".to_string() },
            Expected { expected_type: TokenType::Dot, expected_literal: ".".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "add".to_string() },
            Expected { expected_type: TokenType::LParen, expected_literal: "(".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "five".to_string() },
            Expected { expected_type: TokenType::Comma, expected_literal: ",".to_string() },
            Expected { expected_type: TokenType::Ident, expected_literal: "ten".to_string() },
            Expected { expected_type: TokenType::RParen, expected_literal: ")".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::Eof, expected_literal: "".to_string() },
        ];

        assert_tokens(tests, l);
    }

    #[test]
    fn test_monkey_symbols() {
        let input = br###"
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            "foobar"
            "foo bar"
            [1, 2]
            {"foo": "bar"}
        "###.to_vec();
        let l = &mut lex(input.into_iter());

        let tests = vec![
            Expected { expected_type: TokenType::Bang, expected_literal: "!".to_string() },
            Expected { expected_type: TokenType::Minus, expected_literal: "-".to_string() },
            Expected { expected_type: TokenType::Slash, expected_literal: "/".to_string() },
            Expected { expected_type: TokenType::Asterisk, expected_literal: "*".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "5".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "5".to_string() },
            Expected { expected_type: TokenType::Lt, expected_literal: "<".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "10".to_string() },
            Expected { expected_type: TokenType::Gt, expected_literal: ">".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "5".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::If, expected_literal: "if".to_string() },
            Expected { expected_type: TokenType::LParen, expected_literal: "(".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "5".to_string() },
            Expected { expected_type: TokenType::Lt, expected_literal: "<".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "10".to_string() },
            Expected { expected_type: TokenType::RParen, expected_literal: ")".to_string() },
            Expected { expected_type: TokenType::LBrace, expected_literal: "{".to_string() },
            Expected { expected_type: TokenType::Return, expected_literal: "return".to_string() },
            Expected { expected_type: TokenType::True, expected_literal: "true".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::RBrace, expected_literal: "}".to_string() },
            Expected { expected_type: TokenType::Else, expected_literal: "else".to_string() },
            Expected { expected_type: TokenType::LBrace, expected_literal: "{".to_string() },
            Expected { expected_type: TokenType::Return, expected_literal: "return".to_string() },
            Expected { expected_type: TokenType::False, expected_literal: "false".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::RBrace, expected_literal: "}".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "10".to_string() },
            Expected { expected_type: TokenType::Eq, expected_literal: "==".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "10".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "10".to_string() },
            Expected { expected_type: TokenType::NotEq, expected_literal: "!=".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "9".to_string() },
            Expected { expected_type: TokenType::SemiColon, expected_literal: ";".to_string() },
            Expected { expected_type: TokenType::String, expected_literal: "foobar".to_string() },
            Expected { expected_type: TokenType::String, expected_literal: "foo bar".to_string() },
            Expected { expected_type: TokenType::LBracket, expected_literal: "[".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "1".to_string() },
            Expected { expected_type: TokenType::Comma, expected_literal: ",".to_string() },
            Expected { expected_type: TokenType::Int, expected_literal: "2".to_string() },
            Expected { expected_type: TokenType::RBracket, expected_literal: "]".to_string() },
            Expected { expected_type: TokenType::LBrace, expected_literal: "{".to_string() },
            Expected { expected_type: TokenType::String, expected_literal: "foo".to_string() },
            Expected { expected_type: TokenType::Colon, expected_literal: ":".to_string() },
            Expected { expected_type: TokenType::String, expected_literal: "bar".to_string() },
            Expected { expected_type: TokenType::RBrace, expected_literal: "}".to_string() },
            Expected { expected_type: TokenType::Eof, expected_literal: "".to_string() },
        ];

        assert_tokens(tests, l);
    }
}
