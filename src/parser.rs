use std::{iter::Peekable, collections::HashMap};

use crate::{
    lexer::{
        token::Token,
        token_type::TokenType,
    },
    parser::precedence::Precedence,
    error::{Result, Error},
    ast::*,
};

mod precedence;

pub struct Parser<I: Iterator<Item = Token>> {
    l: Peekable<I>,
    tok: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, fn(&mut Self) -> Option<Ast>>,
    infix_parse_fns: HashMap<TokenType, fn(&mut Self, Ast) -> Option<Ast>>,
    precedences: HashMap<TokenType, Precedence>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(mut l: Peekable<I>) -> Result<Self> {
        let tok = match l.next() {
            Some(t) => t,
            None => return Err(Error::new(String::from("Unexpected EOF."))),
        };

        let mut precedences = HashMap::new();
        crate::parser::precedence::compute_priority_map(&mut precedences);

        let mut p = Self {
            l,
            tok,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences,
        };

        p.register_prefix(TokenType::Let, Self::parse_let_expression);
        p.register_prefix(TokenType::Return, Self::parse_return_expression);
        p.register_prefix(TokenType::Ident, Self::parse_identifier);
        p.register_prefix(TokenType::Int, Self::parse_integer_literal);
        p.register_prefix(TokenType::True, Self::parse_boolean);
        p.register_prefix(TokenType::False, Self::parse_boolean);
        p.register_prefix(TokenType::Bang, Self::parse_prefix_expression);
        p.register_prefix(TokenType::Minus, Self::parse_prefix_expression);
        p.register_prefix(TokenType::LParen, Self::parse_grouped_expression);
        p.register_prefix(TokenType::If, Self::parse_if_expression);
        p.register_prefix(TokenType::Function, Self::parse_function_expression);
        p.register_prefix(TokenType::String, Self::parse_string_expression);
        p.register_prefix(TokenType::LBracket, Self::parse_array_expression);
        p.register_prefix(TokenType::LBrace, Self::parse_hash_expression);

        p.register_infix(TokenType::Plus, Self::parse_infix_expression);
        p.register_infix(TokenType::Minus, Self::parse_infix_expression);
        p.register_infix(TokenType::Slash, Self::parse_infix_expression);
        p.register_infix(TokenType::Percent, Self::parse_infix_expression);
        p.register_infix(TokenType::Asterisk, Self::parse_infix_expression);
        p.register_infix(TokenType::Eq, Self::parse_infix_expression);
        p.register_infix(TokenType::NotEq, Self::parse_infix_expression);
        p.register_infix(TokenType::Lt, Self::parse_infix_expression);
        p.register_infix(TokenType::Gt, Self::parse_infix_expression);
        p.register_infix(TokenType::Le, Self::parse_infix_expression);
        p.register_infix(TokenType::Ge, Self::parse_infix_expression);
        p.register_infix(TokenType::LParen, Self::parse_call_expression);
        p.register_infix(TokenType::LBracket, Self::parse_index_expression);

        Ok(p)
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut program = Program::new();

        while !self.curr_token_is(TokenType::Eof) {
            if let Some(expr) = self.parse_expression_statement() {
                program.exprs.push(expr);
            }
            self.next_token()?;
        }

        Ok(program)
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn parse_expression_statement(&mut self) -> Option<Ast> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::SemiColon)?;

        Some(expr)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Ast> {
        let mut left = if let Some(prefix) = self.prefix_parse_fns.get(&self.tok.token_type) {
            prefix(self)?
        } else {
            self.errors.push(format!("Prefix parse function for {:?} not found.", self.tok.token_type));
            return None;
        };

        while !self.peek_token_is(TokenType::SemiColon) && precedence < self.peek_precedence() {
            let tok = match self.l.peek() {
                Some(tok) => tok.clone(),
                None => return Some(left),
            };
            if let Err(_) = self.next_token() { return Some(left) };

            left = if let Some(infix) = self.infix_parse_fns.get(&tok.token_type) {
                infix(self, left)?
            } else {
                return Some(left);
            };
        }

        Some(left)
    }

    fn parse_block_expression(&mut self) -> Option<BlockExpression> {
        let token = self.tok.clone();
        let mut exprs = Vec::new();

        self.ignore_next()?;

        while !self.curr_token_is(TokenType::RBrace) && !self.curr_token_is(TokenType::Eof) {
            if let Some(expr) = self.parse_expression_statement() {
                exprs.push(expr);
            }
            self.ignore_next()?;
        }

        Some(
            BlockExpression {
                token,
                exprs,
            }
        )
    }

    fn parse_return_expression(&mut self) -> Option<Ast> {
        let token = self.tok.clone();

        self.ignore_next()?;

        let retval = self.parse_expression(Precedence::Lowest)?;

        Some(
            Ast::Return(
                ReturnExpression {
                    token,
                    retval: Box::new(retval),
                }
            )
        )
    }

    fn parse_let_expression(&mut self) -> Option<Ast> {
        let token = self.tok.clone();

        self.expect_peek(TokenType::Ident)?;

        let name = Identifier {
            token: self.tok.clone(),
            value: self.tok.literal.clone(),
        };

        self.expect_peek(TokenType::Assign)?;
        self.ignore_next()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        Some(
            Ast::Let(
                LetExpression {
                    token,
                    name,
                    value: Box::new(value),
                }
            )
        )
    }


    fn parse_identifier(&mut self) -> Option<Ast> {
        Some(
            Ast::Ident(
                Identifier {
                    token: self.tok.clone(),
                    value: self.tok.literal.clone(),
                }
            )
        )
    }

    fn parse_integer_literal(&mut self) -> Option<Ast> {
        let lit = match self.tok.literal.parse::<i128>() {
            Ok(x) => x,
            Err(_) => {
                self.errors.push(format!("Could not parse {} as integer", self.tok.literal));
                return None
            },
        };

        Some(
            Ast::Int(
                IntegerLiteral {
                    token: self.tok.clone(),
                    value: lit,
                }
            )
        )
    }

    fn parse_boolean(&mut self) -> Option<Ast> {
        Some(
            Ast::Bool(
                BooleanLiteral {
                    token: self.tok.clone(),
                    value: self.curr_token_is(TokenType::True),
                }
            )
        )
    }

    fn parse_grouped_expression(&mut self) -> Option<Ast> {
        self.ignore_next()?;

        let exp = self.parse_expression(Precedence::Lowest);

        self.expect_peek(TokenType::RParen)?;

        exp
    }

    fn parse_if_expression(&mut self) -> Option<Ast> {
        let token = self.tok.clone();

        self.expect_peek(TokenType::LParen)?;
        self.ignore_next()?;

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::RParen)?;
        self.expect_peek(TokenType::LBrace)?;

        let consequence = self.parse_block_expression()?;
        let mut alternative: Option<BlockExpression> = None;

        if self.peek_token_is(TokenType::Else) {
            self.ignore_next()?;
            self.expect_peek(TokenType::LBrace)?;

            alternative = self.parse_block_expression();
        }

        Some(
            Ast::If(
                IfExpression {
                    token,
                    condition: Box::new(condition),
                    consequence,
                    alternative,
                }
            )
        )
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut params = Vec::new();

        if self.peek_token_is(TokenType::RParen) {
            if let Err(_) = self.next_token() { return params; };
            return params;
        };
        if let Err(_) = self.next_token() { return params; };

        params.push(Identifier { token: self.tok.clone(), value: self.tok.literal.clone() });

        while self.peek_token_is(TokenType::Comma) {
            if let Err(_) = self.next_token() { return params; };
            if let Err(_) = self.next_token() { return params; };
            params.push(Identifier { token: self.tok.clone(), value: self.tok.literal.clone() });
        }

        match self.expect_peek(TokenType::RParen) {
            Some(_) => params,
            None => params,
        }
    }

    fn parse_function_expression(&mut self) -> Option<Ast> {
        let token = self.tok.clone();

        self.expect_peek(TokenType::Ident)?;

        let name = Identifier {
            token: self.tok.clone(),
            value: self.tok.literal.clone(),
        };

        self.expect_peek(TokenType::LParen)?;

        let params = self.parse_function_parameters();

        self.expect_peek(TokenType::LBrace)?;

        let body = self.parse_block_expression()?;

        Some(
            Ast::Fn(
                FnLiteral {
                    name,
                    token,
                    params,
                    body,
                }
            )
        )
    }

    fn parse_string_expression(&mut self) -> Option<Ast> {
        Some(
            Ast::Str(
                StringLiteral {
                    token: self.tok.clone(),
                    value: self.tok.literal.clone(),
                }
            )
        )
    }

    fn parse_array_expression(&mut self) -> Option<Ast> {
        let token = self.tok.clone();
        let elements = self.parse_expression_list(TokenType::RBracket);

        Some(
            Ast::Vec(
                VecLiteral {
                    token,
                    elements,
                }
            )
        )
    }

    fn parse_hash_expression(&mut self) -> Option<Ast> {
        let token = self.tok.clone();
        let mut pairs = HashMap::new();
        while !self.peek_token_is(TokenType::RBrace) {
            self.ignore_next()?;

            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(TokenType::Colon)?;
            self.ignore_next()?;

            let value = self.parse_expression(Precedence::Lowest)?;

            pairs.insert(key, value);

            if !self.peek_token_is(TokenType::RBrace) {
                if !self.peek_token_is(TokenType::Comma) {
                    return None;
                };
                self.ignore_next()?;
            };
        }

        self.expect_peek(TokenType::RBrace)?;

        Some(
            Ast::Hash(
                HashLiteral {
                    token,
                    pairs,
                }
            )
        )
    }

    fn parse_prefix_expression(&mut self) -> Option<Ast> {
        let token = self.tok.clone();
        let operator = token.literal.clone();

        self.ignore_next()?;

        let right = self.parse_expression(Precedence::Prefix)?;

        Some(
            Ast::Pre(
                Prefix {
                    token,
                    operator,
                    right: Box::new(right),
                }
            )
        )
    }

    fn parse_infix_expression(&mut self, left: Ast) -> Option<Ast> {
        let token = self.tok.clone();
        let operator = token.literal.clone();
        let precedence = self.curr_precedence();

        self.ignore_next()?;

        let right = self.parse_expression(precedence)?;

        Some(
            Ast::In(
                Infix {
                    token,
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                }
            )
        )
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Ast> {
        let mut args = Vec::new();

        if self.peek_token_is(end) {
            if let Err(_) = self.next_token() { return args; };
            return args;
        };
        if let Err(_) = self.next_token() { return args; };

        if let Some(expr) = self.parse_expression(Precedence::Lowest) {
            args.push(expr);
        } else {
            return args;
        }

        while self.peek_token_is(TokenType::Comma) {
            if let Err(_) = self.next_token() { return args; };
            if let Err(_) = self.next_token() { return args; };

            if let Some(expr) = self.parse_expression(Precedence::Lowest) {
                args.push(expr);
            } else {
                return args;
            }
        }

        match self.expect_peek(end) {
            Some(_) => args,
            None => args,
        }
    }

    fn parse_call_expression(&mut self, function: Ast) -> Option<Ast> {
        let token = self.tok.clone();
        let args = self.parse_expression_list(TokenType::RParen);

        Some(
            Ast::Call(
                FnCall {
                    token,
                    function: Box::new(function),
                    args,
                }
            )
        )
    }

    fn parse_index_expression(&mut self, left: Ast) -> Option<Ast> {
        let token = self.tok.clone();
        self.ignore_next()?;

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::RBracket)?;

        Some(
            Ast::Index(
                IndexOperation {
                    token,
                    left: Box::new(left),
                    index: Box::new(index),
                }
            )
        )
    }

    fn next_token(&mut self) -> Result<()> {
        self.tok = match self.l.next() {
            Some(t) => t,
            None => Token { token_type: TokenType::Eof, literal: "".to_string() },
        };
        Ok(())
    }

    fn ignore_next(&mut self) -> Option<()> {
        match self.next_token() {
            Ok(_) => Some(()),
            Err(_) => None,
        }
    }

    fn expect_peek(&mut self, t: TokenType) -> Option<()> {
        if self.peek_token_is(t) {
            self.ignore_next()
        } else {
            self.peek_error(t);
            None
        }
    }

    fn curr_token_is(&self, t: TokenType) -> bool {
        self.tok.token_type == t
    }

    fn peek_token_is(&mut self, t: TokenType) -> bool {
        if let Some(tok) = self.l.peek() {
            tok.token_type == t
        } else {
            t == TokenType::Eof
        }
    }

    fn peek_error(&mut self, t: TokenType) {
        let actual = if let Some(tok) = self.l.peek() {
            tok.token_type
        } else {
            TokenType::Eof
        };
        let msg = format!("Expected next token to be {:?}, got {:?} instead.", t, actual);
        self.errors.push(msg);
    }

    fn peek_precedence(&mut self) -> Precedence {
        if let Some(tok) = self.l.peek() {
            if let Some(&p) = self.precedences.get(&tok.token_type) {
                return p;
            }
        }

        Precedence::Lowest
    }

    fn curr_precedence(&mut self) -> Precedence {
        if let Some(&p) = self.precedences.get(&self.tok.token_type) {
            p
        } else {
            Precedence::Lowest
        }
    }

    fn register_prefix(&mut self, tt: TokenType, func: fn(&mut Self) -> Option<Ast>) {
        self.prefix_parse_fns.insert(tt, func);
    }

    fn register_infix(&mut self, tt: TokenType, func: fn(&mut Self, Ast) -> Option<Ast>) {
        self.infix_parse_fns.insert(tt, func);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{utils::*, lexer::Lexer};

    fn test_let_expression(node: &Ast, expected_ident: &String, expected_expr: &Ast) {
        let let_expr = match node {
            Ast::Let(l) => l,
            _ => panic!("Expected let statement. Got {:?}", node),
        };

        assert_eq!(*expected_ident, let_expr.name.value);
        assert_eq!(*expected_expr, *let_expr.value);
    }

    fn test_infix_expression(actual: &Ast, left: &Ast, operator: String, right: &Ast) {
        if let Ast::In(x) = actual {
            assert_eq!(*left, *x.left);
            assert_eq!(operator, x.operator);
            assert_eq!(*right, *x.right);
        } else {
            panic!("Expression `{}` was not an infix expression.", actual);
        };
    }

    #[test]
    fn test_let_statements() -> Result<()> {
        let input = r###"
            let x = 5;
            let y = true;
            let foobar = y;
        "###.to_string();

        let program = parse(input)?;
        assert_eq!(3, program.exprs.len());

        let tests = vec![
            ("x".to_string(), int_node!(5)),
            ("y".to_string(), bool_node!(true)),
            ("foobar".to_string(), ident_node!("y")),
        ];

        for (i, t) in tests.iter().enumerate() {
            if let Some(stmt) = program.exprs.get(i) {
                test_let_expression(stmt, &t.0, &t.1);
            } else {
                panic!("Statement {} was missing", i);
            }
        }

        Ok(())
    }

    #[test]
    fn test_parser_errors() -> Result<()> {
        let input = r###"
            let x 5
            let 838383
        "###
            .to_string()
            .as_bytes()
            .to_vec()
            .into_iter()
            .peekable();

        let l = Lexer::new(input)?;
        let tokens = l
            .map(Result::unwrap)
            .peekable();
        let mut p = Parser::new(tokens)?;
        p.parse()?;


        let tests = vec![
            (TokenType::Assign, TokenType::Int),
            (TokenType::SemiColon, TokenType::Let),
            (TokenType::Ident, TokenType::Int),
            (TokenType::SemiColon, TokenType::Eof),
        ];

        for (i, err) in p.errors.iter().enumerate() {
            let test = tests.get(i).unwrap();

            let msg = format!("Expected next token to be {:?}, got {:?} instead.", test.0, test.1);
            assert_eq!(msg, *err);
        }

        assert_eq!(4, p.errors.len());

        Ok(())
    }

    #[test]
    fn test_return_statement() -> Result<()> {
        let input = r###"
            return 5;
            return 10;
            return 993322;
        "###.to_string();

        let program = parse(input)?;

        assert_eq!(3, program.exprs.len());

        for expr in program.exprs {
            if let Ast::Return(ret) = expr {
                assert_eq!("return", ret.token.literal);
            } else {
                panic!("expr {:?} was not a Ast::Return.", expr);
            }
        }

        Ok(())
    }

    #[test]
    fn test_identifier_expressions() -> Result<()> {
        let input = r###"
            foobar;
        "###.to_string();

        let program = parse(input)?;
        assert_eq!(1, program.exprs.len());

        assert_eq!(ident_node!("foobar"), program.exprs[0]);

        Ok(())
    }

    #[test]
    fn test_integer_literal_expressions() -> Result<()> {
        let input = r###"
            5;
        "###.to_string();

        let program = parse(input)?;

        assert_eq!(1, program.exprs.len());
        assert_eq!(int_node!(5), program.exprs[0]);

        Ok(())
    }

    #[test]
    fn test_boolean_expressions() -> Result<()> {
        let input = r###"
            true;
            false;
        "###.to_string();

        let program = parse(input)?;

        assert_eq!(2, program.exprs.len());
        assert_eq!(bool_node!(true), program.exprs[0]);
        assert_eq!(bool_node!(false), program.exprs[1]);

        Ok(())
    }

    #[test]
    fn test_parsing_prefix_expressions() -> Result<()> {
        let tests = vec![
            ("!5;".to_string(), "!".to_string(), int_node!(5)),
            ("-15;".to_string(), "-".to_string(), int_node!(15)),
            ("!true;".to_string(), "!".to_string(), bool_node!(true)),
            ("!false;".to_string(), "!".to_string(), bool_node!(false)),
        ];

        for tt in tests {
            let program = parse(tt.0)?;
            assert_eq!(1, program.exprs.len());

            if let Ast::Pre(ref expr) = program.exprs[0] {
                assert_eq!(tt.1, expr.operator);
                assert_eq!(tt.2, *expr.right);
            } else {
                panic!("Expression `{}` was not a Prefix expression.", program.exprs[0]);
            };
        };

        Ok(())
    }

    #[test]
    fn test_parsing_infix_expressions() -> Result<()> {
        let tests = vec![
            ("5 + 5;".to_string(), int_node!(5), "+".to_string(), int_node!(5)),
            ("5 - 5;".to_string(), int_node!(5), "-".to_string(), int_node!(5)),
            ("5 * 5;".to_string(), int_node!(5), "*".to_string(), int_node!(5)),
            ("5 / 5;".to_string(), int_node!(5), "/".to_string(), int_node!(5)),
            ("5 % 2;".to_string(), int_node!(5), "%".to_string(), int_node!(2)),
            ("5 > 5;".to_string(), int_node!(5), ">".to_string(), int_node!(5)),
            ("5 < 5;".to_string(), int_node!(5), "<".to_string(), int_node!(5)),
            ("5 >= 5;".to_string(), int_node!(5), ">=".to_string(), int_node!(5)),
            ("5 <= 5;".to_string(), int_node!(5), "<=".to_string(), int_node!(5)),
            ("5 == 5;".to_string(), int_node!(5), "==".to_string(), int_node!(5)),
            ("5 != 5;".to_string(), int_node!(5), "!=".to_string(), int_node!(5)),
            ("true == true;".to_string(), bool_node!(true), "==".to_string(), bool_node!(true)),
            ("true != false;".to_string(), bool_node!(true), "!=".to_string(), bool_node!(false)),
            ("false == false;".to_string(), bool_node!(false), "==".to_string(), bool_node!(false)),
        ];

        for tt in tests {
            let program = parse(tt.0)?;
            assert_eq!(1, program.exprs.len());
            test_infix_expression(&program.exprs[0], &tt.1, tt.2, &tt.3);
        };

        Ok(())
    }

    #[test]
    fn test_operator_precedence_parsing() -> Result<()> {
        let tests = vec![
            ("-a * b;".to_string(), "((-a) * b)".to_string()),
            ("!-a;".to_string(), "(!(-a))".to_string()),
            ("a + b + c;".to_string(), "((a + b) + c)".to_string()),
            ("a + b - c;".to_string(), "((a + b) - c)".to_string()),
            ("a * b * c;".to_string(), "((a * b) * c)".to_string()),
            ("a * b / c;".to_string(), "((a * b) / c)".to_string()),
            ("a + b / c;".to_string(), "(a + (b / c))".to_string()),
            ("a + b * c + d / e - f;".to_string(), "(((a + (b * c)) + (d / e)) - f)".to_string()),
            ("3 + 4; -5 * 5;".to_string(), "(3 + 4)((-5) * 5)".to_string()),
            ("5 > 4 == 3 < 4;".to_string(), "((5 > 4) == (3 < 4))".to_string()),
            ("5 < 4 != 3 > 4;".to_string(), "((5 < 4) != (3 > 4))".to_string()),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5;".to_string(), "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string()),
            ("true;".to_string(), "true".to_string()),
            ("false;".to_string(), "false".to_string()),
            ("3 > 5 == false;".to_string(), "((3 > 5) == false)".to_string()),
            ("3 < 5 == true;".to_string(), "((3 < 5) == true)".to_string()),
            ("1 + (2 + 3) + 4;".to_string(), "((1 + (2 + 3)) + 4)".to_string()),
            ("(5 + 5) * 2;".to_string(), "((5 + 5) * 2)".to_string()),
            ("2 / (5 + 5);".to_string(), "(2 / (5 + 5))".to_string()),
            ("-(5 + 5);".to_string(), "(-(5 + 5))".to_string()),
            ("!(true == true);".to_string(), "(!(true == true))".to_string()),
            ("a + add(b * c) + d;".to_string(), "((a + add((b * c))) + d)".to_string()),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));".to_string(), "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_string()),
            ("add(a + b + c * d / f + g);".to_string(), "add((((a + b) + ((c * d) / f)) + g))".to_string()),
            ("a * [1, 2, 3, 4][b * c] * d;".to_string(), "((a * ([1, 2, 3, 4][(b * c)])) * d)".to_string()),
            ("add(a * b[2], b[1], 2 * [1, 2][1]);".to_string(), "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))".to_string()),
        ];

        for tt in tests {
            let program = parse(tt.0)?;
            assert_eq!(tt.1, format!("{}", program));
        }

        Ok(())
    }

    #[test]
    fn test_parsing_if_expressions() -> Result<()> {
        let input = r###"
            if (x < y) { x; };
        "###.to_string();

        let program = parse(input)?;
        assert_eq!(1, program.exprs.len());

        let if_expr = if let Ast::If(x) = program.exprs.get(0).unwrap() {
            x
        } else {
            panic!("Program statement was not an if expression.");
        };

        test_infix_expression(&if_expr.condition, &ident_node!("x"), "<".to_string(), &ident_node!("y"));

        let conseq = &if_expr.consequence;

        assert_eq!(1, conseq.exprs.len());
        assert_eq!(ident_node!("x"), conseq.exprs[0]);

        assert!(if_expr.alternative.is_none());

        Ok(())
    }

    #[test]
    fn test_parsing_if_else_expressions() -> Result<()> {
        let input = r###"
            if (x < y) { x; } else { y; };
        "###.to_string();

        let program = parse(input)?;
        assert_eq!(1, program.exprs.len());

        let if_expr = if let Ast::If(x) = program.exprs.get(0).unwrap() {
            x
        } else {
            panic!("Program statement was not an if expression.");
        };

        test_infix_expression(&if_expr.condition, &ident_node!("x"), "<".to_string(), &ident_node!("y"));

        let conseq = &if_expr.consequence;

        assert_eq!(1, conseq.exprs.len());
        assert_eq!(ident_node!("x"), conseq.exprs[0]);

        let alt = if_expr.alternative.as_ref().unwrap();

        assert_eq!(1, alt.exprs.len());
        assert_eq!(ident_node!("y"), alt.exprs[0]);

        Ok(())
    }

    #[test]
    fn test_parsing_function_literals() -> Result<()> {
        let input = r###"
            fn add(x, y) { x + y; };
        "###.to_string();

        let program = parse(input)?;
        assert_eq!(1, program.exprs.len());

        let fn_expr = if let Ast::Fn(ref f) = program.exprs[0] {
            f
        } else {
            panic!("Program statement was not a Function expression.");
        };

        assert_eq!(2, fn_expr.params.len());

        assert_eq!(ident_node!("x"), Ast::Ident(fn_expr.params[0].clone()));
        assert_eq!(ident_node!("y"), Ast::Ident(fn_expr.params[1].clone()));

        assert_eq!(1, fn_expr.body.exprs.len());

        let expr = fn_expr.body.exprs.get(0).unwrap();
        test_infix_expression(&expr, &ident_node!("x".to_string()), "+".to_string(), &ident_node!("y".to_string()));

        Ok(())
    }

    #[test]
    fn test_function_param_parsing() -> Result<()> {
        let tests = vec![
            ("fn empty() {};".to_string(), Vec::new()),
            ("fn identity(x) {};".to_string(), vec![ident_node!("x")]),
            ("fn xyz(x, y, z) {};".to_string(), vec![ident_node!("x"), ident_node!("y"), ident_node!("z")]),
        ];

        for tt in tests {
            let program = parse(tt.0)?;
            assert_eq!(1, program.exprs.len());

            if let Ast::Fn(ref f) = program.exprs[0] {
                assert_eq!(tt.1.len(), f.params.len());
                let params: Vec<Ast> = f.params
                    .iter()
                    .map(|i| Ast::Ident(i.clone()))
                    .collect();
                assert_eq!(tt.1, params);
            } else {
                panic!("Program statement was not a Function expression.");
            };
        }

        Ok(())
    }

    #[test]
    fn test_fn_call_parsing() -> Result<()> {
        let input = r###"
            add(1, 2 * 3, 4 + 5);
        "###.to_string();

        let program = parse(input)?;
        assert_eq!(1, program.exprs.len());

        if let Ast::Call(call_expr) = &program.exprs[0] {
            assert_eq!(3, call_expr.args.len());

            assert_eq!(ident_node!("add"), *call_expr.function);
            assert_eq!(int_node!(1), call_expr.args[0]);

            test_infix_expression(&call_expr.args[1], &int_node!(2), "*".to_string(), &int_node!(3));
            test_infix_expression(&call_expr.args[2], &int_node!(4), "+".to_string(), &int_node!(5));
        } else {
            panic!("Program statement was not a Function call expression.");
        };

        Ok(())
    }

    #[test]
    fn test_string_expressions() -> Result<()> {
        let input = "\"hello world\";".to_string();

        let program = parse(input)?;

        assert_eq!(1, program.exprs.len());
        assert_eq!(str_node!("hello world"), program.exprs[0]);

        Ok(())
    }

    #[test]
    fn test_vec_expressions() -> Result<()> {
        let input = "[1, 2 * 2, 3 + 3];".to_string();

        let program = parse(input)?;

        assert_eq!(1, program.exprs.len());

        if let Ast::Vec(v) = program.exprs.get(0).unwrap() {
            assert_eq!(int_node!(1), v.elements[0]);
            test_infix_expression(v.elements.get(1).unwrap(), &int_node!(2), "*".to_string(), &int_node!(2));
            test_infix_expression(v.elements.get(2).unwrap(), &int_node!(3), "+".to_string(), &int_node!(3));
        } else {
            panic!("Expected Vec expression");
        };

        Ok(())
    }

    #[test]
    fn test_vec_index_expression() -> Result<()> {
        let input = "myArray[1 * 1];".to_string();

        let program = parse(input)?;

        assert_eq!(1, program.exprs.len());

        if let Ast::Index(index) = program.exprs.get(0).unwrap() {
            assert_eq!(ident_node!("myArray"), *index.left);
            test_infix_expression(&index.index, &int_node!(1), "*".to_string(), &int_node!(1));
        } else {
            panic!("Expected Index Expression");
        };

        Ok(())
    }

    #[test]
    fn test_parsing_empty_hash_literal() -> Result<()> {
        let input = "{};".to_string();

        let program = parse(input)?;
        assert_eq!(1, program.exprs.len());

        if let Ast::Hash(h) = program.exprs.get(0).unwrap() {
            assert_eq!(0, h.pairs.len());
        } else {
            panic!("Expected hash expression");
        };

        Ok(())
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() -> Result<()> {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3};".to_string();

        let program = parse(input)?;

        let tests = HashMap::from([
            ("one".to_string(), 1),
            ("two".to_string(), 2),
            ("three".to_string(), 3),
        ]);

        assert_eq!(1, program.exprs.len());

        if let Ast::Hash(ref h) = program.exprs[0] {
            assert_eq!(3, h.pairs.len());

            for (k, v) in &h.pairs {
                if let Ast::Str(s) = k {
                    let expected = tests.get(&s.value).unwrap();
                    assert_eq!(int_node!(*expected), *v);
                } else {
                    panic!("Expected key to be a string, got: {}", k);
                }
            };
        } else {
            panic!("Expected hash expression");
        };

        Ok(())
    }

    #[test]
    fn test_parsing_hash_literals_boolean_keys() -> Result<()> {
        let input = "{true: 1, false: 2};".to_string();

        let program = parse(input)?;

        let tests = HashMap::from([
            (true, 1),
            (false, 2),
        ]);

        assert_eq!(1, program.exprs.len());

        if let Ast::Hash(ref h) = program.exprs[0] {
            assert_eq!(2, h.pairs.len());

            for (k, v) in &h.pairs {
                if let Ast::Bool(b) = k {
                    let expected = tests.get(&b.value).unwrap();
                    assert_eq!(int_node!(*expected), *v);
                } else {
                    panic!("Expected key to be a bool, got: {}", k);
                }
            };
        } else {
            panic!("Expected hash expression");
        };

        Ok(())
    }

    #[test]
    fn test_parsing_hash_literals_int_keys() -> Result<()> {
        let input = "{1: \"one\", 2: \"two\"};".to_string();

        let program = parse(input)?;

        let tests = HashMap::from([
            (1, "one".to_string()),
            (2, "two".to_string()),
        ]);

        assert_eq!(1, program.exprs.len());

        if let Ast::Hash(ref h) = program.exprs[0] {
            assert_eq!(2, h.pairs.len());

            for (k, v) in &h.pairs {
                if let Ast::Int(i) = k {
                    let expected = tests.get(&i.value).unwrap();
                    if let Ast::Str(s) = v {
                        assert_eq!(*expected, s.value);
                    } else {
                        panic!("expected value to be a string, got: {}", v);
                    }
                } else {
                    panic!("Expected key to be an int, got: {}", k);
                }
            };
        } else {
            panic!("Expected hash expression");
        };

        Ok(())
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() -> Result<()> {
        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5};".to_string();

        let program = parse(input)?;

        assert_eq!(1, program.exprs.len());

        if let Ast::Hash(ref h) = program.exprs[0] {
            assert_eq!(3, h.pairs.len());
            let one = h.pairs.get(&str_node!("one")).unwrap();
            let two = h.pairs.get(&str_node!("two")).unwrap();
            let three = h.pairs.get(&str_node!("three")).unwrap();
            test_infix_expression(one, &int_node!(0), "+".to_string(), &int_node!(1));
            test_infix_expression(two, &int_node!(10), "-".to_string(), &int_node!(8));
            test_infix_expression(three, &int_node!(15), "/".to_string(), &int_node!(5));
        } else {
            panic!("Expected hash expression");
        };

        Ok(())
    }

    #[test]
    fn test_parsing_functions_with_names() -> Result<()> {
        let input = "fn myFunction() { };".to_string();
        let program = parse(input)?;
        assert_eq!(1, program.exprs.len());

        match &program.exprs[0] {
            Ast::Fn(f) => assert_eq!(ident_node!("myFunction"), Ast::Ident(f.name.clone())),
            x => panic!("Expected function expression: {}", x),
        };

        Ok(())
    }
}
