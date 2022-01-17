#[macro_export]
macro_rules! kint {
    ($value:expr) => (
        $crate::object::Primitive::Int(
            $crate::object::Integer {
                value: $value
            }
        )
    );
}

#[macro_export]
macro_rules! kstr {
    ($value:expr) => (
        $crate::object::Primitive::Str(
            $crate::object::KString {
                value: $value.to_string()
            }
        )
    );
}

#[macro_export]
macro_rules! kerr {
    ($value:expr) => (
        $crate::object::Primitive::Error(
            $crate::object::KError {
                value: $value.to_string()
            }
        )
    );
}

#[macro_export]
macro_rules! kvec {
    () => ({
        $crate::object::KVector {
            elements: std::vec::Vec::new()
        }
    });

    ( $( $elem:expr ),*) => ({
        let elements = vec![ $( $elem ), * ];

        $crate::object::KVector {
            elements,
        }
    });

    ( $( $elem:expr ),* ,) => ({
        kvec![ $( $elem ), * ]
    });
}

#[macro_export]
macro_rules! khash {
    () => ({
        $crate::object::KHash {
            pairs: std::collections::HashMap::new()
        }
    });

    ( $( ($k:expr, $v:expr) ),*) => ({
        let mut pairs = std::collections::HashMap::new();

        $(
            let key = match $k.clone() {
                $crate::object::Primitive::Str(x) => $crate::object::HashKey::Str(x),
                $crate::object::Primitive::Int(x) => $crate::object::HashKey::Int(x),
                $crate::object::Primitive::Bool(x) => $crate::object::HashKey::Bool(x),
                _ => panic!("Expected key to be Int, Str, or Bool. Got: {:?}", $k),
            };

            let pair = $crate::object::HashPair {
                key: $k,
                value: $v,
            };

            pairs.insert(key, pair);
        )*

        $crate::object::Primitive::Hash(
            $crate::object::KHash {
                pairs,
            }
        )
    });

    ( $( ($k:expr, $v:expr) ),* ,) => ({
        khash![ $( ($k, $v) ), * ]
    });
}

#[macro_export]
macro_rules! int_node {
    ($value:expr) => (
        $crate::ast::KNode::Int(
            $crate::ast::IntegerLiteral {
                token: $crate::lexer::token::Token { token_type: TokenType::Int, literal: format!("{}", $value) },
                value: $value,
            }
        )
    );
}

#[macro_export]
macro_rules! str_node {
    ($value:expr) => (
        $crate::ast::KNode::Str(
            $crate::ast::StringLiteral {
                token: $crate::lexer::token::Token { token_type: TokenType::String, literal: $value.to_string() },
                value: $value.to_string(),
            }
        )
    );
}

#[macro_export]
macro_rules! bool_node {
    ($value:expr) => ({
        let literal = if $value {
            $crate::ast::BooleanLiteral {
                token: $crate::lexer::token::Token { token_type: TokenType::True, literal: "true".to_string() },
                value: $value,
            }
        } else {
            $crate::ast::BooleanLiteral {
                token: $crate::lexer::token::Token { token_type: TokenType::False, literal: "false".to_string() },
                value: $value,
            }
        };

        $crate::ast::KNode::Bool(literal)
    });
}

#[macro_export]
macro_rules! ident_node {
    ($value:expr) => (
        $crate::ast::KNode::Ident(
            $crate::ast::Identifier {
                token: $crate::lexer::token::Token { token_type: TokenType::Ident, literal: $value.to_string() },
                value: $value.to_string(),
            }
        )
    );
}

#[macro_export]
macro_rules! vec_node {
    () => (
        $crate::ast::KNode::Vec(
            $crate::ast::VecLiteral {
                token: $crate::lexer::token::Token { token_type: TokenType::RBracket, literal: "[".to_string() },
                elements: std::vec::Vec::new()
            }
        )
    );

    ( $( $elem:expr ),*) => ({
        let elements = vec![ $( $elem ), * ];

        $crate::ast::KNode::Vec(
            $crate::ast::VecLiteral {
                token: $crate::lexer::token::Token { token_type: TokenType::RBracket, literal: "[".to_string() },
                elements,
            }
        )
    });

    ( $( $elem:expr ),* ,) => ({
        vec_node![ $( $elem ), * ]
    });
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        object::*,
        ast::*,
        lexer::{
            token_type::TokenType,
            token::Token,
        },
    };

    #[test]
    fn test_kint() {
        let int = Integer { value: 32 };
        let expected = Primitive::Int(int.clone());

        assert_eq!(expected, kint!(32));
        assert_eq!(expected, kint!(int.value));
    }

    #[test]
    fn test_kstr() {
        let str = KString { value: "derek".to_string() };
        let expected = Primitive::Str(str.clone());

        assert_eq!(expected, kstr!("derek"));
        assert_eq!(expected, kstr!(str.value));
    }

    #[test]
    fn test_kerr() {
        let expected = Primitive::Error(
            KError {
                value: "arguments to `first` must be ARRAY, got 1".to_string(),
            }
        );

        let actual = kerr!("arguments to `first` must be ARRAY, got 1");

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty_kvec() {
        let expected = KVector {
            elements: Vec::new(),
        };

        let actual = kvec![];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_kvec() {
        let expected = KVector {
            elements: vec![kint!(1), kint!(2), kint!(3)],
        };

        let actual = kvec![
            kint!(1),
            kint!(2),
            kint!(3),
        ];

        assert_eq!(expected, actual);
        assert_eq!(3, actual.elements.len());

        assert_eq!(kint!(1), actual.elements[0]);
        assert_eq!(kint!(2), actual.elements[1]);
        assert_eq!(kint!(3), actual.elements[2]);
    }

    #[test]
    fn test_empty_khash() {
        let expected = KHash {
            pairs: HashMap::new(),
        };

        let actual = khash![];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_khash() {
        let expected = Primitive::Hash(
            KHash {
                pairs: HashMap::from([
                    (HashKey::Int(Integer { value: 1 }), HashPair { key: kint!(1), value: kint!(2) }),
                    (HashKey::Int(Integer { value: 3 }), HashPair { key: kint!(3), value: kint!(4) }),
                ]),
            }
        );

        let actual = khash![
            (kint!(1), kint!(2)),
            (kint!(3), kint!(4)),
        ];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_int_node() {
        let int = IntegerLiteral {
            value: 32,
            token: Token { token_type: TokenType::Int, literal: "32".to_string() }
        };
        let expected = KNode::Int(int.clone());

        assert_eq!(expected, int_node!(32));
        assert_eq!(expected, int_node!(int.value));
    }

    #[test]
    fn test_str_node() {
        let str = StringLiteral {
            value: "foobar".to_string(),
            token: Token { token_type: TokenType::String, literal: "foobar".to_string() }
        };
        let expected = KNode::Str(str.clone());

        assert_eq!(expected, str_node!("foobar"));
        assert_eq!(expected, str_node!(str.value));
    }

    #[test]
    fn test_bool_node() {
        let bool = BooleanLiteral {
            value: true,
            token: Token { token_type: TokenType::True, literal: "true".to_string() }
        };
        let expected = KNode::Bool(bool.clone());

        assert_eq!(expected, bool_node!(true));
        assert_eq!(expected, bool_node!(bool.value));
    }

    #[test]
    fn test_ident_node() {
        let ident = Identifier {
            value: "foobar".to_string(),
            token: Token { token_type: TokenType::Ident, literal: "foobar".to_string() }
        };
        let expected = KNode::Ident(ident.clone());

        assert_eq!(expected, ident_node!("foobar"));
        assert_eq!(expected, ident_node!(ident.value));
    }

    #[test]
    fn test_empty_vec_node() {
        let expected = KNode::Vec(
            VecLiteral {
                token: Token { token_type: TokenType::RBracket, literal: "[".to_string() },
                elements: Vec::new(),
            }
        );

        assert_eq!(expected, vec_node![]);
    }

    #[test]
    fn test_vec_node() {
        let expected = KNode::Vec(
            VecLiteral {
                token: Token { token_type: TokenType::RBracket, literal: "[".to_string() },
                elements: vec![int_node!(1), int_node!(2), int_node!(3)],
            }
        );

        let actual = vec_node![
            int_node!(1),
            int_node!(2),
            int_node!(3),
        ];

        assert_eq!(expected, actual);
        if let KNode::Vec(v) = actual {
            assert_eq!(3, v.elements.len());

            assert_eq!(int_node!(1), v.elements[0]);
            assert_eq!(int_node!(2), v.elements[1]);
            assert_eq!(int_node!(3), v.elements[2]);
        } else {
            panic!("Not a vector node: {}", actual);
        };
    }
}

