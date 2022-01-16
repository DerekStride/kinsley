#[macro_export]
macro_rules! kint {
    ($value:literal) => (
        $crate::object::Primitive::Int(
            $crate::object::Integer {
                value: $value
            }
        )
    );
}

#[macro_export]
macro_rules! kstr {
    ($value:literal) => (
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

#[cfg(test)]
mod tests {
    use crate::object::*;
    use std::collections::HashMap;

    #[test]
    fn test_kint() {
        let expected = Primitive::Int(Integer { value: 32 });
        let actual = kint!(32);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_kstr() {
        let expected = Primitive::Str(KString { value: "derek".to_string() });
        let actual = kstr!("derek");

        assert_eq!(expected, actual);
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
}

