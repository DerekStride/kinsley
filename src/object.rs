use std::{fmt, collections::HashMap};

pub const TRUE: Primitive = Primitive::Bool(Boolean { value: true });
pub const FALSE: Primitive = Primitive::Bool(Boolean { value: false });
pub const NULL: Primitive = Primitive::Null;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct KObject {
    klass: KClass,
}

impl fmt::Display for KObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.klass.name)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
struct KClass {
    name: String,
    instance_vars: Vec<KObject>,
    methods: Vec<KFunction>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Primitive {
    Null,
    Int(Integer),
    Bool(Boolean),
    Str(KString),
    Vec(KVector),
    Hash(KHash),
    Fn(KFunction),
    Closure(KClosure),
    Error(KError),
    Obj(KObject),
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Null => write!(f, "null"),
            Primitive::Int(x) => write!(f, "{}", x),
            Primitive::Bool(x) => write!(f, "{}", x),
            Primitive::Str(x) => write!(f, "{}", x),
            Primitive::Vec(x) => write!(f, "{}", x),
            Primitive::Hash(x) => write!(f, "{}", x),
            Primitive::Fn(x) => write!(f, "{}", x),
            Primitive::Closure(x) => write!(f, "{}", x),
            Primitive::Error(x) => write!(f, "{}", x),
            Primitive::Obj(x) => write!(f, "{}", x),
        }
    }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
pub struct Integer {
    pub value: i128,
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
pub struct Boolean {
    value: bool,
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub struct KString {
    pub value: String,
}

impl fmt::Display for KString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct KVector {
    pub elements: Vec<Primitive>,
}

impl fmt::Display for KVector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements.iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum HashKey {
    Str(KString),
    Bool(Boolean),
    Int(Integer),
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HashKey::Str(x) => write!(f, "{}", x),
            HashKey::Bool(x) => write!(f, "{}", x),
            HashKey::Int(x) => write!(f, "{}", x),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct HashPair {
    pub key: Primitive,
    pub value: Primitive,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct KHash {
    pub pairs: HashMap<HashKey, HashPair>,
}

impl fmt::Display for KHash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pairs.iter()
                .map(|(_, v)| format!("{}: {}", v.key, v.value))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}



#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct KFunction {
    instructions: Vec<u8>,
    num_locals: u8,
    num_params: u8,
}

impl fmt::Display for KFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "CompiledFunction {{\nnum_locals: {},\nnum_params: {}\ninstructions: [\n{:?}]",
            self.num_locals,
            self.num_params,
            &self.instructions,
            // Code::new().format(&self.instructions)
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct KClosure {
    f: KFunction,
    free: Vec<Primitive>,
}

impl fmt::Display for KClosure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Closure {{\nfn: {}\nfree variables: [{}]}}",
            self.f,
            self.free
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub struct KError {
    pub value: String,
}

impl fmt::Display for KError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ERROR: {}", self.value)
    }
}
