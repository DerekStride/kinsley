use std::{
    fmt,
    collections::HashMap,
    hash::{Hash, Hasher},
};

use crate::lexer::token::Token;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum KNode {
    Prog(Program),
    Let(LetExpression),
    Return(ReturnExpression),
    Block(BlockExpression),
    Ident(Identifier),
    Int(IntegerLiteral),
    Bool(BooleanLiteral),
    Str(StringLiteral),
    Vec(VecLiteral),
    Hash(HashLiteral),
    Pre(Prefix),
    In(Infix),
    If(IfExpression),
    Fn(FnLiteral),
    Call(FnCall),
    Index(IndexOperation),
}

impl fmt::Display for KNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            KNode::Prog(x) => write!(f, "{}", x),
            KNode::Let(x) => write!(f, "{}", x),
            KNode::Return(x) => write!(f, "{}", x),
            KNode::Block(x) => write!(f, "{}", x),
            KNode::Ident(x) => write!(f, "{}", x),
            KNode::Int(x) => write!(f, "{}", x),
            KNode::Bool(x) => write!(f, "{}", x),
            KNode::Str(x) => write!(f, "{}", x),
            KNode::Vec(x) => write!(f, "{}", x),
            KNode::Hash(x) => write!(f, "{}", x),
            KNode::Pre(x) => write!(f, "{}", x),
            KNode::In(x) => write!(f, "{}", x),
            KNode::If(x) => write!(f, "{}", x),
            KNode::Fn(x) => write!(f, "{}", x),
            KNode::Call(x) => write!(f, "{}", x),
            KNode::Index(x) => write!(f, "{}", x),
        }
    }
}


#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Program {
    pub exprs: Vec<KNode>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            exprs: Vec::new(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.exprs {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

// ============================================================================
// Expressions
// ============================================================================

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct LetExpression {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<KNode>,
}

impl fmt::Display for LetExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} = {};", self.token.literal, self.name, self.value)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct ReturnExpression {
    pub token: Token,
    pub retval: Box<KNode>,
}

impl fmt::Display for ReturnExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {};", self.token.literal, self.retval)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct BlockExpression {
    pub token: Token,
    pub exprs: Vec<KNode>,
}

impl fmt::Display for BlockExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for expr in &self.exprs {
            write!(f, "{}", expr)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i128,
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct VecLiteral {
    pub token: Token,
    pub elements: Vec<KNode>,
}

impl fmt::Display for VecLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        write!(
            f,
            "{}",
            self.elements.iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<String>>()
                .join(", ")
        )?;
        write!(f, "]")
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<KNode, KNode>,
}

impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, _state: &mut H) {
    }
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        write!(
            f,
            "{}",
            self.pairs.iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<String>>()
                .join(", ")
        )?;
        write!(f, "}}")
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<KNode>,
    pub consequence: BlockExpression,
    pub alternative: Option<BlockExpression>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} {{ {} }}", self.condition, self.consequence)?;
        if let Some(a) = &self.alternative {
            write!(f, " else {{ {} }}", a)?;
        };
        Ok(())
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Prefix {
    pub token: Token,
    pub operator: String,
    pub right: Box<KNode>,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Infix {
    pub token: Token,
    pub left: Box<KNode>,
    pub operator: String,
    pub right: Box<KNode>,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct FnLiteral {
    pub token: Token,
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: BlockExpression,
}

impl fmt::Display for FnLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}(", self.token.literal, self.name)?;
        write!(
            f,
            "{}",
            self.params.iter().map(|p| format!("{}", p)).collect::<Vec<String>>().join(", ")
        )?;
        write!(f, ") {{ {} }}", self.body)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct FnCall {
    pub token: Token,
    pub function: Box<KNode>,
    pub args: Vec<KNode>,
}

impl fmt::Display for FnCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.function)?;
        write!(
            f,
            "{})",
            self.args.iter().map(|p| format!("{}", p)).collect::<Vec<String>>().join(", ")
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct IndexOperation {
    pub token: Token,
    pub left: Box<KNode>,
    pub index: Box<KNode>,
}

impl fmt::Display for IndexOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}
