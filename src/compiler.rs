use crate::{
    error::*,
    ast::*,
};

pub mod code;

pub struct Compiler  {}

impl Compiler {
    pub fn new() -> Self {
        Self { }
    }

    pub fn compile(&mut self, _node: KNode) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler() {
        let mut c = Compiler::new();
        assert!(c.compile(KNode::Prog(Program { exprs: Vec::new() })).is_ok());
    }
}
