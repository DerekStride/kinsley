use crate::{
    error::*,
    ast::*,
    object::*,
    compiler::{
        code::*,
        compilation_scope::CompilationScope,
    },
};

pub mod code;
mod compilation_scope;
mod emitted_instruction;

pub struct Compiler {
    constants: Vec<Primitive>,
    scopes: Vec<CompilationScope>,

    code: Code,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            scopes: vec![CompilationScope::new()],
            code: Code::new(),
        }
    }

    pub fn compile(&mut self, node: KNode) -> Result<()> {
        match node {
            KNode::Prog(prog) => {
                for expr in prog.exprs { self.compile(expr)?; };
            },
            KNode::Int(int) => {
                let literal = kint!(int.value);
                self.constants.push(literal);
                self.emit(Opcode::Load, Opcode::load(0, self.constants.len() as u16));
            },
            x => return Err(Error::new(format!("Compilation not implemented for node: {:?}", x))),
        };

        Ok(())
    }

    fn emit(&mut self, op: Opcode, operands: Operands) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.emit(&self.code, op, operands);
        };
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
