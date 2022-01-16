use std::fmt;
use crate::compiler::code::*;

#[derive(Debug)]
pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub position: usize,
}

impl fmt::Display for EmittedInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let code = Code::new();
        let def = code.lookup(self.opcode).unwrap();

        write!(
            f,
            "{:0>4} {}",
            self.position,
            def.name,
        )
    }
}

