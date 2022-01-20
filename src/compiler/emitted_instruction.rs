use std::fmt;
use crate::compiler::code::*;

#[derive(Debug)]
pub struct EmittedInstruction {
    pub instruction: Instruction,
    pub position: usize,
}

impl fmt::Display for EmittedInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:0>4} {}",
            self.position,
            self.instruction,
        )
    }
}

