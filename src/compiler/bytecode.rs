use std::fmt;

use crate::{
    object::Primitive,
    compiler::code::Instruction,
};

pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Primitive>,
}

impl Bytecode {
    pub fn format_instructions(ins: &Vec<Instruction>) -> String {
        ins
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn format_constants(con: &Vec<Primitive>) -> String {
        con
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join("\n")
    }
}
impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Bytecode:\n\nInstructions:\n{}\n\nConstants:\n{}\n",
            Bytecode::format_instructions(&self.instructions),
            Bytecode::format_constants(&self.constants),
        )
    }
}

