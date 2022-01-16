use crate::compiler::{
    code::*,
    emitted_instruction::EmittedInstruction,
};

pub struct CompilationScope {
    instructions: Vec<Instruction>,

    last_emitted_instruction: Option<EmittedInstruction>,
    prev_emitted_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),

            last_emitted_instruction: None,
            prev_emitted_instruction: None,
        }
    }

    pub fn emit(&mut self, code: &Code, opcode: Opcode, operands: Operands) {
        let ins = code.make(opcode, &operands);
        self.set_last_instruction(opcode, self.instructions.len());
        self.instructions.push(ins);
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        std::mem::swap(&mut self.prev_emitted_instruction, &mut self.last_emitted_instruction);
        self.last_emitted_instruction = Some(EmittedInstruction { opcode, position });
    }
}
