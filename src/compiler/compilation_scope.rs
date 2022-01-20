use crate::compiler::{
    code::*,
    emitted_instruction::EmittedInstruction,
};

pub struct CompilationScope {
    pub instructions: Vec<Instruction>,
    registers: Register,

    pub last_emitted_instruction: Option<EmittedInstruction>,
    pub prev_emitted_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            registers: 0,

            last_emitted_instruction: None,
            prev_emitted_instruction: None,
        }
    }

    pub fn emit(&mut self, ins: Instruction) {
        self.set_last_instruction(ins, self.instructions.len());
        self.instructions.push(ins);
    }

    pub fn next_register(&mut self) -> Register {
        let next = self.registers;
        self.registers += 1;

        next
    }

    pub fn last_dest_register(&self) -> Register {
        for ins in self.instructions.iter().rev() {
            if let Some(reg) = ins.last_dest_register() {
                return reg;
            };
        };

        self.registers
    }

    fn set_last_instruction(&mut self, instruction: Instruction, position: usize) {
        std::mem::swap(&mut self.prev_emitted_instruction, &mut self.last_emitted_instruction);
        self.last_emitted_instruction = Some(EmittedInstruction { instruction, position });
    }
}
