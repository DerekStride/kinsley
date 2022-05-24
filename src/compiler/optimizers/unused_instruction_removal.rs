use crate::{
    object::{Integer, Primitive},
    compiler::{
        optimizer::{self, Optimizer},
        code::Instruction,
        LiveRanges,
    },
};

pub struct UnusedInstructionRemoval {
    unused_instructions: Vec<usize>,
    unused_constants: Vec<usize>,
    live_ranges: Option<LiveRanges>,
}

impl UnusedInstructionRemoval {
    pub fn new() -> Self {
        Self {
            unused_instructions: Vec::new(),
            unused_constants: Vec::new(),
            live_ranges: None,
        }
    }
}

impl Optimizer for UnusedInstructionRemoval {
    fn optimize(&mut self, current_index: usize, instructions: &mut [Instruction], constants: &mut Vec<Primitive>) {
        if self.live_ranges.is_none() {
            self.live_ranges = Some(LiveRanges::from(instructions.as_ref()));
        };
        let live_ranges = self.live_ranges.as_mut().unwrap();
        let ins = instructions[current_index];
        let old_register = match ins.last_dest_register() {
            Some(x) => x,
            None => return,
        };

        if live_ranges.is_not_used_again(old_register, current_index) {
            self.unused_instructions.push(current_index);
        };
    }

    fn finalize(&mut self, instructions: &mut Vec<Instruction>, constants: &mut Vec<Primitive>) {
        for idx in self.unused_instructions.iter().rev() {
            instructions.remove(*idx);
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        error::Result,
        ast::Ast,
        utils::{parse, test_instructions},
        compiler::Compiler,
    };

    #[test]
    fn test_register_allocation() -> Result<()> {
        let input = r#"
            let a = 0;
            let b = 1;
            a + b;
            let c = 2;
            let d = 3;
            b + c;
            c + d;
        "#.to_string();
        let program = parse(input)?;
        let mut compiler = Compiler::new();
        compiler.compile(Ast::Prog(program))?;

        let original = vec![
            // let a = 0;
            load!(0, 0),
            set_global!(0, 0),
            // let b = 1;
            load!(1, 1),
            set_global!(1, 1),
            // a + b;
            add!(2, 0, 1), // Unused = **remove**
            // let c = 2;
            load!(3, 2),
            set_global!(2, 3),
            // let d = 3;
            load!(4, 3),
            set_global!(3, 4),
            // b + c;
            add!(5, 1, 3), // Unused = **remove**
            // c + d;
            add!(6, 3, 4), // Unused = **remove**
        ];

        test_instructions(&original, &compiler.bytecode().instructions);

        let expected = vec![
            // let a = 0;
            load!(0, 0),
            set_global!(0, 0),
            // let b = 1;
            load!(1, 1),
            set_global!(1, 1),
            // let c = 2;
            load!(3, 2),
            set_global!(2, 3),
            // let d = 3;
            load!(4, 3),
            set_global!(3, 4),
        ];

        compiler.optimize(&mut UnusedInstructionRemoval::new())?;

        test_instructions(&expected, &compiler.bytecode().instructions);

        Ok(())
    }
}
