use std::ops::RangeInclusive;

use crate::{
    object::Primitive,
    compiler::{
        code::*,
        LiveRanges,
        optimizer::Optimizer,
    },
};

#[derive(Debug)]
struct Update {
    old_register: Register,
    new_register: Register,
    range: RangeInclusive<usize>,
}

pub struct RegisterAllocator {
    live_ranges: Option<LiveRanges>,
    updates: Vec<Update>,
}

impl RegisterAllocator {
    pub fn new() -> Self {
        Self {
            live_ranges: None,
            updates: Vec::new()
        }
    }
}

impl Optimizer for RegisterAllocator {
    fn optimize(&mut self, current_index: usize, instructions: &mut [Instruction], _constants: &mut Vec<Primitive>) {
        if self.live_ranges.is_none() {
            self.live_ranges = Some(LiveRanges::from(instructions.as_ref()));
        };
        let live_ranges = self.live_ranges.as_mut().unwrap();

        let ins = instructions[current_index];
        let old_register = match ins.last_dest_register() {
            Some(x) => x,
            None => return,
        };
        let (new_register, range) = match live_ranges.reassign(old_register, current_index) {
            Some(x) => x,
            None => return,
        };

        self.updates.push(Update { old_register, new_register, range });
    }

    fn finalize(&mut self, instructions: &mut Vec<Instruction>, _constants: &mut Vec<Primitive>) {
        for update in self.updates.iter_mut() {
            for idx in &mut update.range {
                match instructions[idx].try_replace_register(update.old_register, update.new_register) {
                    Some(ins) => instructions[idx] = ins,
                    _ => continue,
                };
            };
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
            add!(2, 0, 1),
            // let c = 2;
            load!(3, 2),
            set_global!(2, 3),
            // let d = 3;
            load!(4, 3),
            set_global!(3, 4),
            // b + c;
            add!(5, 1, 3),
            // c + d;
            add!(6, 3, 4),
        ];

        test_instructions(&original, &compiler.bytecode().instructions);

        let reallocated = vec![
            // let a = 0;
            load!(0, 0),
            set_global!(0, 0),
            // let b = 1;
            load!(1, 1),
            set_global!(1, 1),
            // a + b;
            add!(2, 0, 1),
            // let c = 2;
            load!(0, 2), // Can reuse r0 because 'a' is no longer used.
            set_global!(2, 0),
            // let d = 3;
            load!(2, 3), // Can reuse r2 because the result of a + b is no longer used.
            set_global!(3, 2),
            // b + c;
            add!(3, 1, 0),
            // c + d;
            add!(1, 0, 2), // Can reuse r1 because b is no longer used.
        ];

        compiler.optimize(&mut RegisterAllocator::new())?;

        test_instructions(&reallocated, &compiler.bytecode().instructions);

        Ok(())
    }

    #[test]
    fn test_simple_allocator() -> Result<()> {
        let input = r#"
            let a = 0;
            let b = 1;
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
        ];

        test_instructions(&original, &compiler.bytecode().instructions);

        let reallocated = vec![
            // let a = 0;
            load!(0, 0),
            set_global!(0, 0),
            // let b = 1;
            load!(0, 1),
            set_global!(1, 0),
        ];

        compiler.optimize(&mut RegisterAllocator::new())?;

        test_instructions(&reallocated, &compiler.bytecode().instructions);

        Ok(())
    }
}
