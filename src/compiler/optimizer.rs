use crate::{
    object::Primitive,
    compiler::code::{
        Instruction,
        Instruction::*,
    },
};

#[derive(Debug)]
pub struct Change {
    pub instructions_to_replace: Vec<(usize, Instruction)>,
    pub instructions_to_remove: Vec<usize>,

    pub constants_to_replace: Vec<(usize, Primitive)>,
    pub constants_to_remove: Vec<usize>,
}


pub trait Optimizer {
    fn optimize(&mut self, current_index: usize, instructions: &[Instruction], constants: &[Primitive]) -> Option<Change>;
}

// When we remove constants from the constant pool we need to iterate over the instructions and if
// they reference a constant we need to compute it's new location in the constant pool.
pub fn remap_constants(instructions: &mut [Instruction], constants: &mut Vec<Primitive>, constants_to_remove: &[usize]) {
    let mut removed_constants = Vec::new();
    for idx in constants_to_remove {
        constants.remove(*idx - removed_constants.len());
        removed_constants.push(*idx);
    };

    let mut index = 0;

    while index < instructions.len() {
        let pos = index;
        index += 1;

        let (dest, constant) = match instructions[pos] {
            Load { dest, constant } => (dest, constant),
            _ => continue,
        };

        let shift_count = removed_constants
            .iter()
            .fold(0, |acc, x| {
                if *x < (constant as usize) {
                    acc + 1
                } else {
                    acc
                }
            });

        if (constant as usize) < shift_count { continue; };

        instructions[pos] = load!(dest, constant - shift_count as u16);
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remap_constants() {
        let constants_to_remove = [1, 3];
        let mut constants = vec![
            kint!(3),       // 0
            kint!(-4),      // 1 **remove**
            kint!(6),       // 2
            kint!(8),       // 3 **remove**
            kint!(-123),    // 4
        ];
        let mut instructions = [
            load!(0, 0),
            load!(1, 2),
            load!(2, 4),
        ];

        remap_constants(&mut instructions, &mut constants, &constants_to_remove);

        assert_eq!(vec![kint!(3), kint!(6), kint!(-123)], constants);
        assert_eq!([load!(0, 0), load!(1, 1), load!(2, 2)], instructions);
    }
}
