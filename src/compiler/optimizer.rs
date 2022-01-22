use crate::{
    object::Primitive,
    compiler::code::Instruction,
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
