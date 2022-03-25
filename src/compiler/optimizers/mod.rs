mod numeric_constant_propagation;
mod register_allocator;
mod unused_instruction_removal;

pub type NumericConstantPropagation = numeric_constant_propagation::NumericConstantPropagation;
pub type RegisterAllocator = register_allocator::RegisterAllocator;
pub type UnusedInstructionRemoval = unused_instruction_removal::UnusedInstructionRemoval;
