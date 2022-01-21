use crate::{
    error::Result,
    compiler::Compiler,
};

pub trait Optimizer {
    fn optimize(&mut self, _: &mut Compiler) -> Result<()>;
}
