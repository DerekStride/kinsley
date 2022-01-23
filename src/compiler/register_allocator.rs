use crate::{
    error::*,
    compiler::code::*,
};

pub struct RegisterAllocator { }

impl RegisterAllocator {
    pub fn allocate(instructions: &mut [Instruction]) -> Result<()> {

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::Ast,
        test_utils::{parse, test_instructions},
        compiler::{
            Compiler,
            Bytecode,
        },
    };

    #[test]
    fn test_register_allocation() -> Result<()> {
        let input = r#"
            let a = 0;
            let b = 1;
            a;
            let c = 2;
            let d = 3;
            b; c; d;
        "#.to_string();
        let expected = vec![
            // let a = 0;
            load!(0, 0),
            set_global!(0, 0),
            // let a = 1;
            load!(1, 1),
            set_global!(1, 1),
            // a;
            get_global!(2, 0),
            // let c = 2;
            load!(0, 2), // Can reuse r0 because a is no longer used.
            set_global!(2, 0),
            // let d = 3;
            load!(2, 3),
            set_global!(3, 2),
        ];


        let program = parse(input)?;
        let mut compiler = Compiler::new();
        compiler.compile(Ast::Prog(program))?;

        let Bytecode { instructions, .. } = compiler.bytecode();
        test_instructions(&expected, &instructions[..9]);

        Ok(())
    }
}
