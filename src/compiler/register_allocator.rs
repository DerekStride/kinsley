use crate::{
    error::*,
    compiler::{
        code::*,
        LiveRanges,
    },
};

pub struct RegisterAllocator { }

impl RegisterAllocator {
    pub fn allocate(instructions: &mut [Instruction]) -> Result<()> {
        let live_ranges = LiveRanges::from(instructions.as_ref());
        println!("{}", live_ranges);

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
            a + b;
            let c = 2;
            let d = 3;
            b + c;
            c + d;
        "#.to_string();
        let program = parse(input)?;
        let mut compiler = Compiler::new();
        compiler.compile(Ast::Prog(program))?;

        let Bytecode { mut instructions, .. } = compiler.bytecode();
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

        test_instructions(&original, &instructions);

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
            add!(1, 3, 4), // Can reuse r1 because b is no longer used.
        ];

        RegisterAllocator::allocate(&mut instructions)?;
        test_instructions(&reallocated, &instructions);

        Ok(())
    }
}
