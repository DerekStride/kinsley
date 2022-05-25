use std::env;

use kinsley::{
    error::*,
    utils::parse_file,
    compiler::{
        Compiler,
        optimizers::*,
    },
};

const FILE_MSG: &'static str = r#"
Error: file not found.
Usage:
    kinc FILEPATH
    cargo run --bin=kinc -- FILEPATH

Example(s):
    kinc examples/unused_instruction_removal.kin
    cargo run --bin=kinc -- examples/unused_instruction_removal.kin
"#;

fn main() -> Result<()> {
    let args: Vec<String> = env::args()
        .into_iter()
        .collect();

    let filepath = match args.get(1) {
        Some(x) => x,
        None => {
            eprintln!("{}", FILE_MSG);
            return Err(Error::new(FILE_MSG.to_string()))
        },
    };

    let program = parse_file(filepath)?;
    let mut compiler = Compiler::new();
    compiler.compile(program)?;
    compiler.optimize(&mut NumericConstantPropagation::new())?;
    compiler.optimize(&mut UnusedInstructionRemoval::new())?;
    compiler.optimize(&mut RegisterAllocator::new())?;

    println!("{}", compiler.bytecode());
    println!("{:?}", compiler.bytecode().encode());

    Ok(())
}
