use std::env;

use kinsley::{
    error::*,
    utils::parse_file,
    compiler::{
        Compiler,
        optimizers::UnusedInstructionRemoval,
    },
};

const FILE_MSG: &'static str = r#"
Error: file not found.
Usage:
    cargo run --bin=unused_instruction_removal -- FILEPATH

Example(s):
    cargo run --bin=unused_instruction_removal -- examples/unused_instruction_removal.kin
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

    println!("Before removal:\n{}", compiler.bytecode());

    compiler.optimize(&mut UnusedInstructionRemoval::new())?;

    println!("After removal:\n{}", compiler.bytecode());

    Ok(())
}
