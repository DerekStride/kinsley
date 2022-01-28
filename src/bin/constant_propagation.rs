use std::env;

use kinsley::{
    error::*,
    utils::parse_file,
    compiler::{
        Compiler,
        optimizers::NumericConstantPropagation,
    },
};

const FILE_MSG: &'static str = r#"
Error: file not found.
Usage:
    cargo run --bin=constant_propagation -- FILEPATH

Example(s):
    cargo run --bin=constant_propagation -- examples/constant_propagation.kin
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

    println!("Before Constant Propagation:\n{}", compiler.bytecode());

    compiler.optimize(&mut NumericConstantPropagation::new())?;

    println!("After Constant Propagation:\n{}", compiler.bytecode());

    Ok(())
}

