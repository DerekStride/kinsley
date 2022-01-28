use std::env;

use kinsley::{
    error::*,
    utils::parse_file,
    compiler::{
        Compiler,
        LiveRanges,
        optimizers::RegisterAllocator,
    },
};

const FILE_MSG: &'static str = r#"
Error: file not found.
Usage:
    cargo run --bin=register_reallocator -- FILEPATH

Example(s):
    cargo run --bin=register_reallocator -- examples/register_reallocator.kin
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

    let live_ranges = LiveRanges::from(compiler.bytecode().instructions.as_slice());
    println!("Before reassignment:\n{}", live_ranges);

    compiler.optimize(&mut RegisterAllocator::new())?;

    let live_ranges = LiveRanges::from(compiler.bytecode().instructions.as_slice());
    println!("After reassignment:\n{}", live_ranges);

    Ok(())
}
