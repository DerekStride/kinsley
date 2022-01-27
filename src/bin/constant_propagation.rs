use std::{
    env,
    fs::File,
    io::{
        Read,
        BufReader,
    },
};

use kinsley::{
    ast::Ast,
    error::*,
    utils::parse_stream,
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

    let file = File::open(filepath)?;
    let buf_reader = BufReader::new(file);
    let stream = buf_reader
        .bytes()
        .map(std::result::Result::unwrap)
        .peekable();

    let program = parse_stream(stream)?;
    let mut compiler = Compiler::new();
    compiler.compile(Ast::Prog(program))?;

    println!("Before Constant Propagation:\n{}", compiler.bytecode());

    compiler.optimize(&mut NumericConstantPropagation::new())?;

    println!("After Constant Propagation:\n{}", compiler.bytecode());

    Ok(())
}

