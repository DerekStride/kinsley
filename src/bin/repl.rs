use std::io;

use kinsley::repl::{start, Engine};

fn main() {
    let input = io::stdin();
    let mut output = io::stdout();

    let mut engine = Engine::vm();

    start(input, &mut output, &mut engine).unwrap();
}
