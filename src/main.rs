#[macro_use]
extern crate lalrpop_util;

mod frontend;
mod parser;
mod position;
mod tokenizer;

// TODO Use anyhow and make main return a Result
fn main() {
    let source =
        std::fs::read_to_string("tests/zelkova/simple_constant.zel").expect("Can't read file");

    // This is the first pass of our "compiler"
    let tokenizer = tokenizer::make_tokenizer(&source);
    let tokens: Vec<_> = tokenizer.collect();
    println!("tokens: {:?}", tokens);

    // next passes
    // parser

    // desugar
    // type check
    // core (first IR)

    // then depending on the program type
    // evaluate (repl)
    // web assembly (emitted)
}
