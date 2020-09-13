#[macro_use]
extern crate lalrpop_util;

mod compiler;

fn main() {
    // Will probably need more love than that :p

    compiler::compile_files(vec![
        "std/core/src/Maybe.zel".as_ref(),
    ]);
}
