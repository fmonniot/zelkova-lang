#[macro_use]
extern crate lalrpop_util;

mod compiler;


fn main() {
    // Will probably need more love than that :p
    compiler::compile_file("std/core/src/Maybe.zel");
}
