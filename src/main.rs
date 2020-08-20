#[macro_use]
extern crate lalrpop_util;

mod compiler;


fn main() {
    // Will probably need more love than that :p
    compiler::compile_file("tests/zelkova/simple_constant.zel");
}
