use zelkova_lang::compiler;

fn main() {
    // Will probably need more love than that :p

    match compiler::compile_package("std/core/src".as_ref()) {
        Ok(ok) => println!("success: {:?}", ok),
        Err(err) => println!("failure: {:?}", err),
    };
}
