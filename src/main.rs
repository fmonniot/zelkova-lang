use zelkova_lang::compiler;

fn main() {
    // Will probably need more love than that :p

    compiler::compile_files(vec![
        "std/core/src/Basics.zel".as_ref(),
        "std/core/src/Maybe.zel".as_ref(),
    ]);
}
