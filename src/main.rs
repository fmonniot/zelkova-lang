use zelkova_lang::compiler;

fn main() {
    env_logger::init();

    // Will need more love than that :p
    if let Err(err) = compiler::compile_package("std/core/src".as_ref()) {
        // `compile_package` renders a diagnostic for every error it accumulated and
        // hands them back as `Many`, so re-printing those here would only repeat what
        // the user just read. Errors raised before the file database exists — package
        // loading — never reach that reporter and would otherwise be silent.
        if !matches!(err, compiler::CompilationError::Many(_)) {
            eprintln!("failure: {:?}", err);
        }

        // A package that does not compile must not look like one that does to whatever
        // called us: a build script, CI, or a future codegen step.
        std::process::exit(1);
    }
}
