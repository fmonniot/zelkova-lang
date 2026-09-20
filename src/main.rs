use zelkova_lang::compiler;

fn main() {
    env_logger::init();

    // Will need more love than that :p
    if let Err(err) = compiler::compile_package("std/core".as_ref()) {
        // `compile_package` renders a diagnostic for every error it accumulated and
        // hands them back as `Many`, so re-printing those here would only repeat what
        // the user just read. Errors raised before the file database exists — the
        // manifest, and package loading — never reach that reporter and would otherwise
        // be silent.
        //
        // They are shown through `as_diagnostic` rather than `{:?}`: a `Debug` dump names
        // Rust types where the user needs the sentence `message()` already writes. Only
        // the headline and the notes are printed by hand, because emitting the diagnostic
        // properly needs the `Files` database these errors are raised before.
        if !matches!(err, compiler::CompilationError::Many(_)) {
            let diagnostic = err.as_diagnostic();
            eprintln!("error: {}", diagnostic.message);
            for note in &diagnostic.notes {
                eprintln!("  {}", note);
            }
        }

        // A package that does not compile must not look like one that does to whatever
        // called us: a build script, CI, or a future codegen step.
        std::process::exit(1);
    }
}
