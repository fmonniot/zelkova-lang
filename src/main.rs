use std::path::PathBuf;

use clap::{Parser, Subcommand};

use zelkova_lang::{compiler, driver};

/// `zelkova` — the compiler's command line.
#[derive(Parser)]
#[command(name = "zelkova")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile a package: the directory holding its `zelkova.toml`.
    Compile {
        /// The package root. Defaults to the current directory.
        #[arg(default_value = ".")]
        dir: PathBuf,
    },
    /// Compile a package and run its `main` under `node`.
    Run {
        /// The package root. Defaults to the current directory.
        #[arg(default_value = ".")]
        dir: PathBuf,
    },
    /// Compile a package and its tests, then run every test the package holds under `node`.
    Test {
        /// The package root. Defaults to the current directory.
        #[arg(default_value = ".")]
        dir: PathBuf,
    },
}

fn main() {
    env_logger::init();

    let cli = Cli::parse();

    let result = match cli.command {
        Command::Compile { dir } => driver::compile_package(&dir).map(|()| 0),
        Command::Run { dir } => compiler::program_runner::run(&dir),
        Command::Test { dir } => compiler::test_runner::run(&dir),
    };

    // `run` answers the code `node` ended with, which is non-zero when a test did not
    // pass or a program aborted. Neither is an error to report: the entry point printed it
    // already.
    match result {
        Ok(0) => {}
        Ok(code) => std::process::exit(code),
        Err(err) => fail(err),
    }
}

/// Report `err` and end the process with a failing exit code.
fn fail(err: driver::BuildError) -> ! {
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
    if !matches!(err, driver::BuildError::Many(_)) {
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
