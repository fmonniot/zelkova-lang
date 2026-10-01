//! Running a package's program.
//!
//! [`run`] is what `zelkova run` calls: it reads the package's manifest, compiles the
//! package ([`compile_package`]), writes [`entry_point`]'s text to `build/out/js/main.mjs`
//! and hands that file to `node`
//! ([*The compiler's interface*](../../../docs/spec/toolchain.md#the-compilers-interface)).
//! `zelkova compile` does not write the entry point: it is the one file of `build/out/js/`
//! that is not a module of the build, and only the command that runs it needs it. Every
//! build prunes what it did not write, so the file is written again by each run.
//!
//! # The entry point
//!
//! [`entry_point`] is the text of `main.mjs`, which sits at the root of `build/out/js/`
//! beside the runtime. It imports the runtime's `$runTask` and `import()`s the file the
//! build emitted for the manifest's `main` module, then hands the module's `main` export to
//! `$runTask` and waits for it. Anything that rejects — the run, or the evaluation of the
//! module while it loads — is an
//! [abort](../../../docs/spec/evaluation-semantics.md#when-a-program-aborts): the entry point
//! writes `aborted: ` and the message of what was thrown to standard error and sets
//! `process.exitCode` to `1`.
//!
//! The module is loaded with a dynamic `import()` and not a static import, because every
//! parameterless binding is evaluated when its module loads
//! ([*A binding with no parameters is evaluated
//! once*](../../../docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once)),
//! so a module can abort as it loads, and only an `import()` lets that be reported the way
//! any other abort is. The runtime is the one static import.
//!
//! A `Task` that never finishes leaves the entry point waiting on a promise nothing
//! settles. When that leaves the event loop empty, `node` ends the process and the entry
//! point's `exit` handler says the `Task` never finished and sets exit code `1`. `node`
//! writes its own `Warning: Detected unsettled top-level await` to standard error first, and
//! whether it does, and how it words it, is `node`'s and varies by version. A `Task`
//! that keeps the event loop alive for ever, such as a live timer, is not detected.
//!
//! This phase and [`test_runner`](super::test_runner) are the only places the compiler
//! starts `node`.

use std::path::{Path, PathBuf};
use std::process::Command;

use super::name::Name;
use super::{javascript, manifest, CompilationError, PackageName, PhaseError};
use crate::driver::{compile_package, BUILD_DIRECTORY};

/// The entry point's file name, at the root of `build/out/js/`, beside the runtime.
pub const MAIN_FILE: &str = "main.mjs";

/// The program the entry point runs under, looked up on `PATH`.
const NODE: &str = "node";

/// Why a program could not be run, as opposed to a program that ran and aborted — that
/// one is `node`'s exit code, not an error.
#[derive(Debug)]
pub enum Error {
    /// The manifest has no `main`, so the package is a library and there is nothing to
    /// run.
    NoMain { package: PackageName },
    /// The entry point could not be written.
    WriteEntryPoint {
        path: PathBuf,
        error: std::io::Error,
    },
    /// `node` could not be started: it is not on `PATH`, or it could not be executed.
    NodeNotStarted { error: std::io::Error },
    /// `node` started and ended without an exit code, which on Unix means a signal
    /// stopped it. The program did not finish, so the run cannot say how it went.
    NodeTerminated,
}

impl PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::NoMain { package } => {
                format!(
                    "package `{}` has no `main`, so there is nothing to run",
                    package
                )
            }
            Error::WriteEntryPoint { path, error } => {
                format!("could not write `{}`: {}", path.display(), error)
            }
            Error::NodeNotStarted { error } => {
                format!("could not run `{}`: {}", NODE, error)
            }
            Error::NodeTerminated => {
                format!("`{}` ended before the program finished", NODE)
            }
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Error::NoMain { .. } => vec![
                "a package is a program when its `zelkova.toml` names the module holding \
                 `main` in a `main` field"
                    .to_string(),
            ],
            Error::NodeNotStarted { .. } => vec![format!(
                "`zelkova run` runs the compiled program under `{}`, which it looks for on `PATH`",
                NODE
            )],
            _ => Vec::new(),
        }
    }
}

/// Compile the package rooted at `package_dir`, run its `main` under `node`, and answer
/// the exit code the process should end with.
///
/// The answer is `Ok(0)` when the program's `Task` completed and `node`'s own exit code
/// otherwise, which is non-zero when the program aborted. Anything that stops the program
/// being run — a manifest without `main`, a build that did not compile, `node` missing —
/// is an `Err`, and in each of those `node` is never started. The manifest is read first,
/// so a package with no `main` is reported as that and not compiled.
///
/// `node`'s output goes straight to this process's own stdout and stderr.
pub fn run(package_dir: &Path) -> Result<i32, CompilationError> {
    let manifest = manifest::load(package_dir)?;
    let Some(main) = manifest.main.clone() else {
        return Err(CompilationError::ProgramRun(Error::NoMain {
            package: manifest.name,
        }));
    };

    compile_package(package_dir)?;

    let js_root = package_dir.join(BUILD_DIRECTORY).join("out").join("js");
    let entry = js_root.join(MAIN_FILE);
    std::fs::write(&entry, entry_point(&manifest.name, &main)).map_err(|error| {
        CompilationError::ProgramRun(Error::WriteEntryPoint {
            path: entry.clone(),
            error,
        })
    })?;

    let status = Command::new(NODE)
        .arg(&entry)
        .status()
        .map_err(|error| CompilationError::ProgramRun(Error::NodeNotStarted { error }))?;

    status
        .code()
        .ok_or(CompilationError::ProgramRun(Error::NodeTerminated))
}

/// The text of `main.mjs` for the program whose `main` is exposed by the module `main` of
/// `package`, which sits at the root of `build/out/js/`: see this module's documentation for
/// what it does.
///
/// Both specifiers it imports are relative to that root: the runtime's is
/// [`javascript::RUNTIME_FILE`], and the module's is built by [`javascript::module_file`]
/// below the package's own directory, so that each names the file the build wrote.
pub fn entry_point(package: &PackageName, main: &Name) -> String {
    let file = Path::new(package.as_str()).join(javascript::module_file(main));
    format!(
        "{}import {{ $runTask }} from {};\n\nconst file = {};\n{}",
        HEAD,
        string_literal(&format!("./{}", javascript::RUNTIME_FILE)),
        string_literal(&format!("./{}", slashed(&file))),
        TAIL
    )
}

/// `path` with `/` between its segments, as an import specifier is written on every
/// platform.
fn slashed(path: &Path) -> String {
    path.components()
        .map(|component| component.as_os_str().to_string_lossy().into_owned())
        .collect::<Vec<_>>()
        .join("/")
}

/// A JavaScript string literal holding `text` and nothing else.
fn string_literal(text: &str) -> String {
    let mut literal = String::from("\"");
    for c in text.chars() {
        match c {
            '"' => literal.push_str("\\\""),
            '\\' => literal.push_str("\\\\"),
            '\n' => literal.push_str("\\n"),
            '\r' => literal.push_str("\\r"),
            c if c.is_control() || c == '\u{2028}' || c == '\u{2029}' => {
                literal.push_str(&format!("\\u{{{:x}}}", c as u32))
            }
            c => literal.push(c),
        }
    }
    literal.push('"');
    literal
}

const HEAD: &str = "\
// Generated by `zelkova run`. Do not edit: the next run writes it again.
";

const TAIL: &str = "\
let finished = false;
// Node ends a process whose top-level `await` can never be woken, and it runs this handler
// before it does, so a program that gets here unfinished is one whose `Task` never finished.
process.on(\"exit\", () => {
  if (!finished) {
    console.error(\"aborted: the program's Task never finished\");
    process.exitCode = 1;
  }
});

try {
  const { main } = await import(file);
  await $runTask(main);
} catch (error) {
  // A rejected run, or a module that aborted as it loaded, is an abort.
  const message = error instanceof Error ? error.message : String(error);
  console.error(`aborted: ${message}`);
  process.exitCode = 1;
}
finished = true;
";

#[cfg(test)]
mod tests {
    use super::*;

    fn text(package: &str, main: &str) -> String {
        entry_point(&PackageName::new(package).unwrap(), &Name::new(main))
    }

    /// The text names the runtime and the emitted file of the `main` module, in the layout
    /// `javascript::module_file` writes: below the package's directory, one directory per
    /// segment of a nested module's name but the last.
    ///
    /// Mutation-checked by dropping the package directory from the path in `entry_point`:
    /// the specifier then names `./Deep/App.mjs` and the last assertion goes red.
    #[test]
    fn the_entry_point_imports_the_runtime_and_the_main_module_file() {
        let text = text("acme", "Deep.App");

        assert!(
            text.contains("import { $runTask } from \"./zelkova.mjs\";"),
            "{}",
            text
        );
        assert!(
            text.contains("const file = \"./acme/Deep/App.mjs\";"),
            "{}",
            text
        );
    }

    /// The text hands the module's `main` export to `$runTask`, and reports a rejection by
    /// writing to standard error and setting a non-zero exit code.
    ///
    /// Mutation-checked by deleting the `process.exitCode = 1;` line from the `catch` in
    /// `TAIL`: the exit-code assertion then goes red.
    #[test]
    fn the_entry_point_runs_main_and_reports_an_abort() {
        let text = text("acme", "App");

        assert!(text.contains("await $runTask(main);"), "{}", text);
        assert!(
            text.contains(
                "console.error(`aborted: ${message}`);\n  process.exitCode = 1;\n}\nfinished = true;"
            ),
            "{}",
            text
        );
    }
}
