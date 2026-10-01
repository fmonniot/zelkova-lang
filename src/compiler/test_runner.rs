//! Running a package's tests.
//!
//! [`run`] is what `zelkova test` calls: it compiles both of a package's roots
//! ([`compile_package_with_tests`]), asks [`test_collection::collect`] which values of the
//! `tests/` modules are tests, writes [`entry_point`]'s text to `build/test/js/run.mjs` and
//! hands that file to `node`. The tests are found and reported on the Zelkova side; the
//! only thing decided in JavaScript is which verdict the value a test evaluated to comes to
//! ([*What a test is*](../../../docs/spec/packages.md#what-a-test-is)).
//!
//! # The entry point
//!
//! [`entry_point`] is the text of `build/test/js/run.mjs`. It holds the list of test
//! modules and the names collected from each, and no test logic beyond that: for each
//! module it `import()`s the file the build emitted and reads each collected export. That
//! value is a `Test` of `zelkova-test` (`std/test/src/Test.zel`), and the entry point reads
//! it by the constructor names that module declares:
//!
//! - `Awaiting` holds a `Task`. The entry point hands it to the runtime's `$runTask`,
//!   awaits the `Test` it produces and judges that one the same way, so a `Task` that
//!   produces another `Awaiting` is run in turn. A run whose promise rejects has aborted,
//!   and the test is reported as errored, carrying the message of what was thrown.
//! - `Pass` is a pass.
//! - `Fail` is a failure. When the `Maybe String` it carries is `Just`, that string is
//!   printed on the test's line, after its name.
//! - Anything else is a failure.
//!
//! Each test is judged to the end before the next one starts, so the tests run one at a
//! time, in the order they are listed. It prints one line per test named
//! `<Module>.<value>`, then a summary, and sets `process.exitCode` to `1` when any test did
//! not pass.
//!
//! The modules are loaded one at a time with a dynamic `import()` and not as static
//! imports, because every parameterless binding is evaluated when its module loads
//! ([*A binding with no parameters is evaluated
//! once*](../../../docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once))
//! and a test is such a binding, so a module whose test aborts fails to load. A static
//! import would take the whole run down with it. An `import()` that rejects instead marks
//! every test collected from that module as errored, saying the module failed to load and
//! carrying the message of what was thrown, and the run moves on to the next module. An
//! errored test counts against the exit code exactly as a failed one does. The runtime,
//! which `run.mjs` imports `$runTask` from, is the one static import: it sits beside
//! `run.mjs` at the root of `build/test/js/` and evaluates no test.
//!
//! A `Task` that never finishes leaves the entry point waiting on a promise nothing
//! settles. When that leaves the event loop empty, `node` ends the process and the entry
//! point's `exit` handler reports the test being run as errored, saying its `Task` never
//! finished, then prints the summary and sets exit code `1`; the tests listed after it do
//! not run and are not counted. A `Task` that keeps the event loop alive for ever, such as
//! a live timer, is not detected: there is no timeout, and the run hangs.
//!
//! The entry point is also coupled to the shape `Maybe` compiles to, which it reads for a
//! `Fail`'s reason: a `$` of `"Just"` and the value in `a`.
//!
//! This phase is the only place the compiler starts `node`.
//!
//! A module that holds no test is left out of the list: there is nothing to attribute a
//! failure to, so loading it could only turn a package with no tests into a failing one.

use std::path::{Path, PathBuf};
use std::process::Command;

use super::name::Name;
use super::test_collection::{self, ModuleTests};
use super::{javascript, PhaseError};
use crate::driver::{compile_package_with_tests, test_tree, BuildError, BUILD_DIRECTORY};

/// The entry point's file name, at the root of `build/test/js/`, beside the runtime.
pub const RUN_FILE: &str = "run.mjs";

/// The program the entry point runs under, looked up on `PATH`.
const NODE: &str = "node";

/// Why a test run could not be carried out, as opposed to a test that ran and did not
/// pass — that one is `node`'s exit code, not an error.
#[derive(Debug)]
pub enum Error {
    /// The entry point could not be written.
    WriteEntryPoint {
        path: PathBuf,
        error: std::io::Error,
    },
    /// `node` could not be started: it is not on `PATH`, or it could not be executed.
    NodeNotStarted { error: std::io::Error },
    /// `node` started and ended without an exit code, which on Unix means a signal
    /// stopped it. The tests did not finish, so the run cannot say how they went.
    NodeTerminated,
}

impl PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::WriteEntryPoint { path, error } => {
                format!("could not write `{}`: {}", path.display(), error)
            }
            Error::NodeNotStarted { error } => {
                format!("could not run `{}`: {}", NODE, error)
            }
            Error::NodeTerminated => {
                format!("`{}` ended before the tests finished", NODE)
            }
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Error::NodeNotStarted { .. } => vec![format!(
                "`zelkova test` runs the compiled tests under `{}`, which it looks for on `PATH`",
                NODE
            )],
            _ => Vec::new(),
        }
    }
}

/// Compile the package rooted at `package_dir` with its tests, run every test it holds
/// under `node`, and answer the exit code the process should end with.
///
/// The answer is `Ok(0)` when every test passed and when the package holds none. It is
/// `node`'s own exit code otherwise, which is non-zero exactly when the entry point saw a
/// test not pass or `node` itself failed. Anything that stops the tests being run — a
/// build that did not compile, `node` missing — is an `Err`.
///
/// `node`'s output goes straight to this process's own stdout and stderr.
pub fn run(package_dir: &Path) -> Result<i32, BuildError> {
    let interfaces = compile_package_with_tests(package_dir)?;
    let modules = test_collection::collect(&interfaces);

    if modules.iter().all(|module| module.tests.is_empty()) {
        println!("no tests found");
        return Ok(0);
    }

    let entry = test_tree(&package_dir.join(BUILD_DIRECTORY)).join(RUN_FILE);
    std::fs::write(&entry, entry_point(&modules)).map_err(|error| {
        BuildError::TestRun(Error::WriteEntryPoint {
            path: entry.clone(),
            error,
        })
    })?;

    let status = Command::new(NODE)
        .arg(&entry)
        .status()
        .map_err(|error| BuildError::TestRun(Error::NodeNotStarted { error }))?;

    status
        .code()
        .ok_or(BuildError::TestRun(Error::NodeTerminated))
}

/// The text of `run.mjs` for `modules`, which sits at the root of `build/test/js/`: see
/// this module's documentation for what it does.
///
/// Every specifier it imports is relative to that root: the runtime's is
/// [`javascript::RUNTIME_FILE`], and each test module's is built by
/// [`javascript::module_file`], so that each names the file the build wrote.
pub fn entry_point(modules: &[ModuleTests]) -> String {
    let entries: Vec<String> = modules
        .iter()
        .filter(|module| !module.tests.is_empty())
        .map(|module| {
            let file = Path::new(module.module.package().as_str())
                .join(javascript::module_file(module.module.name()));
            let names: Vec<String> = module
                .tests
                .iter()
                .map(|test: &Name| string_literal(test.as_str()))
                .collect();
            format!(
                "  {{ module: {}, file: {}, tests: [{}] }},\n",
                string_literal(module.module.name().as_str()),
                string_literal(&format!("./{}", slashed(&file))),
                names.join(", ")
            )
        })
        .collect();

    format!(
        "{}import {{ $runTask }} from {};\n\n{}{}{}",
        HEAD,
        string_literal(&format!("./{}", javascript::RUNTIME_FILE)),
        MODULES,
        entries.concat(),
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
// Generated by `zelkova test`. Do not edit: the next run writes it again.
";

const MODULES: &str = "\
const modules = [
";

const TAIL: &str = "\
];

let passed = 0;
let failed = 0;
let errored = 0;

function summarize() {
  const total = passed + failed + errored;
  console.log(`${total} ${total === 1 ? \"test\" : \"tests\"}: ${passed} passed, ${failed} failed, ${errored} errored`);
}

// The test whose `Task` is being run, and null once it has been judged. Node ends a process
// whose top-level `await` can never be woken, and it runs this handler before it does, so
// a test still named here is one whose `Task` never finished.
let inFlight = null;
process.on(\"exit\", () => {
  if (inFlight !== null) {
    console.log(`ERROR ${inFlight}: its Task never finished`);
    errored += 1;
    summarize();
    process.exitCode = 1;
  }
});

for (const { module, file, tests } of modules) {
  let loaded;
  try {
    loaded = await import(file);
  } catch (error) {
    // Evaluating a module evaluates every binding it declares without parameters, so a
    // test that aborts takes its whole module with it. Say so for each of its tests.
    const message = error instanceof Error ? error.message : String(error);
    for (const test of tests) {
      console.log(`ERROR ${module}.${test}: module failed to load: ${message}`);
      errored += 1;
    }
    continue;
  }
  for (const test of tests) {
    let value = loaded[test];
    inFlight = `${module}.${test}`;
    try {
      // A `Test` that holds a `Task` is judged by the `Test` the `Task` produces, which may
      // hold another `Task`. Awaiting one before starting the next keeps the tests running
      // one at a time, in the order they are listed.
      while (value !== undefined && value !== null && value.$ === \"Awaiting\") {
        value = await $runTask(value.a);
      }
    } catch (error) {
      // A rejected run is an abort, not a verdict.
      const message = error instanceof Error ? error.message : String(error);
      console.log(`ERROR ${module}.${test}: ${message}`);
      errored += 1;
      inFlight = null;
      continue;
    }
    inFlight = null;
    if (value !== undefined && value !== null && value.$ === \"Pass\") {
      console.log(`pass  ${module}.${test}`);
      passed += 1;
    } else if (value !== undefined && value !== null && value.$ === \"Fail\" && value.a.$ === \"Just\") {
      console.log(`FAIL  ${module}.${test}: ${value.a.a}`);
      failed += 1;
    } else {
      console.log(`FAIL  ${module}.${test}`);
      failed += 1;
    }
  }
}

summarize();
if (failed + errored > 0) {
  process.exitCode = 1;
}
";

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::{ModuleName, PackageName};

    fn module(package: &str, name: &str, tests: &[&str]) -> ModuleTests {
        ModuleTests {
            module: ModuleName::new(PackageName::new(package).unwrap(), Name::new(name)),
            tests: tests.iter().map(|test| Name::new(*test)).collect(),
        }
    }

    /// Two test modules holding three tests between them, one of them nested, and a third
    /// module holding none: the text names each module's emitted file, in the layout
    /// `javascript::module_file` writes, and each collected export — and the empty module
    /// does not appear at all.
    ///
    /// Mutation-checked by changing `entry_point`'s filter to keep every module: the empty
    /// module's file then appears and the last assertion goes red.
    #[test]
    fn the_entry_point_lists_each_test_module_and_its_exports() {
        let text = entry_point(&[
            module("acme", "AppTest", &["addsUp", "subtracts"]),
            module("acme", "Deep.ListTest", &["empty"]),
            module("acme", "HelpersOnly", &[]),
        ]);

        assert!(
            text.contains(
                "{ module: \"AppTest\", file: \"./acme/AppTest.mjs\", tests: [\"addsUp\", \"subtracts\"] },"
            ),
            "{}",
            text
        );
        assert!(
            text.contains(
                "{ module: \"Deep.ListTest\", file: \"./acme/Deep/ListTest.mjs\", tests: [\"empty\"] },"
            ),
            "{}",
            text
        );
        assert!(!text.contains("HelpersOnly"), "{}", text);
    }

    /// What the generated code does with what it reads, which is the part of the entry
    /// point that is not data: it loads a module by a dynamic `import()` it can catch,
    /// reads each collected export off the loaded module by its name, counts only a `$` of
    /// `"Pass"` as a pass, reports a module that failed to load against each of its tests,
    /// and turns any test not passing into exit code 1.
    ///
    /// Mutation-checked by deleting the `process.exitCode = 1;` line from `TAIL`: the last
    /// assertion goes red. Deleting the `try`/`catch` fails the `catch (error)` one,
    /// comparing `$` against a different tag fails the `value.$` one, and reading the export
    /// by another key fails the `loaded[test]` one.
    #[test]
    fn the_entry_point_reports_and_sets_the_exit_code() {
        let text = entry_point(&[module("acme", "AppTest", &["addsUp"])]);

        assert!(text.contains("await import(file)"), "{}", text);
        assert!(text.contains("let value = loaded[test];"), "{}", text);
        assert!(text.contains("value.$ === \"Pass\""), "{}", text);
        assert!(text.contains("catch (error)"), "{}", text);
        assert!(
            text.contains(
                "console.log(`ERROR ${module}.${test}: module failed to load: ${message}`);"
            ),
            "{}",
            text
        );
        assert!(
            text.contains("if (failed + errored > 0) {\n  process.exitCode = 1;\n}"),
            "{}",
            text
        );
    }

    /// What the generated code does with a `Test` that holds a `Task`: it imports
    /// `$runTask` from the runtime beside it, keeps handing an `Awaiting`'s `Task` to it
    /// and awaiting the `Test` produced until the value is something else, reports a
    /// rejected run as errored, and prints the reason a `Fail` carries in a `Just`.
    ///
    /// The `exit` handler for a `Task` that never finishes is mutation-checked by renaming
    /// its event: the assertion on it goes red.
    ///
    /// Running the text is `tests/js/TestRunnerChecks.mjs`'s.
    ///
    /// Mutation-checked by turning the `while` into an `if`, which fails the loop's
    /// assertion; by dropping `await`, which fails the same one; by deleting the `catch`
    /// branch's `console.log`, which fails the `ERROR` one; by importing from `./runtime.mjs`,
    /// which fails the import one; and by printing the reason from `value.a`, which fails the
    /// last.
    #[test]
    fn the_entry_point_runs_a_task_and_judges_what_it_produces() {
        let text = entry_point(&[module("acme", "AppTest", &["addsUp"])]);

        assert!(
            text.contains("import { $runTask } from \"./zelkova.mjs\";"),
            "{}",
            text
        );
        assert!(
            text.contains(
                "while (value !== undefined && value !== null && value.$ === \"Awaiting\") {\n        value = await $runTask(value.a);\n      }"
            ),
            "{}",
            text
        );
        assert!(
            text.contains("console.log(`ERROR ${module}.${test}: ${message}`);"),
            "{}",
            text
        );
        assert!(
            text.contains("process.on(\"exit\"") && text.contains("its Task never finished"),
            "{}",
            text
        );
        assert!(
            text.contains("value.$ === \"Fail\" && value.a.$ === \"Just\""),
            "{}",
            text
        );
        assert!(
            text.contains("console.log(`FAIL  ${module}.${test}: ${value.a.a}`);"),
            "{}",
            text
        );
    }

    /// A name goes into the text as a string literal, so nothing a name holds can end it.
    #[test]
    fn a_name_is_written_as_a_string_literal() {
        assert_eq!(string_literal("addsUp"), "\"addsUp\"");
        assert_eq!(string_literal("a\"b\\c"), "\"a\\\"b\\\\c\"");
    }
}
