//! End-to-end tests of the `zelkova` binary's command line itself — `src/main.rs`'s
//! `Cli`/`Command` wiring — as opposed to `tests/pipeline.rs`, which drives
//! `compile_package` directly and never goes through `main`.
//!
//! Every one of GEN-17's Acceptance clauses is a shell-level check (`cargo run --
//! compile std/core`, a fixture, the bare invocation, `--manifest-path`), verified by
//! hand when that ticket landed. Nothing here duplicates the compiler's own behaviour —
//! `tests/pipeline.rs` and the layers below it already pin that — this file pins only
//! the three things the CLI layer itself adds on top: that `Compile { dir }` defaults
//! to `.`, that the subcommand actually routes to `compile_package`, and that a missing
//! subcommand exits non-zero rather than silently doing nothing.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// The compiled `zelkova` binary, built by Cargo before this test binary runs.
fn zelkova_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_zelkova"))
}

/// Root of one of the small package fixtures under `tests/fixtures/`, the same
/// directories `tests/pipeline.rs::fixture_package` uses.
fn fixture_package(name: &str) -> PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    Path::new(&manifest).join("tests/fixtures").join(name)
}

fn run(cwd: &Path, args: &[&str]) -> Output {
    Command::new(zelkova_bin())
        .args(args)
        .current_dir(cwd)
        .output()
        .expect("failed to run the zelkova binary")
}

/// A bare `zelkova`, with no subcommand, must not silently succeed — it is the
/// no-op a caller (a build script, CI, a future codegen step) needs to see fail.
///
/// Neutralised by widening `Cli::command` from `Command` to `Option<Command>` and
/// mapping `None` to `Ok(())` in `main` — clap then accepts the bare invocation and
/// `main` reports success. This test went red (exit code 0 instead of the expected
/// non-zero, and no "Usage:" on stderr) under that change, confirming it is the
/// `command: Command` field — not something else — that this test pins.
#[test]
fn bare_invocation_exits_non_zero_and_prints_usage() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let output = run(Path::new(&manifest), &[]);

    assert!(
        !output.status.success(),
        "a bare `zelkova` must exit non-zero, got {:?}",
        output.status
    );
    assert!(output.stdout.is_empty(), "must print nothing on stdout");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Usage:"),
        "must print usage on stderr, got: {}",
        stderr
    );
}

/// `Compile { dir }` defaults to `.`, so `zelkova compile` with no directory argument
/// compiles whatever package the current directory holds.
///
/// Neutralised by changing `#[arg(default_value = ".")]` to
/// `#[arg(default_value = "/nonexistent-zelkova-cli-test-dir")]` in `src/main.rs`.
/// This test went red (exit code 1, "No such file or directory" instead of "parsed 1
/// modules" on stderr) under that change, confirming it is that default that makes a
/// `zelkova compile` run from inside `package_checks` compile `package_checks`.
#[test]
fn compile_defaults_to_current_directory() {
    let package_dir = fixture_package("package_checks");
    let output = run(&package_dir, &["compile"]);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "expected success, got {:?}\nstderr: {}",
        output.status,
        stderr
    );
    assert!(
        stderr.contains("parsed 1 modules"),
        "expected the default `.` to compile `package_checks` itself, got: {}",
        stderr
    );

    // Compiling wrote `build/` beside the fixture's manifest; it is gitignored, but
    // clean it up so the fixture tree does not accumulate stale output between runs.
    let _ = std::fs::remove_dir_all(package_dir.join("build"));
}

/// An explicit `zelkova compile DIR` routes `DIR` to `compile_package` — not, say, a
/// hardcoded path left over from before GEN-17 gave the compiler a CLI.
///
/// Neutralised by changing the match arm in `src/main.rs` from
/// `Command::Compile { dir } => compiler::compile_package(&dir)` to
/// `Command::Compile { .. } => compiler::compile_package(Path::new("tests/fixtures/package_type_error"))`
/// — ignoring the parsed `dir` — while running from the repository root with
/// `compile tests/fixtures/package_checks` as the argument. This test went red
/// (exit code 1, an `Int`/`Bool` mismatch on stderr instead of "parsed 1 modules")
/// under that change, confirming it is the parsed `dir` — not a fixed path — that
/// reaches `compile_package`.
#[test]
fn compile_routes_explicit_dir_to_compile_package() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let repo_root = PathBuf::from(&manifest);
    let output = run(&repo_root, &["compile", "tests/fixtures/package_checks"]);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "expected success, got {:?}\nstderr: {}",
        output.status,
        stderr
    );
    assert!(
        stderr.contains("parsed 1 modules"),
        "expected the explicit dir argument to reach compile_package, got: {}",
        stderr
    );

    let _ = std::fs::remove_dir_all(repo_root.join("tests/fixtures/package_checks/build"));
}

/// A package that fails to compile makes `zelkova compile` exit non-zero, with the
/// diagnostic on stderr — this is `compile_package`'s own contract
/// (`tests/pipeline.rs`'s `compile_package reports failure when a module fails`), but
/// nothing before this file pinned that `main` actually surfaces it through the exit
/// code rather than, say, swallowing the `Err` and exiting 0.
///
/// Neutralised by changing `std::process::exit(1)` to `std::process::exit(0)` at the
/// end of `fail` in `main.rs`. This test went red (exit code 0 instead of the expected non-zero)
/// under that change, confirming it is that call — not clap or `compile_package`
/// itself — that this test pins.
#[test]
fn compile_failure_exits_non_zero_with_diagnostic() {
    let package_dir = fixture_package("package_type_error");
    let output = run(&package_dir, &["compile"]);

    assert!(
        !output.status.success(),
        "a package that fails to compile must exit non-zero, got {:?}",
        output.status
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("cannot match"),
        "expected the type error's diagnostic on stderr, got: {}",
        stderr
    );
}

/// A `PATH` naming a directory that does not exist, so that no `node` is found however the
/// machine is set up — and so that no test in this file can run one. `cargo test` never
/// invokes `node` ([`DEC-18` decision 6](../docs/decisions/dec-18.md)).
const NO_NODE_PATH: &str = "/nonexistent-zelkova-cli-test-path";

/// Like [`run`], with `PATH` set to [`NO_NODE_PATH`]. The binary is started by its absolute
/// path, so the missing `PATH` only affects what the binary itself looks up.
fn run_without_node(cwd: &Path, args: &[&str]) -> Output {
    Command::new(zelkova_bin())
        .args(args)
        .current_dir(cwd)
        .env("PATH", NO_NODE_PATH)
        .output()
        .expect("failed to run the zelkova binary")
}

/// `zelkova test` on a package that does not compile exits 1 and runs nothing: the build's
/// diagnostic is on stderr, no entry point was written, and — because the run happens with
/// no `node` reachable — an attempt to start one would have shown up as an error naming it.
///
/// Neutralised by replacing the `?` after `compile_package_with_tests(package_dir)` in
/// `test_runner::run` with `.unwrap_or_default()`, so a failed build carries on with nothing
/// collected. This test went red (exit code 0 and "no tests found") under that change.
#[test]
fn test_on_a_package_that_does_not_compile_exits_1_without_running_node() {
    let package_dir = fixture_package("package_type_error");
    let output = run_without_node(&package_dir, &["test"]);

    assert_eq!(output.status.code(), Some(1), "{:?}", output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("cannot match"),
        "expected the type error's diagnostic on stderr, got: {}",
        stderr
    );
    assert!(
        !stderr.contains("`node`"),
        "a package that does not compile must not reach the `node` step, got: {}",
        stderr
    );
    assert!(!package_dir.join("build/test/js/run.mjs").exists());
}

/// A package that holds no test says so and exits 0 — it has not failed any — without
/// `node`: with none reachable, the run still succeeds.
///
/// Neutralised by deleting the `if modules.iter().all(..)` early return in
/// `test_runner::run`. This test went red (the run wrote an entry point and failed to find
/// `node`) under that change.
#[test]
fn test_on_a_package_with_no_tests_exits_0_without_running_node() {
    let package_dir = fixture_package("package_no_tests");
    let output = run_without_node(&package_dir, &["test"]);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "expected success, got {:?}\nstderr: {}",
        output.status,
        stderr
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "no tests found\n");

    let _ = std::fs::remove_dir_all(package_dir.join("build"));
}

/// When `node` cannot be started, `zelkova test` says so by name and exits non-zero. It
/// never reports success for tests that did not run.
///
/// Run from inside the fixture with no directory argument, so this also pins that
/// `Test { dir }` defaults to `.`. The fixture holds two tests, so the run does reach the
/// `node` step.
///
/// Neutralised by replacing the `map_err(..)?` on `Command::status` in `test_runner::run`
/// with an `unwrap_or_else` that falls back to the status of `true`. This test went red
/// (exit code 0) under that change.
#[test]
fn test_without_node_on_path_fails_naming_node() {
    let package_dir = fixture_package("package_test_run");
    let output = run_without_node(&package_dir, &["test"]);

    assert!(
        !output.status.success(),
        "a run that could not start `node` must not succeed, got {:?}",
        output.status
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("could not run `node`"),
        "expected an error naming `node`, got: {}",
        stderr
    );

    let _ = std::fs::remove_dir_all(package_dir.join("build"));
}
