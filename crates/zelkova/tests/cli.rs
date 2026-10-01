//! End-to-end tests of the `zelkova` binary's command line itself — `crates/zelkova/src/main.rs`'s
//! `Cli`/`Command` wiring — as opposed to `crates/zelkova/tests/pipeline.rs`, which drives
//! `compile_package` directly and never goes through `main`.
//!
//! Every one of GEN-17's Acceptance clauses is a shell-level check (`cargo run --
//! compile std/core`, a fixture, the bare invocation, `--manifest-path`), verified by
//! hand when that ticket landed. Nothing here duplicates the compiler's own behaviour —
//! `crates/zelkova/tests/pipeline.rs` and the layers below it already pin that — this file pins only
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
/// directories `crates/zelkova/tests/pipeline.rs::fixture_package` uses.
fn fixture_package(name: &str) -> PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    Path::new(&manifest)
        .join("../..")
        .join("tests/fixtures")
        .join(name)
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
    let output = run(&Path::new(&manifest).join("../.."), &[]);

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
/// `#[arg(default_value = "/nonexistent-zelkova-cli-test-dir")]` in `crates/zelkova/src/main.rs`.
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
/// Neutralised by changing the match arm in `crates/zelkova/src/main.rs` from
/// `Command::Compile { dir } => driver::compile_package(&dir)` to
/// `Command::Compile { .. } => driver::compile_package(Path::new("tests/fixtures/package_type_error"))`
/// — ignoring the parsed `dir` — while running from the repository root with
/// `compile tests/fixtures/package_checks` as the argument. This test went red
/// (exit code 1, an `Int`/`Bool` mismatch on stderr instead of "parsed 1 modules")
/// under that change, confirming it is the parsed `dir` — not a fixed path — that
/// reaches `compile_package`.
#[test]
fn compile_routes_explicit_dir_to_compile_package() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let repo_root = PathBuf::from(&manifest).join("../..");
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
/// (`crates/zelkova/tests/pipeline.rs`'s `compile_package reports failure when a module fails`), but
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
/// `driver::test` with `.unwrap_or_default()`, so a failed build carries on with nothing
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

/// A directory holding one executable named `node` that runs `script`, for a test to put on
/// `PATH` in place of the real one. `cargo test` never runs a real `node`
/// ([`DEC-18` decision 6](../docs/decisions/dec-18.md)); what these tests pin is what
/// `zelkova test` does with the way a `node` ended, which a stub can end in any way.
///
/// `name` keeps each test's directory its own, since the tests of this file run in parallel.
#[cfg(unix)]
fn stub_node_dir(name: &str, script: &str) -> PathBuf {
    use std::os::unix::fs::PermissionsExt;

    let dir = Path::new(env!("CARGO_TARGET_TMPDIR")).join(format!("stub-node-{}", name));
    std::fs::create_dir_all(&dir).expect("failed to create the stub directory");
    let node = dir.join("node");
    std::fs::write(&node, format!("#!/bin/sh\n{}\n", script)).expect("failed to write the stub");
    std::fs::set_permissions(&node, std::fs::Permissions::from_mode(0o755))
        .expect("failed to make the stub executable");
    dir
}

/// A copy of the fixture `fixture` under its own directory, its `std/` paths made absolute so
/// the copy can sit anywhere. The tests that put a stub `node` on `PATH` each get their own,
/// because running `zelkova` writes `build/` beside the manifest and the tests of this file
/// run in parallel: two processes writing one `build/` prune each other's files.
///
/// `name` keeps each test's directory its own, so it is unique per test and not per fixture.
#[cfg(unix)]
fn scratch_package(fixture: &str, name: &str) -> PathBuf {
    let fixture = fixture_package(fixture);
    let dir = Path::new(env!("CARGO_TARGET_TMPDIR")).join(format!("package-scratch-{}", name));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("failed to create the scratch package");
    for tree in ["src", "tests"] {
        if !fixture.join(tree).exists() {
            continue;
        }
        let copied = Command::new("cp")
            .arg("-R")
            .arg(fixture.join(tree))
            .arg(dir.join(tree))
            .status()
            .expect("failed to run `cp`");
        assert!(copied.success(), "failed to copy `{}`", tree);
    }
    let std_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("std");
    let manifest = std::fs::read_to_string(fixture.join("zelkova.toml"))
        .expect("failed to read the fixture's manifest")
        .replace("../../../std", &std_root.display().to_string());
    std::fs::write(dir.join("zelkova.toml"), manifest).expect("failed to write the manifest");
    dir
}

/// A copy of the `package_test_run` fixture: see [`scratch_package`].
#[cfg(unix)]
fn scratch_test_run_package(name: &str) -> PathBuf {
    scratch_package("package_test_run", name)
}

/// Like [`run`], with `PATH` set to `path` alone.
#[cfg(unix)]
fn run_with_path(cwd: &Path, path: &Path, args: &[&str]) -> Output {
    Command::new(zelkova_bin())
        .args(args)
        .current_dir(cwd)
        .env("PATH", path)
        .output()
        .expect("failed to run the zelkova binary")
}

/// The exit code `node` ends with is the exit code `zelkova test` ends with: a stub `node`
/// that is handed the entry point and exits 3 makes the run exit 3, not 0 and not a fixed 1.
/// The stub exits 9 when it is not handed `run.mjs`, so the code also shows the entry point
/// reached `node`.
///
/// Neutralised two ways, each going red (exit code 0 instead of 3): replacing
/// `Ok(code) => std::process::exit(code)` in `main` with `Ok(_) => {}`, and replacing
/// `status.code().ok_or(..)` in `test_runner::run` with `Ok(0)`.
#[cfg(unix)]
#[test]
fn test_exits_with_the_code_node_exits_with() {
    let package_dir = scratch_test_run_package("exit-code");
    let stub = stub_node_dir(
        "exit-code",
        "case \"$1\" in */run.mjs) exit 3 ;; *) exit 9 ;; esac",
    );
    let output = run_with_path(&package_dir, &stub, &["test"]);

    assert_eq!(output.status.code(), Some(3), "{:?}", output);
}

/// A `node` that a signal ended has no exit code, and the tests it was running did not
/// finish: `zelkova test` reports that and exits non-zero rather than reading the missing
/// code as a pass.
///
/// Neutralised by replacing `status.code().ok_or(..)` in `test_runner::run` with
/// `Ok(status.code().unwrap_or(0))`. This test went red (exit code 0) under that change.
#[cfg(unix)]
#[test]
fn test_fails_when_node_is_ended_by_a_signal() {
    let package_dir = scratch_test_run_package("signal");
    let stub = stub_node_dir("signal", "kill -9 $$");
    let output = run_with_path(&package_dir, &stub, &["test"]);

    assert_eq!(output.status.code(), Some(1), "{:?}", output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("`node` ended before the tests finished"),
        "expected the error to say node ended early, got: {}",
        stderr
    );
}

/// A stub `node` that records being started, by creating the returned marker file with a
/// shell redirect (`PATH` holds only the stub), and then ends with `code`. A test that must show `node` was never started asserts the marker is
/// absent. The stub exits 9 when it is not handed a `main.mjs`, so the exit code also shows
/// the entry point reached it.
#[cfg(unix)]
fn recording_stub_node(name: &str, code: i32) -> (PathBuf, PathBuf) {
    let marker = Path::new(env!("CARGO_TARGET_TMPDIR")).join(format!("node-started-{}", name));
    let _ = std::fs::remove_file(&marker);
    let stub = stub_node_dir(
        name,
        &format!(
            ": > '{}'\ncase \"$1\" in */main.mjs) exit {} ;; *) exit 9 ;; esac",
            marker.display(),
            code
        ),
    );
    (stub, marker)
}

/// `zelkova run` on a package with no `main` — a library — exits 1 saying so, compiles
/// nothing and never starts `node`.
///
/// Neutralised by replacing the `let Some(main) = .. else` in `program_runner::run` with
/// `let main = Name::new("App")`: the run then compiles the library and starts the stub,
/// and this test went red (the marker exists, and there is no error naming `main`).
#[cfg(unix)]
#[test]
fn run_on_a_package_with_no_main_exits_1_without_starting_node() {
    let package_dir = scratch_package("package_no_tests", "run-no-main");
    let (stub, marker) = recording_stub_node("run-no-main", 0);
    let output = run_with_path(&package_dir, &stub, &["run"]);

    assert_eq!(output.status.code(), Some(1), "{:?}", output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("has no `main`, so there is nothing to run"),
        "expected the error to say there is no `main`, got: {}",
        stderr
    );
    assert!(!marker.exists(), "`node` must not be started");
    assert!(!package_dir.join("build").exists(), "nothing is compiled");
}

/// `zelkova run` on a program that does not compile exits 1 with the build's diagnostic,
/// writes no entry point and never starts `node`.
///
/// Neutralised by replacing the `?` after `compile_package(package_dir)` in
/// `program_runner::run` with `let _ =`: the run then carries on to write the entry point,
/// and this test went red ("could not write" on stderr).
#[cfg(unix)]
#[test]
fn run_on_a_package_that_does_not_compile_exits_1_without_starting_node() {
    let package_dir = scratch_package("package_run_type_error", "run-type-error");
    let (stub, marker) = recording_stub_node("run-type-error", 0);
    let output = run_with_path(&package_dir, &stub, &["run"]);

    assert_eq!(output.status.code(), Some(1), "{:?}", output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("cannot match"),
        "expected the type error's diagnostic on stderr, got: {}",
        stderr
    );
    assert!(
        !stderr.contains("could not write"),
        "a build that failed must stop before the entry point, got: {}",
        stderr
    );
    assert!(!marker.exists(), "`node` must not be started");
    assert!(!package_dir.join("build/out/js/main.mjs").exists());
}

/// The exit code `node` ends with is the exit code `zelkova run` ends with: a stub `node`
/// handed the entry point that exits 3 makes the run exit 3 — not 0 and not a fixed 1 — and
/// the entry point it was handed is the one `run` wrote at `build/out/js/main.mjs`.
///
/// Neutralised two ways, each going red (exit code 0 instead of 3): replacing
/// `status.code().ok_or(..)` in `program_runner::run` with `Ok(0)`, and replacing
/// `Command::Run { dir } => ..` in `main` with one that maps the result to `Ok(0)`.
#[cfg(unix)]
#[test]
fn run_exits_with_the_code_node_exits_with() {
    let package_dir = scratch_package("package_main_ok", "run-exit-code");
    let (stub, marker) = recording_stub_node("run-exit-code", 3);
    let output = run_with_path(&package_dir, &stub, &["run"]);

    assert_eq!(output.status.code(), Some(3), "{:?}", output);
    assert!(marker.exists(), "`node` must be started");
    let entry = std::fs::read_to_string(package_dir.join("build/out/js/main.mjs"))
        .expect("the entry point must be written");
    assert!(entry.contains("./package-main-ok/App.mjs"), "{}", entry);
}

/// A stub `node` that ends with 0 makes `zelkova run` exit 0.
///
/// Neutralised by replacing `Ok(0) => {}` in `main` with `Ok(_) => std::process::exit(1)`.
/// This test went red (exit code 1) under that change.
#[cfg(unix)]
#[test]
fn run_exits_0_when_node_does() {
    let package_dir = scratch_package("package_main_ok", "run-exit-zero");
    let (stub, _marker) = recording_stub_node("run-exit-zero", 0);
    let output = run_with_path(&package_dir, &stub, &["run"]);

    assert_eq!(output.status.code(), Some(0), "{:?}", output);
}

/// A `node` that a signal ended has no exit code and the program did not finish: `zelkova
/// run` reports that and exits non-zero.
///
/// Neutralised by replacing `status.code().ok_or(..)` in `program_runner::run` with
/// `Ok(status.code().unwrap_or(0))`. This test went red (exit code 0) under that change.
#[cfg(unix)]
#[test]
fn run_fails_when_node_is_ended_by_a_signal() {
    let package_dir = scratch_package("package_main_ok", "run-signal");
    let stub = stub_node_dir("run-signal", "kill -9 $$");
    let output = run_with_path(&package_dir, &stub, &["run"]);

    assert_eq!(output.status.code(), Some(1), "{:?}", output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("`node` ended before the program finished"),
        "{}",
        stderr
    );
}

/// When `node` cannot be started, `zelkova run` says so by name and exits non-zero. It runs
/// from inside the package with no directory argument, so this also pins that `Run { dir }`
/// defaults to `.`.
///
/// Neutralised by replacing the `map_err(..)?` on `Command::status` in
/// `program_runner::run` with an `unwrap_or_else` that falls back to the status of `true`.
/// This test went red (exit code 0) under that change.
#[cfg(unix)]
#[test]
fn run_without_node_on_path_fails_naming_node() {
    let package_dir = scratch_package("package_main_ok", "run-no-node");
    let output = run_without_node(&package_dir, &["run"]);

    assert_eq!(output.status.code(), Some(1), "{:?}", output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("could not run `node`"),
        "expected an error naming `node`, got: {}",
        stderr
    );
}
