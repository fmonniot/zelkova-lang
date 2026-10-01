# TOOL-7 · The checking pipeline names its backend, its runners and the test package

**Sizing:** medium. The size is in `tests/pipeline.rs`, which matches on `CompilationError` in
most of its build tests and has every one of those patterns retargeted. It grows if
[`TOOL-3`](README.md) left the checking half handing back something other than what *Approach*
step 2 assumes.

**Part of:** the *Active work: editor support* section of [the index](README.md).
[`TOOL-5`](tool-5.md) depends on it: this ticket makes the dependency order between the future
crates true inside the one crate, so that `TOOL-5` moves files and changes no behaviour.

**Depends on:** [`TOOL-3`](README.md), which splits `compile` into a checking half and a CLI
half. This ticket moves the CLI half; it does not make that split.

**Location:** `src/compiler/mod.rs` — `CompilationError`'s `Emit`, `Output`, `TestRun` and
`ProgramRun` variants and their arms in `as_diagnostic_in` and `module`; `phase_diagnostic`;
`compile_package` and its three siblings; `BUILD_DIRECTORY`, `test_tree`, `ModuleToEmit`,
`to_modules_to_emit`, `emit_build`, `emit_modules`; `PackageName::test_package`.
`src/compiler/test_collection.rs` — `test_type`, `is_test`. `src/compiler/test_runner.rs` and
`src/compiler/program_runner.rs` — `run`. `src/main.rs`. `tests/pipeline.rs`, `tests/cli.rs`.

**Found while** settling [`TOOL-5`](tool-5.md)'s open decisions. Its table had
`zelkova-compiler` not depending on `zelkova-js`, which the code does not allow as it stands.

**Problem:** the modules that check a package reach upwards into the ones that emit and run
it, in five places:

- `compile` calls `javascript::Unions::of`, `javascript::emit` (through `emit_build` and
  `emit_modules`) and `output::write`.
- `to_modules_to_emit`, called from `compile_in_build` and `compile_tests`, finds a facade's
  companion with `javascript::module_file`, and `compile` fills each test facade's
  `companion_imports` with `javascript::test_companion_import`. Which file is a companion is
  the backend's rule ([*A facade names a boundary, not a
  backend*](../spec/interop.md#a-facade-names-a-boundary-not-a-backend)), and it is applied
  while checking.
- `CompilationError` holds `javascript::Error`, `output::Error`, `test_runner::Error` and
  `program_runner::Error`, so the error type of a check names every module above it.
- `PackageName::test_package` is built from `test_collection::TEST_PACKAGE`. Nothing else in
  the checking modules knows `zelkova-test` exists.
- `test_runner::run` and `program_runner::run` each compile the package before running it, so
  a runner depends on the build that feeds it.

**Approach:** every choice below is made. Each step leaves the tree green, and is its own
commit.

1. **`src/driver.rs`, a new top-level module `zelkova_lang::driver`**, beside `compiler` in
   `src/lib.rs`. It takes the CLI half of `compile` as `TOOL-3` left it: `compile_package`,
   `compile_package_into`, `compile_package_with_tests`, `compile_package_with_tests_into`,
   `BUILD_DIRECTORY`, `test_tree` (now `pub`), `ModuleToEmit`, `to_modules_to_emit`,
   `emit_build` and `emit_modules`, with their doc comments. The paragraph of
   `src/compiler/mod.rs`'s module documentation that describes emitting and writing moves to
   `driver`'s. `src/main.rs`, `tests/pipeline.rs` and `tests/cli.rs` call `driver::` where they
   called `compiler::`.
2. **Companion discovery is the driver's.** What the checking half hands back per module
   keeps the `CheckedModule` and its `SourceFileId` and gains `root_dir`, the directory of the
   source root the module was read under, which is the argument `to_modules_to_emit` takes
   today. It carries no `companion` and no `companion_imports`. The struct is public, in
   `src/compiler/mod.rs`, and is named `CheckedSource` unless `TOOL-3` already gave it a public
   name. `driver` builds its own `ModuleToEmit` from it, and the `test_companion_import` loop
   moves with it. `check_main` reads the new struct.
3. **`driver::BuildError`**, the error of everything `driver` returns:

   ```rust
   pub enum BuildError {
       Check(CompilationError),
       Emit(Vec<javascript::Error>, Name),
       Output(output::Error),
       TestRun(test_runner::Error),
       ProgramRun(program_runner::Error),
       InFile(Box<BuildError>, SourceFileId),
       Many(Vec<BuildError>),
   }
   ```

   - `CompilationError` loses `Emit`, `Output`, `TestRun` and `ProgramRun`, and nothing else
     about it changes.
   - `Many` is flat. Each error of the checking half is one `Check(..)` member, whether it is
     a bare variant or a `CompilationError::InFile`. A `CompilationError::Many` is never
     wrapped: its members are. `BuildError::InFile` wraps `Emit` and nothing else.
   - An error raised before the accumulator exists — the manifest, the resolution — comes
     back as a bare `Check(..)`, unrendered, as it does today. `impl From<CompilationError>
     for BuildError` is what `?` goes through.
   - `Many` still means "already rendered", which is what `src/main.rs`'s `fail` tests for.
   - `BuildError::as_diagnostic` delegates `Check` to `CompilationError::as_diagnostic` and
     builds the rest through two functions `src/compiler/mod.rs` exports: `phase_diagnostic`,
     made `pub` as it is, and a new `pub fn plain_diagnostic<E: PhaseError>(error: &E)` holding
     the message-and-notes body the `Output`, `TestRun` and `ProgramRun` arms share today.
     `Emit` renders with the phase name `"code generation"`, as it does today.
   - `BuildError::module` answers for `Emit`, and delegates for `Check` and `InFile`.
   - `CLAUDE.md`'s *An error has to describe itself* invariant says
     `CompilationError::as_diagnostic` is the only place a `Diagnostic` is built. Reword it:
     `as_diagnostic`, and the two functions it shares with `BuildError::as_diagnostic`.
4. **The test runner takes a built tree.**
   `test_runner::run(test_tree: &Path, interfaces: &[Interface]) -> Result<i32, Error>`, with
   `Error` its own. It collects, prints `no tests found` when there is nothing to run, writes
   `RUN_FILE` under `test_tree` and starts `node`. It compiles nothing. A new
   `driver::test(package_dir: &Path) -> Result<i32, BuildError>` calls
   `compile_package_with_tests`, then `run` on `test_tree(..)`, mapping its error into
   `BuildError::TestRun`. `src/main.rs`'s `Command::Test` calls `driver::test`.
5. **`program_runner::run` keeps its shape** and returns `Result<i32, BuildError>`. It reads
   the manifest, calls `driver::compile_package`, writes `MAIN_FILE` and starts `node`, as
   today.
6. **`zelkova-test` is named by the test modules alone.** `is_test` compares the type's
   `QualName` field by field, the way `program::is_core_task` does: `package().as_str()`
   against `TEST_PACKAGE`, `module_name()` and `unqualified_name()` against `"Test"`.
   `test_type` and `PackageName::test_package` are deleted. The unit test
   `the_test_package_name_is_a_legal_one` moves to `test_collection.rs`, asserting that
   `PackageName::new(TEST_PACKAGE)` is `Ok`.

`program::check` stays where it is, called from `check_main`. It is a rule of the language
([*Programs*](../spec/packages.md#programs)) that every build checks, not part of running one.

No file moves between directories here. `javascript.rs`, `output.rs`, `test_runner.rs`,
`test_collection.rs` and `program_runner.rs` stay under `src/compiler/` and stay declared in
`src/compiler/mod.rs`; [`TOOL-5`](tool-5.md) moves them.

**Acceptance:**

- With `UPPER` standing for `javascript.rs`, `output.rs`, `test_runner.rs`,
  `test_collection.rs` and `program_runner.rs`: no other file under `src/compiler/` contains
  `javascript::`, `output::`, `test_runner`, `test_collection`, `program_runner` or `driver`
  outside a comment, except the five `pub mod` lines in `src/compiler/mod.rs`. Among `UPPER`,
  only `program_runner.rs` names `driver`.
- `cargo test --workspace` is green. `tests/cli.rs` is green **unchanged apart from the path
  of `compile_package`**, which is the check that the CLI's output did not move.
- The two tests in `tests/pipeline.rs` that match `javascript::Error::MissingCompanion` now
  match it as `BuildError::InFile` around `BuildError::Emit`, and go red when `emit_modules`
  is made to drop the error it pushes instead of pushing it.
- A new test in `tests/pipeline.rs` builds a fixture with one type error through
  `driver::compile_package_into` and asserts the result is `Err(BuildError::Many(..))` whose
  one member is a `Check` around a `CompilationError::InFile`. It goes red when the driver is
  made to return `Ok` on a non-empty accumulator, which is `BUG-1`'s defect.
- `cargo run -- compile std/core` prints `parsed 10 modules`, lists all ten as checked, and
  exits 0. `cargo run -- test std/core` reports the count `CLAUDE.md` records.
- `cargo clippy --workspace --all-features -- -D warnings`, `cargo fmt --all --check` and the
  local rustdoc command in `CLAUDE.md` are green.
