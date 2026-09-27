# LANG-69 · There is no `zelkova test`: nothing runs a package's tests

**Sizing:** medium. It adds a subcommand, a generated JavaScript entry point, and a child
process. What could make it bigger is reporting a test module that fails to *load*, because the
module aborts while it is being evaluated. The runner has to attribute that failure to the
module's tests rather than crash with it.

**Part of:** the [bootstrap](README.md#active-work-bootstrap) section.

**Depends on:** [`GEN-17`](README.md), now closed (the binary and its `compile` subcommand),
[`LANG-63`](README.md), now closed too (`Test`, and the pass that collects the values that
have it — `test_collection::collect`), and [`GEN-18`](README.md), now closed too (the test
build is written to `build/test/js/`).

**Location:** `src/main.rs`, which [`GEN-17`](README.md) gives a clap `Command` enum.
`src/compiler/mod.rs` has `compile_package_with_tests`. `src/compiler/javascript.rs` has
`RUNTIME`, the precedent for JavaScript text the compiler carries and writes out.

**Decided ([*Running a package's tests*](../spec/toolchain.md#running-a-packages-tests), and by
the language owner on 2026-09-27):**

- Running a package's tests compiles both roots and then runs every test the package holds. A
  dependency's tests are never run.
- **Provisional:** a run with a failing test exits non-zero. A run that could not compile either
  root exits non-zero in the same way a failed build does.
- The command is **`zelkova test [DIR]`**, a clap subcommand beside `compile`, where `DIR`
  defaults to `.`.
- A test is a `Test` value, which is `Pass` or `Fail` ([`LANG-63`](README.md)), and it is
  reported under `<Module>.<value>`.
- The run happens under **`node`**, found on `PATH`. JavaScript is the only backend, and the
  build is ES modules. This means `zelkova test` needs `node` installed and `zelkova compile`
  does not. `cargo test` still never runs `node`
  ([`DEC-18` decision 6](../decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node)).

**Problem:** with the three prerequisites landed, the tests are collected and written, and
nothing runs them. A package author has no command to invoke, and a test module's values are
never evaluated.

**Approach:**

1. **`zelkova test [DIR]`** calls `compile_package_with_tests`. If compilation fails, it exits
   1 just as `compile` does, and runs nothing.
2. It collects the tests with [`LANG-63`](README.md)'s pass,
   `compiler::test_collection::collect`. With no tests, it prints that there are none and
   exits 0. A package with no tests has not failed any.
3. **The entry point.** It writes `build/test/js/run.mjs`, a module the compiler generates. The
   generated module does not hard-code any test logic beyond the list it is given. For each
   test module it:
   1. `await import()`s the module's emitted file;
   2. reads each collected export;
   3. treats a `$` of `"Pass"` as a pass and anything else as a failure;
   4. prints one line per test;
   5. prints a summary;
   6. sets `process.exitCode` to 1 if anything failed.
   An `import()` that rejects means the module aborted while being evaluated, since every
   parameterless binding is evaluated when the module loads
   ([*A binding with no parameters is evaluated once*](../spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once)).
   When that happens, the run reports every collected test of that module as errored, together
   with the thrown error's message, and moves on to the next module. Put the text generation in
   a function that `tests/javascript.rs` or a unit test can pin without running it.
4. **The child process.** It spawns `node build/test/js/run.mjs` with stdout and stderr
   inherited, and exits with node's exit code. If `node` cannot be spawned, the run prints an
   error naming `node` and exits non-zero. It must never report success in that case, as
   `CLAUDE.md`'s *A pass that emitted an error must not report success* requires.
5. **Docs.** Delete or narrow the **Not implemented:** paragraphs that cite this ticket. They
   are in [*Running a package's tests*](../spec/toolchain.md#running-a-packages-tests) and
   [*What a test is*](../spec/packages.md#what-a-test-is), plus the decision-entry clauses
   [`LANG-63`](README.md) moved here. [*Testing a companion*](../spec/interop.md#testing-a-companion)
   describes a companion test as a facade whose checks are `Task`s. That shape still has no
   runner. Narrow its paragraph to say so, rather than deleting it. Under `CLAUDE.md`'s
   *Commands*, add `cargo run -- test <dir>`.

**Not in this ticket:** running the tests in CI, which is [`TEST-3`](test-3.md), and writing
any real test, which is [`GEN-14`](gen-14.md). Filtering tests by name, running them in
parallel, and timing them are also out.

**Acceptance:**

- A Rust test pins the generated `run.mjs` text for a two-module, three-test input. It checks
  the import paths, the export names read, and the exit-code handling.
- Neutralise-check it by dropping the `process.exitCode` line. The test goes red.
- A new fixture package under `tests/fixtures/` holds one passing and one failing test. By
  hand: `cargo run -- test tests/fixtures/<it>` prints both, marks the second failed, and exits
  1. Removing the failing test makes it exit 0. Paste both runs into the PR.
- `cargo run -- test tests/fixtures/package_type_error` exits 1 without spawning `node`.
- `PATH= cargo run -- test <fixture>` fails with an error that names `node`, and exits
  non-zero. Run it with the binary's absolute path if `cargo` itself needs `PATH`.
- `cargo test --workspace` does not invoke `node`.
- `cargo run -- compile std/core` still prints `parsed 8 modules`, lists all eight as checked,
  and exits 0.
