# GEN-22 · There is no `zelkova run`: nothing runs a program's `main`

**Sizing:** small-to-medium. It adds a clap subcommand, an entry-point file written beside the
build, and a `node` invocation. [`LANG-69`](README.md)'s `test_runner` is the template for all
three. The toolchain appendix gains a paragraph first.

**Part of:** [Active work: effects](README.md#active-work-effects), as the ticket that makes a
Zelkova program something that runs.

**Depends on:** [`LANG-75`](lang-75.md), so that what is run is known to be a `Task ()`;
[`GEN-21`](gen-21.md), for `$runTask`. Nothing here needs [`GEN-16`](gen-16.md): a `main` of
`Task.succeed ()` exercises all of it. A program that does anything observable does need an
effectful facade, and so the end-to-end acceptance below waits for `GEN-16` as well.

**Location:** `src/main.rs` — `Command`, beside `Compile` and `Test`;
`src/compiler/test_runner.rs` — `run`, `entry_point`, `RUN_FILE` and `Error`, the pattern to follow
(or to share, if a common "write an entry point and hand it to node" helper falls out);
`src/compiler/mod.rs` — `compile_package`; [`docs/spec/toolchain.md`](../spec/toolchain.md) —
*The compiler's interface*.

**Problem:** [Running a `Task`](../spec/evaluation-semantics.md#running-a-task) says a program hands
the runtime one `Task`, and that running the program means running it.
[Programs](../spec/packages.md#programs) says `main` is that `Task`. The compiler emits `main`'s
module and stops there. Nothing loads it or runs the `Task`, so a program can be compiled and
never executed.

**Approach:**

1. **Spec first, in its own commit.** [*The compiler's interface*](../spec/toolchain.md#the-compilers-interface)
   names `compile` and `test`. Add `zelkova run [DIR]`: it compiles the package at `DIR` (default
   `.`), is an error for a package with no `main`, runs `main` under `node`, and exits `0` when
   the `Task` completes. It exits non-zero when the build fails or the program
   [aborts](../spec/evaluation-semantics.md#when-a-program-aborts). Mark it **Provisional:** like
   its neighbours.
2. Write an entry point beside the build output, e.g. `build/out/js/main.mjs`. The name and place
   follow the `run.mjs` precedent and the path layout in `javascript.rs`'s *Paths* section. It
   imports the `main` module, hands `main` to `$runTask`, and on rejection prints the abort's
   description and sets a non-zero exit code.
3. Whether `zelkova compile` also writes that entry point, so that `node build/out/js/main.mjs`
   works without the compiler, is this ticket's call. Say which.
4. `node` is looked up on `PATH`, the way `test_runner` does it, and a missing `node` is the same
   error there.

**Tests:** `cargo test` [never runs a real `node`](../decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node).
`tests/cli.rs` already works around that for `zelkova test`, with `run_without_node` and a stub
`node` put on `PATH`. Follow the same pattern. A package with no `main` exits non-zero without
starting `node`. A package that does not compile exits non-zero without starting `node`. A stub
`node` that exits non-zero makes `zelkova run` exit non-zero. Unit-test the entry point's text in
`src/compiler/`, the way `test_runner::entry_point` is. Mind [`TEST-6`](test-6.md) and give each
test its own fixture directory. Running the entry point for real is the acceptance below, done
under `node` by hand and, once [`TEST-3`](test-3.md) lands, in CI.

**Acceptance:** `zelkova run` on a package whose `main` is `Task.succeed ()` exits `0`. The same
package with a `main` that calls an effectful facade whose companion writes to stdout prints that
output. That second check waits on [`GEN-16`](gen-16.md) and lands with whichever of the two
closes last. The toolchain appendix describes the subcommand.
