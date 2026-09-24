# GEN-13 · Write the build

**Sizing:** medium. The output layout, the file writing, the runtime's placement, and the wiring
into `compile_package` and `src/main.rs`.

**Depends on:** `GEN-9`, closed — `javascript::emit` in `src/compiler/javascript.rs` produces a
module's text. Its `module_specifier` and `runtime_specifier` are the only places an import path
is built, and both are provisional until this ticket settles the layout: a module name is all an
imported reference carries, so neither knows which package declared it. Sits with
[`GEN-12`](README.md), which places a companion into the layout this decides.

`emit` refuses four of `std/core`'s eight modules today, which bears on this ticket's
acceptance. Its three facades emit ([`GEN-12`](README.md)), and so does `Bitwise`, which only
forwards to one of them. The causes differ by module:

- `Maybe` and `Result` hold a `case`, which [`GEN-10`](gen-10.md) emits.
- `Basics` and `Tuple` each hold a declaration the typer could not check, and `emit` refuses a
  module missing a declaration rather than writing it without one. Each of them matches a
  pattern in a parameter — a constructor in `never (JustOneMore nvr)`, a tuple in
  `Tuple.first (x,_) = x` and in `Basics.fromPolar`/`toPolar` — and the typer's
  `wrap_with_patterns` translates only a variable or `_` there, which is
  [`BUG-39`](bug-39.md).

**Part of:** [`GEN-1`](gen-1.md).

**Location:** `src/compiler/mod.rs` — `compile`, which accumulates errors and reports, and
`check_module`, which ends at `exhaustiveness::check`; `compile_in_build`, which is one package
of the build. `src/main.rs`, which calls `compile_package("std/core")`, prints and writes
nothing. `.gitignore`.

**Decided ([`DEC-18` decision 5](../decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest), and
[*The compiler's interface*](../spec/toolchain.md#the-compilers-interface)):** output goes to
`build/js/<package-name>/<module path>.mjs`, beside the **root** package's manifest and never
beside a source it read. One emitted file per Zelkova module.

A module's file path uses its name **within its own package**. The namespace a dependent writes
is added at the boundary and [does not appear under `src/`](../spec/packages.md#the-namespace),
so it does not appear in the output tree either: `zelkova-core`'s `Js.Basics` is
`build/js/zelkova-core/Js/Basics.mjs`, whatever a dependent calls it. That also means an import
across a package boundary resolves to a sibling package's directory, which is why every package
in the build gets its own directory rather than the whole build being flattened.

**A build that emitted any error writes no output**, which the same appendix requires and which
`compile_package`'s existing accumulate-then-report shape already sets up: emission runs only
when the error vector is empty.

**Problem:** there is no build directory and nothing writes a file. `compile_package`
accumulates errors, prints which modules checked, and drops every `canonical::Module` it
produced. `src/main.rs` exits 0 or 1 and leaves nothing behind.

**Approach:** emission is a step after the whole build has checked, not a step inside
`check_module` — a module cannot be written until it is known that no *other* module failed,
because a partial build is the output this ticket must not produce.

Place the runtime [`GEN-8`](gen-8.md) wrote at a fixed path in the output, `build/js/`'s root
being the obvious candidate since every package's modules import it. It is copied from the
compiler's own tree; how the compiler finds its own runtime file at run time is this ticket's to
settle, and the choice is worth a doc comment because it is the one piece of the output that is
not generated.

`cargo run` emits on every invocation. Add `build/` to `.gitignore`.

**The existing smoke test must keep its meaning.** `cargo run` prints `parsed 8 modules`, lists
all eight as checked and exits 0, and `tests/pipeline.rs::stdlib_package_compiles` pins the same
thing — CLAUDE.md calls it a genuine pass/fail test. A failure to emit joins that: it is an
error on the accumulator like any other, so the run exits non-zero and writes nothing.

**Not in this ticket:** a command-line interface. `src/main.rs` takes no arguments and this
ticket does not give it any — what the compiler's interface becomes is
[`GEN-17`](gen-17.md)'s, and widening `main.rs` here would be building half of it in the wrong
place.

**Acceptance:** `cargo run` writes `build/js/zelkova-core/` holding eight `.mjs` files named
after the eight modules, the three `Js/*` companions beside their facades, and the runtime at
`build/js/`'s agreed path; it still prints `parsed 8 modules`, lists all eight and exits 0. This
carries forward the closed [`GEN-12`](README.md)'s own Acceptance clause of the same shape
("`cargo run` emits a module for each of the three `Js/*` facades with their companions beside
them") — `GEN-12` produced the text `javascript::emit` writes, not the write itself, so that
clause is not met until this ticket lands. A test in `tests/pipeline.rs` compiles a fixture
package into a temporary directory and asserts the file tree. A second asserts a package with a
failing module writes **no** file at all — neutralise-check it by emitting before the error
check, which turns it red. `build/` is gitignored and `git status` is clean after a `cargo run`.
