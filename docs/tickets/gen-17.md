# GEN-17 · A `zelkova` binary that compiles and runs

**Sizing:** large, and **unscheduled**. Filed now because two other tickets point at it and
would otherwise each invent half of it.

**Depends on:** [`GEN-1`](gen-1.md)'s program. A binary whose job is to run a program cannot be
written before something can be run, which is why it is a successor to code generation and not a
prerequisite of it.

**Location:** `src/main.rs`, which calls `compile_package("std/core".as_ref())`, takes no
arguments and has the comment `// Will need more love than that :p` over that line.
`src/compiler/mod.rs` — `compile_package` and `compile_package_with_tests`, the latter having no
caller outside `tests/pipeline.rs`.

**Decided ([`docs/spec/toolchain.md`](../spec/toolchain.md#the-compilers-interface), marked
**Provisional:** there and so open to argument):** the compiler is pointed at a package root —
the directory holding `zelkova.toml` — resolves, compiles every module of `src/`, and writes its
output beside that directory rather than beside any source it read. Errors are reported with a
caret in the file they came from, and a build that emitted any error exits non-zero and writes no
output.

[Running a package's tests](../spec/toolchain.md#running-a-packages-tests) compiles both roots
and then runs every test the package holds — [every value a module under `tests/` exposes whose
type is `Test`](../spec/packages.md#what-a-test-is). A run with a failing test exits non-zero,
and a run that could not compile either root is the same non-zero exit a failed build already
is.

**Problem:** the compiler has no interface. `src/main.rs` hardcodes one package path and takes
no arguments, so nothing in the toolchain can ask for another package, for a package's tests, or
for a target. `compile_package_with_tests` exists and is unreachable outside the test suite.

Two tickets are waiting on this and say so:

- [`LANG-63`](lang-63.md), the `Test` type and the test runner, which needs something that can
  ask the compiler for a package's tests and then run them.
- [`GEN-14`](gen-14.md), which emits a fixture package through whatever `cargo run` does,
  because there is no way to point the compiler at a directory. That ticket works around it
  rather than waiting, and the workaround is what this replaces.

**Approach:** not decided. What has to be settled: the command surface — whether building,
testing and running are subcommands or flags; how a target is named, now that
[a facade is written once for all of them](../spec/interop.md#a-facade-names-a-boundary-not-a-backend)
and a build reads the companion for the target it is building; where the binary lives relative
to the library, and whether the `tools/` crates are the precedent; and what "run" means before
[`main`](../spec/packages.md#programs) and `Task` exist — which is the same blocker
[`GEN-16`](gen-16.md) has.

The rest of [`docs/spec/toolchain.md`](../spec/toolchain.md) — fetching, the cache, the lock
file, vendoring, publishing — is [`LANG-61`](lang-61.md)'s and is **not** this ticket. This one
is the interface, not the package manager behind it.

**Acceptance:** not written — like [`GEN-15`](gen-15.md), a ticket sized this way is replaced by
its children. What makes it closable is a decided command surface and a binary that can be
pointed at a package other than `std/core`.
