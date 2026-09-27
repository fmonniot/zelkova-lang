# GEN-17 · The compiler has no command line: `src/main.rs` compiles `std/core` and takes no arguments

**Sizing:** small-to-medium. The binary itself is small. What makes it bigger is the sweep:
42 ticket files, `CLAUDE.md` and five skills say `cargo run` where they mean "compile
`std/core`", and every one of them goes stale when a bare `cargo run` stops doing that.

**Part of:** the [bootstrap](README.md#active-work-bootstrap) section. [`LANG-69`](lang-69.md)
adds `zelkova test` to the binary this ticket creates.

**Location:** `src/main.rs`, which calls `compile_package("std/core".as_ref())` under the comment
`// Will need more love than that :p` and reads no arguments. `Cargo.toml`, which declares no
`[[bin]]`, so the binary is named after the crate: `zelkova-lang`. `src/compiler/mod.rs` has
`compile_package`, the function this binary calls.

**Decided ([`docs/spec/toolchain.md`](../spec/toolchain.md#the-compilers-interface), marked
**Provisional:** there, and settled by the language owner on 2026-09-27 for the parts the chapter
left open):**

- The compiler is pointed at a package root, meaning the directory holding `zelkova.toml`. It
  resolves that package, compiles every module of `src/`, and writes its output beside that
  directory. Errors are reported with a caret in the file they came from. A build that emitted
  any error exits non-zero and writes no output. `compile_package` already does all of this.
  This ticket gives it a caller that can name the directory.
- **The binary is `zelkova`, and building is a subcommand: `zelkova compile [DIR]`.** `DIR`
  defaults to `.`. `zelkova test [DIR]` is the second subcommand and is
  [`LANG-69`](lang-69.md)'s to add.
- **Arguments are parsed with [`clap`](https://docs.rs/clap)** (derive API), rather than by hand
  over `std::env::args`. That gives usage text and argument errors without writing them.
- **A bare `zelkova` with no subcommand does not compile anything.** It prints usage and exits
  non-zero, which is clap's default. `cargo run` therefore stops being the smoke test, and
  `cargo run -- compile std/core` replaces it.

**Problem:** the compiler cannot be asked for any package except `std/core`. So nothing outside
this repository can be compiled without editing `src/main.rs`, and the fixture packages under
`tests/fixtures/` are reachable only from `tests/pipeline.rs`. That hardcoding is also why
[`LANG-63`](lang-63.md) and [`GEN-14`](gen-14.md) both had nowhere to put a `zelkova test`.

**Approach:**

1. Add `clap` with the `derive` feature, and a `[[bin]]` named `zelkova` whose path is
   `src/main.rs`.
2. `main` parses a `Command` enum with one variant, `Compile { dir: PathBuf }`, whose default is
   `.`, and calls `compile_package(&dir)`. The error handling `main` already has stays as it is:
   a non-`Many` error is printed through `as_diagnostic`, and any error exits 1. Keep its comment
   explaining why `Many` is not printed again.
3. The sweep. Every place that says `cargo run` meaning "compile `std/core`" now says
   `cargo run -- compile std/core`. That covers `CLAUDE.md` (the *Commands* block and the
   paragraph under it), the `review-pr`, `fix-pr-comments`, `write-spec-chapter`, `work-ticket`
   and `create-ticket` skills, and the open tickets under `docs/tickets/` (list them with
   `grep -l 'cargo run' docs/tickets/*.md`). Leave `cargo run -p spec-site` alone, since that
   one names another binary. It is a mechanical replace, but read each hit, because a few name
   `cargo run` in running prose.
4. In [*The compiler's interface*](../spec/toolchain.md#the-compilers-interface), record the
   command surface under its **Provisional:** paragraph.

**Not in this ticket:** a `run` subcommand. Nothing can be run before `main : Task ()` and a
runtime for `Task` exist; that is the same blocker [`GEN-16`](gen-16.md) has. A `--target` flag
is also out: JavaScript is the only backend, and the flag arrives with [`GEN-15`](gen-15.md).
Fetching, the lock file and the cache belong to [`LANG-61`](lang-61.md).

**Acceptance:**

- `cargo run -- compile std/core` prints `parsed 8 modules`, lists all eight as checked, and
  exits 0.
- `cargo run -- compile tests/fixtures/package_type_error` exits non-zero with its diagnostic
  on stderr.
- `cargo run` with no arguments prints usage and exits non-zero.
- `cd std/core && cargo run --manifest-path ../../Cargo.toml -- compile` compiles the package
  in the working directory.
- `grep -rn '`cargo run`' CLAUDE.md .claude/skills docs/tickets` finds no hit that still means
  "compile `std/core`".
- `tests/pipeline.rs::stdlib_package_compiles` is unchanged and green.
- `cargo test --workspace` is green.
