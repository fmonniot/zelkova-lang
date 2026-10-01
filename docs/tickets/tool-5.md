# TOOL-5 · The compiler, its JavaScript backend and its command line are one crate

**Sizing:** medium to large, all of it by count: every file under `src/` and `tests/` moves,
and every path the repository writes down moves with them. No behaviour changes and no
decision is left to make. It grows with every open ticket whose **Location** it invalidates,
which is why it goes after [`TOOL-2`](README.md) through [`TOOL-4`](tool-4.md) and not before.

**Part of:** the *Active work: editor support* section of [the index](README.md). Nothing
depends on it except [`TIDY-12`](tidy-12.md): [`TOOL-6`](tool-6.md) can start as a workspace
member depending on today's `zelkova-lang` library.

**Depends on:** [`TOOL-7`](tool-7.md), which makes the dependency order below true inside the
one crate. Without it `zelkova-compiler` cannot be built apart from `zelkova-js`.

**Location:** `Cargo.toml`; `build.rs`; everything under `src/` and `tests/`;
`.github/workflows/rust.yml` and `rustdoc.yml`; `tools/spec-site/assets/index.html` and
`tools/spec-site/src/main.rs` — `API_REDIRECT`; `CLAUDE.md`; `.claude/skills/`; the
**Location** line of every open ticket.

**Problem:** one package, `zelkova-lang`, holds the parser, the checker, the JavaScript
backend, the `node` test and program runners, and the `clap` command line. That was right
while the command line was its only consumer. With a language server coming, it has three
costs:

- **A consumer takes everything.** A language server that only checks still links the
  backend, `node` spawning and `clap`. A formatter or highlighter that only tokenizes links
  the type checker.
- **Nothing enforces the backend boundary.** The `ir` module's doc comment says the
  `ir::Module` is what a backend reads. `javascript.rs` can reach any `pub(crate)` item in the
  compiler, and a second backend ([`GEN-15`](gen-15.md)) should not be able to.
- **`build.rs` regenerates the LALRPOP grammar for the one crate**, so an unrelated edit and a
  grammar edit share one compilation unit.

**Approach:** five crates under `crates/`, each in a directory named after its package. The
root `Cargo.toml` becomes a virtual manifest.

| Package | Directory | Holds | Depends on |
|---|---|---|---|
| `zelkova-syntax` | `crates/zelkova-syntax/` | the tokenizer, the layout pass, the grammar and its `build.rs`, the `parser` AST, `position`, `tuple`, `Name` | — |
| `zelkova-compiler` | `crates/zelkova-compiler/` | everything that checks a build: `manifest`, `resolve`, `source`, `dependencies`, `canonical`, `typer`, `exhaustiveness`, `ir`, `default_imports`, `scalars`, `program`, `QualName`, `utils`, and what `src/compiler/mod.rs` holds | syntax |
| `zelkova-js` | `crates/zelkova-js/` | `javascript`, `output` | syntax, compiler |
| `zelkova-test-runner` | `crates/zelkova-test-runner/` | `test_runner`, `test_collection` | syntax, compiler, js |
| `zelkova` | `crates/zelkova/` | a library, `driver` and `program_runner`, and the `zelkova` binary, `main.rs` | all four |

The canonicalizer, typer and IR stay one crate. They share `CheckedModule`, `Interface`,
`QualName` and `PhaseError`, and splitting them would turn the internals they share into public
API. `program` stays with them, because `program::check` is a rule every build checks. The
test runner is a crate of its own so that a second way of running tests is a sibling of it,
and is not named `zelkova-test` because that is the Zelkova package under `std/test`.

### Where each file goes

`src/compiler/mod.rs` becomes `zelkova-compiler`'s `src/lib.rs`, so the `compiler` module
layer is gone: `zelkova_lang::compiler::typer` is `zelkova_compiler::typer`.

| From | To |
|---|---|
| `src/compiler/parser/` (every file), `position.rs`, `tuple.rs` | `crates/zelkova-syntax/src/`, same names |
| `src/compiler/name.rs` — `Name`, its two `impl`s and their tests | `crates/zelkova-syntax/src/name.rs` |
| `build.rs` | `crates/zelkova-syntax/build.rs` |
| `src/compiler/name.rs` — the rest: `QualName` and its tests | `crates/zelkova-compiler/src/name.rs` |
| `src/compiler/mod.rs` | `crates/zelkova-compiler/src/lib.rs` |
| `src/utils.rs` | `crates/zelkova-compiler/src/utils.rs` |
| `src/compiler/canonical/`, `ir/`, `source/`, `typer/`, `default_imports.rs`, `dependencies.rs`, `exhaustiveness.rs`, `manifest.rs`, `program.rs`, `resolve.rs`, `scalars.rs` | `crates/zelkova-compiler/src/`, same names |
| `src/compiler/javascript.rs` | `crates/zelkova-js/src/lib.rs` |
| `src/compiler/output.rs` | `crates/zelkova-js/src/output.rs` |
| `src/compiler/test_runner.rs` | `crates/zelkova-test-runner/src/lib.rs` |
| `src/compiler/test_collection.rs` | `crates/zelkova-test-runner/src/collection.rs` |
| `src/driver.rs` | `crates/zelkova/src/lib.rs` |
| `src/compiler/program_runner.rs` | `crates/zelkova/src/program_runner.rs` |
| `src/main.rs` | `crates/zelkova/src/main.rs` |
| `src/lib.rs` | deleted; its `#[macro_use] extern crate lalrpop_util;` goes to `zelkova-syntax`'s `src/lib.rs`, which is new and declares `name`, `parser`, `position` and `tuple` |

`runtime/`, `std/`, `tests/fixtures/` and `tests/js/` stay at the repository root: more than
one crate reads each of the first three, and `tests/js/` runs the built binary, not a crate.

Use `git mv` for every row, so that `git log --follow` survives the move.

### What changes inside a moved file

Nothing but the following.

- **Paths.** `crate::compiler::x` and `super::x` become `crate::x` inside the crate that
  holds `x`, and `zelkova_syntax::x`, `zelkova_compiler::x` or `zelkova_js::x` across a
  boundary. `grammar.lalrpop`'s three `use crate::compiler::` lines are among them.
- **`Name` has two paths and one definition.** `zelkova_compiler::name` holds `QualName` and
  `pub use zelkova_syntax::name::Name;`, so everything above the syntax crate keeps importing
  `name::{Name, QualName}` from one place. `QualName`'s methods that build a `Name` through its
  private field (`to_name`, `unqualified_name`, `module_name`) call `Name::new` instead.
- **Visibility.** An item reached across one of the new boundaries becomes `pub`. The
  compiler's errors name each one. Nothing else changes visibility, and nothing is made `pub`
  to satisfy a test.
- **Intra-doc links pointing up the dependency order** cannot resolve, and rustdoc runs with
  `-D warnings`. Each becomes a code span naming the item by its new path: the five
  `` [`javascript::emit`](crate::compiler::javascript::emit) `` links in `ir/mod.rs` become
  `` `zelkova_js::emit` ``. Links pointing down the order keep resolving once their path is
  rewritten.
- **Links into `docs/`.** The 144 doc-comment links of the form `](../../../docs/…)` are
  relative to the rendered rustdoc page, so each has one `../` per segment of its module's
  path, the crate's name included. Rewrite each to that rule: `zelkova_compiler::program`
  takes `../../docs/`, `zelkova_compiler` itself and `zelkova_js` itself take `../docs/`,
  `zelkova_compiler::source::files` takes `../../../docs/`. Nothing checks these links, and
  they resolve on the published site neither before nor after: the site mounts no
  `api/docs/`. This ticket keeps the convention and fixes nothing about it;
  [`SITE-3`](site-3.md) is the ticket that does. If that one landed first and the links are
  absolute, there is nothing here to rewrite.
- **`RUNTIME`'s `include_str!`** in `zelkova-js`'s `src/lib.rs` reads
  `"../../../runtime/js/zelkova.mjs"`.

### Manifests

- **Root `Cargo.toml`:** `[workspace]` with `resolver = "2"`, `members` listing the five
  crates and the two under `tools/`, and `default-members` listing the five crates. With
  those `default-members`, `cargo run -- compile std/core` and `cargo test --test spec` keep
  the spelling `CLAUDE.md`, the skills and CI use, and a bare `cargo test` still skips
  `tools/`. `[workspace.package]` carries `version` and `authors`; the five crates inherit
  both. `tools/spec-doc` and `tools/spec-site` keep their own manifests untouched.
- **Each crate** keeps `edition = "2018"`. Moving off it is [`TIDY-12`](tidy-12.md), after
  this ticket, so that this diff holds no reformatting.
- **Dependencies** go to the crate whose code uses them, and nowhere else: `lalrpop` (build),
  `lalrpop-util` and `unic-ucd-category` to `zelkova-syntax`; `clap` to `zelkova`; the rest by
  what each crate's `use` lines name once moved, as a dev-dependency where only `#[cfg(test)]`
  code or a file under `tests/` names it. `indoc` and `spec-doc`
  (`path = "../../tools/spec-doc"`) are dev-dependencies of the crates whose tests use them.
- **`crates/zelkova/Cargo.toml`** declares a library and `[[bin]] name = "zelkova"`, so
  `CARGO_BIN_EXE_zelkova` keeps its name.

### Tests

| From | To |
|---|---|
| `tests/compiler/parser/` | `crates/zelkova-syntax/tests/parser/`, declared by a new `crates/zelkova-syntax/tests/parser_tests.rs` holding the `mod parser { … }` block of `tests/compiler_tests.rs` |
| `tests/compiler/canonical.rs` | `crates/zelkova-compiler/tests/canonical.rs`, a test binary of its own, with a plain `mod support;` |
| `tests/support/mod.rs` | `crates/zelkova-compiler/tests/support/mod.rs` |
| `tests/typer.rs`, `tests/ir.rs`, `tests/spec.rs` | `crates/zelkova-compiler/tests/` |
| `tests/javascript.rs` | `crates/zelkova-js/tests/javascript.rs` |
| `tests/pipeline.rs`, `tests/cli.rs` | `crates/zelkova/tests/` |
| `tests/compiler_tests.rs` | deleted |

- `tests/pipeline.rs` moves whole. It calls `driver::compile_package` and `javascript::emit`
  throughout, so the only crate that can hold it is `zelkova`. Sorting its `check_module`
  tests into `zelkova-compiler` is not part of this ticket.
- **One copy of `support`.** `crates/zelkova-js/tests/javascript.rs` and
  `crates/zelkova/tests/pipeline.rs` include it with
  `#[path = "../../zelkova-compiler/tests/support/mod.rs"] mod support;`, the device
  `tests/compiler/canonical.rs` uses today.
- **The repository root is two levels up.** Every test that builds a path from
  `CARGO_MANIFEST_DIR` — to `std/`, `tests/fixtures/` or `docs/` — joins `../..` first.
  `tests/js/*.mjs` run `cargo run` from the repository root and do not change.
- Unit tests stay in the file they are in and move with it.

### What names the old layout

- **CI and the site.** `.github/workflows/rust.yml`'s `rustdoc` job and `rustdoc.yml` run
  `cargo doc --no-deps -p zelkova-syntax -p zelkova-compiler -p zelkova-js
  -p zelkova-test-runner -p zelkova`. `tools/spec-site/assets/index.html` links
  `api/zelkova_compiler/`, and `API_REDIRECT` in `tools/spec-site/src/main.rs` points at
  `zelkova_compiler/index.html` in both places it names the path. The compiler crate is the
  landing page; rustdoc's own crate list reaches the other four from it.
- **`CLAUDE.md`:** the rustdoc command under *Commands*; every path in the *Architecture*
  table; `Name` and `QualName`'s file under it; *Testing notes*, whose first bullet describes
  `tests/compiler_tests.rs` and whose second gives the `mod support;` and `#[path]` rules.
  The commands themselves do not change.
- **`.claude/skills/` and `.claude/memory/`:** every `src/compiler/` path, and
  `write-spec-chapter`'s `use zelkova_lang::compiler::parser;`.
- **`tools/`:** the comments in `tools/spec-doc/src/lib.rs` and
  `tools/spec-site/src/render.rs` that name `tests/spec.rs`.
- **`docs/`:** the **Location** line of every open ticket, and every `src/` or `tests/` path
  in `docs/spec/`, `docs/decisions/` and `README.md`. A tombstone row and a closed ticket's
  text are history and are left alone.
- **`std/test/src/Test.zel`** names `src/compiler/test_runner.rs` in a doc comment.

### Order of work

Each step is one commit and leaves `cargo test --workspace` green.

1. `zelkova-syntax`: move its files, write its manifest, and have the old crate depend on it.
2. `zelkova-compiler`: move the old crate's remaining checking modules into it, with the
   tests that go with it. The old crate, now the backend, the runners, the driver and
   `main.rs`, depends on it, and its `src/lib.rs` declares what is left until step 4.
3. `zelkova-js`, then `zelkova-test-runner`, the same way.
4. What is left is `zelkova`: move it under `crates/`, and make the root manifest virtual.
5. CI, the site, `CLAUDE.md`, the skills and the tickets.

**Acceptance:**

- `cargo test --workspace`, `cargo clippy --workspace --all-features -- -D warnings`,
  `cargo fmt --all --check` and
  `RUSTFLAGS="-D warnings -W unreachable-pub" RUSTDOCFLAGS="-D warnings" cargo doc --no-deps
  -p zelkova-syntax -p zelkova-compiler -p zelkova-js -p zelkova-test-runner -p zelkova` are
  all green, and `CLAUDE.md` gives that rustdoc command.
- The number of tests `cargo test --workspace` runs is the number it ran on the commit before
  this ticket's first.
- `cargo run -- compile std/core` prints `parsed 10 modules`, lists all ten as checked, and
  exits 0. `cargo run -- test std/core` reports the count `CLAUDE.md` records.
  `node --test 'tests/js/**/*.mjs'` passes.
- `cargo tree -p zelkova-syntax` shows no other workspace crate. `cargo tree
  -p zelkova-compiler` shows `zelkova-syntax` and none of `zelkova-js`, `zelkova-test-runner`,
  `zelkova` or `clap`. `cargo tree -p zelkova-js` shows neither `zelkova-test-runner` nor
  `zelkova`.
- No `src/` or `tests/*.rs` remains at the repository root, and `tests/` there holds
  `fixtures/` and `js/` only.
- `cargo run -p spec-site -- --out site` renders, `site/index.html` links
  `api/zelkova_compiler/`, and after `cp -r target/doc/. site/api/` the file
  `site/api/zelkova_compiler/index.html` exists.
- `grep -rnE "src/compiler/|zelkova_lang|zelkova-lang --no-deps|[^/]tests/[a-z_]+\.rs"
  CLAUDE.md README.md .claude .github tools std crates docs/spec docs/decisions` finds
  nothing, and the same pattern over `docs/tickets` finds only tombstone rows. The last
  alternative is a root-level test file: a path under `crates/` has a `/` before `tests/`.
- `git diff --stat -M` for the whole ticket shows every file under `crates/` as a rename,
  except the five new `Cargo.toml`s, `zelkova-syntax`'s `src/lib.rs`, the two halves of
  `name.rs`, and `crates/zelkova-syntax/tests/parser_tests.rs`.
