# TOOL-5 · The compiler, its JavaScript backend and its command line are one crate

**Sizing:** medium to large. Most of it is moving files, but every path the repository writes
down moves with them. It grows with every open ticket whose **Location** it invalidates, which
is why it goes after [`TOOL-2`](tool-2.md) through [`TOOL-4`](tool-4.md) and not before.

**Part of:** the *Active work: editor support* section of [the index](README.md). Nothing
depends on it: [`TOOL-6`](tool-6.md) can start as a workspace member depending on today's
`zelkova-lang` library.

**Location:** `Cargo.toml` (`[workspace] members` holds only `tools/spec-doc` and
`tools/spec-site`); `src/lib.rs`; `src/main.rs`; `build.rs`; `src/compiler/` as a whole, and in
particular `program_runner.rs` and `test_runner.rs` (the two modules that spawn `node`),
`javascript.rs` and `output.rs` (the backend), and `parser/` and `position.rs` (the syntax).

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

**Approach:** the split proposed when this effort was scoped. It is a proposal, and the crate
boundaries are the thing to review:

| Crate | Holds | Depends on |
|---|---|---|
| `zelkova-syntax` | `tokenizer`, `layout`, `grammar.lalrpop` and `build.rs`, the `parser` AST, `position`, `name`, `tuple` | — |
| `zelkova-compiler` | `manifest`, `resolve`, `source`, `dependencies`, `canonical`, `typer`, `exhaustiveness`, `ir`, `default_imports`, `scalars`, `CompilationError`/`PhaseError`, `check_module` | syntax |
| `zelkova-js` | `javascript`, `output`, the `runtime/js` it `include_str!`s | compiler |
| `zelkova` (bin) | `main.rs`, `program_runner`, `test_runner`, `test_collection`, `program` | compiler, js |

The canonicalizer, typer and IR stay one crate. They share `CheckedModule`, `Interface`,
`QualName` and `PhaseError`, and splitting them would turn the internals they share into public
API. Where each of `test_collection` and `program` lands depends on what they read, which is
to be checked when the move is made.

What moves with it, each of which is part of this ticket, not a follow-up:

- **CI and the site name the crate.** `.github/workflows/rust.yml`'s `rustdoc` job and
  `rustdoc.yml` run `cargo doc -p zelkova-lang`. `tools/spec-site/assets/index.html` links
  `api/zelkova_lang/`, and `tools/spec-site/src/main.rs`'s redirect points at
  `zelkova_lang/index.html`. Keeping the package name `zelkova-lang` for the compiler crate
  avoids all four. Renaming it means updating all four and deciding what the published rustdoc
  landing page is once there are several crates.
- **Tests move with their crate.** `tests/cli.rs` uses `CARGO_BIN_EXE_zelkova`, so it has to
  live in the binary's package. `tests/typer.rs`, `tests/ir.rs`, `tests/pipeline.rs`,
  `tests/compiler/` and `tests/spec.rs` go with the compiler, and `tests/javascript.rs` with
  the backend. `tests/support/mod.rs` either follows them or becomes a dev-only crate.
- **Relative paths are depth-sensitive.** `javascript.rs` does
  `include_str!("../../runtime/js/zelkova.mjs")`, and 144 doc comments under `src/` link into
  `docs/` with `../`-relative paths whose depth changes with the move. Whether anything checks
  those links is to be established, not assumed.
- **The written record names paths.** `CLAUDE.md`'s *Architecture* table and *Testing notes*,
  `.claude/skills/`, and the **Location** line of every open ticket.

Not decided here: whether the crates live under `crates/` or at the top level, whether the
compiler keeps the `zelkova-lang` package name, and whether this is the moment to move off
`edition = "2018"`. The last one is better done as its own commit either way.

**Acceptance:**

- `cargo test --workspace`, `cargo clippy --workspace --all-features -- -D warnings`,
  `cargo fmt --all --check` and the local rustdoc command in `CLAUDE.md` (with the new `-p`
  if it changed) are all green.
- `cargo run -- compile std/core` (or its new `-p zelkova` spelling, documented in
  `CLAUDE.md`) prints `parsed 10 modules`, lists all ten as checked, and exits 0.
  `cargo run -- test std/core` reports the same count as before the move.
- `zelkova-syntax` does not depend on `zelkova-compiler`, and `zelkova-compiler` does not
  depend on `zelkova-js`. `cargo tree -p zelkova-compiler` shows neither `clap` nor the backend.
- `cargo run -p spec-site -- --out site` renders, and its rustdoc link resolves once the docs
  are mounted the way `rustdoc.yml` mounts them.
- `grep -rn "src/compiler/" CLAUDE.md .claude docs/tickets` finds no path that no longer
  exists.
