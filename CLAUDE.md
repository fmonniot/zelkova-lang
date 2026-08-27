# CLAUDE.md

Zelkova is a compiler for an Elm-inspired functional language, written in Rust. Source files
use the `.zel` extension. The eventual target is WebAssembly, with JavaScript as an
intermediate target because it is easier to integrate with. This is a learning project about
compiler construction — correctness and understanding matter more than production polish.

The name: zelkova trees are in the same family as elm trees.

## Commands

```sh
cargo test                     # full suite: unit tests + tests/
cargo build
cargo run                      # compiles std/core/src/ — the de-facto smoke test
cargo fmt --all
cargo clippy --all-features
```

`cargo run` prints `parsed 8 modules`, then lists all eight as checked, and **exits 0**. It is
a genuine pass/fail smoke test: any error, any module missing from the checked list, a parse
failure or a panic is a regression you introduced. (The `.ignored` files under `std/core/src`
are invisible to the source loader, which only collects `.zel` — they are not part of the
eight.) `tests/pipeline.rs::stdlib_package_compiles` pins the same thing as a test.

Note that `.github/workflows/rust.yml` marks the `fmt` and `clippy` jobs `continue-on-error:
true`, so **CI does not actually gate on them**. Run both locally; a red clippy will not be
caught for you.

## Where work is tracked

Open work lives in `docs/tickets/`, one markdown file per ticket, indexed by
[`docs/tickets/INDEX.md`](docs/tickets/INDEX.md). Read that index before proposing work — it
carries the conventions, the open list, and a dated tombstone row for everything already
closed.

Do not leave a `TODO` comment in code for anything worth a ticket. A comment in a file nobody
opens is not a record. (The codebase still has plenty of pre-existing ones; don't add more.)

The `.claude/skills/` directory holds the skills that drive this loop: `create-ticket`,
`work-ticket`, `review-pr`, `fix-pr-comments`.

## Architecture

The pipeline is documented at the top of `src/compiler/mod.rs`. `compile_package` walks a
package directory; `check_module` runs the per-module phases.

| Phase | Where | State |
|---|---|---|
| Source loading | `src/compiler/source/` | walks a package dir for `.zel`, maps paths to module names |
| Tokenizing | `src/compiler/parser/tokenizer.rs` | hand-written, Unicode-aware lexer producing `Spanned<Position, Token>` |
| Layout | `src/compiler/parser/layout.rs` | offside rule; injects `OpenBlock`/`CloseBlock`. 2-space indent, no tabs |
| Parsing | `src/compiler/parser/grammar.lalrpop` | LALRPOP grammar → `parser::Module`. Compiled by `build.rs` |
| Dependency resolution | `src/compiler/dependencies.rs` | petgraph; Tarjan SCC for cycles; yields a topological order |
| Canonicalization | `src/compiler/canonical/` | resolves imports against `Interface`s, qualifies names, validates exports → `canonical::Module` |
| Type checking | `src/compiler/typer/` | Hindley–Milner: `annotate.rs` → `constraint.rs` → `unifier.rs`. **Wired into `check_module`** |
| Exhaustiveness | `src/compiler/exhaustiveness.rs` | **stub** — `check` inspects nothing and accepts every module. `Error::NonExhaustiveMatch` exists and renders, but nothing constructs it yet |
| Code generation | — | not started |

`Name` (`src/compiler/name.rs`) is an unqualified identifier; `QualName` is one that carries
its module. Everything after parsing should be reaching for `QualName`.

## Standing invariants

These outlive any single ticket. Each is here because breaking it produced a bad diff.

- **No `panic!`, `unwrap()`, `expect()` or `todo!()` on a non-test path.** Return a phase
  `Error` and let the caller accumulate diagnostics. This was the whole subject of `ERR-1`;
  do not reintroduce it. `unwrap()` inside `#[cfg(test)]` is fine.
- **A pass that emitted an error must not report success.** `compile_package` accumulates
  `CompilationError`s rather than stopping at the first one, and that accumulation *is* the
  return value: empty is `Ok(())`, non-empty is `Err(CompilationError::Many(..))`, and
  `src/main.rs` exits non-zero on `Err`. Errors are kept typed until the end so
  `as_diagnostic` stays the single rendering point. A new failure path in `compile_package`
  pushes onto that vector; nothing is rendered and then dropped. Rendering diagnostics and
  returning `Ok` regardless was `BUG-1`.
  The per-module phases have the same *shape* one level down: `canonicalize`, `type_check`
  and `exhaustiveness::check` each return `Result<_, Vec<Error>>`, a vector rather than a
  single error, so one broken declaration cannot hide the next. That is a claim about the
  shape only — how much each phase actually puts in the vector differs, and the architecture
  table above is the accurate account (`type_check` skips unsupported constructs and unbound
  variables silently; `exhaustiveness::check` is a stub that finds nothing). `check_module`
  tags each vector with the module's `Name` — a phase never carries it, because a phase only
  ever sees one module.
- **An error has to describe itself.** Every phase error implements `PhaseError`
  (`src/compiler/mod.rs`): a `message()` written in the vocabulary of the user's source, plus
  optional `notes()`. `CompilationError::as_diagnostic` is the only place a
  `codespan_reporting::Diagnostic` is ever built and it composes those two — it has no
  phase-specific knowledge to fall back on, which is exactly why `format!("{:?}", e)` in a
  note is not an option: a `Debug` dump names Rust types, not source constructs. A new error
  variant gets a message written for the person reading it.
  Spans are settled the other way, deliberately: only `parser::Error` carries one, because
  `grammar.lalrpop` never captures `@L`/`@R` and so every phase after parsing reads an AST
  with no positions in it. Giving both ASTs spans is `ERR-3`; until it lands, adding a `span`
  field to a canonical or typer error would only move the problem to a construction site that
  has no span either. The `SourceFileId` is never a phase's business — `compile_package`
  attaches it, being the only place that knows which file a module was read from.
- **A grammar change is never a one-file change.** `grammar.lalrpop`, the `parser` AST in
  `parser/mod.rs`, and the `from_parser*` conversions in `canonical/mod.rs` move together, in
  the same commit. Splitting them leaves the tree uncompilable or, worse, silently dropping a
  construct during canonicalization.
- **Tuples are size 2 or 3 only**, matching Elm, and that rule is written down exactly once:
  `Tuple<T>` (`src/compiler/tuple.rs`) has a `Two` and a `Three` variant and nothing else. Both
  the parser and canonical ASTs hold every tuple — type, pattern and expression — in it, so no
  other arity is representable, and `grammar.lalrpop` has one production per arity, so a
  four-element tuple is a parse error that never reaches canonicalization. Don't reintroduce a
  `Vec` or an `Option`-shaped third element on either side: three separate arity checks that
  disagreed was `AST-2`. `canonical::Error::InvalidTupleSize` is kept but unconstructed today;
  it is there for a future tuple source that builds one from a list.
- **A `Result`-yielding iterator must advance or stop — never repeat one error.** `Layout`
  (`parser/layout.rs`) diagnoses an indentation violation without touching its context stack,
  so replaying the offending token would reproduce that error unchanged; it therefore fuses,
  returning `None` from `next` after any `Err`. That and its `Token::EndOfFile` termination go
  through one latch, so it is a `FusedIterator` and `layout()` says so in its signature.
  When you add an error path to a pipeline iterator, either consume input or stop. Fully
  draining one that did neither once consumed ~20GB of RAM before the OS killed it (`BUG-4`).
  `Tokenizer` has the same class of defect and does not advance either: `handle_indentation`
  returns `TabError` without consuming the tab, so it repeats forever. Tracked as `BUG-5`.
- **Zelkova has no `Elm.Kernel.*`.** A std module that needs a JS primitive gets a
  `module javascript Js.<Name>` facade — type annotations with no bodies, no infixes, no type
  declarations — plus a companion `Js/<Name>.mjs` whose exports take a plain parameter list
  rather than Elm's curried `F2`/`F3` wrappers. `Js/Basics`, `Js/Utils` and `Js/Bitwise` are
  the worked examples. Most of the `.ignored` modules under `std/core/src` still carry Elm's
  kernel imports verbatim; porting one means writing its facade, not resurrecting the kernel.
- **A doc comment describes what the code at that site does** — not what you intended, and
  not what it used to do. An overstated comment is a real defect because it is what the next
  reader trusts. Prefer saying less over saying more than you verified.

## Testing notes

- `tests/compiler_tests.rs` is the integration entry point; it declares the `tests/compiler/`
  submodules. A new file under `tests/compiler/` has to be registered there or it never runs.
- `tests/support/mod.rs` holds the shared helpers — `test_package()`, `parse_source()`,
  `canonicalize_standalone()`, `canonicalize_with_interfaces()`, `maybe_interface()`. Reach
  for these before writing a new harness. Top-level test binaries (`tests/typer.rs`,
  `tests/pipeline.rs`) get them with a plain `mod support;`; files nested under
  `tests/compiler/` need `#[path = "../support/mod.rs"]`.
- Three layers exist: `tests/compiler/canonical.rs` (source string → `canonical::Module`
  assertions), `tests/typer.rs` (source string → expected type or expected error), and
  `tests/pipeline.rs` (`check_module` end-to-end, including on real `std/core/src/` modules).
- Use `indoc!` for `.zel` source literals — the layout pass is indentation-sensitive and a
  stray leading space changes the parse.
- **A green test proves nothing until you have seen it fail.** For each test you add,
  neutralise the behaviour it is meant to pin — revert the one line that constitutes the fix,
  delete the new branch — re-run *that* test, confirm it goes red, then restore. Tests that
  pass both ways are the most common review finding there is.

## Language notes

Implemented: modules with `exposing`/`import`/`as`, union types, pattern matching via `case
… of`, `if/then/else`, function declarations with annotations, multi-line function
declarations with pattern matching (a deliberate divergence from Elm), infix declarations,
tuples, JS interop via `module javascript` facades with companion `.mjs` files, `--` and
`{- -}` comments.

Not implemented: string literals, `let … in`, lambdas, records, lists, negative literals, the
unit type, type aliases, and the `zelkova.json` package manifest. The standard library under
`std/core/src/` carries `.ignored` files for modules that do not compile yet.

Open design question, unresolved: the std library uses Elm's constrained type variables
(`number`, `comparable`, `appendable`). Whether those become real type classes, compiler-known
constraints, or nothing at all has not been decided. Don't assume an answer in a diff.
