# CLAUDE.md

Zelkova is a compiler for an Elm-inspired functional language, written in Rust. Source files
use the `.zel` extension. The eventual target is WebAssembly, with JavaScript as an
intermediate target because it is easier to integrate with. This is a learning project about
compiler construction — correctness and understanding matter more than production polish.

The name: zelkova trees are in the same family as elm trees.

**This file routes; it does not explain.** A fact earns a line here only when no source file,
spec chapter or ticket would naturally carry it, *and* a session that does not know it writes a
bad diff. Everything else is written at its site and linked from here in a sentence. When you
promote a lesson, that is the order to try: a doc comment at the code site, then a spec chapter
or a decision entry, then this file. Two copies of a rule drift apart, and the copy nobody
maintains is the one the next reader trusts.

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
failure or a panic is a regression you introduced. `tests/pipeline.rs::stdlib_package_compiles`
pins the same thing as a test.

Note that `.github/workflows/rust.yml` marks the `fmt` and `clippy` jobs `continue-on-error:
true`, so **CI does not actually gate on them**. Run both locally; a red clippy will not be
caught for you.

## Where work is tracked

Three directories, each with an index to read before touching it:

- [`docs/tickets/`](docs/tickets/README.md) — open work, one markdown file per ticket. The
  index carries the conventions, the open list, and a dated tombstone row for everything
  closed. Read it before proposing work.
- [`docs/spec/`](docs/spec/README.md) — what the *language* is, as opposed to the compiler
  that implements it. Normative. Every code example, link and anchor in it is checked by
  `cargo test --test spec`, so renaming a chapter header or deleting a ticket file a chapter
  cites turns that suite red.
- [`docs/decisions/`](docs/decisions/README.md) — *why* a rule is what it is, and what it was
  chosen over. Not normative, but its links and anchors are checked by the same binary. A
  chapter states its rule without arguing against the alternatives; the argument lives here.

Do not leave a `TODO` comment in code for anything worth a ticket. A comment in a file nobody
opens is not a record. (The codebase still has plenty of pre-existing ones; don't add more.)

`.claude/skills/` holds the skills that drive both loops: `create-ticket`, `work-ticket`,
`review-pr` and `fix-pr-comments` change the compiler; `write-spec-chapter` and `prose-pass`
specify the language. Both spec skills — and any session writing spec prose without one — are
held to [`docs/spec/conventions.md`](docs/spec/conventions.md), whose wording rules have no
test behind them.

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

These outlive any single ticket. Each is here because breaking it produced a bad diff. Where a
bullet points at a doc comment, that comment is the full account — read it before changing what
it describes.

- **No `panic!`, `unwrap()`, `expect()` or `todo!()` on a non-test path.** Return a phase
  `Error` and let the caller accumulate diagnostics. This was the whole subject of `ERR-1`;
  do not reintroduce it. `unwrap()` inside `#[cfg(test)]` is fine.
- **A pass that emitted an error must not report success.** `compile_package` accumulates
  `CompilationError`s rather than stopping at the first one, and that accumulation *is* the
  return value: empty is `Ok(())`, non-empty is `Err(CompilationError::Many(..))`, and
  `src/main.rs` exits non-zero on `Err`. A new failure path pushes onto that vector; nothing
  is rendered and then dropped. Rendering diagnostics and returning `Ok` regardless was
  `BUG-1`. The per-module phases have the same *shape* one level down — `canonicalize`,
  `type_check` and `exhaustiveness::check` each return `Result<_, Vec<Error>>`, so one broken
  declaration cannot hide the next — but that is a claim about the shape only, and the
  architecture table above is the accurate account of how much each phase actually finds.
  `check_module` tags each vector with the module's `Name`, because a phase only ever sees one
  module.
- **An error has to describe itself, and say where.** Every phase error implements `PhaseError`
  (`src/compiler/mod.rs`): a `message()` written in the vocabulary of the user's source, plus
  optional `notes()` and `labels()`. `CompilationError::as_diagnostic` is the only place a
  `codespan_reporting::Diagnostic` is ever built, which is exactly why `format!("{:?}", e)` in
  a note is not an option — a `Debug` dump names Rust types, not source constructs. Read
  `PhaseError`'s doc comment before adding a variant, and `Origin`, `Constraint` and the head
  of `typer/constraint.rs` before touching how a type error is blamed. One rule spans both and
  is stated in neither: a group error (`Error::Many`, `EnvironmentErrors`) must flatten its
  members' labels the way it flattens their messages, or it silently drops every caret it
  swallowed.
- **A grammar change is never a one-file change.** `grammar.lalrpop`, the `parser` AST in
  `parser/mod.rs`, and the `from_parser*` conversions in `canonical/mod.rs` move together, in
  the same commit. Splitting them leaves the tree uncompilable or, worse, silently dropping a
  construct during canonicalization.
- **Tuples are size 2 or 3 only**, matching Elm, and `Tuple<T>` (`src/compiler/tuple.rs`) is
  where that rule is written down — in the *shape* of the type rather than in a check, so no
  other arity is representable. Don't reintroduce a `Vec` or an `Option`-shaped third element
  on either AST: three separate arity checks that disagreed was `AST-2`. The module's doc
  comment has the rest.
- **A `Result`-yielding iterator must advance or stop — never repeat one error.** When you add
  an error path to a pipeline iterator, either consume input or stop. Fully draining one that
  did neither once consumed ~20GB of RAM before the OS killed it (`BUG-4`); the tokenizer had
  the same defect on tabs (`BUG-5`). `layout()` returns a `FusedIterator` and says why in its
  doc comment.
- **Zelkova has no `Elm.Kernel.*`.** A std module that needs a JS primitive gets a **facade**
  plus a companion `<Name>.mjs`; `Js/Basics`, `Js/Utils` and `Js/Bitwise` are the worked
  examples. Most of the `.ignored` modules under `std/core/src` still carry Elm's kernel
  imports verbatim, and porting one means writing its facade rather than resurrecting the
  kernel. What a facade *is* — the `Task`/`unsafe` split, which types may cross, the
  plain-parameter-list guarantee the companion makes — is
  [`docs/spec/interop.md`](docs/spec/interop.md), with [`DEC-11`](docs/decisions/dec-11.md) and
  [`DEC-12`](docs/decisions/dec-12.md) behind it. None of that is implemented: the compiler
  spells the modifier `module javascript` where the spec says `foreign`
  ([`LANG-54`](docs/tickets/lang-54.md)), and `unsafe` is [`LANG-53`](docs/tickets/lang-53.md).
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
- `NodeSpan`'s `PartialEq` always returns `true`, so a whole-value `assert_eq!` proves nothing
  about position; assert on `.span` or on `diagnostic.labels[..].range` directly. The type's
  doc comment says why it is blind.
- **A green test proves nothing until you have seen it fail.** For each test you add,
  neutralise the behaviour it is meant to pin — revert the one line that constitutes the fix,
  delete the new branch — re-run *that* test, confirm it goes red, then restore. Tests that
  pass both ways are the most common review finding there is.

## Language notes

[`docs/spec/`](docs/spec/README.md) is the normative record of what Zelkova *is*. This section
is only a status check on the compiler as it stands today.

Implemented: modules with `exposing`/`import`/`as`, union types, pattern matching via `case
… of`, `if/then/else`, function declarations with annotations, infix declarations, tuples, JS
interop via facades with companion `.mjs` files, `--` and `{- -}` comments.

Not implemented: string literals, `let … in`, lambdas, records, lists, negative literals, the
unit type, type aliases, effects (`Task`, and the `main` and test discovery built on it), and
the `zelkova.toml` package manifest. **Multi-clause function declarations** — a deliberate
divergence from Elm — parse but are rejected by canonicalization
(`Error::MultipleBindingsUnsupported`); `LANG-20` is the ticket. The standard library under
`std/core/src/` carries `.ignored` files for modules that do not compile yet.

`number`, `comparable` and `appendable` are **ordinary type variables** and always were — the
compiler never special-cased them, and `std/core/src/` now spells all three `a`. **Type
classes**, without higher-kinded variables, are what replaces them:
[`docs/spec/type-classes.md`](docs/spec/type-classes.md) specifies the mechanism,
[`DEC-2`](docs/decisions/dec-2.md) holds the eleven decisions behind it, and the LANG-37
through LANG-42 program in [`docs/tickets/README.md`](docs/tickets/README.md) carries the order
the six implementing tickets have to land in. Read the chapter before touching any of it.

One of its rules constrains diffs outside that program today: **`=>`, `class` and `instance`
become reserved, and `where` becomes reserved as a type variable.** All four are ordinary
identifiers now, so this is a breaking change — and `instance C T where …` currently
*misparses* as a function declaration named `instance` rather than being rejected. Four more
cross-cutting rules — instance placement, `derived` bodies, no constraint on a facade
signature, and the two constraints codegen inherits — are stated in the chapter.
