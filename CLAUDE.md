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
[`docs/tickets/README.md`](docs/tickets/README.md). Read that index before proposing work — it
carries the conventions, the open list, and a dated tombstone row for everything already
closed.

Do not leave a `TODO` comment in code for anything worth a ticket. A comment in a file nobody
opens is not a record. (The codebase still has plenty of pre-existing ones; don't add more.)

The `.claude/skills/` directory holds the skills that drive this loop: `create-ticket`,
`work-ticket`, `review-pr`, `fix-pr-comments`. A fifth, `write-spec-chapter`, drives the
separate loop below — specifying the language rather than changing the compiler.

The language itself — as opposed to the compiler that implements it — is specified under
[`docs/spec/`](docs/spec/README.md). It is normative and every code example in it is checked
against the compiler by `cargo test --test spec`; see that index before writing prose about
what Zelkova's syntax or semantics are.

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
  variables, both of which are gaps in the typer rather than mistakes in the source — its doc
  comment says which and why; `exhaustiveness::check` is a stub that finds nothing).
  `check_module` tags each vector with the module's `Name` — a phase never carries it, because
  a phase only ever sees one module.
- **An error has to describe itself.** Every phase error implements `PhaseError`
  (`src/compiler/mod.rs`): a `message()` written in the vocabulary of the user's source, plus
  optional `notes()`. `CompilationError::as_diagnostic` is the only place a
  `codespan_reporting::Diagnostic` is ever built and it composes those two — it has no
  phase-specific knowledge to fall back on, which is exactly why `format!("{:?}", e)` in a
  note is not an option: a `Debug` dump names Rust types, not source constructs. A new error
  variant gets a message written for the person reading it.
  An error also says *where*, when it can: `PhaseError::labels` returns `SpanLabel`s, and
  `grammar.lalrpop` captures `@L`/`@R` in every production that builds a node, so every
  declaration, expression, pattern and type carries a `NodeSpan`. Both ASTs hold it the same
  way: `Expression`, `Pattern` and `Type` are a `span` field beside a `…Kind` enum, so
  children stay `Box<Expression>` and a reader matches `&e.kind`. A canonicalization error
  that names an identifier — `VariableNotFound`, `VariantNotFound` — therefore puts the caret
  under that name and not under the declaration around it.
  One thing deliberately carries no span, and says so at its definition:
  `canonical::Type`/`TypeConstructor`, because they are cloned out of an `Environment` and may
  have been written in another file — `ERR-5` made that half solvable, but
  `Type::from_parser_type` still discards `parser::Type`'s per-node spans by choice and nobody
  has written the walk that would keep them. `labels` still defaults to empty for any error
  raised while walking a node the grammar does not span, and that is a real answer, not a
  stub — such an error renders with no caret; `parser::Exposed` (`ERR-9`) is no longer an
  example of one, so a new error naming an exposing-list entry should carry its span rather
  than reach for this fallback. A group (`Error::Many`, `EnvironmentErrors`) flattens its
  members' labels, the way it already flattens their messages; forgetting that silently drops
  every caret it swallowed.
  The typer is the one phase that does not check the canonical AST — it translates it into
  its own `Term`/`Constraint` language — so it carries the spans across: every `Term` and
  `TermPattern` keeps the canonical node's `NodeSpan`, every `Constraint` records an `Origin`
  (the span it came from plus a `Reason` naming *why* two types had to match), and `unify`
  reports the origin of the constraint it failed on. A type error therefore points at the
  sub-expression, with a secondary label under the annotation that explains what was
  expected. Three things that follow, and that a change here must keep: constraints live in a
  `Vec` and not a `HashSet`, because deduplication drops provenance and an unordered
  collection makes *which* error is reported vary between runs; a term's own constraints
  are collected before its children's, so an expected type is substituted inward before the
  inner constraints are solved — collect children first and the caret moves back out to the
  whole declaration, and `constraint::collect` has no exception to this, `Case` included; and
  provenance is tracked *per side* of a constraint. That last one is the subtle one. When
  `unify` solves `t := T`, `T` was read off one side, and only a substitution that rewrote
  *that* side put it there — a constraint whose other side was rewritten is relaying a type,
  not introducing one, and crediting it names a line the reader cannot act on. `result :
  Bool` / `result = not 42` blaming the annotation, when `42` has to be a `Bool` because of
  `not`'s own type, is what a side-blind version of this produces. A label names its `Reason`
  and never a type: by the time a constraint fails, substitution and decomposition mean
  neither of its sides is reliably the type of the text under the caret.
  A phase still never knows the `SourceFileId` of the module it is checking — that half is
  unchanged, and `compile_package` still attaches it via `CompilationError::InFile`, being the
  only place that knows which file a module was read from. What `ERR-5` added is the other
  half: a diagnostic that also names something written in a *different* module. `Interface`
  (`src/compiler/mod.rs`) carries a `file: Option<SourceFileId>`, passed to
  `canonical::Module::to_interface` by `dependencies::ModuleWalker::check_in_order` — driver
  code, not a phase, the same as `compile_package` — once a module has checked and its
  interface is about to go into the shared map; `Interface::values` pairs each value's `Type`
  with the `NodeSpan` of the declaration it is the type of, and `Interface::source_span`
  combines that span with `file` into a `SourceSpan` when both are known. A phase error offers
  one of those to a diagnostic through `SpanLabel::file: Option<SourceFileId>` — `None`
  (everything built before `ERR-5`) means "in the module under check" and falls back on the
  file `compile_package` supplies at render time; `Some` renders in the label's own file and
  needs no fallback at all. `canonical::Error::AmbiguousVariables` is the worked example: its
  primary label sits in the module doing the ambiguous import, and it carries one secondary
  label per candidate, each in that candidate's own file.
  `NodeSpan`'s `PartialEq` always returns `true`, so a whole-value `assert_eq!` in the parser
  tests proves nothing about position; a test that cares about a position asserts on `.span`
  or on `diagnostic.labels[..].range` directly.
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
  `Tokenizer` had the same class of defect: `handle_indentation`'s `Some('\t')` arm returned
  `TabError` without consuming the tab or clearing `at_line_start`, so it repeated the same
  error forever. Fixed by advancing past the tab and clearing the flag before returning,
  mirroring how the sibling `IndentationError` already recovers (`BUG-5`).
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

What Zelkova's syntax and semantics *are* is specified under `docs/spec/`, not here — see
*Where work is tracked*, above. This section stays only as a quick implemented/not-implemented
status check for the compiler as it stands today; `docs/spec/` is the normative record and the
place to look for anything beyond that split, including open design questions.

Implemented: modules with `exposing`/`import`/`as`, union types, pattern matching via `case
… of`, `if/then/else`, function declarations with annotations, infix declarations, tuples, JS
interop via `module javascript` facades with companion `.mjs` files, `--` and `{- -}`
comments.

Not implemented: string literals, `let … in`, lambdas, records, lists, negative literals, the
unit type, type aliases, and the `zelkova.toml` package manifest. **Multi-clause function
declarations** — a deliberate divergence from Elm — parse but are rejected by canonicalization
(`Error::MultipleBindingsUnsupported`); `LANG-20` is the ticket. The standard library under
`std/core/src/` carries `.ignored` files for modules that do not compile yet.

Settled by `SPEC-11`, then `SPEC-12`: `number`, `comparable` and `appendable` are **ordinary
type variables** and always were — the compiler never special-cased them — and `std/core/src/`
now spells all three `a` rather than implying a restriction the language cannot express. No
chapter names those three: they are covered by
[`docs/spec/types.md`](docs/spec/types.md)'s rule that no lowercase spelling means anything.
**Type classes**, without higher-kinded variables, are what replaces them, and
[`docs/spec/type-classes.md`](docs/spec/type-classes.md) specifies that mechanism rather than
merely recording the direction.

Read the chapter before touching any of it; the `CLASS-` program in
[`docs/tickets/README.md`](docs/tickets/README.md) carries the order the six implementing
tickets have to land in. Four of its rules constrain diffs outside that program:

- **`=>`, `class` and `instance` become reserved, and `where` becomes reserved as a type
  variable.** All four are ordinary identifiers today, so this is a breaking change — and
  `instance C T where …` currently *misparses* as a function declaration named `instance`
  rather than being rejected, which is why both words are reserved outright and cannot be soft
  the way `javascript` is.
- **An instance may be declared only in the module declaring its class or its type.**
- **A `module javascript` facade signature may not carry a constraint**, which is what preserves
  the plain-parameter-list guarantee
  [`docs/spec/js-interop.md`](docs/spec/js-interop.md) makes.
- **A class dictionary is erased by specialisation before code generation**, never passed — a
  constraint the first codegen work inherits.
