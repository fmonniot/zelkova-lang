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
cargo test --workspace         # full suite: unit tests + tests/ + the tools/ crates'
cargo build
cargo run -- compile std/core  # compiles std/core — the de-facto smoke test
cargo run -- test <dir>        # compiles a package and its tests, runs them under node
cargo run -- run <dir>         # compiles a program and runs its `main` under node
cargo run -p spec-site -- --out site   # renders the site locally; open site/index.html
cargo fmt --all
cargo clippy --workspace --all-features
```

Bare `cargo test` runs only the compiler's own tests and silently skips `tools/spec-site`'s —
use `--workspace`. `tools/spec-doc` carries no tests of its own; its logic is exercised through
`crates/zelkova-compiler/tests/spec.rs`, which depends on it.

`cargo run -- compile std/core` prints `parsed 10 modules`, then lists all ten as checked, and
**exits 0**. It is a genuine pass/fail smoke test: any error, any module missing from the
checked list, a parse failure or a panic is a regression you introduced.
`crates/zelkova/tests/pipeline.rs::stdlib_package_compiles` pins the same thing as a test. A bare `zelkova` or
`cargo run` with no subcommand compiles nothing — it prints usage and exits non-zero, clap's
default for a missing required subcommand.

`cargo test` never loads a `.mjs` companion. `std/core`'s Zelkova tests run with `cargo run --
test std/core`. A companion's checks belong to the package that ships it, as a test facade
under that package's own `tests/` root — `std/core/tests/Js/` holds the ones that exist — and
run as that package's Zelkova tests. Where such a file goes and why is [*Testing a
companion*](docs/spec/interop.md#testing-a-companion); each file's header says what it covers.
What the *compiler* emits is checked by running it under `node --test 'tests/js/**/*.mjs'`: each
file there compiles a fixture under `tests/fixtures/` itself, with `cargo run`, and loads the
output or runs its tests. CI's `javascript` job runs both, in that order.

`cargo run -- test std/core` currently reports **`98 tests: 98 passed, 0 failed, 0 errored`
and exits 0**. `std/core/tests/FloatTests.ignored` is excluded from that count by its extension:
`Basics.add` sends a `Float` through the `addInt` facade and its boundary check aborts, so
`FloatTests`' two tests fail to load and are disabled until [`BUG-44`](docs/tickets/bug-44.md)
closes, tracked there rather than left red in CI. Any error, failure, or a different count from
what's left is a regression you introduced.

`.github/workflows/rust.yml` gates a PR on `fmt`, on `clippy`, and on a `rustdoc` job that
builds the crates' docs with the flags `rustdoc.yml` deploys them with. The clippy job passes
no `-D warnings`, so it fails on a clippy error and only annotates a warning: run
`cargo clippy --workspace --all-features -- -D warnings` locally to catch those. To reproduce
the `rustdoc` job locally:

```sh
RUSTFLAGS="-D warnings -W unreachable-pub" RUSTDOCFLAGS="-D warnings" cargo doc --no-deps -p zelkova-syntax -p zelkova-compiler -p zelkova-js -p zelkova-test-runner -p zelkova
```

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

The published site — the landing page, the rendered spec, and the rustdoc, deployed from
`.github/workflows/rustdoc.yml` — is built by [`tools/spec-site/`](tools/spec-site/src/main.rs),
which shares its `expect=` scanner with `crates/zelkova-compiler/tests/spec.rs` via `tools/spec-doc/`.

Do not leave a `TODO` comment in code for anything worth a ticket. A comment in a file nobody
opens is not a record. (The codebase still has plenty of pre-existing ones; don't add more.)

`.claude/skills/` holds the skills that drive both loops: `create-ticket`, `work-ticket`,
`review-pr` and `fix-pr-comments` change the compiler; `write-spec-chapter` and `prose-pass`
specify the language. Which model the first three spawn their agents on is
[`.claude/model-policy.md`](.claude/model-policy.md), not each skill's own judgment: the default
is sonnet everywhere, opus needs one of that file's named triggers, and an agent that hits a
decision its ticket does not make stops and escalates rather than guessing. Both spec skills —
and any session writing spec prose without one — are held to
[`docs/spec/conventions.md`](docs/spec/conventions.md), whose wording rules have no test behind
them.

## Architecture

The compiler is five crates under `crates/`, each depending only on those before it:
`zelkova-syntax`, `zelkova-compiler`, `zelkova-js`, `zelkova-test-runner`, and `zelkova` (the
driver and the binary). The root `Cargo.toml` is a virtual manifest.

The pipeline is documented at the top of `crates/zelkova-compiler/src/lib.rs`. `check_package`
is pointed at a package directory and checks that package and everything it depends on,
printing and writing nothing; `crates/zelkova/src/lib.rs`'s `compile_package` is what emits,
writes and reports what
it found. `compile_in_build` is one package of that build, and `check_module` runs the per-module
phases and hands back a `CheckedModule` — the `canonical::Module` an `Interface` is built
from, beside the `ir::Module` a backend will read.

| Phase | Where | State |
|---|---|---|
| Manifest | `crates/zelkova-compiler/src/manifest.rs` | reads and validates `zelkova.toml` before anything else; the package's name and its two source roots come from it. Errors are `CompilationError::Manifest` and go back unrendered |
| Package resolution | `crates/zelkova-compiler/src/resolve.rs` | follows `dependencies`, plus the root package's `test-dependencies`, to the other packages (`path` sources only — a `git` one is reported, not fetched) and orders them dependencies-first; then, per package, builds the one map of module names it can import and reports a name two modules both answer to. Steps below run once per package |
| Source loading | `crates/zelkova-compiler/src/source/` | walks a package's two source roots for `.zel`, maps each path to a module name under its own root. `tests/` is walked for the package whose tests were asked for and for no other |
| Tokenizing | `crates/zelkova-syntax/src/parser/tokenizer.rs` | hand-written, Unicode-aware lexer producing `Spanned<Position, Token>` |
| Layout | `crates/zelkova-syntax/src/parser/layout.rs` | offside rule; injects `OpenBlock`/`CloseBlock`. 2-space indent, no tabs |
| Parsing | `crates/zelkova-syntax/src/parser/grammar.lalrpop` | LALRPOP grammar → `parser::Module`. Compiled by `crates/zelkova-syntax/build.rs` |
| Dependency resolution | `crates/zelkova-compiler/src/dependencies.rs` | petgraph; Tarjan SCC for cycles; yields a topological order |
| Canonicalization | `crates/zelkova-compiler/src/canonical/` | resolves imports against `Interface`s, qualifies names, validates exports → `canonical::Module` |
| Type checking | `crates/zelkova-compiler/src/typer/` | Hindley–Milner: `annotate.rs` → `constraint.rs` → `unifier.rs`. **Wired into `check_module`** |
| Exhaustiveness | `crates/zelkova-compiler/src/exhaustiveness.rs` | **stub** — `check` inspects nothing and accepts every module. `Error::NonExhaustiveMatch` exists and renders, but nothing constructs it yet |
| Backend IR | `crates/zelkova-compiler/src/ir/` | the shape a backend reads: a type on every node, the four kinds of name apart, arity, saturation and a constructor's place in its declaration. `ir::build` turns the canonical module and what the typer solved into one `ir::Module`. Its module doc comment is where the WebAssembly constraints are written, and is what to read before changing the shape |
| Code generation | `crates/zelkova-js/src/lib.rs`, `crates/zelkova-js/src/output.rs` | `zelkova_js::emit` turns one `CheckedModule` into the text of an ES module. Once the whole build has checked, `zelkova::compile_package` emits every module and writes them, the runtime and each facade's companion to `build/out/js/` beside the root manifest — or nothing, if anything failed; `zelkova-js`'s *Paths* section is the layout. It emits every module of `std/core`, `case` included, and an `unsafe` facade's forwarding code runs its companion's result through the predicate of the declared type (*The boundary check*); an effectful facade's call site builds a `Task` over the runtime's `$effect` (`Ok`, `Threw`, `Malformed`); it refuses a facade with no companion, a facade result no predicate decides, a module holding a declaration the typer could not check, and a declaration holding a name that did not resolve. Its module doc comment has the shape, the representations and the call rule |

`Name` (`crates/zelkova-syntax/src/name.rs`, re-exported by `zelkova_compiler::name`) is an
unqualified identifier; `QualName` (`crates/zelkova-compiler/src/name.rs`) is one that carries
its module. Everything after parsing should be reaching for `QualName`.

## Standing invariants

These outlive any single ticket. Each is here because breaking it produced a bad diff. Where a
bullet points at a doc comment, that comment is the full account — read it before changing what
it describes.

- **No `panic!`, `unwrap()`, `expect()` or `todo!()` on a non-test path.** Return a phase
  `Error` and let the caller accumulate diagnostics. This was the whole subject of `ERR-1`;
  do not reintroduce it. `unwrap()` inside `#[cfg(test)]` is fine.
- **A pass that emitted an error must not report success.** `check_package` accumulates
  `CompilationError`s rather than stopping at the first one, `driver::compile_package` adds
  each to its own accumulator of `BuildError`s, and that accumulation *is* the return value:
  empty is `Ok(())`, non-empty is `Err(BuildError::Many(..))`, and `crates/zelkova/src/main.rs` exits
  non-zero on `Err`. A new failure path pushes onto that vector; nothing
  is rendered and then dropped. Rendering diagnostics and returning `Ok` regardless was
  `BUG-1`. The per-module phases have the same *shape* one level down — `canonicalize`,
  `type_check` and `exhaustiveness::check` each return `Result<_, Vec<Error>>`, so one broken
  declaration cannot hide the next — but that is a claim about the shape only, and the
  architecture table above is the accurate account of how much each phase actually finds.
  `check_module` tags each vector with the module's `Name`, because a phase only ever sees one
  module.
- **An error has to describe itself, and say where.** Every phase error implements `PhaseError`
  (`crates/zelkova-compiler/src/lib.rs`): a `message()` written in the vocabulary of the user's source, plus
  optional `notes()` and `labels()`. `CompilationError::as_diagnostic`, and the two functions
  it shares with `driver::BuildError::as_diagnostic` (`phase_diagnostic` and
  `plain_diagnostic`), are the only places a `codespan_reporting::Diagnostic` is ever built,
  which is exactly why `format!("{:?}", e)` in a note is not an option — a `Debug` dump names Rust types, not source constructs. Read
  `PhaseError`'s doc comment before adding a variant, and `Origin`, `Constraint` and the head
  of `typer/constraint.rs` before touching how a type error is blamed. One rule spans both and
  is stated in neither: a group error (`Error::Many`, `EnvironmentErrors`) must flatten its
  members' labels the way it flattens their messages, or it silently drops every caret it
  swallowed.
- **A grammar change is never a one-file change.** `grammar.lalrpop`, the `parser` AST in
  `parser/mod.rs`, and the `from_parser*` conversions in `canonical/mod.rs` move together, in
  the same commit. Splitting them leaves the tree uncompilable or, worse, silently dropping a
  construct during canonicalization.
- **Tuples are size 2 or 3 only**, matching Elm, and `Tuple<T>` (`crates/zelkova-syntax/src/tuple.rs`) is
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
  [`DEC-12`](docs/decisions/dec-12.md) behind it. A facade signature naming a type variable or a
  function type is rejected, an unmarked signature's result must be exactly
  `Task (Result Failure a)`, and `Task` may appear nowhere else in a facade signature.
- **A doc comment describes what the code at that site does** — not what you intended, and
  not what it used to do. An overstated comment is a real defect because it is what the next
  reader trusts. Prefer saying less over saying more than you verified.

## Testing notes

- Each crate's integration tests are under its own `tests/`, one binary per top-level file.
  The parser tests are the exception: `crates/zelkova-syntax/tests/parser_tests.rs` declares
  the `tests/parser/` submodules, and a new file there has to be registered in it or it never
  runs.
- `crates/zelkova-compiler/tests/support/mod.rs` holds the shared helpers — `test_package()`, `parse_source()`,
  `canonicalize_standalone()`, `canonicalize_with_interfaces()`, `maybe_interface()`,
  `basics_interface()`, `char_interface()`. Reach for these before writing a new harness. A
  type name that resolves to nothing is a canonicalization error, so a standalone module
  checked against an empty interface map cannot name `Int`, `Char` or `Bool` at all — put
  `basics_interface()` and `char_interface()` in the map, which is also what makes a bare
  `Int` the scalar `Basics.Int` rather than some other declaration of the same spelling. The
  test binaries of `crates/zelkova-compiler/tests/` get them with a plain `mod support;`;
  `crates/zelkova-js/tests/javascript.rs` and `crates/zelkova/tests/pipeline.rs` need
  `#[path = "../../zelkova-compiler/tests/support/mod.rs"]`. A test that builds a path to
  `std/`, `tests/fixtures/` or `docs/` from `CARGO_MANIFEST_DIR` joins `../..` first.
- Five layers exist: `crates/zelkova-compiler/tests/canonical.rs` (source string →
  `canonical::Module` assertions), `crates/zelkova-compiler/tests/typer.rs` (source string →
  expected type or expected error), `crates/zelkova/tests/pipeline.rs` (`check_module`
  end-to-end, including on real `std/core/src/` modules), `crates/zelkova-compiler/tests/ir.rs`
  (source string → the `ir::Module` a backend reads), and `crates/zelkova-js/tests/javascript.rs`
  (source string → the JavaScript text it emits as).
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
… of`, `if/then/else`, function declarations with annotations, infix declarations, tuples, the
unit type `()` (checked and emitted, as `undefined` on JavaScript), JS interop via facades with
companion `.mjs` files, single-line string literals, `--` and `{- -}` comments.

Not implemented: multi-line `"""` string literals, `let … in`, lambdas, records, lists,
negative literals, type aliases, and a `Task` that does more than `succeed` and `map`
(`std/core` declares `Task`,
`Failure`, `succeed` and `map`; `zelkova run` runs a package's `main`, and `zelkova test` runs
a `Test` that holds a `Task`).
**Multi-clause function declarations** — a deliberate
divergence from Elm — parse but are rejected by canonicalization
(`Error::MultipleBindingsUnsupported`); `LANG-20` is the ticket. The standard library under
`std/core/src/` carries `.ignored` files for modules that do not compile yet.

`number`, `comparable` and `appendable` are **ordinary type variables** and always were — the
compiler never special-cased them, and `std/core/src/` now spells all three `a`. **Type
classes**, without higher-kinded variables, are what replaces them:
[`docs/spec/type-classes.md`](docs/spec/type-classes.md) specifies the mechanism,
[`DEC-2`](docs/decisions/dec-2.md) holds the eleven decisions behind it, and the LANG-37
through LANG-42 program, plus LANG-70 and LANG-71, in
[`docs/tickets/README.md`](docs/tickets/README.md) carries the order the eight implementing
tickets have to land in. Read the chapter before touching any of it.

One of its rules constrains diffs outside that program today: **`class` and `instance` become
reserved, and `where` becomes reserved as a type variable.** All three are ordinary identifiers
now, so this is a breaking change — and `instance C T where …` currently *misparses* as a
function declaration named `instance` rather than being rejected. Four more cross-cutting rules
— instance placement, `derived` bodies, no constraint on a facade signature, and the two
constraints codegen inherits — are stated in the chapter.
