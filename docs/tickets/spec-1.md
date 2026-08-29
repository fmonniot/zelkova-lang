# SPEC-1 · Scaffold `docs/spec/` with an executable-example harness, and write the Layout chapter

**Sizing:** medium. The harness is small — a fence scanner plus a dispatch over
`tests/support/mod.rs`'s existing helpers, no new dependency. The chapter is the real work:
Zelkova's layout rules have never been written down, so writing them means reading
`layout.rs` and `tokenizer.rs` and deciding what is a rule versus what is an accident. What
could make it bigger: that reading is likely to turn up behaviour nobody intended (see *On
what this will find*, below).

**Location:** new `docs/spec/` and new `tests/spec.rs`; existing `lang.md` (root, deleted by
this ticket); `tests/support/mod.rs` — `parse_source`, `canonicalize_standalone`,
`canonicalize_with_interfaces`, `test_package`; `src/compiler/parser/layout.rs` — `layout`,
`Layout::handle_next_token`, `Context`, `Offside`; `src/compiler/parser/tokenizer.rs` —
`Tokenizer::handle_indentation`.

**Problem:** the language has no specification, and the one document that gestures at being
one has already drifted. `lang.md` is 40 lines, opens with `TODO: Reuse Elm's language
definition/documentation`, and its single worked example is wrong:

```
lang.md:22                      module javascript Basics.Js exposing
std/core/src/Js/Basics.zel:1    module javascript Js.Basics exposing
```

Nothing reads `lang.md` and nothing checks it, so the rename went unnoticed. That is the
failure mode the whole ticket is designed against, and it is the same one
[`INDEX.md`](INDEX.md) states for tickets: two records of one decision means the unmaintained
one is what someone eventually reads. Here the compiler is the first record. A prose spec is
a second one, and it will lose unless something keeps it honest.

`README.md`'s *Documentation* section records the same gap from the other side ("next to
nothing in term of documentation"), and `cargo doc` documents the compiler's Rust, not the
language.

**Approach:**

1. **`docs/spec/`, one markdown file per chapter**, sibling to `docs/tickets/`, with an
   `INDEX.md` listing chapters and their status. The spec is **normative**: it describes
   Zelkova as designed, including constructs the compiler does not implement yet, rather than
   being a description of today's binary. `CLAUDE.md`'s *Language notes* has the current
   implemented/not-implemented split and should end up pointing at `docs/spec/` rather than
   competing with it.

2. **Every `.zel` block in a chapter carries an expectation, and `tests/spec.rs` runs it.**
   A new top-level integration binary, taking the shared helpers with a plain `mod support;`
   the way `tests/typer.rs` and `tests/pipeline.rs` do. It walks `docs/spec/*.md`, extracts
   fenced blocks whose info string names a language and an expectation, and dispatches each to
   the phase that expectation implies. Sketch, not a mandated vocabulary:

   ````
   ```zel expect=ok
   ```zel expect=parse-error
   ```zel expect=canonical-error:VariableNotFound
   ```zel expect=unimplemented
   ````

   A block with no expectation is a hard failure, not a skip — an unchecked example in a spec
   is exactly `lang.md` again. Fence scanning is a dozen lines of `str` work; do not add a
   markdown dependency for it (`Cargo.toml`'s dev-dependencies are `indoc` alone today).

3. **Write the Layout chapter**, and use it to prove the harness carries real content. Layout
   is first because it is where Zelkova is least documentable by pointing at Elm: Elm never
   specified its offside rule, and `layout.rs`'s own `Context` doc comment says it is working
   from an informal list of "surprisingly few indentation rules" that it then had to make
   precise. Two rules are already unambiguous in the tree and belong in the chapter with
   examples:

   - indentation must be a **multiple of two spaces** — `handle_indentation` returns
     `TokenizerErrorType::IndentationError` on `spaces % 2 != 0`;
   - a **tab** used for indentation is `TokenizerErrorType::TabError`, never a width.

   The rest — what `Context::CaseExpression`, `CaseBranch`, `Let` and `TopLevelDeclaration`
   each require of the lines under them, and where `OpenBlock`/`CloseBlock` land — has to be
   read out of `handle_next_token` and `Offside::min_indent`, and confirmed by writing
   examples and running them through the harness.

4. **Delete `lang.md` in this same change**, moving its JS-interop content into a
   `docs/spec/` chapter with `Js.Basics` spelled the way `std/core/src/Js/` spells it. Leaving
   both files is the defect this ticket exists to fix. Point `README.md`'s *Documentation*
   section at `docs/spec/` while there.

**Open, this ticket does not pick:**

1. **How a multi-module example is expressed.** `canonicalize_standalone` takes one source
   string, but the chapters on modules, imports and cross-module errors need two or three
   modules at once. Options: adjacent fenced blocks sharing one expectation; a block that
   names the module it defines and accumulates into an implicit package;
   `canonicalize_with_interfaces` with a hand-built `Interface`; or writing the example out to
   a temp directory and calling `compile_package` the way `tests/pipeline.rs::fixture_package`
   drives `tests/fixtures/`. The Layout chapter does not need this, so it can be decided by
   whichever chapter ticket needs it first — but the harness's dispatch should not be shaped
   so as to make it impossible.

2. **What `expect=unimplemented` asserts.** Asserting only "this fails somehow" is weak
   enough to pass for the wrong reason. Asserting a specific error pins the spec to tokenizer
   and grammar internals, so a refactor that changes which phase rejects a lambda edits the
   spec. The value of the tag is that it goes red the day the feature lands and forces the
   chapter to be updated in the same PR; pick the strictness that preserves that without
   making the doc a mirror of `parser::Error`.

**Layout rules established while grounding this ticket**, by running snippets through
`parser::parse`. The chapter states these; each is already checkable with an executable block:

- Indentation is a multiple of two spaces (`handle_indentation`, `spaces % 2 != 0` →
  `IndentationError`); a tab used for indentation is `TabError`.
- The first branch of a `case … of` fixes the column for every branch in that block
  (`Context::CaseBlock(Some(col))`, read back by `Offside::min_indent`).
- A branch body must be at least one two-space level deeper than its pattern — `CaseBranch` is
  pushed at `min_indent + 1`, so anything at or left of that closes the branch.
- Nesting works, and the scrutinee may sit on its own line: `case` / `m` / `of` on three lines
  parses.
- `let … in` is tokenized and laid out — `layout.rs` has a full `Context::Let` — but
  `grammar.lalrpop`'s `extern` token list has no `let` or `in`, so it fails as
  `UnexpectedToken { value: Let }`. This is the natural first `expect=unimplemented` example:
  it goes red the day `let` is implemented.

**Two rules decided by the language owner** while grounding this ticket, both of which the
chapter states and both of which the compiler already enforces — badly. Each has an `ERR-`
ticket for the diagnostic, and the spec block for each is `expect=parse-error`, which pins the
rule today and keeps pinning it after the message improves:

- **All branches of one `case … of` start on the same column.** A deeper line beginning a new
  branch is an error, not a continuation of the previous branch's body. Today it is absorbed
  and the parse error names the `->` two tokens later — [ERR-11](err-11.md).
- **A source file's first token is at column 1.** Any space or tab before `module` is invalid.
  Today an indented file is rejected only because a `column == 1` literal disagrees with the
  context bootstrap, and the caret lands on the second declaration — [ERR-12](err-12.md).

**On what else this will find:** the rest of `layout.rs` is likely to hold more of the same.
File those the way ERR-11 and ERR-12 were filed and specify the behaviour the chapter *should*
describe, with the example tagged accordingly — do not fix compiler behaviour in this ticket. A spec change and a semantics change in one diff is
unreviewable, and the point of the harness is that a spec claim the compiler fails is a red
test, which is a working record rather than a lost one.

**Follow-up chapters are separate tickets**, one each, in rough priority order — they are the
places Zelkova cannot defer to Elm's documentation: multi-line function declarations with
pattern matching (`CLAUDE.md` calls it "a deliberate divergence from Elm"); JS interop
(`module javascript` facades and the `.mjs` plain-parameter calling convention, versus Elm's
`F2`/`F3`); lexical structure, including the soft keywords — `tokenizer.rs` groups four of
them (`left`, `right`, `non`, `javascript`) under one comment, but `grammar.lalrpop`'s
`VarIdent` maps only the first three back to a `Name`, so `javascript` is reserved in
identifier position and the other three are not; tuples being arity 2 or 3 only (`tuple.rs` —
`Tuple<T>`). Constrained type variables
(`number`, `comparable`, `appendable`) get a chapter that records the question as open, per
`CLAUDE.md`'s *Language notes*; a spec is a good place to hold an undecided design question
and a bad place to accidentally settle one.

**Acceptance:**

- `cargo test --test spec` passes, and `cargo test` still passes whole.
- Neutralisation, per `CLAUDE.md`'s *Testing notes*: edit one `expect=ok` block in the Layout
  chapter so it violates the rule that block illustrates — e.g. re-indent a `case` branch by
  three spaces — re-run `cargo test --test spec`, confirm it goes red naming that chapter and
  block, then restore. A harness that cannot fail on a bad example has not been tested.
- A block in a chapter with no expectation in its info string makes the run fail, rather than
  being silently skipped. Cover it with a fixture the harness reads, not by committing a bad
  block to a real chapter.
- The Layout chapter states the two-space and no-tab rules, each with a passing example and a
  rejected one.
- `lang.md` no longer exists; `git grep -n "Basics.Js"` returns nothing; the JS-interop prose
  is under `docs/spec/`.
- `cargo run` still prints `parsed 8 modules`, lists all eight as checked, and exits 0 —
  unchanged, since this ticket touches docs and tests only.
