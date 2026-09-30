# TOOL-4 · One syntax error discards the whole module

**Sizing:** large. It touches the grammar, the layout pass's stop-at-first-error contract, and
what the rest of the pipeline does with a module that is only partly there. Either approach
below is a real design change to the parser.

**Part of:** the *Active work: editor support* section of [the index](README.md).
[`TOOL-6`](tool-6.md) works without it, but reports only the first syntax error per file and
nothing else about that file until it parses.

**Location:** `src/compiler/parser/mod.rs` — `parse`, which returns `Result<Module, Error>`;
`src/compiler/parser/grammar.lalrpop` — no rule uses LALRPOP's `!` error-recovery token;
`src/compiler/parser/layout.rs` — `layout`, whose iterator stops at the first `Err` by design
(its doc comment and `BUG-4`); `src/compiler/mod.rs` — `parse_root`, which counts a failed
module in `failures` and drops it from the build.

**Problem:** the first tokenizer, layout or grammar error in a module ends its parse, and
`parse_root` then drops the module from the build entirely. That has two costs:

- **Only one syntax error is ever reported per module.** A file with three typos needs three
  compile-fix rounds.
- **The module disappears, and its importers fail too.** Its name, its exports and every
  declaration that did parse are gone, so each module importing it reports errors about a
  missing module rather than about its own code.

On the command line this is an inconvenience. In an editor it is the steady state: a file
being typed is almost never syntactically complete, and a language server that loses the whole
module at every keystroke cannot offer hover, go-to-definition or completion for any of it.

**Approach:** two viable shapes, and this ticket does not pick.

- **Declaration-level resynchronisation.** A top-level declaration starts at column 1
  (`docs/spec/layout.md`), so the token stream can be cut at each column-1 token and every
  declaration parsed on its own, keeping the ones that parse and reporting one error per one
  that does not. It is cheap, needs no grammar change, and matches how offside-rule languages
  are usually made robust. Its granularity is a whole declaration: an error inside a function
  body loses that function and nothing else. The cut leans on column 1 being where every
  declaration starts: a declaration indented off it is read as part of the one above, so the
  two are lost together.
- **LALRPOP error recovery.** Add `!` productions at chosen points (a declaration, a `case`
  branch, an expression) that produce an error node and collect `ErrorRecovery` values. The
  granularity is finer, but it needs an error variant in the parser AST, and the grammar,
  `parser/mod.rs` and the `from_parser*` conversions move together
  (`CLAUDE.md` — *A grammar change is never a one-file change*). Canonicalization then has to
  treat an error node as "unknown", not as a construct to reject.

Either way, the tokenizer and layout errors have to be survivable too, or recovery only ever
covers grammar errors. The layout pass's stop-at-first-`Err` contract exists because of
`BUG-4`, and changing it means keeping its "advance or stop" invariant
(`CLAUDE.md` — *A `Result`-yielding iterator must advance or stop*).

What a partly parsed module does downstream is also this ticket's to settle. The simplest rule
that keeps importers quiet is that a module with any syntax error is not type checked, but its
name and the exports of the declarations that did parse are still published.

**Acceptance:**

- A test in `tests/compiler/parser/` parses a module with two independent syntax errors in
  two different top-level declarations and gets both back, plus the declarations between them.
  It is mutation-checked by reverting to first-error-wins and confirming the test fails.
- A test in `tests/pipeline.rs` compiles a package where `A` has a syntax error and `B`
  imports a value from `A` that parsed. `B` reports no missing-module error.
- `cargo test --workspace` is green, `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked, and exits 0, and `cargo test --test spec`'s
  `expect=parse-error` blocks still go red for the reason their paragraphs give.
