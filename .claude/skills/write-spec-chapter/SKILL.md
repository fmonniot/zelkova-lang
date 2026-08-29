---
name: write-spec-chapter
description: Write one chapter of the Zelkova language specification under docs/spec/, with every example checked by cargo test --test spec, and file a ticket for every divergence it uncovers instead of fixing it. Use when the user says "write the modules chapter", "let's do Expressions next", "continue the spec", or names any chapter from docs/spec/INDEX.md's list.
argument-hint: <chapter name from docs/spec/INDEX.md's chapter list>
---

# Write Spec Chapter

Write one chapter of `docs/spec/`, the normative specification of Zelkova the language, so
that every claim it makes is an executable example the compiler is held to.

**This skill runs in the current session, and it is interactive.** No worktree, no subagent.
A chapter cannot be written without settling design questions nobody has settled, and those
are the user's to answer — a subagent would guess at them, which is the one failure mode this
skill exists to prevent.

## Input

$ARGUMENTS

A chapter name from the table in `docs/spec/INDEX.md`. Often it is not in the arguments at all
but in the preceding conversation ("do the next one"), in which case take the next `planned`
row in reading order.

## Why this exists

`docs/spec/` is normative: it describes Zelkova **as designed**, including constructs the
compiler does not implement. That makes writing a chapter a design activity, not a
transcription one — and the value is mostly in what it turns up. `SPEC-1` produced `ERR-11`
and `ERR-12`; `SPEC-2` produced `BUG-12`, `BUG-13` and four `LANG-` tickets, because writing
down a rule that had never been written down is how you find out the compiler had quietly
picked a different one.

The policy the chapters inherit, set by `SPEC-2`: **the spec is self-contained.** Zelkova
began as a fork of Elm's surface syntax and owes it most of its good ideas, but Elm is an
inspiration, never a reference. No chapter may resolve a question by pointing at Elm's
documentation. Where a rule is inherited, write it out in full.

## Step 1 — Read the conventions before writing a word

- `docs/spec/INDEX.md` — the `expect=` vocabulary, the **Known gap:** / **Not implemented:**
  lead-ins, and *A spec change and a semantics change do not share a diff*. This is the
  document chapter authors are written against; read all of it.
- `tests/spec.rs` — what the harness actually enforces, as opposed to what the index says it
  does. The two are kept in step deliberately; if they have drifted, that is the first finding.
- `docs/spec/layout.md` and `docs/spec/lexical-structure.md` — the two worked examples, for
  voice and structure. Chapters explain *why* a rule is what it is, not only what it is.
- `CLAUDE.md`'s *Standing invariants*, if the chapter touches a phase that has one.

## Step 2 — Ground yourself in the surface, then probe it

Read enough of `grammar.lalrpop`, the parser and canonical ASTs, and the real `.zel` sources
under `std/core/src/` to know what the construct looks like in practice.

**Do not let the compiler decide what the language is.** The spec is normative and the
compiler is frequently behind it, occasionally ahead of it, and sometimes somewhere sideways.
Grounding is for getting the `expect=` tags right, not for settling design.

Then **probe rather than reason**. Write a throwaway `tests/scratch_probe.rs`:

```rust
//! TEMPORARY probe — delete before finishing.
use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::parser;

fn probe(label: &str, source: &str) {
    let file = SimpleFile::new("Probe.zel".to_string(), source.to_string());
    let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser::parse(&file)));
    match r {
        Ok(Ok(_)) => println!("[{}] PARSED OK", label),
        Ok(Err(e)) => println!("[{}] parse error: {:?}", label, e),
        Err(_) => println!("[{}] *** PANIC ***", label),
    }
}

#[test]
fn probes() {
    probe("some case", "module Example exposing (f)\n\nf = 1\n");
}
```

`cargo test --test scratch_probe -- --nocapture`, then **delete the file** before you finish.

The `catch_unwind` is not decoration: `SPEC-2` found four inputs that panic the tokenizer, and
a panic inside the real harness aborts every chapter's examples at once. Probing is how you
find those before they cost you a run.

Reasoning about tokenizer or grammar behaviour instead of probing it produces wrong answers at
a high rate. `SPEC-2` predicted the behaviour of eight constructs and got three wrong,
including two that looked obvious.

## Step 3 — Ask the design decisions before writing prose

Collect every question the chapter must commit to and that nothing in the tree settles, then
put them to the user in `AskUserQuestion` calls — **max 4 per call, two calls is fine** — up
front, before drafting. Not one at a time mid-draft.

Each option gets a concrete `preview` showing the syntax or the consequence, and the
recommended option goes first. The user is deciding what the language *is*; an abstract
description of a trade-off is much harder to decide from than four lines of the code it
produces.

Questions worth asking look like: two spellings exist for one concept, which is the language?
What is this literal's range? Does this construct nest? Is this word reserved everywhere or
only in one position? Questions **not** worth asking are ones the tree already answers, or
where a conventional default is obvious — decide those, say you did, and move on.

If the chapter's job is to *record* an open question rather than settle one — the
Constrained type variables chapter is the planned example — bring the user the framing, not a
decision.

## Step 4 — Write `docs/spec/<chapter-name>.md`

- **Every ```` ```zel ```` block carries an `expect=` tag.** No tag is a hard test failure, not
  a skip. `expect=fragment` is the only opt-out and must be explicit.
- **Pin the parse-error reason whenever the prose describes the error the reader will see.**
  A bare `expect=parse-error` stays green across a diagnostic fix and lets the paragraph
  describing the old error rot silently; the pin is what turns that into a red test.
- **Prose describing today's wrong behaviour opens with `**Known gap:**`.** Prose describing a
  feature ahead of the compiler opens with `**Not implemented:**`. Both are greppable on
  purpose. Say what the language's answer is, and link the ticket.
- **Prefer prose plus checked examples.** Drop into EBNF only where English is genuinely
  worse — an exposing list's nesting, an operator table. Nothing checks an EBNF block, which
  makes it the one thing in the directory that can drift.
- Cross-link sibling chapters by relative path and anchor (`[Layout](layout.md#tabs-are-legal-only-inside-a-comment)`).

## Step 5 — Prove the tests can fail

`cargo test --test spec` going green on the first run proves nothing. Pick two or three of the
**least obvious** tags — a `canonical-error:` variant, a pinned parse-error reason, a
`**Known gap:**` block — neutralise each by changing the tag to something wrong, confirm the
harness reports it, and restore.

Report in your summary that you did this and which blocks you used. `CLAUDE.md`: *tests that
pass both ways are the most common review finding there is.*

## Step 6 — File tickets, fix nothing

`docs/spec/INDEX.md`'s *A spec change and a semantics change do not share a diff* is the rule.
A spec claim the compiler fails is a red test, which is a working record rather than a lost
one. Use the `create-ticket` skill, or match its format.

Which prefix, from `docs/tickets/INDEX.md`:

- **`BUG-`** — code failing at what it was trying to do. A panic, a swallowed line, a
  never-terminating iterator.
- **`LANG-`** — code succeeding at something the language has since decided against. It was
  never wrong until this chapter was written. Every `LANG-` names the chapter that decided it
  **and the tagged block that goes red when it lands**.
- **`SPEC-n`** for the chapter itself, listing what it settled — the chapter states each rule
  but not that it was ever open, and that record is worth keeping somewhere.

Then flag, in your report, **any `**Known gap:**` whose block stays green across its own fix**.
That gap has no test holding it to account and its paragraph has to be deleted by hand; say so
in the ticket too. `LANG-4` is the worked example: prefix negation's syntax is unchanged by the
fix, only its meaning.

## Step 7 — Update both indexes

- `docs/spec/INDEX.md` — move the chapter's row from `planned` to `written`, linked.
- `docs/tickets/INDEX.md` — a row per new ticket, above the tombstones, grouped by prefix. Add
  any new prefix to the header list with a sentence on what distinguishes it.

## Step 8 — Verify

```sh
cargo test                     # includes --test spec
cargo run                      # must print "parsed 8 modules", list all eight, exit 0
cargo fmt --all --check
cargo clippy --all-features
```

`cargo run` matters even for a docs-only change: it is the smoke test, and a chapter that
needed a `tests/` helper may have touched more than intended. CI does not gate on fmt or
clippy — run both locally.

## Step 9 — Report

What the chapter settled, what it turned up, and the block count from the spec run (it prints
`spec: N block(s) passed`). Name the tickets filed and, explicitly, any gap with no red test
behind it. Do not commit without asking.

## Notes — harness gotchas that cost time otherwise

- **Blocks canonicalize with no interfaces.** An `expect=ok` block must declare every name it
  references. `Int`, `Float`, `Bool`, `List`, `Maybe` are *not* in scope — declare a local
  type instead. This is the single most common reason a correct-looking example fails.
- **An `infix` declaration's function must exist in the same block**, or canonicalization
  reports `InfixReferenceInvalidValue`. Operators are not built in: to use one in an example,
  declare it.
- **`BUG-8` means an exposing list is never checked**, so `exposing (x)` is free even when `x`
  does not exist. Convenient, and do not build an example on it — it goes red when `BUG-8`
  lands.
- **A panic aborts the entire spec run** and takes every other chapter's examples with it.
  Never put a panicking input in a block. Describe it in prose, file it, and say in the chapter
  why it is described rather than shown — `docs/spec/lexical-structure.md`'s last section is
  the model.
- **One module per block.** How to write an example spanning two modules is still open —
  `docs/spec/INDEX.md` lists four candidate mechanisms and says whichever chapter needs it
  first decides. The **Modules, `exposing` and imports** chapter is that chapter; settle it
  before starting, not halfway through.
- **`NodeSpan`'s `PartialEq` always returns `true`**, so a whole-value assertion in any test
  you add proves nothing about position. Assert on `.span` or on `diagnostic.labels[..].range`.
- **Leading whitespace in a block is load-bearing** — layout is indentation-sensitive, and an
  editor stripping trailing whitespace can quietly turn an example into one that proves
  nothing. `docs/spec/layout.md`'s blank-line example says so in prose for exactly this reason.
