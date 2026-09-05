---
name: write-spec-chapter
description: Write one chapter of the Zelkova language specification under docs/spec/, with every example checked by cargo test --test spec, and file a ticket for every divergence it uncovers instead of fixing it. Use when the user says "write the modules chapter", "let's do Expressions next", "continue the spec", or names any chapter from docs/spec/README.md's list.
argument-hint: <chapter name from docs/spec/README.md's chapter list>
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

A chapter name, or a `SPEC-n` ticket ID. Every chapter in `docs/spec/README.md`'s table is
already written, so this is normally a chapter that does not exist yet — records and lists are
the two the other chapters defer to. Either way, a `SPEC-n` ticket for the chapter must already
exist, filed ahead of time with `create-ticket` — see Step 3. Often the chapter name is not in
the arguments at all but in the preceding conversation ("do the next one"), in which case take
the oldest open `SPEC-` ticket whose title names a chapter.

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

- `docs/spec/conventions.md` — the `expect=` vocabulary, `package=`, *Tag every claim the
  chapter makes*, *The words a chapter uses* (the wording rules, including that `should` is
  reserved for compiler-facing prose), *A chapter says what the language is*, and *A spec
  change and a semantics change do not share a diff*. This is the document chapter authors are
  written against; read all of it.
- `docs/spec/README.md` — the index a reader of the language sees: what the spec is, the three
  lead-ins (**Known gap:**, **Not implemented:**, **Provisional:**), and the chapter table your
  new row goes into.
- `tests/spec.rs` — what the harness actually enforces, as opposed to what the index says it
  does. The two are kept in step deliberately; if they have drifted, that is the first finding.
- `docs/spec/layout.md` and `docs/spec/lexical-structure.md` — the two worked examples, for
  voice and structure. Chapters explain *why* a rule is what it is, not only what it is — where
  the reason is a property of the language rather than a fact about this project's past.
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

## Step 3 — Confirm the `SPEC-n` ticket already exists

**This skill does not file the `SPEC-n` ticket.** Filing it from inside the same run that later
drafts and closes it collapses the ticket straight back into a same-PR file-and-delete — the
exact pattern normal tickets avoid, and the reason `SPEC-n` moved to this convention in the
first place. The ticket has to come from a separate `create-ticket` run, committed to `main` on
its own, before this skill starts.

Check `docs/tickets/README.md` for an open `SPEC-` ticket whose title names this chapter. If
one exists, read it — but treat its **Problem** and **Approach** as a lead, not a scope. They
were written from a quick grounding pass, not from the probing Step 2 demands, and the ticket
says so in its own **Grounding note**. Re-verify anything specific it claims, and don't let its
bullet list cap what the chapter ends up covering — Steps 1–2 (read, then probe) and Step 4
(design questions) are what actually decide that. **Acceptance** is the one section that is
load-bearing as written: it's about the process holding (every block tagged and proven to
fail, the index row moved), not about limiting content.

If none exists, **stop here.** Report what Steps 1–2 just turned up — it's exactly the
grounding `create-ticket` needs — and tell the user to file `SPEC-n` before running this skill
again. Do not offer to file it as a convenience; that offer is how the same-PR pattern creeps
back in.

## Step 4 — Ask the design decisions before writing prose

Collect every question the chapter must commit to and that nothing in the tree settles, then
put them to the user in `AskUserQuestion` calls — **max 4 per call, multiple calls is fine** — up
front, before drafting. Not one at a time mid-draft.

Each option gets a concrete `preview` showing the syntax or the consequence, and the
recommended option goes first. The user is deciding what the language *is*; an abstract
description of a trade-off is much harder to decide from than four lines of the code it
produces.

Questions worth asking look like: two spellings exist for one concept, which is the language?
What is this literal's range? Does this construct nest? Is this word reserved everywhere or
only in one position? Questions **not** worth asking are ones the tree already answers, or
where a conventional default is obvious — decide those, say you did, and move on.

If the chapter's job is to *record* an open question rather than settle one, bring the user the
framing rather than a decision — and write the question as the language's open question, not as
a note about who has yet to answer it.

## Step 5 — Write `docs/spec/<chapter-name>.md`

- **Every ```` ```zel ```` block carries an `expect=` tag.** No tag is a hard test failure, not
  a skip. `expect=fragment` is the only opt-out and must be explicit.
- **Pin the parse-error reason whenever the prose describes the error the reader will see.**
  A bare `expect=parse-error` stays green across a diagnostic fix and lets the paragraph
  describing the old error rot silently; the pin is what turns that into a red test.
- **Prose describing today's wrong behaviour opens with `**Known gap:**`.** Prose describing a
  feature ahead of the compiler opens with `**Not implemented:**`. Both are greppable on
  purpose. Say what the language's answer is, and link the ticket.
- **Write what the language is, in the present tense.** A chapter is not a record of how it got
  that way. Never write project history into one — no `SPEC-n` ticket that settled a question, no
  "was spelled `x` until", no "which superseded", no "this pass found", no chapter narrating its
  own writing ("this chapter is the record of that design", "that block matters more than it
  looks"). All of that is real and all of it belongs in `docs/tickets/` and in the commit
  message; a reader of the spec needs the rule and the reason it is a good rule. Two things you
  are *not* being told to drop: rationale that is a property of the language, and the
  **Known gap:** / **Not implemented:** lead-ins, which describe the compiler rather than the
  language. `docs/spec/conventions.md`'s *A chapter says what the language is* is the full rule.
- **Prefer prose plus checked examples.** Drop into EBNF only where English is genuinely
  worse — an exposing list's nesting, an operator table. Nothing checks an EBNF block, which
  makes it the one thing in the directory that can drift.
- Cross-link sibling chapters by relative path and anchor (`[Layout](layout.md#tabs-are-legal-only-inside-a-comment)`).

## Step 6 — Prove the tests can fail

`cargo test --test spec` going green on the first run proves nothing. Pick two or three of the
**least obvious** tags — a `canonical-error:` variant, a pinned parse-error reason, a
`**Known gap:**` block — neutralise each by changing the tag to something wrong, confirm the
harness reports it, and restore.

Report in your summary that you did this and which blocks you used. `CLAUDE.md`: *tests that
pass both ways are the most common review finding there is.*

## Step 7 — File tickets for what the chapter turns up, fix nothing

This step is for `BUG-`/`LANG-` tickets discovered while drafting — the `SPEC-n` ticket for
the chapter itself was confirmed in Step 3 (filed separately, before this run) and gets
tombstoned in Step 8, not filed here.

`docs/spec/conventions.md`'s *A spec change and a semantics change do not share a diff* is the
rule.
A spec claim the compiler fails is a red test, which is a working record rather than a lost
one. Match the `create-ticket` skill's format — read it for the ticket anatomy and the
grounding rules, both of which apply here unchanged.

**One thing does not apply: commit these tickets on the chapter's own branch, not on `main`.**
`create-ticket`'s Step 5 says to file on `main` so a ticket is not invisible until its
discovering PR merges, and that is right for a finding that stands on its own. A `BUG-`/`LANG-`
ticket found here does not. It is cited by name from the chapter's `**Known gap:**` paragraphs,
and a chapter often introduces a prefix (`LANG-`) whose meaning only its own text explains — so
splitting it onto `main` leaves the branch linking to a file whose reason for existing is on
the other side of the split, and leaves `main` holding a ticket that references a chapter
nobody can read yet. Keep it with the chapter and let the branch land as one story.

Which prefix, from `docs/tickets/README.md`:

- **`BUG-`** — code failing at what it was trying to do. A panic, a swallowed line, a
  never-terminating iterator.
- **`LANG-`** — code succeeding at something the language has since decided against. It was
  never wrong until this chapter was written. Every `LANG-` names the chapter that decided it
  **and the tagged block that goes red when it lands**.

Then flag, in your report, **any `**Known gap:**` whose block stays green across its own fix**.
That gap has no test holding it to account and its paragraph has to be deleted by hand; say so
in the ticket too. `LANG-4` is the worked example: prefix negation's syntax is unchanged by the
fix, only its meaning.

## Step 8 — Update both indexes

- `docs/spec/README.md` — add the chapter's row to the chapter table, linked, in reading
  order, with a one-line *Covers*. If any chapter deferred to this one, drop the sentence in
  that file's gap paragraph that says the cross-reference arrives nowhere.
- `docs/tickets/README.md` — a row per new `BUG-`/`LANG-` ticket, above the tombstones, grouped
  by prefix. Add any new prefix to the header list with a sentence on what distinguishes it.
- **Tombstone the `SPEC-n` ticket confirmed in Step 3**: delete `docs/tickets/spec-n.md` and
  rewrite its row in place — status becomes the close date, no SHA, no PR number — the same
  convention every other ticket closes under, per `docs/tickets/README.md`'s *Closing
  convention*.

## Step 9 — Verify

```sh
cargo test                     # includes --test spec
cargo run                      # must print "parsed 8 modules", list all eight, exit 0
cargo fmt --all --check
cargo clippy --all-features
```

`cargo run` matters even for a docs-only change: it is the smoke test, and a chapter that
needed a `tests/` helper may have touched more than intended. CI does not gate on fmt or
clippy — run both locally.

Then read the chapter once more for the present-tense rule, since nothing tests it. A drafting
session knows *why* it wrote each rule and leaks that reasoning into the prose without noticing;
the leaks are easy to spot on a second pass and read as noise to everyone else.
`grep -n 'SPEC-\|until\|used to\|no longer\|this chapter' docs/spec/<chapter>.md` catches most
of them — every hit is either a **Known gap:** / **Not implemented:** sentence about the
compiler, or a sentence to rewrite.

## Step 11 — Report, and commit only if asked

What the chapter settled, what it turned up, and the block count from the spec run (it prints
`spec: N block(s) passed`). Name the tickets filed and, explicitly, any gap with no red test
behind it. **Do not commit without being asked.**

When you are asked, split the branch into distinct commits in this order:

1. **The `BUG-`/`LANG-` tickets** — every new file under `docs/tickets/` plus its INDEX rows.
   This goes first so the chapter's `../tickets/<id>.md` links resolve at every commit in the
   branch, rather than dangling until the tip. The `SPEC-n` ticket is not part of this commit —
   it was filed on `main` by a separate `create-ticket` run before this branch existed at all.
2. **The chapter** — `docs/spec/`, including the INDEX row moving to `written`, together with
   tombstoning `SPEC-n`: deleting its file and rewriting its `docs/tickets/README.md` row. The
   two move together because the tombstone *is* the chapter landing.
3. **Anything else** the session produced, such as a skill or a test helper.

If a later commit adds a pointer to something an earlier one does not have yet, take that
sentence out, commit, and put it back — a one-line edit is cheaper than a reference that is
broken for one commit of history.

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
- **One module per block, unless the block carries a `package=` label.** Blocks sharing one
  label within a chapter are compiled together as one package, each keeping its own `expect=`;
  a block with no label is compiled alone against no interfaces at all.
  `docs/spec/conventions.md`'s *More than one module* has the mechanism and the four things a
  group cannot do.
- **`NodeSpan`'s `PartialEq` always returns `true`**, so a whole-value assertion in any test
  you add proves nothing about position. Assert on `.span` or on `diagnostic.labels[..].range`.
- **Leading whitespace in a block is load-bearing** — layout is indentation-sensitive, and an
  editor stripping trailing whitespace can quietly turn an example into one that proves
  nothing. `docs/spec/layout.md`'s blank-line example says so in prose for exactly this reason.
