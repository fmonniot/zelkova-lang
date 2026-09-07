# DEC-4 · Design rationale gets its own directory

**Settled:** 2026-09-06, by the repository owner (`SPEC-26`).
**Status:** live.
**Where the rule lives:** [this directory's README](README.md), and
[`docs/tickets/README.md`](../tickets/README.md)'s closing convention, which gained a third
promotion destination.

Rationale had been kept in three unrelated places: an appendix of `docs/spec/` that explained
its own filing and said a better home would exist; a numbered decision list cited five times
that existed only inside a deleted ticket; and fifty lines of prose about documentation policy
at the head of a Rust test binary. Four questions had to be answered together, because the
answer to each constrains the rest.

## 1 — The track is a sibling directory, `docs/decisions/`

Not a subdirectory of `docs/spec/`, and not a `DEC-` prefix inside `docs/tickets/`.

**Against a spec subdirectory:** the spec is normative and every claim in it is checked. A
rationale document's whole job is to hold the arguments for options that were *rejected*, and
filing those under the directory whose premise is "this is what the language requires" blurs
the one boundary [the spec index](../spec/README.md) works hardest to draw. The mechanics
agree: `tests/spec.rs` reads `*.md` directly under `docs/spec/` and nothing below it, so a
subdirectory would have been unchecked anyway.

**Against a ticket prefix:** tempting, because the tooling already exists — an index, stable
IDs, a tombstone convention, a `work-ticket` skill. Wrong for the reason the closing
convention itself gives: everything in `docs/tickets/` is *work*, work is finished, and a
finished ticket is deleted. A record whose status is never anything but "live" contradicts the
directory it would live in, and would sit in a table where every other row is on its way to
being a tombstone.

## 2 — An entry is a `DEC-n` file of free prose, with numbered decisions inside it

The alternatives were a strict ADR skeleton (context / decision / consequences / status) on
every entry, and one file per individual decision.

**Against the skeleton:** [DEC-1](dec-1.md) is a nine-language comparison that decided one
thing by ruling out three families of mechanism. Cut into four headings it says less than it
does now, and the cut buys uniformity nobody reading it needs.

**Against one file per decision:** [DEC-2](dec-2.md)'s eleven were taken in one sitting and
several only make sense against each other — decision 7's second consequence is decision 5,
decision 6 exists because of a promise in a chapter decision 1 changed the syntax of. Eleven
files would be maximally citable and would fragment the session that is the actual unit of
thought.

So: a `## <n> — <claim>` header per decision, cited as `DEC-2 decision 6`, with the ID and the
number both stable forever. That citation form is what the five broken ones were reaching for
and could not have — `SPEC-12 decision 6` named a ticket, and a ticket is deleted.

## 3 — The track is checked for links, and not for examples

`cargo test --test spec` resolves every relative link and anchor an entry writes, and holds
each entry to writing at least one header and one link. It does **not** read `zel` blocks
here, and `docs/decisions/` has no `expect=` vocabulary.

The link half is not optional: the failure this directory was created out of is a citation
nothing kept alive, and leaving the citations themselves unchecked would reproduce it one
directory over. It is the same argument [DEC-3](dec-3.md#1--links-into-docstickets-are-checked)
made for checking ticket citations from the chapters, and the cost is the same one — an entry
that names a chapter section pays when that header is renamed.

The example half is not merely unnecessary but wrong. An entry shows designs that were
rejected and code from other languages; a tag saying what the Zelkova compiler does with a
Haskell block is meaningless, and a tag on a rejected Zelkova design would be pinning the
compiler's behaviour on syntax the language deliberately does not have.

## 4 — Rationale about the tooling moves too

`tests/spec.rs`'s header carried the argument for two of its own scope decisions. It is good
writing about a convention for markdown files, readable only by someone who opened a Rust file
to find out why a test exists. It is now [DEC-3](dec-3.md), and the module comment keeps a
sentence saying what the tests check plus a pointer — which is what `CLAUDE.md`'s *a doc
comment describes what the code at that site does* asks of it.

The narrower alternative was to scope this directory to the language and leave the tooling's
reasoning in the tooling. Rejected because the test that decides what belongs here — would a
later reader otherwise re-open the question — does not care which half of the repository the
question is about, and because `SPEC-23`'s scope decisions are precisely the kind that get
re-opened by someone who finds the check expensive.
