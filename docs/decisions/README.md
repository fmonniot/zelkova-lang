# Zelkova — Design decisions

This directory holds **why a rule is what it is**, and the alternatives it was chosen over. It
is the fourth of this repository's long-lived records, and it exists because each of the other
three refuses the job for a reason worth keeping:

| Where | Holds | Lifetime |
|---|---|---|
| [`docs/spec/`](../spec/README.md) | what the language **is** — normative, and every example checked | as long as the rule |
| [`docs/tickets/`](../tickets/README.md) | what is **to be done** | deleted the day the work lands |
| `CLAUDE.md` | rules that constrain **a diff** | as long as the rule |
| here | **why**, and what was rejected | permanent |

A chapter states a rule and cannot afford to also argue it — [a chapter says what the language
is](../spec/conventions.md#a-chapter-says-what-the-language-is) rules out alternatives
considered and dropped, in those words. A ticket carries the argument while the work is open
and is then deleted, which is right for an implementation narrative and wrong for a decision:
the reasoning outlives the tree it was written against. The gap between the two is what this
directory is.

The failure it exists to stop is concrete. `SPEC-12` settled eleven questions about type
classes; its ticket file was deleted when the chapter landed, and five live files went on
citing "`SPEC-12` decision 6" and "decision 7" — citations that resolved nowhere, one of them
naming a chapter that has never carried a numbered list. The decisions were recoverable only
by `git show` on a path the reader had no reason to guess. They are [DEC-2](dec-2.md) now.

## What belongs here

The test is whether **a later reader would otherwise re-open the question.** That is true of a
choice made against real alternatives, where the alternatives are not recoverable from the
rule that won — and false of most commits, which decide nothing anyone will ask about again.

Three shapes recur:

- **A design session that settled several questions at once.** [DEC-2](dec-2.md) is eleven of
  them, taken together in one sitting, each normative for a chapter and a program of tickets.
- **A survey that ruled options out.** [DEC-1](dec-1.md) compares nine languages' deriving
  mechanisms; the chapter it decided states one mechanism and says nothing about the eight it
  is not.
- **A choice about the project's own machinery** whose cost lands on someone else later.
  [DEC-3](dec-3.md) is why the spec harness checks ticket citations, which is a bill paid by
  whoever closes a cited ticket.

## What does not

- **A rule.** What the language requires goes in [`docs/spec/`](../spec/README.md), which is
  normative and checked; what constrains a diff goes in `CLAUDE.md`'s *Standing invariants*.
  An entry here **points at** the rule it decided and never restates it. Two records of one
  rule means the unmaintained one is what someone eventually reads.
- **Work.** Anything with an acceptance check is a ticket. An entry is never "open" and is
  never assigned.
- **A narrative of how a change was implemented.** That belongs to the ticket, and dies with
  it on purpose: it describes a tree that no longer exists.
- **Everything else.** This is not a licence to write a record per commit. If nobody would
  re-open it, it is not a decision, it is a diff.

## Conventions

**One file per entry: `dec-<n>.md`.** IDs are stable and are never reused, the same rule
[`docs/tickets/`](../tickets/README.md) holds its own to, and for a sharper reason: a ticket ID is cited
while the work is open, a decision ID is cited forever.

**An entry is never deleted.** A decision that is overturned is *superseded* — its `Status`
says by what, and the entry stays, because a reader arriving at a citation needs to find the
decision and its fate rather than a missing file. This is the one convention that inverts the
ticket directory's.

**Three fields open every entry**, immediately under the title:

- **Settled:** the date the decision was taken, and by whom where that matters.
- **Status:** `live`, or what superseded it. Where an entry holds a numbered list and only
  some of it moved, this says which numbers.
- **Where the rule lives:** a link to the normative record — a chapter, `CLAUDE.md`, or the
  code. Every entry has one, because an entry that decided nothing anybody wrote down is an
  entry that decided nothing.

**A numbered decision is addressable.** Where an entry settles several questions, each gets
its own `## <n> — <claim>` header, and is cited as `DEC-2 decision 6` with a link to that
header. The number belongs to the entry and never changes; a decision that is later
superseded keeps its number and gains a note.

**After the header block, an entry is free prose.** No fixed context/decision/consequences
skeleton: [DEC-1](dec-1.md) is a comparison essay and [DEC-2](dec-2.md) a numbered list, and
forcing either into the other's shape would cost more than the uniformity is worth. What is
fixed is the header block above and the numbering rule.

**Code in an entry is illustrative and nothing runs it.** The `expect=` vocabulary belongs to
[`docs/spec/`](../spec/README.md) and stops at its door: a ```` ```zel ```` block here carries
no tag and is not compiled, which is what lets an entry show a design that was *rejected*. An
entry is not normative, so a block in one is never the answer to "what does the language do" —
the `Where the rule lives` link is.

**Cross-references are checked.** `cargo test --test spec` resolves every relative link and
every anchor an entry writes, exactly as it does for a chapter, and holds each entry to
writing at least one header and one link. It does *not* read `zel` blocks here. A citation
nothing keeps alive is the failure this directory was created out of, so the directory that
holds the citations is checked too — [DEC-4](dec-4.md) decision 3.

## Entries

| ID | title | status |
|---|---|---|
| [DEC-1](dec-1.md) | Deriving in other languages | live |
| [DEC-2](dec-2.md) | The type-class mechanism: eleven decisions | live; 8 superseded, 11 overtaken |
| [DEC-3](dec-3.md) | What the spec harness checks, and what it declines to | live |
| [DEC-4](dec-4.md) | Design rationale gets its own directory | live |
| [DEC-5](dec-5.md) | A pattern's sign is the pattern grammar's, not the tokenizer's | live |
| [DEC-6](dec-6.md) | Which types may cross the JavaScript boundary | live; 1 extended by DEC-13 |
| [DEC-7](dec-7.md) | Lists: six decisions | live |
| [DEC-8](dec-8.md) | Records: nine decisions | live |
| [DEC-9](dec-9.md) | What a program may rely on about space: sharing, not closures | live |
| [DEC-10](dec-10.md) | The law on a derivation's `combine` is checked by nothing, permanently | live |
| [DEC-11](dec-11.md) | What a value describing an effect is: seven decisions | live; 5 extended by DEC-12 |
| [DEC-12](dec-12.md) | What a broken companion does: a survey and seven decisions | live |
| [DEC-13](dec-13.md) | A facade names a boundary, not a backend: seven decisions | live |
| [DEC-14](dec-14.md) | A companion's test is a facade under `tests/`: four decisions | live |
