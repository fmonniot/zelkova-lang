---
name: prose-pass
description: Tighten the prose of a docs/ markdown file — normally a docs/spec/ chapter — without changing a rule it states, and address any USER REVIEW comments left in it. Use when the user says "simplify this chapter", "this reads too much like AI", "too verbose", "address my comments in the doc", "clean up the prose", or asks for a prose pass on a file.
argument-hint: <path to a docs/ markdown file> (defaults to the file under discussion)
---

# Prose Pass

Cut a `docs/` markdown file down to what it has to say, and nothing it says twice.

**This skill runs in the current session.** No worktree, no subagent. Deciding whether a
sentence carries a rule or merely defends one needs the neighbouring sections, the decision
entries behind them, and often what the user said two messages ago.

## Input

$ARGUMENTS

A path. Often there is none — use the file under discussion or open in the IDE. If the file
carries `USER REVIEW:` markers, those are the brief; see Step 1.

## The one invariant

**A prose pass never changes what the language says.** Every rule, admitted type, encoding,
diagnostic and tag survives the pass unchanged. Cutting is licensed for arguments, restatements
and commentary — never for a claim a reader acts on.

Two things stop the pass and go to the user:

- A cut that would change a rule, or that you cannot tell from one that would.
- A design question the pass surfaces. `SPEC-18`'s pass turned up "should a union cross as
  `{$, args: []}` instead of `{$, a, b, c}`?" — that is the language owner's to answer, and the
  answer belongs in `docs/decisions/` whichever way it goes.

Everything else: act, then report. The point of this skill is that the user should not have to
talk you through a pass they have already described.

## Step 0 — Load the ground rules

Read `docs/spec/conventions.md`, sections *The words a chapter uses* and *A chapter says what
the language is*. They are the authority; the steps below are a checklist for applying them.

Three things are load-bearing and are never reworded, never cut, and never moved away from the
block they belong to: the **Known gap:**, **Not implemented:** and **Provisional:** lead-ins,
and the `expect=` tag on every fence. A pass that touches a fenced block or a tag is not a prose
pass.

## Step 1 — Collect the user's own comments

```bash
grep -n "USER REVIEW" <file>
```

Each marker is the brief, and **each one generalizes**: "this paragraph is too verbose" means
find the other paragraphs like it, and one of the markers usually says so outright.

Two mechanics that cost time when missed:

- A pasted marker can contain a non-breaking space (U+00A0), so `Edit` fails on a line that
  looks byte-identical to what you typed. Confirm with `sed -n <n>p <file> | hexdump -C`.
- Delete the markers in one sweep rather than one `Edit` each:
  `perl -0pi -e 's/^USER REVIEW:.*\n(\n)?//mg' <file>`

## Step 2 — Structure before sentences

Reorder before rewriting; a paragraph you are about to cut may belong in another section.

**What is possible comes first, what is forbidden comes after and smaller.** Sections that each
say what cannot be done merge into one — `js-interop.md`'s *A facade is monomorphic*, *A
function does not cross* and *A facade signature may not carry a constraint* became one *What a
facade signature may not name*, half the length of the three.

Merging or renaming a header moves an anchor. That is Step 5, and it is not optional.

## Step 3 — The eight cuts, one sweep per pattern

Sweep the whole file for **one pattern at a time**. A single read-through finds one instance of
a pattern and moves on; a sweep finds all of them. Every example below is a real before/after
from `docs/spec/js-interop.md`.

1. **Throat-clearing openers.** "A user can mark a Zelkova module as a JavaScript interface.
   This is done by using the `javascript` modifier" → "A user marks a Zelkova module as a
   JavaScript interface with the `javascript` modifier".

2. **Decorative negation tails.** A closing "rather than X" / "and not X" / "instead of X" that
   only negates what the first half already ruled out. "an error at the boundary rather than a
   wrong answer somewhere further on" → "an error at the boundary". Also: "read directly rather
   than invoked", "paid once per crossing rather than once per use", "on exactly the terms
   `Bool` is and not as a tolerated exception". Grep for `rather than`, `not as`, `instead of`.

3. **Self-restating closers.** A last clause that says the paragraph's own point again:
   "Ordinary code and `std/core` reach the runtime the same way, which is the property this
   design exists to preserve."

4. **Commentary on the document.** "The predicate is what makes a facade signature mean
   anything", "a price worth naming", "the chapter states it rather than letting someone
   discover it". `conventions.md` forbids these outright — say the thing instead of announcing
   that you are about to.

5. **Reassurance after a cost.** The cost is stated, then softened: "That price is smaller than
   it looks, because … the constructor names of an exposed union are already public API." State
   the cost and stop.

6. **Defensive justification.** A rule gets the one property a reader acts on, not a case for
   the rule. Two independent reasons for one rejection is one too many for a chapter; the second
   goes to `docs/decisions/` (Step 6). Explaining *why* stays in scope when the reason is a
   property of the language — `conventions.md` has the test — so this is the cut that most needs
   judgment, and the one to under-do rather than over-do.

7. **Repeated templates.** Three "is what" sentences in a paragraph, a "which means … which
   means" chain. Vary, or cut the weakest.

8. **Duplicated conclusions across sections.** Two sections ending on the same sentence: keep it
   where the rule lives, and have the other link to it.

## Step 4 — Repair what the cuts broke

**After each cut, re-read the whole paragraph, not the line you edited.** Both repair cases
below came from cuts that were individually correct:

- **Dangling antecedents.** "Both are questions about the value" survived the deletion of the
  sentence naming the two. "A constructor's name is part of that interface" survived the
  deletion of the interface.
- **Counts and cross-references inside prose.** "Two blocks in that chapter … each carries a
  **Known gap:** paragraph" stops being true when the blocks merge.
- **Whitespace.** Trailing spaces and NBSPs left by earlier edits.
- **Hard wrap.** Files in this tree are hard-wrapped (≈95–100 columns in `docs/spec/`); rewrap
  every paragraph you edited. A long link line that cannot wrap is fine — the file has others.

## Step 5 — Anchors and citations

For every header renamed or merged in Step 2:

```bash
grep -rn "<basename>.md#" docs/
```

`cargo test --test spec` checks links written **from** `docs/spec/` and `docs/decisions/`, so a
broken anchor there turns the suite red. It does not check links written from `docs/tickets/`,
which is where a stale anchor rots silently — `LANG-43` was already citing a
`js-interop.md#constraints` that had never existed.

Update the citing *prose*, not only the URL: `LANG-43`'s "Two blocks in that chapter … each
carries a **Known gap:** paragraph" had to become "both in one section, covered by the one
paragraph that follows them".

## Step 6 — `docs/decisions/` keeps what the chapter drops

The chapter states rules; the arguments live in `docs/decisions/`. Two obligations follow, and
both are easy to skip:

- **An argument worth keeping is moved, not deleted.** A second independent reason for a
  rejection, or an alternative weighed and dropped during the pass, goes into the relevant
  entry.
- **An entry that asserts something about the chapter is amended when the pass makes it false.**
  `DEC-6` decision 2 said "Both reasons are given in the chapter"; cutting the second reason
  from `js-interop.md` made that sentence wrong, and it was fixed in the same commit.

## Step 7 — Verify

```bash
cargo test --test spec          # every example, anchor and relative link in docs/spec + docs/decisions
grep -n "USER REVIEW" <file>    # must be empty
git diff --stat
```

If the diff touched a fenced block or an `expect=` tag, find out why before committing — that is
a spec change wearing a prose pass's clothes.

## Step 8 — Report, then commit

Report **by category**, one line each, each naming a `file:line` and what went. Then say what
was deliberately kept and why — "kept the O(size of value) crossing cost, a fact a reader acts
on" is the half of the report that tells the user the pass had a rule and not just a mood.

Commit the file, every citing file, and any amended decision entry **in one commit**: an anchor
rename split across two commits leaves the tree with a red spec suite in between. Name the
categories cut in the message body, the way this branch's prose commits do.

## What this skill does not do

- It does not rewrite for style preference alone. Every edit belongs to a category above or to a
  `USER REVIEW:` marker.
- It does not touch code, tests, or `.zel` sources — a chapter's examples included.
- It does not answer a design question it surfaces, and it does not quietly pick an answer.
