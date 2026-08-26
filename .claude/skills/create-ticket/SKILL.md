---
name: create-ticket
description: File a new ticket under docs/tickets/, grounded in the actual code, and add its row to the index. Use when the user says "file a ticket for X", "make a ticket out of that", "create a bug for the thing we just found", or when a problem surfaces mid-work that is real but out of scope for what is being done.
argument-hint: <description of the problem or task> [--prefix BUG|ERR|AST|PERF|TIDY|TEST]
---

# Create Ticket

Turn a described problem into a self-contained ticket file under `docs/tickets/`, plus its row
in `docs/tickets/INDEX.md`.

**This skill runs in the current session.** No worktree, no subagent. Filing needs the
conversation context that just produced the finding, and the result is a docs-only commit.

## Input

$ARGUMENTS

A description of the problem or task. Often it is not in the arguments at all but in the
preceding conversation ("file a ticket for that"), in which case use what was just discussed.

## Why this exists

The habit worth protecting: a problem found while doing something else becomes a ticket rather
than scope creep. Both halves matter — the finding does not get silently dropped, and the diff
in flight does not grow to swallow it. A ticket citing where it was found ("noticed while
working on `AST-1`; deliberately left unfixed there because …") is more useful than either
outcome.

## Step 1 — Pick the ID

```bash
git fetch origin main
git show origin/main:docs/tickets/INDEX.md
```

Read the whole table, **tombstone rows included**. IDs are never reused, so the next `BUG-` is
one past the highest `BUG-` in the table whether that row is open or closed. Reusing a closed
ID silently breaks its `git log --diff-filter=D` recovery path, because two files then share
one name.

Prefixes in use are listed in the INDEX header. Use an existing one unless the work is
genuinely a new theme; a prefix with one ticket in it is noise. `BUG-` is for defects — code
that does the wrong thing. Everything else is a task.

Read the working tree's `docs/tickets/INDEX.md` too if it differs from `origin/main` — a ticket
filed locally but not yet pushed still owns its number.

## Step 2 — Ground it in the code before writing a word

**A ticket that has not been checked against the tree is worse than no ticket**, because it
gets believed. Before writing:

- Find the real symbols. Open the files. Confirm the problem is present on `main` and has not
  already been fixed — `TODO.md` was migrated into this directory carrying two items that had
  been done for months and one that was half-done, and every one of those was only caught by
  looking.
- Check the INDEX for an existing ticket covering it. If one does, extend that ticket instead
  of filing a second.
- If a fix is proposed, confirm it is *possible* — that the API exists, that the type permits
  it. A ticket may honestly say the approach is undecided; it may not confidently propose
  something that cannot be done.
- Reproduce it if it is reproducible. `cargo run` and `cargo test` output pasted into the
  Problem section is worth more than any amount of description.

## Step 3 — Write `docs/tickets/<id-lower>.md`

The house format — bold labels, no YAML frontmatter. Read two or three existing tickets first
and match them; `bug-2.md` and `ast-2.md` are the models.

```markdown
# <ID> · <Title, same text as the INDEX row>

**Severity:** medium (parenthetical justification)   ← bugs only
**Sizing:** small                                    ← tasks only, instead of severity
**Location:** `path/to/file.rs` — `symbol_name`, `OtherSymbol`
**Depends on:** [AST-1](ast-1.md)                    ← optional
**Problem:** …
**Fix:** …          ← bugs.  `**Approach:**` with numbered steps for tasks.
**Acceptance:** …
```

Rules that are not negotiable:

- **Cite symbols, not line numbers.** `` `src/compiler/mod.rs` — `compile_package`'s final
  `Ok(())` `` survives the next refactor; `mod.rs:287` does not. The migrated `TODO.md` cited
  line numbers and most of them had already drifted.
- **Severity for bugs**: `high` = miscompile or data loss, `medium` = wrong behaviour under
  normal use, `low` = edge case or polish. Justify it in a parenthetical. **Sizing for tasks**:
  `small` / `small-to-medium` / `medium` / `large`, and say what could make it bigger.
- **Acceptance is a check someone can run**, and it is the yardstick `review-pr` later holds
  the diff against. "Works correctly" is not one. Name the command, the assertion, the
  observable output. If the fix needs a test, say which file it goes in.
- **Do not decide what you have not decided.** Where there is a real choice — two viable
  representations, fix-versus-remove — lay out both with their trade-offs and say the ticket
  does not pick. A ticket that pretends to a decision it did not make gets that decision
  implemented badly.
- **Record where it came from** when it was found during other work, including why it was left
  unfixed there.
- Cross-reference siblings as relative links (`[AST-1](ast-1.md)`), and design context in
  `CLAUDE.md` by section name.

## Step 4 — Add the INDEX row

Insert into the table in `docs/tickets/INDEX.md`, **above the tombstone rows**, grouped with
its own prefix:

```
| [BUG-4](bug-4.md) | bug | medium | open | <title, matching the file's H1> |
```

`type` is `bug` or `task`; `sev` is the severity for bugs and `—` for tasks; `status` is
`open`. If a new prefix was introduced, add it to the prefix list in the INDEX header.

## Step 5 — Commit on `main`, in the main repo

```bash
git -C "$(git rev-parse --show-toplevel)" add docs/tickets/
git commit -m "docs: file <ID> for <short summary>"
```

**Not on the feature branch that discovered it.** A ticket that lands only when its discovering
PR merges is invisible for exactly the period it is most useful — while someone is deciding
whether to fix the thing now or later. If the current branch is not `main`, say so and either
switch, or write the file and tell the user it needs to be committed separately.

Do not push without asking; the user may want to read it first.

## Step 6 — Report

One line: the ID, the title, the file path, and — in a sentence — what the acceptance check is.
If anything in Step 2 turned out differently from what was described (already fixed, already
ticketed, not reproducible), say that instead of filing, and say what you found.

## Notes

- **One ticket, one piece of work.** If the description covers two independent changes, file
  two and link them with `Depends on`. `AST-1` and `AST-2` are the example: related, ordered,
  separately shippable.
- **Filing is cheap; a wrong ticket is not.** Deleting a ticket costs one `git rm`. A ticket
  confidently describing code that does not exist costs whoever picks it up an hour before
  they stop trusting it.
- **Don't file the tree's existing TODO comments wholesale.** There are many. File one when
  it is about to matter, with the grounding of Step 2 — a bulk import would recreate `TODO.md`
  with more files.
