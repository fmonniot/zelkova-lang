# Zelkova — Ticket index

_Last updated: 2026-08-27._

One file per ticket: `docs/tickets/<id-lower>.md`. Bugs and tasks share one ID namespace, one
closing convention and this one table — a bug is a ticket **type**, not a separate file. Each
ticket is self-contained so it can be picked up on its own: it names the **location**, the
**problem**, a suggested **fix** or **approach**, and an **acceptance** check that says when
it is done.

IDs are stable and are never reused; reference them when starting work ("work on `AST-1`").
Severity applies to bugs: **high** = miscompile or data loss, **medium** = wrong behaviour
under normal use, **low** = edge case or polish. Tasks carry a **Sizing** note in their own
file instead.

Prefixes are created ad-hoc per theme. Current ones: `BUG-` (defects), `ERR-` (error handling
and diagnostics), `AST-` (parser and canonical AST shape), `PERF-` (allocation and hot paths),
`TIDY-` (small self-contained cleanups), `TEST-` (test infrastructure).

## The diagnostics program

`ERR-3` through `ERR-7`, plus `ERR-9`, are one body of work rather than loose items. The goal
is that **every phase can point its error at the source that caused it** — a caret under the offending text,
secondary labels for context, a suggestion where one exists. They have a dependency order, and
picking one up out of order mostly does not work:

```
BUG-6  rendering panics on 4 parse errors + 2 tokenizer errors   ← done 2026-08-27
  │
ERR-3  spans in the parser + canonical ASTs; PhaseError::labels() ← done 2026-08-27
  │
  ├── ERR-4  type-error provenance (typer Term → Constraint → unifier)
  ├── ERR-5  cross-module labels (Interface carries source ids)
  │      └── ERR-6  dependency cycles point at the `import` lines
  ├── ERR-9  span `parser::Exposed` — the one node ERR-3 left unspanned
  └── ERR-7  "did you mean" suggestions (better after ERR-9: a suggestion on
             `ValueNotFound` wants a caret under the name it is about)
ERR-8  warnings as a severity                                    ← independent, and see its
                                                                   own "open question"
```

`ERR-2` (closed) is the ancestor of all of them: it made every phase error describe itself in
prose, which is what left spans as the only thing missing.

**Closing convention: delete the ticket file, then rewrite its row below as a tombstone** —
same table, `status` becomes the close date. A closed ticket keeps accreting implementation
narrative that describes the tree as of the day it closed; the first change underneath it
turns that into a confident description of code which no longer exists. Anything worth keeping
longer than the fix is **promoted** before the ticket dies — into the code as a doc comment
where it explains behaviour, or into `CLAUDE.md`'s *Standing invariants* where it is a rule.
Two records of one decision means the unmaintained one is what someone eventually reads.

A tombstone row carries **no SHA and no PR number**. The commit that deletes a ticket file is
a commit on a branch, and when it is written neither the merge SHA nor the PR number exists
yet. The row may only contain what the closing commit can know about itself — and it doesn't
need more, because the file path is the query key.

## Recovering a closed ticket

The tombstone's job is not to link anywhere. It is to tell you that
`docs/tickets/<id>.md` once existed, because you cannot `git log` a path you have never heard
of.

```sh
git log --oneline --diff-filter=D -- docs/tickets/ast-1.md   # the commit that closed it
git show <that-sha>^:docs/tickets/ast-1.md                   # its full final text
git log --follow -- docs/tickets/ast-1.md                    # the ticket's whole life
```

Merge commits are transparent to this: `--diff-filter=D` on a path resolves to the branch
commit that did the delete, not to the merge. The PR is reachable separately —
`git log --grep=AST-1`, or `git log --merges -i --grep=ast-1`, since branch names put the ID
in the merge subject. That path often matters more than the ticket text: the review thread is
where "why were the first two revisions rejected" actually lives.

**The two tickets migrated as tombstones on 2026-08-25 are an exception.** `ERR-1` and
`TEST-1` were never files — they were items 1 and 9 of `TODO.md`, already complete when this
directory was created, and are recorded here so the numbering has no unexplained gap. Their
history is in `TODO.md` itself:

```sh
git log --oneline --diff-filter=D -- TODO.md    # the commit that removed it
git show <that-sha>^:TODO.md                    # the nine items in their final form
```

## Tickets

Open tickets link to their file. Rows with a close date are tombstones — the file is gone; see
[Recovering a closed ticket](#recovering-a-closed-ticket).

| ID | type | sev | status | title |
|---|---|---|---|---|
| BUG-1 | bug | medium | closed 2026-08-25 | `compile_package` reports success after emitting error diagnostics |
| BUG-2 | bug | medium | closed 2026-08-26 | One failing module discards every module that checked successfully |
| BUG-3 | bug | low | closed 2026-08-25 | `Bitwise.zel` imports the non-existent `Elm.Kernel.Bitwise` |
| BUG-4 | bug | medium | closed 2026-08-25 | The `Layout` iterator never terminates after a `LayoutError` |
| BUG-5 | bug | medium | closed 2026-08-26 | The `Tokenizer` never terminates on a tab used for indentation |
| BUG-6 | bug | medium | closed 2026-08-27 | Rendering a parse error panics for four of `parser::Error`'s five variants |
| [BUG-7](bug-7.md) | bug | low | open | The unclosed-char diagnostic draws two invisible carets, and swaps their messages |
| ERR-2 | task | — | closed 2026-08-26 | Unify the error-handling strategy across compiler phases |
| ERR-3 | task | — | closed 2026-08-27 | Give the parser and canonical ASTs spans, so diagnostics can point at source |
| [ERR-4](err-4.md) | task | — | open | Type errors point at the sub-expression, not at the whole declaration |
| [ERR-5](err-5.md) | task | — | open | A diagnostic can point into another module |
| [ERR-6](err-6.md) | task | — | open | A dependency cycle points at the `import` lines that form it |
| [ERR-7](err-7.md) | task | — | open | "Did you mean …?" on unresolved names |
| [ERR-8](err-8.md) | task | — | open | Let a phase report a warning |
| [ERR-9](err-9.md) | task | — | open | Span `parser::Exposed`, so an exposing list can be underlined |
| AST-1 | task | — | closed 2026-08-25 | Remove `Box<Vec<_>>` from the parser AST |
| AST-2 | task | — | closed 2026-08-26 | Unify the tuple representation across the parser and canonical ASTs |
| AST-3 | task | — | closed 2026-08-26 | Unify the typer's tuple representation with `Tuple<T>` |
| PERF-1 | task | — | closed 2026-08-25 | Reduce cloning in the `Layout` iterator |
| TIDY-1 | task | — | closed 2026-08-25 | Make `Name`'s inner `String` private |
| TIDY-2 | task | — | closed 2026-08-25 | Replace the tokenizer's keyword `HashMap` with a `match` |
| TIDY-3 | task | — | closed 2026-08-25 | Fix the `associativy` typo |
| TIDY-4 | task | — | closed 2026-08-25 | Test-module doc comments still describe the type checker as a stub |
| TIDY-5 | task | — | closed 2026-08-25 | Fix all outstanding `cargo clippy` warnings |
| TIDY-6 | task | — | closed 2026-08-26 | Stale doc comment on `canonical_type_to_typer_type` |
| ERR-1 | task | — | closed 2026-08-25 | Replace `panic!`/`unwrap()` with proper error handling in non-test code |
| TEST-1 | task | — | closed 2026-04-12 | Add integration tests running the full pipeline on `.zel` sources |
