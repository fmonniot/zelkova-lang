# Zelkova — Ticket index

_Last updated: 2026-08-29._

`SPEC-2` opened a second body of work alongside the diagnostics program below: specifying the
language itself, one chapter at a time. It is where the first four `LANG-` tickets came from, and it
will keep producing them — writing down a rule that was never written down is how you find out
the compiler had quietly picked a different one. `SPEC-3` added four more, plus three `BUG-`s,
from one chapter.

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
`TIDY-` (small self-contained cleanups), `TEST-` (test infrastructure), `SPEC-` (specifying
and documenting the language itself, under `docs/spec/`), `LANG-` (bringing the compiler into
line with a rule `docs/spec/` has since settled), `SITE-` (the public GitHub Pages site built
from this repo — rustdoc, the rendered spec, the landing page).

`LANG-` and `BUG-` are not the same thing and the split is worth keeping straight. A `BUG-` is
code that fails at what it was trying to do. A `LANG-` is code that succeeds at something the
language has since decided against — it was never wrong until a chapter was written, and the
chapter is the only reason it is a ticket. Every `LANG-` therefore names the chapter that
decided it and the tagged block there that goes red when it lands.

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
  ├── ERR-4  type-error provenance (typer Term → Constraint → unifier)  ← done 2026-08-27
  ├── ERR-5  cross-module labels (Interface carries source ids)    ← done 2026-08-27
  │      └── ERR-6  dependency cycles point at the `import` lines  ← done 2026-08-28
  ├── ERR-9  span `parser::Exposed` — the one node ERR-3 left unspanned ← done 2026-08-28
  └── ERR-7  "did you mean" suggestions (better after ERR-9: a suggestion on
             `ValueNotFound` wants a caret under the name it is about) ← done 2026-08-28
ERR-10  first real warning: unused imports in canonicalization    ← independent of the span
                                                                     work; exists to unblock ERR-8
  └── ERR-8  warnings as a severity                                 ← was "independent", now
                                                                       gated on ERR-10 landing a
                                                                       concrete diagnostic to carry
```

`ERR-2` (closed) is the ancestor of all of them: it made every phase error describe itself in
prose, which is what left spans as the only thing missing.

`ERR-9`'s Acceptance clause asked for a test covering an undeclared *value* in a module's own
`exposing (...)` header; the PR that closed it (#141) covers an undeclared *infix* instead.
That substitution was forced by the tree rather than a shortcut: `do_exports`
(`canonical/mod.rs`) only checked existence for the `Operator` case at the time, so a `Lower`
or `Upper` name in a header was accepted unconditionally and there was no way to reach
`Error::ExportNotFound` through either of them. That gap is now `BUG-8`, filed the same day
the ticket closed.

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
| [BUG-8](bug-8.md) | bug | medium | open | `do_exports` never checks that an exposed value or type actually exists |
| [BUG-9](bug-9.md) | bug | medium | open | A module's `exposing` list is computed and then never consulted |
| [BUG-10](bug-10.md) | bug | low | open | A `case` branch level with, or left of, the `case` keyword is accepted |
| [BUG-11](bug-11.md) | bug | low | open | The `Tokenizer` never terminates on a tab outside leading whitespace |
| [BUG-12](bug-12.md) | bug | medium | open | Four `unwrap()`s on user input panic the compiler instead of reporting a syntax error |
| [BUG-13](bug-13.md) | bug | medium | open | Block comments are lexed only at the start of a line, swallow the rest of their closing line, do not nest, and are accepted unterminated |
| [BUG-14](bug-14.md) | bug | medium | open | A top-level value with no type annotation never reaches the module's interface |
| [BUG-15](bug-15.md) | bug | medium | open | An imported operator is unresolvable unless the function behind it is also in scope |
| [BUG-16](bug-16.md) | bug | medium | open | An unresolved type name is invented rather than reported |
| ERR-2 | task | — | closed 2026-08-26 | Unify the error-handling strategy across compiler phases |
| ERR-3 | task | — | closed 2026-08-27 | Give the parser and canonical ASTs spans, so diagnostics can point at source |
| ERR-4 | task | — | closed 2026-08-27 | Type errors point at the sub-expression, not at the whole declaration |
| ERR-5 | task | — | closed 2026-08-27 | A diagnostic can point into another module |
| ERR-6 | task | — | closed 2026-08-28 | A dependency cycle points at the `import` lines that form it |
| ERR-7 | task | — | closed 2026-08-28 | "Did you mean …?" on unresolved names |
| [ERR-8](err-8.md) | task | — | open | Let a phase report a warning |
| ERR-9 | task | — | closed 2026-08-28 | Span `parser::Exposed`, so an exposing list can be underlined |
| [ERR-10](err-10.md) | task | — | open | Give a phase its first real warning: unused imports in canonicalization |
| [ERR-11](err-11.md) | task | — | open | A `case` branch indented deeper than its siblings is absorbed, and the error names the wrong token |
| [ERR-12](err-12.md) | task | — | open | Leading indentation before `module` is rejected only by accident, and the caret lands on an unrelated line |
| SPEC-1 | task | — | closed 2026-08-28 | Scaffold `docs/spec/` with an executable-example harness, and write the Layout chapter |
| SPEC-2 | task | — | closed 2026-08-29 | Make `docs/spec/` self-contained, and write the Lexical structure chapter |
| SPEC-3 | task | — | closed 2026-08-29 | Write the Modules, `exposing` and imports chapter, and settle multi-module examples |
| [LANG-1](lang-1.md) | task | — | open | Remove the `true`/`false` keywords; booleans are ordinary constructors |
| [LANG-2](lang-2.md) | task | — | open | `javascript` is reserved outright, unlike the other three soft keywords |
| [LANG-3](lang-3.md) | task | — | open | The tokenizer accepts a titlecase-initial identifier and a float with no digit after the point |
| [LANG-4](lang-4.md) | task | — | open | Prefix `-` is desugared to `0 - e`, so negating a `Float` mixes it with an `Int` literal |
| [LANG-5](lang-5.md) | task | — | open | An `import` is accepted anywhere among the declarations |
| [LANG-6](lang-6.md) | task | — | open | A module's declared name is unrelated to the file it lives in |
| [LANG-7](lang-7.md) | task | — | open | Nothing checks an import list for duplicates, alias collisions or self-imports |
| [LANG-8](lang-8.md) | task | — | open | There is no default import list |
| [SITE-1](site-1.md) | task | — | open | Publish a landing page and the rendered spec alongside the rustdoc on GitHub Pages |
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
