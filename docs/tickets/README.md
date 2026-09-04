# Zelkova — Ticket index

_Last updated: 2026-09-04._

`SPEC-2` opened a second body of work alongside the diagnostics program below: specifying the
language itself, one chapter at a time. It is where the first four `LANG-` tickets came from, and it
will keep producing them — writing down a rule that was never written down is how you find out
the compiler had quietly picked a different one. `SPEC-3` added four more, plus three `BUG-`s,
from one chapter, `SPEC-5` another four plus three `BUG-`s and a `TEST-`, `SPEC-11` a
`BUG-` and an `ERR-`, `SPEC-10` a `BUG-` and two `LANG-`s, `SPEC-7` another five, `SPEC-6`
three `LANG-`s, two `BUG-`s and a `SPEC-`, `SPEC-4` four more `LANG-`s, `SPEC-8` six
`LANG-`s and an `ERR-`, and `SPEC-9` two `LANG-`s, a `BUG-` and a `SPEC-`.

`SPEC-4` through `SPEC-11` were filed together on 2026-08-29, one per remaining `planned`
chapter in `docs/spec/README.md`'s table, rather than one at a time as each is picked up. That
is a deliberate exception: `write-spec-chapter` no longer files its own tracking ticket (doing
so from inside the run that later closes it reproduced the same-PR file-and-delete pattern
tickets otherwise avoid), so a chapter's `SPEC-n` has to already exist before that skill will
touch it. Filing the whole remaining list up front means it always does.

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
line with a rule `docs/spec/` has since settled), `CLASS-` (the type-class program below —
building a mechanism the language has decided on but has never had), `SITE-` (the public GitHub
Pages site built from this repo — rustdoc, the rendered spec, the landing page), `GEN-` (code
generation — turning a checked module into runnable JavaScript, a phase that does not exist
yet).

`CLASS-` is neither a `BUG-` nor a `LANG-`, and the distinction is the same one that separates
those two. A `LANG-` is code that succeeds at something a chapter has since decided against; a
`CLASS-` is a construct the language does not have at all and is going to grow. It gets its own
prefix rather than joining `LANG-` because the six of them are one ordered body of work, the way
`ERR-3` through `ERR-9` were, and a reader picking one up needs the rest of the order more than
they need the theme.

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

## The type-classes program

`CLASS-1` through `CLASS-6` are one body of work, filed together on 2026-08-29 after the
language owner settled the mechanism. The goal is that **a signature can say what it needs of
its type** — `min : Comparable a => a -> a -> a` rather than `a -> a -> a`, which is what
`min`'s type has always actually been.

[`docs/spec/type-classes.md`](../spec/type-classes.md) is the normative record and the thing to
read before picking any of these up: none of them re-argues a decision, and several would look
arbitrary without it. `SPEC-12` settled those decisions and wrote that chapter; its own ticket
file is gone, per the closing convention above, because the chapter is where the decisions live
now and two records of one decision means the unmaintained one is what someone eventually
reads.

They have a dependency order, and three tickets that already existed sit inside it rather than
beside it:

```
CLASS-1  `=>` becomes a token; a constrained annotation parses
  │      (the only one that can start today)
  │
CLASS-2  `class` / `instance` declarations, and a `where` block of members
  │      ← LANG-9 sequences before this: an instance head wants `(List a)`
  │      ← SPEC-14 is cheaper before this than after: one of the three
  │        shapes it weighs is a change to this declaration's grammar
  │
CLASS-3  resolution, the instance environment, and the orphan rule
  │      ← BUG-17 and BUG-16 are HARD prerequisites. Both would sabotage
  │        instance-head resolution silently: BUG-17 makes two instance
  │        heads indistinguishable, BUG-16 invents a type for a misspelt one.
  │
CLASS-4  the solver: obligations are collected, deferred and discharged
  │      ← LANG-12 is a HARD prerequisite. Without rigid annotation
  │        variables a constrained declaration proves `Comparable Int`
  │        and publishes `Comparable a` — strictly weaker than its own
  │        signature, and nothing downstream notices.
  │
  └── CLASS-6  `std/core` declares Eq, Comparable, Number, Appendable
                 ← needs CLASS-5, which is no longer inside this order:
                   `docs/spec/expressions.md` settles that a literal's type
                   is its spelling, so there is no obligation to discharge
                   and CLASS-5 can land at any point.  Closes BUG-20 for
                   the right reason.

CLASS-5  `Type::Number` retires; an integer literal is an `Int`   ← independent
           ← supersedes ERR-13

SPEC-12  the Type classes chapter          ← done 2026-08-29
```

**`TEST-2` was placed as a gate on the chapter and turned out not to be one.** The reasoning was
that every claim a class mechanism makes is a type-level claim and the harness stops at
canonicalization. Writing the chapter showed that is true of the claims it will make *once the
mechanism exists*, and not of the claims it makes today: nothing about a class parses, so all
eleven of its class-and-constraint blocks are `expect=unimplemented`, which the harness checks
perfectly well. `TEST-2` becomes load-bearing when `CLASS-4` lands and those blocks start wanting
`expect=type-error` — `docs/spec/expressions.md`'s *A literal's type is its spelling* section
already carries one `**Known gap:**` with no red test behind it for exactly this reason. The `CLASS-` tickets are
held to `tests/typer.rs`, which already reaches the typer.

**What is not a ticket:** dictionary erasure. `SPEC-12` decision 7 settles that a constrained
function is specialised per instantiation and no dictionary exists at runtime — which is a
constraint on code generation, and code generation has not started. It is recorded in
`docs/spec/type-classes.md` and in `docs/spec/js-interop.md`, and [`GEN-1`](gen-1.md) — the
ticket that starts the backend — inherits it from there, rather than it being filed twice.

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
| [BUG-17](bug-17.md) | bug | high | open | A type application's arguments are discarded when its head resolves |
| [BUG-18](bug-18.md) | bug | medium | open | A variant that is not a constructor application is silently dropped |
| [BUG-19](bug-19.md) | bug | medium | open | A line whose first token starts with `-` leaves the tokenizer measuring indentation mid-line |
| [BUG-20](bug-20.md) | bug | high | open | `Js.Utils`'s comparison and append facades declare a type the JavaScript cannot honour |
| [BUG-21](bug-21.md) | bug | medium | open | Every error from the source-directory walk is discarded, so a missing package root compiles as success |
| [BUG-22](bug-22.md) | bug | high | open | An operator's declared precedence and associativity are recorded and then ignored |
| [BUG-23](bug-23.md) | bug | medium | open | An `else` does not close a `case` block, so a `case` in a `then` arm is a layout error |
| [BUG-24](bug-24.md) | bug | medium | open | Two `.mjs` companions call helpers no file defines, so `modBy 0` and comparing functions are `ReferenceError`s |
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
| [ERR-13](err-13.md) | task | — | open | A type error spells the numeric-literal type `number`, which the language reads as an ordinary type variable |
| [ERR-14](err-14.md) | task | — | open | A qualified name whose module is not imported is reported as a missing value |
| SPEC-1 | task | — | closed 2026-08-28 | Scaffold `docs/spec/` with an executable-example harness, and write the Layout chapter |
| SPEC-2 | task | — | closed 2026-08-29 | Make `docs/spec/` self-contained, and write the Lexical structure chapter |
| SPEC-3 | task | — | closed 2026-08-29 | Write the Modules, `exposing` and imports chapter, and settle multi-module examples |
| SPEC-4 | task | — | closed 2026-09-02 | Write the Declarations chapter |
| SPEC-5 | task | — | closed 2026-08-29 | Write the Types and type annotations chapter |
| SPEC-6 | task | — | closed 2026-08-30 | Write the Expressions chapter |
| SPEC-7 | task | — | closed 2026-08-29 | Write the Patterns chapter |
| SPEC-8 | task | — | closed 2026-09-02 | Write the Name resolution and scoping chapter |
| SPEC-9 | task | — | closed 2026-09-02 | Write the Evaluation semantics chapter |
| SPEC-10 | task | — | closed 2026-08-30 | Write the Packages and source layout chapter |
| SPEC-11 | task | — | closed 2026-08-29 | Write the Constrained type variables chapter |
| SPEC-12 | task | — | closed 2026-08-29 | Write the Type classes chapter, superseding Constrained type variables |
| [SPEC-13](spec-13.md) | task | — | open | Whether a pattern's negative literal is a token or a pattern production is unsettled, and two chapters answer it differently |
| [SPEC-14](spec-14.md) | task | — | open | Nothing specifies how a structural instance is derived, and equality needs it |
| [LANG-1](lang-1.md) | task | — | open | Remove the `true`/`false` keywords; booleans are ordinary constructors |
| [LANG-2](lang-2.md) | task | — | open | `javascript` is reserved outright, unlike the other three soft keywords |
| [LANG-3](lang-3.md) | task | — | open | The tokenizer accepts a titlecase-initial identifier and a float with no digit after the point |
| [LANG-4](lang-4.md) | task | — | open | Prefix `-` is desugared to `0 - e`, so negating a `Float` mixes it with an `Int` literal |
| [LANG-5](lang-5.md) | task | — | open | An `import` is accepted anywhere among the declarations |
| [LANG-6](lang-6.md) | task | — | open | A module's declared name is unrelated to the file it lives in |
| [LANG-7](lang-7.md) | task | — | open | Nothing checks an import list for duplicates, alias collisions or self-imports |
| [LANG-8](lang-8.md) | task | — | open | There is no default import list |
| [LANG-9](lang-9.md) | task | — | open | A type argument must be a bare name, so `Maybe (Maybe Int)` does not parse |
| [LANG-10](lang-10.md) | task | — | open | A trailing `\|` and a variant-less `type T =` are both accepted |
| [LANG-11](lang-11.md) | task | — | open | A type annotation may sit anywhere in the file, and a repeated one silently wins |
| [LANG-12](lang-12.md) | task | — | open | An annotation more general than its body is accepted and silently specialised |
| [LANG-13](lang-13.md) | task | — | open | A package has no manifest, and its name is hardcoded |
| [LANG-14](lang-14.md) | task | — | open | Nothing implements a package boundary |
| [LANG-15](lang-15.md) | task | — | open | A package has no test root, and nothing runs a package's tests |
| [LANG-16](lang-16.md) | task | — | open | A constructor pattern may not nest, and may not be parenthesised in a `case` branch |
| [LANG-17](lang-17.md) | task | — | open | A constructor pattern's arity is never checked |
| [LANG-18](lang-18.md) | task | — | open | A pattern may bind the same name more than once |
| [LANG-19](lang-19.md) | task | — | open | Nothing checks that a `case` covers its type |
| [LANG-20](lang-20.md) | task | — | open | A declaration may have only one clause |
| [LANG-21](lang-21.md) | task | — | open | A `case … of` cannot be parenthesised, so it is not an expression |
| [LANG-22](lang-22.md) | task | — | open | An operator's right operand may not be an `if`, a `case`, or a negation |
| [LANG-23](lang-23.md) | task | — | open | An operator cannot be named in an expression, so an exported one is unusable as a value |
| [LANG-24](lang-24.md) | task | — | open | An `infix` precedence outside 0–9 is accepted |
| [LANG-25](lang-25.md) | task | — | open | A declaration may not name fewer parameters than its annotation has arrows |
| [LANG-26](lang-26.md) | task | — | open | A declaration's clauses need not stand together |
| [LANG-27](lang-27.md) | task | — | open | An operator may carry more than one `infix` declaration, and the last silently wins |
| [LANG-28](lang-28.md) | task | — | open | An `infix` declaration's function is never checked to take two arguments |
| [LANG-29](lang-29.md) | task | — | open | A top-level declaration silently shadows a name imported unqualified |
| [LANG-30](lang-30.md) | task | — | open | Ambiguity is detected for values only; a type, constructor or operator is taken from the last import |
| [LANG-31](lang-31.md) | task | — | open | A variant may use a type variable its declaration does not bind |
| [LANG-32](lang-32.md) | task | — | open | A module may declare one type twice, and the second silently replaces the first |
| [LANG-33](lang-33.md) | task | — | open | There is no `let … in` production, so a local binding cannot be written |
| [LANG-34](lang-34.md) | task | — | open | There is no lambda production, so `\x -> x` is read as an operator |
| [LANG-35](lang-35.md) | task | — | open | A parameterless binding may depend on itself, and nothing notices |
| [LANG-36](lang-36.md) | task | — | open | `std/core`'s `Basics` documents three semantics the language does not have |
| [CLASS-1](class-1.md) | task | — | open | A type annotation may carry a constraint context, written `Class a =>` |
| [CLASS-2](class-2.md) | task | — | open | `class` and `instance` declarations parse, with a `where` block of members |
| [CLASS-3](class-3.md) | task | — | open | Resolve classes and instances, and enforce the orphan rule |
| [CLASS-4](class-4.md) | task | — | open | Discharge class constraints in the type checker |
| [CLASS-5](class-5.md) | task | — | open | Retire `Type::Number` in favour of a `Number` class, defaulting to `Int` |
| [CLASS-6](class-6.md) | task | — | open | `std/core` declares `Eq`, `Comparable`, `Number` and `Appendable` |
| [SITE-1](site-1.md) | task | — | open | Publish a landing page and the rendered spec alongside the rustdoc on GitHub Pages |
| [GEN-1](gen-1.md) | task | — | open | Emit runnable JavaScript for a checked module |
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
| [TEST-2](test-2.md) | task | — | open | The spec harness stops at canonicalization, so no chapter can pin a type error |
