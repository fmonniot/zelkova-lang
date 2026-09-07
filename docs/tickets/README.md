# Zelkova — Ticket index

One file per ticket: `docs/tickets/<id-lower>.md`. Bugs and tasks share one ID namespace, one
closing convention and this one table — a bug is a ticket **type**, not a separate file. Each
ticket is self-contained so it can be picked up on its own: it names the **location**, the
**problem**, a suggested **fix** or **approach**, and an **acceptance** check that says when
it is done.

IDs are stable and are never reused; reference them when starting work ("work on `AST-1`").
Severity applies to bugs: **high** = miscompile or data loss, **medium** = wrong behaviour
under normal use, **low** = edge case or polish. Tasks carry a **Sizing** note in their own
file instead.

## Prefixes

Created ad-hoc per theme. Current ones: `BUG-` (defects), `ERR-` (error handling and
diagnostics), `AST-` (parser and canonical AST shape), `PERF-` (allocation and hot paths),
`TIDY-` (small self-contained cleanups), `TEST-` (test infrastructure), `SPEC-` (specifying
and documenting the language itself, under `docs/spec/`), `LANG-` (bringing the compiler into
line with a rule `docs/spec/` has since settled), `CLASS-` (the type-class program below —
building a mechanism the language has decided on but has never had), `SITE-` (the public GitHub
Pages site built from this repo — rustdoc, the rendered spec, the landing page), `GEN-` (code
generation — turning a checked module into runnable JavaScript, a phase that does not exist
yet).

Three distinctions worth keeping straight when filing a new one:

- **`LANG-` vs `BUG-`**: a `BUG-` is code that fails at what it was trying to do. A `LANG-` is
  code that succeeds at something the language has since decided against — it was never wrong
  until a chapter was written, and the chapter is the only reason it is a ticket. Every `LANG-`
  names the chapter that decided it and the tagged block there that goes red when it lands.
- **`CLASS-` vs `LANG-`**: a `CLASS-` is a construct the language does not have at all and is
  going to grow, not code that regressed against a settled rule. It gets its own prefix because
  the six of them are one ordered body of work (see below), and a reader picking one up needs
  the order more than the theme.
- **`SPEC-` tickets are filed before the chapter is written, not after.** The `write-spec-chapter`
  skill requires a chapter's `SPEC-n` to already exist before it will touch that chapter, so new
  `planned` chapters in `docs/spec/README.md` get their ticket filed up front.

## Closing a ticket

**Delete the ticket file, then rewrite its row below as a tombstone** — same table, `status`
becomes the close date. A closed ticket keeps accreting implementation narrative that describes
the tree as of the day it closed; the first change underneath it turns that into a confident
description of code which no longer exists. Anything worth keeping longer than the fix is
**promoted** before the ticket dies, to one of three places: into the code as a doc comment
where it explains behaviour, into `CLAUDE.md`'s *Standing invariants* where it is a rule, or
into [`docs/decisions/`](../decisions/README.md) where it is the argument for a choice rather
than the choice itself. Two records of one decision means the unmaintained one is what someone
eventually reads. A decision list in a closing ticket is cited as e.g. `DEC-2 decision 6` — an
entry is never deleted, so that citation keeps resolving.

A tombstone row carries **no SHA and no PR number**: the commit that deletes a ticket file is a
commit on a branch, and neither the merge SHA nor the PR number exists yet when it is written.
The file path is the query key; see [Recovering a closed ticket](#recovering-a-closed-ticket).

**Deleting a ticket a chapter cites turns `cargo test --test spec` red**, and fixing it is part
of closing the ticket. Chapters in [`docs/spec/`](../spec/README.md) cite ticket files from
their **Known gap:** and **Not implemented:** paragraphs, and `spec_cross_references_resolve`
checks that every one of those files still exists — a citation of a deleted ticket would be a
claim about a gap that may no longer exist. Grep the chapters for the ID before deleting its
file; usually the whole citing paragraph goes, because the gap it describes is the one that
just closed. The reasoning for checking this at all is [DEC-3](../decisions/dec-3.md).

## Active work: diagnostics

`ERR-3` through `ERR-9` (the "every phase can point its error at the source that caused it"
work) are done. The one live edge left is `ERR-10` (unused-import warnings), which gates
`ERR-8` (warnings as a severity) by giving it a concrete diagnostic to carry.

## Active work: type classes

`CLASS-1` through `CLASS-6` are one body of work, filed together after the language owner
settled the mechanism. The goal is that **a signature can say what it needs of its type** —
`min : Comparable a => a -> a -> a` rather than `a -> a -> a`, which is what `min`'s type has
always actually been.

[`docs/spec/type-classes.md`](../spec/type-classes.md) is the normative record and the thing to
read before picking any of these up: none of them re-argues a decision, and several would look
arbitrary without it. The decisions themselves, and the arguments the chapter does not carry,
are [DEC-2](../decisions/dec-2.md) — which is what the tickets below cite by number.

They have a dependency order, and three tickets that already existed sit inside it rather than
beside it:

```
CLASS-1  `=>` becomes a token; a constrained annotation parses
  │      (the only one that can start today)
  │
CLASS-2  `class` / `instance` declarations, and a `where` block of members
  │      ← LANG-9 sequences before this: an instance head wants `(List a)`
  │      ← an instance body is a member list or the single word `derived`;
  │        a class body may carry `derived <member>` — both specified in
  │        the chapter
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
  │      ← TEST-2 gates this one specifically, not the chapter: nothing
  │        about a class parses yet, so today's spec examples are all
  │        `expect=unimplemented`. They want `expect=type-error` once
  │        CLASS-4 lands, and the spec harness stops at canonicalization
  │        until TEST-2 extends it.
  │
  └── CLASS-6  `std/core` declares Eq, Comparable, Number, Appendable
                 ← needs CLASS-5, which is independent of this order

CLASS-5  `Type::Number` retires; an integer literal is an `Int`   ← independent
           ← supersedes ERR-13
```

**What is not a ticket: dictionary erasure.**
[`DEC-2` decision 7](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed)
settles that a constrained function is specialised per instantiation and no dictionary exists
at runtime — a constraint on code generation, which has not started. It is recorded in
`docs/spec/type-classes.md` and `docs/spec/js-interop.md`, and [`GEN-1`](gen-1.md) inherits it
from there rather than it being filed twice.

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
| [BUG-25](bug-25.md) | bug | medium | open | Three of the four `Float -> Int` conversions never wrap, so `round nan` and `round 1.0e20` are not `Int`s |
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
| SPEC-14 | task | — | closed 2026-09-06 | Nothing specifies how a structural instance is derived, and equality needs it |
| [SPEC-15](spec-15.md) | task | — | open | Nothing says what an effect is, so `main`'s type and what a test is are both undesigned |
| [SPEC-16](spec-16.md) | task | — | open | The spec makes one promise about space and does not say whether it makes others |
| SPEC-17 | task | — | closed 2026-09-06 | Nothing says a `Float`-returning operation may not totalize with a zero, and one of them does |
| [SPEC-18](spec-18.md) | task | — | open | "A subset of the Zelkova standard types" names no subset, and the compiler enforces none |
| [SPEC-19](spec-19.md) | task | — | open | `javascript` is the only interop modifier, and the WebAssembly equivalent is undesigned |
| SPEC-20 | task | — | closed 2026-09-06 | A facade constant is called unsettled by the chapter and shipped by `std/core` |
| [SPEC-21](spec-21.md) | task | — | open | Records are part of the language and no chapter says what one looks like |
| [SPEC-22](spec-22.md) | task | — | open | Lists are part of the language and the chapter specifying them does not exist |
| SPEC-23 | task | — | closed 2026-09-05 | Nothing checks the spec's own cross-references, and 271 of them are one rename from silence |
| SPEC-24 | task | — | closed 2026-09-05 | The conventions name every tag a chapter may write but not a single word it may write, and the tag table is four names short |
| [SPEC-25](spec-25.md) | task | — | open | A derivation walks two values, so the classes worth deriving most cannot be |
| SPEC-26 | task | — | closed 2026-09-06 | Design rationale has nowhere to live, so it is kept in three unrelated places or lost |
| [SPEC-27](spec-27.md) | task | — | open | A derivation's `combine` must be a monoid and nothing checks it, at any point |
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
