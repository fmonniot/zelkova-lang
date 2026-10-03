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

This is a closed list — pick the one that fits, don't mint a new one. A theme that doesn't fit
any of these is a question for the language owner, not a call a session makes on its own.

| Prefix | Theme |
|---|---|
| `BUG-` | Defects |
| `ERR-` | Error handling and diagnostics |
| `AST-` | Parser and canonical AST shape |
| `PERF-` | Allocation and hot paths |
| `TIDY-` | Small self-contained cleanups |
| `TEST-` | Test infrastructure |
| `SPEC-` | Specifying and documenting the language itself, under `docs/spec/` |
| `LANG-` | Bringing the compiler into line with a rule `docs/spec/` has since settled |
| `SITE-` | The public GitHub Pages site built from this repo — rustdoc, the rendered spec, the landing page |
| `GEN-` | Code generation — turning a checked module into runnable JavaScript, a phase that does not exist yet |
| `TOOL-` | Developer tooling outside the compile pipeline — editor support, the language server, and the crate layout that serves them |

Two distinctions worth keeping straight when filing a new ticket:

- **`LANG-` vs `BUG-`**: a `BUG-` is code that fails at what it was trying to do. A `LANG-` is
  code that succeeds at something the language has since decided against — it was never wrong
  until a chapter was written, and the chapter is the only reason it is a ticket. Every `LANG-`
  names the chapter that decided it and the tagged block there that goes red when it lands.

## Closing a ticket

**Delete the ticket file, then rewrite its row below as a tombstone** — same table, `status`
becomes the close date. A closed ticket keeps accreting implementation narrative that describes
the tree as of the day it closed; the first change underneath it turns that into a confident
description of code which no longer exists. Anything worth keeping longer than the fix is
**promoted** before the ticket dies, and the destination is chosen in this order: into the code
as a doc comment where it explains behaviour, into [`docs/spec/`](../spec/README.md) where it is
a rule about the language, into [`docs/decisions/`](../decisions/README.md) where it is the
argument for a choice rather than the choice itself, and into `CLAUDE.md`'s *Standing
invariants* only when none of the three would carry it. `CLAUDE.md` comes last because it is the
one destination that can be appended to without opening the thing it describes — it doubled in a
fortnight that way — and a line added there means first checking that no existing line already
covers it. Two records of one decision means the unmaintained one is what someone eventually
reads. A decision list in a closing ticket is cited as e.g. `DEC-2 decision 6` — an
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

## Active work: type classes

Eleven tickets are one body of work: `LANG-37` through `LANG-42`, `LANG-70`, `LANG-71`,
`LANG-83`, `LANG-12` and `GEN-24`. The goal is that **a signature can say what it needs of its
type** — `min : Comparable a => a -> a -> a` rather than `a -> a -> a`, which is what `min`'s
type has always actually been.

They get their own section because most tickets each close a complete, independently shippable
gap on landing, while these are fragments of one mechanism that only works once the chain
lands. `LANG-41` is the exception: it stands alone, and is in the order because `LANG-40` needs
it gone first.

[`docs/spec/type-classes.md`](../spec/type-classes.md) is the normative record and the thing to
read before picking any of these up: none of them re-argues a decision, and several would look
arbitrary without it. The arguments the chapter does not carry are two entries:
[DEC-2](../decisions/dec-2.md), the eleven decisions that settled what a class is, and
[DEC-24](../decisions/dec-24.md), the twelve that implementing one had to settle — which is
what the tickets below cite by number. **No ticket in the order leaves a language decision
open.** Where one says a choice is the implementer's, it means that and names the constraints;
anything else that looks like a decision is a gap to report, not to fill.

They land in this order, one at a time, each on a `main` whose `std/core` compiles and whose
tests pass:

```
LANG-37  `=>` becomes a token; a constrained annotation parses   ← closed
  │
LANG-71  a context holds any number of constraints, as a list
  │      ← first, because a class head and an instance head parse a
  │        context too, and LANG-38 is written against this shape
  │
LANG-41  `Type::Number` retires; an integer literal is an `Int`
  │      ← needs nothing and may land beside LANG-38 or LANG-39; it is
  │        here because LANG-40 must not meet an obligation at a type
  │        that is neither `Int` nor `Float`
  │      ← closes ERR-13
  │
LANG-38  `class` / `instance` parse, with a `where` block of members
  │      ← syntax only: canonicalization rejects each one, the way a
  │        multi-clause declaration is rejected today
  │
LANG-39  resolution: what a class and an instance are, members in the
  │      value namespace, instances across modules, the orphan rule
  │      ← the emitter refuses a module holding a class from here on
  │
LANG-70  a constraint in an annotation is resolved; its context is
  │      kept, on the value and in the `Interface`
  │
LANG-40  the solver: obligations are collected, deferred and discharged
  │      ← on today's flexible annotation variables. A constrained
  │        declaration can under-prove its signature until LANG-12,
  │        and a test here pins that it does
  │
LANG-83  a derivation is checked, and a `derived` instance gets members
  │
GEN-24   specialisation: a member, an instance and a constrained
  │      function are emitted
  │      ← the first point at which a program using a class runs
  │
LANG-42  `std/core` declares Eq, Comparable, Number, Appendable
  │      ← closes BUG-20, BUG-44 and BUG-46
  │
LANG-12  an annotation's variables are rigid
         ← last, where it used to be LANG-40's hard prerequisite: until
           LANG-42 it rejects thirteen declarations of `Basics` itself
           (DEC-24 decision 10). Closes SPEC-36, and the order
```

Two tickets sit just outside it. [`LANG-4`](lang-4.md) wants prefix `-` to mean `negate`, and
has a `Float`-capable `negate` to desugar to once `LANG-42` lands; between `LANG-41` and then,
`-x` on a `Float` is a type error where it used to abort at run time.
[`PERF-2`](perf-2.md) is narrowed by `LANG-42`, which rewrites most of the forwarding
declarations it is about, and not closed by it.

## Active work: records

The order is complete: its last ticket, `GEN-25`, has landed. The goal was that **a value of
several parts can name them** — `{ taken : Celsius, expected : Celsius }` where a tuple says
which part is which by position and stops at three.

The work gets a section for the reason the type classes do: none of its tickets is a record on
its own. A
record has a spelling in the type, expression and pattern grammars and a rule in the typer, and
the tickets are cut along those lines, with the emitter last.

[`docs/spec/records.md`](../spec/records.md) is the normative record and
[DEC-8](../decisions/dec-8.md) holds the ten decisions behind it, which the tickets cite by
number. **No ticket in the order leaves a language decision open**, and none re-argues one.
Where a ticket says a choice is the implementer's, it means that and names the constraints.
`LANG-52` and `LANG-50` have landed and put the whitespace rule in the tokenizer: it reads a `.`
written against an operand on its left and against the character after it as `Dot`, any other
`.` written against a lowercase name after it as the `AccessorDot` an accessor begins with,
`(.name)` included, and the rest as `SpacedDot`. An access may be written on every atomic
expression but a bare constructor name, so `Widget.size` stays a qualified name. The reasons are
in the doc comments on `consume_operator` in `crates/zelkova-syntax/src/parser/tokenizer.rs` and
on `Accessible` in `grammar.lalrpop`. Anything else that looks like a decision is a gap to report,
not to fill.

`LANG-48` and `LANG-50` have landed: a record type, a record, an update, a field access and an
accessor parse and reach the canonical module, the type as a set of fields, and a repeated label
is reported as `canonical::Error::RepeatedLabel`.

`LANG-16`, which the order took as a prerequisite, has landed too: a pattern nests to any depth,
an applied constructor written as a sub-pattern being parenthesised, and the typer checks a
nested pattern as it does one at the top. That is what the first record-pattern block in two
chapters needs, since it matches a constructor inside a field, and what `LANG-84` built on.

`LANG-49` has landed: a record pattern parses and reaches the canonical module, the `{ x }`
shorthand desugared in the grammar to `x = x`, and a repeated label in one is the
`RepeatedLabel` the other three forms report.

`LANG-51` has landed: the typer has a record type, two of which unify when they carry the same
labels and each pair of field types unifies, and it checks a record, an update, a field access
and an accessor. The last three are read once unification has run over the declaration, and are an error where
nothing in it supplies the record type; `FieldConstraint` in `typer/mod.rs` is the mechanism.

`LANG-84` has landed: the typer checks a record pattern, each entry read against the matched
record type by that same mechanism, and it is an error where that type lacks a label the
pattern names or nothing supplies the type.

`GEN-25` has landed, and a program using a record runs. A record is a plain JavaScript object
keyed by its labels, its fields in the order they were written; an update spreads the record it
updates into a new object; an access and an accessor are a property read; and a record pattern
tests nothing of its own, reading each entry as a property. A record crosses a facade when it
holds exactly its labels as own keys, each field passing its own type's predicate. The
representation is written in `zelkova-js`'s module doc comment, *Representations*, and not in
a chapter: [Interop](../spec/interop.md) leaves a record's encoding to code generation, and
whether it publishes one is the language owner's. `==` on two records works today because
`Js.Utils`'s companion walks an object's keys; `LANG-42` replaces that forwarding and
`LANG-85` is the walk that takes its place.

Two tickets follow from records and are outside the order. [`LANG-85`](lang-85.md) is a record
under a derivation — what `==` on two records computes once [`LANG-42`](lang-42.md) lands — and
is the one ticket that needs both this order and the type classes' finished.
[`LANG-86`](lang-86.md) is `type alias`. It is not a record question and needs none of the
above, but a record type is written out in full wherever it appears, so records are where its
absence is felt. [DEC-25](../decisions/dec-25.md) holds its two decisions. Neither ticket leaves
a language decision open; `LANG-85` leaves one choice to the implementer and says so.

Three more sit beside the order. [`LANG-33`](lang-33.md) is `let`, the second place an
irrefutable record pattern may be written. [`LANG-19`](lang-19.md) scopes exhaustiveness to the
patterns the language has today, and `LANG-49` landed first, so `LANG-19` is the one that adds
the record pattern to them. [`LANG-40`](lang-40.md) adds a constraint the solver answers only
once unification has run, which is what `LANG-51`'s accessor is too: the two are written in the
same files of `typer/`, `LANG-51` landed first, and `LANG-40` is written against its mechanism,
whose doc comment says where a class constraint would sit.

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
| BUG-7 | bug | low | closed 2026-09-11 | The unclosed-char diagnostic draws two invisible carets, and swaps their messages |
| BUG-8 | bug | medium | closed 2026-09-12 | `do_exports` never checks that an exposed value or type actually exists |
| BUG-9 | bug | medium | closed 2026-09-12 | A module's `exposing` list is computed and then never consulted |
| BUG-10 | bug | low | closed 2026-09-12 | A `case` branch level with, or left of, the `case` keyword is accepted |
| BUG-11 | bug | low | closed 2026-09-11 | The `Tokenizer` never terminates on a tab outside leading whitespace |
| BUG-12 | bug | medium | closed 2026-09-12 | Four `unwrap()`s on user input panic the compiler instead of reporting a syntax error |
| BUG-13 | bug | medium | closed 2026-09-12 | Block comments are lexed only at the start of a line, swallow the rest of their closing line, do not nest, and are accepted unterminated |
| BUG-14 | bug | medium | closed 2026-09-12 | A top-level value with no type annotation never reaches the module's interface |
| BUG-15 | bug | medium | closed 2026-09-14 | An imported operator is unresolvable unless the function behind it is also in scope |
| BUG-16 | bug | medium | closed 2026-09-19 | An unresolved type name is invented rather than reported |
| BUG-17 | bug | high | closed 2026-09-10 | A type application's arguments are discarded when its head resolves |
| BUG-18 | bug | medium | closed 2026-09-13 | A variant that is not a constructor application is silently dropped |
| BUG-19 | bug | medium | closed 2026-09-12 | A line whose first token starts with `-` leaves the tokenizer measuring indentation mid-line |
| [BUG-20](bug-20.md) | bug | high | open | `Js.Utils`'s comparison and append facades declare a type the JavaScript cannot honour |
| BUG-21 | bug | medium | closed 2026-09-13 | Every error from the source-directory walk is discarded, so a missing package root compiles as success |
| BUG-22 | bug | high | closed 2026-09-10 | An operator's declared precedence and associativity are recorded and then ignored |
| BUG-23 | bug | medium | closed 2026-09-12 | An `else` does not close a `case` block, so a `case` in a `then` arm is a layout error |
| BUG-24 | bug | medium | closed 2026-09-13 | Two `.mjs` companions call helpers no file defines, so `modBy 0` and comparing functions are `ReferenceError`s |
| BUG-25 | bug | medium | closed 2026-09-13 | Three of the four `Float -> Int` conversions never wrap, so `round nan` and `round 1.0e20` are not `Int`s |
| BUG-26 | bug | medium | closed 2026-09-16 | A module that declares `Bool`, `Int`, `Char` or `Float` cannot annotate anything with it |
| BUG-27 | bug | medium | closed 2026-09-21 | A canonicalized infix operator is qualified under its own symbol, not the function its `infix` declaration names |
| BUG-28 | bug | low | closed 2026-09-30 | The `Tokenizer` never terminates on an unterminated character literal |
| BUG-29 | bug | medium | closed 2026-09-30 | A top-level declaration whose first token is not at column 1 fails to parse |
| BUG-30 | bug | medium | closed 2026-10-01 | An `Upper(..)` import entry does not check the type was exposed transparently |
| [BUG-31](bug-31.md) | bug | medium | open | `do_exports` accepts a `Lower`/`Upper` name that resolves only through an import |
| [BUG-32](bug-32.md) | bug | medium | open | An exposed infix's unannotated backing function is silently dropped from the interface |
| [BUG-33](bug-33.md) | bug | low | open | `SourceFileError::notes()` dumps `io::Error`'s `Debug` form instead of its `Display` form |
| BUG-34 | bug | low | closed 2026-10-01 | A failed sub-pass in `canonicalize` reports as if it found nothing, cascading into spurious errors from every later pass that depended on it |
| BUG-35 | bug | medium | closed 2026-09-16 | The typer identifies a union type by its unqualified name, so two modules' `Size` are one type |
| BUG-36 | bug | medium | closed 2026-09-23 | A value that reaches an imported constructor or an imported value is never type checked |
| BUG-37 | bug | high | closed 2026-09-27 | A package is not part of a type's identity, so two packages' same-named modules are one type |
| BUG-38 | bug | high | closed 2026-09-27 | A parameterless binding that reaches another only through a function it calls is not ordered after it |
| BUG-39 | bug | medium | closed 2026-09-23 | A function parameter pattern other than a variable or `_` is never type checked |
| BUG-40 | bug | high | closed 2026-09-27 | Two same-named modules from different packages emit colliding local import bindings |
| [BUG-41](bug-41.md) | bug | low | open | A union reached only transitively has no spelling, and `Spellings::spell` falls back to a name that can still collide |
| [BUG-42](bug-42.md) | bug | low | open | A module-name collision with a test-dependency's module is found only after `src/` checks |
| BUG-43 | bug | high | closed 2026-09-28 | A call to another module's function of two or more parameters is emitted one argument at a time against a plain n-ary function |
| [BUG-44](bug-44.md) | bug | medium | open | `Float` arithmetic aborts at the `Int` facade's boundary check |
| [BUG-45](bug-45.md) | bug | low | open | A facade signature may name a union whose constructors hold a type no predicate decides |
| [BUG-46](bug-46.md) | bug | low | open | `Js.Utils.compareInt` and `compareFloat` declare an `Int` result their companion returns as a number |
| [BUG-47](bug-47.md) | bug | low | open | A qualified name an imported module does not expose is reported as under the importing module |
| [BUG-48](bug-48.md) | bug | medium | open | `Js.Utils`'s structural equality throws a `ReferenceError` on a value nested more than a hundred deep |
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
| [ERR-15](err-15.md) | task | — | open | `TypeNotFound` carries no "did you mean …?" suggestion |
| [ERR-16](err-16.md) | task | — | open | `ModuleNameCollision` and `ReservedModuleName` have a file to point at and don't |
| [ERR-17](err-17.md) | task | — | open | A mistyped bare identifier or constructor body gets no caret of its own |
| [ERR-18](err-18.md) | task | — | open | An unexpected-token error names the token by its Rust variant, not as the user wrote it |
| [ERR-19](err-19.md) | task | — | open | A module name used as a constructor is reported as a missing constructor of the current module |
| [ERR-20](err-20.md) | task | — | open | A body using a name a record pattern binds at the wrong type is reported at the pattern |
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
| SPEC-13 | task | — | closed 2026-09-06 | Whether a pattern's negative literal is a token or a pattern production is unsettled, and two chapters answer it differently |
| SPEC-14 | task | — | closed 2026-09-06 | Nothing specifies how a structural instance is derived, and equality needs it |
| SPEC-15 | task | — | closed 2026-09-08 | Nothing says what an effect is, so `main`'s type and what a test is are both undesigned |
| SPEC-16 | task | — | closed 2026-09-08 | The spec makes one promise about space and does not say whether it makes others |
| SPEC-17 | task | — | closed 2026-09-06 | Nothing says a `Float`-returning operation may not totalize with a zero, and one of them does |
| SPEC-18 | task | — | closed 2026-09-06 | "A subset of the Zelkova standard types" names no subset, and the compiler enforces none |
| SPEC-19 | task | — | closed 2026-09-09 | `javascript` is the only interop modifier, and the WebAssembly equivalent is undesigned |
| SPEC-20 | task | — | closed 2026-09-06 | A facade constant is called unsettled by the chapter and shipped by `std/core` |
| SPEC-21 | task | — | closed 2026-09-07 | Records are part of the language and no chapter says what one looks like |
| SPEC-22 | task | — | closed 2026-09-06 | Lists are part of the language and the chapter specifying them does not exist |
| SPEC-23 | task | — | closed 2026-09-05 | Nothing checks the spec's own cross-references, and 271 of them are one rename from silence |
| SPEC-24 | task | — | closed 2026-09-05 | The conventions name every tag a chapter may write but not a single word it may write, and the tag table is four names short |
| SPEC-25 | task | — | closed 2026-09-08 | A derivation walks two values, so the classes worth deriving most cannot be |
| SPEC-26 | task | — | closed 2026-09-06 | Design rationale has nowhere to live, so it is kept in three unrelated places or lost |
| SPEC-27 | task | — | closed 2026-09-08 | A derivation's `combine` must be a monoid and nothing checks it, at any point |
| SPEC-28 | task | — | closed 2026-09-15 | Two chapters disagree on `Int`'s range, and the tokenizer enforces a third bound |
| SPEC-29 | task | — | closed 2026-09-15 | `unsafe` marks 45 `std/core` signatures and none of the chapter's own examples |
| SPEC-30 | task | — | closed 2026-09-15 | `unsafe` outside a facade is rejected, and no chapter says so |
| SPEC-31 | task | — | closed 2026-09-14 | A facade has no legitimate way to name `Int`, and `BUG-16` is what hides it |
| SPEC-32 | task | — | closed 2026-09-15 | A module is made ambiguous by an import it never wrote |
| SPEC-33 | task | — | closed 2026-09-15 | Which default imports a module gets is a fixed point over the whole package |
| SPEC-34 | task | — | closed 2026-09-27 | Only `zelkova-core` may declare a module the default imports name, and the exemption is keyed on the package rather than on module names |
| SPEC-35 | task | — | closed 2026-09-27 | A package cannot be tested with a library that depends on it |
| [SPEC-36](spec-36.md) | task | — | open | The `double` block in `expressions.md` cannot go red for the reason its paragraph gives |
| SPEC-37 | task | — | closed 2026-09-28 | How a `Task` is represented and run is undesigned, on either target |
| [SPEC-38](spec-38.md) | task | — | open | `patterns.md` parenthesises every sub-pattern and also writes `Circle n :: rest` bare |
| LANG-1 | task | — | closed 2026-10-01 | Remove the `true`/`false` keywords; booleans are ordinary constructors |
| LANG-2 | task | — | closed 2026-09-13 | `javascript` is reserved outright, unlike the other three soft keywords — subsumed by LANG-54 |
| [LANG-3](lang-3.md) | task | — | open | The tokenizer accepts a titlecase-initial identifier and a float with no digit after the point |
| [LANG-4](lang-4.md) | task | — | open | Prefix `-` is desugared to `0 - e`, so negating a `Float` mixes it with an `Int` literal |
| [LANG-5](lang-5.md) | task | — | open | An `import` is accepted anywhere among the declarations |
| [LANG-6](lang-6.md) | task | — | open | A module's declared name is unrelated to the file it lives in |
| [LANG-7](lang-7.md) | task | — | open | Nothing checks an import list for duplicates, alias collisions or self-imports |
| LANG-8 | task | — | closed 2026-09-13 | There is no default import list |
| LANG-9 | task | — | closed 2026-09-27 | A type argument must be a bare name, so `Maybe (Maybe Int)` does not parse |
| [LANG-10](lang-10.md) | task | — | open | A trailing `\|` and a variant-less `type T =` are both accepted |
| [LANG-11](lang-11.md) | task | — | open | A type annotation may sit anywhere in the file, and a repeated one silently wins |
| [LANG-12](lang-12.md) | task | — | open | An annotation more general than its body is accepted and silently specialised |
| LANG-13 | task | — | closed 2026-09-19 | A package has no manifest, and its name is hardcoded |
| LANG-14 | task | — | closed 2026-09-20 | Nothing implements a package boundary |
| LANG-15 | task | — | closed 2026-09-20 | A package has no test root, and nothing runs a package's tests |
| LANG-16 | task | — | closed 2026-10-02 | A constructor pattern may not nest, and may not be parenthesised in a `case` branch |
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
| LANG-35 | task | — | closed 2026-09-20 | A parameterless binding may depend on itself, and nothing notices |
| [LANG-36](lang-36.md) | task | — | open | `std/core`'s `Basics` documents three semantics the language does not have |
| LANG-37 | task | — | closed 2026-09-27 | A type annotation may carry a constraint context, written `Class a =>` |
| [LANG-38](lang-38.md) | task | — | open | `class` and `instance` declarations parse, with a `where` block of members |
| [LANG-39](lang-39.md) | task | — | open | Resolve classes and instances, and enforce the orphan rule |
| [LANG-40](lang-40.md) | task | — | open | Discharge class constraints in the type checker |
| [LANG-41](lang-41.md) | task | — | open | Retire `Type::Number`: an integer literal is an `Int` |
| [LANG-42](lang-42.md) | task | — | open | `std/core` declares `Eq`, `Comparable`, `Number` and `Appendable` |
| LANG-43 | task | — | closed 2026-09-27 | A facade signature may name any type at all, including ones no runtime predicate can decide |
| [LANG-44](lang-44.md) | task | — | open | There is no list-literal production, so `[1, 2]` does not parse |
| [LANG-45](lang-45.md) | task | — | open | There is no list pattern, so neither `[]` nor `first :: rest` can be matched |
| [LANG-46](lang-46.md) | task | — | open | `std/core` declares `List`, opaquely, with `(::)` over it |
| LANG-47 | task | — | closed 2026-10-02 | `{` and `}` are not tokens, so nothing in a record reaches the grammar |
| LANG-48 | task | — | closed 2026-10-02 | There is no record production, so a record type, a record and an update do not parse |
| LANG-49 | task | — | closed 2026-10-02 | There is no record pattern production |
| LANG-50 | task | — | closed 2026-10-02 | Field access `r.name` and the accessor `.name` do not parse |
| LANG-51 | task | — | closed 2026-10-02 | The typer has no record type, so nothing checks a field, an update or an accessor |
| LANG-52 | task | — | closed 2026-10-02 | Whitespace around a qualification dot is accepted, and records need it not to be |
| LANG-53 | task | — | closed 2026-09-13 | A facade signature cannot be marked `unsafe`, and an unmarked one is held to nothing |
| LANG-54 | task | — | closed 2026-09-13 | The interop modifier is `foreign`, not `javascript` |
| LANG-55 | task | — | closed 2026-09-16 | The `Char` and `String` default imports bring their modules but not their types |
| LANG-56 | task | — | closed 2026-09-20 | `std/core`'s JavaScript companions implement a 32-bit `Int` held in a number |
| LANG-57 | task | — | closed 2026-09-15 | The default imports are dropped entry by entry, not by package |
| LANG-58 | task | — | closed 2026-09-17 | A module underneath `Basics` cannot name a scalar type |
| LANG-59 | task | — | closed 2026-09-17 | A scalar type's declaration is an ordinary union |
| LANG-60 | task | — | closed 2026-09-16 | The typer gives `Bool` a literal type, so inside `Basics` it does not match `True` and `False` |
| [LANG-61](lang-61.md) | task | — | open | A `git` dependency is not fetched, and nothing writes or reads `zelkova.lock` |
| [LANG-62](lang-62.md) | task | — | open | The compiler carries no copy of `zelkova-core`, so a package has to write it in `dependencies` |
| LANG-63 | task | — | closed 2026-09-27 | Nothing declares `Test`, and nothing finds a package's tests |
| LANG-64 | task | — | closed 2026-09-20 | A shift count is clamped into `0 .. 64` |
| LANG-65 | task | — | closed 2026-09-21 | Three more `std/core` JavaScript functions still read an `Int` as a number |
| [LANG-66](lang-66.md) | task | — | open | What a negative `Int` exponent means for `pow` is undecided |
| [LANG-67](lang-67.md) | task | — | open | `pow`'s `bigint` branch can materialize an astronomically large intermediate before masking |
| LANG-68 | task | — | closed 2026-09-28 | An unmarked facade signature is not held to the `Task (Result Failure a)` result shape |
| LANG-69 | task | — | closed 2026-09-27 | There is no `zelkova test`: nothing runs a package's tests |
| [LANG-70](lang-70.md) | task | — | open | A constraint in an annotation is resolved, and its context reaches the canonical module |
| [LANG-71](lang-71.md) | task | — | open | A constraint context of four or more constraints does not parse |
| LANG-72 | task | — | closed 2026-09-28 | `()` is not recognised as a type, an expression or a pattern |
| LANG-73 | task | — | closed 2026-09-28 | `std/core` declares no `String`, so no annotation can name one |
| LANG-74 | task | — | closed 2026-09-28 | `std/core` declares no `Task` and no `Failure` |
| LANG-75 | task | — | closed 2026-09-29 | The manifest's `main` is read, and nothing checks what it names |
| LANG-76 | task | — | closed 2026-09-29 | A `Test` cannot hold a `Task`, so no effectful check can be a test |
| LANG-77 | task | — | closed 2026-09-30 | String literals are specified but not tokenized |
| [LANG-78](lang-78.md) | task | — | open | `std/core`'s `Task` has no `andThen` |
| [LANG-79](lang-79.md) | task | — | open | Multi-line `"""` string literals are specified but not tokenized |
| [LANG-80](lang-80.md) | task | — | open | The spec does not settle a string's unknown escape, surrogate escape or `\u{…}` digit count |
| [LANG-81](lang-81.md) | task | — | open | A `Float` or `String` literal pattern is not checked by the typer and not emitted |
| [LANG-82](lang-82.md) | task | — | open | A character literal recognises no escape sequence |
| [LANG-83](lang-83.md) | task | — | open | A derivation is not checked, and a `derived` instance has no members |
| LANG-84 | task | — | closed 2026-10-02 | A record pattern is not type checked |
| [LANG-85](lang-85.md) | task | — | open | An obligation at a record type is never discharged, so no derivation walks a record |
| [LANG-86](lang-86.md) | task | — | open | There is no `type alias` production, so a type cannot be given a second name |
| SITE-1 | task | — | closed 2026-09-11 | Publish a landing page and the rendered spec alongside the rustdoc on GitHub Pages |
| [SITE-2](site-2.md) | task | — | open | An image reference in a chapter is not rewritten, and has nowhere to land |
| [SITE-3](site-3.md) | task | — | open | A doc comment's link into `docs/` resolves nowhere, and nothing checks it |
| [GEN-1](gen-1.md) | task | — | open | Emit runnable JavaScript for a checked module |
| GEN-2 | task | — | closed 2026-09-28 | Emit the boundary predicate a facade signature promises |
| GEN-3 | task | — | closed 2026-09-21 | The typer hands back the types it solved |
| GEN-4 | task | — | closed 2026-09-21 | The backend IR |
| GEN-5 | task | — | closed 2026-09-23 | A `case` becomes a decision tree in the IR |
| [GEN-6](gen-6.md) | task | — | open | A self tail call is marked in the IR |
| GEN-7 | task | — | closed 2026-09-22 | Parameterless bindings get an initialisation order |
| GEN-8 | task | — | closed 2026-09-22 | The JavaScript runtime module |
| GEN-9 | task | — | closed 2026-09-22 | Emit a module |
| GEN-10 | task | — | closed 2026-09-25 | Emit a `case` |
| [GEN-11](gen-11.md) | task | — | open | Emit the tail-call loop |
| GEN-12 | task | — | closed 2026-09-22 | Emit an `unsafe` facade call, and place its companion |
| GEN-13 | task | — | closed 2026-09-26 | Write the build |
| GEN-14 | task | — | closed 2026-09-27 | Nothing checks that an emitted program computes the right value |
| [GEN-15](gen-15.md) | task | — | open | The WebAssembly backend |
| GEN-16 | task | — | closed 2026-09-29 | The wrapper an effectful facade's call site gets |
| GEN-17 | task | — | closed 2026-09-27 | The compiler has no command line: `src/main.rs` compiles `std/core` and takes no arguments |
| GEN-18 | task | — | closed 2026-09-27 | A build that compiles the tests writes none of them, so nothing can run one |
| GEN-19 | task | — | closed 2026-09-27 | Production and test output should be at the same folder level |
| GEN-20 | task | — | closed 2026-09-28 | Emit `()` |
| GEN-21 | task | — | closed 2026-09-29 | The JavaScript runtime cannot run a `Task` |
| GEN-22 | task | — | closed 2026-09-29 | There is no `zelkova run`: nothing runs a program's `main` |
| [GEN-23](gen-23.md) | task | — | open | An `unsafe` facade's forwarding code does not catch what its companion throws |
| [GEN-24](gen-24.md) | task | — | open | A class member, an instance and a constrained function are not emitted |
| GEN-25 | task | — | closed 2026-10-02 | A record, a field access, an update, an accessor and a record pattern are not emitted |
| [GEN-26](gen-26.md) | task | — | open | A `case` over a tuple of constructors emits code exponential in its number of branches |
| AST-1 | task | — | closed 2026-08-25 | Remove `Box<Vec<_>>` from the parser AST |
| AST-2 | task | — | closed 2026-08-26 | Unify the tuple representation across the parser and canonical ASTs |
| AST-3 | task | — | closed 2026-08-26 | Unify the typer's tuple representation with `Tuple<T>` |
| AST-4 | task | — | closed 2026-09-15 | A canonical type carries an unqualified name, so two types of one name are one type |
| PERF-1 | task | — | closed 2026-08-25 | Reduce cloning in the `Layout` iterator |
| [PERF-2](perf-2.md) | task | — | open | Every `Basics` operator backed by a parameterless binding is called through `$curry` |
| TIDY-1 | task | — | closed 2026-08-25 | Make `Name`'s inner `String` private |
| TIDY-2 | task | — | closed 2026-08-25 | Replace the tokenizer's keyword `HashMap` with a `match` |
| TIDY-3 | task | — | closed 2026-08-25 | Fix the `associativy` typo |
| TIDY-4 | task | — | closed 2026-08-25 | Test-module doc comments still describe the type checker as a stub |
| TIDY-5 | task | — | closed 2026-08-25 | Fix all outstanding `cargo clippy` warnings |
| TIDY-6 | task | — | closed 2026-08-26 | Stale doc comment on `canonical_type_to_typer_type` |
| [TIDY-7](tidy-7.md) | task | — | open | Four label/diagnostic messages in `Error::Tokenizer`'s match are still capitalized |
| [TIDY-8](tidy-8.md) | task | — | open | Two tokenizer comments describe the `Int` width as unsettled and cite a closed ticket |
| [TIDY-9](tidy-9.md) | task | — | open | `Module::from_declarations` has a `panic!` on a declaration kind its own bucketing rules out |
| [TIDY-10](tidy-10.md) | task | — | open | `Interface::arities` is a parallel map, and a miss silently reads as arity 0 |
| [TIDY-11](tidy-11.md) | task | — | open | `BUG-20` and `Js/Utils.mjs` describe a tuple encoding and a test file that no longer match the tree |
| [TIDY-12](tidy-12.md) | task | — | open | The compiler's crates are on edition 2018 |
| [TIDY-13](tidy-13.md) | task | — | open | `CompilationError::Many` is never constructed |
| TIDY-14 | task | — | closed 2026-10-02 | CI's clippy job never lints test code, and test code already fails it |
| ERR-1 | task | — | closed 2026-08-25 | Replace `panic!`/`unwrap()` with proper error handling in non-test code |
| TEST-1 | task | — | closed 2026-04-12 | Add integration tests running the full pipeline on `.zel` sources |
| TEST-2 | task | — | closed 2026-09-10 | The spec harness stops at canonicalization, so no chapter can pin a type error |
| TEST-3 | task | — | closed 2026-09-27 | CI runs neither a package's Zelkova tests nor a `.mjs` companion's checks |
| TEST-4 | task | — | closed 2026-09-11 | A facade's `.mjs` companion test lives in the compiler repo, not in the package that ships the companion |
| [TEST-5](test-5.md) | task | — | open | Two `manifest` unit tests can be handed the same temporary directory, so the suite fails intermittently |
| [TEST-6](test-6.md) | task | — | open | The `crates/zelkova/tests/cli.rs` tests that run `zelkova` on a shared fixture write one `build/` between them |
| TEST-7 | task | — | closed 2026-09-29 | `std/core`'s companion checks are run by `node --test` and not as Zelkova tests |
| [TEST-8](test-8.md) | task | — | open | CI never runs the runtime's own checks, `runtime/js/tests/zelkovaChecks.mjs` |
| [TEST-9](test-9.md) | task | — | open | A test companion's import of a companion under test that the build does not rewrite fails at run time, with a build path in the message |
| TOOL-1 | task | — | closed 2026-10-01 | No editor highlights a `.zel` file |
| TOOL-2 | task | — | closed 2026-10-01 | A source file can only be read from disk, so nothing can check an unsaved buffer |
| TOOL-3 | task | — | closed 2026-10-01 | Checking a package always prints to stderr and writes JavaScript |
| TOOL-4 | task | — | closed 2026-10-01 | A module's first syntax error is the only one reported |
| TOOL-5 | task | — | closed 2026-10-01 | The compiler, its JavaScript backend and its command line are one crate |
| [TOOL-6](tool-6.md) | task | — | open | There is no language server |
| TOOL-7 | task | — | closed 2026-10-01 | The checking pipeline names its backend, its runners and the test package |
| TOOL-8 | task | — | closed 2026-10-01 | A module that fails type checking hides itself from its importers and from the editor |
| TOOL-9 | task | — | closed 2026-10-01 | A declaration that fails canonicalization takes its whole module with it |
| TOOL-10 | task | — | closed 2026-10-01 | A failed type, operator or import is reported again by everything that names it |
| TOOL-11 | task | — | closed 2026-10-02 | A module with a syntax error is dropped from the build |
| TOOL-12 | task | — | closed 2026-10-02 | One unresolved name costs a declaration its whole typed tree |
| [TOOL-13](tool-13.md) | task | — | open | A facade that declares a type is still reported for the type it declared |
| [TOOL-14](tool-14.md) | task | — | open | A declaration a failed chunk shares a name with is spanned over everything between them |
