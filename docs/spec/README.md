# Zelkova — Language specification

This is the specification of Zelkova the language, as distinct from `docs/tickets/`
(the compiler's own work log) and `cargo doc` (the compiler's Rust API). One markdown
file per chapter, sibling files in this directory, plus the non-normative
[appendices](#appendices) at the foot of this index. A third sibling,
[Conventions](conventions.md), is about this document rather than about Zelkova: the rules a
chapter is written against, and the reasons they are what they are.

**The spec is normative.** It describes Zelkova as designed, including constructs the
compiler does not implement yet — it is not a description of today's binary. Where a
chapter documents something unimplemented, it says so and marks its prose accordingly
(see the lead-ins below). `CLAUDE.md`'s *Language notes* section has the
compiler's current implemented/not-implemented split; this directory is where that
split gets explained rather than just listed.

**The spec is self-contained.** Zelkova began as a fork of Elm's surface syntax and owes
it most of its good ideas, but Elm is an *inspiration*, not a reference: no chapter here
may resolve a question by pointing at Elm's documentation. Where a rule is inherited it
is written out in full; where the two languages differ, this directory is the answer.

That rule is not fastidiousness. Deferring works only for as long as the two languages
agree, so it fails precisely when a divergence is wanted — at which point the spec has to
answer a question in vocabulary that lives in someone else's document, describing someone
else's language, and changing without notice. It also frames every deliberate divergence
as a defect. Writing the rules down here is what makes "we do this differently, and here
is why" a sentence this directory can hold (`SPEC-2`).

## Every example is checked

Every fenced ```` ```zel ```` block in a chapter carries an `expect=` tag in its info
string, and `cargo test --test spec` runs every one of them through the compiler. A
chapter claim the compiler disagrees with is a red test, not a stale sentence nobody
notices — which is the failure mode this directory exists to prevent: documentation
nothing checks drifts from the code it describes, silently and indefinitely.

A tag says what the compiler is expected to do with the block: compile it, reject it in the
parser, reject it in canonicalization, or fail because the construct does not exist yet. An
untagged block is a hard test failure rather than a silent skip.
[Conventions](conventions.md#the-expect-vocabulary) has the full vocabulary, along with the
`package=` label that lets one chapter compile two modules together.

## Reading a chapter

Three bolded lead-ins appear in chapter and appendix prose. Each marks a sentence that is
about the *compiler* rather than about the language, which is the one kind of "not yet" the
spec admits:

| Lead-in | Marks |
|---|---|
| **Known gap:** | Behaviour the compiler has today and the language says it should not. The rule around it is Zelkova; the example beside it is the binary. |
| **Not implemented:** | A rule of the language the compiler does not have yet. Its examples are tagged `expect=unimplemented` and fail on purpose. |
| **Provisional:** | A mechanism nothing has built and nothing has settled — to argue with rather than to build against. |

The spellings are fixed so that each is greppable: `grep -r "Known gap:" docs/spec/` finds
every one.

A question the language has not answered is an **Open question** at the foot of the chapter
that would own the answer. It is a gap in the language rather than in the writing, and it
stays where the rules around it are.

## Chapters

The chapters are the language, in reading order. Each one states its rules in full rather
than deferring them, so together they are meant to be the whole of Zelkova and not a set of
footnotes to somebody else's manual — that is what *the spec is self-contained* costs, and it
is the point.

| Chapter | Covers |
|---|---|
| [Lexical structure](lexical-structure.md) | What the characters of a file mean before any structure is imposed on them: source text, comments, identifiers, reserved words, literals, operators and punctuation. |
| [Layout (the offside rule)](layout.md) | How a line's leading whitespace and starting column decide which construct it belongs to — for the file, a `case … of`, and a `let … in`. |
| [Modules, exposing and imports](modules.md) | The module header, the four forms an `exposing` entry can take, `import … as … exposing`, the default imports, and how a module name maps to a file path. |
| [Declarations](declarations.md) | The five declaration forms, and in full the two made of neither a type nor a module name: bindings, including multi-clause functions, and `infix`. |
| [Types and type annotations](types.md) | Type expressions in full — application, the function arrow, variables, tuples, the unit type — plus the two declarations made of them, `name : Type` and `type`. |
| [Expressions](expressions.md) | Every expression form: literals, names, application, grouping, `if … then … else`, `case … of`, `let … in`, lambdas, and the operator table. |
| [Patterns](patterns.md) | Every pattern form, where each may appear, how they nest, and which of them can fail. |
| [Name resolution and scoping](name-resolution.md) | The five namespaces, the scopes a name is looked up in, what shadows what, and what makes a reference ambiguous rather than merely unresolved. |
| [Evaluation semantics](evaluation-semantics.md) | Strictness, purity, order of evaluation, which forms evaluate their subexpressions conditionally, what `==` means structurally, and what a function value is. |
| [JS interop](js-interop.md) | The `module javascript` facade: what such a signature may say, what its companion `.mjs` exports, and why it is the only way into JavaScript. |
| [Packages and source layout](packages.md) | The package directory, the `zelkova.toml` manifest, the two source roots, dependencies, and what a package boundary means for visibility and for a module's name. |
| [Type classes](type-classes.md) | The `class` and `instance` declarations, how a constraint is written in an annotation, superclasses, where an instance may be declared, and the words this reserves. |

Two constructs are named by the chapters above and specified by none of them: **records**
and **lists**. Both are part of the language, both have chapters' worth of design left to
settle, and a cross-reference to either currently arrives nowhere —
[`SPEC-21`](../tickets/spec-21.md) and [`SPEC-22`](../tickets/spec-22.md) are the tickets, and
each ends with a row added to the table above.

## Appendices

An appendix is a sibling file in this directory that is deliberately **not** part of the
language. It describes what surrounds Zelkova rather than what Zelkova is, and nothing in
one is normative or checked. There is one:

| Appendix | Covers |
|---|---|
| [The toolchain](toolchain.md) | Fetching a dependency, resolution and `zelkova.lock`, the cache, vendoring and offline builds, publishing, running a package's tests, and the compiler's interface. |

An appendix exists because a language decision leaves a toolchain question with one sensible
answer, and leaving it unwritten means every reader invents that answer privately and slightly
differently. `docs/spec/packages.md` settles what a dependency entry *means*; something still
has to say what happens when one is fetched, and that is not a sentence about the language.
[Conventions](conventions.md#chapter-or-appendix) has the test for which of the two a rule
belongs to.
