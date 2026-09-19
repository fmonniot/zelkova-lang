# DEC-15 · The scalar types are declared in Zelkova and known by qualified name: five decisions

**Settled:** 2026-09-14, by the language owner (`SPEC-31`).
**Status:** live; decision 3's trigger re-scoped by [DEC-17](dec-17.md) decision 3.
**Where the rule lives:** [Scalar types](../spec/types.md#scalar-types), and [The default
imports](../spec/modules.md#the-default-imports) for the rule that puts them in scope
underneath `Basics`.

`std/core/src/Basics.zel` imports two facades and declares the types their signatures name.
`Js/Basics.zel:22` writes `unsafe idiv : Int -> Int -> Int` and `Js/Utils.zel:14` writes
`unsafe equal : a -> a -> Bool`, neither file carrying an `import` line, both sitting
underneath the declarations of `Int` at `Basics.zel:123` and `Bool` at `:458`. Writing the
import out is [a cycle](../spec/modules.md#imports-may-not-form-a-cycle), and the implicit one
is withheld for that same cycle: a module `Basics` depends on is exactly the case [the default
imports](../spec/modules.md#the-default-imports) drop the `Basics` entry for. So no spelling,
written or implicit, puts `Int` in either file's scope.

Both compiled anyway, because an unresolved type name was fabricated rather than reported
([`BUG-16`](../tickets/README.md)) — and the fabrication happened to be right, since the
compiler identified a type by its unqualified name, which made the invented `Int` and
`Basics`' `Int` the same type. Fixing `BUG-16` would have turned both files red, which is why
that ticket waited on this one.

`Js/Bitwise.zel` is the control. Also a facade, also naming `Int` in every signature, also
carrying no `import` line — but `Basics` does not import it, so it keeps its `Basics` entry and
resolves `Int` honestly. Being a facade is not what breaks. Being underneath the declaration
is.

## What the question turned out to be

Two questions wearing one, and each of `SPEC-31`'s four shapes answered both at once.

The first is **where a scalar type is written down**: whether `Int` is a thing the compiler has
and `Basics` describes, or a thing `Basics` declares and the compiler recognises. The second is
**how a module underneath `Basics` names one**.

Answering the first with *the compiler has it* makes the second disappear, and costs the
declaration. If `Int` is supplied to every module then `Basics`' `type Int = Int` is a second
record of the same type, and the spec owes a rule for which one a reader is looking at.
Answering it with *`Basics` declares it* keeps one record and leaves the second question
standing. The five decisions below take the second answer and then pay for it, which is the
whole of the design.

## 1 — A scalar type is known by its qualified name

The compiler knows five names: `Basics.Int`, `Basics.Float`, `Basics.Bool`, `Char.Char` and
`String.String`. For each it knows the arity — all five are nullary — and the representation
each target gives it, which is what fills [the first five rows of the admitted-type
table](../spec/interop.md#which-types-may-cross-the-boundary).

It knows this **without reading the module the name points at**, and that is what removes the
cycle rather than exempting anything from it. Resolving `Int` in `Js.Basics` asks the compiler
for a name it already holds, so nothing in `Js.Basics` needs `Basics`' interface, and the
dependency the graph rejected is not created.

Knowing the *qualified* name rather than the spelling is the load-bearing half. A module
declaring its own `Int` declares an ordinary union that shares four letters with a scalar, and
every phase treats it as one: no shadowing order, no precedence between a built-in and a
declaration, nothing for a chapter to explain. `BUG-26` was that
distinction missing — the typer read four names as its own literal types wherever they appeared
(`src/compiler/typer/mod.rs:625-642`, matching on `name.as_str()`), so a module declaring
`Bool = True | False` failed to unify with itself. This decision picks the second of the two
fixes that ticket weighs and rules the first out.

What it costs is the qualified name reaching the typer at all. `canonical::Type::Type` carries
a `Name` (`src/compiler/canonical/mod.rs:302`), so a scalar is indistinguishable from any other
nullary type by the time the typer sees it. Moving it to a `QualName` is the prerequisite for
everything here, and it is the direction `CLAUDE.md` already gives for everything after
parsing.

## 2 — A scalar type is declared in Zelkova, and an opaque one's declaration names itself

`type Int = Int` stays at `Basics.zel:123`, doc comment and all, and is the only record of the
type.

`Int`, `Float`, `Char` and `String` are **opaque**: nothing in the language constructs or
inspects a value of one. Their declarations have no body worth writing, so the rule is that
each writes its own name and contributes no constructor. `type Int = Int` is accepted and its
body discarded; `type Int = I32` is an error; `Int` is not a value.

The rule is local — it binds a declaration of a name the compiler knows, in the module that
name points at, and nothing more. In particular nothing requires the declaration to exist.
`Char` and `String` are `.ignored` files today and declare nothing, while `Char.Char` and
`String.String` are known and usable; what they lack is a place to read about them.

Keeping a declaration the compiler discards is the point of the decision, and the reason is
that the declaration is where everything a user wants is already written. Above
`Basics.zel:123` sit the valid literal syntaxes, the well-defined range and what happens
outside it on each target, and the etymology of the abbreviation with four references. A reader
asking what an `Int` is finds that by hovering the name, by following go-to-definition, or in
the generated documentation beside every other type in `Basics`. A compiler-supplied name has
none of those unless the compiler also ships the prose, and prose shipped by the compiler is
the second record this decision exists to avoid.

## 3 — A module that loses the `Basics` default import receives the scalar type names in its place

The hole and the patch are the same set of modules. [The default
imports](../spec/modules.md#the-default-imports) drop an entry for a module that already
depends on the module the entry names; a module that drops `Basics` on that rule instead
receives `Int`, `Float`, `Bool`, `Char` and `String`, bound to the five names decision 1
holds.

They are bindings to those names, not new types. `Js.Basics`' `Int` *is* `Basics.Int` — the
same type, by construction rather than by the coincidence of unqualified identity that made
the fabricated one work then. The rule is the import that cannot be written, written by the
compiler out of what it already knows.

It scopes itself. A module underneath `Basics` can only be a module of the package that
declares `Basics`, since a package's dependencies run one way, so the rule reaches `std/core`
and nothing else without naming `std/core`. Two modules receive it as the tree stands:
`Js.Basics` and `Js.Utils`.

**Re-scoped:** [DEC-17](dec-17.md) removed the drop this decision triggers on — no module of
`zelkova-core` receives the default imports at all — so the trigger is now membership of that
package rather than the loss of the `Basics` entry. The same modules are reached, for the
reason this paragraph gives.

It also costs a language user nothing to know. The model stays *`Int` is declared in `Basics`,
and `Basics` is imported by default* — one rule, the one already learned, with the same
answer for `Int` as for `Maybe` and `identity`.

## 4 — The scalar names arrive without their values

Type names only: no constructors, no functions. A module that loses `Basics` can write `Bool`
in a signature and cannot write `True`.

That is the whole of what the case needs, because a facade has no body. Extending it is not
free: the moment `True` and `False` come along, the list stops being *the names the compiler
knows* and becomes a curated slice of `Basics`, and the argument for those two values over
`not`, `+` or `identity` has to be made and then maintained.

It leaves a real limitation, which no module has hit yet. A hand-written module underneath
`Basics` could annotate a `Bool` and be unable to produce one. That module has a layering
problem — it wants `Basics`' values from below `Basics` — and the limitation is where the
problem surfaces.

## 5 — `Bool` is a scalar, and an ordinary union, and both at once

`type Bool = True | False` at `Basics.zel:458` is the genuine definition, and decision 2's
self-naming rule does not touch it. `True` and `False` are constructors, matched and written
like any others, which is what [Lexical
structure](../spec/lexical-structure.md#reserved-words) means by `true` and `false` not being
reserved.

What the compiler knows about `Bool` is only its representation: a JavaScript boolean, WIT's
`bool`, and the [admitted-type](../spec/interop.md#which-types-may-cross-the-boundary) row that
follows. It knows nothing about the type's structure, so `Bool` is on decision 1's list and
outside decision 2's, and the two lists differ by exactly this name.

An earlier draft put `Bool` in one list with the other four and produced a type with a name and
no values — annotatable everywhere, writable only where `Basics` was in scope. That is the
sharpest evidence available that `Bool` does not belong beside `Int`, and splitting the lists
is what this decision does with it.

## What it was chosen over

Four shapes, three of them `SPEC-31`'s and the first being what an implementation of its
leading candidate turned out to look like.

**Supplying the scalar names to every module.** `SPEC-31`'s first shape, implemented on a draft
branch: the five names seeded into every scope before imports are processed, so anything
imported or declared overwrites them. It works, and it is where the cost of answering both
questions at once lands. `Basics` still declares `Int`, so the name reaches an ordinary module
twice; the two coincide only while a type is identified by its unqualified name; and the spec
gains a shadowing order a user can observe and therefore has to be taught. The draft filed a
follow-up to delete the declarations from `Basics` and dissolve the duplicate, which trades the
second record for no record — the documentation at `Basics.zel:100-123` has nowhere else to go.
Decisions 1 and 2 answer the two questions separately instead, and the duplicate never forms.

**Exempting a `module foreign` facade from the cycle rule.** `SPEC-31` costed this as the
smallest change and it is not one. The cycle is real at the interface level and not an artefact
of the diagnostic: `Basics` needs `Js.Basics`' interface to canonicalize `not = Js.Basics.not`,
and `Js.Basics` needs `Basics`' to resolve `Int`. Dropping the rejection leaves the ordering,
and making the ordering work means splitting every module's check into a types pass and a
values pass. Decision 1 removes the second half of that knot rather than untying it — after it,
`Js.Basics` needs nothing from `Basics` at all.

**A `Prim` module that imports nothing**, declaring the scalars, re-exported by `Basics` and
imported directly by the facades. No cycle and no new compiler concept, which is its
attraction. It loses on decision 2: the declaration moves out of the module a reader looks
`Int` up in, into a module that exists for the compiler's benefit and that a chapter then has
to explain or hide. It also leaves the typer's spelling match untouched, so `BUG-26` survives
it.

**Stopping `Basics` importing the facades.** This does not remove the cycle. Whatever module
takes over importing `Js.Basics` is still reachable from `Basics`, so the loop closes one
module further out. The cycle is between a facade and wherever the scalar types are declared,
and moving the import moves the cycle.

## What nothing checks

All of it, when this was written. No pass implemented any of the five decisions then: the
typer matched four scalars by spelling, `canonical::Type::Type` carried no qualified name,
nothing recognised a self-naming declaration, and the two facades resolved `Int` through the
fabrication [`BUG-16`](../tickets/README.md) described. The tickets `SPEC-31` left behind are
what closed each of those, in the order they name.
