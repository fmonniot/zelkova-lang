# DEC-2 · The type-class mechanism: eleven decisions

**Settled:** 2026-08-29, by the language owner, in one session (`SPEC-12`).
**Status:** live, with two exceptions recorded in [what has changed
since](#what-has-changed-since) — decision 8 is superseded and decision 11 was overtaken.
**Where the rule lives:** [Type classes](../spec/type-classes.md), and for decision 6 also
[Foreign interoperability](../spec/interop.md).

Zelkova replaced the `number`/`comparable`/`appendable` spellings with type classes, and this
is the session that decided what a class is. Eleven questions were settled together; each was
normative for [the chapter](../spec/type-classes.md) that was then written and for the six
[type-class](../tickets/README.md#active-work-type-classes) tickets that implement it.

They are numbered because they are cited by number, from four ticket files and from the ticket
index. The numbering is the one given here, and it does not change: a decision that is later
superseded keeps its number and gains a note.

The chapter is the normative record and states each of these as a rule of the language. What
this file adds is the argument — for several of them, the reason is a fact about the project
or about the alternatives, which is exactly what [a chapter may not
say](../spec/conventions.md#a-chapter-says-what-the-language-is).

## 1 — A constraint is written before the type, with `=>`

```zel
min : Comparable a => a -> a -> a
lookup : (Comparable k, Eq v) => k -> v -> Bool
```

`=>` becomes its own token. It was an ordinary `Operator` and a legal user-defined infix —
probed at the time: `infix left 5 (=>) = f` compiled — so this is a breaking change, taken
with nothing in the tree relying on it.

## 2 — `class` and `instance` are hard keywords, and `where` opens the body

A class or instance body is a block of members, one per line:

```zel
class Comparable a where
  compare : a -> a -> Order
  lt : a -> a -> Bool

instance Comparable Colour where
  compare a b =
    EQ
  lt a b =
    False
```

`class` and `instance` **cannot** be soft keywords the way `javascript` is, and the reason is
structural rather than stylistic: `instance C T where …` already parses today, as a function
declaration named `instance`, so a soft spelling would misread it rather than reject it.
[`LANG-38`](../tickets/lang-38.md) carries the detail. `where` is soft in every *value*
position and hard in the *type-variable* position only — a split narrower than what was
offered, because probing found the wider version ambiguous.

## 3 — An instance lives with its class or with its type

An `instance C T` declaration is legal in the module declaring `C`, and in the module
declaring `T`'s head, and nowhere else. This makes instance coherence a property of the source
rather than of what happened to be linked.

## 4 — Classes may have superclasses, from the start

`class Eq a => Comparable a where …`. An `instance Comparable Colour` is then rejected unless
`instance Eq Colour` is in scope. Chosen over flat classes deliberately: retrofitting
superclasses changes both the surface and the solver, and `Eq`/`Comparable` is the pair
`std/core` needs on day one.

## 5 — No higher-kinded variables

Unchanged from `SPEC-11` and from [Types](../spec/types.md#type-variables): a type variable
stands for a complete type and is never applied. A class is always over a complete type.
`Functor` and `Monad` are out of reach, and that is the price of not needing a kind system.
The chapter says so plainly rather than leaving a reader to discover it by trying.

It is also what closes off the route every other language takes to deriving — a generic
representation the classes program against, which needs variables ranging over type
constructors. [DEC-1](dec-1.md#two-a-generic-representation-the-classes-program-against) is
that argument in full.

## 6 — A `module javascript` facade signature may not carry a constraint

The sharpest question `SPEC-11` left open. [Foreign interoperability](../spec/interop.md) promises a
companion `.mjs` export a **plain parameter list**, and a dictionary passed as a hidden
argument is exactly the calling convention that file is promised it will never see. So the
constraint lives one level up, in an ordinary Zelkova function, and the facade underneath it
is monomorphic:

```zel
-- Js/Utils.zel — unconstrained, and now only callable at types the JS can handle
compareInt : Int -> Int -> Int

-- Basics.zel — the constraint lives here
instance Comparable Int where
  compare a b =
    orderOf (Js.Utils.compareInt a b)
```

## 7 — Dictionaries are erased by specialisation, not passed

Codegen specialises each constrained function per instantiation; no dictionary is built or
passed at runtime. There was no ticket for this when it was decided — code generation had not
started — so it was recorded in the chapter and in `interop.md` as a constraint the first
codegen ticket inherits, and [`GEN-1`](../tickets/gen-1.md) inherits it there.

Two consequences the chapter states: whole-package compilation is assumed (there is no
separate compilation to preserve), and polymorphic recursion over a constraint would not
terminate, which decision 5 already rules out.

## 8 — A numeric literal defaults to `Int`; nothing else defaults

An otherwise-undetermined `Number` constraint resolves to `Int`. Every other constraint the
solver cannot discharge is an error naming the class and the type. No `default` declaration
form.

**Superseded.** See [what has changed since](#what-has-changed-since): a literal has a type
before any constraint exists, so there is no undetermined `Number` constraint left to default.
The second half survives — nothing defaults, and the compiler knows no class by name.

## 9 — `std/core` grows four classes

`Eq`, `Comparable` (with `Eq` as its superclass), `Number`, `Appendable`. `Eq` is in the set
because it is the one whose runtime genuinely crashes today: `_Utils_eqHelp` calls
`__Debug_crash(5)` on a function value. [`LANG-42`](../tickets/lang-42.md) is the ticket.

## 10 — `std/core` keeps `SPEC-11`'s rewrite to `a`

The 25 signatures stay spelled `a` until `LANG-42` gives them real constraints. `a -> a -> a`
is what those types *are*; a second pass over the same lines is the cost of not shipping a
signature that describes a restriction the language cannot express.

## 11 — `TEST-2` gates the chapter and nothing else

[`TEST-2`](../tickets/test-2.md) adds `expect=type-error` and `expect=type-error:Variant` to
the spec harness; it does **not** tighten `expect=ok` to mean "and type checks". It was placed
as a prerequisite of the chapter, on the reasoning that every claim a class mechanism makes is
a type-level claim, and as a prerequisite of none of the type-class tickets.

**Overtaken.** See [what has changed since](#what-has-changed-since).

## What has changed since

Two of the eleven have moved, and one has been extended. Everything else stands as written.

**Decision 8 is superseded.** [Expressions](../spec/expressions.md#a-literals-type-is-its-spelling)
settles that a literal's type is its spelling — `1` is an `Int`, `1.5` is a `Float` — which it
decides before any constraint exists rather than by defaulting one. [Type
classes](../spec/type-classes.md#numeric-literals) now says the opposite of decision 8's first
clause and the same as its second: **nothing** in the language defaults, in every case and
with no exception carved out for arithmetic. The alternative both rule out is a literal that
stands for a value in any type with a `Number` instance: that spelling puts a conversion member
on every such instance, and a call to it under every literal in every program — machinery, and
invisible work at runtime, spread across the whole language. The visible consequence is that
[`LANG-41`](../tickets/lang-41.md) left the type-class ticket dependency order — with no
obligation to discharge, it can land at any point.

**Decision 11 was overtaken.** `TEST-2` turned out not to gate the chapter either: nothing
about a class parses, so all eleven of the chapter's class-and-constraint blocks are
`expect=unimplemented`, which the harness checks perfectly well. `TEST-2` becomes load-bearing
when [`LANG-40`](../tickets/lang-40.md) lands and those blocks start wanting
`expect=type-error`. Its narrower half — that it does not tighten `expect=ok` — is unchanged.

**Decision 2 was extended, not overturned.** `SPEC-14` settled two further body forms, both
now in the chapter: an instance body may be the single word
[`derived`](../spec/type-classes.md#an-instance-may-be-derived), and a class body may carry
[`derived <member>`](../spec/type-classes.md#a-class-says-how-it-is-derived) plus three
bindings. `derived` is a soft keyword in both positions and does not join the reserved words
decision 2 names.
