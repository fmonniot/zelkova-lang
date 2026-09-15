# DEC-15 · The scalar type names belong to the compiler, not to a module

**Settled:** 2026-09-14, by the language owner (`SPEC-31`).
**Status:** live.
**Where the rule lives:** [Types — Built-in type names](../spec/types.md#built-in-type-names),
pointed at from [The default imports](../spec/modules.md#the-default-imports) and
[Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary).

`Basics` is built from two [facades](../spec/interop.md), and their signatures name types they
have no way to import. `unsafe idiv : Int -> Int -> Int` in `Js/Basics.zel` and `unsafe equal :
a -> a -> Bool` in `Js/Utils.zel` are underneath the module that declared `Int` and `Bool`, so
writing the import out is [a cycle](../spec/modules.md#imports-may-not-form-a-cycle) and the
implicit one is dropped for the same reason. Both modules compiled anyway, because an
unresolved type name was invented rather than reported (`BUG-16`) — and the invention happened
to be right, since the compiler identifies a type by its unqualified name and the fabricated
`Int` was therefore `Basics`' `Int`.

`Js/Bitwise.zel` is the control. It is a facade naming `Int` in every signature, `Basics` does
not import it, and it therefore receives `Basics` as [a default
import](../spec/modules.md#the-default-imports) like any other module and resolves `Int`
honestly. The problem is not facades; it is being underneath the declaration.

## What the cycle is actually between

Four shapes were weighed. Three of them share one defect, which is easiest to see in the one
that looks furthest from the others: **stop `Basics` importing the facades**, and route its
`unsafe` bindings through some new module above them. The cycle survives it. Whatever module
ends up importing `Js.Basics` is still reachable from `Basics`, and `Js.Basics` still needs
`Int`, so the loop closes one module further out. The cycle is between a facade and *wherever
the primitive types are declared* — moving the other end moves nothing.

**Exempting a facade from the cycle rule** fails for the neighbouring reason. A facade has no
body, so the exemption reads as harmless, but `dependencies` uses the import graph for check
*order*: `Basics` needs `Js.Basics`'s interface to canonicalize `not = Js.Basics.not`, and
`Js.Basics` needs `Basics`'s to resolve `Int`. Neither can be checked first. The exemption
removes the diagnostic and leaves the ordering, and making the ordering work means splitting
every module's check into a types pass and a values pass — the largest of the four shapes
rather than, as it first appeared, the smallest.

**A `Prim` module that imports nothing**, declaring the primitives for `Basics` and the facades
both, is the one shape that works as written. It costs more than it looks. A type a module
declares reaches its dependents through `Interface::unions`, which `canonical::Module::to_interface`
builds from that module's *own* declarations — so `Basics` cannot re-export what `Prim`
declares, and `Prim` has to join the default import list to stay invisible. That is a ninth
entry, in every program's namespace, for a module that exists for the compiler's benefit.

And it answers the question everywhere except where it was asked. `zelkova-core` [is a
dependency of every package](../spec/packages.md#zelkova-core-is-a-dependency-of-every-package),
so a facade in *someone else's* package can be told that `Int` comes from a module — while the
facades that made the question urgent are the ones that package cannot reach. A rule with an
exception at the place it matters most is the wrong rule.

## 1 — The five scalar type names are supplied by the compiler

[Built-in type names](../spec/types.md#built-in-type-names). `Int`, `Float`, `Bool`, `Char` and
`String` resolve in every module with nothing written at the top of the file, and no module has
to be present for them to.

The argument against was that the compiler should know as little as possible, and
[`LANG-41`](../tickets/lang-41.md) — which retires `Type::Number` — runs that way. It does not
survive contact with how much the compiler already knows about these five names *without
consulting any declaration*: the type checker maps four of them to its own literal types by
spelling, [a literal's type is its
spelling](../spec/expressions.md#a-literals-type-is-its-spelling) with nothing a module writes
entering into it,
[the admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary) gives each a
JavaScript predicate and a WIT spelling, and code generation will emit `s32` for one of them
without asking a module's permission. A type the compiler must know by name *and* a module must
declare is two records of one thing, and [`BUG-26`](../tickets/bug-26.md) — where a module
declaring `Bool` cannot annotate anything with it, and the error reads *cannot match `Bool` with
`Bool`* — is what that costs.

`type Int = Int` was the other half of the argument. It is a nullary constructor standing in for
a machine integer, and `std/core/src/Basics.zel` says so beside it: `NOTE: The compiler provides
the real implementation.`

## 2 — The list is the scalars a facade signature may name

Exactly the five scalar rows of [the admitted-type
table](../spec/interop.md#which-types-may-cross-the-boundary), and nothing else. Every other
admitted form is either syntax — a tuple, a record, a list — or a union some module declares and
any module can import. A facade is where the language meets its target, so the types crossing
that boundary are the ones that cannot be made to depend on a module being there.

That is a different test from *has no declaration a module could write*, which would take `Int`,
`Float`, `Char` and `String` and leave `Bool` out: `Bool` is an ordinary union with two ordinary
constructors. It is on the list because a facade names it.

The compiler supplies the five **names** and no values. `True` and `False` still come from
`Basics`, so a module reaching neither can annotate a `Bool` and cannot write one. A facade has
no bodies, so the case this was decided for does not need them; decision 4 is where the rest of
it sits.

## 3 — A built-in name is the weakest entry in a scope

A module declaring a type of one of those names, or importing one, gets that one. The seeded
names go into the scope before the imports are processed, so anything else overwrites them.

The alternative was to reserve the five outright, which is what "the compiler owns them" most
naturally suggests, and it loses three ways. It would make `Bool` a reserved name, contradicting
[the rule that it is an ordinary union type](../spec/lexical-structure.md#reserved-words). It
would settle [`BUG-26`](../tickets/bug-26.md) by making its example illegal rather than by
fixing the type checker — deciding another chapter's rule as a side effect of this one. And the
cost lands on every program: a package modelling ASCII may call its string type `String`, and
nothing about a facade's scope requires taking that away.

Shadowing also keeps the mechanism honest about what it is. These are names in a scope, not
tokens; the [thirteen reserved words](../spec/lexical-structure.md#reserved-words) are a lexical
rule and this is not one.

## 4 — `Basics` keeps its three declarations for now

`Int`, `Float` and `Bool` stay declared and exposed in `std/core/src/Basics.zel`, so those names
reach an ordinary module both from the compiler and through the default import. Nothing breaks,
because the compiler identifies a type by its unqualified name and the two are therefore the
same type.

Deleting them is a two-line change and was deliberately not taken with this one. The
declarations carry the documentation a reader looks `Int` up to find, and a declaration is what
a documentation generator would attach it to; removing them before there is somewhere for that
prose to live trades a duplicate record for a lost one.
[`SPEC-34`](../tickets/spec-34.md) is where it is settled, and it has a shape — the compiler
recognising a declaration of a built-in name as documentation for it — that keeps one record
without losing the home.

## What nothing checks

A built-in name and a module's own declaration of the same name are indistinguishable downstream:
`canonical::Type::Type` carries an unqualified `Name`, so decision 3's shadowing is observable in
an arity check and nowhere else. The day type identity carries the module a name came from, both
this and decision 4 need revisiting.

`BUG-16` still fabricates a type for any unresolved name, so the rule that these five resolve is
not yet distinguishable from every name resolving. It is unblocked by this entry: with the names
seeded, nothing in `std/core` or in `docs/spec/` fabricates a primitive, and what remains is a
`type` declaration naming its own siblings — which is `BUG-16`'s own second item.
