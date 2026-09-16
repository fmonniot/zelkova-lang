# DEC-17 · The default imports are decided by package, not by the import graph: three decisions

**Settled:** 2026-09-15, by the language owner (`SPEC-33`).
**Status:** live.
**Where the rule lives:** [The default imports](../spec/modules.md#the-default-imports) for
which modules receive the list, and [Scalar types](../spec/types.md#scalar-types) for the five
names that arrive in its place.

Something has to give when an import the compiler supplies would close a cycle. `Basics` cannot
import `Basics`; `Maybe` and `Result` would import each other; a facade `Basics` is built from
cannot import `Basics` back. The mechanism that landed with `LANG-8` gave way one entry at a
time: a module dropped the entry for a module that already depended on it, tested against the
import graph as built so far, and kept the rest.

That is a defensible rule and it was implemented and documented correctly. It is also a rule
with no local answer. Which entries a module gets depends on the whole package's dependency
graph and on the order the list itself is written in, a drop propagates through the implicit
edges already allocated, and stating it precisely took three paragraphs of
[Modules](../spec/modules.md) including a two-hop example and a tie-break. Probing `std/core`
showed three facades with three different sets:

```
Tuple      -> []                                    Basics -> []
Js.Utils   -> ["Tuple"]                             Maybe  -> []
Js.Basics  -> ["Tuple"]                             Result -> []
Js.Bitwise -> ["Basics", "Maybe", "Result", "Tuple"]
Bitwise    -> ["Maybe", "Result", "Tuple"]
```

`Js.Bitwise` gets `Basics` and the other two facades do not, for no reason either file shows.

## What the question turned out to be

Not *how should the drop be computed*, but *what is the drop a property of*. The per-entry rule
makes it a property of a module's position in the import graph. Every instance of it is in one
package:

- The import graph is built per package — `ModuleWalker::new` drops imports of modules outside
  the one being compiled — so in a package that does not contain `Basics`, the pass that
  allocates implicit edges finds no node to point at and adds nothing. No edge is ever refused.
- `zelkova-core` is [a dependency of every
  package](../spec/packages.md#zelkova-core-is-a-dependency-of-every-package) and dependencies
  run one way, so no module outside it can be one the eight depend on.
- [No other package may declare a module under one of their
  names](../spec/packages.md#two-modules-under-one-name-is-an-error), so no other package can
  manufacture the situation either.

So the rule was general and its instances were four modules of one package. The three decisions
below make the scope the rule.

## 1 — The exception is `zelkova-core`, and it is all-or-nothing

No module of the package that declares the eight receives any of them; every module of every
other package receives all eight.

What this buys is a question with a local answer. *Which defaults do I get* stops depending on
what `Basics` happens to import, on what the modules beside you import, and on the order
`DEFAULT_IMPORTS` is written in — a reader of `Js/Utils.zel` no longer has to read `Basics.zel`
to know what is in scope. For everyone outside core, which is every language user, the answer
is *all eight, always*, and two modules of one package can no longer differ.

It costs the language a named special case. That is a real cost and it is the one being
accepted: `zelkova-core` is where the language's primitives are declared, so it is already
special in [Packages](../spec/packages.md#zelkova-core-is-a-dependency-of-every-package) and in
[Scalar types](../spec/types.md#scalar-types), and the exception being *about* that package is
more honest than an exception that is about the import graph and only ever fires there.

## 2 — Core's modules write every import they use

The corollary, and the thing that makes decision 1 affordable. A module that receives nothing
has to say what it needs.

This costs `std/core` nothing, because it is already how `std/core` is written. `Basics.zel`,
`Maybe.zel`, `Result.zel` and `Bitwise.zel` write their imports out. So does every `.ignored`
port waiting to be turned on — `Dict`, `List`, `String`, `Set`, `Array`, `Task` all open with
`import Basics exposing (..)` and `import Maybe exposing (..)`, because Elm's core is written
that way too.

And nothing loses an import it was using. Of the seven modules that receive an entry today,
none names one: `Bitwise` receives `Maybe`, `Result` and `Tuple` and writes `import Basics
exposing (Int)` itself; the three `Js.*` facades receive `Tuple` and name only scalar types.
The expressiveness the per-entry rule bought is expressiveness nothing spends.

## 3 — The five scalar names go to every module of `zelkova-core`

[`DEC-15` decision 3](dec-15.md#3--a-module-that-loses-the-basics-default-import-receives-the-scalar-type-names-in-its-place)
gave `Int`, `Float`, `Char`, `String` and `Bool` to a module that *lost the `Basics` entry*.
There is no such module under decision 1, so the trigger is re-scoped to the package: every
module of core has the five names, whether or not it imports `Basics`.

The set of modules this reaches is the same one — core is exactly where a module could lose
`Basics` — so this re-words the trigger rather than widening the rule. Giving them to a module
that also imports `Basics` changes nothing, because a seeded name and an imported one are
bindings to the same declaration, and `DEC-15` decision 1 is what makes that true by
construction.

Decision 4 of that entry is untouched: type names only, no constructors and no values, so a
module reaching `Bool` this way still cannot write a `True`.

## What it was chosen over

**Keeping the per-entry fixed point.** It is strictly more expressive — a module keeps the
entries that do not conflict — and the expressiveness has no user, as decision 2 shows. Against
that stands three paragraphs of chapter, a propagation rule, a tie-break settled by the order of
a list the reader has to consult, and about thirty-five lines of doc comment in
`add_default_import_edges` arguing for a loop nesting whose purpose is to make the collisions
deterministic. Collisions that cannot happen do not need a deterministic outcome.

**The coarser graph rule:** a module that any default import depends on receives none of the
list. One sentence, and it was the alternative `SPEC-33` opened with. It is still a question
about the import graph, which means it is still answered by reading `Basics.zel` rather than by
knowing which package you are in, and it still lets two modules of one package differ. It buys
nothing decision 1 does not, and keeps the dependence decision 1 removes.

**A declared exempt list**, carried by the compiler: these modules, plus these dependencies of
theirs, get nothing. Cheap to explain and it goes stale the first time `Basics`' imports change
— the list would have to be edited by whoever adds a facade under `Basics`, and the failure
mode when they forget is a module silently receiving an import that closes a cycle. Decision 1
is scoped by package membership, which nothing has to remember to update.

## What nothing checks

The rule is unobservable outside `zelkova-core`, which is what makes it a good rule and also
means no test of an ordinary package can pin it: a package that does not contain `Basics`
behaved identically under all three options. What the tests can hold is core's own sets, and
that is what [`LANG-57`](../tickets/lang-57.md) asks for.

Until that ticket lands the compiler still decides entry by entry, which the chapter carries as
a **Known gap:**. Nothing in `std/core` names an entry it receives that way, so the two rules
accept exactly the same tree today — the divergence is in what the compiler would do to a
module nobody has written yet.
