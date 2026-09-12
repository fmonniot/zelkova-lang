# DEC-14 · A companion's test is a facade under `tests/`: four decisions

**Settled:** 2026-09-11, by the language owner (`TEST-4`).
**Status:** live.
**Where the rule lives:** [Testing a companion](../spec/interop.md#testing-a-companion).

`std/core/src/Js/Utils.mjs` gained the repository's first JavaScript test while `BUG-20` was
open, and that test was written in two places — beside the companion, then under the compiler's
own `tests/` tree — because nothing said where it belonged. `TEST-4` asked the question and
offered two answers. The decision is neither.

## What the question turned out to be

A facade has two tests and only one of them is a `.mjs` file.

The first is a **contract test**, written in Zelkova above the facade, asking whether
`Core.Prim.idiv` truncates. It is target-independent: the same module checks the WebAssembly
companion, which is what [one signature list serving every
target](../spec/interop.md#a-facade-names-a-boundary-not-a-backend) is worth. It needs nothing
new — a module under `tests/` may import any module of its own package, a facade included — and
this entry decides nothing about it.

The second asks what the companion does with a value the signature does not admit: `idiv 7.5 2`,
or `compare` handed an object carrying a `$` field. Those cannot be written in Zelkova at all,
because the type that would have to be passed is the one the facade refuses. Every assertion in
`Utils.test.mjs` that pins the `BUG-20` fix is of this kind. So the second test does not dissolve
into the first once code generation lands, and it needs an address.

## 1 — A companion's test is a facade under `tests/`, and its companion holds the target code

`tests/Core/PrimChecks.zel` with `PrimChecks.mjs` and `PrimChecks.wasm` beside it, checking
`src/Core/Prim.zel` and its two.

It wins on needing no new rule. [Source roots](../spec/packages.md#source-roots) introduces the
two roots and ends that paragraph with *Everything below is true of both*; the sentence
permitting a companion to sit beside its facade is below that line. A facade under `tests/`
carrying companions was legal the day the chapter was written, and this decision writes down a
consequence rather than adding a rule to either root.

It also puts location where a naming convention would otherwise go. Every `.mjs` under `tests/`
is a test companion, because that is what `tests/` is — and [DEC-11 decision
7](dec-11.md#7--a-test-is-an-exposed-value-of-type-test) already refused to find a test by how it
is spelled.

Three alternatives, in the order they lose.

**The compiler's own `tests/` tree**, which is where the file sits today, addresses it by a path
that exists because this repository happens to also be the compiler's source tree. A package that
is not the compiler has nowhere to put one at all, and `std/core` distributed on its own — once
`LANG-13` and `LANG-14` make that possible — leaves its companion's test behind.

**Beside the companion under `src/`, as `<Base>.test.mjs`** came closest, and has one real
advantage this decision gives up: the test imports `./Prim.mjs` as a sibling, where a test under
`tests/` reaches across the two roots. It loses on three counts. `*.test.mjs` is a pattern
nothing enforces. It has no answer for a `.wasm` companion's test, which is not a `.test.wasm`
but source in some third language. And it asks `src/` — [what the package *is*, and the only
thing it ships](../spec/packages.md#source-roots) — to carry files that are not part of it.

**A `tests/js/` subdirectory** is refused by a rule already written: every directory under a root
is a segment of a module name and must be spelled like one, so `js` is not a legal directory
under either root.

## 2 — A test facade declares an effect, and is never `unsafe`

No reporting protocol has to be invented, because the effectful wrapper is already one.

`Test` is not an [admitted type](../spec/interop.md#which-types-may-cross-the-boundary) and
cannot become one: it belongs to `zelkova-test` and is exposed without its constructors, so no
predicate can be written for it. A test companion therefore cannot hand a `Test` back, and
something has to carry pass and fail across the boundary instead. A raise from an effectful
companion already arrives as `Err (Threw ..)` ([DEC-12 decision
1](dec-12.md#1--a-boundary-failure-is-a-value-and-the-author-writes-the-type-that-holds-it)), and
a failed `node:assert` assertion raises. The signature is `Task (Result Failure ())`: `Ok ()` is
a pass, and the failure text rides the `Threw`.

The alternative was a payload the companion computes — `unsafe check : () -> Result String ()`,
returning the message it would have raised — which is an assertion library rebuilt on the far
side of a boundary that already carried one.

`unsafe` is worse here than redundant. A raise behind an `unsafe` signature [aborts the
program](dec-12.md#3--an-unsafe-facade-that-breaks-its-promise-aborts), so the first failed
assertion ends the run and every test after it goes unreported.

## 3 — The test companion reaches the companion under test as a module of its target

`PrimChecks.mjs` imports `../../src/Core/Prim.mjs` and calls `idiv` directly, with no boundary
between them.

This is what lets the arrangement ask the question it exists for. A test written *above* the
facade can pass only what the facade admits, since every crossing is checked against the type the
signature declares — so from Zelkova there is no way to ask what `idiv` does with a fraction. The
companion is target code and the target can call it as such.

The cost is the relative path across the two roots. It is stable, the roots being fixed by the
spec, and it is one more thing [`GEN-1`](../tickets/gen-1.md) has to leave working when it
settles how a companion is found and emitted.

## 4 — The layout is adopted before anything can run it

Nothing above runs: `tests/` is not a root the compiler knows
([`LANG-15`](../tickets/lang-15.md)), `foreign` is not a word the grammar knows
([`LANG-54`](../tickets/lang-54.md)), no wrapper is generated around an effectful call
([`GEN-1`](../tickets/gen-1.md)), and there is no `zelkova-test` and no runner.

The alternative was to place the file where decision 1's runner-up put it and move it when the
runner arrives. That buys a sibling import for a year and costs two normative rules — an interim
one and the real one — which is the arrangement `CLAUDE.md` names as the way a rule and its copy
drift apart.

So the file goes to its final path now, and the target's own runner is pointed at it directly
until a Zelkova one exists. What that costs is an orphan: a `.mjs` under `tests/` whose facade
half is not written yet.

## `Js/Utils` is the file this was decided on, and the worst example of it

`std/core/src/Js/Utils.mjs` is ported Elm kernel code that reads the value encoding directly —
`$: '#2'` on a tuple, `{ $: 0 }` for an empty list. That encoding is code generation's output,
so the file's contract is with the compiler rather than with `std/core`, and under this decision
its test becomes one of `std/core`'s: a codegen regression would surface as a standard-library
test failure.

That is a fact about `Js/Utils` rather than an argument against the rule. `Js/Basics.mjs` and
`Js/Bitwise.mjs` are ordinary companions over plain values, and they are what a third-party
facade looks like. The question `Js/Utils` really raises is whether an emitted runtime belongs in
a package's `src/` at all, and it is [`GEN-1`](../tickets/gen-1.md)'s.

## What nothing checks

All of it. No compiler pass, no test and no harness observes any of the four decisions above, and
the spec section they produced is held only to failing to parse on its modifier. Until
`LANG-15` gives `tests/` a meaning, the rule is kept by whoever places the next file.
