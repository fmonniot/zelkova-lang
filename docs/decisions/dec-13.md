# DEC-13 · A facade names a boundary, not a backend: seven decisions

**Settled:** 2026-09-09, by the language owner (`SPEC-19`).
**Status:** live.
**Where the rule lives:** [Foreign interoperability](../spec/interop.md), the whole chapter.

[JS interop](../spec/interop.md) had, since it was written, described one boundary and named it
in its own syntax: `module javascript`. Its open questions conceded that WebAssembly — the
project's eventual target, with JavaScript an intermediate one — had no equivalent declaration
and that nobody had asked what one would look like. The question turned out not to be about
WebAssembly at all.

## 1 — The modifier names no backend

`foreign`, not `javascript` and not `javascript`-plus-`webassembly`. A facade declares that a
module's bodies are elsewhere; which language they are written in is a property of the build.

The alternative that was expected to win, and did not, was **a second modifier**:
`module webassembly Wasm.Hash` beside `module javascript Js.Basics`, parallel in every respect
and cheap to add. What killed it is one level up from the header. A module written *above* a
facade would have to name a target too — `Basics` imports `Js.Basics` today, and with a second
facade module it would import one of the two and stop compiling for the other. A target-neutral
modifier is what keeps `Basics` target-neutral for nothing.

Two more were considered. **Parameterising the modifier** — `module foreign "webassembly"` —
is blocked outright: Zelkova has no [string literal syntax](../spec/lexical-structure.md#strings).
Its uppercase variant, `module foreign WebAssembly`, survives that objection and loses to the
same argument as the second modifier, plus one of its own: it advertises an open set of targets,
and the set is closed by construction, since a target the compiler cannot generate for cannot be
named by any spelling.

**Deferring the question entirely** was a real option and the ticket said so. It loses because
the cost of the `bool` this decision was supposed to price only grows, and because the deferral
would have been indistinguishable from never having asked.

## 2 — The target chooses the companion, and only the companion

One facade, one signature list, one companion per target, sitting beside it: `Core/Prim.zel`
with `Core/Prim.mjs` and `Core/Prim.wasm` — [A facade names a boundary, not a
backend](../spec/interop.md#a-facade-names-a-boundary-not-a-backend).

This is what makes decision 1 buildable, and it costs nothing in
[Packages](../spec/packages.md#source-roots), which already says that a file not ending in
`.zel` is not a module and is not read. The mechanism that was *not* available is target-scoped
source roots — `src/js/` and `src/wasm/` — because that chapter fixes the roots at two and
requires every directory under one to be spelled as a module-name segment, which a lowercase
target name is not.

## 3 — A facade whose exports differ per target is two facades

Both companions implement the whole exposing list. The alternative was a per-signature target
marker, letting one facade cover a mostly-shared boundary with a few divergent exports; it was
rejected because it makes reading a signature depend on knowing which target is being built,
which is the property decision 1 exists to buy. A facade that cannot be provided on some target
ships one companion and builds for one target — which is what a facade over a browser API is,
and it needs no marker to say so.

## 4 — The WebAssembly side binds a component, not a core module

A `.wasm` companion is a [Component
Model](https://component-model.bytecodealliance.org/design/wit.html) component, whose exported
interface is written in WIT.

A **core module** was the honest-about-the-artefact option: raw Wasm is i32, i64, f32, f64 and
linear memory, and it needs no toolchain beyond a compiler. It loses because nothing but a
number can cross a core boundary without the facade also specifying a memory layout — a string,
a list, a record and a union all become a second design far larger than this one, and one that
would duplicate what WIT already is.

Leaving the artefact kind to the backend work was the third option, and it would have left [the
admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary) unwritable, which is
the half the facade design rests on.

## 5 — A type is admitted when both boundaries can hold code to it

[DEC-6 decision 1](dec-6.md) admitted a type exactly when the compiler can emit a JavaScript
**predicate** for it. That rule is JavaScript's, and a WebAssembly boundary has no dynamic value
to inspect: what it has is a declared interface, checked once when the component loads. So the
rule becomes a conjunction — a predicate *and* a WIT spelling — and the chapter keeps one table
with a column for each.

The conjunction costs nothing today: all ten rows DEC-6 admitted have both. The alternative,
per-target admitted sets checked against their union, was rejected on that ground — it splits
the one table the chapter is built around in order to admit types nobody has asked for, and it
moves a facade's rejection from the source to whichever target happens to be built.

DEC-6's other three decisions are untouched. A bare type variable and a function type are still
rejected, and now for the same reason twice: neither has a predicate, and WIT has neither a type
variable nor a function type to spell them with.

## 6 — What varies per target is when the check happens, and a program can feel it

A predicate runs on every value on every crossing; a component's interface is checked once at
load and nothing runs per call. Both are normative and the chapter states both, because the
difference is a cost a program pays — a facade taking a list of a thousand tuples checks a
thousand tuples per call on one target and nothing per call on the other.

Stating one mechanism and calling the other an implementation detail was the tempting
simplification. It would make the chapter's own sentence about the cost of a predicate false on
half the targets it covers.

## 7 — The effect rules are the same on every target

[An effectful facade](../spec/interop.md#an-effectful-facade) and [an `unsafe`
facade](../spec/interop.md#an-unsafe-facade) are unchanged by any of the above.
[DEC-12](dec-12.md)'s survey had already found the correspondence this rests on: a component
function with a `result` type may only return that result or trap, and the trap is the Component
Model's name for what Zelkova calls [an
abort](../spec/evaluation-semantics.md#when-a-program-aborts). So `Err (Threw ..)` covers a
thrown exception, a rejected promise and a trap, and the word `unsafe` promises the same two
things about a component that it promises about an ES module.

The variant that was rejected: saying that `Err (Malformed ..)` is unreachable at a WebAssembly
boundary, since the interface was checked at load. It is a plausible reading of decision 6 and
it makes `Failure` mean subtly different things per target, which is the one thing decision 1
was chosen to avoid. The rule is uniform and any narrowing of it belongs to the backend that can
demonstrate it.
