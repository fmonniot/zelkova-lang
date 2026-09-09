# DEC-12 · What a broken companion does: a survey and six decisions

**Settled:** 2026-09-08, by the language owner (`SPEC-15`).
**Status:** live.
**Where the rule lives:** [JS interop — An effectful facade](../spec/js-interop.md#an-effectful-facade)
and [Which types may cross](../spec/js-interop.md#which-types-may-cross-the-boundary),
[Evaluation semantics — An effect that can fail](../spec/evaluation-semantics.md#an-effect-that-can-fail)
and [When a program aborts](../spec/evaluation-semantics.md#when-a-program-aborts).

[DEC-11](dec-11.md) settled what an effect is and left the failure story as a contract: *a
companion may not throw*, kept the way [purity](../spec/evaluation-semantics.md#purity-and-the-javascript-boundary)
is kept, by the person who wrote the `.mjs`. The two are not alike, and the difference is what
reopened the question. A companion that lies about purity has no runtime consequence — the
program keeps running and *no meaning the language defines* is a complete answer, because nothing
observes the gap. A companion that throws has one at a definite point: the generated wrapper must
have code there. Whatever that code is becomes the language's behaviour, chosen by whoever writes
the backend rather than by the specification.

The goal this entry was decided against: **a Zelkova program cannot fail unexpectedly.** Not that
failure is rare or well-reported — that a program which type checks has no way to stop other
than by finishing.

## The survey

Four strategies exist, and every language with a foreign function interface uses one of them.

**Type the failure and admit the typing is partial.** ZIO is the most refined version and the
most useful data point, because it is the design that tried hardest. `ZIO[R, E, A]` carries a
typed error channel, and the documentation is direct that this does not buy the goal above:
*typed errors don't guarantee the absence of defects and interruptions*. A `ZIO[Any, String, Int]`
can still die of a `NumberFormatException`. So ZIO splits failure in two — `Fail[E]` for what
was declared, `Die(Throwable)` for what was not — and makes the undeclared half a first-class
value that `Cause` describes and `sandbox` exposes. cats-effect does not split it at all:
`IO[A]` is `MonadError[IO, Throwable]`, one untyped channel, always present. Two competing
industrial effect systems, a decade each, and both concluded the untyped channel cannot be
removed and should be made visible.

**Type the failure as an effect and get a real guarantee.** Koka has what the goal above
describes. `exn` is an effect like any other in a row-polymorphic effect type, and an expression
that types without `exn` will never throw an unhandled exception. This is the genuine article and
it is the only entry in the survey that is. The price is an effect-row system in the type
checker — a different axis from the higher-kinded variables [a class variable is
not](../spec/type-classes.md#a-class-is-always-over-a-complete-type), so not automatically
foreclosed, but a larger change than anything Zelkova has taken.

**Remove the permission to write foreign code.** Elm's real mechanism is social: only the
compiler's author writes kernel code, so the throwing surface is small and audited by one person.
Roc made the same idea structural — effects come from a **platform**, a separately authored
artifact that decides how each one is implemented, and an application author cannot write host
code at all. This is the only strategy that shrinks the throwing surface rather than describing
it. Roc still ships a `crash` keyword.

**Force the shape at the boundary and trap on violation.** The WebAssembly Component Model is the
machine-checked version, and it is Zelkova's eventual target. Its rule: a core function can
return, throw, suspend or trap, while a component function with a `result` type may only return
that result or trap — every unhandled event reaching a component boundary becomes a trap. They
forced `result` into the interface language and could not eliminate the trap.

## What the survey settles

**Nobody has the goal while allowing user-written foreign code.** The two designs that come
closest get there by taking the permission away, and both keep a named abort anyway.

Which reframes the question. The choice is not *third outcome or no third outcome* — every
system above has one. It is **named or unnamed**. Roc calls it `crash`, WebAssembly calls it
`trap`, ZIO calls it `Die` and lets a program catch it. A specification that says nothing has
one too; it just has not said where.

## 1 — A boundary failure is a value, and the author writes the type that holds it

[An effectful facade](../spec/js-interop.md#an-effectful-facade). A facade's result type must be
`Task (Result Failure a)`; any other `Task` is an error, and `a` is the only part its author
chooses. The `Result` is still built by the compiler's wrapper — it catches what the companion
throws and runs the predicate over what it returns — so a signature declares a `Result` no
companion ever produces.

Two alternatives, both real. **Leaving it a contract** and defining what a violation does —
`DEC-11`'s position, made explicit — was rejected on the goal above: an unenforceable rule whose
violation stops the program is the thing being designed away.

**Elaborating the type instead of requiring it** was the near miss, and it was the draft this
entry was first written around. A facade would declare `read : String -> Task String` and its
callers would receive `Task (Result Failure String)`, the compiler supplying the `Result` the way
it supplies the wrapper. It is less to write, and the six tokens it saves are identical on every
effectful facade that will ever exist.

It loses on diagnostics. A declared type that is not the type makes every message naming that
value print something the source does not contain, and a reader who goes to the facade to check
finds a different signature there. That cost is not confined the way the reading cost is: a
facade is [never importable outside its package](../spec/packages.md#what-a-package-exposes), so
the *surprise* stops at the wrapper module, but a type error travels to wherever the value is
misused. Requiring the annotation also keeps a facade's signature resolving exactly as any other
annotation does, which is one fewer place the compiler holds two types for one name.

The cost is ceremony: an effect that cannot fail still declares `Result Failure`, and an author
writes the same wrapper on every effectful facade.

## 2 — `Failure` has two constructors and belongs to `Task`

`Threw String` and `Malformed String`
([An effect that can fail](../spec/evaluation-semantics.md#an-effect-that-can-fail)). One
constructor would do, and two are kept because the repairs differ: `Threw` is JavaScript doing
something the author did not plan for, `Malformed` is a `.mjs` that disagrees with the signature
above it. The second is always a bug in hand-written JavaScript and never a condition a caller
should be handling.

This also answers [`GEN-2`](../tickets/gen-2.md)'s first open question, which asked what a failing
predicate does and recorded a thrown JavaScript error as *the only one available while the
language has no error type of its own*. It now has one.

`Failure` does **not** join [the default imports](../spec/modules.md#the-default-imports)
alongside the `Task` [DEC-11](dec-11.md#the-name-and-the-eighth-default-import) put there, though
decision 1 makes every effectful facade name it. That list is for what every program writes, and
`main`'s annotation is why `Task` is on it; a package declaring no effect of its own never
spells `Failure` at all. The constructors decide it: `Threw` and `Malformed` are ordinary enough
words that putting them in every module's namespace costs more than the import line it saves.

## 3 — A pure facade that breaks its contract aborts

[When a program aborts](../spec/evaluation-semantics.md#when-a-program-aborts). The required
result type in decision 1 is available to an effectful facade because it has somewhere to put an
outcome. A pure one does not: a `Result` in the result of `and : Int -> Int -> Int` describes a
different function, and most of the language's arithmetic is written as a facade.

So the boundary is treated asymmetrically on purpose. An effect is a description of work the
runtime performs, and a failure to perform it is part of the description; a pure facade claims to
be a function, and a function that cannot produce its result has broken a claim rather than
produced a value.

## 4 — A program can abort, and the chapter names the three causes

A broken pure facade, exhaustion of memory or stack, and the host stopping the program. Naming
them costs the unconditional headline in [Two
outcomes](../spec/evaluation-semantics.md#two-outcomes) and buys the claim being true. The header
survives unchanged because the claim under it is about evaluating an expression, and an abort is
an outcome of running a program: a program has a boundary and an expression does not.

The rejected alternative was leaving a violation undefined, which is what a specification does by
saying nothing. It was rejected before the survey ran and the survey confirmed it: the abort
exists whether or not it is written down, and an unwritten one is decided by the backend.

## 5 — There is no `crash`

Roc has one and Zelkova declines it. Every cause in decision 4 is the runtime reporting that it
cannot continue, and none is a step a program takes; a keyword would add a way for a program to
stop that has nothing to do with the boundary this entry is about. Nothing catches an abort
either, for the same reason — a program that could catch one would be handling a condition the
language has just finished promising it cannot be in.

## 6 — Who may declare an effect stays open

Elm's and Roc's strategy is the only one in the survey that shrinks the throwing surface, and it
is rejected outright: keeping the boundary open to every package is an explicit goal of the
language, and [DEC-11 decision 2](dec-11.md#2--a-primitive-effect-is-a-facade-whose-result-type-is-task-a)
is the form it takes. A package declares an effect on the terms `zelkova-core` declares one.

Decision 1 is what makes that affordable. The reason Elm can leave the contract unenforced is
that one person writes the code the contract binds; Zelkova has deliberately given that up, so it
buys the same safety by construction instead.

## What nothing checks

None of it is implemented. `zelkova-core` declares no `Task` and therefore no `Failure`, no
wrapper is generated, no predicate is emitted, and nothing runs a program, so no abort can occur
([`GEN-1`](../tickets/gen-1.md), [`GEN-2`](../tickets/gen-2.md)). `Failure`'s two constructors
carry a `String`, which has [no literal syntax](../spec/lexical-structure.md#strings) yet.

Decision 1 is unwritable before [`LANG-9`](../tickets/lang-9.md) lands: a type argument must be a
bare name today, so `Task (Result Failure String)` is a syntax error and every block in the two
chapters that shows one is tagged `expect=unimplemented` for that reason. The shape check itself
is [`LANG-43`](../tickets/lang-43.md)'s and inherits the same sequencing — no fixture for it can
be written until the form parses.

## Sources

- [ZIO: Typed Errors Guarantees](https://zio.dev/reference/error-management/typed-errors-guarantees/)
- [ZIO: Defects](https://zio.dev/reference/error-management/types/defects/)
- [ZIO: Sandboxing](https://zio.dev/reference/error-management/recovering/sandboxing/)
- [Koka: Programming with Row-polymorphic Effect Types](https://arxiv.org/pdf/1406.2061)
- [The Koka Programming Language](https://koka-lang.github.io/koka/doc/book.html)
- [Roc: Functional](https://www.roc-lang.org/functional)
- [roc-lang/roc#6688: `crash` keyword causes exit with code 0](https://github.com/roc-lang/roc/issues/6688)
- [WebAssembly Component Model: Explainer](https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md)
- [WebAssembly Component Model: WIT Reference](https://component-model.bytecodealliance.org/design/wit.html)
