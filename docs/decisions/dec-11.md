# DEC-11 · What a value describing an effect is: seven decisions

**Settled:** 2026-09-08, by the language owner (`SPEC-15`).
**Status:** live; decision 5 is extended by [DEC-12](dec-12.md), which decides how a failure
gets into the value.
**Where the rule lives:** [Evaluation semantics — Effects](../spec/evaluation-semantics.md#effects),
[JS interop — An effectful facade](../spec/js-interop.md#an-effectful-facade),
[Packages — Programs](../spec/packages.md#programs) and
[What a test is](../spec/packages.md#what-a-test-is).

Three open questions in two chapters were one question asked three times: what a value describing
an effect is, what type `main` must have, and what makes a declaration under `tests/` a test. The
second and third are consequences of the first, which is why they were settled together.

Two rules already written down did most of the deciding, and any answer had to survive both. A
class variable [stands for a complete type](../spec/type-classes.md#a-class-is-always-over-a-complete-type),
so there is no `Monad` for an effect type to be an instance of. And a facade is
[the only way into JavaScript](../spec/js-interop.md), with no privileged escape hatch the
standard library may use and a user's package may not — which rules out Elm's answer directly,
since Elm's `Task` comes out of kernel code no user can write.

## 1 — An effect is a value of an ordinary type, not a construct the language knows

[`Task`](../spec/evaluation-semantics.md#effects) is a type `zelkova-core` declares and exposes
without its constructors. The compiler knows its name, the way it already knows `Position`, and
knows to run the one `main` names; nothing else about it is built in.

**Sequencing as syntax** was the alternative — a `do` block the compiler compiles into the
generated driver, with no `andThen` to desugar to. It sidesteps both restrictions above at once,
which is what made it a real candidate. It loses because it makes effects second class: a `Task`
that is a value can be held in a list, passed to a function, returned from one and stored in a
record, and every one of those needs a new form once sequencing is syntax rather than a function.

## 2 — A primitive effect is a facade whose result type is `Task a`

[An effectful facade](../spec/js-interop.md#an-effectful-facade) is where an effect enters the
language, and the only place it does. The companion returns the payload rather than a `Task`; the
compiler builds the `Task` around the call, which is what keeps an impure companion reachable
only through a value the runtime forces.

This is one boundary rather than two, and it is what makes a user's package able to declare an
effect on the terms `zelkova-core` declares one. **A second boundary** — a new module modifier
beside `javascript` — was considered and rejected as machinery for a distinction the result type
already draws.

## 3 — The admitted-type rules are untouched

[Which types may cross](../spec/js-interop.md#which-types-may-cross-the-boundary) constrained the
answer: whatever crosses has to be something a runtime predicate can decide, which rules out a
bare type variable and a function type. Because a `Task` never crosses — the companion hands back
an `a` and takes arguments of admitted types — an effectful facade is exactly as monomorphic as a
pure one, and no new predicate had to be invented. `Task` is confined to the whole of a result
type for the same reason: as an argument it would be a value JavaScript has no predicate for.

## 4 — Sequencing is ordinary functions over one concrete type

`Task.andThen`, `Task.map` and `Task.succeed` are ordinary functions
([Sequencing](../spec/evaluation-semantics.md#sequencing)). The ticket framed the lack of
higher-kinded variables as the crux, on the grounds that a monadic interface is unavailable and
this is the first place the language needs one. That framing was wrong in a useful way: what is
unavailable is a monad *class*, and what is needed is one concrete type with functions over it.
`andThen : (a -> Task b) -> Task a -> Task b` quantifies over no type constructor and is an
ordinary signature.

The restriction is therefore not relaxed and not worked around. What the language gives up is
abstraction *over* effect types — there is one, so there is nothing to abstract over.

## 5 — `Task` takes one type parameter; failure goes in the value

An effect that can fail produces a `Task (Result e a)`
([An effect that can fail](../spec/evaluation-semantics.md#an-effect-that-can-fail)), which is
where [every other failure the language admits](../spec/evaluation-semantics.md#two-outcomes)
goes.

**Elm's `Task e a`**, with a failure channel `andThen` short-circuits on, was the alternative, and
it reads better at a chain of fallible effects. It costs a second parameter on every signature, a
`Never` type so that a task which cannot fail can say so, and two ways to express failure in one
language — where the chapter's own table already sends a lookup, a parse and a conversion into
`Maybe` and `Result`.

**Extended.** This decision left *which* value, and how it gets there, to the author of the
`.mjs`: the chapter's rule was that a companion may not throw. [DEC-12](dec-12.md) replaces that
contract with a required result type, so an effectful facade declares `Task (Result Failure a)`
and cannot declare anything else. The decision above is unchanged — one type parameter, and
failure in the value — and what moved is that the compiler now checks it rather than trusting it.

## 6 — `main : Task ()`

[Programs](../spec/packages.md#programs). The ticket ruled out one answer in advance: `main` as a
value of an arbitrary type the compiler prints, which defers the question and makes the first real
effect a breaking change to every program.

The `()` rather than an unconstrained `a` is the smaller choice inside the larger one. Nothing is
waiting on a program's result, so a `main` of any other type names a value nobody reads.

## 7 — A test is an exposed value of type `Test`

[What a test is](../spec/packages.md#what-a-test-is). `Test` is declared by `zelkova-test` and
exposed without its constructors; what a `Test` *holds* is that package's, not the language's.

**A naming convention** — an exposed value whose name begins with `test` — was the alternative
that needs nothing from the type system, and it loses because nothing else in Zelkova is found by
how a thing is spelled. **A declaration form the language does not have** was the third, and it
buys nothing a type does not: a runner reads a module's interface either way.

**`Test` declared by `zelkova-core`**, beside the `Task` this entry puts there, is where this
decision first landed, and it is rejected. Core is a dependency of every package and is not
written in `dependencies`, so that spelling puts `Test` in scope for every program that will
never run one. Declaring it in a package reached through
[`test-dependencies`](../spec/packages.md#test-dependencies) takes the restriction out of
dependency resolution instead of out of a new rule about source roots, and leaves the runner a
type only a package that asked for it can name.

The asymmetry with `main` is deliberate and is the reason a type rather than a manifest field
does the work here. A package has one entry point, so a field can name its module and a fixed name
can find it inside; a package has as many tests as it likes, spread over as many modules, so
there is no one name to fix and nowhere to list them.

## The name, and the eighth default import

`Task` over `Effect` and `IO`. `Effect` collides with the word the chapters need for the concept,
and *an `Effect` describes an effect* is a sentence that teaches nothing; `IO` carries the monadic
reading decision 4 cannot honour. `Task` is Elm's name, which costs nothing here — the spec is
self-contained, so a name is inherited in full or not at all, and this one is written out where it
lands.

`Task` joins [the default imports](../spec/modules.md#the-default-imports) as
`import Task exposing (Task)`, the shape `List` already has. The list exists so that the types
appearing in ordinary annotations are always writable, and `main`'s is the one annotation every
program has to write. `Test` is not on that list and could not be: it is drawn from
`zelkova-core`'s modules, and `Test` is `zelkova-test`'s.

## What nothing checks

The mechanism has no implementation and this decision did not give it one. Every block the
chapters gained is held only to failing to compile, or — for the three that name `Task` in a
facade signature — to compiling for a reason unrelated to effects: an unresolved type name is
invented rather than reported ([`BUG-16`](../tickets/bug-16.md)), so `Task` is read as a type the
build does not have. Those three go red when `BUG-16` lands and green again once `zelkova-core`
declares the type, which is the only accountability any of this carries until
[`GEN-1`](../tickets/gen-1.md) emits a runtime and [`LANG-15`](../tickets/lang-15.md) grows a
`tests/` root.
