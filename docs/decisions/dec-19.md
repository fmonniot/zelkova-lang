# DEC-19 · A binding depends on what it reaches by mention, through functions too

**Settled:** 2026-09-27, by the language owner (`BUG-38`).
**Status:** live.
**Where the rule lives:**
[Evaluation semantics — A binding with no parameters is evaluated
once](../spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once) and [A
binding may not depend on itself](../spec/evaluation-semantics.md#a-binding-may-not-depend-on-itself).

A parameterless binding is evaluated once, before the program runs, after everything it
*depends on*; a parameterless binding that depends on itself is an error. Both rules need a
definition of *depends on*, and the one the chapter gives is **transitive mention**: the
declarations a binding's body mentions, the ones their bodies mention, and so on, through
functions as well as parameterless bindings. A cycle of functions only is not an error, since a
function's value exists before its body runs.

The earlier reading stopped at parameterless bindings: a binding that mentioned a function got
no dependency on anything that function's body read. That let `a = f 1` beside `f x = z` place
`a` before `z`, and let `a = f 1` beside `f x = a` through as acyclic, and on JavaScript both
emit a module that reads a `const` before it is initialised and throws at load — a crash in a
well-typed program, which [Two outcomes](../spec/evaluation-semantics.md#two-outcomes) rules out.

## Why mention is sound

Initialising a binding runs its own body and whatever functions that body calls. A function can
only be called if it is named somewhere in code that runs, or handed over as a value by code that
named it. An imported module cannot call back in except through such a value, since [imports
may not form a cycle](../spec/modules.md#imports-may-not-form-a-cycle). So the code that can run
while a binding is initialised is contained in what the binding reaches by mention, and ordering
by that relation never reads a binding before it has a value.

## Why nothing finer

Mention over-approximates. `a = f` beside `f x = a` mentions `f` without calling it, so it would
be safe at load, and it is an error; so is a binding that calls a function whose branch reading
the binding is never taken during initialisation. Telling those apart is undecidable in general,
and any fixed approximation between the two — "only a function in call position", say — is a
rule a user has to learn, and can still be defeated by passing the function along instead of
calling it. Mention is the widest relation that is sound, and the one with nothing to learn
beyond "what you name". A binding whose value is a function can be written with its parameter
instead (`a x = f x`), which makes it a function and takes it out of the rule entirely.

It is also Elm's rule: Elm rejects a cycle among top-level definitions when any definition in it
takes no arguments, and accepts one made of functions only.

## Initialising lazily was rejected

Emitting each parameterless binding as a value computed on first read would accept every program
above, and turn a true cycle into non-termination, which [Two
outcomes](../spec/evaluation-semantics.md#two-outcomes) permits. It was rejected on three
counts. It contradicts the chapter's "evaluated once, before the program runs". It puts a check
on every read of a top-level value, on both targets. And it moves a mistake the compiler can
report into a program that hangs.
