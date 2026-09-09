# DEC-9 · What a program may rely on about space: sharing, not closures

**Settled:** 2026-09-08, by the language owner (`SPEC-16`).
**Status:** live.
**Where the rule lives:** [Evaluation semantics — Sharing](../spec/evaluation-semantics.md#sharing).

[Evaluation semantics](../spec/evaluation-semantics.md) made exactly one promise about memory —
a self tail call runs in constant stack — and left open whether it made any other. Three
positions were on the table, and they are not points on one scale: promise nothing beyond the
tail-call rule, promise that a value's identity is preserved when it is passed, returned, bound
or stored ("sharing"), or promise sharing plus that a partially applied function is not rebuilt
per call. The question was worth settling before a code generator existed to answer it
incidentally: the first backend's incidental behaviour would otherwise become the rule the
second backend broke.

## 1 — Sharing is preserved; a closure guarantee is not

The middle position won. [Sharing](../spec/evaluation-semantics.md#sharing) states it: a value is
not copied when it changes hands, so binding a large structure to a second name costs nothing
beyond the binding itself.

**Nothing beyond the tail-call rule** was the cheapest to write and the most permissive for a
backend, and it loses on the reader it leaves behind. A program that binds a large structure to a
second name would have no portable way to know whether that costs anything, and would find out
only by measuring whatever [`GEN-1`](../tickets/gen-1.md) happens to emit. A promise this weak is
not neutral — it makes the first generated backend's incidental behaviour the specification,
which is the outcome `SPEC-16` was filed to avoid.

**Sharing plus a closure guarantee** — that a partially applied function is not rebuilt per call,
so `f x` in a loop where `f` is applied to a constant does not allocate a closure each time — is
the strongest of the three, and it loses for a reason the other two don't share: preserving
sharing needs only an account of value identity, which is what this decision gives; not
rebuilding a closure needs an account of what currying compiles to, which is a claim about a
compiler pass rather than about a value.
[Function values](../spec/evaluation-semantics.md#function-values) already lets applying a
function to fewer arguments than its type has arrows produce a function value carrying whatever
its body needs from its scope, and says nothing about whether two such values, built from the
same partial application, are one value or two — committing to "one" would be a real
strengthening of that section, and this decision leaves it unmade.

Nothing outside this decision checks either half of it: the spec harness compiles and
canonicalizes modules and observes nothing about a running program's memory, and
[`TEST-2`](../tickets/test-2.md) is the ticket for the runtime that would let it. Whether a code
generator honours the sharing promise is [`GEN-1`](../tickets/gen-1.md)'s problem, on the same
terms as every other rule this directory hands it unenforced today.
