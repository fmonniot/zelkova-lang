# GEN-11 · Emit the tail-call loop

**Sizing:** small. One alternative shape for a declaration's body, plus the argument-assignment
care the **Approach** names.

**Depends on:** [`GEN-6`](gen-6.md) (the mark) and [`GEN-9`](gen-9.md) (the emitter).

**Part of:** [`GEN-1`](gen-1.md).

**Location:** the backend module [`GEN-9`](gen-9.md) creates, where a declaration's body is
emitted.

**Decided ([`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md#recursion-and-tail-calls)):**
**a self tail call runs in constant stack**, compiled as a jump back to the top of the
declaration with new arguments — so a recursion written that way is as deep as the compiler's
stack allows. [`GEN-6`](gen-6.md) has already decided which calls those are; this ticket emits
them.

**Problem:** recursion is the only way to iterate in Zelkova. A self call emitted as a call
grows the stack however it is written, so without this rewrite every loop in every Zelkova
program is bounded by the host's stack, and the chapter's one non-obvious promise about the
generator is unkept.

**Approach:** a declaration holding at least one marked call emits its body inside
`while (true) { … }`. A marked call becomes: compute the new arguments, assign them to the
parameters, `continue`. Every other path out of the body `return`s.

**Assign all the parameters at once, not one at a time.** `count (Succ acc) m` assigns both, and
the new value of one may be computed from the old value of another — assigning `acc` before `m`
is read gives the wrong answer for any declaration whose arguments cross over. Evaluate every
new argument into a temporary first, left to right, and only then assign. That order is also
what [Order of evaluation](../spec/evaluation-semantics.md#order-of-evaluation) requires of the
arguments themselves.

A declaration with no marked call emits exactly what [`GEN-9`](gen-9.md) emits today: no loop,
no temporaries. The rewrite is not free to read, and a declaration that does not need it should
not carry it.

**Not in this ticket:** mutual tail recursion, which
[carries no guarantee](../spec/evaluation-semantics.md#recursion-and-tail-calls) and is not
rewritten. Say so in the doc comment so the next reader does not take its absence for an
oversight.

**Acceptance:** a test asserts the emitted text for a declaration with a marked call contains
the loop and for one without does not. The behavioural half — through the harness
[`GEN-14`](gen-14.md) sets up — runs a self-recursive function in tail position to a depth deep
enough that a non-tail emission exhausts Node's stack, and asserts the answer. A second
behavioural test covers the crossing case: a two-parameter tail-recursive declaration whose
recursive call swaps its arguments, asserting the value simultaneous assignment gives and not
the one sequential assignment gives.

Neutralise-check both: revert the loop emission and the depth test overflows; assign the
parameters in sequence without temporaries and the swap test goes red. `cargo run` still prints
`parsed 8 modules`, lists all eight and exits 0.
