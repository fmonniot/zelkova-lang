# GEN-6 · A self tail call is marked in the IR

**Sizing:** small. One walk over a declaration's body with a notion of tail position, setting a
flag. No emission and no rewriting.

**Depends on:** `GEN-4`, closed — the IR is `src/compiler/ir/`.

**Part of:** [`GEN-1`](gen-1.md).

**Location:** the declaration and `Apply` nodes of the IR in `src/compiler/ir/`, and a new
pass beside the one [`GEN-5`](gen-5.md) adds.

**Decided ([`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md#recursion-and-tail-calls)):**
a self tail call runs in constant stack, compiled as a jump back to the top of the declaration
with new arguments. The chapter defines tail position exactly, and the definition is the whole
specification of this pass:

> An expression is in **tail position** when its value is the value of the whole declaration
> body. The body itself is; both arms of an `if` in tail position are; every branch body of a
> `case` in tail position is; and the expression after `in` of a `let` in tail position is.
> Nothing else is — not an argument, not an operand, not a scrutinee, not an `if`'s condition.

And the guarantee is narrow: it **covers a call to the declaration the call is written in, and
nothing wider.** Mutual tail recursion between two declarations carries no guarantee, so a call
to another declaration is never marked, however it sits.

**Problem:** recursion is the only way to iterate — there is no loop form and nothing to mutate
that a loop would use — so the chapter's guarantee is the only thing standing between an
ordinary Zelkova iteration and a stack overflow. It is also the one rule in the chapter that
cannot be met by emitting the obvious thing: a self call emitted as a call is a self call that
grows the stack, whatever position it is in.

Nothing in the tree has a notion of tail position at all.

**Approach:** walk each declaration's body carrying one bit — whether the node being visited is
in tail position — seeded true at the body and propagated by the chapter's four rules and no
others. Mark an `Apply` when it is in tail position, its callee is the enclosing declaration,
and it is saturated (`ir::Declaration::arity` and `ir::Saturation` carry both facts). An unsaturated self call in tail
position is not a jump: it produces a function value rather than re-entering, so it is a call
like any other.

There is no `let` in the language yet ([`LANG-33`](lang-33.md)), so the fourth rule has nothing
to apply to. Write it anyway if the IR has a `Let` node — the typer's term already did — and
say in the doc comment that the case is unreachable from source today.

**Not in this ticket:** emitting the loop, which is [`GEN-11`](gen-11.md). This ticket produces
a mark nothing reads yet, which is what makes it separately testable: the assertions are about
where the mark is and is not, and they do not need a runtime.

**Acceptance:** unit tests over the chapter's own `count` example asserting the mark is set, and
over `Succ (count acc m)` asserting it is not. Five more assert it is not set for a self call
that is an argument, an operand, a scrutinee, an `if`'s condition, or unsaturated; two assert it
*is* set in both arms of an `if` in tail position and in every branch body of a `case` in tail
position. One asserts a saturated call to a *different* declaration in tail position is not
marked, which is the mutual-recursion exclusion. `cargo run` still prints `parsed 8 modules`,
lists all eight and exits 0.

Neutralise-check by seeding the walk's bit true everywhere: the five "is not set" tests go red.
