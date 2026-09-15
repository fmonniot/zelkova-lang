# LANG-56 · `std/core`'s JavaScript companions implement a 32-bit `Int` held in a number

**Sizing:** medium — two companion files, mechanical once the representation is settled, except
for one function the ticket does not decide.

**Location:** `std/core/src/Js/Basics.mjs` — `add`, `sub`, `mul`, `idiv`, `remainderBy`, `modBy`,
`truncate`, `ceiling`, `floor` and `round` — and the whole of `std/core/src/Js/Bitwise.mjs`,
including its header comment.

**Decided ([`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md#numbers) and
[the admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary), argued in
[DEC-16](../decisions/dec-16.md)):** `Int` is a 64-bit signed two's-complement integer, wrapping
on overflow, the same on every target; on JavaScript it is represented as a `BigInt`, because a
number is exact on integers only to `2^53`.

**Problem:** the companions carry an `Int` in a JavaScript number, and wrap it at 32 bits where
they wrap it at all. Neither is what the language now says, and the two failures point in
opposite directions.

`add`, `sub` and `mul` are `a + b`, `a - b` and `a * b` on numbers. Nothing wraps: past `2^31`
the result keeps growing, and past `2^53` it stops being exact, so `mul` returns a number that is
neither the mathematical product nor the wrapped one.

`idiv`, `truncate`, `ceiling`, `floor` and `round` apply `| 0`, which is JavaScript's coercion to
a signed 32-bit integer. Those wrap at the wrong width, and their doc comments cite
[Converting a `Float` to an `Int`](../spec/evaluation-semantics.md#converting-a-float-to-an-int)
for a rule that now names 64 bits.

`Js/Bitwise.mjs` is the same defect stated out loud. Every one of its seven functions uses a
JavaScript bitwise operator, each of which coerces to 32 bits, and its header comment says so
approvingly: *"JavaScript's bitwise operators coerce their operands to 32 bit integers, which is
exactly the range Zelkova's `Int` promises to be well defined over"*. That sentence was true when
it was written and is not now.

**Approach:** carry an `Int` as a `BigInt` in both files, and let the target's own wrapping do
the work it was doing at the wrong width. `BigInt` arithmetic does not wrap on its own — it is
arbitrary precision — so each operation ends in a mask to 64 bits, the way `| 0` was a mask to
32. `BigInt.asIntN(64, x)` is that mask.

The three zero-divisor answers stay what [An operation with no
answer](../spec/evaluation-semantics.md#an-operation-with-no-answer) sets — `n // 0`,
`modBy 0 n` and `remainderBy 0 n` are all `0` — but they stop falling out for free: `idiv` gets
them today from `(a / 0) | 0` being `0`, and `BigInt` division by zero throws. Each needs the
explicit guard `modBy` already has.

**`shiftRightZfBy` names its width** ([DEC-16](../decisions/dec-16.md) decision 6). `BigInt` has
no `>>>`, so the operation reads its operand as a 64-bit two's-complement pattern, shifts zeros
in from the left, and reads the result back as a signed `Int`:

```js
export function shiftRightZfBy(offset, a) {
  return BigInt.asIntN(64, BigInt.asUintN(64, a) >> BigInt(offset));
}
```

The outer mask is not redundant. It is a no-op for every offset of 1 or more — a zero-filled
shift leaves at most 63 significant bits — and it is what makes an offset of `0` the identity
rather than `2^64 - 1`.

[`Bitwise.zel`](../../std/core/src/Bitwise.zel)'s doc comment needs the same pass. Its three
examples are 32-bit: `shiftRightZfBy 1 -32 == 2147483632` is `9223372036854775792` at 64 bits,
and the two positive cases (`shiftRightZfBy 1 32 == 16`, `shiftRightZfBy 2 32 == 8`) are
unchanged. The sentence about handing back a value outside `Int`'s range goes: it cannot happen
now, which is the one place this widening made a function's contract simpler.

**Still to escalate: what a negative shift count means.** It is not `shiftRightZfBy`'s question
alone — `shiftLeftBy` and `shiftRightBy` have it too. Under `BigInt` a negative count reverses
the shift's direction, so `shiftRightZfBy -1 8` is `16`, a right shift that shifted left.
JavaScript's operators mask the count to five bits and never had the case. Ask before picking;
nothing else in either file depends on the answer.

**Note — this ticket has no red test behind it.** `cargo test` never loads a `.mjs`, there is no
code generator to exercise these functions, and `std/core/tests/` carries checks for `Js/Utils`
only. Every claim above is read off the files by eye, and nothing will go green or red when it
lands.

**Acceptance:** no `| 0`, and no bare `+`, `-` or `*` on a value the file treats as an `Int`, in
either companion. `Js/Bitwise.mjs`'s header comment names 64 bits and no longer cites a 32-bit
range as `Int`'s promise. `shiftRightZfBy` masks as above, and `Bitwise.zel`'s doc comment for it
carries the 64-bit example and no longer promises a result outside `Int`'s range. `cargo run`
still prints `parsed 8 modules` and lists all eight as checked.
