# LANG-67 · `pow`'s `bigint` branch can materialize an astronomically large intermediate before masking

**Sizing:** small — same shape as [LANG-64](lang-64.md): a guard added before the operation
runs, in the one function that lacks it.

**Location:** `std/core/src/Js/Basics.mjs` — `pow`'s `bigint` branch (`BigInt.asIntN(64, a **
b)`).

**Depends on:** none directly, but should land before or alongside [GEN-1](gen-1.md) (emitting
runnable JavaScript for a checked module), which is what makes `pow` reachable from compiled
Zelkova code.

**Found:** during review of the PR implementing [LANG-65](lang-65.md), which brought `pow` to
the 64-bit `BigInt` representation `DEC-16` settled. Out of that PR's stated scope (`toFloat`,
`pow`'s dispatch, and `_Utils_isOrdered`), so noted at its site rather than fixed there.

**Problem:** `pow`'s `bigint` branch is `BigInt.asIntN(64, a ** b)` — it computes `a ** b` to
full precision and masks only the final result. `shiftLeftBy`/`shiftRightBy`
(`std/core/src/Js/Bitwise.mjs`, [LANG-64](lang-64.md)) faced the same shape of question for a
large-magnitude operand and answered it the other way: they clamp their shift count into `0 ..
64` *before* doing any work, so the operation itself never runs on an unbounded value.

`pow` has no analogous guard. For a large but otherwise valid non-negative `Int` exponent —
`pow(2n, 10000000000000n)`, say — JavaScript's `**` must build a `BigInt` with trillions of
bits before `BigInt.asIntN` ever gets to mask it down to 64. That is a real risk of an OOM or a
multi-second hang, not the wrapped 64-bit answer DEC-16 promises for `Int` arithmetic — and
unlike a genuine "no answer" case (`n // 0`), a bounded answer exists and is cheap to compute:
for any base `|a| >= 2`, a 64-bit `Int` result is forced to `0` once the exponent is large
enough that `a ** b` cannot help but overflow 64 bits (`b >= 64` for `|a| >= 2`, with `a` in
`{-1, 0, 1}` handled by the existing arithmetic without needing the large-exponent path at all).
Modular exponentiation (masking at each squaring step instead of at the end) would compute the
same wrapped answer in bounded time for every exponent, whether small or large.

This is not reachable today: there is no code generator yet ([GEN-1](gen-1.md)), so nothing
calls `pow`'s companion from compiled code. Neither LANG-65 nor [LANG-66](lang-66.md) (which is
scoped to what a *negative* `Int` exponent means) considers the cost of a large *non-negative*
one.

**Approach:** mirror LANG-64's precedent of bounding before computing rather than after. Two
shapes work:

1. **Short-circuit on a large exponent.** Once `b` exceeds a small bound (64 is enough for any
   `|a| >= 2`), return the wrapped answer directly — `0n` for `|a| >= 2`, and `a`'s own fixed
   point for `a` in `{-1n, 0n, 1n}` — without invoking `**` at all.
2. **Modular exponentiation.** Square-and-mask in a loop (`BigInt.asIntN(64, ..)` applied at
   each step, not just the final one), which is mathematically equivalent to the current
   computation for any exponent that would have finished, and bounded in cost for one that
   would not.

Either keeps the `Float` branch (`Math.pow`) untouched — this is a `bigint`-only gap.

**Acceptance:** a `node --test` addition in `std/core/tests/Js/BasicsChecks.mjs` calls `pow`
with a large non-negative `Int` exponent (e.g. `pow(2n, 10000000000000n)`) and asserts it
returns promptly with the correct wrapped 64-bit answer, rather than hanging or exhausting
memory. `pow`'s existing small-exponent checks (`pow(3n, 2n)`, `pow(2n, 64n)`, `pow(2n, 63n)`)
still pass unchanged.
