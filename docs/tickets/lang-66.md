# LANG-66 · What a negative `Int` exponent means for `pow` is undecided

**Sizing:** small — one decision, then a one-line change to `pow`.

**Location:** `std/core/src/Js/Basics.mjs` — `pow`; `std/core/src/Basics.zel` — `pow`'s doc
comment; `docs/decisions/dec-16.md` — a new decision.

**Depends on:** [LANG-65](README.md), which brought `pow` to the 64-bit `BigInt` representation
of `Int` [DEC-16](../decisions/dec-16.md) settled, for its `Float` operand and its non-negative
`Int` exponent. This ticket is the one case LANG-65 deliberately did not answer.

**Found:** while implementing LANG-65. That ticket's own text said a negative `Int` exponent "is
a real question with no `docs/spec/` or `DEC-16` answer today," the same shape of question
[LANG-64](README.md) settled for a negative shift count, and asked not to guess.

**Problem:** `Basics.zel` types `pow` `a -> a -> a`, backing both `Int` and `Float`
exponentiation. `Float` exponentiation keeps `Math.pow`'s IEEE answer, including a negative
exponent (`Math.pow(2, -1) === 0.5`). `Int` has nowhere to put that answer: `2 ^ -1` is `0.5`,
which no 64-bit two's-complement pattern denotes. `**`, the operator LANG-65 uses for the
`Int` case, throws `RangeError: Exponent must be positive` on a negative `BigInt` exponent
rather than silently producing a wrong pattern, so today's behaviour is "throws" for this one
case — not a chosen answer, just what the operator happens to do.

This is not the same shape as a negative shift count. `shiftLeftBy -1 x`'s question was *which
existing bit pattern* the operation should read as the answer — a question with several
plausible readings and no correct one. `2 ^ -1`'s question is different: the exact answer
(`0.5`) exists, is well known, and simply is not an `Int`. That makes it the same shape as
`n // 0` — an operation whose result type has no room for the true answer, and a value has to
be picked to stand in for it (see [An operation with no
answer](../spec/evaluation-semantics.md#an-operation-with-no-answer)) — but `0` is `//`'s
identity-flavoured stand-in for "no answer here," and it is not obvious the same value fits
`pow`'s shape, or that a stand-in is even the right move here rather than, say, treating a
negative `Int` exponent as a type error the compiler could in principle check for a literal
exponent (it cannot check a non-literal one, since nothing in the language restricts `pow`'s
second argument to being non-negative).

**Options, none picked:**

1. **`0` as the stand-in**, matching `//`'s "no answer" reading and requiring no new rule beyond
   applying the one [An operation with no answer](../spec/evaluation-semantics.md#an-operation-with-no-answer)
   already states. Costs: `0` is `2 ^ -1`'s reciprocal's *floor*, not obviously the "identity"
   the way `n // 0 == 0` at least echoes; a caller reading `pow 2 -1 == 0` has less to anchor
   the choice to than `n // 0 == 0` gives them.
2. **`1` as the stand-in**, on the reading that a negative power is "no scaling happened" the
   way `x ^ 0 == 1` is `pow`'s own identity. Costs the same arbitrariness as option 1, from the
   other direction.
3. **Extend [An operation with no answer](../spec/evaluation-semantics.md#an-operation-with-no-answer)
   with a new, explicitly-named case** rather than reusing `0`'s existing "divisor of zero"
   framing, if the language owner judges `pow`'s gap is not actually the same shape as `//`'s.
4. **Restrict `pow`'s second argument at the type level** (e.g. `Number a => a -> Nat -> a`, or
   an annotation-only convention the compiler does not yet enforce) so a negative `Int` exponent
   is rejected before it reaches the companion, rather than answered by one. This is a much
   larger change — `Nat` does not exist, and `LANG-42`'s type-class program is what would carry
   an unsigned-exponent constraint if the language ever wants one — and is listed for
   completeness rather than as a live candidate for this ticket's size.

Picking is a language decision — it changes what `pow` promises for a whole class of inputs —
not an implementation detail, so this ticket does not choose. Whichever is picked, name it in
`docs/decisions/dec-16.md` (a new decision, since decision 5 is about the JavaScript
representation and this is a different question) and in `Basics.zel`'s doc comment for `pow`,
which today shows no negative-exponent example.

**Acceptance:** `docs/decisions/dec-16.md` states which of the above (or another option) was
chosen, and why. `Basics.zel`'s doc comment for `pow` gains a worked example with a negative
`Int` exponent matching the decision. `Js/Basics.mjs`'s `pow` implements it — a guard before the
`**` if a stand-in value was picked (options 1–3), or an unchanged `pow` with the restriction
enforced elsewhere if option 4 was picked. A `node --test` check in
`std/core/tests/Js/BasicsChecks.mjs` pins the chosen behaviour for at least one negative `Int`
exponent, replacing `PINS pow still throws on a negative Int exponent, pending LANG-66`, whose
name and body cite this ticket by number and are exactly what is stale once it closes.
