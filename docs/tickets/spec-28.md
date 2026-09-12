# SPEC-28 · Two chapters disagree on `Int`'s range, and the tokenizer enforces a third bound

**Sizing:** small-to-medium — the chapter edits are small, but the question underneath them is a
language decision this ticket does not make, and the answer decides whether a `LANG-` follow-up
exists at all.

**Location:** [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s *Integers* and
*Numeric literals that are rejected*, [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s
*Numbers*, and — for the third bound — `src/compiler/parser/tokenizer.rs`'s `consume_number`,
whose `i64::from_str(&buf)` arm raises `TokenizerErrorType::IntegerOverflow`, carrying its value
in `Token::Integer { value: i64 }`.

**Problem:** three different answers to "how large may an integer be", two of them normative.

*Integers* says:

> The language guarantees that every integer in `-2^31 .. 2^31 - 1` is representable on every
> target. Beyond that range the compilation target decides: the JavaScript backend is exact to
> `2^53`, and a future WebAssembly backend would use 64-bit two's-complement arithmetic with the
> wraparound that implies.

*Numbers* says:

> **`Int` is a 32-bit signed two's-complement integer.** Arithmetic wraps: `2147483647 + 1` is
> `-2147483648`. The range and the wrapping are the same on every compilation target, so a
> program computes the same answer wherever it is run.

Those cannot both hold. The first leaves everything above `2^31 - 1` to the target and names two
different behaviours for two targets; the second closes the range at 32 bits and makes it
target-independent, which is the whole point of the sentence. *Numbers* is the later chapter
(`SPEC-9`) and [`LANG-36`](lang-36.md) is already open to delete the same target-dependent claim
from `std/core/src/Basics.zel`'s `type Int` comment — so *Integers* looks like the stale copy,
but nothing records that decision and this ticket does not make it.

Neither chapter says what happens to an integer **literal** whose value is outside the range.
*Floats* answers the corresponding question for floats — a literal too large in magnitude denotes
positive infinity — and no sentence anywhere answers it for integers: rejection, wrapping and
saturation are all consistent with what is written.

The compiler answers it with a bound no chapter names. `consume_number` carries the value in an
`i64`, so `f = 99999999999999999999` is rejected on every target and `f = 3000000000` — outside
`Int`'s 32-bit range under *Numbers*, and target-dependent under *Integers* — is accepted
everywhere. The `expect=parse-error:IntegerOverflow` block in *Numeric literals that are
rejected* pins that rejection, and the **Known gap:** paragraph beside it cites this ticket as
the record that `i64` is a carrier rather than a rule.

**Found:** in review on PR #191, which closed `BUG-12` by turning `consume_number`'s
`i64::from_str(&buf).unwrap()` panic into an `IntegerOverflow` error. The reviewer noticed that
the paragraph introducing the new block stated the `i64` bound in the rule-stating voice while
*Integers* said something different; reading the two chapters against each other turned up the
contradiction above. Deliberately not fixed there: `BUG-12` replaced a panic with a diagnostic
and changed no bound, and *A spec change and a semantics change do not share a diff* keeps the
decision out of that PR.

**Approach:** decide the range first, then write it in exactly one place.

1. Settle whether `Int` is 32-bit on every target or target-dependent beyond `2^31 - 1`. The
   options are not balanced — *Numbers* argues the case for target-independence at length, and
   `LANG-36` is filed on the assumption that it won — but the decision belongs to the language
   owner, and the losing chapter's paragraph is deleted rather than softened.
2. State the surviving rule in one chapter and have the other link its anchor; *A conclusion is
   stated in one place*. `Int`'s arithmetic is *Numbers*' subject, so that is the likely home,
   with *Integers* keeping only what is about the token.
3. Answer the out-of-range **literal** question in *Integers*, since it is a rule about the
   token: rejected, wrapped, or saturated. Rejection is what the compiler does today and the
   only one of the three a reader would guess.
4. If the settled bound is not `i64`, file a `LANG-` for `consume_number` and point the **Known
   gap:** at it instead of at this ticket. If the answer makes the current behaviour correct,
   that paragraph is deleted and the block stands on its own.

**Acceptance:** `grep -rnF "2^31" docs/spec/` finds the range stated in one chapter; the other
links to it. *Integers* says what an out-of-range integer literal does. The **Known gap:**
paragraph in *Numeric literals that are rejected* either cites a `LANG-` ticket for the
compiler's remaining divergence or is gone. `cargo test --test spec` is green — it reads every
block and link in both chapters, so a reworded paragraph that drops an anchor fails it.
