# SPEC-17 · What a `Float` literal denotes is unspecified, and the tokenizer has already picked an answer

**Sizing:** small. Prose in two chapters and one decision, most of which is confirming that the
answer the tokenizer already gives is the answer the language wants.

**Location:** [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s *Floats*, which
says what a float literal is *spelled* like and not what it denotes;
[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *Open questions*, fourth
entry. The behaviour is in `src/compiler/parser/tokenizer.rs` — `consume_number`, the
`f64::from_str(&buf)` at its end.

**Problem:** [Evaluation semantics](../spec/evaluation-semantics.md#open-questions) says:

> `Float` is binary64 and most decimal literals are not, so a literal is rounded. Which rounding,
> and whether a literal that is not representable is an error rather than a rounding, is
> unspecified.

[Lexical structure](../spec/lexical-structure.md)'s *Floats* specifies the spelling exactly — a
run of digits, a `.`, at least one digit on each side, an optional exponent — and stops there. So
between the two chapters, `0.1` is a well-formed token whose value the language does not name.

Meanwhile the compiler has an answer. `consume_number` accumulates the literal's characters into
a `String` and hands it to `f64::from_str`, which is Rust's correctly-rounded decimal-to-binary64
conversion: round-to-nearest, ties-to-even, and no failure for a value out of range — a literal
larger than `f64::MAX` becomes `inf` rather than an error. That is a defensible answer and quite
possibly the right one. It is not a *specified* one, and the gap matters in the direction that
gaps usually matter: `1e400` compiles today and silently denotes infinity, which no chapter
either permits or forbids.

**The question to settle:** two questions, and the second is the live one.

- **Which rounding.** Round-to-nearest, ties-to-even is what IEEE 754 specifies for
  decimal-to-binary conversion, what `f64::from_str` implements, and what every other language a
  reader is likely to come from does. Choosing anything else would need a reason this project
  does not have. Expect this to be a paragraph confirming the obvious rather than a decision.
- **What happens to a literal that is not representable.** Three positions: it rounds to the
  nearest representable value, including to `inf` when it overflows and to `0.0` when it
  underflows, which is what the compiler does today; it rounds but *overflow* specifically is an
  error, on the grounds that a literal denoting infinity is always a mistake in a way that
  `0.1`'s rounding is not; or the compiler warns. The first is free and is already true. The
  second costs a diagnostic and is arguably what a reader wants — nobody writes `1e400`
  deliberately. This ticket does not pick.

Whichever wins, the chapter also has to say whether `Float` is binary64 *normatively* — the open
question asserts it in passing and no section states it as a rule — and what the language says
about the values binary64 has that decimal notation does not spell: `NaN`, the two infinities and
negative zero are reachable at runtime whether or not a literal can name them.

**Approach:** follow `write-spec-chapter`, at the scale of a section. Then:

1. [Types](../spec/types.md) or [Evaluation semantics](../spec/evaluation-semantics.md) — whichever
   the chapter split puts it in — states normatively that `Float` is IEEE 754 binary64, which the
   open question currently assumes without saying.
2. *Floats* in [Lexical structure](../spec/lexical-structure.md) gains a sentence on what the
   token denotes, linking to the rule rather than restating it. A reader who arrives at *Floats*
   asking what `0.1` is should not leave with only its spelling.
3. The fourth entry in [Evaluation semantics](../spec/evaluation-semantics.md)' *Open questions*
   is deleted.
4. If the answer is that overflow is an error, that is a `LANG-` ticket against
   `consume_number`, filed separately and not fixed here — a spec change and a semantics change
   do not share a diff.

**What this is not.** Not a fix for the `unwrap()` in `consume_number`; that is
[`BUG-12`](bug-12.md), which already owns both of that function's unwraps and reproduces the
inputs that panic. This ticket is about what a *well-formed* literal means, which is the case
`BUG-12` does not touch. Do not narrow the token's spelling either: *Floats* is checked by a
tagged block and the spelling is settled.

**Acceptance:** `docs/spec/` states that `Float` is binary64, which rounding a literal undergoes,
and what happens to one that overflows. The fourth entry in
[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)' *Open questions* is gone,
and *Floats* links to wherever the answer landed. `cargo test --test spec` green. If the settled
answer is that the compiler's current behaviour is correct, the chapter says so with a tagged
block; if it is not, the block is tagged for what the compiler does today and a `LANG-` ticket
carries the change.

**Found:** while auditing `docs/spec/` for open questions with no ticket attached, on 2026-09-04.
