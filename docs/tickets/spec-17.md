# SPEC-17 · Nothing says a `Float`-returning operation may not totalize with a zero, and one of them does

**Sizing:** small. Prose in one chapter — a rule stated beside the `Int` zeros it contrasts
with, one clause on the two-outcomes table, and one decision. No compiler change unless the
decision goes against `consume_number`, and then it is a separate `LANG-`.

**Location:** [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md) — the
*Numbers* section, the [two-outcomes table](../spec/evaluation-semantics.md#two-outcomes) near
the top of the chapter (`Integer division by zero | Defined below to be 0`), and *Open
questions*. The literal behaviour is in `src/compiler/parser/tokenizer.rs` — `consume_number`,
the `f64::from_str(&buf)` at its end.

**Problem:** the chapter keeps its [two-outcomes promise](../spec/evaluation-semantics.md#two-outcomes)
by inventing a value wherever an operation would otherwise fail, and for `Int` the invented
value is always a zero: `n // 0` is `0`, `modBy 0 n` is `0`, `remainderBy 0 n` is `0`. `Int`
has no alternative. Every one of a 32-bit two's-complement integer's values is a number
somebody might have meant, so whatever `// 0` returns is indistinguishable from a real answer,
and the chapter says as much — "a caller for whom a zero divisor is a real case tests the
divisor."

`Float` is not in that position and the chapter never says so. binary64 reserves a value for
exactly this case: `nan` is not any number, it propagates through every arithmetic operation it
reaches, and it is detectable — `std/core`'s `Basics` already exposes `isNaN` and `isInfinite`.
So a `Float` operation with no answer has somewhere honest to put that fact, where an `Int` one
does not.

Today every `Float`-returning operation in `std/core` does put it there, but by inheritance
rather than by rule. `sqrt -1`, `0.0 / 0.0` and `logBase 0 0` — `Js.Basics.log 0` twice, then
`fdiv`, so `-inf / -inf` — are all `nan` because `Js/Basics.mjs` hands the work to JavaScript's
`Math` and JavaScript implements IEEE. No chapter requires it. A `Float` function written in
Zelkova rather than behind a facade, or the same `std/core` on a WebAssembly target, inherits
nothing.

And the one place the *language* rather than IEEE has to name a `Float` out of nothing, it
names a zero. A decimal literal too small for binary64 denotes positive zero, which is the rule
the rest of this branch writes down after `f64::from_str`: a programmer who wrote a run of 324
digits meaning a very small non-zero number gets a value indistinguishable from a written
`0.0`. That is the `// 0` pattern reappearing in the one type that did not have to accept it.

**The question to settle:** whether `docs/spec/` states, as a standing rule, that **a
`Float`-returning operation with no meaningful answer produces `nan` and never a totalizing
zero** — and what that rule then does to the underflowing literal.

Three things it decides, in ascending order of how live they are.

- **What is already true.** Nearly all of it: every `Float` operation the language has produces
  IEEE's answer, and IEEE's answer to a domain error is `nan`. Writing the rule down mostly
  ratifies that. What it buys is that the behaviour stops being an accident of the JavaScript
  target — a future backend, or a `Float` function written in Zelkova, is bound by it rather
  than left to rediscover it.
- **Whether it binds a facade.** A [`module javascript` facade](../spec/js-interop.md)
  returning a `Float` is code the compiler cannot inspect, and the chapter's *Purity and the
  JavaScript boundary* section already constrains what such a companion may do. Whether this
  rule reaches across that boundary — whether a companion returning `0` for an input it has no
  answer for is breaking a language rule or merely being unhelpful — is a real question and the
  section that answers it is the one to extend.
- **The underflowing literal**, the single case where the rule and the current behaviour
  disagree. Three positions. Leave it: round-to-nearest is IEEE's own rule for
  decimal-to-binary conversion, and a rule about *operations* does not reach a *literal*, so
  there is nothing to reconcile. Make it `nan`: loud and propagating, but a deliberate
  divergence from IEEE that *Numbers* would have to state as one, in a chapter that currently
  says "IEEE's own answers throughout". Or reject it at compile time: equally loud, with a
  caret and a source location instead of a value that shows up three functions away, and no
  divergence from IEEE at all — at the cost of a diagnostic and a `LANG-`. This ticket does not
  pick. Note the case is barely reachable today, since exponents are not tokenized and
  underflow therefore takes a literal ~324 digits long; that is an argument for the cheapest
  answer and against none of them.

**Approach:** follow `write-spec-chapter`, at the scale of a section. Then:

1. *Numbers* states the rule next to the `Int` zeros, so a reader meets the contrast where the
   two types force it rather than two sections apart.
2. The two-outcomes table's integer-division row says *why* the answer is a zero — because
   `Int` has no value meaning "no answer" — which is the clause that makes the absence of a
   `Float` row meaningful rather than an omission.
3. The underflowing-literal sentence in *Numbers* is settled either way. If the answer is `nan`
   or rejection, the compiler change is a `LANG-` filed from here and not made in the same
   diff.
4. The *Open questions* entry naming this ticket is deleted.

**What this is not.** Not a proposal to give `Int` a `nan`: a 32-bit two's-complement integer
has nowhere to put one, and the three zeros stay exactly as specified. Not a reopening of the
rounding rule — round-to-nearest with ties to even is settled on this branch and is not in
question. Not [`BUG-24`](bug-24.md), which is the two companion `.mjs` files whose
division-by-zero behaviour does not match the zeros the chapter already specifies. Not the
`unwrap()` in `consume_number`, which is [`BUG-12`](bug-12.md). And do not narrow the float
token's spelling: *Floats* in [Lexical structure](../spec/lexical-structure.md) is checked by a
tagged block and the spelling is settled.

**Acceptance:** [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md) says
whether a `Float`-returning operation may produce a totalizing zero, whether that reaches a
`module javascript` companion, and what an underflowing literal denotes. The *Open questions*
entry pointing here is gone. `cargo test --test spec` green. If the settled answer differs from
what the compiler does today, a `LANG-` ticket carries the change and this diff stays
docs-only.

**Found:** the original SPEC-17 — "what a `Float` literal denotes is unspecified, and the
tokenizer has already picked an answer" — was filed on 2026-09-04 while auditing `docs/spec/`
for open questions with no ticket attached. Writing it answered the rounding half and turned up
this one: respecialized on 2026-09-06, with the rounding rule landing on the same branch.
