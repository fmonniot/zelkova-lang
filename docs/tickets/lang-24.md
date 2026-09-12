# LANG-24 · An `infix` precedence outside 0–9 is accepted

**Sizing:** small.

**Location:** `src/compiler/parser/grammar.lalrpop`, the `Infix` production —
`let precedence = u8::try_from(p).map_err(...)`. The value is carried as a `u8` from there
through `parser::Infix`, `canonical::Infix` and `Interface::infixes`.

**Decided by:** [`docs/spec/declarations.md`](../spec/declarations.md)'s *Precedence is 0
through 9*.

**Problem:** the language gives an operator a precedence of `0` through `9`. The grammar
accepts any integer literal the tokenizer produced and narrows it to a `u8`, so `infix left
10 (^) = pow` and `infix left 255 (^) = pow` both compile.

Above 255 the narrowing is rejected with `parser::Error::InfixPrecedenceOutOfRange` — that was
`BUG-12`, now closed, and the `=>?` fallible action it added to the `Infix`
production is the mechanism this ticket reuses.

**Approach:** validate the literal in the `Infix` production and return a user error for
anything outside `0..=9`, using the same `ParseError::User` path `BUG-12` established —
`u8::try_from(p).map_err(|_| ParseError::User { error: Error::InfixPrecedenceOutOfRange { .. } })?`
inside the `=>?` action. Either extend that variant to also cover `0..=9` or add a sibling one;
report the range in the message either way, since a reader who wrote `10` needs to be told
what the ceiling is rather than that `10` is wrong.

The narrowed type can then be tightened or left alone; `u8` is a fine carrier for `0..=9`,
and a newtype is only worth it if something else starts depending on the range.

**Acceptance:** the block under *Precedence is 0 through 9* in
[`docs/spec/declarations.md`](../spec/declarations.md), tagged `expect=ok` today, goes **red**
— retag it `expect=parse-error` with the reason the new error carries, and delete the **Known
gap:** paragraph beneath it. A parser test for `infix left 10` asserting on that error, seen
to fail before the fix.
