# BUG-12 · Four `unwrap()`s on user input panic the compiler instead of reporting a syntax error

**Severity:** medium. Ordinary malformed source aborts the process rather than producing a
diagnostic, and every accumulated error for every other module goes with it.

**Location:** three in `src/compiler/parser/tokenizer.rs` — `consume_number`, the
`f64::from_str(&buf).unwrap()` and `i64::from_str(&buf).unwrap()` at the end of it — and one
in `src/compiler/parser/grammar.lalrpop`, the `Infix` production's
`u8::try_from(p).unwrap()`, which already carries a `TODO don't panic once we have a way to
collect errors`.

**Found:** while writing `docs/spec/lexical-structure.md` under `SPEC-2`, probing what the
tokenizer does with literals the language rejects.

**Problem:** `consume_number` accumulates characters into a `String` with a rule looser than
the one either parser accepts, then unwraps the parse. Three inputs reach it:

| Source | Buffer | Panic |
|---|---|---|
| `f = 99999999999999999999` | `"99999999999999999999"` | `ParseIntError { kind: PosOverflow }` |
| `f = 1.2.3` | `"1.2.3"` | `ParseFloatError { kind: Invalid }` |
| `f = 1١` | `"1١"` (U+0661 ARABIC-INDIC DIGIT ONE) | `ParseIntError { kind: InvalidDigit }` |

The first is an out-of-range literal. The second is a run of digits with two `.` in it — the
loop breaks only on a character that is neither `is_numeric()` nor `.`, so it never stops at
the second point. The third is `c.is_numeric()` being true for every Unicode numeric
character, including digits `i64::from_str` will not accept.

The fourth is unrelated in mechanism and identical in consequence:
`infix left 300 (|+|) = f` parses the precedence as an `"integer"` — an `i64` — and narrows
it to `u8` with an unwrap, so any precedence above 255 aborts.

All four violate `CLAUDE.md`'s first standing invariant (no `panic!`/`unwrap()` on a non-test
path), and all four are ordinary syntax errors under
[`docs/spec/lexical-structure.md`](../spec/lexical-structure.md).

**Approach:** the tokenizer already has the machinery — `TokenizerError` and
`TokenizerErrorType` — so this is a matter of adding variants rather than plumbing. Suggested:
one new `TokenizerErrorType` per failure the author can act on, rather than a single generic
one, since the three cases want different sentences ("this integer is too large", "a number
has one decimal point", "a numeric literal is written with the digits 0-9"). Tighten the
accumulation loop at the same time: `c.is_ascii_digit()` rather than `c.is_numeric()`, and
stop at a second `.`, so the buffer handed to `from_str` can only fail on range.

The `infix` one is the awkward one, because a LALRPOP action cannot return an error without
the grammar being written for it — the file already declares `type Error = Error`, so the
production can return `Err(ParseError::User { .. })`; check how the tokenizer's errors are
already surfaced through that path before inventing a second mechanism.

Fixing the range check needs the answer that `docs/spec/lexical-structure.md` now gives: an
integer literal is guaranteed representable in `-2^31 .. 2^31 - 1`, and beyond that the target
decides. The tokenizer holds `i64`, so `i64` is the bound it can enforce today; a literal that
overflows it is the error.

**Acceptance:** a tokenizer test for each of the three numeric inputs above asserting the
specific `TokenizerErrorType`, and a parser test for `infix left 300`, all four asserting on
the error rather than on `is_err()` alone. Each must have been seen to fail before the fix.

Then add the four blocks to `docs/spec/lexical-structure.md`'s final section — *Numeric
literals the tokenizer cannot represent* — replacing that section's prose with tagged
examples, since a panic currently aborts `cargo test --test spec` for every chapter at once
and is why they are described there rather than shown.
