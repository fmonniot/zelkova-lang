# BUG-7 · The unclosed-char diagnostic draws two invisible carets, and swaps their messages

**Type:** bug. **Severity:** low — one tokenizer error variant, on an input that is already a
typo. It renders rather than panicking, so it is a quality defect, not a crash.

**Location:** `src/compiler/parser/error.rs`, the `TokenizerErrorType::CharNotClosedError(Some(_))`
arm of `Error::diagnostic` (lines 62–73).

**Problem:** two defects in six lines, both pre-existing and both the same class as `BUG-6`.

1. **Both labels are zero-width.** The arm calls `BytePos::to_range()` on
   `err.error.span.start` and `err.error.span.end`, and `BytePos::to_range` is `u..u` by
   construction. codespan-reporting renders such a range as a caret with no character above it,
   which is exactly the failure `BUG-6` fixed everywhere else in this `match`. The helpers that
   fix it — `one_byte_at` and `non_empty` — now live two screens below in the same file, and
   are already applied to `Layout`, `ExtraToken`, `InvalidToken` and `UnexpectedEOF`.

2. **The two messages are transposed.** The tokenizer builds this error with
   `start = self.position.absolute` (the *opening* quote) and `end = start + 1 + 1` (where the
   closing quote should have been) — see `tokenizer.rs:608-620`, and the `'aa` case in its own
   tests, which pins `BytePos(0)..BytePos(2)`. The arm then puts "We were expecting a single
   quote here" on `start` and "For the opening quote here" on `end`. Both are the wrong way
   round: `start` is the opening quote, `end` is where the closing one was expected.

Together, a user who writes `'ab` gets two carets pointing at nothing, each captioned as the
other one.

**Fix:** run both ranges through `one_byte_at`, and swap the two messages so the secondary
label ("for the opening quote here") sits at `span.start` and the primary ("we were expecting a
single quote here") at `span.end`. The commented-out sketch at the top of the `Error::Tokenizer`
arm already describes that arrangement — `Label::primary((), 2..2)` for the expected quote,
`Label::secondary((), 0..0)` for where the char started — so this is restoring the intent, not
inventing one.

**Acceptance:** a test in the style of the `BUG-6` ones in `error.rs` — tokenize `'ab`, render
the diagnostic, and assert both labels have non-empty ranges *and* that the label at the
opening quote is the one that mentions the opening quote.
