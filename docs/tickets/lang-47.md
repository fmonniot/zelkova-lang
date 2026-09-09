# LANG-47 · `{` and `}` are not tokens, so nothing in a record reaches the grammar

**Sizing:** small. One file, two token variants, and the test module beside them.

**Location:** `src/compiler/parser/tokenizer.rs` — the `Token` enum, which has `LBracket` and
`RBracket` and no brace beside them, and the character dispatch that emits them.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decision 1):**
records are written in braces, in a type, an expression, an update and a pattern.
[Records](../spec/records.md#the-type) is the rule.

**Not implemented:** `{` reaches the tokenizer's fallback and is reported as
`UnrecognizedToken { tok: '{' }`. The one place a `{` is read at all is the opening of a `{-`
block comment. So every record example fails in the tokenizer before the grammar sees a token,
which is why [Records](../spec/records.md) cites this ticket beside nearly every block it has.

**Approach:** add `Token::LBrace` and `Token::RBrace` and emit them from the single-character
dispatch, the way `LBracket`/`RBracket` are emitted. The `{-` case already runs ahead of that
dispatch and must keep doing so: a `{` followed by `-` opens a comment and is not a brace.

The tokenizer's own test module asserts a token sequence for a punctuation soup input; extend it
rather than adding a second one.

**Watch the block-comment gap.** `{-` is recognised only in a line's leading whitespace today, so
`f = {- a note -} 1` is rejected — [Lexical structure](../spec/lexical-structure.md#comments)
states that and it is not this ticket's to fix. Emitting `LBrace` from the dispatch must not turn
that rejection into a *silent* misread, where `{-` mid-line lexes as a brace and an operator and
the grammar then reports something unrelated to the comment the author wrote.

**Acceptance:** `f = { a = 1 }` fails in the grammar rather than the tokenizer; a tokenizer test
asserts `LBrace`/`RBrace` in a sequence; `f = {- x -} 1` is still rejected and the block comment
tests are unchanged. No block in [Records](../spec/records.md) goes green on this alone — every
one of them still needs a production — so this ticket lands with the spec suite unchanged, and
[`LANG-48`](lang-48.md) is what turns them.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
