# LANG-82 · A character literal recognises no escape sequence

**Sizing:** small. `consume_escape` already decodes the table for strings, so the tokenizer
half is a reuse. What could make it bigger is [`LANG-80`](lang-80.md)'s open rules for what an
unknown or surrogate escape means.

**Part of:** no active program. Noticed while reviewing [`LANG-77`](README.md), which added
`consume_escape` for string literals and deliberately left character literals alone.

**Location:** `src/compiler/parser/tokenizer.rs` — `consume_char`'s `'\''` arm, which matches
exactly `'` + one character + `'` on a fixed three-character lookahead;
`consume_escape`, the string-side decoder; `CharNotClosedError`.
`docs/spec/lexical-structure.md` — [*Characters*](../spec/lexical-structure.md#characters),
its `expect=unimplemented` example (`newline = '\n'`) and its **Not implemented:** paragraph.

**Problem:** the escape table in *Characters* applies to character literals and to strings,
but only strings decode it. A character literal is read as three characters of lookahead, so:

- `'\n'` is rejected with `CharNotClosedError`, because the third character is `n`, not a
  closing quote. Likewise `'\u{1F600}'`, `'\''` and `'\"'`.
- `'\'` is *accepted* as the backslash character, since it is quote, `\`, quote. Once escapes
  are recognised it is an unclosed literal, so the fix changes what an input that lexes today
  means.

**Approach:**

1. In the `'\''` arm, when the character after the opening quote is a backslash, decode it with
   `consume_escape` (or a shared helper; it currently takes the position of a string's opening
   quote to report an unclosed escape against) and then require the closing quote.
2. Keep every error path consuming input, as the two existing `CharNotClosedError` returns do
   (BUG-28, closed): each consumes the opening quote before returning, and the comment above
   them says why and what their span means to `error.rs`.
3. Follow whatever [`LANG-80`](lang-80.md) decides for an unknown escape, a surrogate and a
   `\u{…}` digit count. Until it does, a character literal inherits `consume_escape`'s current
   answers, which the strings already rely on.
4. Flip the `newline = '\n'` example to `expect=ok`, cover the rest of the table in a second
   example, and delete the **Not implemented:** paragraph. Add a tokenizer test per escape and
   one for `'\'`.

**Acceptance:** `cargo test --test spec` is green with `'\n'` under `expect=ok`;
`cargo test --workspace` is green; a tokenizer test asserts `'\n'` yields `Token::Char` holding
a line feed and `'\'` is rejected, and goes red if the `'\''` arm stops calling the escape
decoder.
