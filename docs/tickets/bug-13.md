# BUG-13 · Block comments are lexed only at the start of a line, swallow the rest of their closing line, do not nest, and are accepted unterminated

**Severity:** medium. Two of the four silently discard source the author wrote.

**Location:** `src/compiler/parser/tokenizer.rs` — `consume_comment`'s `{-` branch, and
`consume_char`'s `match` on the current character, which has no arm for `{`.

**Found:** while writing `docs/spec/lexical-structure.md` under `SPEC-2`.

**Problem:** four defects in one small piece of code. Each has a `**Known gap:**` block in
[the Comments section](../spec/lexical-structure.md#comments) that goes red when it is fixed.

1. **Only recognised in leading whitespace.** `handle_indentation` tests for `{` followed by
   `-` and calls `consume_comment`; `consume_char`, which handles every character *after* a
   line's first token, has no such arm and falls through to its catch-all. So
   `f = {- a note -} 1` fails with `UnrecognizedToken { tok: '{' }`, naming a character that
   is not the problem. `--` comments do not have this defect: `consume_char` has a `'-'` arm
   that checks for a second hyphen.

2. **The closing `-}` discards the rest of its line.** On finding `-}` the branch calls
   `self.skip_end_of_line()` before breaking. So `{- a note -} f =` becomes nothing at all,
   and a body indented beneath it is left with no declaration to attach to. This is silent
   code loss: the author sees a confusing error one line later, or — if what was swallowed
   happened not to matter — no error at all.

3. **No nesting.** The loop breaks at the first `-}` whatever came before it, so an outer
   comment containing an inner one ends early. Combined with (2), whether that is even
   visible depends on what follows on the line, which is why the spec's example spreads the
   comment over several lines to make the failure observable.

4. **Unterminated is accepted.** `None => break` ends the loop at end-of-file with no error,
   so a stray `{-` comments out the remainder of the file and the module compiles without it.

**Decided (SPEC-2, by the language owner):** a block comment may appear anywhere a space may;
it ends exactly at its matching `-}` and the rest of that line is ordinary source; block
comments nest; and reaching end-of-file inside one is an error.

**Approach:** give `consume_char` a `'{'` arm mirroring its existing `'-'` one, so both
comment forms are recognised in both positions, and rewrite `consume_comment`'s block branch
around a depth counter — `+1` on `{-`, `-1` on `-}`, done at zero — dropping the
`skip_end_of_line()` call so the tokenizer resumes at the character after `-}`. Reaching
`None` with a non-zero depth returns a new `TokenizerErrorType`, spanned at the `{-` that
opened the outermost unclosed comment, since that is the character the author has to go fix.

Watch the interaction with `handle_indentation`: it calls `consume_comment` and then resets
`spaces = 0` and keeps scanning, which is correct for a comment that ends a line and wrong
for one that does not. After this change a leading `{- x -} f = 1` must leave the loop with
`at_line_start` cleared and the indentation already counted, or the offside rule will measure
the wrong column. That case has no test today; it needs one.

Careful with (1) and the existing `'-'` arm together: `{-` is not made of operator characters
(`{` is in neither `is_operator_char` nor `is_identifier_start`), so no maximal-munch
interaction has to be untangled — the new arm can be unconditional on the two-character
lookahead the struct already keeps.

**Acceptance:** four tokenizer or parser tests, one per defect, each seen to fail first:
an inline block comment mid-expression; code after `-}` on the same line still parsing; a
nested comment; and an unterminated comment reporting an error whose primary label is on the
opening `{-`. Then retag the four blocks in
[`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s *Comments* section — three
become `expect=ok` and the unterminated one becomes a `parse-error` — and delete the
`**Known gap:**` paragraph under each, which is what the red tests will force anyway.

`cargo run` must still print `parsed 8 modules` and exit 0. `std/core/src/` uses `{-| … -}`
doc comments heavily, all of them at the start of a line with nothing after the `-}`, so
they exercise (2)'s code path with an empty tail; that is the regression to watch for.
