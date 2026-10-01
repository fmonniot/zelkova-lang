# LANG-79 · Multi-line `"""` string literals are specified but not tokenized

**Sizing:** small-to-medium. The tokenizer path is a sibling of `consume_string`, and nothing
past the tokenizer changes: a multi-line string produces the same `Token::String` the
single-line form does. Two things could make it bigger. One is the spec question in the
Problem, which has to be settled before the tokenizer can be written. The other is that the
literal spans lines, so it crosses the tokenizer's line-start bookkeeping and the layout pass.

**Part of:** no active program. Split out of [`LANG-77`](README.md), which landed the
single-line form and left this one out because the chapter does not yet say what value the
multi-line form has.

**Location:** `crates/zelkova-syntax/src/parser/tokenizer.rs` — `consume_char`'s `'"'` arm and
`consume_string`, whose doc comment says what a `"""` reads as today. `docs/spec/lexical-structure.md`
— [*Strings*](../spec/lexical-structure.md#strings), whose multi-line example is tagged
`expect=unimplemented` and whose **Not implemented:** paragraph cites this ticket.

**Problem:** [*Strings*](../spec/lexical-structure.md#strings) specifies a multi-line string,
delimited by `"""`, that may contain line endings and unescaped double quotes and uses the
same escapes as the single-line form. The tokenizer does not recognise it. `consume_string`
reads `"""` as the empty string `""` followed by the opening quote of a second single-line
string, and the line ending after it leaves that string unclosed, so the chapter's `poem`
example is rejected with `StringNotClosedError`.

The chapter does not say what a multi-line string's value is, and its own example depends on
the answer:

```
poem =
  """
  one
  two
  """
```

- Whether the line ending right after the opening `"""` belongs to the value.
- Whether the leading spaces on each line (here, the two spaces of layout indentation) belong
  to it, or are stripped up to some column: the closing `"""`'s, the least-indented line's,
  or none.
- Whether the line ending before the closing `"""` belongs to it.

This ticket does not pick. A decision entry (or the chapter itself) settles it first, per
`docs/spec/conventions.md`'s *A spec change and a semantics change do not share a diff*.

**Approach:**

1. Settle the three questions above in `docs/spec/lexical-structure.md` (and a `docs/decisions/`
   entry if an alternative is worth recording), in a change of its own.
2. In `consume_char`, recognise `"""` before the single-line `"` arm and read up to the next
   `"""`, reusing `consume_escape` for backslashes. A line ending inside is part of the
   literal; end of file before the closing `"""` is an unclosed-string error spanned from the
   opening delimiter.
3. The tokenizer tracks `at_line_start` and the line and column in `next_char`, and
   `handle_indentation` measures leading spaces. A literal that spans lines must not have its
   interior lines measured as indentation. Check that `next_char` setting `at_line_start` on a
   `\n` inside the literal does not trigger `handle_indentation` on the next poll, and that the
   layout pass (`crates/zelkova-syntax/src/parser/layout.rs`), which reads token positions, does not treat
   the token after the literal as starting a new line.
4. Change the chapter's multi-line example to `expect=ok`, remove its **Not implemented:**
   paragraph, and delete this ticket.

**Tests:** in `tokenizer.rs`'s tests, a multi-line literal's value for the chapter's `poem`
shape, one containing an unescaped `"`, one containing an escape, an unclosed one at end
of file, and `"""hi"""` written on one line, which today tokenizes as the three strings `""`, `"hi"`
and `""` and so reaches the typer as an application of a `String`. A `crates/zelkova-compiler/tests/typer.rs` case that a binding to one infers `String`. A test that the
declaration after a multi-line literal still parses at the right indentation. Mutation-check
each one.

**Acceptance:** the multi-line example in [*Strings*](../spec/lexical-structure.md#strings)
is `expect=ok`, and `cargo test --test spec` and `cargo test --workspace` are green.
