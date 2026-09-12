# BUG-28 · The `Tokenizer` never terminates on an unterminated character literal

**Severity:** low. `parser::parse` is not affected — `Layout` fuses on the first `Err`, so the
error is seen once and the pipeline stops — which is the only reason this is not the same
incident as BUG-4. Any other consumer of the raw `Tokenizer` iterator gets the identical error
forever.

**Location:** `src/compiler/parser/tokenizer.rs` — `consume_char`, the `'\''` arm. Both of its
error returns are affected: the `(Some(v), Some(closing))` arm
(`CharNotClosedError(Some(closing))`) and the `(v, _)` arm (`CharNotClosedError(None)`). Their
fixed siblings are the two `'\t'` arms — the one in `handle_indentation` (BUG-5) and the one in
`consume_char`'s own main loop (BUG-11) — both of which now call `next_char()` before returning
and say why in a comment.

**Found:** during review of the PR that landed BUG-11, while checking whether the never-advance
shape appeared anywhere else in the same `match c`. Left unfixed there because the fix has a
design choice in it (below) and BUG-11's diff was a single arm.

**Problem:** neither arm consumes anything before returning. `self.lookahead` still holds the
opening quote and `self.position` has not moved, so the next poll re-enters `consume_char`,
matches `'\''` again on the same character, and returns a byte-identical error:

```rust
(Some(v), Some(closing)) => {
    // We haven't moved the cursor yet, but we know where
    // the error is, so we build the position manually
    let end = self.position.absolute
        + (v.len_utf8() as u32)
        + (closing.len_utf8() as u32);
    return Err(TokenizerError::new(
        self.position.absolute,
        end,
        TokenizerErrorType::CharNotClosedError(Some(closing)),
    ));
}
```

The comment on that arm — "we haven't moved the cursor yet" — is the defect stated out loud.
Verified by polling each of the three shapes 50 times past the first error; none terminates and
every item is identical:

| Source | Every item, forever |
|---|---|
| `'` | `CharNotClosedError(None)` at `BytePos(0)..BytePos(1)` |
| `'a` | `CharNotClosedError(None)` at `BytePos(0)..BytePos(2)` |
| `'ab` | `CharNotClosedError(Some('b'))` at `BytePos(0)..BytePos(2)` |

The `literal_char` test already covers all three inputs and does not catch it, because it uses
`collect::<Result<Vec<_>, _>>()`, which short-circuits on the first `Err` and so can never
observe a repeat — the same reason `invalid_indentation` missed BUG-5 and `refuse_tab_in_expression`
missed BUG-11.

This is the class of defect `CLAUDE.md`'s standing invariant *A `Result`-yielding iterator must
advance or stop — never repeat one error* exists to prevent; fully draining the one in BUG-4
consumed ~20GB before the OS killed it.

**Fix:** make both arms advance, mirroring the two tab arms. **How far to advance is a real
choice and this ticket does not pick one:**

1. **Consume only the opening quote.** `'ab` then yields the error followed by the identifier
   `ab`; `'` yields the error followed by `EndOfFile`. Smallest change, and it recovers the way
   a reader would — the quote was the typo, the rest of the line is real source.
2. **Consume the opening quote and the characters the arm inspected** (`v`, and `closing` where
   it exists). `'ab` then yields the error and nothing else. Treats the whole malformed literal
   as consumed, so no fragment of it is re-tokenized as something it was not.

Either terminates, which is what this ticket is about; they differ only in what follows the
error.

**Whichever is chosen, check the span before changing it.** Both arms compute their `end` from
character widths rather than reading `self.position` after advancing, which is the pattern the
tab arms deliberately moved away from. Switching to two observed positions is the consistent
thing to do, but it would change the reported span, and there is now a reader of that span:
`src/compiler/parser/error.rs`'s `CharNotClosedError(Some(_))` arm renders `span.start` as the
opening quote and `span.end` as where the closing quote should have been, and says so in a
comment citing this arm by name (BUG-7, closed 2026-09-11). Move the span only together with
that comment and the label placement it justifies. `literal_char` in `tokenizer.rs` pins all
three spans today and would need updating too.

**Acceptance:** a test in `tokenizer.rs`'s `mod tests`, using the drain-and-inspect helper the
two `*_does_not_hang` tests share, that polls each of `'`, `'a` and `'ab` past the first error
and asserts the iterator terminates within the cap, that no two consecutive items are the same
error, and that exactly one error is raised per source. Neutralise the fix — remove the
`next_char()` call — and confirm the test goes red before keeping it.
