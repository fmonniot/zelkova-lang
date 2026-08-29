# BUG-19 · A line whose first token starts with `-` leaves the tokenizer measuring indentation mid-line

**Severity:** medium (valid source is rejected, with an error naming a rule the line does not
break — and whether it is rejected at all depends on the parity of a run of spaces).

**Location:** `src/compiler/parser/tokenizer.rs` — `handle_indentation`, the `Some('-')` and
`Some('{')` arms:

```rust
Some('-') => {
    if let Some('-') = self.lookahead.1 {
        self.consume_comment()?;
        spaces = 0;
    } else {
        break;          // <- `at_line_start` is never cleared
    }
}
```

**Problem:** `handle_indentation` clears `self.at_line_start` in exactly one place — the `_`
arm, "we arrived at the first character of the line". The two arms above reach the same
conclusion for `-` and `{` (a lone `-` is not a comment, so it is the line's first real
character) and `break` without clearing the flag.

`process_next_tokens` re-enters `handle_indentation` before every `consume_char` while the
flag is set, so after the `-`-initial token is emitted the tokenizer scans the run of spaces
*following it* as though that run were the line's leading whitespace — and applies the
two-space rule to it ([Layout](../spec/layout.md#indentation-is-measured-in-two-space-levels)).

So a multi-line type annotation whose continuation begins with `->` is rejected:

```zel
f : Int
  -> Int
f a = a
```

`Tokenizer(IndentationError)`, with the caret on the single space between `->` and `Int`. The
same shape appears in an expression continued with a leading operator:

```zel
f =
  1
  - 2
```

The parity dependence is what makes this unmistakable rather than arguable. Probed:

| Continuation line | Spaces after the leading token | Result |
|---|---|---|
| `  ->Int` | 0 | accepted |
| `  -> Int` | 1 | `IndentationError` |
| `  ->  Int` | 2 | accepted |

Nothing about the language distinguishes those three. Only the parity of a run of spaces in
the middle of a line does, which is the rule this code was applying to the wrong text.

The `Some('{')` arm has the identical shape. It has no observable consequence today only
because `{` is not a token the tokenizer knows — see [BUG-13](bug-13.md) — so fix both arms
together rather than leaving one to surface when block comments are.

Found while writing [`docs/spec/types.md`](../spec/types.md) (`SPEC-5`), whose *An annotation
may span several lines* section carries the tagged block.

**Fix:** set `self.at_line_start = false` before `break` in both arms, matching the `_` arm.
The `Some('\t')` arm already does this for the same reason, and says so (`BUG-5`).

Check that the fix does not disturb the count `handle_indentation` returns. The caller
(`process_next_tokens`) discards it today, so nothing observable depends on it, but the two
`break`s leave `spaces` holding the run counted so far and a later change might start reading
it.

**Acceptance:** a tokenizer unit test asserting that `f : Int\n  -> Int\nf a = a\n` tokenizes,
beside the existing `invalid_indentation` test — and, since the parity is the tell, that it
does so for one, two and three spaces after the `->` alike. The `**Known gap:**` block in
[`docs/spec/types.md`](../spec/types.md) tagged `expect=parse-error:IndentationError` goes red
and is retagged `expect=ok` with its paragraph deleted.
