# BUG-5 · The `Tokenizer` never terminates on a tab used for indentation

**Severity:** medium (a `.zel` file indented with a tab is not exotic input, and any consumer
that keeps polling the tokenizer past an error hangs and grows memory without bound; today's
pipeline is shielded only because the LALRPOP parser happens to stop at the first `Err`)

**Location:** `src/compiler/parser/tokenizer.rs` — `Tokenizer::handle_indentation`, the
`Some('\t')` arm that returns `TokenizerErrorType::TabError`; `Tokenizer::process_next_tokens`,
whose `if self.at_line_start { self.handle_indentation()?; }` re-enters it; and
`Tokenizer::next`, which returns `Some(Err(..))` rather than stopping.

**Problem:** `handle_indentation` loops over the leading characters of a line, consuming them
with `next_char`. Every arm advances the character iterator — except the tab arm, which
returns immediately:

```rust
Some('\t') => {
    // Zelkova forbid the use of tabs for indentation
    return Err(TokenizerError::new(
        self.position.absolute,
        self.position.absolute + '\t'.len_utf8() as u32,
        TokenizerErrorType::TabError,
    ));
}
```

No `next_char`, so `self.lookahead.0` is still `Some('\t')`, `self.position` is unchanged, and
crucially `self.at_line_start` is still `true` — it is only cleared by the arm that reaches the
first real character of the line. The `?` in `process_next_tokens` propagates the error out
before the `while` body can consume anything, and `Tokenizer::next` wraps it as
`Some(Err(..))`. The next call re-enters `process_next_tokens`, sees `at_line_start`, re-enters
`handle_indentation`, reads the same tab, and returns a byte-identical `TabError`. Forever, with
no forward progress.

Reproduced directly on `"a\n\tb\n"` by polling the tokenizer six times (temporary test, not
committed):

```
0 => Some(Ok(Spanned { .. value: LowerIdentifier("a") }))
1 => Some(Err(TokenizerError { .. BytePos(2)..BytePos(3), value: TabError }))
2 => Some(Err(TokenizerError { .. BytePos(2)..BytePos(3), value: TabError }))
3 => Some(Err(TokenizerError { .. BytePos(2)..BytePos(3), value: TabError }))
…
```

Note the asymmetry with the sibling error in the same function. `IndentationError` — returned
after the loop when `spaces % 2 != 0` — is raised only once the spaces have already been
consumed by `next_char` and `at_line_start` has been cleared, so it *does* make forward
progress. On `"a\n b\n"` the same six-poll probe yields `Ok(a)`, one `IndentationError`,
`Ok(b)`, then `None`. So this is specifically the tab arm, not a general property of tokenizer
errors.

Nothing in today's pipeline hits the loop: `parser::parse` (`src/compiler/parser/mod.rs`) feeds
the tokenizer through `layout::layout` into the LALRPOP-generated parser, which stops pulling
from its token iterator at the first `Err`. The existing `invalid_indentation` test in
`tokenizer.rs` also collects into `Result<Vec<_>, _>`, which short-circuits on the first `Err`
and so cannot observe the repetition.

**Found while** reviewing [PR #129](https://github.com/fmonniot/zelkova-lang/pull/129), which
fixed the structurally identical defect one phase later (`BUG-4`, the `Layout` iterator
repeating a `LayoutError` without bound — closed 2026-08-25, see `INDEX.md`). It was
deliberately left unfixed there to keep that diff to one phase; `CLAUDE.md`'s *Standing
invariants* now carries the rule ("an iterator that yields an `Err` must either advance or
stop") and points here.

**Fix:** there is a real choice and this ticket does not make it. Both options are local to
`tokenizer.rs`; do not widen this into `parser::parse` or the general accumulation question,
which is `ERR-2`.

- **Advance past the tab.** Call `self.next_char()` before returning the error, and clear
  `self.at_line_start` (or let the loop continue and report the error once the line's
  indentation has been consumed, mirroring how `IndentationError` is raised after the loop).
  The tokenizer then makes progress and a consumer sees one `TabError` per tab-indented line
  rather than one repeated forever. This matches the sibling error's existing shape and keeps
  the accumulate-and-keep-going design the rest of the file has. Care is needed on the span:
  the error's `start`/`end` are read from `self.position` *before* it moves, so compute them
  first or the reported span shifts — the `invalid_indentation` test pins
  `BytePos(2)..BytePos(3)` for `"  \ta"` and must keep passing.
- **Fuse the iterator on error.** Add a flag checked at the top of `Tokenizer::next`, set in
  the `Err` arm, exactly as `BUG-4` did for `Layout`. This fixes every consumer at one choke
  point and is the smaller diff, but it also fuses `IndentationError`, which currently recovers
  and keeps producing tokens — so it removes working behaviour to fix a broken sibling. If this
  option is taken, `Layout` sits downstream and already fuses, so the two would agree; weigh
  that against losing the multi-error tokenizer output that `ERR-2` is heading toward.

**Acceptance:** a new unit test in `tokenizer.rs`'s existing `#[cfg(test)] mod tests` tokenizes
`"a\n\tb\n"` and polls past the first error with an explicit iteration cap that fails the test
rather than hanging (not `collect::<Result<Vec<_>, _>>()`, which short-circuits and would pass
today). It asserts that the identical `TabError` at the same span is not observable twice in a
row — either the iterator terminated, or it moved on. The existing `invalid_indentation` test
still passes unchanged, including its exact `BytePos` spans. `cargo test` passes in bounded
time and `cargo run` is unaffected (this path is not reached from the real pipeline today, per
Problem).
