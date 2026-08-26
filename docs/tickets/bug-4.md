# BUG-4 · The `Layout` iterator never terminates after a `LayoutError`

**Severity:** medium (not reachable through today's compiler pipeline, but any future consumer
that fully drains the iterator on malformed input hangs and grows memory without bound — this
has already happened once, see Problem)

**Location:** `src/compiler/parser/layout.rs` — `Layout::handle_next_token`'s indentation-rule
check (the `if token.start().column.cmp(min_indent_required) == Ordering::Less` branch that
returns `Err(LayoutError::LayoutError { .. }.into())`), and `Layout::next_token`, which is what
replays the token back out of `reprocess_tokens`.

**Problem:** when a token appears at an indentation column below what its context requires,
`handle_next_token` does this immediately before returning the error:

```rust
if token.start().column.cmp(min_indent_required) == Ordering::Less {
    let offside = offside.clone();
    self.reprocess_tokens.push(token.clone());
    return Err(LayoutError::LayoutError { offside, token }.into());
};
```

The token is pushed onto `reprocess_tokens` and the function returns `Err`. Nothing about
`self.contexts` changed in this branch — the context stack is exactly as it was on entry. The
next call to `Iterator::next` invokes `handle_next_token` again, which calls `next_token`,
which pops that same token straight back off `reprocess_tokens` (see the
`self.reprocess_tokens.pop().map(Ok).unwrap_or_else(..)` at the top of `next_token`). Same
token, same context, same comparison, same `Err`. `Iterator::next` returns `Some(Err(..))`
rather than `None`, so nothing stops a caller from calling it again — and every such call
reproduces the identical error, forever, with no forward progress.

This is not hypothetical. `next_token` and `handle_next_token` are otherwise well aware of this
exact class of problem — a few lines further down, the implicit-block-closing branch pops a
context specifically "to break an infinite loop where we would always be checking the current
token against the current context" (that comment is already in the file). The `LayoutError`
branch is the one place that guard was not applied.

Nothing in today's pipeline hits this: `parser::parse` (`src/compiler/parser/mod.rs`) feeds
`layout::layout(tokenizer)` straight into `grammar::ModuleParser::new().parse(..)`, and the
LALRPOP-generated parser stops pulling from its token iterator on the first `Err` it receives.
That is also why this went unnoticed. But it was found by two independent agents working
`BUG-1` and `PERF-1` (both closed 2026-08-25, see `INDEX.md`) on the same evening, and one of
them reproduced it directly: a temporary diagnostic test (not committed — see `PERF-1`'s PR)
fully drained `layout::layout(..)` over erroring input in order to dump the token stream for a
before/after comparison, hit this loop, and grew a `String` without bound until the process was
killed by the OS after consuming roughly 20GB of RAM — freezing the host machine in the
process. Any future full-drain consumer (a diagnostic tool, a fuzzer, a REPL that wants every
diagnostic in a buffer rather than stopping at the first) reproduces this exactly.

**Fix:** there is a real choice here and this ticket does not make it:

- **Fuse the iterator on error.** Have `Iterator::next` return `None` on any call after it has
  yielded an `Err`, matching how it already fuses on `Token::EndOfFile`. This fixes every
  present and future consumer at the one choke point, at the cost of callers who might have
  wanted to recover and keep going after a `LayoutError` losing that option (nothing in the
  codebase currently relies on that, but `canonical`/`typer` do accumulate past individual
  errors elsewhere, so it's worth asking before foreclosing it here too).
  - **This candidate needs care with the *other* nine `reprocess_tokens.push` call sites**,
    none of which pair with an `Err` return — they all push a token back precisely so a
    *following* `Ok` can be reprocessed as part of normal, successful iteration. A "stop after
    any `Err`" implementation must not disturb the tokenizer-error path handled by
    `next_token`'s `?` propagation either; check that one behaves correctly under whichever
    fuse mechanism is chosen (a `bool` flag checked at the top of `next`, or similar) rather
    than assuming it is unaffected.
- **Make forward progress instead.** Change the `LayoutError` branch to pop the offending
  context (or otherwise mutate `self.contexts`) before pushing the token back, the same way the
  implicit-block-closing branch a few lines below it already does. The iterator then keeps
  going and the parser sees one `LayoutError` per genuinely distinct offside violation instead
  of one repeated forever. This is more consistent with the file's existing accumulate-and-keep-
  going design, but is also more work to get right — get the state transition wrong and the
  loop is merely slower to notice, not fixed.

Either way, **do not** widen this into fixing every place callers currently assume "stop after
the first error is fine" (`parser::parse` today, `ERR-2` more generally) — that's a larger,
separate change already tracked by `ERR-2`. This ticket is about the iterator itself never
being safe to fully drain, independent of who currently chooses not to.

**Acceptance:** a new unit test in `layout.rs`'s existing `#[cfg(test)] mod tests` constructs a
token stream that triggers `LayoutError::LayoutError`, then continues polling the iterator past
that point (e.g. `.take(5).collect::<Vec<_>>()`, or a bounded loop with an explicit iteration
cap that fails the test if exhausted) and asserts iteration terminates or makes forward progress
— it must not be possible to observe the same `LayoutError` twice in a row from the same input
position. `cargo test` passes, completes in bounded time without any `break`-on-error workaround
in the test itself, and `cargo run` is unaffected (this path isn't reached from the real
pipeline today, per Problem).
