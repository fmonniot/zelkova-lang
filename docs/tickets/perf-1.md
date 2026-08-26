# PERF-1 · Reduce cloning in the `Layout` iterator

**Sizing:** small-to-medium — eleven call sites, but the borrow-checker shape is the reason it
was left alone, so budget for one restructuring that does not work.

**Location:** `src/compiler/parser/layout.rs` — the `Layout` struct's own doc comment asks for
this; the clones are in `handle_next_token` and the `Iterator::next` impl below it.

**Problem:** the author flagged it on the struct itself:

> **TODO**: I really need to use references with this structure. There are too many `clone()`
> call on something which is part of the core loop. Let's do so once I have a somewhat working
> algorithm.

The algorithm has been working for a while. There are eleven `clone()` calls in the file. They
fall into three groups, and only two are worth attention:

- **Nine `self.reprocess_tokens.push(token.clone())`** in `handle_next_token`. Each clones a
  `Spanned<Position, Token>` in order to both return it and stash it for reprocessing. This is
  the hot one — it runs per token, and `Token` owns a `String` for every identifier and
  operator in the file.
- **Two `offside.clone()`**, where the value is cloned out of `self.contexts` because the
  following code needs `&mut self`. This is the borrow-checker workaround the TODO is really
  about, and it is what makes the fix a restructuring rather than a find-and-replace.
- **`pos.clone()` twice** in `next`. `Position` is three `usize`s; these should just be
  `Copy`. Adding `#[derive(Copy)]` to `Position` in `src/compiler/position.rs` removes both and
  is nearly free — do that part first and separately, it is worth landing even if the rest
  stalls.

**Approach:**

1. Derive `Copy` on `Position` and drop the two `pos.clone()` calls. Check nothing else relied
   on the move semantics.
2. Restructure `handle_next_token` so it does not need `&mut self` while reading
   `self.contexts`. The usual shapes: read what is needed out of `contexts` into locals up
   front (arity and indentation column are small `Copy` values even though `Offside` is not);
   or split the read into a small `&self` helper called before the mutation begins.
3. With that done, revisit the `reprocess_tokens` pushes — several may be able to move the
   token rather than clone it, returning a value reconstructed from the buffer instead.

**This is an optimisation with no behavioural change, so the bar is that it changes nothing.**
The layout pass has five unit tests in the file and is exercised by every parser test; they are
the whole safety net. Do not "simplify" a rule while in here.

**Acceptance:** the count of `clone()` in `layout.rs` is materially down (the nine
`reprocess_tokens` pushes are the target; getting only the `Position` ones is not enough to
close this), the struct's TODO comment is removed or rewritten to describe what is actually
left, and `cargo test` passes unchanged. `cargo run` still reports seven parsed modules — a
layout regression shows up there as a parse error, not as a wrong answer.
