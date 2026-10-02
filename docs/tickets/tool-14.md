# TOOL-14 · A declaration a failed chunk shares a name with is spanned over everything between them

**Sizing:** small. One representation to pick, one field's worth of change, and a test to
update. It becomes medium if the choice is to carry several spans, since `ir::Unchecked` and
every reader of `Broken::span` then change shape.

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `Rejected::unparsed`,
`unparsed_values`, `Broken::span`; `crates/zelkova-compiler/src/ir/mod.rs` — `Unchecked::span`,
which `ir::build` copies from `Broken::span`; `crates/zelkova-syntax/src/position.rs` —
`NodeSpan::merge`.

**Part of:** the *Active work: editor support* section of [the index](README.md); read before
[`TOOL-6`](tool-6.md) starts hover or go-to-definition.

**Problem:** [`DEC-23`](../decisions/dec-23.md) decision 4 names a value after a failed chunk
that opens on its lowercase identifier, and `canonicalize_recovering` records a function that
such a chunk names as `Broken`. `Rejected::unparsed` sets the `Broken`'s span to the parsed
`Function`'s span merged with the chunk's, and `unparsed_values` has already merged the spans of
every failed chunk of one name. `NodeSpan::merge` is the smallest span covering both operands,
the minimum start and the maximum end. So when the chunk naming `f` sits below `f`'s annotation
with other declarations between them:

```
f : Int -> Int

g : Int
g = 1

f x = = x
```

Under `module Test exposing (f, g)`, `f`'s span is bytes 29..70 and `g = 1` starts at byte 53.
`f`'s `Broken::span`, and so the `Unchecked::span` that `ir::build` copies from it, runs from the
annotation to the end of the failed binding and encloses `g`. Checked with `module Test
exposing (f, g)` above that source: `f`'s span is bytes 29..70 and `g = 1` starts at byte 53. Two failed chunks of one name do
the same, with whatever lies between them. Nothing reads these spans by position today, so
nothing is wrong yet. [`TOOL-6`](tool-6.md)'s hover and go-to-definition look up the innermost
node at an offset, and a span that contains other declarations will answer for them.

`a_function_named_by_a_failed_chunk_is_spanned_over_the_chunk_too` in
`crates/zelkova-compiler/tests/canonical.rs` pins the merge as it stands, adjacent
declarations only, so it does not decide the shape.

Found in review of the `TOOL-11` PR, and left there: the ticket makes `Broken` carry the
chunk, and the review's point is about what a reader of the span then sees.

**Approach:** the ticket does not pick between these.

1. Keep the parsed `Function`'s span (and `annotation_span`) as `Broken::span`, and carry the
   failed chunks' spans apart, as a second field on `Broken` and on `Unchecked`. A positional
   lookup then finds each piece where it was written. It costs a field, and a reader wanting
   "everything that declares `f`" has to join them.
2. Keep one merged span and document that it may enclose other declarations, with the
   position-to-node lookup `TOOL-6` builds expected to prefer the smallest span containing an
   offset. It costs nothing now and puts the burden on every reader.

**Acceptance:** whichever is chosen, a test in `crates/zelkova-compiler/tests/canonical.rs`
canonicalizes the source above and asserts on the `Broken`'s span or spans against byte offsets
of `f`'s annotation and of the failed binding, and `g`'s declaration lies inside none of them
under option 1; it turns red when `Rejected::unparsed` merges the spans again. The doc comment
on `Broken::span` says what the span covers. `cargo test --workspace` is green and
`cargo run -- compile std/core` still prints `parsed 10 modules`.
