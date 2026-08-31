# BUG-23 · An `else` does not close a `case` block, so a `case` in a `then` arm is a layout error

**Severity:** medium — a legal shape is rejected outright, and the reported error blames the
`else` rather than anything the reader did wrong.

**Location:** `src/compiler/parser/layout.rs` — the `CaseBlock` context and the tokens that
close it; `src/compiler/parser/grammar.lalrpop` — the `if` alternative of `Expr`, whose arms
are plain `Expr`s with no block structure of their own.

**Problem:** a `case` in the `then` arm of an `if` cannot be followed by its `else`, at any
indentation:

```zel
f c v =
  if c then
    case v of
      On ->
        Off

      Off ->
        On
  else
    On
```

reports a `LayoutError` on the `Else` token, with the offside context naming the `CaseBlock`
opened at column 7. The branch block is closed by a line at a shallower column, and `else` is
such a line — but the layout pass treats it as an ordinary token rather than as one that closes
the enclosing block, so the block is still open when the grammar sees `Else` and expects a
branch pattern.

Indenting the `else` deeper does not help, and indenting it shallower cannot help either: there
is nowhere to put it. The `else` arm has no such problem — a `case` there is closed by the end
of the declaration — so this is the one position in the language where a `case` fits on one
side of a construct and not the other.

Found while writing [`docs/spec/expressions.md`](../spec/expressions.md) (`SPEC-6`), whose
*`case … of`* section specifies that a `case` in a `then` arm ends where the `else` begins.

**Approach:** `else` closes any block opened since the matching `then`. That is the same
question [LANG-21](lang-21.md) asks about `)` and `,` — which tokens close an open `CaseBlock`
— and the two want one answer rather than two special cases, though neither blocks the other.

Note the layout pass has no notion of the `if` construct at all today; it tracks blocks, not
keywords. Whether `else` becomes a block-closing token unconditionally, or only against a
context opened after a `then`, is the judgement call here — the unconditional version is
simpler and is probably right, since `else` cannot legally appear inside an unclosed `case`
branch for any other reason.

**Acceptance:** the example above parses, with a test in the layout pass's own test module
asserting the emitted `CloseBlock`, and one in the parser's asserting the resulting `If` node.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked. The
`expect=parse-error:LayoutError` block in
[`docs/spec/expressions.md`](../spec/expressions.md)'s *`case … of`* section goes red — that
pin's whole job — and is retagged `expect=ok` with its `**Known gap:**` paragraph deleted.
