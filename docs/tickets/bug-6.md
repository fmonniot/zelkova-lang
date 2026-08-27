# BUG-6 · Rendering a parse error panics for four of `parser::Error`'s five variants

**Severity:** medium — the compiler aborts instead of reporting, on the mistakes users make most.

**Location:** `src/compiler/parser/error.rs` — the catch-all `e => todo!("{:?}", e)` at the end
of `Error::diagnostic`, and the `TokenizerErrorType::StringError` / `UnicodeError` arms inside
the `Error::Tokenizer` branch.

**Problem:** `Error::diagnostic` is the only place in the compiler that builds a labelled
`Diagnostic`, and it handles two of the five `Error` variants. `Layout`, `InvalidToken`,
`UnexpectedEOF` and `ExtraToken` all fall into a `todo!()`.

That is not a latent gap. `compile_package` renders every accumulated parse error through
`CompilationError::as_diagnostic`, whose `Source` arm delegates straight here, so a source with
a mis-indented line reaches the user as a compiler panic rather than as an error message. It is
a live violation of the no-`panic!`/`todo!()` standing invariant, on the path a beginner hits
first.

Two of the four variants have a second, quieter problem: `InvalidToken(BytePos)` and
`UnexpectedEOF { position, .. }` carry a bare `BytePos`, and `BytePos::to_range`
(`src/compiler/position.rs`) returns a **zero-width** `u..u`. Handing that to `Label::primary`
produces a caret with nothing under it, so simply removing the `todo!()` would trade a panic for
an invisible diagnostic.

**Approach:** write a real arm per variant. Every position needed is already in the error
value — `LayoutError::LayoutError` carries a `Spanned` token plus the offside column,
`ExtraToken` carries a `Spanned` — so nothing upstream has to change and this ticket does not
depend on `ERR-3`. Give the two point-position variants a non-empty range rather than the
zero-width one, and say at the site why.

**Acceptance:** `grep 'todo!' src/compiler/parser/error.rs` is empty. One test per
newly-handled variant, each asserting a prose message *and* a label whose range is non-empty
(`start < end`) — the zero-width range is half of what is being fixed, so a test that only
checks `!labels.is_empty()` would not pin it. `cargo run` still prints `parsed 8 modules`, lists
all eight as checked, exits 0.
