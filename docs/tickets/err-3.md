# ERR-3 · Give the parser and canonical ASTs spans, so diagnostics can point at source

**Sizing:** large — a grammar change that reaches every AST node, every construction site and
every parser test. It is the half of `ERR-2` that was deliberately not attempted there.

**Location:** `src/compiler/parser/grammar.lalrpop` (every production), the `parser` AST in
`src/compiler/parser/mod.rs`, the `from_parser*` conversions and the AST in
`src/compiler/canonical/mod.rs`, `canonical::Error`, `typer::Error`,
`CompilationError::as_diagnostic` and the `PhaseError` trait in `src/compiler/mod.rs`,
`tests/compiler/parser/{expressions,types,modules}.rs`.

**Problem:** a `codespan_reporting::Diagnostic` points at source through a `Label`, which needs
a byte range and the `SourceFileId` it belongs to. Only `parser::Error` supplies one today. Its
variants carry `BytePos` and `Spanned<BytePos, Token>` straight off the token stream, so
`parser::Error::diagnostic` builds a labelled diagnostic and a parse failure gets an underlined
caret in the user's file.

Nothing after parsing can do that, and the reason is not the shape of the error types — it is
that **the parser AST has no positions in it at all**. `grammar.lalrpop` never captures
`@L`/`@R`; no `parser::Module`, `Function`, `Match`, `Pattern`, `Expression` or `Type` node has
a span field. Positions are computed by the tokenizer, consumed by the layout pass and the
LALRPOP parser, and dropped on the floor at the moment the AST is built. Canonicalization then
derives its own AST from a span-less one, so it has nothing to propagate.

That is why `ERR-2` (closed 2026-08-26, see [`INDEX.md`](INDEX.md)) stopped where it did.
`ERR-2` settled the half of the question that could be settled without touching the grammar:
every phase error now renders itself as prose through the `PhaseError` trait, and the
`SourceFileId` is attached by `compile_package`, which is the only place that knows which file
a module was read from. What it could not do is give a canonical or typer error a span, because
adding a `span` field to those errors only moves the problem to the construction site, which
has no span to hand it either. `PhaseError`'s doc comment records that decision and points
here.

The user-visible consequence: a type error says

```
error: [Test] type mismatch: expected `Int`, found `Bool`
```

with no file, no line and no caret, in a module that may have a hundred declarations.

**Approach:** this is large enough that it should land in more than one commit, but each
commit must keep `CLAUDE.md`'s standing invariant intact — `grammar.lalrpop`, the `parser` AST
and the `from_parser*` conversions in `canonical/mod.rs` move **together**, in the same commit.
A plausible split by *node kind* rather than by file:

1. **Decide how a span is stored.** Two options, and picking one is the first piece of work.
   Either every AST node gains a `span: Span<BytePos>` field, or nodes are wrapped in the
   existing `Spanned<BytePos, T>` (`src/compiler/position.rs`), which is already what the
   tokenizer produces. `Spanned` is less invasive to write in the grammar but pushes `.value`
   through every match arm in `canonical/mod.rs`; a field is the reverse trade.
2. **Decide what spans do to `PartialEq`.** The parser tests compare whole `Module` values
   against literals built by hand (`tests/compiler/parser/expressions.rs` and `types.rs` each
   have one helper that builds a `Function`). Those helpers cannot know byte offsets, so either
   the tests learn to assert on a span-stripped view, or `PartialEq` is implemented by hand to
   ignore spans. The second is what `syn` does and it is a real trap: two nodes that differ
   only in position compare equal, so a test can no longer pin *where* something parsed.
   Whichever is chosen, write it down at the site.
3. **Start with declarations, not expressions.** `FunBinding` and `FunType` are one production
   each in the grammar and one `Declaration` each; giving those two a span, carrying it into
   `parser::Function` and then into `canonical::Value`, is enough for `typer::Error` to point
   at the declaration that failed to type-check. That is the single highest-value span in the
   compiler and it does not require touching `Expression` at all.
4. **Then expressions and patterns**, which is what turns "this declaration is wrong" into
   "this sub-expression is wrong", and is where the bulk of the work is.
5. **Extend `PhaseError`** — or bypass it the way `parser::Error` does — so a phase error can
   offer `Vec<(Span<BytePos>, String)>` for `as_diagnostic` to turn into `Label`s against the
   `SourceFileId` the driver already attaches.

**Acceptance:** a type error in a `.zel` source renders with a `Label` pointing at the failing
declaration — assert on `diagnostic.labels` in `tests/pipeline.rs`, not just on
`diagnostic.message`, which `ERR-2` already pins. `cargo run` still prints `parsed 8 modules`,
lists all eight as checked and exits 0.
