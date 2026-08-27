# ERR-3 · Give the parser and canonical ASTs spans, so diagnostics can point at source

**Sizing:** large — a grammar change that reaches every AST node, every construction site and
every parser test. It is the half of `ERR-2` that was deliberately not attempted there.

**Role:** the foundation of the diagnostics program — see
[`INDEX.md`](INDEX.md#the-diagnostics-program). `ERR-4`, `ERR-5` and `ERR-7` all wait on it.

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

## Decisions

The two questions this ticket originally left open are settled. Both were real trades and both
belong in the code as comments once this lands.

1. **A `span` field, not a `Spanned<BytePos, T>` wrapper.** Struct nodes take the field
   directly. The three recursive *enums* — `Expression`, `Pattern`, `Type` — become
   `struct X { span: NodeSpan, kind: XKind }`, which is the same choice extended to enums. It
   beats wrapping because children stay `Box<Expression>` rather than
   `Box<Spanned<BytePos, Expression>>`, so `canonical/mod.rs` reads `match &e.kind` once per
   function instead of `.value` at every child.
2. **`PartialEq` ignores spans**, via a `NodeSpan` newtype whose `PartialEq` always returns
   `true`. This is the `syn` trap the ticket originally warned about and it is being walked into
   knowingly: the parser tests compare whole `Module` values against literals built by hand,
   those literals cannot know byte offsets, and the alternative — a hand-written span-stripping
   traversal — silently weakens every test the day someone forgets a node in it. The cost is
   that a whole-value `assert_eq!` can no longer pin *where* something parsed, so **at least one
   test must assert on `.span` directly**; without it the entire span plumbing is
   green-but-unverified. Keeping the blindness inside one newtype leaves `Span` and `Spanned`
   with real equality, so tokenizer, layout and parser-error tests are untouched.

**Approach:** two commits, each keeping `CLAUDE.md`'s standing invariant intact —
`grammar.lalrpop`, the `parser` AST and the `from_parser*` conversions in `canonical/mod.rs`
move **together**, in the same commit.

1. **Declarations first.** `FunType`, `FunBinding`, `Import`, `Union` and `Infix` are one
   production each; carrying their spans into `parser::Function` and then `canonical::Value` is
   enough for `typer::Error` to point at the declaration that failed to type-check. That is the
   single highest-value span in the compiler and it does not require touching `Expression`.
   Capture `@L`/`@R` *inside* each production rather than around the `Decl` wrapper, so the span
   covers the user's text and not the layout-injected `OpenBlock`/`CloseBlock`, which are
   emitted zero-width.
   Landing with it: `PhaseError` gains a defaulted `labels()` returning a `SpanLabel`
   (`{ span, message, primary }` — the primary/secondary distinction is needed *within* one file
   for "expected because of this annotation", not only across files), and `compile_package`
   pairs those spans with the `SourceFileId` it already has. The file id stays out of the
   phases, exactly as `ERR-2` settled.
2. **Then expressions, patterns and types**, which is what turns "this declaration is wrong"
   into "this sub-expression is wrong", and is where the bulk of the work is. Watch the two
   productions that synthesise nodes not present in the source — the `-` prefix desugaring and
   the `InfixExpr` rewrite — and give them the operator's span rather than none, so a diagnostic
   on desugared code still lands somewhere real.

## What this ticket does not do

Naming these keeps the scope honest and keeps a reviewer from expecting them.

- **`canonical::Type` and `TypeConstructor` get no span.** They are cloned out of
  `Environment`/`Interface`, so a type reaching this module may have been written in a
  *different file*, and a file-less span would name the wrong source. The fix is a
  `(SourceFileId, Span)` pair on the interface, which is `ERR-5`. Write the reasoning at the
  type definition.
- **Type errors still point at the declaration, not the sub-expression.** The typer translates
  canonical into its own `Term`/`Constraint` language and drops positions on the way, so
  declaration granularity is the ceiling regardless of how good the canonical AST gets. That is
  `ERR-4`.
- **Suggestions** on unresolved names are `ERR-7`; this ticket only gives them something to
  attach to.

**Acceptance:** a type error in a `.zel` source renders with a `Label` pointing at the failing
declaration — assert on `diagnostic.labels` in `tests/pipeline.rs`, not just on
`diagnostic.message`, which `ERR-2` already pins, and assert the label's **range**, since a
zero-width or wrong-node span would satisfy a mere `!labels.is_empty()`. A canonicalization
error (an unknown variable, an import of a module that does not exist) likewise renders with a
label. At least one parser test asserts a `.span` value directly. `cargo run` still prints
`parsed 8 modules`, lists all eight as checked and exits 0.
