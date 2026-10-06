# ERR-23 · An unknown constructor in a pattern is underlined with its arguments

**Sizing:** small. One span reaches one diagnostic. It is larger under the first approach
below, which changes the shape of a parser AST node every phase after it reads.

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `Pattern::from_parser`'s
`Constructor` arm, which builds `Error::VariantNotFound` with `p.span`;
`crates/zelkova-syntax/src/parser/mod.rs` — `PatternKind::Constructor(Name, Vec<Pattern>)`,
which holds no span for the name; `docs/spec/patterns.md` — [*Qualified
constructors*](../spec/patterns.md#qualified-constructors), the sentence "A constructor that is
not in scope is reported as such, with the caret under the name".

**Problem:** the only span a constructor pattern has is the whole pattern's, so the caret for a
constructor that does not resolve covers its arguments as well:

```
error: [App] 2 canonical errors
   ┌─ scratch-caret:src/App.zel:10:5
10 │     Circl n ->
   │     ^^^^^^^ no type constructor of this name is in scope — did you mean `Circle`?
   ·
13 │     Circle (Circl m) ->
   │            ^^^^^^^^^ no type constructor of this name is in scope — did you mean `Circle`?
```

The chapter's sentence holds for a nullary constructor, which is what its block writes, and for
no other. An unknown constructor in an *expression* is already underlined alone, because
`ExpressionKind::TypeConstructor` is a node of its own and its arguments belong to the
application around it.

**Approach:** the ticket does not choose between two shapes.

1. Give the name a span in the parser AST, so `PatternKind::Constructor` holds the name with its
   position and `Pattern::from_parser` passes that to `Error::VariantNotFound`. Exact in every
   position. It is a change to a node `canonical/mod.rs` and the parser tests all match on.
2. Derive the name's span from the pattern's: it starts where the pattern does and is as long
   as the name's text, which holds no whitespace. This needs nothing from the grammar, and it is
   wrong wherever a pattern's span starts before its name, which today is a parenthesised
   applied constructor. [`LANG-89`](lang-89.md) removes that case, so this shape depends on it.

**Depends on:** [`LANG-89`](lang-89.md), soft, and only for the second approach.

**Acceptance:** a test in `crates/zelkova-compiler/tests/canonical.rs` asserts that
`diagnostic.labels[0].range` for `Circl n ->` and for `Circle (Circl m) ->` is the text `Circl`
alone, and for `S.Circl n ->` the text `S.Circl`, each seen red against today's span.
`cargo test --workspace` is green.

**Found:** deciding the span question [`LANG-89`](lang-89.md) had left open. Left unfixed there
because it is a diagnostics change and that ticket is a grammar one.
