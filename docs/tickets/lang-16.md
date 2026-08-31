# LANG-16 · A constructor pattern may not nest, and may not be parenthesised in a `case` branch

**Sizing:** small in the grammar, but it is a grammar change, so `CLAUDE.md`'s *A grammar
change is never a one-file change* applies. The parser AST already represents what is missing,
so the change is confined to `grammar.lalrpop` plus tests.

**Location:** `src/compiler/parser/grammar.lalrpop` — the three pattern productions,
`Pattern`, `CasePattern` and `DeclPattern`.

**Decided ([`docs/spec/patterns.md`](../spec/patterns.md), *Patterns nest*):** every pattern
position takes a whole pattern, so patterns nest to any depth. An applied constructor written
as a sub-pattern is parenthesised, because juxtaposition inside a constructor pattern already
separates one argument from the next; a nullary one needs no parentheses anywhere.

**Problem:** `Pattern` — the production used for every *sub*-pattern position, and for the
parenthesised group — has alternatives for `_`, a variable, a literal, `( Pattern )`, and the
two tuple arities. It has no constructor alternative at all. `CasePattern` and `DeclPattern`
each add constructor alternatives on top of it, but their arguments are `Pattern*`, so the
constructor forms never reach a nested position.

Three consequences, each a syntax error today:

```zel
case pair of
  (On, On) -> On         -- a nullary constructor as a tuple element
  _ -> Off

case w of
  Wrapper (Circle n) -> n    -- an applied constructor as a constructor argument
  _ -> One

case shape of
  (Circle n) -> n            -- a parenthesised constructor heading a case branch
  Dot -> One
```

The third falls out of the same cause: `( … )` in a pattern is `Pattern`'s own grouping
alternative, so it admits exactly what `Pattern` admits.

`grammar.lalrpop` already flags the area as unfinished — "I'm sure we are missing quite a bit
of legal syntax, so I'll need to go back on that later on" sits directly above `DeclPattern`.

Found while writing [`docs/spec/patterns.md`](../spec/patterns.md) (`SPEC-7`).

**Approach:** give `Pattern` a nullary-constructor alternative (`QualTypeIdent` with no
arguments) and a parenthesised-applied-constructor alternative
(`"(" QualTypeIdent Pattern* ")"`), which is what `DeclPattern` already carries — at which
point `DeclPattern` becomes `Pattern` and can go away, and `CasePattern` keeps only the bare
applied form that a branch head allows. Expect LALRPOP to report an ambiguity between the new
`"(" QualTypeIdent Pattern* ")"` with zero arguments and `"(" Pattern ")"` wrapping a bare
constructor; collapsing the two into one production resolves it.

Watch the `@L`/`@R` capture: `"(" <p: Pattern> ")"` deliberately builds no node and keeps the
inner pattern's span, and the new alternatives must keep spanning the constructor **and** its
arguments, since that is the text `canonical::Error::VariantNotFound`'s caret sits under.

**Acceptance:** the three examples above parse, with tests in the parser's own test module
asserting the nested `PatternKind`. `cargo run` still prints `parsed 8 modules` and lists all
eight as checked. The three `expect=parse-error:UnexpectedToken` blocks in
[`docs/spec/patterns.md`](../spec/patterns.md)'s *Patterns nest* section go red — that pin's
whole job — and are retagged `expect=ok` with their `**Known gap:**` paragraph deleted.
