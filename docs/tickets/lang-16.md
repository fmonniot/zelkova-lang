# LANG-16 · A constructor pattern may not nest, and may not be parenthesised in a `case` branch

**Sizing:** small-to-medium. Small in the grammar, but it is a grammar change, so `CLAUDE.md`'s
*A grammar change is never a one-file change* applies; the parser AST already represents what is
missing. The typer's half is one refusal to lift, and what could make it bigger is a nested
pattern that the constraints or the emitter turn out not to handle.

**Location:** `crates/zelkova-syntax/src/parser/grammar.lalrpop` — the three pattern productions,
`Pattern`, `CasePattern` and `DeclPattern`. `crates/zelkova-compiler/src/typer/mod.rs` —
`translate_sub_pattern`, which admits a variable, `_` and `()` below the top of a pattern and
refuses everything else.

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

**The typer's half.** A pattern nested in another is left unchecked today, whatever it is:
`translate_sub_pattern` answers `None` for anything but a variable, `_` or `()`, so the
declaration holding it is one the typer does not reach and the emitter refuses. That already
covers what parses — `(1, x)`, `((a, b), c)` — and after the grammar change it would cover every
nested constructor. Five doc comments say lifting it is this ticket's (`translate_sub_pattern`,
`pattern_constraints`, `ir::SubPattern`, and `ir/decision.rs`'s module comment and test module),
and each was written so that nothing else has to change: `pattern_constraints` recurses into
sub-patterns already, and `decision_tree` walks them. Remove the refusal, so that a sub-pattern
is translated by `translate_pattern` like a pattern anywhere else, and rewrite those comments to
describe what the code then does.

**Acceptance:** the three examples above parse, with tests in the parser's own test module
asserting the nested `PatternKind`. In `crates/zelkova-compiler/tests/typer.rs`: a `case` over a
tuple with a constructor in one element, and one over a constructor holding an applied
constructor, each give their bindings the right types, and a nested constructor of the wrong
type is an error with its caret under the nested pattern. In `crates/zelkova-compiler/tests/ir.rs`:
the decision tree built from *source* for a nested constructor pattern holds a `Test` below
`Occurrence::Root`. In `crates/zelkova-js/tests/javascript.rs`: the text emitted for one such
`case`. Each seen red with what it pins neutralised. If the emitter turns out not to handle a
nested test, that is a `GEN-` ticket to file and not a fix to make here.

`cargo run -- compile std/core` still prints `parsed 10 modules` and lists all ten as checked,
and `cargo run -- test std/core` still reports `98 tests: 98 passed`. The three
`expect=parse-error:UnexpectedToken` blocks in
[`docs/spec/patterns.md`](../spec/patterns.md)'s *Patterns nest* section go red — that pin's
whole job — and are retagged `expect=ok` with their `**Known gap:**` paragraph deleted. The
`expect=ok` row of [`docs/spec/conventions.md`](../spec/conventions.md) lists "a pattern nested
inside another pattern" among what the typer leaves unchecked; that clause goes.
