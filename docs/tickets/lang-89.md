# LANG-89 · A tuple pattern's element and a record pattern's entry reject a bare applied constructor

**Sizing:** small-to-medium. A grammar change, so `CLAUDE.md`'s *A grammar change is never a
one-file change* applies, though no new `PatternKind` is needed: `(Just x, y)` is a
`PatternKind::Tuple` holding a `PatternKind::Constructor`, both of which exist. What could make
it bigger is a conflict LALRPOP reports once `"(" … ")"` and the tuple alternatives all open on
the same tier, and the span question in step 3.

**Location:** `crates/zelkova-syntax/src/parser/grammar.lalrpop` — `Pattern`, `PatternField` and
`CasePattern`, and the comment above `Pattern`, which cites *Patterns nest* for a rule that
section no longer states. `docs/spec/patterns.md` — [*Where a pattern is
parenthesised*](../spec/patterns.md#where-a-pattern-is-parenthesised), its **Not implemented:**
paragraph and the `expect=unimplemented` block above it. `docs/spec/records.md` — [*Record
patterns*](../spec/records.md#record-patterns), the same pair.

**Decided (`SPEC-38`, by the language owner; [`DEC-26`](../decisions/dec-26.md)):** a pattern
that is not atomic is parenthesised in an argument position — a constructor's argument or a
parameter — and written bare in every other position. [*Where a pattern is
parenthesised*](../spec/patterns.md#where-a-pattern-is-parenthesised) is the rule.

**Problem:** `Pattern` is one production serving four positions, and it takes an applied
constructor only parenthesised. Two of the four are not argument positions. A tuple's element
ends at a `,` or a `)` and a record pattern's entry at a `,` or a `}`, so `(Full x, y)` and
`{ taken = Celsius t }` each have one reading, and both are rejected:

```
error: unexpected token: `Comma`
  ┌─ App.zel:8:12
8 │     (Full x, y) ->
  │            ^ unexpected token
```

`((Full x), y)` and `{ taken = (Celsius t) }` are the spellings that compile.

**Approach:**

1. Split `Pattern` into two tiers. The **atom** tier is what an argument position takes: `_`, a
   variable, a literal, a nullary constructor, `()`, a tuple, a record pattern and a
   parenthesised whole pattern. The **whole** tier is an atom or a constructor applied to one or
   more atoms, which is what `CasePattern` is today.
2. Point each position at its tier. A constructor's arguments and a declaration head's
   `Pattern*` take atoms. A tuple's elements, `PatternField`'s value, the inside of `"(" … ")"`
   and a `case` branch take whole patterns. `CasePattern` then has no alternative of its own
   left and goes.
3. Decide what the `"(" QualTypeIdent Pattern+ ")"` alternative becomes. It is subsumed by a
   parenthesised whole pattern, but its span covers the parentheses, which its comment says is
   the extent `canonical::Error::VariantNotFound`'s caret sits under, while the plain
   parenthesised alternative keeps the inner pattern's span. Either keep it as its own
   alternative, if LALRPOP accepts the overlap, or let the caret move inside the parentheses
   and update the tests that pin it. The ticket does not choose.
4. Rewrite the comment above `Pattern` to describe the two tiers and cite *Where a pattern is
   parenthesised*.

The cons tier and the as tier of the chapter's rule are not this ticket's. A cons pattern is
[`LANG-45`](lang-45.md), which adds its tier between the applied constructor and the whole
pattern, whichever of the two lands first. No ticket implements an as-pattern yet.

**Acceptance:** parser tests in `crates/zelkova-syntax/tests/parser/patterns.rs` assert that
`(Just x, y)` parses to a `PatternKind::Tuple` whose first element is a
`PatternKind::Constructor` with one argument, and that `{ taken = Celsius t }` parses to a
record pattern whose entry holds one, each seen red against the current grammar. A third
asserts an argument position is unchanged: `Wrapper Circle n` in a `case` branch is `Wrapper`
with two arguments. The `expect=unimplemented` blocks in [*Where a pattern is
parenthesised*](../spec/patterns.md#where-a-pattern-is-parenthesised) and in [*Record
patterns*](../spec/records.md#record-patterns) go red and are retagged `expect=ok`, and their
**Not implemented:** paragraphs go. `cargo test --workspace` is green.

**Found:** deciding `SPEC-38`, which was filed from the review of `LANG-16`'s PR.
