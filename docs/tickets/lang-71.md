# LANG-71 · A constraint context of four or more constraints does not parse

**Sizing:** small-to-medium. Every option below is a small edit to the grammar, but the choice
between them is a language decision, and one of them changes what a context is on the parser AST.

**Location:** `crates/zelkova-syntax/src/parser/grammar.lalrpop` — `ConstrainedType` and `AtomicType`, whose
tuple productions stop at three elements; `crates/zelkova-syntax/src/tuple.rs` — `Tuple<T>`;
`crates/zelkova-compiler/src/canonical/mod.rs` — `validate_context`, whose doc comment already says a list of
two or three is the largest shape that reaches it; `parser::FunType::context`
(`crates/zelkova-syntax/src/parser/mod.rs`).

**Depends on:** none.

**Found while:** reviewing the PR that closed [LANG-37](README.md), which made a constraint
context parse. The chapter records the gap under *Constraining an annotation*
([`docs/spec/type-classes.md`](../spec/type-classes.md)), where a `**Known gap:**` paragraph and an
`expect=parse-error:UnexpectedToken` block pin it, so the block goes red when this is closed.

**Problem:** [`docs/spec/type-classes.md`](../spec/type-classes.md) says "several constraints are
parenthesised and comma-separated" and puts no cap on the count, as does
[DEC-2](../decisions/dec-2.md) decision 1. The compiler caps it at three:

```zel
f : (Eq a, Eq b, Eq c, Eq d) => a -> b -> c -> d -> Bool
```

is `unexpected token: Comma` at the fourth element, expecting `)`. The cause is the one
`ConstrainedType`'s comment gives. At the `(` of `(Eq a, Eq b) =>` an LALR(1) parser cannot tell a
list of constraints from a two-tuple type, so the context is parsed *as a type*, and
`AtomicType` has a tuple production for two elements and one for three and for no other arity.
That cap is deliberate for types (`CLAUDE.md`, *Tuples are size 2 or 3 only*; `Tuple<T>`), and it
is inherited here by a construct that is not a type.

**Approach:** the ticket does not choose. Three ways out, each with a cost:

1. **Give a list of four or more constraints its own production in `ConstrainedType`**, ahead of
   the tuple ones: `"(" Type "," Type "," Type "," Type ("," Type)* ")" "=>" Type`. For two and
   three elements the parser still meets the tuple productions at `)`. From the fourth element on,
   only the new production is viable, so the two may not conflict; whether LALRPOP accepts it has
   to be tried, because `LANG-37` found the obvious spellings of a separate context syntax
   ambiguous. Its cost is on the AST: `FunType::context` is an `Option<Type>` and a `Tuple<Type>`
   cannot hold four, so the context changes shape (a `Vec` of constraints, say), and
   `validate_context` and the class/instance head reuse that [LANG-38](lang-38.md) plans follow it.
   It also leaves a context nested in a context, `((Eq a, Eq b, Eq c, Eq d), Eq e) =>`, unparsed.
2. **Lift the cap for types**, so a four-tuple type exists and the context is one. It is the least
   grammar work, and the one that contradicts a standing invariant: a four-element tuple type
   becomes legal everywhere a type is, and Zelkova would no longer match Elm on it. That is the
   language owner's decision and `AST-2` is why it was made.
3. **Leave the cap and specify it.** The chapter states that a context holds at most three
   constraints, the `**Known gap:**` becomes a rule, and the block retags to a plain
   `expect=parse-error` under that rule. A signature that needs four classes has to name
   them through a class that has them as [superclasses](../spec/type-classes.md#superclasses),
   which is a workaround for its author and no change to the compiler.

**Acceptance:** whichever is chosen, `cargo test --test spec` is green with the four-constraint
block in [`docs/spec/type-classes.md`](../spec/type-classes.md) retagged for the result (`expect=ok`
for 1 and 2, `expect=parse-error` under a stated rule for 3) and its `**Known gap:**` paragraph
deleted or turned into the rule. For 1 and 2, a test in `crates/zelkova-syntax/tests/parser/types.rs` asserts
the four constraints reach `FunType::context`, each with a span, and a test in
`crates/zelkova-compiler/tests/canonical.rs` puts an `InvalidConstraint` caret under the fourth of four when it is
malformed. Each is seen red with the new production removed. `cargo run -- compile std/core` still
prints `parsed 8 modules` and lists all eight as checked.
