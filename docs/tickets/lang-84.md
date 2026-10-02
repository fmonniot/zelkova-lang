# LANG-84 · A record pattern is not type checked

**Sizing:** medium. One new pattern form in the typer's term language and its constraint. What
could make it bigger is that the constraint is not an equation: it is solved late, the way
[`LANG-51`](lang-51.md)'s accessor is, and it is small only if that mechanism is reused.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `translate_pattern`, whose catch-all
arm answers `None`, and `translate_sub_pattern`; `crates/zelkova-compiler/src/typer/constraint.rs`
— `pattern_constraints`; `crates/zelkova-compiler/src/ir/mod.rs` — `TermPatternKind`,
`SubPattern` and `TermPattern::bindings`; `crates/zelkova-compiler/src/ir/decision.rs` —
`Step` and `decision_tree`.

**Depends on:** [`LANG-49`](lang-49.md), hard — there is no record pattern to type until the
grammar builds one — and [`LANG-51`](lang-51.md), hard, for the record type and for the
late-solved constraint its accessor needs.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decisions 5 and
10):** a record pattern names a **subset** of a record's fields, takes its record type from the
value being matched, is an error when it names a label that type does not have, and is an error
when nothing supplies that type. [Records](../spec/records.md#record-patterns) is the rule.

**Not implemented:** neither ticket above owns this. `LANG-49` stops at canonicalization, and
`LANG-51` cites decisions 2, 3, 4 and 6 with no pattern among its acceptance cases. So once both
have landed, `translate_pattern` meets a `canonical::PatternKind` it has no arm for and answers
`None`: the declaration is left unchecked, which raises no error, and `zelkova_js::emit` refuses
the module that holds it. A pattern naming a label the record does not have is accepted.

**Approach:**

1. **`TermPatternKind` gains a record form**: one entry per label the pattern writes, each a
   label and a `SubPattern`, which already carries the type of the value found at a position.
   `TermPattern::bindings` walks the entries the way it walks a tuple's elements. The shorthand
   needs nothing: `LANG-49` desugars `{ x }` in the grammar action.

2. **The constraint is not `own == against`.** A tuple pattern builds its whole type from its
   elements and equates it with the type it is matched against. A record pattern cannot: it
   names a subset, so it does not know the record's other fields, and records are closed, so
   there is no row variable to stand for them. What it says is that `against` is a record type
   holding each label it names, at that entry's type. That is the constraint `LANG-51` gives an
   accessor — `.name` says the same thing about one label — and it is solved at the same point,
   once unification has said what `against` is. Write it against that mechanism and not beside
   it.

   `Origin` and `Reason` carry it so that the caret lands under the **label** the type lacks,
   not under the whole pattern, and a field whose sub-pattern disagrees with the field's type
   is blamed on the sub-pattern.

3. **An entry's sub-pattern is a whole pattern**, translated by `translate_sub_pattern` like a
   constructor's argument. [`LANG-16`](lang-16.md) is ahead of this in the order and lifts that
   function's refusal of anything but a variable, `_` and `()`, so `{ taken = Celsius }` — a
   constructor in a field — is checked with nothing written here for it.

4. **`decision_tree` needs an arm the day the variant exists.** A record has one shape, so the
   pattern itself tests nothing, and each entry is a sub-occurrence reached by its label: `Step`
   gains a field step beside `ConstructorArgument` and `TupleElement`. That much is written
   here. Emitting a read of it is [`GEN-25`](gen-25.md)'s.

5. **A record pattern whose type nothing supplies is an error**, with the caret on the pattern
   and a message saying an annotation would supply the type
   ([A use does not decide a record's type](../spec/records.md#a-use-does-not-decide-a-records-type),
   [`DEC-8` decision 10](../decisions/dec-8.md#10--a-use-does-not-decide-a-records-type)). It is
   the error `LANG-51` reports for a field access on an unsolved type, reached the same way:
   read once unification has run over the declaration, and never solved by the pattern itself.
   `nameOf { name } = name` with no annotation is that error; under
   `nameOf : { name : Text } -> Text` it checks.

**Acceptance:** in `crates/zelkova-compiler/tests/typer.rs`, each seen red with what it pins
neutralised:

- a parameter annotated with a record type and written `{ name }` binds `name` at the field's
  type, and the declaration's type is the annotation's;
- a pattern naming two of a record's three fields checks;
- a pattern naming a label the type does not have is an error, asserted on
  `diagnostic.labels[..].range` to sit under that label;
- `{ centre = { x } }` against a record holding a record binds `x` at the inner field's type;
- a record pattern inside a constructor pattern takes its type from the constructor's argument;
- `{ taken = Celsius, expected = e }` checks, and a constructor of the wrong type in a field is
  an error under that constructor;
- an unannotated `nameOf { name } = name` is the error of step 5, asserted to sit under the
  pattern, and the same declaration annotated checks.

In `crates/zelkova-compiler/tests/ir.rs`: the decision tree of a `case` over a record with one
branch `{ x, y }` holds no `Decision::Test`, and each binding's occurrence ends in a field step.

The record-pattern blocks in [Records](../spec/records.md#record-patterns) and
[Patterns](../spec/patterns.md#record-patterns) are `expect=ok` after `LANG-49` only because the
typer leaves them unchecked. After this they are checked, and every one of them is annotated, so
each stays green; one that goes red is a bug in this ticket and not a tag to change. `cargo test --workspace` is green and
`cargo run -- compile std/core` still lists all ten modules as checked.

**Found:** while ordering the record tickets for *Active work: records* in
[the index](README.md). Not fixed there because that section orders tickets and writes no code.
