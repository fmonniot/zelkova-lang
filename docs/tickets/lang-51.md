# LANG-51 · The typer has no record type, so nothing checks a field, an update or an accessor

**Sizing:** large. A new type form in both the canonical and the typer's own languages, plus the
unification rule for it — the first structural type Zelkova has.

**Location:** `canonical::Type` in `src/compiler/canonical/`; `src/compiler/typer/`, all three of
`annotate.rs`, `constraint.rs` and `unifier.rs`.

**Depends on:** [`LANG-48`](lang-48.md) and [`LANG-50`](lang-50.md), hard. There is no record node
to type until the grammar builds one.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decisions 2, 3, 4
and 6):** a record type is a set of labelled fields, order-insensitive and structural; an update
has the type of the record it updates and may not add, remove or retype a field; an accessor is
typed from where it is written; records are closed and there are no row variables.
[Records](../spec/records.md#a-record-type-is-a-set-of-fields) is the rule.

**Not implemented:** neither `canonical::Type` nor the typer's type language has a record form,
so nothing checks a field's type, nothing rejects `{ r | absent = x }`, and there is no rule to
unify two record types under.

**Approach:** a record type is a map from label to type, and two record types unify when they
carry **the same label set** and each pair of field types unifies. Field order is not part of the
type, so the representation is order-independent and the comparison is a set comparison — an
ordered `Vec` compared elementwise would make `{ x : Int, y : Int }` and `{ y : Int, x : Int }`
different types, which is precisely the thing decided against.

**No row variables.** Records are closed, so unification never grows a record and never has a
variable standing for "the rest of the fields". A label mismatch is a plain unification failure
naming the labels that differ. Keeping that true is what stops this becoming a second axis in the
type system, which [Records](../spec/records.md#records-are-closed) says the language declines.

**An accessor is the awkward case** and is worth designing before writing code. `.name` has no
type it can stand for, so it cannot be annotated into the environment the way a value is: it
constrains the type it is applied to to be a record having `name`, and that is not a constraint
this unifier can express without rows. The workable shape is to resolve an accessor once the type
it is used at is known, and to report an error naming the accessor when nothing fixes it — which
means the accessor's constraint is solved late rather than being an ordinary equation. `Origin`
and `Reason` gain a case for it, so the caret lands on the accessor.

**The blocks this makes correct are held to account, as long as they are tagged `expect=ok`.**
The spec harness type checks every `expect=ok` block, so a block that ought to be a type error
and is not one goes red the day this ticket lands. The **Known gap:** paragraphs
[`LANG-48`](lang-48.md) and [`LANG-50`](lang-50.md) attach — on the order-insensitivity block
and the field-adding update — are what that red block asks you to delete. Grep
[Records](../spec/records.md) for `LANG-51` before closing.

**Acceptance:** `tests/typer.rs` cases for a record's inferred type, for the two spellings of one
record type unifying, for a field access at the field's type, for an update keeping the record's
type, for `{ r | absent = x }` and `{ r | x = wrongType }` each failing with a caret under the
offending field, and for an accessor typed from an argument position and an accessor with nothing
fixing it. Two `**Known gap:**` paragraphs in [Records](../spec/records.md) deleted.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
