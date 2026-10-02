# LANG-51 · The typer has no record type, so nothing checks a field, an update or an accessor

**Sizing:** large. A new type form in the typer's own language, beside the canonical one that
already exists, plus the unification rule for it — the first structural type Zelkova has.

**Location:** `canonical::Type` in `crates/zelkova-compiler/src/canonical/`; `crates/zelkova-compiler/src/typer/`, all three of
`annotate.rs`, `constraint.rs` and `unifier.rs`.

**Depends on:** `LANG-50`, hard, and landed ([the index](README.md)). There was no access or
accessor node to type until the grammar built one.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decisions 2, 3, 4,
6 and 10):** a record type is a set of labelled fields, order-insensitive and structural; an
update has the type of the record it updates and may not add, remove or retype a field; an
accessor is typed from where it is written; records are closed and there are no row variables;
and a field access, an update and an accessor are each an error where nothing else in the
declaration supplies the record type.
[Records](../spec/records.md#a-record-type-is-a-set-of-fields) is the rule, with
[A use does not decide a record's type](../spec/records.md#a-use-does-not-decide-a-records-type)
for the last.

**Not implemented:** the typer's type language has no record form — `canonical::Type::Record`
does, and `canonical_type_to_typer_type` answers `None` for it — so nothing checks a field's type, nothing rejects `{ r | absent = x }`, and there is no rule to
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

**A field access and an update are the same case, and take the same mechanism.** `person.name`
says of `person`'s type only that it has a `name`, and `{ r | x = 1 }` only that `r`'s has an
`x`. Neither is an equation while that type is an unsolved variable, and neither may solve it:
[the type is never worked out from the fields a declaration touches](../spec/records.md#a-use-does-not-decide-a-records-type).
So all three are read once unification has run over the whole declaration. If the type is a
record by then, the label is looked up in it; if it is still a variable, that is the error, with
the caret on the access, the update or the accessor and a message saying an annotation would
supply the type. What supplies it may be written after the form as well as before, which is why
this cannot be decided at the point the form is met. [`LANG-84`](lang-84.md) writes a record
pattern against the same mechanism.

**The record expression's fields stay in written order** in the term this builds, as
canonicalization keeps them: only the type is a set.

**The emitter refuses what this adds.** `zelkova_js::emit` matches on `TypedTermKind`, so each
new form needs an arm, and until [`GEN-25`](gen-25.md) that arm is a refusal named in
`Construct`, the way a `let` is. A module holding a record type checks and is not built.

**The blocks this makes correct are held to account, as long as they are tagged `expect=ok`.**
The spec harness type checks every `expect=ok` block, so a block that ought to be a type error
and is not one goes red the day this ticket lands. The **Known gap:** paragraphs
`LANG-48` attaches — on the order-insensitivity block
and the field-adding update — are what that red block asks you to delete. The ones `LANG-50`
attaches under [Reading a field](../spec/records.md#reading-a-field), [The
accessor](../spec/records.md#the-accessor) and
[Expressions](../spec/expressions.md#forms-the-compiler-does-not-have) sit beside blocks that
stay green, so nothing turns red to point at them. Grep [Records](../spec/records.md) and
[Expressions](../spec/expressions.md) for `LANG-51` before closing.

**Acceptance:** `crates/zelkova-compiler/tests/typer.rs` cases for a record's inferred type, for the two spellings of one
record type unifying, for a field access at the field's type, for an update keeping the record's
type, for `{ r | absent = x }` and `{ r | x = wrongType }` each failing with a caret under the
offending field, and for an accessor typed from an argument position and an accessor with nothing
fixing it; for `f person = person.name` and `f r = { r | x = 1 }`, each an error with its caret
on the form, and each accepted once `f` is annotated; and for an access whose record is supplied
by a record expression written *after* it in the same declaration. In
`crates/zelkova-js/tests/javascript.rs`, a module holding a record is refused. Each seen red with
what it pins neutralised.

Every `**Known gap:**` paragraph naming `LANG-51` in [Records](../spec/records.md) and
[Expressions](../spec/expressions.md) deleted — the five above, and the one `LANG-50` attaches
under [A use does not decide a record's type](../spec/records.md#a-use-does-not-decide-a-records-type),
whose block becomes `expect=type-error`.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
