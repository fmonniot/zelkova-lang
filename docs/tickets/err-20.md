# ERR-20 · A body using a name a record pattern binds at the wrong type is reported at the pattern

**Sizing:** medium. The fix is a change to when a record pattern's entries are read, and that
mechanism is shared with the field access, the update and the accessor.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `FieldConstraint` (its doc comment's
*A record pattern* section states the behaviour) and `infer_annotated`;
`crates/zelkova-compiler/src/typer/unifier.rs` — `read_fields`;
`crates/zelkova-compiler/src/typer/constraint.rs` — `pattern_constraints` and the `Case` arm of
`walk`.

**Problem:** a name a record pattern binds is bound at the entry's own type, a fresh variable
that only the entry's `FieldConstraint` relates to the field's declared type, and every
`FieldConstraint` is read after all of the declaration's equations, the branch body's included.
So when the body uses the name at another type than its field's, the body's equation solves the
variable first, and the mismatch surfaces when the entry is read — under the binding in the
pattern, with the reason `Reason::RecordPatternEntry`:

```zel
nameOf : { name : Char, age : Int } -> Int
nameOf { name } =
  name
```

reports ``cannot match `Char` with `Int` `` under `name` in `{ name }`, explained by the
annotation. The pattern is not what the user has to change; the body is. A tuple pattern in the
same place is reported at the body (`Reason::DeclarationBody`), because its own equation comes
before the body's and has already fixed the binding's type when the body's equation fails.

The test that pins today's place is
`a_body_using_a_bound_field_at_another_type_is_reported_at_the_entry` in
`crates/zelkova-compiler/tests/typer.rs`, and it is expected to go red when this lands.

**Approach:** read a field constraint as soon as its record type is known instead of only after
every equation. The constraint list is in written order, and a record pattern's entries are
collected before the branch body's constraints, so reading each one at its place in the
equation sequence — when its record type is already solved, as it is from an annotation — puts
the field's type on the binding before the body is solved, as a tuple pattern's equation does. A
constraint whose record type is still a variable at that point waits for the late read exactly
as today, so what is accepted does not change, only where a failure is reported. The cost is
interleaving `unifier::unify`'s fold with the field reads without changing which solution is
recorded as the first to rewrite a side (`Origin::left_from`), which decides every other
diagnostic's secondary label; whether that is achievable without changing an existing
diagnostic is the open part of this ticket.

**Acceptance:** the test named above expects `Reason::DeclarationBody` with the caret under the
body's `name`, and is seen red before the change; every other test in
`crates/zelkova-compiler/tests/typer.rs` and `crates/zelkova/tests/pipeline.rs` passes
unchanged, so no other diagnostic moves.

**Found by:** `LANG-84`, whose ticket specified the late read and left this case unaddressed;
its tests pin the behaviour described above.
