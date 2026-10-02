# LANG-85 · An obligation at a record type is never discharged, so no derivation walks a record

**Sizing:** medium, on top of two finished programs. The rule is short. What could make it
bigger is where the walk's definition lives: a record type is declared in no module, so there is
no instance to put it in.

**Location:** `crates/zelkova-compiler/src/typer/` — the solver [`LANG-40`](lang-40.md) adds,
at the step that reads an obligation with the final substitution applied;
`crates/zelkova-compiler/src/canonical/mod.rs` — the context inference
[`LANG-83`](lang-83.md) adds for a `derived` instance; `crates/zelkova-compiler/src/ir/` — the
specialisation pass [`GEN-24`](gen-24.md) adds. None of the three exists yet.

**Depends on:** [`LANG-51`](lang-51.md) for the record type; [`LANG-40`](lang-40.md) and
[`LANG-83`](lang-83.md) for obligations and for what a derivation is; [`GEN-24`](gen-24.md) and
[`GEN-25`](gen-25.md) before a program using it runs. It is the one ticket that needs both
*Active work* orders in [the index](README.md) finished.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decision 8):** a
record is walked by a derivation field by field, **in label order**, folded with `combine`; a
two-value derivation ends at `matched`, a one-value derivation starts at the first field's
answer; `differed` and `atConstructor` are never reached.
[Records and derivation](../spec/records.md#records-and-derivation) is the rule. A record type
is [the head of no instance](../spec/type-classes.md#what-an-instance-is-declared-for), so the
walk is not an instance and `instance Eq { x : Int }` stays unwritable.

**Not implemented:** neither program reaches a record. `LANG-40` discharges an obligation whose
type is a declared type, a tuple or `()`, rejects one at a function, and resolves one at a
variable; a record is none of those. `LANG-83` walks a union, a tuple and `()`, and infers a
derived instance's context from each variant's arguments, with no case for an argument that is a
record. So `r == s` on two records has no rule, and neither does `instance Eq Reading where
derived` for `type Reading = Reading { taken : Celsius }`. Whichever of `LANG-40` and `LANG-51`
lands second meets the gap first, and until this ticket it is an error.

**Approach:**

1. **An obligation `C { l1 : T1, … }` is discharged by its fields.** When `C` is derivable —
   `LANG-83` records that on the class — it becomes one obligation `C Ti` per field, each
   discharged or reported in the ordinary way. The error for a field with no instance names the
   **label** and the field's type; a function-typed field is that error with no possible fix.

2. **When `C` carries no derivation, it is an error** naming the class and saying a record has
   no instance. Whether a record type may ever be a head is
   [an open question](../spec/type-classes.md#open-questions) of the chapter; this ticket
   implements the rule as it stands and does not answer it.

3. **A derived instance's context reads through a record.** A variant argument, or a tuple
   element, whose type is a record needs what each of its fields needs — the same recursion
   `LANG-83` step 3 runs for an application.

4. **The member at a record type is the fold `LANG-83` step 4 describes, with only the fold.**
   Fields in label order whatever order the type was spelled in — labels compared character by
   character, by code point, a label that another begins with first, which is the chapter's
   rule; right-nested; `combine`'s body placed and not called, its first parameter bound once
   ([`DEC-24` decision 8](../decisions/dec-24.md#8--combines-first-parameter-is-a-value-and-its-second-is-the-rest-of-the-walk)).

**Where that definition lives is the implementer's choice**, within the three constraints
below. It is not a language question: no program can observe it. A derived instance's
definitions are canonical code placed where a written binding would be, and a record has no such
place. The two shapes that fit:

- **In `GEN-24`'s pass**, as a specialisation keyed by the class, the member and the record
  type, emitted into the using module like any other. It needs no new home, and the cost
  `GEN-24` already accepts applies: two modules using `Eq` at one record type each carry a copy.
- **In canonicalization**, generated per record type a module mentions and published through
  its `Interface`. It keeps `LANG-83`'s shape, and has to answer which module owns the walk of
  a type two modules both spell.

Whichever is chosen: no dictionary is passed, two spellings of one record type are one key, and
the output is deterministic.

**Acceptance:** each seen red with what it pins neutralised.

In `crates/zelkova-compiler/tests/typer.rs`: `eq` at a record whose fields have instances
checks; one field without an instance is an error naming the label and the type; a
function-typed field is that error; a class with no derivation at a record is the error of
step 2; a constrained function called at a record discharges through the fields.

In `crates/zelkova-compiler/tests/canonical.rs`: a `derived` `Eq` for a union holding a record
infers its context from the record's fields.

In `crates/zelkova-compiler/tests/ir.rs`: for `Comparable`, `{ b : Int, a : Int }` and
`{ a : Int, b : Int }` produce one walk, and it compares `a` first.

In a fixture run under `node`, the way `GEN-24`'s is: two records equal field by field; two that
differ in one field; a record holding a record; and a comparison decided by the first label in
label order when the type was written in another.

`cargo test --workspace` is green, `cargo run -- compile std/core` still lists all ten modules
as checked.

**Found:** while ordering the record tickets for *Active work: records* in
[the index](README.md). `LANG-83` and `LANG-40` were both written after the Records chapter and
neither names a record.
