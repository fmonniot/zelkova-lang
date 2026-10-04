# ERR-24 · A type error between two classes of one bare name spells both alike

**Sizing:** small. The class counterpart of what `AdtNames` does for unions, and one pinned
message.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `predicate_text`, which writes a
predicate's class with `predicate.class.unqualified_name()`; `ErrorKind::NoInstance`'s
`message()` and notes, `MissingConstraint`'s and `ConstraintNeedsAnnotation`'s, which write the
class the same way; `AdtNames::collide`, `Spellings`, the existing rule for unions.

**Depends on:** [LANG-40](README.md), which adds the messages that name a class.

**Related:** [ERR-22](err-22.md), the same defect between a scalar and a union.

**Found:** while reviewing the PR for `LANG-40`. Left unfixed there because a rule for spelling
a class qualified is a diagnostics decision outside that ticket's Acceptance.

**Problem:** a message qualifies two unions only when they share a bare name, and nothing does
the same for classes: every message writes a class by its bare name. Two modules may each
declare a `class Eq`, and a program that imports both qualified can need one through the other.
Reproduced against the `LANG-40` branch, with `A` declaring `class Eq` (member `eq`), `B`
declaring `class Eq` (member `same`), and

```zel
instance B.Eq a => A.Eq (Box a) where
  eq x y =
    True

check : Box Colour -> Box Colour -> Bool
check x y =
  A.eq x y
```

```
error: [App] there is no instance of `Eq` for `Colour`
16 │   A.eq x y
   │   ^^^^ this use requires an instance, through another's context
   = it is needed because `Eq (Box Colour)` is required here, and the instance of `Eq` that
     answers that asks the same of this type
```

The failing obligation is `B.Eq Colour` and the one the use raised is `A.Eq (Box Colour)`. The
message writes both `Eq`, and the note's "asks the same of this type" is wrong for exactly
this case, where it asks something else of it.

**Fix:** write a class qualified, by its declaring module, in a message that names two classes
whose bare names agree, and drop "the same" from the note when the two classes differ. How a
class is qualified when it is — `B.Eq`, or the spelling the source's import gave it — is the one
open choice, and the ticket does not pick; follow how `AdtNames::Qualified` already writes a
union.

**Acceptance:** a test in `crates/zelkova-compiler/tests/typer.rs` (building the two modules
with `check_package_module` from `tests/support/mod.rs`) asserting that the message and the
note spell the two classes differently, and that goes red when the comparison is removed.
`cargo test --workspace` stays green.
