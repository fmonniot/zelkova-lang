# LANG-90 · A context written on a `derived` instance is rejected, where the chapter makes it the instance's context

**Sizing:** small. An implementation with its tests exists on the local branch `lang-90`, cut
from the session that settled `SPEC-39`; landing it is a review, not a rewrite.

**Part of:** the type-class work, after `LANG-83`.

**Location:** `crates/zelkova-compiler/src/canonical/classes.rs` — `do_instances`, which raises
`Error::DerivedInstanceWritesContext` for any context on a `derived` instance and gives a derived
instance no context in the `written` list. `crates/zelkova-compiler/src/canonical/derivation.rs`
— `derive_all`, whose fixed point starts every derived instance's context empty.
`docs/spec/type-classes.md` — [*What a derived instance
requires*](../spec/type-classes.md#what-a-derived-instance-requires).
[`DEC-27` decision 1](../decisions/dec-27.md#1--a-written-context-is-an-upper-bound).

**Problem:** the chapter says a context written on a `derived` instance is the whole of the
instance's context, and that it must provide every constraint the type's arguments need, itself
or through a superclass. The compiler rejects every written context. Three things the chapter
states are therefore not so today:

- `instance (Eq a, Hash a) => Eq (Box a) where derived` is rejected and should be accepted, with
  the context written.
- `instance Hash a => Eq (Box a) where derived` is rejected for carrying a context, and should be
  rejected for leaving `Eq a` out, naming the constraint and the variant that needs it.
- `instance Eq a => Comparable (Phantom a) where derived` is rejected, so a derived instance whose
  inferred context fails its superclass obligation has no spelling that compiles.

**Approach:** a derived instance that writes a context keeps it: `derive_all` reads it as where
that instance's context starts and stays, so another derived instance holding the type needs what
was written, and checks each constraint the arguments need against it and the superclasses of
what it names. `Error::DerivedInstanceWritesContext` gives way to an error for the constraint a
written context leaves out.

**Acceptance:** the two blocks of *What a derived instance requires* tagged
`expect=canonical-error:DerivedInstanceWritesContext` are retagged, the first `expect=ok` and the
second for the new error, and the **Not implemented:** paragraph under them is deleted;
`cargo test --test spec` is green. Tests in `crates/zelkova-compiler/tests/canonical.rs` pin that
a written context is kept as written, that one leaving a needed constraint out is an error at the
context, that a superclass provides, and that another derived instance needs what was written,
each seen red. `CLAUDE.md`'s *Language notes* stops saying a written context is an error.
