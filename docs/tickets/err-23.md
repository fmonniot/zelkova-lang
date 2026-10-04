# ERR-23 · A use at a type whose instance was rejected is reported as having no instance

**Sizing:** small-to-medium. Small if the answer is to reword the note; medium if the typer is
to learn which instances failed.

**Location:** `crates/zelkova-compiler/src/canonical/classes.rs` — the loop that pushes
`Error::MissingSuperclassInstance` (and `DuplicateInstance`) onto `instance_errors` and keeps an
instance only `if instance_errors.is_empty()`; `crates/zelkova-compiler/src/canonical/mod.rs` —
`without_restated`, whose doc comment is where [`DEC-23` decision
3](../decisions/dec-23.md#3--an-error-that-restates-a-reported-failure-is-dropped-by-a-flag-on-the-scope)
is applied to canonicalization; `crates/zelkova-compiler/src/typer/classes.rs` — `ClassTable::of`,
which builds its instance table from `module.instances` and `module.imported_instances`;
`crates/zelkova-compiler/src/typer/mod.rs` — `ErrorKind::NoInstance`'s notes.

**Depends on:** [LANG-40](README.md), which makes a use at a type with no instance an error.

**Found:** while reviewing the PR for `LANG-40`. Left unfixed there because the shape of the
answer is a decision about `DEC-23`'s flag that the ticket did not make.

**Problem:** an instance that fails canonicalization is left out of the module, so it is in no
`ClassTable`. A use of the class at its head type is then a `NoInstance`, and the module is
told twice about one mistake, the second time falsely. Reproduced against the `LANG-40` branch:

```zel
class Eq a where
  eq : a -> a -> Bool

class Eq a => Comparable a where
  lt : a -> a -> Bool

type Colour = Red | Green

instance Comparable Colour where
  lt a b =
    True

check : Colour -> Colour -> Bool
check x y =
  lt x y
```

```
error: [App] an instance of `Comparable` for `Colour` needs an instance of its superclass `Eq` for `Colour`
11 │ instance Comparable Colour where
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ no instance of `Eq` for `Colour` is in scope

error: [App] there is no instance of `Comparable` for `Colour`
17 │   lt x y
   │   ^^ this use requires an instance
   = an instance of `Comparable` for this type is declared in the module that declares
     `Comparable` or in the module that declares the type, and none is in scope here
```

The first error is right. The second restates it, and its note says no instance is declared
when one is, a line above, and was rejected. Every importing module repeats the second error
too: an instance that failed is published by no interface (the `without_restated` doc comment
says so for `MissingSuperclassInstance`), so each use in each importer is its own `NoInstance`
while the module that wrote the instance has already said why.

**Approach:** the ticket does not pick. The options, with what each costs:

1. **Drop a `NoInstance` in a module whose scope is incomplete**, the coarse flag `DEC-23`
   decision 3 already uses. Cheap, and consistent with how a missing name is treated; but a
   failed instance does not set `Module::incomplete` today, and setting it would also drop
   every not-found error after it, which is a wider loss than one instance warrants.
2. **Record each rejected instance's class and head name** on `canonical::Module` and on
   `Interface`, and have the typer drop a `NoInstance` that names one of them. Exact, at the
   cost of a second list that crosses the package boundary the way `instances` does.
3. **Keep the error and correct the note**: say that an instance for this type was declared
   and rejected, naming it. No suppression mechanism; the user sees two errors and the second
   is true. Does not help the importers, whose note would have to be found from an `Interface`
   that does not carry the rejected instance.

`DEC-23` decision 3 states "no error is dropped without another one standing"; options 1 and 2
have to show it holds for a failure in another package of the build, which publishes nothing.

**Acceptance:** a test in `crates/zelkova-compiler/tests/typer.rs` (or
`crates/zelkova/tests/pipeline.rs` for the importer case) for the module above whose assertion
depends on the option chosen — option 1 or 2 reports `MissingSuperclassInstance` and no
`NoInstance`; option 3 asserts the note's new text — and goes red when the new rule is removed.
`cargo test --workspace` stays green. If a suppression is chosen, an entry in
[`docs/decisions/`](../decisions/README.md) extending `DEC-23` decision 3 to say what it now
covers.
