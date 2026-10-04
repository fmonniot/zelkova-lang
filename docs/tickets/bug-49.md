# BUG-49 · A superclass of a superclass is not provided when no module the package can see declares the class between them

**Severity:** medium (a valid program is rejected, under ordinary use of a class hierarchy three
packages deep: `grandparent : Ordered a => a -> a -> Bool` using `eq` is an error naming a
constraint the annotation does provide. The error is loud and the workaround is one manifest
line, which is what keeps it from `high`.)

**Location:** `crates/zelkova-compiler/src/typer/classes.rs` — `ClassTable::of`, which builds
the table of classes from the `interfaces` it is handed, and `ClassTable::provide`, whose
`if let Some(signature) = self.class(class)` stops the superclass walk without a word at a
class the table does not hold; `crates/zelkova-compiler/src/lib.rs` — `Interface::classes`,
which carries only the classes its own module declares and exposes.

**Depends on:** [LANG-40](README.md), which adds `ClassTable` and the givens' superclass closure.

**Found:** while reviewing the PR for `LANG-40`. The author's report described it as needing a
module that imports nothing which exposes the middle class; the review reproduced it and found
the real condition is wider than "imports" and narrower than "anywhere", as below. Left unfixed
there because what an `Interface` has to carry is a decision outside that ticket's Acceptance.

**Problem:** a given is closed over superclasses by `provide`, which reads each class's
signature out of `ClassTable`, and `ClassTable::of` fills that from every `Interface` in the
map the module is checked against. For a module of the package being built, that map holds
every module of the package checked so far and the modules of its **direct** dependencies; a
class declared by a package the module does not depend on directly is in no interface in it.
`provide` then finds `Ordered`'s superclass `Comparable`, cannot find `Comparable`'s signature,
and stops, so `Comparable`'s own superclass `Eq` is never provided.

Reproduced against the `LANG-40` branch with four packages, `p1` declaring `class Eq`, `p2`
declaring `class Eq a => Comparable a` (depending on `p1`), `p3` declaring
`class Comparable a => Ordered a` (depending on `p2`), and a root depending on `p1` and `p3`
only:

```zel
module App exposing (grandparent)

import P1.C1 exposing (Eq)
import P3.C3 exposing (Ordered)

grandparent : Ordered a => a -> a -> Bool
grandparent x y =
  eq x y
```

```
error: [App] `Eq a` is required here, and the annotation does not provide it
8 │   eq x y
  │   ^^ this use requires an instance
  = add `Eq a` to the constraints of the annotation on `grandparent`
```

Adding `p2` to the root's `dependencies` makes the same source check, and so does putting all
three classes in one package (checked: the in-package variant needs no import of the middle
class's module either). The program is the same and only the manifest differs.

**Approach:** the ticket does not pick between two shapes, because each decides what an
`Interface` publishes.

1. **An `Interface` also carries the signatures of the superclasses of the classes it exposes**,
   transitively, in a table `ClassTable::of` reads beside `classes`. Costs a second map on
   `Interface` and a rule for what an importer may name (the entry is for lookup, not for
   `import`).
2. **A `ClassSignature` carries its superclass closure**, computed where the class is
   canonicalized, so `provide` needs no lookup of the middle class at all. Costs the closure
   being fixed at declaration: it is a fact about the class, and nothing about it changes by
   where it is looked at from, which is the argument for it.

Either way `provide` stops being a place where an unknown class passes silently: a class the
table cannot find is a state the typer should treat as a failure of its own and not as "no
superclasses".

**Acceptance:** a test in `crates/zelkova-compiler/tests/typer.rs` or
`crates/zelkova/tests/pipeline.rs`, whichever can build the three-package chain
(`tests/fixtures/` packages are the other route), in which `grandparent` above checks with the
root depending on `p1` and `p3` only. It goes red when `ClassTable::of` is given only the
direct dependencies' classes again. `cargo test --workspace` stays green and
`cargo run -- compile std/core` is unchanged.
