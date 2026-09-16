# BUG-35 · The typer identifies a union type by its unqualified name, so two modules' `Size` are one type

**Severity:** medium (wrong behaviour under normal use — a module importing two types that share
a name accepts a value of one where the other is annotated, and a constructor pattern can be
checked against the wrong declaration).

**Location:** `src/compiler/typer/mod.rs` — `Type::Adt(String, Vec<Type>)`; `type_check`'s
second pass, which builds each constructor's result type from the bare `type_name` key of
`module.types`; `canonical_type_to_typer_type`'s `canonical::Type::Type` arm, which keeps only
`name.unqualified_name()`; `translate_pattern`'s `PatternKind::Constructor` arm, which looks the
union up with `module_types.get(&ctor.tpe.unqualified_name())` and sets `adt_name` from the same
half (its comment already describes the collision). `src/compiler/typer/unifier.rs` —
`unify_one_constraint`'s `(Type::Adt(n1, ..), Type::Adt(n2, ..)) if n1 == n2` arm.
`src/compiler/typer/constraint.rs` — `collect`, which rebuilds `Type::Adt(adt_name, adt_args)`
for a constructor pattern.

**Problem:** since `AST-4` a canonical type names its declaration in full, and the typer throws
the module half away on the way in. Two declarations spelled alike become one `Type::Adt`, and
the unifier compares the strings. Probed against this branch with `check_module`, `A` and `B`
each declaring `type Size = S` and exposing `Size(..)`:

```zel
module Main exposing (..)

import A
import B

x : A.Size
x = B.S
```

checks clean. It should fail to unify `A.Size` with `B.Size`.

The pattern side has the same defect in a second form. `module_types` holds only the module's
own declarations, keyed by the name the `type` line wrote, so an imported constructor's pattern
finds a local declaration that happens to share its type's name:

```zel
module Main exposing (..)

import A

type Size = Big

f : Size -> Int
f s =
  case s of
    A.S ->
      1
```

also checks clean: `A.S` is looked up as `Size`, finds `Main.Size`, and is given its type.

Found while working on `BUG-26`, where a scalar became its qualified name and the same question
arose for `Bool`: [`LANG-60`](lang-60.md) makes `Bool` an ordinary `Type::Adt`, and under the
current representation any module's own `type Bool` would then satisfy an `if` condition.
Left unfixed there because it changes the typer's type representation for every union, which is
a ticket of its own.

**Fix:** carry the qualified name. `Type::Adt` holds a `QualName`; `type_check`'s second pass
qualifies `type_name` with `module.name`; `canonical_type_to_typer_type` passes the
`canonical::Type::Type` head through whole; `translate_pattern` builds `adt_name` from
`ctor.tpe` whole, and its lookup finds the union only when `ctor.tpe` is a declaration of *this*
module. The unifier's equality is then on the qualified name with no change to the arm.

`Display` keeps writing the unqualified half, since that is how a single module's source spells
its own types and every existing message quotes it that way. Where the two sides of a
`UnificationFailed` share an unqualified half, both are written qualified, so the message for
the first example reads *cannot match `A.Size` with `B.Size`* rather than naming one type twice.

**Out of scope, noticed while probing:** a `case` over an *imported* constructor is not type
checked at all. `translate_pattern` finds no local union for it and returns `None`, and
`type_check` skips the whole value — so `case b of Lib.Box x -> 'c'` under `f : Lib.Box Int ->
Int` checks clean. After this fix that lookup still fails for an imported constructor, now
without finding a wrong local one; giving the typer the imported union's variables is separate
work.

**Acceptance:** tests in `tests/pipeline.rs`, built the way `check_importer` builds its
interfaces:

- the first module above is rejected with a `UnificationFailed` whose message is *cannot match
  `A.Size` with `B.Size`* (in either order), mutation-checked by comparing only the unqualified
  halves in the unifier;
- the second module is no longer checked against `Main.Size`: `translate_pattern` does not
  produce a pattern at `Main.Size` for `A.S` — asserted in `src/compiler/typer/mod.rs`'s unit
  tests, since the whole-module result stays `Ok` while the out-of-scope gap above stands;
- every existing message in `tests/typer.rs` still quotes its types unqualified.

`cargo run` still prints `parsed 8 modules`, lists all eight as checked, and exits 0.
