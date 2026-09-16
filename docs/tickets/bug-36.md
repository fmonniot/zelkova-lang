# BUG-36 · A value that matches or builds an imported constructor is never type checked

**Severity:** medium (wrong behaviour under normal use — a `case` over `Maybe`, which every
module receives through the default imports, is accepted whatever its branches return, and
nothing says the declaration was skipped).

**Location:** `src/compiler/typer/mod.rs` — `type_check`, whose second pass registers
constructors from `module.types` only, and which `continue`s past a value when
`value_to_term_and_annotation` returns `None` or inference reports `UnboundVariable`;
`translate_pattern`'s `PatternKind::Constructor` arm, which returns `None` when
`module_types.get(&ctor.tpe.unqualified_name())` finds nothing; `canonical_expr_to_term`'s
`VarConstructor` arm, which looks the constructor up as an identifier in that same
module-local environment. `src/compiler/mod.rs` — `check_module`, which holds the interfaces map
and calls `typer::type_check(&canonical)` without it.

**Problem:** the typer's environment is built from the module under check alone, and a
constructor declared anywhere else is absent from it. What happens next depends on where the
constructor appears, and neither path reports anything.

In a **pattern**, `translate_pattern` needs the constructor's union to learn its type
variables, finds no local declaration, and returns `None`; `value_to_term_and_annotation`
propagates it and `type_check` skips the whole value. In an **expression**, the constructor
becomes an identifier the environment does not hold, inference fails with `UnboundVariable`,
and `type_check` skips the value for that reason instead.

Probed against `fix/bug-26-scalar-by-qualified-name` with `check_module`, a `Lib` exposing
`type Box a = Box a` and `type Flag = On | Off`, and `basics_interface()` and
`maybe_interface()` in the map. Every one of these checks clean:

```zel
module Main exposing (..)

f : Maybe Int -> Int
f m =
  case m of
    Just x ->
      'c'

    Nothing ->
      1
```

```zel
module Main exposing (..)

import Lib exposing (Box(..))

f : Box Int
f = Box 'c'
```

and so do the same `case` written over `Lib.Box x` qualified, over `Box x` exposed, and over
the nullary `On`. Because the skip is per value, it also hides errors that have nothing to do
with the constructor: an `if 'c' then …` wrapped around a `case` over `On` is accepted, while
the same `if` without the `case` is rejected with *cannot match `Bool` with `Char`*.

`type_check`'s doc comment describes both skips as deliberate gaps, since reporting either would
blame the user for a limit of the typer, and points at `ERR-8` for a warning. That is still
right for constructs the term language cannot express. An imported constructor is not one of
those: canonicalization resolved it and knows its declaration.

`std/core` has one today: `Result.fromMaybe`'s `case maybe of Just v -> Ok v`
(`std/core/src/Result.zel`) is skipped for this reason.

Found while probing [`BUG-35`](bug-35.md), whose fix keys the pattern lookup on the
constructor's qualified name. That stops an imported constructor finding a *local* type of the
same name, and leaves it finding nothing, which is this ticket.

**Fix:** give the typer the imported unions. Two placements, and this ticket does not pick:

- *Hand `type_check` the interfaces.* `check_module` already holds the map. The second pass
  registers the constructors of every union an interface exposes, and `translate_pattern` looks a
  constructor's union up by `ctor.tpe`'s module in the map when it is not this module's own.
  This changes `type_check`'s signature, which `tests/spec.rs` also calls directly.
- *Carry the union's type variables on the canonical constructor.* `TypeConstructor` gains the
  declaration's `variables`, filled in by `do_types` and carried through the interface, so a
  pattern and an expression hold everything the typer needs without a lookup. This changes a
  canonical type every phase shares, and whether `to_interface` should be the thing that writes
  it is part of the call.

Either one needs [`BUG-35`](bug-35.md) first or alongside: registering two modules' constructors
in one environment under today's unqualified `Type::Adt` would make every same-named imported
type collide.

Out of scope: `VarForeign`, a reference to an imported *value*, which `canonical_expr_to_term`
also returns `None` for. It already carries its type from canonicalization and is a separate
change.

**Acceptance:** tests in `tests/pipeline.rs`, with interfaces built the way `check_importer`
builds them:

- each of the five constructor probes above is rejected with a `CompilationError::Type` whose
  one error is a `UnificationFailed` with its primary label under the `'c'`;
- the `if 'c'` probe is rejected with *cannot match `Bool` with `Char`*;
- mutation-check by restoring the local-only lookup in `translate_pattern` and the local-only
  registration in `type_check`'s second pass, and watching the pattern and expression tests go red.

`cargo run` still prints `parsed 8 modules`, lists all eight as checked, and exits 0 —
`Result.fromMaybe` is newly checked, so a failure there is either a real error in `std/core` or
a regression, and has to be told apart before landing.
