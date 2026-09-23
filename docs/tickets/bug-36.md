# BUG-36 · A value that reaches an imported constructor or an imported value is never type checked

**Severity:** medium (wrong behaviour under normal use — a `case` over `Maybe`, which every
module receives through the default imports, is accepted whatever its branches return, and
nothing says the declaration was skipped; the same silence covers a declaration that does
nothing but forward an imported value, such as `std/core`'s `Basics.add = Js.Basics.add`).

**Location:** `src/compiler/typer/mod.rs` — `type_check`'s first pass, which registers this
module's own annotated values into `global` and nothing else, and second pass, which registers
constructors from `module.types` only; `Translation::of`, whose `module_types` and
`constructors` are built the same way — module-local, nothing imported (its own doc comment
says as much: "giving the typer the imported interfaces is `BUG-36`"); `translate_pattern`'s
`PatternKind::Constructor` arm, which returns `None` when `module_types.get(&ctor.tpe)` finds
nothing; `canonical_expr_to_term`'s `VarConstructor` arm, which returns `None` (via
`translation.constructors.get(qname)?`) for the same reason on the expression side; and its
`VarForeign` arm, which *does* build a `Reference` (`ReferenceKind::Foreign`) — translation never
needs anything module-local for it — but for a name `global` never held: `annotate.rs`'s
`TermKind::Identifier` arm looks a `Reference` up by `reference.name` alone, regardless of its
`kind`, so inference reports `UnboundVariable` and `type_check` records `Solved::UnboundName`
instead of skipping silently. `src/compiler/mod.rs` — `check_module`, which holds the interfaces
map and calls `typer::type_check(&canonical)` without it.

**Problem:** the typer's environment is built from the module under check alone, and a name
declared anywhere else — a constructor, or a plain value — is absent from it. What happens next
depends on where and how the name appears, and none of the three paths reports anything.

In a **pattern**, `translate_pattern` needs the constructor's union to learn its type variables,
finds no local declaration, and returns `None`; `value_to_term_and_annotation` propagates it and
`type_check` records `Solved::Untranslatable` for the whole value. Building a constructor in an
**expression** takes the same route: `canonical_expr_to_term`'s `VarConstructor` arm needs the
constructor's place in its declaration — to build the `Reference` a backend reads — and, finding
nothing in `translation.constructors`, returns `None` the same way: `Untranslatable` again, not
a type error. Forwarding an imported **value** is the third path and the only one that reaches
inference at all: `VarForeign` builds its `Reference` without needing anything module-local, so
translation always succeeds; the name then meets `global`, which never held it, inference
reports `UnboundVariable`, and `type_check` records `Solved::UnboundName` — the same outcome,
reached differently.

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
right for constructs the term language cannot express. An imported constructor, or an imported
value, is not one of those: canonicalization resolved it and knows its declaration.

`std/core` has more than the one case today: `Result.fromMaybe`'s `case maybe of Just v -> Ok v`
(`std/core/src/Result.zel`) is skipped for the pattern-side constructor reason above, and most of
`std/core/src/Basics.zel` and `std/core/src/Bitwise.zel` — every declaration that does nothing
but forward a `Js.*` facade value, `add = Js.Basics.add` and `and = Js.Bitwise.and` among them —
is skipped for the `VarForeign` reason.

Found while probing `BUG-35`, whose fix keys the pattern lookup on the constructor's qualified
name. That stops an imported constructor finding a *local* type of the same name, and leaves it
finding nothing, which is this ticket.

**Fix:** give the typer the imported interfaces. Two placements for the constructor half, and
this ticket does not pick:

- *Hand `type_check` the interfaces.* `check_module` already holds the map. The first pass
  additionally registers every value an interface exposes into `global`, keyed the way a
  `VarForeign` reference spells one — closing `VarForeign` directly, since inference looks a
  name up there regardless of what kind of reference produced it. The second pass registers the
  constructors of every union an interface exposes the same way it registers this module's own,
  `translate_pattern` looks a constructor's union up by `ctor.tpe`'s module in the map when it
  is not this module's own, and `Translation::constructors` — read by `canonical_expr_to_term`'s
  `VarConstructor` arm and by `callee_arity` — is built from every interface's unions too, not
  just `module_types`. This changes `type_check`'s signature, which `tests/spec.rs` also calls
  directly.
- *Carry the union's type variables on the canonical constructor.* `TypeConstructor` gains the
  declaration's `variables`, filled in by `do_types` and carried through the interface, so a
  pattern and an expression hold everything the typer needs without a lookup. This changes a
  canonical type every phase shares, and whether `to_interface` should be the thing that writes
  it is part of the call. **This placement does not close `VarForeign`** — forwarding an
  imported value is not a constructor and carries no union to attach variables to — so picking
  it still leaves `global` needing the interfaces' values from the first placement, just for a
  narrower reason than before.

The expression side has a third option the pattern side does not, and whichever placement is
picked has to say why it is not taken: translate `VarConstructor`'s carried type through
`canonical_type_to_typer_type` and drop the `translation.constructors` lookup entirely. For a
*nullary* constructor that type is already right — `canonical/mod.rs` builds
`Type::Type(ctor.tpe, [])` — so this alone would make the cross-module probe below fail with the
message it asks for, with no interfaces in play. It is not enough on its own because the
non-nullary branch beside it is admittedly wrong (its own `TODO` says the arrow is built out of
the constructor's parameters with the result type in front), because an expression that
*applies* a constructor still needs the union's variables from somewhere, and because it does
nothing for `VarForeign` either — the same gap the second placement leaves.

Either constructor placement needed `BUG-35`, which is closed: registering two modules'
constructors in one environment under an unqualified `Type::Adt` would have made every
same-named imported type collide. `Type::Adt` now carries the declaring module, so it does not.

**Acceptance:** tests in `tests/pipeline.rs`, with interfaces built the way `check_importer`
builds them:

- each of the five constructor probes above is rejected with a `CompilationError::Type` whose
  one error is a `UnificationFailed` with its primary label under the `'c'`;
- the `if 'c'` probe is rejected with *cannot match `Bool` with `Char`*;
- **inherited from `BUG-35`**, whose first acceptance case this is: with `A` and `B` each
  declaring `type Size = S` and exposing it,

  ```zel
  module Main exposing (..)

  import A
  import B

  x : A.Size
  x = B.S
  ```

  is rejected with *cannot match `A.Size` with `B.Size`* (either order — unification is
  symmetric). `BUG-35` made the two `Size`es two types and could not pin this, because the
  declaration is skipped before they are ever compared: `B.S` becomes `Untranslatable` today, and
  the two types are never brought together. It is the shortest probe that shows the expression
  side reaching the unifier at all, so it belongs here rather than being lost with `BUG-35`'s
  file;
- a `VarForeign` probe: with `Lib` declaring `size : Int` and exposing it,

  ```zel
  module Main exposing (..)

  import Lib exposing (size)

  f : Char
  f = size
  ```

  is rejected with *cannot match `Int` with `Char`*;
- mutation-check by restoring the local-only lookup in `translate_pattern`, the local-only
  build in `Translation::constructors`, and the local-only registration in `type_check`'s first
  and second passes, and watching the pattern, expression and `VarForeign` tests go red.

`cargo run` still prints `parsed 8 modules`, lists all eight as checked, and exits 0 —
`Result.fromMaybe` and every forwarding declaration in `Basics`/`Bitwise` are newly checked, so a
failure there is either a real error in `std/core` or a regression, and has to be told apart
before landing.
