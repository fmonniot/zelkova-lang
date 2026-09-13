# SPEC-29 · `unsafe` marks 45 `std/core` signatures and none of the chapter's own examples

**Sizing:** small — six `module foreign` blocks, each either gains `unsafe` or a **Known gap:**
note; no compiler change.

**Location:** [`docs/spec/interop.md`](../spec/interop.md) — the *Facade constants* example
(`module foreign Core.Basics exposing ( pi , e )`, right after *An `unsafe` facade*), the
*A union crosses as a tagged value* example (`Core.Colour`, `rgb`/`luminance`, and `Core.Palette`,
`toHex`), and the *What a facade signature may not name* examples (`Core.Utils`, `equal`, and
`Core.List`, `count`). [`docs/spec/packages.md`](../spec/packages.md) — the *Dependencies* lead-in
example (`Core.Widget`, `measure`). [`docs/spec/type-classes.md`](../spec/type-classes.md) — *A
constrained function may not be a foreign facade* (`Core.Cmp`, `compare`).

**Problem:** [Foreign interoperability](../spec/interop.md#foreign-interoperability) opens with
the rule every facade signature is held to: "A facade declares an **effect** unless it says
otherwise. Its result type is `Task (Result Failure a)` … Writing `unsafe` before a signature
removes the `Task`". `LANG-53` added `unsafe` to the grammar and put the word on all 45
`std/core` signatures that needed it, plus the four `interop.md` blocks its own acceptance named
(`Core.Prim`'s `fdiv`/`idiv`, the two `Task`-returning examples, and the reserved-word block). It
did not touch any other facade example already in the spec, so six blocks across three chapters
now show a `module foreign` signature that names neither `Task (Result Failure a)` nor `unsafe`
— an unmarked facade declaring an effect it does not have, under the chapter's own rule.

The sharpest case is *Facade constants*:

```zel
module foreign Core.Basics exposing
  ( pi
  , e
  )

pi : Float
e : Float
```

`LANG-53` rewrote exactly these two declarations in `std/core/src/Js/Basics.zel` to `unsafe pi :
Float` / `unsafe e : Float`. Same module name, same two constants, and the chapter and the
library it is drawn from now disagree — before `LANG-53`, neither could carry the word, so there
was no disagreement to have.

The other five are less pointed because none is a real `std/core` module, but the same rule
applies to each: `Core.Colour`'s `rgb`/`luminance` (`interop.md`, *A union crosses as a tagged
value*'s sibling section just above it), `Core.Palette`'s `toHex` (the union-crossing example
itself), `Core.Utils`'s `equal` and `Core.List`'s `count` (*What a facade signature may not
name* — both already carry a **Known gap:** paragraph, but for an unrelated reason: `equal`
names a type variable and `count` takes a function, which [`LANG-43`](lang-43.md) is what
rejects), `Core.Widget`'s `measure` (`packages.md`, illustrating `private-modules`), and
`Core.Cmp`'s `compare` (`type-classes.md`, already `expect=unimplemented` for a different reason
— a constrained facade signature).

None of the six is rejected today, because nothing enforces the opening rule yet — that
enforcement is [`LANG-43`](lang-43.md), already filed and scoped to the admitted-types /
result-type check. This ticket is about the six examples reading consistently with the rule the
chapter already states, not about making the compiler enforce it.

**Approach:** for each of the six, decide — per example, based on what it is illustrating —
whether it should:

1. Gain `unsafe`, if the point being illustrated has nothing to do with effects (the *Facade
   constants*, union-crossing and `private-modules` examples are the likely candidates: none of
   them is about `Task` or effectfulness), or
2. Stay unmarked but gain an explicit **Known gap:** note (per
   [`docs/spec/conventions.md`](../spec/conventions.md)'s lead-in convention) stating that under
   the opening rule it should declare `Task (Result Failure a)` or be marked `unsafe`, and isn't,
   citing [`LANG-43`](lang-43.md) as the ticket that will let this be checked mechanically.
   `Core.Utils` and `Core.List` already have a **Known gap:** paragraph for their own reason;
   extend it rather than adding a second one to the same block.

Leave `Core.File`, `Core.Time` and `Core.PrimChecks` alone — the *Testing a companion* paragraph
already explains why a test facade is correctly unmarked, and they are not part of this
inconsistency.

**Acceptance:** every `module foreign` example under `docs/spec/` that is not a test facade
(`Core.File`, `Core.Time`, `Core.PrimChecks`) either declares `Task (Result Failure a)`, is
marked `unsafe`, or carries a **Known gap:** or **Not implemented:** note explaining why it is
neither yet. `cargo test --test spec` is green — no example's `expect=` tag changes meaning, and
`Core.Basics`'s `pi`/`e` block matches the spelling `std/core/src/Js/Basics.zel` actually uses.
