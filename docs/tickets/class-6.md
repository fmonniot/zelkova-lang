# CLASS-6 · `std/core` declares `Eq`, `Comparable`, `Number` and `Appendable`

**Sizing:** large. Not because any one signature is hard, but because it is the first time the
package's over-general types meet a checker that can reject them, and because it narrows the
`Js.Utils` facades that half of `Basics` is built on.

**Location:** `std/core/src/Basics.zel` — the comparison, arithmetic, equality and append
signatures, and the `infix` declarations aliasing them; `std/core/src/Js/Utils.zel` and
`std/core/src/Js/Utils.mjs` — `equal`, `notEqual`, `lt`, `le`, `gt`, `ge`, `compare`, `append`;
`std/core/src/Js/Basics.zel` and `.mjs` — the arithmetic facades.

**Depends on:** [CLASS-4](class-4.md) and [CLASS-5](class-5.md).

**Closes:** [BUG-20](bug-20.md), and for the right reason. That ticket says the fix is in two
halves — make the runtime say so, and make the type say so — and that only the first was
available at the time. This is the second.

**Decided (`SPEC-12`, by the language owner):** four classes, `Eq` a superclass of
`Comparable`, and a `module javascript` facade signature may not carry a constraint.

**Problem:** `Basics` publishes six comparison functions, one equality pair and an append, all
typed `a -> a -> …`, all backed by JavaScript that cannot honour that type. `_Utils_cmp`
compares a non-object with `<` and otherwise assumes a tuple, reading `.a`, `.b` and `.c` off
it; handed a user union value it recurses into three `undefined` fields. `_Utils_eqHelp` calls
`__Debug_crash(5)` on a function. The type checker accepts every one of these calls today
because the declared type genuinely does accept them.

`SPEC-11` rewrote those signatures from `comparable`/`number`/`appendable` to `a`, which made
the over-promise visible instead of hiding it behind a word that looked like a restriction. This
ticket is the other end of that: the signatures get to say what they meant.

**The shape decision 6 forces.** A facade may not be constrained, so the constraint moves up one
level and the facade underneath it becomes monomorphic:

```zel
-- Js/Utils.zel — unconstrained, and now callable only at types the JS can handle
compareInt : Int -> Int -> Int
compareFloat : Float -> Float -> Int
compareChar : Char -> Char -> Int

-- Basics.zel — the constraint lives here
class Eq a where
  eq : a -> a -> Bool

class Eq a => Comparable a where
  compare : a -> a -> Order

instance Comparable Int where
  compare a b =
    orderOf (Js.Utils.compareInt a b)
```

That is a real change of shape for the package, not a retyping: **43 of `Basics` and `Bitwise`'s
declarations are bare facade re-exports** (`add = Js.Basics.add`), and a constrained function
cannot be one — its body has to dispatch. Probing found those 43 are also exactly the
declarations `type_check` skips today, so they have never been checked by anything; expect this
ticket to be the first pass over them and expect it to find more than it was looking for.

**Approach:** decide these before writing, because each changes the size of the diff.

1. **Which types get instances.** `Int`, `Float`, `Char` and `Bool` are what exists. `String` and
   `List` are what `Appendable` was for, and neither is implemented — so `Appendable` ships with
   an instance list that does not include its motivating cases. Declaring it anyway, versus
   deferring it until strings and lists exist, is a real choice and this ticket does not make
   it.

2. **What happens to the operators.** `infix non 4 (<) = lt` aliases a function that is about to
   become a class member. Confirm an `infix` declaration may name one — nothing about that is
   settled, and it is the kind of gap that only shows up here.

3. **`Js.Utils.compare` returning `Int`.** `Basics.compare` returns `Order`. Today the
   conversion is implicit and wrong; with instances it becomes an explicit `orderOf` per
   instance, which needs writing and needs the `.mjs` to keep returning what it says it does.

4. **The `.mjs` files.** BUG-20's first half — make the runtime reject what it cannot handle —
   is still worth doing here even though the types now prevent it, because the facades are the
   package's boundary and a boundary that trusts its caller is one bad codegen away from the
   original bug.

**Acceptance:** `min Red Blue`, on a user union type with no `Comparable` instance, is a type
error — the program `SPEC-11`'s chapter and BUG-20 both use as their worked example, and the
single check this ticket exists for. `eq` applied to a function value is a type error rather
than a runtime crash. Tests in `tests/typer.rs` for both, plus `tests/pipeline.rs` coverage
that the real `std/core` modules still check. `cargo run` still prints `parsed 8 modules` and
lists all eight as checked. `docs/tickets/bug-20.md` is deleted and its row tombstoned.

The blocks in `docs/spec/type-classes.md` showing a constrained standard-library signature go
from `expect=unimplemented` to `expect=ok`, and their
`**Not implemented:**` paragraphs are deleted. If that chapter has not been written when this
ticket lands, say so in the PR rather than skipping the clause: it means `TEST-2` and `SPEC-12`
fell behind the compiler work and the mechanism has shipped unspecified.
