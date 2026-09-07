# SPEC-25 · A derivation walks two values, so the classes worth deriving most cannot be

**Sizing:** small. One section added to an existing chapter, plus the paragraph in
[`docs/spec/type-classes.md`](../spec/type-classes.md)'s *Open questions* that this closes. No
compiler change — the mechanism it extends does not exist either
([`CLASS-2`](class-2.md) onward).

**Location:** [`docs/spec/type-classes.md`](../spec/type-classes.md), the three sections
*[A class says how it is derived](../spec/type-classes.md#a-class-says-how-it-is-derived)*,
*[What a derived instance computes](../spec/type-classes.md#what-a-derived-instance-computes)*
and *[What a derived instance requires](../spec/type-classes.md#what-a-derived-instance-requires)*,
and that chapter's *Open questions* entry naming this ticket.

**Problem:** a member may carry a derivation only when its signature is `a -> a -> R`. That
covers `eq` and `compare`, which is to say it covers the two classes `std/core` happens to
declare, and it covers nothing anybody asks for next.

The shapes it excludes are not equally out of reach, and the chapter now says so — but it says
so in an *Open questions* entry, which is a promise that someone will decide, not a decision:

- **`a -> R`** — `hash : a -> Int`, a size, a checksum, a structural digest. This is the same
  walk over **one** value rather than two. It is a strictly smaller mechanism than the one
  already specified: there are no two constructors to disagree, so `differed` collapses to an
  answer for *the* constructor, and the class supplies `atConstructor : Position -> R` and the
  same `combine`. Everything else — arguments answered by their own instances, the fold, the
  monoid law — is unchanged.
- **`a -> a -> a`** — `add`, `append`, a merge. Ruled out, and the chapter says why: the walk has
  no way to produce a third value of the type it is walking.
- **No `a` at all** — `bottom : a`, `allValues : List a`. Ruled out for a different reason: the
  walk would have to run backwards from a description of the type's constructors, and no such
  description exists in this design.

So there is one shape in the list that is buildable and unbuilt, and this ticket is that shape.

**What it does not reach, and why that has to be written down.** `toString : a -> String` has the
`a -> R` signature and is the member most readers will expect to fall out of it. It does not, and
the reasons are the boundary of the whole approach rather than an oversight:

1. It needs a constructor's **name**. A `Position` orders constructors and converts to an `Int`;
   neither yields `"Red"`.
2. It needs **parenthesisation**, which is context handed *downwards* into the arguments. Haskell
   spells this `showsPrec :: Int -> a -> ShowS` precisely because it cannot be a fold. A walk that
   collects answers and folds them upwards has no downward channel at all.
3. It needs to know which argument is the **first**, to place a separator. A fold over `String`
   with `""` as its identity gives `RedGreen`, never `Red Green`.

A chapter that adds `a -> R` without saying this leaves every reader to discover it on their own,
and some of them will discover it by designing around it.

**Approach:**

1. Settle with the language owner whether `a -> R` is wanted at all. The cheap alternative is to
   decide that it is not, delete the *Open questions* entry, and say in the chapter that a
   derivation walks two values by design — which is a legitimate answer and a smaller one.
2. If it is wanted: specify the one-value derivation in the same shape as the two-value one —
   `derived <member>`, `atConstructor`, `combine`, the same monoid law, the same "every answer
   comes from an instance" rule, the same inlining.
3. Say in the chapter why `toString` is not on the list, in roughly the three points above, and
   name what a `toString` would need instead — a mechanism that reads constructor names, or a
   compiler primitive of the kind Elm's `Debug.toString` is.
4. Update [`CLASS-2`](class-2.md) if the parse of a class body grows a second derivation form.

**What this is not.** Not a generic representation of a type. The chapter's
*[A class is always over a complete type](../spec/type-classes.md#a-class-is-always-over-a-complete-type)*
rules out variables over type constructors, which is what a `Rep`-style encoding needs; the
one-value walk is reachable *because* it asks for less than that, not because the restriction has
softened.

**Acceptance:** [`docs/spec/type-classes.md`](../spec/type-classes.md) either specifies a
one-value derivation or states that it will not have one, and in both cases says why `toString`
is out of reach. The chapter's *Open questions* no longer carries the *A walk over one value*
entry. `cargo test --test spec` green, with any new block tagged `expect=unimplemented` and
proven to fail.

**Found:** while comparing this chapter's derivation mechanism against the equivalent in Haskell,
Clean, Scala 3, Rust, OCaml, PureScript and Elm, on 2026-09-06. That survey is
[`DEC-1`](../decisions/dec-1.md), and its *What the two-value shape gives up* section is the
long form of the argument above.
