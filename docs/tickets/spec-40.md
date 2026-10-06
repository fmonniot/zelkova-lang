# SPEC-40 · No chapter says how `Comparable` orders a `Char`, a `String` or a `nan`

**Sizing:** small. One rule stated in a sentence or a short section of an existing chapter; the
language owner ratifies it. It is not larger unless the owner wants an order other than the one
the code already has.

**Location:** `docs/spec/type-classes.md` or `docs/spec/evaluation-semantics.md` — wherever the
owner wants the ordering of the scalar types written; `std/core/src/Basics.zel` — the
`instance Comparable Char` and `instance Comparable String` bindings, and the doc comment of
`Comparable`; `std/core/src/Js/Utils.mjs` — `_Utils_lessByCodePoint`, which `ltChar` and
`ltString` are built over.

**Found while:** reviewing the PR for [`LANG-42`](README.md), which declares `Comparable` in
`std/core`. Left out of that PR because *A spec change and a semantics change do not share a
diff* and the rule is the owner's to ratify.

**Problem:** `Comparable`'s `Char` and `String` instances order by Unicode code point. A `String`
is ordered lexicographically by its code points, a string that is a prefix of another coming
first. `_Utils_lessByCodePoint` remaps UTF-16 surrogates so a character outside the Basic
Multilingual Plane orders by its code point rather than by its UTF-16 units, and breaks a tie on
length. A program can see this, and no chapter states it: it appears only in `Basics.zel`'s doc
comment and in a comment in `Js/Utils.zel`.

The spec already points that way. [Lexical structure](../spec/lexical-structure.md) makes a
character literal "one Unicode code point", and
[*Records and derivation*](../spec/records.md#records-and-derivation) orders record labels "each
compared by its code point". Ordering by UTF-16 unit would have made the JavaScript
representation part of the language, which is why the code does not.

**Approach:**

1. Ask the language owner to ratify code-point order for `Char` and `String`, `Int` numerically
   and `Float` by IEEE 754 (the last is already in
   [*Numbers*](../spec/evaluation-semantics.md#numbers) for the four operators).
2. Write it in one place and link to it from the other. Put it in the chapter the owner picks;
   `type-classes.md`'s section on what `std/core` declares and *Numbers* are the two candidates,
   and this ticket does not choose between them.
3. A `zel` block is not needed, since the rule has no source form that fails. If one is wanted, it
   is `expect=fragment`.
4. State the `nan` ruling above in the same place.

**The `nan` ruling, made by the language owner on the `LANG-42` PR:** `compare` answers `GT` for
any pair holding a `Float` that is `nan`, on either side. `lt` and `le` read `compare a b`, and
`gt` and `ge` read `compare b a`, so `<`, `<=`, `>` and `>=` are all `False` against a `nan`, as
[*Numbers*](../spec/evaluation-semantics.md#numbers) already says. `std/core/src/Basics.zel`
documents it on `Comparable`. What a chapter has to state beyond the four operators: `compare`
is not antisymmetric on a `nan` (`compare nan 1.5` and `compare 1.5 nan` are both `GT`), and
`min` and `max` depend on argument order (`min nan 1.5` is `1.5`, `min 1.5 nan` is `nan`). The
ruling is made; this ticket writes it down, beside the code-point rule.

**Acceptance:** `grep -n "code point" docs/spec/type-classes.md docs/spec/evaluation-semantics.md`
finds a sentence stating the order of `Char` and `String` under `Comparable`;
a sentence stating that `compare` answers `GT` for a `nan` on either side; `cargo test --test spec` passes; `std/core/tests/ComparableTests.zel`'s code-point cases, already
passing, are unchanged.
