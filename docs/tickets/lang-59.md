# LANG-59 · A scalar type's declaration is an ordinary union, so `Int` is a value and any body is accepted

**Sizing:** small. One check at one site, plus the error variant it raises and the constructor
registration it suppresses.

**Location:** `src/compiler/canonical/mod.rs` — `do_types` at :1687, which walks a module's
`type` declarations; `src/compiler/canonical/environment.rs` — `insert_union_type` at :766,
which registers a declaration's constructors. In the tree: `std/core/src/Basics.zel:123` and
`:146`, and `std/core/src/Char.ignored:62` and `String.ignored:93` for when those compile.

**Decided ([`DEC-15` decision
2](../decisions/dec-15.md#2--a-scalar-type-is-declared-in-zelkova-and-an-opaque-ones-declaration-names-itself),
by the language owner):** `Int`, `Float`, `Char` and `String` are opaque. Each is declared in
Zelkova — that declaration is where a reader finds out what the type is — and it writes the
type's own name and contributes no constructor. [*Scalar
types*](../spec/types.md#scalar-types) states the rule.

**Depends on:** [BUG-26](bug-26.md). The check has to fire on `Basics`' `Int` and not on a
module's own, which needs the qualified name.

**Renumbered from LANG-54** on 2026-09-15: that ID had already been used and closed (the
interop modifier is `foreign`, not `javascript`). See `docs/tickets/README.md`'s tombstone row.

**Problem:** `type Int = Int` at `std/core/src/Basics.zel:123` is read as a one-constructor
union, so `do_types` registers a constructor `Int` and `Int` is a value of type `Int`. Nothing
in the language can build an `Int`, which makes that constructor a way to produce a value the
target has no representation for.

The body is also unchecked in the other direction. `type Int = I32` in `Basics` compiles today:
`I32` resolves to nothing and a type is fabricated for it ([`BUG-16`](bug-16.md)), so the
declaration a reader goes to `Basics.zel` to read can say anything at all.

**Fix:** in `do_types`, recognise a declaration whose qualified name is one of the four opaque
scalars. Require its variant list to be exactly the type's own name with no arguments, raise a
canonicalization error naming the type otherwise, and register no constructor for it.

`Bool` is not one of the four. `type Bool = True | False` at `Basics.zel:458` is its genuine
definition and stays an ordinary union — [`DEC-15` decision
5](../decisions/dec-15.md#5--bool-is-a-scalar-and-an-ordinary-union-and-both-at-once).

**Acceptance:** `Int` used as a value in a module that can see `Basics`' `Int` is an unresolved
name; `type Int = I32` in `Basics` is a canonicalization error carrying the declaration's span;
`type Int = Int` in a module that is not `Basics` is an ordinary union whose `Int` is still a
value. Tests in `tests/compiler/canonical.rs`, each checked by reverting the branch and
watching it go red. `cargo run` still prints `parsed 8 modules`, lists all eight as checked,
and exits 0. The **Not implemented:** paragraph closing [*Scalar
types*](../spec/types.md#scalar-types) loses its clause about the body being unchecked.
