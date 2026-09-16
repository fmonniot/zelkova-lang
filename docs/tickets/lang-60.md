# LANG-60 · The typer gives `Bool` a literal type, so inside `Basics` it does not match `True` and `False`

**Sizing:** small-to-medium. Deleting one `TypeLiteral` variant and following the compile
errors through `constraint.rs`, `unifier.rs` and their unit tests. What could make it bigger is
a spec block or test that only passes because an `if` condition is checked against a type no
module declares; none is known.

**Location:** `src/compiler/typer/mod.rs` — `TypeLiteral::Bool`; `scalar_literal`'s
`scalars::BOOL` entry; `translate_pattern`'s `PatternKind::Bool` arm; `Type`'s `Display` arm for
`Literal(Bool)`. `src/compiler/typer/constraint.rs` — `collect`'s `TypedTermKind::Bool` and
`TypedTermKind::If` arms, and the `constrains_bool` / `if` unit tests.
`src/compiler/typer/unifier.rs` — the `(Literal(Bool), Literal(Bool))` arm and the unit tests
built from it. `src/compiler/scalars.rs` — `BOOL`, which stays.

**Decided ([`DEC-15` decision
5](../decisions/dec-15.md#5--bool-is-a-scalar-and-an-ordinary-union-and-both-at-once), by the
language owner):** `Bool` is a scalar *and* an ordinary union. `type Bool = True | False` in
`Basics` is its whole definition; what the compiler knows about it is only its representation on
each target, and nothing about its structure. [*Scalar types*](../spec/types.md#scalar-types)
states it, and [*`if … then … else`*](../spec/expressions.md#if--then--else) requires a
condition to be a `Bool` — which, by [decision
1](../decisions/dec-15.md#1--a-scalar-type-is-known-by-its-qualified-name), is `Basics.Bool`.

**Depends on:** `BUG-35`, which is closed — `Type::Adt` carries the qualified name, so an `if`
checked against `Adt(Basics.Bool)` does not accept a module's own `type Bool`, which decision 1
rules out.

**Problem:** the typer still has a literal type for `Bool`, a representation of the kind
decision 2 reserves for the four *opaque* scalars. Since `BUG-26`, `Bool` in an annotation
becomes `Type::Literal(TypeLiteral::Bool)` exactly when it names `Basics.Bool`, while `True` and
`False` are registered, like every constructor, at the union's `Type::Adt`. The two only meet
inside `Basics`, and there they do not unify:

```zel
module Basics exposing (Bool(..), yes)

type Bool = True | False

yes : Bool
yes = True
```

fails with *cannot match `Bool` with `Bool`*. *Scalar types* pins it as a `**Known gap:**`
block tagged `expect=type-error:UnificationFailed`. `std/core` does not hit it today only because
`Basics`' `not`, `and`, `or` and `xor` are facade values, which `value_to_term_and_annotation`
skips; the first hand-written `Bool` function there will.

The literal type is also what the other three built-in uses reach for: an `if` condition, the
`true`/`false` keywords, and a `true`/`false` pattern all constrain against `Literal(Bool)`.

**Approach:**

1. Delete `TypeLiteral::Bool` and `scalar_literal`'s `BOOL` row, so a `Basics.Bool` annotation
   takes `canonical_type_to_typer_type`'s ordinary `Type::Adt` path. `scalars::BOOL` stays: it is
   still a scalar for the facade boundary ([`LANG-43`](lang-43.md)) and for
   [`LANG-58`](lang-58.md).
2. Point the three built-in uses at `Type::Adt(Basics.Bool, [])`, built from `scalars::BOOL`:
   `collect`'s `If` arm (keeping `Reason::IfCondition`), its `Bool` arm, and
   `translate_pattern`'s `PatternKind::Bool` arm. The last two go away entirely with
   [`LANG-1`](lang-1.md); this ticket does not wait for it.
3. Rewrite the unit tests in `constraint.rs` and `unifier.rs` that build `Literal(Bool)`.

Code generation, when it exists, asks `scalars::BOOL` how to represent `True` and `False`; the
typer does not need to know.

**Acceptance:**

- *Scalar types*' `Basics` block goes red, is retagged `expect=ok`, and its `**Known gap:**`
  paragraph is deleted — `cargo test --test spec`.
- In `tests/typer.rs`: a module named `Basics` declaring `type Bool = True | False` checks `yes :
  Bool` / `yes = True` and a `case` over it; with `basics_interface()` in scope, `if` on a `Bool`
  parameter still checks, and `if` on a parameter annotated with a module's *own* `type Bool` is
  rejected with a `UnificationFailed` naming both. Mutation-check the last by building the `if`
  constraint from the unqualified name.
- `if_non_bool_condition` keeps its label and its *the condition of an `if` must be a `Bool`*
  note.
- `grep -rn "TypeLiteral::Bool" src` is empty.
- `cargo run` still prints `parsed 8 modules`, lists all eight as checked, and exits 0.
