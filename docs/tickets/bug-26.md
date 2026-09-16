# BUG-26 · A module that declares `Bool`, `Int`, `Char` or `Float` cannot annotate anything with it

**Severity:** medium (wrong behaviour under normal use — a correct module is rejected with a
type error that names one type twice: "cannot match `Bool` with `Bool`").

**Location:** `src/compiler/typer/mod.rs` — the first four arms of
`canonical_type_to_typer_type`, which match on the *name* of a nullary type and hand back a
`Type::Literal`; and `type_check`'s second pass, which registers every constructor of every
`module.types` entry at `Type::Adt(type_name, args)`.

**Problem:** the two disagree about what a locally declared `Bool` is. The annotation side
maps the bare name to `Type::Literal(TypeLiteral::Bool)`:

```rust
canonical::Type::Type(name, args) if args.is_empty() && name.as_str() == "Bool" => {
    Some(Type::Literal(TypeLiteral::Bool))
}
```

while the constructor side gives `True` and `False` the type `Adt("Bool", [])`. The two never
unify, so this module — which is the language's own definition of `Bool`, and what
`std/core/src/Basics.zel` declares — fails to type check:

```zel
module Example exposing (Bool, not)

type Bool
  = True
  | False

not : Bool -> Bool
not b =
  case b of
    True ->
      False

    False ->
      True
```

The error is `UnificationFailed { left: Adt("Bool"), right: Literal(Bool) }`, which `Display`
writes as *cannot match `Bool` with `Bool`*. Renaming the type to anything outside the four
names — `Flag`, say — makes the same module check. The match is on the name alone, so `Int`,
`Char` and `Float` have the same defect; `Bool` is the one a program hits first, because it is
the only one of the four the language expects a module to declare.

`std/core/src/` does not fail today only because `Basics`' boolean functions are `Js.Basics`
facade values — `not`, `and`, `or` and `xor` at `std/core/src/Basics.zel:466-513` — whose
bodies `value_to_term_and_annotation` cannot express and therefore skips entirely. The first
hand-written `Bool` function in `std/core` hits this.

Found while extending the spec harness to run the type checker (`TEST-2`);
[`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s *Reserved words* section
carries the `**Known gap:**` block for it.

**Decided ([`DEC-15` decision
1](../decisions/dec-15.md#1--a-scalar-type-is-known-by-its-qualified-name), by the language
owner):** a [scalar type](../spec/types.md#scalar-types) is known by the **qualified name of
its declaration**, never by its spelling. A module declaring its own `Bool` declares an
ordinary type that shares four letters with a scalar, and every phase treats it as one.

**Depends on:** [AST-4](ast-4.md), which is where most of the work is. A canonical type carries
a bare `Name`, so there is nothing for the typer to match a qualified name against until that
lands.

**Blocks:** [LANG-58](lang-58.md) and [LANG-59](lang-59.md), both of which need the compiler to
hold the scalar names before they can seed or check them.

**Fix:** hold the five names the compiler knows — `Basics.Int`, `Basics.Float`, `Basics.Bool`,
`Char.Char` and `String.String` — and have `canonical_type_to_typer_type` match on those rather
than on `name.as_str()`. `TypeLiteral` has four variants and gains no fifth here: `String` is a
scalar for [the boundary](../spec/interop.md#which-types-may-cross-the-boundary) and for
[LANG-58](lang-58.md)'s seeding, and needs no typer arm until there is a string literal to give
a type to.

This is the second of the two shapes this ticket used to weigh. The first — dropping
`Type::Literal` for `Type::Adt` throughout, on the strength of the four having real
declarations in `std/core` — is ruled out by the same decision: a scalar's representation
belongs to each target, and an opaque one has no constructors for an `Adt` to carry
([`DEC-15` decision
2](../decisions/dec-15.md#2--a-scalar-type-is-declared-in-zelkova-and-an-opaque-ones-declaration-names-itself)).
[`LANG-41`](lang-41.md) is unaffected either way: retiring `Type::Number` leaves an integer
literal with the type `Int`, which is `Basics.Int` under this fix.

**Acceptance:** the module above type checks, and its `case` still rejects a branch of the
wrong type — tests in `tests/typer.rs`. A module declaring its own `Int` and a module using
`Basics`' `Int` are two distinct types to the typer, and only the second is admitted at a
facade boundary. `cargo run` still prints `parsed 8 modules` and lists all eight as checked.
Two tagged blocks go red and are retagged `expect=ok` with their paragraphs deleted: the
`**Known gap:**` block in [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s
*Reserved words* section, and the `expect=type-error:UnificationFailed` block in
[*Scalar types*](../spec/types.md#scalar-types).
