# BUG-26 · A module that declares `Bool`, `Int`, `Char` or `Float` cannot annotate anything with it

**Severity:** medium (wrong behaviour under normal use — a correct module is rejected with a
type error that names one type twice: "cannot match `Bool` with `Bool`").

**Location:** `src/compiler/typer/mod.rs` — the first four arms of
`canonical_type_to_typer_type`, which match on the *name* of a nullary type and hand back a
`Type::Literal`; and `type_check`'s second pass, which registers every constructor of every
`module.types` entry at `Type::Adt(type_name, args)`.

**Problem:** the two disagree about what a locally declared `Bool` is. The annotation side
reads the unqualified half of the name and maps it to `Type::Literal(TypeLiteral::Bool)`:

```rust
canonical::Type::Type(name, args)
    if args.is_empty() && name.unqualified_name().as_str() == "Bool" =>
{
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

**Depends on:** AST-4, which was most of the work and is closed — the qualified name
`canonical::Type::Type` now holds is what the typer matches against.

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

**Also in scope, established 2026-09-15:** the fix is a handful of lines and the work is
everything downstream of them. Once a scalar is its qualified name, a bare `Int` in a module
with no `Basics` in scope stops being one — and that describes almost every test module and
spec block in the tree, because neither harness models the [default
imports](../spec/modules.md#the-default-imports) at all. Measured against the fix above:
11 tests in `tests/typer.rs`, 11 in `tests/pipeline.rs` and 6 `expect=ok` blocks in
[`docs/spec/modules.md`](../spec/modules.md) go red, on top of the two blocks this ticket
*wants* red. None of it is fixed by [`LANG-57`](lang-57.md) or [`LANG-58`](lang-58.md) — those
tests hand `check_module` an empty interface map, and `LANG-58` seeds only the core modules
that drop the `Basics` entry — so it cannot be sequenced away and lands here. Four pieces:

1. A `basics_interface()` beside `maybe_interface()` in `tests/support/mod.rs`, declaring
   `Basics.Int`, `Basics.Float` and `Basics.Bool`, for the standalone module harnesses to
   resolve against. That is what the default imports would have given the module, built by
   hand because nothing else in a single-package compile can: under `LANG-57`'s rule a fixture
   package holding its own `Basics` is *core* and receives none of the eight, and one without
   has no interface to find.
2. `an_unresolved_qualified_scalar_name_is_not_the_scalar` (`tests/typer.rs`) re-points from
   `f : Basics.Int -> Int` to `f : Missing.Int -> Int`, because `Basics.Int` now resolves. The
   test gets sharper rather than weaker: with the real `Basics` in scope, reading the written
   module half off the unresolved name would make the module check clean, which is exactly
   what it pins.
3. [`LANG-55`](lang-55.md) folded in and closed with this ticket. Its two
   `Unqualified::Nothing` → `Unqualified::Type` fields are what make a bare `Char` resolve at
   all; without them `char_literal_has_type_char` and `tuple_triple_typechecks` have no
   spelling to use. Its own acceptance — a module resolving `Char` and `String` unqualified —
   has nothing to test against until this ticket's harness exists.
4. The examples that cannot reach a hand-built interface are rewritten to declare a local
   type: the fixture packages under `tests/fixtures/` and the `check_importer` pair in
   `tests/pipeline.rs`, and the six `docs/spec/modules.md` blocks, which annotate `label : Int`
   incidentally in a chapter about module headers. A `type Label = Label` in each states the
   same thing about `exposing` and stops relying on a spelling coincidence (language owner,
   this session).

Measured, not predicted: with pieces 1-3 applied, `tests/typer.rs` is 27/27 green and
`tests/compiler_tests.rs` unchanged; piece 4 is what the remaining 10 `tests/pipeline.rs`
failures and the 6 spec blocks need.

**Acceptance:** the module above type checks, and its `case` still rejects a branch of the
wrong type — tests in `tests/typer.rs`. A module declaring its own `Int` and a module using
`Basics`' `Int` are two distinct types to the typer, and only the second is admitted at a
facade boundary. `cargo run` still prints `parsed 8 modules` and lists all eight as checked.
Two tagged blocks go red and are retagged `expect=ok` with their paragraphs deleted: the
`**Known gap:**` block in [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s
*Reserved words* section, and the `expect=type-error:UnificationFailed` block in
[*Scalar types*](../spec/types.md#scalar-types).
