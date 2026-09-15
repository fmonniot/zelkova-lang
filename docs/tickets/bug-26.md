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

**Fix:** the four literal types are properties of the *type the name resolved to*, not of the
spelling. Nothing in the canonical AST distinguishes them today, which is what makes this more
than a one-line change: `canonical::Type::Type` carries a `Name`, and the typer has to decide
whether a nullary named type is a builtin from what the name resolved to rather than from its
letters.

[`DEC-15`](../decisions/dec-15.md) settled where those four come from and narrows the shapes
worth weighing. They are [built-in type names](../spec/types.md#built-in-type-names) the
compiler supplies, so *giving them a real declaration in `std/core` and dropping
`Type::Literal`* is no longer available — `Int` and `Float` have no declaration anyone could
write, and the decision is that a module is not what answers for them. What the same decision
also grants is [that a module may shadow one](../spec/types.md#built-in-type-names), which is
exactly the case this bug gets wrong: the fix is to carry the distinction from the canonical
phase, where the names `builtin_types` seeds into a scope are what a built-in resolved against,
rather than to re-derive it in the typer from the spelling.

**Acceptance:** the module above type checks, and its `case` still rejects a branch of the
wrong type — tests in `tests/typer.rs`. `cargo run` still prints `parsed 8 modules` and lists
all eight as checked. The `**Known gap:**` block in
[`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s *Reserved words* section goes
red on its `expect=type-error:UnificationFailed` tag and is retagged `expect=ok` with its
paragraph deleted.
