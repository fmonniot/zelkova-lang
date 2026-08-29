# BUG-16 · An unresolved type name is invented rather than reported

**Severity:** medium (wrong behaviour under normal use — a misspelled type name is accepted
silently and surfaces later, if at all, as a type error about something else).

**Location:** `src/compiler/canonical/mod.rs` — `Type::from_parser_type`, the `None` arm of
its `env.find_type(name)` match; and `src/compiler/canonical/environment.rs` —
`process_import`'s `parser::ExposedKind::Upper(_, Privacy::Private)` arm.

**Problem:** two sites accept a type name that resolves to nothing and fabricate a type for
it instead of raising an error.

`from_parser_type` looks the name up and, on a miss, builds a `Type::Type(name, args)` out of
thin air:

```rust
parser::TypeKind::Unqualified(name, vars) => match env.find_type(name) {
    Some(t) => Ok(t.clone()),
    None => {
        // TODO Insert back into Environment ?
        Ok(Type::Type(name.clone(), types))
    }
},
```

So `label : Widgt` — or `label : Int` in a module that never imported `Basics` — canonicalizes
without complaint, and the invented type then flows into the typer as a distinct nominal type
that unifies with nothing.

`process_import`'s opaque-type arm does the same on the import side, and does not even consult
the interface it is importing from:

```rust
parser::ExposedKind::Upper(type_name, parser::Privacy::Private) => {
    let tpe = Type::Type(type_name.clone(), vec![]);
    env.types.insert(type_name.clone(), tpe);
}
```

Its two siblings do check — `Upper(_, Public)` reads `interface.unions` and raises
`EnvError::UnionNotFound`, `Lower` reads `interface.values` and raises
`EnvError::ValueNotFound`, both with an `ERR-7` suggestion. So `import Widget exposing (Size)`
is checked when written `Size(..)` and unchecked when written `Size`, which is the same entry
differing only in whether the constructors come along.

The two sites are one behaviour and are worth fixing together: the import arm is where a
wrong name enters the environment, and `from_parser_type` is where every other wrong name
gets past. Fixing only the import arm leaves annotations unchecked; fixing only
`from_parser_type` leaves a fabricated type sitting in the environment where it will resolve.

Found while writing [`docs/spec/modules.md`](../spec/modules.md) (`SPEC-3`), whose *What an
import's `exposing` list does* section carries the `**Known gap:**` block for the import arm.
The annotation site is not shown there — it belongs to the planned *Types and type
annotations* chapter — and is recorded here so the fix covers both.

**Fix:** in `process_import`, look `type_name` up in `interface.unions` before inserting, and
raise `EnvError::UnionNotFound` with a `suggest_name` suggestion when it is absent — the
`Public` arm already does exactly this and is the model; keep inserting the type *without* its
constructors, which is what makes the entry opaque.

In `from_parser_type`, return a canonicalization error instead of fabricating. That needs a
new `canonical::Error` variant naming the type and carrying `tpe.span` — `parser::Type` has a
span (`ERR-3`) so the caret lands under the name. Two things to check before assuming it is
mechanical: type *variables* arrive as `TypeKind::Variable` and must keep resolving to
nothing; and `std/core/src/` must still compile, which it will only once the modules it uses
genuinely have their types in scope. If it does not, say so rather than working around it —
that is [LANG-8](lang-8.md)'s default imports being needed first, and the two should be
sequenced rather than merged.

**Acceptance:** `import Widget exposing (Missing)`, where `Widget` declares no `Missing`,
fails with `EnvError::UnionNotFound` — a test beside the existing `UnionNotFound` coverage in
`tests/compiler/canonical.rs`. A second test asserts `label : Nope` in a module that declares
no `Nope` fails with the new error, and that `label : a` (a type variable) still compiles.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked. The
`**Known gap:**` block in `docs/spec/modules.md` (the `package=missing-type` pair) goes red on
its `expect=ok` tag and is retagged with its paragraph deleted.
