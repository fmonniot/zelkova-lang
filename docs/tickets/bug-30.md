# BUG-30 · An `Upper(..)` import entry does not check the type was exposed transparently

**Severity:** medium (wrong behaviour under normal use — an importer asking for a type's
constructors gets a silent, constructor-less type instead of a diagnostic, and the mistake
resurfaces later as an unrelated-looking unresolved-constructor error).

**Location:** `src/compiler/canonical/environment.rs` — `process_import`'s
`parser::ExposedKind::Upper(type_name, parser::Privacy::Public)` arm (the `Foo(..)` entry in
an import's `exposing` list).

**Problem:** the arm only checks that the exporting module's interface has *an* entry for
`type_name`:

```rust
parser::ExposedKind::Upper(type_name, parser::Privacy::Public) => {
    let union = interface.unions.get(type_name).ok_or_else(|| {
        let suggestion = suggest_name(type_name, interface.unions.keys().cloned());
        EnvError::UnionNotFound(type_name.clone(), exposed.span, suggestion)
    })?;

    insert_foreign_union_type(
        env,
        None,
        type_name,
        &union.variables,
        union.variants.iter(),
    );
}
```

Since `BUG-9`, `Module::to_interface` (`src/compiler/canonical/mod.rs`) can put an entry into
`interface.unions` whose `variants` is deliberately empty — that is what an opaque export
(`UnionVisibility::Opaque`, a bare `Size` in the exporting module's own `exposing` header)
looks like on the interface. The `Public`/`(..)` arm above cannot tell that case apart from a
type that genuinely has zero constructors: it finds the entry, reads whatever `variants` holds,
and inserts it — silently, with no error either way.

So `import Lib exposing (Opaque(..))`, where `Lib` exposes `Opaque` only as a bare `Opaque` in
its own header (not `Opaque(..)`), succeeds. The importing module ends up with a type named
`Opaque` and no constructors, exactly as if `Lib` had genuinely declared a zero-constructor
type. Nothing at the `import` line says the request for transparency was denied. The mistake
only surfaces if the importer then tries to use one of `Opaque`'s constructors, as an
unrelated-looking "cannot find a type constructor" error far from the line that actually asked
for something it wasn't given.

This is a different condition from `BUG-16` (closed): `BUG-16` was
`interface.unions.get(type_name)` returning `None` — a name the interface does not know about
at all — and its fix is exactly the existence check this arm already has. This ticket's gap is
a name the interface *does* know, but only opaquely, being asked for transparently. The sibling
`Upper(_, Privacy::Private)` arm (a bare `Foo` entry, asking for the type opaquely) is
unaffected: it reads `interface.unions.get(type_name).map(|u| u.variables.clone())` and never
looks at `variants`, so asking opaquely for an opaquely-exposed type is exactly right — the
defect is specific to the `Public` arm reading a `variants` field that may have been emptied by
the exporter's own header rather than genuinely being zero-constructor.

Found in review of PR #196 (`BUG-9`), which is what gave `interface.unions` its first
constructor-emptying arm; confirmed not already covered by `BUG-16`'s own text, whose `Fix`
section proposes only the existence check.

**Fix:** the `Public` arm needs to tell "the interface has no opinion, because the type
genuinely has no constructors" apart from "the interface withheld the constructors on
purpose." `UnionType`'s own shape (`variables`, `variants: Vec<TypeConstructor>`, `span`)
cannot make that distinction today — an empty `Vec` means both. Two directions, and this
ticket does not pick between them:

1. Track opacity explicitly, e.g. a `UnionVisibility`-shaped field on `Interface`'s union
   entries (or a parallel `HashSet<Name>` of opaquely-exposed type names) that
   `Module::to_interface` populates alongside `unions`, and that the `Public` arm consults
   before trusting an empty `variants` as "no constructors" rather than "not offered."
2. Reject any `Upper(_, Privacy::Public)` request whose resolved entry has empty `variants`
   outright. Cheaper, but wrong for the genuine case of an exported type that really has zero
   variants (todo: check whether the grammar allows declaring one at all before ruling this
   out as always safe).

Whichever is chosen, the new error should reuse `EnvError::UnionNotFound`'s shape or add a
sibling variant — `docs/spec/modules.md`'s *Operators*/*exposing* sections have no claim about
this case today, so no spec block needs retagging unless the fix adds one.

**Acceptance:** a test where `Lib` exposes `Opaque` bare (`type Opaque a = Wrapped a`, header
`exposing (Opaque, ...)`) and `Main` does `import Lib exposing (Opaque(..))`, asserting a clean
canonicalization error at the `Opaque(..)` entry's own span rather than success. A regression
test that `Lib` exposing `Clear(..)` transparently in its own header still lets
`import Lib exposing (Clear(..))` succeed, so the fix does not turn off the legitimate case.
