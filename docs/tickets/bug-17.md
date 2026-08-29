# BUG-17 · A type application's arguments are discarded when its head resolves

**Severity:** high (a miscompile in waiting — `f : Maybe Int` and `f : Maybe Char` become the
same type, so the type checker accepts a body that disagrees with its own annotation).

**Location:** `src/compiler/canonical/mod.rs` — `Type::from_parser_type`, the `Some(t)` arm of
its `env.find_type(name)` match:

```rust
parser::TypeKind::Unqualified(name, vars) => match env.find_type(name) {
    Some(t) => Ok(t.clone()),          // <- `vars` is never looked at
    None => { … Ok(Type::Type(name.clone(), types)) }
},
```

**Problem:** `parser::TypeKind::Unqualified(name, vars)` is a type application — `name` applied
to `vars`. When `name` resolves, the arm returns the environment's stored type *and drops
`vars` entirely*. What the environment stores is the declaration's own shape:
`RootEnvironment::insert_union_type` builds `Type::Type(name, [Variable(v) for v in
union.variables])`, so `type Maybe a = …` stores `Maybe a`.

The result is that a written argument is replaced by the declaration's type variable:

```zel
type Maybe a
  = Just a
  | Nothing

f : Maybe Int
f = Nothing
```

canonicalizes to `tpe: Type("Maybe", [Variable("a")])`. `Int` is gone.

Three consequences, verified by probing:

- **The annotation stops constraining.** `f : Maybe Int` with `f = Just 'c'` passes
  `check_module` with no error at all. The annotation the reader trusts is not the type the
  compiler checks.
- **Arity is never checked**, because there is nothing left to check it against. Both
  `f : Maybe` and `f : Maybe Int Int` canonicalize to the same `Maybe a`.
- **The behaviour is asymmetric.** An *unresolved* head keeps its arguments — `f : Foo Int`
  becomes `Type("Foo", [Type("Int", [])])` via the `None` arm. So a misspelt type name is
  more faithfully represented than a correct one.

The `None` arm has its own defect ([BUG-16](bug-16.md), which replaces the fabrication with an
error). The two are in the same match and are worth reading together, but they are separate
bugs: BUG-16 is about a name that resolves to nothing, this one is about a name that resolves.

Found while writing [`docs/spec/types.md`](../spec/types.md) (`SPEC-5`).

**Fix:** the `Some` arm has to *apply* the resolved head to the canonicalized arguments rather
than return it as stored. Two things to settle first, neither mechanical:

- What the environment should store. `insert_union_type` stores a fully-applied `Maybe a`,
  which is a type rather than a type constructor; applying arguments to it means substituting
  `union.variables` positionally, or changing what is stored to carry the arity and the
  variable names separately. The second is cleaner and touches `Environment::find_type`'s
  signature and every caller.
- Where an arity mismatch is reported. Once the arguments survive, `Maybe` with no argument
  and `Maybe Int Int` are both detectable in `from_parser_type`, and each wants a new
  `canonical::Error` variant naming the type, its declared arity and the arity written, with
  `tpe.span` for the caret (`parser::Type` carries one — `ERR-3`).

`std/core/src/` must still compile: `Maybe.zel` and `Result.zel` are full of `Maybe a`,
`Result x a` and `Result x b`, all of which currently canonicalize to their declaration shape
and will start being distinguished from each other.

**Acceptance:** `f : Maybe Int` with `f = Just 'c'` is a type error, and `f : Maybe Int` with
`f = Just 1` is not — a test in `tests/typer.rs`, which is the layer that can see the
difference. A canonicalization test asserts the new arity errors for `f : Maybe` and
`f : Maybe Int Int`. `cargo run` still prints `parsed 8 modules` and lists all eight as
checked.

In [`docs/spec/types.md`](../spec/types.md), the two `expect=ok` blocks under *Applying a type
to arguments* — `f : Maybe` and `f : Maybe Int Int` — go red on their tags and are retagged
with their `**Known gap:**` paragraph deleted. The third gap that section describes, the
annotation that stops constraining, has **no red test behind it**: the spec harness stops at
canonicalization and never runs the typer, so the block showing it compiles either way. Delete
that paragraph by hand as part of this ticket. [TEST-2](test-2.md) is what would have caught
it.
