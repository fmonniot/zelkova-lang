# LANG-31 · A variant may use a type variable its declaration does not bind

**Sizing:** small.

**Location:** `src/compiler/canonical/mod.rs`, `do_types` — the `map` over `tpe.variants` builds
each variant's `type_parameters` with `Type::from_parser_type` and never compares the variables
it finds against `tpe.type_arguments`.

**Decided by:** [`docs/spec/name-resolution.md`](../spec/name-resolution.md)'s *Type variables*.

**Problem:** a `type` declaration's parameters are the binders for the type variables of its
variants, and a variant may use those and no others. Today an unbound one is accepted:

```
type Box
  = Box a
```

`Box` is a type with no parameters whose constructor takes an argument of type `a`, and nothing
can ever say what `a` is — there is no argument position that would fix it, so the constructor
cannot be applied to anything in particular and a match on it binds a value of no nameable
type. The declaration is meaningless rather than merely unusual, which is why it is an error
here and not a type error later.

**Fix:** in `do_types`, collect the `Type::Variable` names appearing anywhere in a variant's
`type_parameters` and check each is in `tpe.type_arguments`. A new `canonical::Error` variant
names the offending variable and carries the variant's span, so the caret sits under the
declaration that used it. Accumulate rather than short-circuit — one declaration may use
several — and keep it inside the existing `collect_accumulate` over variants.

Two things to check: the walk has to descend through `Type::Arrow` and `Type::Tuple`, not only
look at a variant's immediate arguments, since `= Box (a -> a)` is the same defect nested; and
`std/core/src/` must still compile.

**Acceptance:** the last `expect=ok` block of
[`docs/spec/name-resolution.md`](../spec/name-resolution.md) (`type Box = Box a`) goes **red** —
retag it `expect=canonical-error:` with the new variant and delete the **Known gap:** paragraph
above it. A `tests/compiler/canonical.rs` case asserting the variant, and one asserting that
`type Box a = Box a` and a nested `type Box a = Box (a -> a)` still compile, seen to fail before
the fix.
