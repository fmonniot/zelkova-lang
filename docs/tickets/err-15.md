# ERR-15 · `TypeNotFound` carries no "did you mean …?" suggestion

**Sizing:** small.

**Location:** `src/compiler/canonical/mod.rs` — the `Error::TypeNotFound(Name, NodeSpan)`
variant and the `None` arm of `Type::from_parser_type`'s `TypeKind::Unqualified` match that
raises it (`env.find_type(name)` returning nothing); `src/compiler/canonical/environment.rs` —
the `Environment` trait, `suggest_name`, and `RootEnvironment`/`ScopedEnvironment`'s
`value_names`/`type_constructor_names`, which are the pattern to mirror.

**Problem:** `TypeNotFound` was added closing BUG-16 (an unresolved type name used to be
invented rather than reported; see [the index](README.md) for that tombstone) to carry only the
name as written and the span of the type application. Its two closest siblings,
`VariableNotFound` and `VariantNotFound`, both carry an `Option<Name>` "did you mean …?"
suggestion — built with `suggest_name(name, env.value_names().into_iter())` and
`suggest_name(name, env.type_constructor_names().into_iter())` respectively, at the point each
is raised — a mechanism `ERR-7` (see [the index](README.md) for its tombstone) added for
exactly this class of error. A module writing `label : Widgt` where `Widget` is declared gets a
bare "no type of this name is in scope" today, with nothing pointing at the name one edit
distance away.

Deliberately out of scope for BUG-16's own ticket, which asked only for a variant naming the
type and carrying `tpe.span` — raised in review on the PR that closed it.

**Fix:** the `Environment` trait has `value_names()` and `type_constructor_names()` but no
equivalent for type names; add `type_names() -> Vec<Name>` beside them, implemented on
`RootEnvironment` as `self.types.keys().cloned().collect()` and on `ScopedEnvironment` by
delegating to `self.parent.type_names()` (a scope never binds a type, so there is nothing to
add at that layer — `find_type` already delegates the same way). At the `None` arm of
`Type::from_parser_type`, compute `suggest_name(name, env.type_names().into_iter())` and add it
as a fourth field on `TypeNotFound`, matching the `Option<Name>` shape of the other two
variants. `TypeNotFound`'s `labels()` arm (`src/compiler/canonical/mod.rs`, currently
`primary(span, "no type of this name is in scope")`) appends `suggestion_suffix(&suggestion)`
the same way `VariableNotFound`'s and `VariantNotFound`'s arms do.

**Acceptance:** a `tests/compiler/canonical.rs` case with a module declaring `Widget` (or
importing it) and a second module naming `Widgt` in a type annotation, asserting the returned
`TypeNotFound`'s suggestion is `Some("Widget")`. A second case with no near-miss in scope
asserts the suggestion is `None`, so the field cannot be filled in unconditionally. Mutation-check
both by reverting the `suggest_name` call to `None` and confirming the first case goes red.
