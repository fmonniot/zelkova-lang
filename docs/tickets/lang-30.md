# LANG-30 · Ambiguity is detected for values only; a type, constructor or operator is taken from the last import

**Sizing:** medium.

**Location:** `src/compiler/canonical/environment.rs` — `insert_foreign_union_type` (both
`env.types.insert` and `env.constructors.insert`), and `process_import`'s `Exposing::Open` and
`ExposedKind::Operator` arms (`env.infixes.insert`). `insert_foreign_value` is the one that
gets this right, and is the model.

**Decided by:** [`docs/spec/name-resolution.md`](../spec/name-resolution.md)'s *Ambiguous
rather than unresolved*.

**Problem:** a name brought into scope unqualified by two imports is ambiguous at each place it
is written, in every namespace. Values already work that way: `insert_foreign_value` turns a
second `Foreign` into a `ValueType::Foreigns`, and `Expression::from_parser` raises
`Error::AmbiguousVariables` when a use lands on one.

Nothing else does. `env.types`, `env.constructors` and `env.infixes` are plain maps and a second
import overwrites the first, so which module a type, a constructor or an operator comes from is
decided by which `import` line was written last:

```
import Widget exposing (Size(..))
import Gadget exposing (Size(..))

y : Size
y =
  Small
```

`Size` and `Small` are Gadget's, silently. Swapping the two lines changes what the module means
and nothing marks either line as significant. `canonical::Error::AmbiguousVariants` exists for
exactly this and is constructed nowhere — its doc comment says so.

Operators are the same defect with a different fix available to the user: there is no qualified
spelling for one, so an ambiguous operator can only be resolved by editing an `import` line.
That does not change the rule, and it does mean the diagnostic should say so rather than
suggesting a qualified name.

**Approach:** three namespaces, one shape, and it is `insert_foreign_value`'s: the map's value
type grows a "more than one provider" case, and the lookup site raises when it lands on one.

- **Types and constructors.** `env.types` holds a `Type` and `env.constructors` a
  `TypeConstructor`, so each needs a wrapper — the `ValueType` equivalent — carrying either the
  single resolved entry with its module and `SourceSpan`, or the list of candidates.
  `Environment::find_type` and `find_type_constructor` return `Option<&Type>` /
  `Option<&TypeConstructor>` today and would return the wrapper instead; the callers are
  `Type::from_parser_type`, `Pattern::from_parser` and the `TypeConstructor` arm of
  `Expression::from_parser`. The constructor sites raise `AmbiguousVariants`, which already
  exists and already renders. The type site needs a new variant, since ambiguity in a type
  expression is not a variant of anything — and note that `from_parser_type` currently invents a
  type on a miss ([`BUG-16`](bug-16.md)), so the two touch the same match and are worth
  sequencing rather than merging.
- **Operators.** `env.infixes` maps to `Infix`; the same wrapper applies. The lookup is
  `RootEnvironment::find_value`'s redirect through `infixes`, which is also
  [`BUG-15`](bug-15.md)'s subject — that ticket rewrites how an operator resolves, and doing
  this half on top of the redirect as it stands would be building on the thing BUG-15 removes.
  Land BUG-15 first.

A local declaration is never one of the candidates: a name declared in this module and imported
unqualified is [`LANG-29`](lang-29.md)'s collision, reported at the declaration rather than at
each use.

**Acceptance:** the `module Other` and `module Third` blocks of the `package=ambiguous` group in
[`docs/spec/name-resolution.md`](../spec/name-resolution.md) go **red** — retag each
`expect=canonical-error:` with the variant it now raises and delete the **Known gap:** paragraph
above them. `tests/compiler/canonical.rs` cases for an ambiguous type, an ambiguous constructor
and an ambiguous operator, each seen to fail before the fix, plus one asserting that a qualified
spelling still resolves when the unqualified one is ambiguous.
