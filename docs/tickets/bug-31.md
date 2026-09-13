# BUG-31 · `do_exports` accepts a `Lower`/`Upper` name that resolves only through an import

**Severity:** medium (wrong behaviour under normal use — a module can re-export a name it
never declared, which `docs/spec/modules.md` states outright is not allowed).

**Location:** `src/compiler/canonical/mod.rs` — `do_exports`'s `Lower` and `Upper` arms
(`ExposedKind::Lower`/`ExposedKind::Upper`); `src/compiler/canonical/environment.rs` —
`Environment::find_value`/`find_type`, and the `TypeArity` struct.

**Problem:** [`BUG-8`](../tickets/README.md) made `do_exports` check that a `Lower`/`Upper`
entry in a module's `exposing (...)` header resolves to *something* in scope, via
`find_value`/`find_type`. Neither lookup distinguishes a name the module declared itself from
one it only brought in with `import ... exposing (...)`, so a facade module can re-export an
imported name and the header is accepted:

```zel expect=ok package=reexport
module Facade exposing (label)

import Widget exposing (label)
```

`docs/spec/modules.md`'s *Everything exposed must be declared here* section states the rule
this violates: "A module may not re-export something it imported: a name that reaches other
modules through `Widget` is a name `Widget` declared." That section's `**Known gap:**`
paragraph already names this exact example and says it should be rejected once `do_exports`
can tell local from imported.

For values the two are distinguishable today without any new plumbing: `find_value` returns a
`&ValueType`, and that enum already separates `Local`/`TopLevel` (declared in this module) from
`Foreign`/`Foreigns` (brought in by import) — `do_exports`'s `Lower` arm just checks
`is_some()` rather than matching on which variant it got.

For types there is no equivalent signal. `find_type` returns a `&TypeArity`
(`environment.rs`), which carries only `name` and `variables` — nothing marks whether the
entry came from a local `type` declaration or from `process_import`'s `Upper` handling
inserting an imported type into the same environment. Fixing the `Upper` arm needs that
distinction to exist first; it does not.

**Fix:** two independent pieces:

- `Lower`: match `find_value(name)` on its variant instead of `is_some()`. `Local` or
  `TopLevel` is a local declaration and should pass exactly as it does today; `Foreign` or
  `Foreigns` should raise `Error::ExportNotFound(name.clone(), ExportType::Value, exposed.span)`,
  the same error the "resolves to nothing" case already raises.
- `Upper`: needs a local/foreign signal on `TypeArity` (or a parallel local-types set on the
  environment) before the same check is possible. That is a small piece of new state, not
  just a match arm, and is why this ticket does not just extend `BUG-8`'s fix directly — decide
  the shape of that signal before writing the check, mirroring how `ValueType` already carries
  it for values rather than inventing a second, differently-shaped mechanism.

**Acceptance:** `module Facade exposing (label)` importing `label` rather than declaring it
raises `Error::ExportNotFound(_, ExportType::Value, _)` with the label under the exposed name
in the `exposing (...)` list — a canonicalization test. A second test does the same for a
re-exported type. A module that both imports and locally declares a value or type of the same
name — shadowing, if that is even legal; check `LANG-29`/`LANG-30` before assuming it is —
should be handled deliberately rather than by accident of which lookup wins; if the two
tickets interact, say so rather than picking silently. `docs/spec/modules.md`'s
`package=reexport` block naming `Facade` goes from `expect=ok` to
`expect=canonical-error:ExportNotFound`, and its `**Known gap:**` paragraph is deleted.

**Related:** found while closing [`BUG-8`](../tickets/README.md) (PR #197), which fixed the
"resolves to nothing at all" half of the same `do_exports` arms; this ticket is the remaining
"resolves via import, not via local declaration" half the spec's known-gap paragraph already
flagged as unfixed.
