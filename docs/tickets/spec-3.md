# SPEC-3 · Write the Modules, `exposing` and imports chapter, and settle multi-module examples

**Sizing:** medium. The chapter is one file; the harness change it needed is the other half.

**Location:** [`docs/spec/modules.md`](../spec/modules.md) — new; `tests/spec.rs` — the
`package=` grouping and the `expect=dependency-error` expectation; `docs/spec/INDEX.md` — the
vocabulary table and the *One module per block* question this closes.

**Problem this addressed:** the module system was the largest unwritten part of the language,
and the one chapter that could not be written under the existing harness rule of one module
per block — nothing about `import` can be shown with a single module in scope.
`docs/spec/INDEX.md` listed four candidate mechanisms for that and said whichever chapter
needed it first would decide. This is that chapter.

**What it settled**, none of which was written down anywhere before:

- **Multi-module examples.** Blocks sharing a `package=<label>` in one chapter are one
  package: parsed together, ordered by their imports, canonicalized against each other's
  `Interface`s. Each block keeps its own `expect=`, so an example can show a module compiling
  and its importer failing. Chosen over the three alternatives because a per-block verdict is
  what makes a two-module example worth having — the interesting case is almost always one
  module fine and the other not. A block with no `package=` is unchanged.
- **`expect=dependency-error`**, the one expectation that belongs to a group rather than a
  module: the package has no valid import order. It exists so the no-cycles rule is checked
  rather than merely asserted.
- The module header: `exposing` is mandatory, a module name's segments are uppercase-initial,
  the header is one layout block.
- A module's name must match its file's path, and no two files may declare one module name
  ([LANG-6](lang-6.md)).
- The four `exposing` entry forms, that a type is opaque or fully open with nothing in
  between, that a trailing comma is allowed — a deliberate divergence, and the chapter says
  why — and that `(..)` cannot be combined with named entries.
- Everything a module exposes is declared in that module: no re-exporting.
- An alias *replaces* the module's name rather than adding to it.
- Imports come before every other declaration ([LANG-5](lang-5.md)).
- Duplicate imports, duplicate aliases, alias/module-name collisions and self-imports are all
  errors at the `import` line ([LANG-7](lang-7.md)).
- Ambiguity from two imports is an error at the *use* site, not at the import.
- Imports may not form a cycle.
- The default import list — seven modules, written out in full ([LANG-8](lang-8.md)).

**What it turned up:** three defects — [BUG-14](bug-14.md) (an unannotated value never
reaches the interface), [BUG-15](bug-15.md) (an imported operator is unresolvable unless its
backing function is in scope, and the error names the wrong symbol), [BUG-16](bug-16.md) (an
unresolved type name is invented rather than reported) — and four `LANG-` tickets above. It
also confirmed [BUG-8](bug-8.md) and [BUG-9](bug-9.md) from the language side and gave both a
red-on-fix block in the chapter.

**Acceptance:** `cargo test --test spec` green, with `docs/spec/modules.md` contributing its
blocks; `docs/spec/INDEX.md`'s chapter row for *Modules, `exposing` and imports* reading
`written` and linked, its *One module per block* section replaced by the decision, and its
`expect=` table carrying `dependency-error` and `package=`; `tests/spec.rs`'s own fixture
tests covering the group path — a group where one block fails and the other does not, a
cyclic group, a `dependency-error` tag on a group that orders fine, and a `parse-error` tag
inside a group.
