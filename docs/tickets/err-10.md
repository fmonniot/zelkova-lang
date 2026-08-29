# ERR-10 · Give a phase its first real warning: unused imports in canonicalization

**Sizing:** medium (design question first, then implementation — see below).

**Depends on:** nothing to *start*. It exists to be a prerequisite for
[ERR-8](err-8.md): ERR-8's own text says building a severity channel with no caller is
guesswork, and recommends picking it up "together with the first real warning." This ticket
*is* that first warning; ERR-8 should not start until this one has landed a concrete
diagnostic to carry.

**Location:** `src/compiler/canonical/mod.rs` — `new_environment` call in `canonicalize`
(around line 805) and `do_exports`; `src/compiler/canonical/environment.rs` — the
`Environment` trait and `new_environment`/`process_import`, which is where every import
currently gets consumed.

**Problem:** nothing in the compiler currently produces a warning, which is why ERR-8 flagged
itself as premature. `Environment` (`environment.rs`) is built once per module from its
`parser::Import` list and then only ever *read* — `find_value`, `find_type`,
`find_type_constructor`, `local_infix_exists` — there is no bookkeeping anywhere for whether a
given import, or a given name an import exposed, was ever actually looked up while
canonicalizing the rest of the module. An import that exposes names nothing in the module
references — `import Widget exposing (map)` where `map` is never written — canonicalizes
cleanly today with no signal at all.

This is deliberately scoped to *unused imports*, not unreachable `case` branches. The other
candidate mentioned when this ticket was proposed — warning instead of erroring on a
non-exhaustive match — is not available: `exhaustiveness::check` (`exhaustiveness.rs`) is
still a stub that inspects nothing, and `Error::NonExhaustiveMatch` there is meant to be a hard
error, not a warning, per its own doc comment. Building a warning on top of a checker that
does not exist yet is a second, larger, unrelated project.

**Approach — open design question, resolve before implementing:**

`Environment`'s methods take `&self`; recording "this was used" during a lookup needs either
interior mutability (`RefCell<HashSet<Name>>` alongside the existing `HashMap` fields on
`RootEnvironment`/`ScopedEnvironment`) or threading a mutable usage-tracking side channel
through `canonicalize` separately from the trait. `ScopedEnvironment` already nests (see
`new_scope`), so whichever shape is chosen has to answer where a use recorded in a child scope
is credited — against the root `Environment` that owns the import, not the scope it happened
in.

Two things to decide and write down before writing the checker, this ticket does not pick
either:

1. **What counts as "used."** A name reference during canonicalization is the obvious signal,
   but re-exporting an imported name (`exposing (Widget)` in a module that only imports
   `Widget` to re-expose its type) arguably should also count — otherwise a legitimate
   re-export pattern warns.
2. **Granularity of the warning.** Per-import (`import Widget exposing (map, view)` where
   neither is used → one warning on the `import` line) versus per-name (one warning per unused
   name in the exposing list, each with its own span, mirroring how `ERR-9` gave
   `parser::Exposed` its own span). Per-name is more precise but is more diagnostics to design
   at once.

Once decided: extend `Environment`/`RootEnvironment` to track which imported names get looked
up, add an `Error`-shaped (or, once `ERR-8` exists, a `Warning`-shaped) variant for "imported
but unused," and call the check at the end of `canonicalize` for each module, after the rest of
canonicalization has run so every real lookup has happened.

**On severity:** until `ERR-8` lands, this has nowhere to go but `Error` — which would make an
unused import a hard compile failure, clearly wrong for a warning. Land this ticket's
detection and bookkeeping behind a check that is written but not yet wired into
`canonicalize`'s error accumulation (e.g. a function that returns the list of unused imports,
covered directly by a test, without a call site that turns it into a `CompilationError` yet),
and let `ERR-8` wire the call site once `Diagnostic::warning()` exists to render it as.
Alternatively, if `ERR-8`'s scope grows to include this ticket's wiring in the same PR, that is
a legitimate way to close both together — this ticket does not mandate landing them as two
separate PRs, only that the design questions above get answered first.

**Acceptance:** a canonicalization test asserting that `import Widget exposing (map)` in a
module that never references `map` (qualified or unqualified) is detected as unused, and a
second test asserting that an import whose exposed names are all referenced produces no such
result. Given the open question above, the acceptance check operates on whatever function this
ticket lands (detection returning a list/`Result`), not necessarily on a rendered
`Diagnostic::warning()` — that render step belongs to `ERR-8`.
