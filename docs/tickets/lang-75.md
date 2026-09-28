# LANG-75 · The manifest's `main` is read, and nothing checks what it names

**Sizing:** small-to-medium. It is one check after the package's modules have been type checked,
plus up to three error variants with carets. It could grow if the value's solved type is awkward
to reach from where the check has to sit: the check needs a module's interface *and* the type
the typer gave one of its values.

**Part of:** [Active work: effects](README.md#active-work-effects).

**Depends on:** [`LANG-72`](README.md), now closed, and [`LANG-74`](README.md). Without them,
`Task ()` cannot be written, so there is nothing for a `main` to match.

**Location:** `src/compiler/manifest.rs` — `Manifest::main`, validated as a module name and passed
on; `src/compiler/mod.rs` — `compile_in_build`, where a package's checked modules and their
`Interface`s are all in hand; `CompilationError`, which the new errors join.

**Problem:** [Programs](../spec/packages.md#programs) says `main` names a module under `src/`, that
the module must expose a value called `main`, and that the value must have type `Task ()`. The
chapter's **Not implemented:** paragraph says that none of the three is checked. A manifest with
`main = "Nowhere"` builds cleanly today, and so does one naming a module whose `main` is an `Int`.

That matters once [`GEN-22`](gen-22.md) runs a program. An unchecked `main` becomes a JavaScript
error at start-up, or the runtime being handed something that is not a `Task`.

**Approach:**

1. After a package with a `main` has type checked, look up the named module among that package's
   `src/` modules, then `main` in its interface, then compare its type to `Task.Task ()` by
   qualified name. That comparison is by identity, so a user's own `Task` does not pass.
2. Three failures, each its own diagnostic in the source's vocabulary. The module does not exist
   under `src/` (a module under `tests/` does not count). The module exposes no `main`. `main`
   has another type, and the diagnostic prints the type it has. The first has no source span to
   point at, since the name is in `zelkova.toml`. Say so in the notes, the way
   `CompilationError::Manifest` errors do. The other two point at the module's `exposing` list
   and at `main`'s annotation or definition.
3. **The ticket does not pick** whether the check runs for every package in the build that declares
   `main`, or only for the root package. Every package is the stricter reading of "a package can be
   both" (a dependency whose `main` is broken is a broken package). Root-only is all
   [`GEN-22`](gen-22.md) needs. Say which, and why.
4. An annotation-free `main` is allowed: the check reads the solved type, not the annotation.

**Tests:** `tests/pipeline.rs` fixtures for each of the three failures, and one that passes with
`main = Task.succeed ()`. Assert the diagnostic's label ranges for the two that have a span.

**Acceptance:** the block under [Programs](../spec/packages.md#programs) is retagged `expect=ok`
if the harness can express a manifest. If it cannot, a `tests/pipeline.rs` case compiles the same
program. The chapter's **Not implemented:** paragraph is removed. Each failure above makes
`zelkova compile` exit non-zero with its diagnostic.
