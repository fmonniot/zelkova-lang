# LANG-29 · A top-level declaration silently shadows a name imported unqualified

**Sizing:** medium.

**Location:** `src/compiler/canonical/environment.rs` — `RootEnvironment::insert_top_level_value`
and `insert_union_type`, both of which `HashMap::insert` over whatever `process_import` already
put there; `src/compiler/canonical/mod.rs` — `do_values` (which calls the first, for every
top-level binding, before resolving any body) and the `for (n, t) in types.iter()` loop in
`canonicalize` (which calls the second).

**Decided by:** [`docs/spec/name-resolution.md`](../spec/name-resolution.md)'s *A top-level
name comes from exactly one place*.

**Problem:** a module's own declarations and the names its imports bring in unqualified are one
scope, so neither shadows the other and a collision between them is an error. Today the
declaration wins, silently, and the `exposing` entry that named the same thing is dead with
nothing saying so:

```
module Main exposing (x)

import Widget exposing (Size, label)

type Size
  = Big

label : Size
label =
  Big

x : Size
x =
  label
```

Both `label` and `Size` resolve to the local declarations. The import is not reported as
unused, either, so nothing in the file distinguishes it from one that is doing work.

The same holds for `import Widget exposing (..)`, and the rule holds there too — the error is
reported on the `import` line, since there is no exposing entry to point at. That is the case
worth designing for first: an open import collides with whatever the module happens to declare,
so the check must name the colliding name in its message rather than relying on a caret under
an entry that does not exist.

**Approach:** the two `insert_*` sites are where the collision is visible, and both are called
after `process_import` has finished, so each can check what is already under the key. A
`ValueType::Foreign`/`Foreigns` (or an existing `types` entry) sitting under a name being
declared locally is the error; a `Local`/`TopLevel` already there is not this ticket's concern
(a value declared twice is [`LANG-20`](lang-20.md)'s clauses, a type declared twice is
[`LANG-32`](lang-32.md)).

Both need a new `canonical::Error` variant carrying the declaration's span as the primary label
and — through the `Interface`'s `source_span`, the way `AmbiguousVariables` already does — a
secondary label in the module the name was imported from. `insert_top_level_value` and
`insert_union_type` return `()` today and would have to return a `Result`, or record the
collisions for the caller to drain; the second fits `do_values`' accumulating shape better,
since one module may collide on several names and should report all of them.

Note the ordering constraint this exposes: `do_values` inserts *every* top-level name before
resolving any body, which is what makes declarations unordered. The check has to happen in that
first pass, not while resolving bodies, or a collision on a name nothing uses goes unreported.

The `exposing (..)` case has no entry span to use, so `process_import` is the site that knows
the `import` line's span — which means the open-import half may be easier to raise as an
`EnvError` from a second pass over the interface, once the module's own declarations are known.
That is a different call site from the two above and the ticket does not pick between doing it
there or carrying the import span forward; whichever way, the message names the colliding name.

**Acceptance:** the two `expect=ok` blocks under *A top-level name comes from exactly one place*
in [`docs/spec/name-resolution.md`](../spec/name-resolution.md) (`module Main` and `module
Other` of the `package=clash` group) go **red** — retag both `expect=canonical-error:` with the
new variant and delete the **Known gap:** paragraph above them. A `tests/compiler/canonical.rs`
case for each of the three shapes — a value entry, a type entry, and `exposing (..)` — seen to
fail before the fix. `cargo run` still prints `parsed 8 modules` and lists all eight: `std/core`
imports `Basics` openly in every module, so this check is the first thing that would notice a
collision there.
