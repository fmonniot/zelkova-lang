# LANG-7 · Nothing checks an import list for duplicates, alias collisions or self-imports

**Sizing:** small. One pass over `imports` before `process_import` runs, plus the error
variants and their messages.

**Location:** `src/compiler/canonical/environment.rs` — `new_environment`, which loops over
`imports` calling `process_import` and accumulates whatever each one reports, with no view of
the list as a whole; `insert_foreign_value`, whose `Foreign` → `Foreigns` promotion is what
turns a duplicate import into an ambiguity.

**Decided (`SPEC-3`, by the language owner):** four things are errors at the `import` line.

1. Importing the same module twice in one file.
2. Two imports sharing one alias.
3. An alias that collides with the name of another imported module.
4. A module importing itself.

The first three would each let one prefix mean two things at once, with nothing in the line
in front of the reader to show it. The fourth has no meaning to give.

**Problem:** none is checked, and the three failure modes are all different, which is what
makes this one ticket rather than three.

**An alias colliding with a module name silently merges two namespaces.** `process_import`
qualifies every imported name with `alias.as_ref().unwrap_or(imported_module_name)` and
inserts it into one flat `env.variables`, so both modules' values land under the same prefix
and both resolve:

```zel
module Other exposing (y, z)

import Widget as Gadget
import Gadget

y = Gadget.label      -- Widget's
z = Gadget.volume     -- Gadget's
```

Two imports sharing one alias behave the same way. Nothing reports anything.

**Importing the same module twice fails, but not as a duplicate import.** Each name is
inserted twice, and `insert_foreign_value` promotes a repeated name to `ValueType::Foreigns`,
so every *use* of a qualified name from that module is reported as
`Error::AmbiguousVariables` — "`Widget.label` is exposed by several imported modules" —
listing `Widget` twice. The user is told a name is ambiguous between two modules that are the
same module, at the use site rather than at the import line.

**A self-import is reported as a missing module.** `import A` inside `module A` reaches
`process_import` before `A`'s own interface exists — a module's interface is inserted by
`check_in_order` only after it has checked — so it fails `EnvError::InterfaceNotFound`:
"cannot find a module named `A` to import". Tarjan's SCC does not catch it either; a
self-loop is not reported by `dependencies::ModuleWalker`.

Found while writing [`docs/spec/modules.md`](../spec/modules.md) (`SPEC-3`), whose *One
module, one import* section carries the blocks.

**Approach:** do all four in one pass at the top of `new_environment`, before any
`process_import` call, since each is a property of the list rather than of one entry:

1. Build the map from prefix (alias, else module name) to the `parser::Import` that claimed
   it. A second claim on a prefix is case 2 or 3 depending on whether the claimant used an
   alias; a second import of the same *module name* is case 1. All three want both spans, so
   the diagnostic can put a primary label on the offender and a secondary on the first one —
   `Error::AmbiguousVariables` is the worked example of a multi-label error, and
   `SpanLabel::file` stays `None` here since both lines are in the module under check.
2. Case 4 is `imported_module_name == module_name.name()`, which `new_environment` already
   has in scope as its `module_name` parameter.
3. New `EnvError` variants, one per case or one carrying a discriminant — the messages differ
   enough that separate variants probably read better. Each needs a `message()` in the
   vocabulary of the source and a `labels()`, per `CLAUDE.md`'s *An error has to describe
   itself*.

Report all four and keep going rather than returning on the first, so a file with two
problems shows both — `new_environment` already accumulates.

**Acceptance:** four tests in `tests/compiler/canonical.rs`, one per case, each asserting the
specific new error and that its labels point at the `import` lines involved (assert on
`.span` or on `diagnostic.labels[..].range`, never on a whole `NodeSpan` — its `PartialEq` is
always `true`). In [`docs/spec/modules.md`](../spec/modules.md), the `package=duplicates`
group goes red twice over: the `Main` block is tagged
`expect=canonical-error:AmbiguousVariables` and must become the new duplicate-import error,
and the `Other` block is tagged `expect=ok` and must start failing. The standalone
`module Alone` block is tagged `expect=canonical-error:EnvironmentErrors`, which will *not*
go red on its own — a new `EnvError` variant is still an `EnvironmentErrors` — so its
paragraph has to be updated by hand; the chapter's `**Not implemented:**` note says so.
