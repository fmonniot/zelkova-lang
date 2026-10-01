# BUG-47 · A qualified name an imported module does not expose is reported as under the importing module

**Severity:** low (the diagnostic points at the right span and the build fails correctly, but the
message names a value that exists nowhere and sends the reader to the wrong module).

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `Expression::from_parser`'s
`parser::ExpressionKind::Variable` arm, which builds
`Error::VariableNotFound(env.module_name().qualify_name(name), ..)`; the `Constructor` arm's
`Error::VariantNotFound` and the pattern one build theirs the same way. `Error::TypeNotFound`
holds the written `Name`, which is the shape wanted.

**Problem:** `name` is the spelling as written, so for a qualified reference it already carries
the module or alias (`Widget.label`). `env.module_name()` is the module being canonicalized,
so the error names it twice over. With a two-module package in which `Widget` exposes `other`
and `Main` is

```zel
module Main exposing (x)

import Widget

x : Int
x = Widget.label
```

`zelkova compile` reports

```
= cannot find a value named `Main.Widget.label`
```

where `Widget.label` is what the user wrote and the only name worth reporting. An unqualified
miss is unaffected: `Main.label` is correct there, because an unqualified name is looked up in
`Main` itself. `docs/spec/modules.md`'s `Main` block under the unannotated-export rule hits it.

**Fix:** report a qualified miss under the spelling the user wrote. Whether the error carries a
`QualName` at all for a name that resolved to no module is the choice: `Error::VariableNotFound`
holds a `QualName` today, so either a qualified spelling is split into the module the import
named and the value's own name, or the variant carries the written `Name` for this case. The
ticket does not pick. `Error::message` and `labels` read the field, and the suggestion
(`suggest_name`) is computed from `env.value_names()` either way.

**Acceptance:** a test in `crates/zelkova-compiler/tests/canonical.rs`, with `Widget`'s
interface in the map and `x = Widget.label` for a `label` it does not expose, asserts the
rendered message names `Widget.label` and not `Main.Widget.label`; reverting the fix turns it
red. The same for a qualified constructor.
`cargo test --workspace` stays green.

**Found:** in review of `TOOL-9`'s PR. The line predates it and the PR does not touch it, so
it was left alone there.
