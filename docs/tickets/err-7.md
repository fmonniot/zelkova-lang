# ERR-7 · "Did you mean …?" on unresolved names

**Sizing:** small.

**Depends on:** `ERR-3` — a suggestion needs a span to attach to; without one it is a note
floating free of the name it is about.

**Location:** `src/compiler/canonical/mod.rs` (`Error::VariableNotFound`, `VariantNotFound`),
`src/compiler/canonical/environment.rs` (`EnvError::InterfaceNotFound`, `ValueNotFound`,
`UnionNotFound`, `InfixNotFound`).

**Problem:** a typo produces a bare rejection. `Error::VariableNotFound` has carried a
`// add name suggestion ?` TODO since it was written. The environment that just failed to find
the name holds every name that *would* have matched, so the compiler is one edit-distance pass
away from turning

```
error: cannot find a value named `Main.widthDefault`
```

into a message that names `withDefault` and is right almost every time.

**Approach:** an edit-distance scan over the candidate names the `Environment` already has in
hand at the point of failure, with a distance threshold tight enough that a wrong suggestion is
rare — a bad "did you mean" is worse than none, because it sends the reader off to check
something irrelevant. Render it as a `help`-style note, and, once `ERR-3` has landed, hang it off
the label so the caret and the suggestion agree about which name is meant.

Take care with the qualified/unqualified distinction: `Name` versus `QualName` means the
candidate set for `Widget.map` is not the candidate set for `map`, and suggesting a name from
the wrong module would be actively misleading.

**Acceptance:** a canonicalization test asserting that a near-miss name produces a suggestion
naming the intended value, and that a name resembling nothing in scope produces no suggestion at
all.
