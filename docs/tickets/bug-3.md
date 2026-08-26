# BUG-3 · `Bitwise.zel` imports the non-existent `Elm.Kernel.Bitwise`

**Severity:** low (one standard-library module does not compile; nothing else imports it)

**Location:** `std/core/src/Bitwise.zel` — the `import Elm.Kernel.Bitwise` line. The failure
surfaces from `src/compiler/canonical/environment.rs` as
`EnvironmentErrors([InterfaceNotFound(Name("Elm.Kernel.Bitwise"))])`.

**Problem:** `Bitwise.zel` was carried over from Elm's `core` package largely unchanged,
including its dependency on `Elm.Kernel.Bitwise`. Elm's kernel modules are its private JS
FFI; Zelkova has no such thing and no such module. Canonicalization fails on every run:

```
warning: [Bitwise] Canonical error messages are not implemented yet
 = EnvironmentErrors([InterfaceNotFound(Name("Elm.Kernel.Bitwise"))])
```

It is the only module of the seven that fails, which makes it the permanent noise floor of
`cargo run` — the one thing that has to be mentally filtered out before the smoke test tells
you anything. It is also what makes `cargo run` unusable as a plain pass/fail check, and so it
blocks the clean half of `BUG-1`'s acceptance.

Zelkova's equivalent of a kernel module already exists and is the shape to copy: a
`module javascript` facade declaring types with no bodies, plus a companion `.mjs` carrying
the implementation — `std/core/src/Js/Basics.zel` and `Js/Basics.mjs`, `Js/Utils.zel` and
`Js/Utils.mjs`. Both canonicalize today.

**Fix:** two options, and this ticket does not pick between them — the person taking it should.

1. Write `std/core/src/Js/Bitwise.zel` as a `module javascript` facade exposing the seven
   primitives `Bitwise.zel` needs (`and`, `or`, `xor`, `complement`, `shiftLeftBy`,
   `shiftRightBy`, `shiftRightZfBy`), with `Js/Bitwise.mjs` implementing them over JS's `&`,
   `|`, `^`, `~`, `<<`, `>>`, `>>>`, and repoint the import. This is the real fix, and the JS
   operators map one-to-one onto the Elm semantics.
2. Rename it to `Bitwise.ignored`, joining the eleven other modules under `std/core/src` that
   are not expected to compile yet. This is honest and takes a minute, but drops a module that
   is otherwise complete.

Prefer (1) unless the `.mjs` calling convention turns out to be underspecified — read
`Js/Basics.mjs` first and check whether it documents one.

**Acceptance:** `cargo run` completes with no canonicalization error for `Bitwise`. If (1) was
taken, `Bitwise` appears in the checked-modules list (which requires `BUG-2`, or a manual
check of the interfaces map); if (2) was taken, `cargo run` reports six parsed modules and no
errors.
