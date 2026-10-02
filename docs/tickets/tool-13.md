# TOOL-13 · A facade that declares a type is still reported for the type it declared

**Sizing:** small. The rule is [`DEC-23`](../decisions/dec-23.md) decision 3; what is open is
whether the facade's importers are covered by the same change (see Approach).

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `canonicalize_recovering`, the
`source.binding_foreign` branch, which pushes `Error::TypeDeclared` and `Error::InfixDeclared`
and never calls `Env::set_incomplete`; `without_restated`.

**Problem:** [`DEC-23`](../decisions/dec-23.md) decision 3 makes a scope incomplete when "a
`type` or `infix` declaration … failed", and `canonicalize_recovering`'s ordinary branch does
that: it sets the flag after `do_infixes` and `do_types` report an error. A `module foreign`
facade may declare neither, and its branch reports `TypeDeclared` or `InfixDeclared` without
setting the flag. The declared type is never registered, so a signature that names it gets
`TypeNotFound`, which the flag would have dropped as a restatement. With
`src/F.zel`:

```
module foreign F exposing (f)

type T = MkT

f : T -> ()
```

`cargo run -- compile` on that package reports `a module foreign facade cannot declare a type,
but declares T` and, beside it, `cannot find a type named T`. The second restates the first.
The review also reports that a module importing `T` from the facade is told the facade
exposes no such union; that was not reproduced here, and the facade's interface does not set
`Interface::incomplete` today because the module's flag is never set.

It is over-reporting and not a hole: the facade's own error stands, so the build fails either
way. Found in review of the `TOOL-10` PR, and left there because the ticket scoped the
flag to the ordinary branch.

**Approach:**

1. Call `env.set_incomplete()` in the facade branch when `source.infixes` or `source.types` is
   non-empty, after the errors are pushed and before the signatures are canonicalized, so that
   `without_restated` is given an incomplete scope for them.
2. Decide whether the facade's importers should stop seeing a `UnionNotFound` for `T`. They get
   it from `Interface::incomplete`, which `Module::to_interface` copies from the module's flag, so
   step 1 gives it to them too. This ticket does not pick between that and leaving them
   reported; `DEC-23` decision 3 reads as the former.
3. Add the facade case to `crates/zelkova-compiler/tests/canonical.rs` beside
   `a_failed_type_is_reported_once_and_not_again_by_its_constructor`, and mutation-check it by
   dropping the `set_incomplete` call.

**Acceptance:** a test in `crates/zelkova-compiler/tests/canonical.rs` canonicalizes the facade
above and finds `Error::TypeDeclared` alone, with `Module::incomplete` true; the same test
turns red without the `set_incomplete` call. `cargo run -- compile std/core` still prints
`parsed 10 modules` with all ten checked, and `cargo test --workspace` is green.
