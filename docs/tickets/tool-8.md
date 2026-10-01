# TOOL-8 · One failing declaration hides its whole module from its importers and from the editor

**Sizing:** large, and its decisions are open. It changes what `check_module` hands back and
what canonicalization and the typer do after their first failing declaration. The first step of
*Approach* is small and ships alone. Settle the rest before starting it.

**Part of:** the *Active work: editor support* section of [the index](README.md).
[`TOOL-6`](tool-6.md) works without it, but every capability past diagnostics goes dark for a
file with any error in it, and each file importing that one shows an error that is about
neither of them.

**Depends on:** [`TOOL-4`](README.md) for the syntax-error case: it is what hands back the
declarations of a module that parsed beside the ones that did not. The type-error and
canonicalization-error cases depend on nothing.

**Found while** settling [`TOOL-4`](README.md)'s open decisions. That ticket asked that an
importer of a module with a syntax error report no missing-module error. The importer's error
turned out to follow any failure in the module, so it was split out here and `TOOL-4` stops at
the parser.

**Location:** `src/compiler/mod.rs` — `check_module`, whose three phases each end it with `?`,
`parse_root`, which drops a module that has a syntax error, and `Checked`;
`src/compiler/dependencies.rs` — `ModuleWalker::check_in_order`, which inserts an `Interface`
on `Ok` only, and whose doc comment already names "partial progress *within* one failing
module" as open; `src/compiler/canonical/mod.rs` — `canonicalize`, which returns `Err` when
its `errors` vector holds anything, and `Module::to_interface`;
`src/compiler/typer/mod.rs` — `type_check`.

**Problem:** a module either passes every phase or contributes nothing. When it fails, the
modules that import it are checked against an environment it is absent from, and each reports
an error about the import. A package of two modules, `A` with a type error in one declaration
and `B` importing a different, well-typed one:

```zel
module A exposing (T(..), ok, bad)

type T = T

type U = U

ok : T
ok = T

bad : T
bad = U
```

```zel
module B exposing (..)

import A exposing (ok)

b : A.T
b = ok
```

`cargo run -- compile` on it prints:

```
error: [A] cannot match `T` with `U`
   ┌─ acme-imp-type:src/A.zel:10:1
   …
error: [B] cannot find a module named `A` to import
  ┌─ acme-imp-type:src/B.zel:3:1
  │
3 │ import A exposing (ok)
  │ ^^^^^^^^^^^^^^^^^^^^^^ no module of this name was found
```

The second error is false: `A` exists, and `ok` has the type its annotation gives it whatever
`bad` does. Replacing `bad`'s body with a syntax error (`bad = = T`) gives the same second
error, and so does a canonicalization error in `bad`.

On the command line that is one line of noise per importer. In an editor it is the steady
state, since a file being typed in has an error in it most of the time:

- every open file importing it is underlined at its `import` line until the error is fixed;
- the file itself has no `CheckedModule`, so hover, go-to-definition and semantic tokens have
  nothing to read, for the declaration being typed and for every other one in the file.

**Approach:** one step is decided. The rest is a set of choices this ticket does not make.

1. **A module that canonicalized publishes its interface whether or not it type checks.**
   An exposed value must be annotated (`Error::ExportedValueNotAnnotated`), so
   `canonical::Module::to_interface` reads nothing the typer produces. `check_in_order` needs
   the `canonical::Module` back from a `check` that failed after canonicalization, which
   `check_module`'s `Result<CheckedModule, CompilationError>` cannot carry today. The module's
   own type errors are reported as they are now and the build still fails. This removes `B`'s
   error in the example above and can land before anything below is settled.

What is not decided:

- **How a declaration that failed is represented downstream.** Either it is left out of the
  `canonical::Module` and its name is recorded as known-but-broken, so that a reference to it
  is not reported as a missing name; or the canonical AST gains a declaration-level hole that
  the typer gives a fresh type variable. The first is smaller. The second is what lets a
  declaration that calls a broken one still be checked and hovered.
- **Whether a broken declaration with an intact annotation counts as broken to its callers.**
  `f : Int -> Int` followed by a body that does not parse is the commonest state mid-edit.
  [`TOOL-4`](README.md) hands back such a function with its annotation and no binding, which
  canonicalization reports as `Error::NoBindings`. Its callers could be checked against the
  annotation.
- **Which errors about a broken declaration are suppressed.** An `exposing` entry naming it is
  `Error::ExportNotFound` today, and a call to it is a missing name. Both restate an error
  already reported. The same question applies to an importer naming it.
- **What [`TOOL-4`](README.md)'s `Failure` has to carry.** It holds a span and an error. Every
  option above needs the name the declaration would have had, which can only be read off the
  chunk's leading tokens (`f`, `type T`, `unsafe f`), and not always.
- **Whether a module with errors gets an `ir::Module`.** Publishing an interface needs only
  canonicalization. Hover needs the typer to run on the declarations that survived and
  `CheckedModule` to exist for a module that did not fully check, which nothing downstream of
  `check_module` expects. Emission must keep refusing such a module.
- **How this sits with [`BUG-34`](bug-34.md).** That ticket is the same all-or-nothing shape
  one level down: a sub-pass of `canonicalize` that fails substitutes an empty map. A partial
  result per sub-pass is a prerequisite for any option above that keeps the declarations of a
  module that failed canonicalization.

**Acceptance:**

For step 1, which is its own PR:

- A test in `tests/pipeline.rs` compiles the two-module package above. `A`'s type error comes
  back and no error names `B`. It is mutation-checked by restoring the `Ok`-only insert in
  `check_in_order`.
- A companion test gives `A` a canonicalization error instead and asserts `B` still reports the
  missing module, which pins where step 1 stops.

For the rest, once its decisions are made:

- The same package with `bad`'s body replaced by a syntax error, and again by a
  canonicalization error, reports no error naming `B`.
- A module with a syntax error in one declaration yields a typed tree for another declaration
  of the same module, read the way [`TOOL-6`](tool-6.md)'s hover reads it.
- A failing build still writes nothing under `build/`.
- `cargo test --workspace` is green, and `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked, and exits 0.
