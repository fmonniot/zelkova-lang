# TOOL-10 · A failed type, operator or import is reported again by everything that names it

**Sizing:** medium. Nothing is left to decide: the rule is [`DEC-23`](../decisions/dec-23.md)
decision 3. It adds one flag in three places and one filter. What could make it bigger is
`Interface`'s struct literals, of which there are fifteen, each gaining one field.

**Part of:** the *Active work: editor support* section of [the index](README.md), third of the
five tickets `TOOL-8` through [`TOOL-12`](tool-12.md).

**Depends on:** [`TOOL-9`](tool-9.md), for `canonicalize_recovering`, `Module::broken` and the
sub-passes' partial results.

**Location:** `crates/zelkova-compiler/src/canonical/environment.rs` — `new_environment`,
which returns `Err` when any import fails, `process_import` and `RootEnvironment`;
`crates/zelkova-compiler/src/canonical/mod.rs` — `canonicalize_recovering`, `Module` and
`Module::to_interface`; `crates/zelkova-compiler/src/lib.rs` — `Interface` and `check_root`.

**Problem:** after [`TOOL-9`](tool-9.md) a failed *value* costs nothing but its own error,
because every value's name is in scope before any body is resolved. Three other failures still
make names go missing, and each missing name is then reported by everything that uses it.

An operator whose `infix` declaration fails:

```zel
module A exposing ((<+>), use)

infix left 6 (<+>) = nope

use : Int
use = 1 <+> 2
```

reports three errors for one mistake: the declaration names no value, `<+>` is not in scope at
its use, and `<+>` is exposed but not declared.

A type whose declaration fails leaves its constructors out of scope, so each use of one is a
`VariantNotFound`, and an importer writing `import A exposing (T(..))` is told `A` does not
expose `T`.

An import that does not resolve ends canonicalization outright: `new_environment` returns
`Err`, the module has no canonical form at all, and each of *its* importers reports
`cannot find a module named …` about a module that exists.

**Approach:** in one PR. A scope is *incomplete* when a name could be missing from it for a
reason already reported. In an incomplete scope a not-found error is dropped.

1. **The flag.** Add `incomplete: bool` to `RootEnvironment`, with a `pub(crate)` reader and
   a setter; `pub incomplete: bool` to `canonical::Module`; and `pub incomplete: bool` to
   `Interface`, documented as: a name looked up in this interface and not found may be one its
   module declares and could not publish. `Module::to_interface` copies the module's flag.
   Every hand-built `Interface` and `canonical::Module` sets `false`.

2. **An import that fails no longer ends canonicalization.** `new_environment` returns
   `(RootEnvironment, Vec<EnvError>)`: every import that resolved is in the environment, and
   every one that did not is in the list. `canonicalize_recovering` reports the list as one
   `Error::EnvironmentErrors` and carries on, so it returns `Canonicalized` directly and its
   `Result` goes. `canonicalize`, the reduction, keeps its signature.

3. **What makes a scope incomplete.** `new_environment` sets the flag when an import returned
   an error, and when an import resolved to an `Interface` whose own `incomplete` is set.
   `canonicalize_recovering` sets it after `do_infixes` if any `infix` declaration failed, and
   after `do_types` if any `type` declaration failed.

4. **An import entry not found in an incomplete interface is dropped.** In `process_import`'s
   `Exposing::Explicit` arm, a `ValueNotFound`, `UnionNotFound` or `InfixNotFound` raised
   against an interface whose `incomplete` is set is not an error: the entry is skipped.
   This one is decided per interface and not by the scope's flag, because the interface being
   looked in is in hand. `ConstructorsNotExposed` and `InterfaceNotFound` are never dropped.

5. **A not-found error in an incomplete scope is dropped.** Add to `canonical/mod.rs`

   ```rust
   fn without_restated(errors: Vec<Error>, incomplete: bool) -> Vec<Error>
   ```

   which returns `errors` untouched when `incomplete` is false. Otherwise it flattens every
   `Error::Many` into its members and drops `VariableNotFound`, `VariantNotFound`,
   `TypeNotFound`, `ExportNotFound` and `InfixReferenceInvalidValue`. `canonicalize_recovering`
   passes each sub-pass's errors through it, reading the flag as it stood **before** that
   sub-pass ran: a declaration's own failure is reported, and what it makes incomplete is
   everything after it. A declaration whose every error was dropped is still left out, as a
   `Broken` or as a missing type or infix: dropping the error does not make the declaration
   sound.

6. **What makes an interface incomplete.** `Module::incomplete` is the environment's flag at
   the end of `canonicalize_recovering`, or true when any function wrote an annotation that
   did not canonicalize. The second half is the one case where a name goes missing from the
   interface while the module's own scope is whole.

7. **A module with a broken declaration is not listed as checked.** `check_root` puts a
   module among the ones that checked only when its error list is empty **and**
   `canonical.broken` is empty **and** `canonical.incomplete` is false. Until now the first
   implied the other two. A module whose errors were all dropped has declarations with no IR,
   and belongs with the modules that came back with errors.

**Acceptance:**

Tests in `crates/zelkova-compiler/tests/canonical.rs`, on `canonicalize_recovering`:

- The `<+>` module above reports exactly `InfixReferenceInvalidValue`. Mutation-checked by
  not setting the flag after `do_infixes`, which brings the other two back.
- `type T = MkT | (T, T)` beside `k : T` with `k = MkT`: exactly `InvalidVariant`.
- `import Nope` beside `x : Int` with `x = Nope.y`: exactly one `EnvironmentErrors` holding
  `InterfaceNotFound`, and a module is returned.

  Neither asserts what becomes of `k` or `x`: a `Broken` here, a value holding a hole once
  [`TOOL-12`](tool-12.md) lands.
- The control: a module with no failed import, type or infix and a misspelt name in a body
  still reports `VariableNotFound`. Mutation-checked by making `without_restated` ignore its
  flag.
- A second control: in the `import Nope` module, `g : Int` with no binding still reports
  `NoBindings`. Only the five not-found variants are dropped.
- Against a hand-built `Interface` with `incomplete: true`, `import Lib exposing (missing)`
  reports nothing and the module's `incomplete` is true. With `incomplete: false` it reports
  `ValueNotFound`, as `missing_exposed_import_name_labels_the_name_alone` already asserts.
- `f : Nope -> Int` with a sound body, in a module with nothing else wrong: `TypeNotFound`
  is reported and the module's `incomplete` is true.

Tests in `crates/zelkova/tests/pipeline.rs`:

- [`TOOL-9`](tool-9.md)'s test over `package_import_unresolved_import` is inverted: the
  errors are exactly one `Canonical` for `A`, and none names `B`.
- A new fixture, `tests/fixtures/package_import_broken_type/`: `A` exposes `T(..)` and
  declares it `type T = MkT | (T, T)`; `B` writes `import A exposing (T(..))`,
  `b : A.T` and `b = MkT`. The errors are exactly one `Canonical` for `A` holding
  `InvalidVariant`. `B` is in `failing` with `b` in its `ir.unchecked` and `reported: true`,
  and the root's `checked modules` line in `status` lists no module and counts two as failed.
  Neither module names a `Basics` type, so the fixture needs no dependency. Mutation-checked
  twice: by leaving `Interface::incomplete` false in `to_interface`, which reports `B`'s
  import entry, and by dropping step 7's two extra conditions, which puts `B` in the status
  line's list.

And `cargo test --workspace` is green, `cargo run -- compile std/core` still prints
`parsed 10 modules`, lists all ten as checked and exits 0, and `cargo run -- test std/core`
still reports `98 tests: 98 passed, 0 failed, 0 errored`.
