# TOOL-8 · A module that fails type checking hides itself from its importers and from the editor

**Sizing:** medium. Nothing is left to decide: every shape below is settled in
[`DEC-23`](../decisions/dec-23.md), decisions 1 and 5. It changes what three functions return
and adds no behaviour to inference. What could make it bigger is the number of call sites of
`check_in_order`, which is eight, each a mechanical rewrite.

**Part of:** the *Active work: editor support* section of [the index](README.md). It is the
first of five tickets, `TOOL-8` through [`TOOL-12`](tool-12.md), that stop one failing
declaration from hiding its module. This one handles a **type error** and builds the shape the
other four extend. [`TOOL-6`](tool-6.md) works without them, but every capability past
diagnostics goes dark for a file with any error in it.

**Depends on:** nothing.

**Location:** `crates/zelkova-compiler/src/lib.rs` — `check_module`, whose three phases each
end it with `?`, `check_root`, `compile_in_build`, `compile_tests` and `PackageCheck`;
`crates/zelkova-compiler/src/dependencies.rs` — `ModuleWalker::check_in_order`, which inserts
an `Interface` on `Ok` only; `crates/zelkova-compiler/src/typer/mod.rs` — `type_check`, which
discards every declaration it solved when one fails; `crates/zelkova-compiler/src/ir/mod.rs` —
`Solved`, `Unchecked` and `build`; `crates/zelkova-compiler/tests/spec.rs` —
`canonicalize_tagged` and `evaluate_group`, the walker's other checker.

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
   ┌─ acme-imp:src/A.zel:10:1
   …
error: [B] cannot find a module named `A` to import
  ┌─ acme-imp:src/B.zel:3:1
  │
3 │ import A exposing (ok)
  │ ^^^^^^^^^^^^^^^^^^^^^^ no module of this name was found
```

The second error is false: `A` exists, and `ok` has the type its annotation gives it whatever
`bad` does. `A` also comes back with no `CheckedModule`, so an editor has no typed tree for
`ok` either.

Replacing `bad`'s body with a canonicalization error (`bad = nope`) or a syntax error
(`bad = = T`) gives the same second error. Those are [`TOOL-9`](tool-9.md) and
[`TOOL-11`](tool-11.md); this ticket leaves both as they are and pins that it does.

`crates/zelkova-compiler/tests/spec.rs` already has the behaviour this ticket wants: its
checker only canonicalizes, so a module that fails the typer has published its interface by
then. `evaluate_group`'s comment on it is the precedent.

**Approach:** five steps, in one PR. Each new function has a reduced twin that keeps today's
signature, the way `parser::parse` is `parser::parse_recovering` reduced to its first failure,
so no existing test changes.

1. **The typer answers for every declaration and reports its errors beside them.** Add
   `typer::type_check_recovering(module, interfaces) -> TypeCheck`, with
   `pub struct TypeCheck { pub solved: HashMap<Name, Solved>, pub errors: Vec<Error> }`. It is
   today's `type_check` body with one change: where the loop pushes an `Error`, it also
   inserts `Solved::Rejected` for that declaration. `type_check` becomes the reduction:
   `Ok(solved)` when `errors` is empty, `Err(errors)` otherwise. Add the unit variant
   `ir::Solved::Rejected`, documented as "inference reported an error for this declaration".

2. **`ir::Unchecked` says whether an error stands behind it.** Add `pub reported: bool`. In
   `ir::build`, `Solved::Rejected` becomes an `Unchecked` with `reported: true`; the existing
   arms (`Untranslatable`, `UnboundName`, the facade with no signature, and no entry at all)
   set `false`. Both literals of the struct are in `ir::build`; `zelkova_js::emit` only reads
   the list, and keeps refusing a module that has anything in it.

3. **The walker keeps a module that came back with errors.** In `dependencies.rs` add

   ```rust
   pub enum Outcome<M, E> {
       /// Nothing to publish.
       Failed(E),
       /// A module, and everything wrong with it. Empty means it checked.
       Module(M, Vec<E>),
   }
   ```

   `check_in_order`'s `check` returns `Outcome<M, E>`, and `check_in_order` returns
   `Vec<Outcome<M, E>>`, one per module in the order it checked them. It inserts the
   `Interface` for every `Outcome::Module`, errors or not. That insert is the whole of
   decision 1.

4. **`check_module` has a recovering form.** Add
   `check_module_recovering(package, interfaces, source) -> Outcome<CheckedModule, CompilationError>`:
   - canonicalization fails: `Outcome::Failed(CompilationError::Canonical(errors, name))`,
     as today;
   - otherwise call `type_check_recovering` and `exhaustiveness::check`, push a
     `CompilationError::Type` and a `CompilationError::Exhaustiveness` for whichever reported
     anything, build the `ir::Module` from `solved` either way, and return
     `Outcome::Module(CheckedModule { canonical, ir }, errors)`.

   `check_module` becomes the reduction: `Failed(e)` and `Module(_, [e, ..])` are `Err(e)`,
   `Module(m, [])` is `Ok(m)`. `check_root` passes `check_module_recovering` to the walker.

5. **`check_package` hands back the modules of a package that did not check.** Add
   `PackageCheck::failing: Vec<CheckedSource>`: every module the check built a tree for and
   put in none of `modules`, `test_dependency_modules` and `test_modules`. Those three keep
   their meaning, so the driver is untouched.
   - `check_root` partitions the walker's outcomes into the modules that checked
     (`Module(m, [])`), the modules that came back with errors, and the errors, which it tags
     with `InFile` as it does today. Its status line counts every outcome that is not
     `Module(_, [])` as failed.
   - `compile_in_build` takes one more accumulator, `failing: &mut Vec<CheckedSource>`. On the
     `errors.len() != errors_before` return after `check_root`, it pushes every module
     `check_root` handed back, checked or not, onto it.
   - `compile_tests` returns the test modules that checked, as today, and pushes the ones that
     came back with errors onto `failing`.

Update the walker's other checker to match: in `tests/spec.rs`, `canonicalize_tagged` returns
`Outcome::Module(module, vec![])` or `Outcome::Failed((name, errors))`, and `evaluate_group`
reads the outcomes.

Doc comments that describe the old shape and have to change with it: `check_in_order`'s
("partial progress *within* one failing module" is this ticket and the four after it),
`type_check`'s *What comes back*, `compile_in_build`'s paragraph on why a failing package
publishes nothing (still true of the package; no longer true of a module within it),
`PackageCheck::modules`, `ir::Unchecked`, and step 5.1 of the pipeline at the head of `lib.rs`.

**Acceptance:**

- A test in `crates/zelkova/tests/pipeline.rs` runs `check_package` on a new fixture,
  `tests/fixtures/package_import_type_error/`, holding the two modules above. `errors` is
  exactly one `InFile` around a `CompilationError::Type` for `A`. It is mutation-checked by
  making `check_in_order` insert the interface only for a `Module` whose error list is empty,
  which brings `B`'s `InterfaceNotFound` back.
- The same check's `failing` holds `A` and `B`, and `modules` is empty. `A`'s
  `ir.declarations` holds `ok` with a `tpe` that displays as `T`, and its `ir.unchecked` is
  exactly `bad` with `reported: true`. It is mutation-checked by making
  `type_check_recovering` return an empty `solved` when it has errors.
- A companion test runs a second fixture, `tests/fixtures/package_import_canonical_error/`,
  the same package with `bad = T <+> T`, an operator nothing declares, and asserts `B` still
  reports the missing module. That pins where this ticket stops; [`TOOL-9`](tool-9.md)
  inverts it. The fixture uses an operator and not a misspelt name so that it stays a broken
  declaration once [`TOOL-12`](tool-12.md) has turned a misspelt name into a hole.
- A test in `crates/zelkova-compiler/tests/typer.rs` calls `type_check_recovering` on a
  module with one well-typed declaration and one ill-typed: `solved` holds `Solved::Typed`
  for the first and `Solved::Rejected` for the second, and `errors` has one entry.
- `zelkova::compile_package_into` on the first fixture returns `Err` and leaves its build
  directory absent, as `a_build_with_a_failing_module_writes_nothing` asserts for
  `package_type_error`.
- `cargo test --workspace` is green, `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked and exits 0, and `cargo run -- test std/core`
  still reports `98 tests: 98 passed, 0 failed, 0 errored`.
