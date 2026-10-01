# TOOL-9 · A declaration that fails canonicalization takes its whole module with it

**Sizing:** medium-to-large. Nothing is left to decide: the shape is
[`DEC-23`](../decisions/dec-23.md) decision 2. The size is in `canonicalize`, whose four
sub-passes each change what they return, and in `do_values`, whose per-declaration closure is
restructured. It closes [`BUG-34`](bug-34.md) on the way.

**Part of:** the *Active work: editor support* section of [the index](README.md), second of
the five tickets `TOOL-8` through [`TOOL-12`](tool-12.md).

**Depends on:** [`TOOL-8`](tool-8.md), for `Outcome`, `check_module_recovering`,
`type_check_recovering`, `ir::Unchecked::reported` and `PackageCheck::failing`.

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `canonicalize`, which returns
`Err` when its `errors` vector holds anything, its five
`unwrap_or_else(|err| { errors.extend(err); HashMap::new() })` sites, `do_values`, `do_types`,
`do_infixes`, `do_exports`, `Module` and `Module::to_interface`;
`crates/zelkova-compiler/src/utils.rs` — `collect_accumulate`;
`crates/zelkova-compiler/src/typer/mod.rs` — `type_check_recovering`'s first pass;
`crates/zelkova-compiler/src/ir/mod.rs` — `build`;
`crates/zelkova-compiler/src/lib.rs` — `check_module_recovering`.

**Problem:** `canonicalize` answers with a module or with errors, never both. One declaration
that names something misspelt costs the module its interface and its typed tree. With the
package from [`TOOL-8`](tool-8.md)'s Problem and `bad`'s body changed to `nope`:

```
error: [A] cannot find a value named `A.nope`
   ┌─ acme-imp:src/A.zel:11:7
   …
error: [B] cannot find a module named `A` to import
```

Inside `canonicalize` the same shape repeats one level down, which is `BUG-34`: a sub-pass
that fails substitutes an empty map for everything it did resolve. Its live reproduction today
is a constructor of a sound type going missing because a sibling type failed:

```zel
module Example exposing (Size, Pair, small)

type Size
  = Small

type Pair
  = (Size, Size)

small : Size
small = Small
```

reports `Pair`'s tuple variant, which is the real error, and then `cannot find a type
constructor named Example.Small`: `do_types` failed on `Pair`, `types` became empty, and
`insert_union_type` never ran for `Size`. (`BUG-34`'s own two cases no longer cascade, because
`insert_declared_type` now registers every type name before `do_types` runs.)

**Approach:** in one PR.

1. **A partial collector.** In `utils.rs` add
   `collect_partial<T, E, I, R>(iterator: I) -> (R, Vec<E>)`, which keeps every `Ok` item and
   every error. `collect_accumulate` becomes a call to it that answers `Ok(r)` when the error
   list is empty.

2. **Each sub-pass returns what it resolved beside its errors.** `do_infixes`, `do_types` and
   `do_values` return `(HashMap<..>, Vec<Error>)` through `collect_partial`, and so does the
   facade branch's own iterator. `canonicalize`'s five `unwrap_or_else` sites become plain
   destructuring that extends `errors` and keeps the map.

3. **`do_exports` returns the entries that resolved.** For `Exposing::Explicit`, the
   `Exports::Specifics` of every entry that did not error, beside the errors. For
   `Exposing::Open`, `Exports::Everything` beside its `ExportedValueNotAnnotated` errors. The
   fallback to `Exports::Everything` on failure goes: it was safe only while the module was
   thrown away, and would now expose every private declaration of a module with one bad
   `exposing` entry.

4. **A broken value is recorded.** Add to `canonical::Module`

   ```rust
   /// The value declarations that were written and have no canonical form, sorted by name.
   pub broken: Vec<Broken>,
   ```

   ```rust
   pub struct Broken {
       pub name: Name,
       /// Where the declaration was written, annotation and body together.
       pub span: NodeSpan,
       /// The declaration's annotation, when it has one and it canonicalized.
       pub tpe: Option<Type>,
       /// Where that annotation was written; `NodeSpan::none()` when `tpe` is `None`.
       pub annotation_span: NodeSpan,
   }
   ```

   `do_values` canonicalizes a function's annotation first and on its own, so that a body
   error can no longer stop the annotation being read. Per function:

   | annotation | body | result |
   |---|---|---|
   | none, or canonicalizes | canonicalizes | a `Value`, as today |
   | canonicalizes | fails | `Broken` with `tpe: Some`; the body's error is reported |
   | none | fails | `Broken` with `tpe: None`; the body's error is reported |
   | fails | either | `Broken` with `tpe: None`; the annotation's error is reported, and the body's too if it failed |

   "The body fails" covers every error `do_values` raises today other than the annotation's
   own `Type::from_parser_type`: `NoBindings`, `MultipleBindingsUnsupported`,
   `BindingPatternsInvalidLen` (both sites), and anything `Pattern::from_parser` or
   `Expression::from_parser` returns. The facade branch follows the same rule: a signature
   whose type canonicalizes and which a later check rejects (`FacadeTypeNotAdmitted`,
   `FacadeTaskMisplaced`, `FacadeResultNotEffect`, a binding present) is `Broken` with
   `tpe: Some`.

   A type or an infix declaration that fails is left out of `types` or `infixes` and recorded
   nowhere. What that costs its users is [`TOOL-10`](tool-10.md).

5. **`canonicalize_recovering`.** Add

   ```rust
   pub struct Canonicalized { pub module: Module, pub errors: Vec<Error> }

   pub fn canonicalize_recovering(package, interfaces, source)
       -> Result<Canonicalized, Vec<Error>>
   ```

   `Err` is the one case with no module: `new_environment` failed, exactly as today.
   `canonicalize` becomes the reduction: `Err(e)` stays `Err(e)`, and `Ok(c)` is `Ok(c.module)`
   when `c.errors` is empty and `Err(c.errors)` otherwise. Every existing caller keeps working.

6. **The interface reads the recorded annotations.** In `Module::to_interface`, a `Broken`
   with `tpe: Some` that the header exposes as a value joins `values`, with its `span`. The
   lookup behind `infix_functions` reads `broken` as well as `values`, or an exposed operator
   backed by a broken function reaches an importer as `InfixFunction::ImportedUntyped`.
   `arities` records nothing for a broken value, whose parameters are unknown; say so on
   `Interface::arities`, which today names a hand-built interface as the only one that can
   leave an entry out.

7. **The typer reads them too.** In `type_check_recovering`'s first pass, each `Broken` with
   `tpe: Some` goes into `global` under the same two keys an annotated value of this module
   gets. Nothing else in the typer changes: the loop still runs over `module.values`.

8. **Every broken value is in the IR's `unchecked`.** `ir::build` appends one
   `Unchecked { reported: true, .. }` per `Broken` and keeps the list sorted by name. Restate
   the invariant on `Module::unchecked` and on `build`: every value of the canonical module,
   in `values` or in `broken`, ends up in exactly one of the two lists.

9. **`check_module_recovering` carries on past canonicalization errors.**
   `canonicalize_recovering`'s `Err` is `Outcome::Failed`; its `Ok` pushes a
   `CompilationError::Canonical` when `errors` is not empty and continues into the typer and
   `ir::build` as [`TOOL-8`](tool-8.md) wrote them.

`canonical::initialisation_order`'s doc comment says `ir::build` is reached only after
`canonicalize` returned `Ok`. That stops being true; the comment already says what the
function does with a cyclic component, which is the case that can now arrive.

**Acceptance:**

Tests in `crates/zelkova-compiler/tests/canonical.rs`, through a new helper in
`tests/support/mod.rs` that calls `canonicalize_recovering` the way `canonicalize_with_interfaces`
calls `canonicalize`:

- The `Size`/`Pair`/`small` module above reports exactly `InvalidVariant`. Its `types` holds
  `Size` and not `Pair`, and its `values` holds `small`. Mutation-checked by putting
  `HashMap::new()` back at the `do_types` site.
- `BUG-34`'s two cases, as that ticket writes them, each report exactly one error. They pass
  today and are pinned so they stay.
- `f : Int -> Int` with `f x = x <+> 1`, where nothing declares `<+>`, beside `g : Int` with
  `g = f 1`: the errors are exactly one `VariableNotFound`, `broken` is exactly `f` with
  `tpe: Some`, and `values` holds `g`. An operator is used, here and below, because
  [`TOOL-12`](tool-12.md) turns a misspelt *name* into a hole and leaves an unresolved
  operator a broken declaration.
- `f : Nope -> Int` with `f x = x`: exactly one `TypeNotFound`, and `f` is `Broken` with
  `tpe: None`. With the body changed to `x <+> 1`, both errors are reported.
- `module A exposing (ok, missing)` over an annotated `ok` and an annotated, unexposed
  `hidden`: `ExportNotFound` is reported, and `to_interface`'s `values` holds `ok` and not
  `hidden`. Mutation-checked by restoring `Exports::Everything` on a failed `do_exports`.

Tests in `crates/zelkova/tests/pipeline.rs`:

- [`TOOL-8`](tool-8.md)'s companion test over `package_import_canonical_error` is inverted:
  the errors are exactly one `Canonical` for `A`, and none names `B`. `failing` holds `A`
  with `ok` in `ir.declarations` and `bad` in `ir.unchecked` with `reported: true`.
- The `f`/`g` module through `check_module_recovering`: `g` is in `ir.declarations` with a
  `tpe` that displays as `Int`. Mutation-checked by leaving `broken` out of the typer's
  `global`, which turns `g` into an `Unchecked`.
- A new fixture, `tests/fixtures/package_import_unresolved_import/`, where `A` writes
  `import Nope`: `B` still reports the missing module. That pins where this ticket stops;
  [`TOOL-10`](tool-10.md) inverts it.

And `cargo test --workspace` is green, `cargo run -- compile std/core` still prints
`parsed 10 modules`, lists all ten as checked and exits 0, and `cargo run -- test std/core`
still reports `98 tests: 98 passed, 0 failed, 0 errored`.

**Closing:** this closes `BUG-34` too. Delete both files and tombstone both rows.
