# LANG-6 · A module's declared name is unrelated to the file it lives in

**Sizing:** small-to-medium. The check itself is small; deciding where it lives, and what a
package's source root is, is the part that can grow.

**Location:** `src/compiler/source/files.rs` — `SourceFile::load_private`, which computes
`module_name` from the relative path and then marks the field `#[allow(dead_code)]`;
`src/compiler/mod.rs` — `compile_package`'s parse loop, which builds
`module_files: HashMap<Name, SourceFileId>` from `module.name` and carries the standing
`// TODO Verify modules name match file system.` right underneath it.

**Decided (`SPEC-3`, by the language owner):** a module's declared name must match the path
of the file holding it, relative to the package's source root: each `.` in the name is a
directory separator, with `.zel` appended. `module Ui.Widget` lives at `Ui/Widget.zel` and
nowhere else. No two files in a package may declare the same module name.

That rule is what makes an `import` answerable without a search — a reader who sees
`import Ui.Widget` knows which file to open, and a compiler resolving it can go straight to
the file rather than parsing every module in the package first.

**Problem:** neither half is checked. `SourceFile` computes the path-derived name and never
compares it to the header, so a file at `Sub/Thing.zel` may declare `module Elsewhere` and
be imported as `Elsewhere`:

```
src/Sub/Thing.zel     module Elsewhere exposing (x)
src/Main.zel          import Elsewhere            -- resolves today
```

Two files declaring the same module name are accepted too, silently: `compile_package`'s
`module_files.insert(module.name.clone(), id)` overwrites, `ModuleWalker` builds a graph
with two nodes of the same name, and `check_in_order`'s `interfaces.insert` keeps whichever
was checked last. Reproduced by writing two one-declaration modules both headed
`module Same` into a directory and calling `compile_package` on it: it prints
`parsed 2 modules`, lists both as checked, and returns `Ok(())`.

Found while writing [`docs/spec/modules.md`](../spec/modules.md) (`SPEC-3`), whose *The name
and the file* section carries the `**Not implemented:**` paragraph.

**Approach:**

1. `SourceFile` already has both halves in `load_private` — the header is not parsed at that
   point, so the comparison cannot happen there. Either keep `module_name` and compare it in
   `compile_package` once the module is parsed (smallest diff, and the loop already holds
   both), or expose the path-derived name so a caller can.
2. Report it as a `CompilationError` naming the file and both names — the declared one and
   the one the path implies — so the message says which of the two to change. It has a
   `SourceFileId` in hand, so the label can point at the module header.
3. Reject a second file declaring an already-seen module name in the same loop, naming both
   files. `module_files` is the map that would silently lose one, so the check belongs at its
   `insert`.

The package source root is `compile_package`'s `package_path` argument today — `std/core/src`
for `cargo run` — which is enough to implement this. Whether a package may have more than one
source root is a *Packages and source layout* question and this ticket does not answer it.

**Acceptance:** two `tests/pipeline.rs` tests over `tests/fixtures/` packages: one where a
file's declared name does not match its path, asserting `compile_package` returns `Err` with
an error naming the file and both names; one where two files declare the same module name,
asserting `Err`. `cargo run` must still print `parsed 8 modules` and list all eight as
checked — every module under `std/core/src/` is already correctly placed, `Js/Basics.zel`
declaring `module javascript Js.Basics` included, so this must not move. No block in
`docs/spec/modules.md` goes red: a fenced block is source text with no path behind it, so
this rule cannot be expressed as a tagged example. Its `**Not implemented:**` paragraph has
to be rewritten by hand when this lands, and the chapter says so.
