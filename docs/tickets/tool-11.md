# TOOL-11 · A module with a syntax error is dropped from the build

**Sizing:** medium. Nothing is left to decide: the rules are
[`DEC-23`](../decisions/dec-23.md) decisions 3 and 4. It touches the parser's AST, though not
the grammar, so `parser::Module`'s struct literals each gain a field: seventeen of them,
fifteen in `crates/zelkova-syntax/tests/parser/`.

**Part of:** the *Active work: editor support* section of [the index](README.md), fourth of
the five tickets `TOOL-8` through [`TOOL-12`](tool-12.md). With it the floor is complete: no
failure in one declaration hides its module.

**Depends on:** [`TOOL-10`](tool-10.md), for the incomplete flag, and `TOOL-4` (closed), which
is what hands back the declarations that parsed beside the ones that did not.

**Location:** `crates/zelkova-syntax/src/parser/mod.rs` — `Failure`, `Parsed`, `Module`,
`parse_recovering` and `parse_chunks`; `crates/zelkova-compiler/src/lib.rs` — `parse_root`,
which keeps a module only when `failures.is_empty()`, `ParsedRoot`, `record_parse_status`,
`compile_in_build` and `Interface`; `crates/zelkova-compiler/src/source/files.rs` —
`SourceFile`, whose `module_name` is computed and never read;
`crates/zelkova-compiler/src/canonical/mod.rs` — `canonicalize_recovering`, `do_values` and
`do_infixes`.

**Problem:** `parse_recovering` hands back every declaration that parsed, and `parse_root`
throws the module away if any did not. With the package from [`TOOL-8`](tool-8.md)'s Problem
and `bad`'s body changed to `= T`:

```
failure parsed 1 modules, 1 failed to parse
error: unexpected token: `Equal`
   ┌─ acme-imp:src/A.zel:11:7
   …
error: [B] cannot find a module named `A` to import
```

`A` is not in the build at all, so `B`'s error is false and nothing of `A` has a typed tree.
A body being typed is a syntax error most of the time, so in an editor this is the state the
file is in.

Keeping the module is not enough on its own. `bad : T` parsed and its binding did not, so
`bad` reaches canonicalization as an annotation with no binding and is reported a second time,
as `NoBindings`. A failed declaration with no annotation leaves no trace at all, and every
reference to it is a missing name.

**Approach:** in one PR.

1. **A failed chunk says which value it declares, when it can.** Add
   `pub declares: Option<Name>` to `parser::Failure`. `parse_chunks` sets it to `Some(name)`
   for a declaration chunk whose first item is the token `LowerIdentifier(name)`, and to
   `None` for every other chunk and for the header. A chunk opening on a lowercase identifier
   is the annotation or a binding of the value of that name: those are the grammar's only two
   `Decl` alternatives that open on one. A chunk opening on `unsafe` or another soft keyword
   is `None`.

2. **The module records what it is missing.** Add to `parser::Module`

   ```rust
   /// The declaration chunks that failed to parse, in source order.
   pub failed: Vec<Failed>,
   ```

   ```rust
   pub struct Failed {
       /// The chunk's source text, as `Failure::span`.
       pub span: Span<BytePos>,
       /// The value the chunk declares, as `Failure::declares`.
       pub declares: Option<Name>,
   }
   ```

   `parse_recovering` fills it from its failures. `parse` leaves it empty, since it has no
   module to return when anything failed. `Parsed::module`'s doc comment says the module has
   no trace of a failed declaration; that sentence goes.

3. **`parse_root` keeps a module whose header parsed.** Whatever its failures, the module
   joins `modules`, `module_files` and `local_modules`, and every failure's error is pushed as
   today. `ParsedRoot::failures` still counts each file with any syntax error once, and
   `record_parse_status`'s line keeps its meaning: the first number is the files that parsed
   whole.

4. **A value named by a failed chunk is broken, by name.** In `canonicalize_recovering`:
   - every `declares: Some(name)` is registered with `insert_top_level_value`, in both
     branches, so a reference to it resolves;
   - in `do_values` and the facade branch, a function whose name a failed chunk declares is a
     `Broken`. Its body is not canonicalized and no error is reported for it, `NoBindings`
     included, because the syntax error already says what is wrong. Its annotation is read as
     [`TOOL-9`](tool-9.md) reads any other, so `tpe` is `Some` when one parsed and
     canonicalizes;
   - a `declares: Some(name)` with no `parser::Function` of that name is a `Broken` with
     `tpe: None` and the chunk's span;
   - `do_infixes` accepts a failed name as the function an `infix` declaration names.

5. **An unnamed failed chunk makes the scope incomplete.** A `declares: None` sets the
   environment's flag before `do_infixes` runs. `Module::incomplete` is also true when a
   failed chunk names a value that ends up with `tpe: None`: its annotation may be the chunk
   that failed.

6. **A module whose header did not parse is still a module that exists.** Give `SourceFile` a
   `module_name()` reader for the name it already derives from its path. `ParsedRoot` gains
   `headless: Vec<(Name, SourceFileId)>`, one entry per file whose `Parsed::module` is `None`.
   Before `check_root` runs over `src/`, `compile_in_build` inserts into `interfaces`, for
   each headless name no parsed module declares, an `Interface` holding nothing with
   `incomplete: true`. Add `Interface::unavailable(module_name, file)` to build it. An
   importer then resolves the module, finds nothing in it, and by
   [`TOOL-10`](tool-10.md) reports nothing. The package has an error, so it publishes nothing
   and the stand-in never leaves it.

`parse_root`'s doc comment says a file with a syntax error contributes no module, and has to
change with it.

**Acceptance:**

Tests in `crates/zelkova-syntax/tests/parser/recovery.rs`:

- `declares` is `Some("bad")` for `bad = = T` and for an annotation cut short, `f : Int ->`.
  It is `None` for `type U = (`, for `unsafe f : (`, and for a failed header. The module's
  `failed` matches the failures' spans and names. Mutation-checked by returning `None`
  always.

Tests in `crates/zelkova-compiler/tests/canonical.rs`, each parsing with `parse_recovering`:

- `f : Int -> Int` with `f x = = x`, beside `g : Int` with `g = f 1`: canonicalization
  reports nothing, `f` is `Broken` with `tpe: Some`, and `g` is a value.
- `h = = 2` with no annotation, beside `g : Int` with `g = h`: canonicalization reports
  nothing and the module's `incomplete` is true.
- The control: `f : Int` with no binding, in a module with no syntax error, still reports
  `NoBindings`. Mutation-checked by dropping the name test in step 4, which silences it.
- `type U = (` beside `k : U` with `k = MkU`: canonicalization reports nothing, and `k` is
  `Broken`.

Tests in `crates/zelkova/tests/pipeline.rs`:

- A new fixture, `tests/fixtures/package_import_syntax_error/`, the two modules of
  [`TOOL-8`](tool-8.md)'s Problem with `bad = = T`. The errors are exactly one
  `CompilationError::Source`. `failing` holds `A` with `ok` in `ir.declarations`, typed `T`,
  and `bad` in `ir.unchecked` with `reported: true`, and holds `B` with nothing unchecked.
  Mutation-checked by restoring `failures.is_empty()` in `parse_root`.
- A new fixture, `tests/fixtures/package_import_header_error/`, where `A` opens
  `module A exposing (ok,`: the errors are exactly one `CompilationError::Source`, and none
  names `B`. Mutation-checked by not inserting the stand-in interface.
- `a_parse_failure_does_not_also_report_its_module_as_unheld` still passes unchanged.
- `zelkova::compile_package_into` on the first fixture returns `Err` and leaves its build
  directory absent.

And `cargo test --workspace` is green, `cargo run -- compile std/core` still prints
`parsed 10 modules`, lists all ten as checked and exits 0, and `cargo run -- test std/core`
still reports `98 tests: 98 passed, 0 failed, 0 errored`.
