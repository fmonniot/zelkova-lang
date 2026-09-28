# LANG-73 · `std/core` declares no `String`, so no annotation can name one

**Sizing:** small. It adds one module declaring one opaque type. What could make it bigger: the
default imports, or the scalar lookup in `src/compiler/scalars.rs`, might react badly to `String`
starting to resolve. Both were written so that it does not ("each entry starts working on the
day its module compiles"), and this is the first time that claim is exercised for a scalar.

**Part of:** [Active work: effects](README.md#active-work-effects).
[`Failure`](../spec/evaluation-semantics.md#an-effect-that-can-fail)'s two constructors each
carry a `String`, so [`LANG-74`](lang-74.md) cannot declare `Failure` until this lands.

**Location:** `std/core/src/String.ignored` — Elm's `String` module, whose
`type String = String` is the declaration wanted and whose `Elm.Kernel.*` imports are what keeps
it ignored; `src/compiler/scalars.rs` — `STRING`, already keyed on `String.String`;
`src/compiler/default_imports.rs` — the `String` entry, inert today because the module is
missing.

**Problem:** [Scalar types](../spec/types.md#scalar-types) says `String` is declared in Zelkova in
the module `String` and reaches every module through [the default
imports](../spec/modules.md#the-default-imports). The compiler already knows `String.String` by
qualified name and holds its admitted-type predicate. What is missing is the declaration: `std/core`
has only `String.ignored`, so `String` is an unresolved type name everywhere, and
[modules.md's *Known gap*](../spec/modules.md#the-default-imports) lists it among the four entries
that bring nothing.

This ticket does not add string literals. `Failure` carries a `String` that the facade wrapper builds
from a JavaScript string, and nothing on the effects path writes one in source.
[String literals](../spec/lexical-structure.md#strings) stay unimplemented, and so does every
function in Elm's `String` module.

**Approach:**

1. Add `std/core/src/String.zel` declaring `module String exposing (String)` and the opaque
   `type String = String`, in the same shape `Basics` gives `Int` and `Float`. It needs no facade
   and no companion.
2. Decide what happens to `String.ignored`'s body, and say which in the PR. The source walk in
   `src/compiler/source/mod.rs` reads only `.zel`, so the file can stay beside the new one as the
   port's reference. Its `type String` declaration should go, or carry a note, so that it does not
   read as a second declaration of the type.
3. Confirm that the default import now brings `String` into scope in an ordinary module and in
   `Basics`'s dependents, and that a facade signature naming `String` is admitted by identity.
4. Update [modules.md's *Known gap*](../spec/modules.md#the-default-imports) from four missing
   modules to three, and `default_imports.rs`'s module comment, which names `String` among the
   `.ignored` files.

**Tests:** `tests/pipeline.rs::stdlib_package_compiles` counts modules and has to change with them.
Add a `tests/pipeline.rs` case where a module in a package depending on `std/core` annotates
`f : String -> String` with no import and checks.

**Acceptance:** `cargo run -- compile std/core` prints `parsed 9 modules` and lists all nine as
checked. A module annotating a value `String` with no `import` checks. A `module foreign`
signature `unsafe len : String -> Int` canonicalizes without `FacadeTypeNotAdmitted`.
`cargo test --workspace` is green.
