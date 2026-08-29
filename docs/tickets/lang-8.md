# LANG-8 · There is no default import list

**Sizing:** medium. The mechanism is small; the standard-library modules it names do not all
exist yet, and that is the part that decides the size.

**Location:** `src/compiler/canonical/environment.rs` — `new_environment`, which builds a
module's scope from `&Vec<parser::Import>` and nothing else; `src/compiler/mod.rs` —
`compile_package`, which is where the package a module belongs to and the interfaces
available to it are both known.

**Decided (`SPEC-3`, by the language owner):** every module behaves as though it began with
these seven imports, whether they are written or not:

```
import Basics exposing (..)
import List exposing (List)
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import Char
import String
import Tuple
```

Writing any of them explicitly is allowed and changes nothing. Nothing else is implicit.

The list is chosen so the types that appear in ordinary annotations are always writable:
`Maybe` and `Result` bring their constructors because a `Just` that had to be qualified in a
`case` branch would be worse than useless, `List` comes as a bare type because its module's
functions read better qualified.

**Problem:** nothing is implicit today. A module resolves only what it declares and what it
imports by hand, so `x = 1 + 2` in a module with no `import Basics` fails with
`VariableNotFound` for `+`, and every module under `std/core/src/` opens with an explicit
`import Basics`. `new_environment` has no notion of an import the user did not write.

Found while writing [`docs/spec/modules.md`](../spec/modules.md) (`SPEC-3`), whose *The
default imports* section carries the list and the `**Known gap:**` block.

**Approach:** the mechanism is to synthesise the seven `parser::Import`s and run them
through `process_import` ahead of the module's own, so implicit and explicit imports go
through one code path and a name arriving implicitly is indistinguishable from one written
by hand. Four things that have to be settled first, and this ticket settles none of them:

1. **Where the list lives.** A constant in `canonical/environment.rs` is the smallest thing
   that works. It also hard-codes standard-library module names into canonicalization, which
   is a layering choice worth making deliberately rather than by default.
2. **What happens inside `std/core` itself.** `Basics` cannot implicitly import `Basics`, and
   `Maybe` importing `Result` importing `Maybe` is the cycle
   `dependencies::ModuleWalker` exists to reject. The implicit list has to be suppressed
   inside the package that defines it, or the modules on the list have to be exempt, or both.
   This is the substance of the ticket, not a detail.
3. **The modules that do not exist.** `List`, `Char` and `String` are `.ignored` files under
   `std/core/src/` — they do not compile and are invisible to the source loader. Importing a
   module that is not there is `EnvError::InterfaceNotFound`, so an implicit import of `List`
   would break every module in the package. Either the list lands in stages as those modules
   are ported, or a missing implicit import is silently skipped — and silently skipping is
   the kind of rule that makes a later diagnostic incomprehensible.
4. **Spans.** A synthesised import has no source text behind it. `NodeSpan::none()` is the
   honest answer and every error path that would point at an implicit import has to render
   without a caret rather than pointing at an arbitrary line.

Depends on nothing, but [BUG-16](bug-16.md) is easier once this lands: it makes an unknown
type name an error, and today `Int` is an unknown type name in every module that does not
import `Basics`.

**Acceptance:** a `tests/pipeline.rs` test over a `tests/fixtures/` package where a module
writes `x = 1 + 2` with no `import Basics` and compiles, resolving `+` to `Basics`. A second
test asserts an explicit `import Basics exposing (..)` in the same position still compiles,
so the implicit import does not collide with itself and become an `AmbiguousVariables`.
`cargo run` still prints `parsed 8 modules` and lists all eight as checked, which is the real
test of point 2. The `**Known gap:**` block in [`docs/spec/modules.md`](../spec/modules.md)
— the standalone `x = 1 + 2` module, tagged `expect=canonical-error:VariableNotFound` — goes
red either way: it compiles if the spec harness can see `Basics`, and fails with a different
error if it cannot. Whichever happens, the block is retagged and its paragraph rewritten in
the same change.
