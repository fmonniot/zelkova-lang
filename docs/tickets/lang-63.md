# LANG-63 · Nothing declares `Test`, and nothing runs a package's tests

**Sizing:** large, and blocked. The two halves are one ticket because neither is useful alone:
a `Test` nothing runs is a type with no meaning, and a runner with no type to look for has
nothing to find. It is sequenced after [`GEN-1`](gen-1.md) — running a test means running a
Zelkova value, and nothing runs one today — and after [`LANG-9`](lang-9.md), since
`zelkova-test`'s own signatures need a type argument that is not a bare name. What could make
it bigger: `Test` is a value describing work, so what it holds and how a runner performs it is
entangled with `Task` and with [`DEC-11`](../decisions/dec-11.md)'s effect model, neither of
which exists.

**Location:** `src/compiler/mod.rs` — `compile_package_with_tests`, which compiles both source
roots and stops there; it has no caller outside `tests/pipeline.rs`, and `src/main.rs` calls
`compile_package("std/core")` and takes no arguments, so nothing in the toolchain can ask for a
package's tests. `src/compiler/mod.rs` — `Interface`, which is what a runner would read a test
module's exposed values and their types out of. There is no `std/test` or `zelkova-test`
package in the tree; `std/core` is the only package `std/` holds.

**Decided ([`docs/spec/packages.md`](../spec/packages.md),
[`docs/spec/toolchain.md`](../spec/toolchain.md)):**

- [**A test is a value a module under `tests/` exposes whose type is
  `Test`**](../spec/packages.md#what-a-test-is). Every such value is a test and nothing else in
  the module is one, so a test module may declare and expose helpers.
- `Test` is a type `zelkova-test` declares and exposes **without its constructors**. It reaches
  a test module through [`test-dependencies`](../spec/packages.md#test-dependencies) and
  nothing else, so a module under `src/` cannot name it at all.
- A runner has the type to go on and nothing else: no name is fixed, and no manifest field
  lists the tests. It reads the module's interface, and the `exposing` clause decides what is
  in one.
- [Running a package's tests](../spec/toolchain.md#running-a-packages-tests) compiles both
  roots and then runs every test the package holds. A package's tests are run by that package,
  and a dependency's tests are never run — they are not compiled at all.
- A run with a failing test exits non-zero, and a run that could not compile either root is the
  same non-zero exit a failed build already is.

**Problem:** the roots are compiled and nothing happens next. `LANG-15` gave a
package its `tests/` root, the `test-dependencies` field and `compile_package_with_tests`, and
excluded the runner explicitly. So a test module is checked like any other module and then
dropped: no pass looks at what it exposes, nothing anywhere declares `Test`, and a value of
that type could not be run if one existed, because code generation has not started
([`GEN-1`](gen-1.md)). A package that wants to test itself today can write the module and have
it type checked, which is the whole of what it gets.

Six prose sites say so and, until this ticket, named nothing: the **Not implemented:**
paragraphs of [*What a test is*](../spec/packages.md#what-a-test-is),
[*Running a package's tests*](../spec/toolchain.md#running-a-packages-tests) and
[*Testing a companion*](../spec/interop.md#testing-a-companion), and clauses in
[`DEC-10`](../decisions/dec-10.md), [`DEC-11`](../decisions/dec-11.md) and
[`DEC-14`](../decisions/dec-14.md).

**Approach:**

1. `std/test/` — a package named `zelkova-test`, with a manifest and a `Test` module declaring
   `Test` and exposing it without its constructors. What a `Test` *holds* — a name, a check, a
   group of other tests — is not decided here and is the part that needs `Task`: a check that
   cannot fail at runtime is not a check, and failing is an effect.
2. A pass over each checked test module's `Interface`, collecting every exposed value whose
   type is `Test`. This is the half that can be built and observed before there is any way to
   run one, and the half the chapter's rule is actually about.
3. The runner: hand each collected value to whatever `GEN-1` emits, report per test, and exit
   non-zero on a failure or on a root that did not compile.
4. A caller. `src/main.rs` takes no arguments today, so there is no `zelkova test` to be asked;
   whether this ticket grows one or a later toolchain ticket does is not decided here.

**This ticket does not pick what a `Test` holds.** Two shapes are live and the chapter settles
neither: `Test` as an opaque description a runner interprets, like Elm's `elm-explorations/test`,
or `Test` as a `Task` of a result, which makes a check an ordinary effect and needs no runner
vocabulary beyond running a `Task`. The first is testable before effects exist; the second is
smaller once they do. Whoever picks this up decides, or splits step 1 out and asks.

**Acceptance:** steps 1 and 2 are checkable on their own and are what this ticket can be held
to before `GEN-1` lands. A `tests/pipeline.rs` test over a `tests/fixtures/` package that
depends on `zelkova-test` through `test-dependencies`: its `tests/` module exposes two values,
one of type `Test` and one helper of another type, and the collected set names the first and
not the second. A `src/` module naming `Test` still fails as a name that resolves to nothing,
which is the existing `a_test_dependency_does_not_reach_the_src_root` rule and needs no new
test. `cargo run` must still print `parsed 8 modules`, list all eight as checked and exit 0 —
adding a package under `std/` must not change what `std/core` compiles to.

**No block in `docs/spec/packages.md` goes red when this lands.** The chapter's `Test` example
is prose rather than a tagged block. The three **Not implemented:** paragraphs above name this
ticket and have to be deleted or narrowed by hand as part of it, as do the three decision
entries.

**Found while closing `LANG-15`**, in review of PR #227: that ticket was the only one naming
the runner, and closing it left the six sites above citing a gap nothing tracked.
