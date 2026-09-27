# LANG-63 · Nothing declares `Test`, and nothing finds a package's tests

**Sizing:** small-to-medium. It adds one small package under `std/`, one pass over interfaces,
and a fixture. What could make it bigger is the identity check in step 2, if matching an
interface's `canonical::Type` against one declaration turns out to need more than comparing a
`QualName`.

**Part of:** the [bootstrap](README.md#active-work-bootstrap) section. Running the tests this
ticket collects is [`LANG-69`](lang-69.md)'s work. This ticket used to carry the runner too, and
it was split off on 2026-09-27 so that the half that can be observed without `node` lands on its
own.

**Location:** `std/`, which holds only `std/core`; no `zelkova-test` package exists.
`src/compiler/mod.rs` has `compile_package_with_tests`, which compiles both source roots and
stops there, and `Interface`, whose `values` map holds each exposed value's `canonical::Type`.
`tests/fixtures/dep_expect` and `tests/fixtures/package_test_dependency` are the existing
stand-ins for a test library and a package using one.

**Decided ([`docs/spec/packages.md`](../spec/packages.md#what-a-test-is)):**

- A test is a value that a module under `tests/` exposes and whose type is `Test`. Every such
  value is a test and nothing else in the module is one, so a test module may declare and
  expose helpers.
- `Test` is a type that `zelkova-test` declares and exposes without its constructors. It reaches
  a test module through [`test-dependencies`](../spec/packages.md#test-dependencies) and through
  nothing else.
- The runner goes by the type alone. No name is fixed, and no manifest field lists the tests.

**Decided by the language owner on 2026-09-27, for the bootstrap:** a `Test` is a pass-or-fail
verdict computed from a `Bool`. It is **named by the value that holds it**, so the runner
reports `MathTest.addsUp` and a `Test` carries no name of its own. This is the shape the front
end can express today. It needs no string literal, list, lambda, unit or `Task`, and all five
are unimplemented. The richer shape the chapter leaves to `zelkova-test` (a named test, a group,
a failure message) grows from this once those constructs exist. Growing it is not this ticket's
work, and nothing here should be read as the final surface.

```zel
module MathTest exposing (addsUp, divisionByZero)

import Test exposing (Test)

addsUp : Test
addsUp =
  Test.equal (1 + 2) 3

divisionByZero : Test
divisionByZero =
  Test.check (7 // 0 == 0)
```

`import Test` rather than `import ZelkovaTest.Test` assumes the `test-dependencies` entry writes
`wrapped = false`, which every entry in this repository does.

**Problem:** the roots are compiled and nothing happens next. `LANG-15` gave a package its
`tests/` root, the `test-dependencies` field and `compile_package_with_tests`. It left the rest
out on purpose. So a test module is checked like any other module and then dropped. No pass
looks at what it exposes, and nothing anywhere declares `Test`.

**Approach:**

1. **`std/test/`**, a package named `zelkova-test`, with a manifest whose `dependencies` names
   `zelkova-core` by `path = "../core"`. [`LANG-62`](lang-62.md) is not on this path, because an
   explicit entry for core already works. The package has one module, `Test`:

   ```zel
   module Test exposing (Test, equal, check)

   type Test
     = Pass
     | Fail

   equal : a -> a -> Test
   check : Bool -> Test
   ```

   `equal` is `check (expected == actual)`. `==` is structural at run time even though its type
   says nothing: `Basics.eq` is `Js.Utils.equalInt`, whose companion is `_Utils_eq`. That holds
   until [`LANG-42`](lang-42.md) gives `equal` an `Eq a =>` constraint, which it will then need.
   `check` is named `check` because `true` is a keyword until [`LANG-1`](lang-1.md) lands.
   `Test` is exposed without its constructors, so a test cannot fake a verdict. Only the runner
   ([`LANG-69`](lang-69.md)) reads `Pass`/`Fail`, and it reads them through the union encoding
   [`DEC-6` decision 3](../decisions/dec-6.md#3--unions-cross-and-their-encoding-is-published-interop-interface)
   publishes.
2. **Collecting tests.** Write a function over a build's checked test modules that returns, per
   module, the exposed value names whose `canonical::Type` is `zelkova-test`'s `Test`, sorted by
   name. Identify the type by its full `QualName`, package included (which `BUG-37` made
   possible), and never by the spelling `Test`. A package that declares its own `Test` type does
   not get its values run. `compile_package_with_tests` has to return what the function needs,
   so the checked test modules or their `Interface`s come back to the caller rather than being
   dropped. Choose the return shape with [`LANG-69`](lang-69.md) in mind, since it is the only
   caller.
3. When `zelkova-test` is not in the build, there are no tests to collect. That is not an error.

**Acceptance:**

- A `tests/pipeline.rs` test uses a new `tests/fixtures/` package that depends on `std/test`
  through `test-dependencies` with `wrapped = false`. Its `tests/` module exposes three values:
  one of type `Test`, one helper of type `Int`, and one of a locally declared type that is also
  named `Test`. The collected set names the first and neither of the others.
- Neutralise-check this test by matching on the unqualified name instead. The third value then
  gets collected, and the test goes red.
- A `src/` module that names `Test` still fails as a name that resolves to nothing. The existing
  `a_test_dependency_does_not_reach_the_src_root` test already pins that rule.
- `cargo run -- compile std/test` compiles the new package and exits 0.
- `cargo run -- compile std/core` still prints `parsed 8 modules`, lists all eight as checked,
  and exits 0. A new package under `std/` must not change what `std/core` compiles to. (Before
  [`GEN-17`](gen-17.md) lands, run a bare `cargo run` for this check.)

**No block in `docs/spec/packages.md` goes red when this lands,** because the chapter's `Test`
is described in prose, not in a tagged block. The **Not implemented:** paragraph of
[*What a test is*](../spec/packages.md#what-a-test-is) narrows by hand to what
[`LANG-69`](lang-69.md) still owes: nothing runs a collected test. It moves its citation to that
ticket. The paragraphs in [*Running a package's tests*](../spec/toolchain.md#running-a-packages-tests)
and [*Testing a companion*](../spec/interop.md#testing-a-companion), and the clauses in
[`DEC-10`](../decisions/dec-10.md), [`DEC-11`](../decisions/dec-11.md) and
[`DEC-14`](../decisions/dec-14.md), are about running, so they move to cite `LANG-69` as part of
this ticket. Run `grep -rn lang-63 docs/` before deleting the file.

**Found while closing `LANG-15`**, in review of PR #227. That ticket was the only one that named
the runner, and closing it left six sites citing a gap that nothing tracked.
