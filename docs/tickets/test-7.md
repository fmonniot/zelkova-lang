# TEST-7 · `std/core`'s companion checks are run by `node --test` and not as Zelkova tests

**Sizing:** medium. The checks are mechanical to port: 66 `test(…)` calls across three files
become exported functions, plus two `.zel` files per companion. The resolution question in
Approach step 2 is what could make it larger, since it may need a compiler change to the build
layout.

**Part of:** [Active work: effects](README.md#active-work-effects), as its last step. It is the
"what comes after" that the bootstrap section left unfiled because it needed `Task` first.

**Depends on:** [`LANG-76`](README.md) (closed), for a `Test` that holds a `Task`; [`GEN-16`](gen-16.md),
for the wrapper that turns a check that throws into `Err (Threw ..)`; [`LANG-72`](README.md) (closed)
and [`GEN-20`](README.md) (closed), for the `()` in each check's type; [`LANG-68`](README.md)
(closed), so that the new facades are held to the shape they declare.

**Location:** `std/core/tests/Js/BasicsChecks.mjs` (29 checks), `BitwiseChecks.mjs` (12),
`UtilsChecks.mjs` (25), each registered with `node:test` and pointed at directly, as their headers
explain; `src/compiler/mod.rs` — where a facade under `tests/` gets its companion placed (the
comment citing *Testing a companion*); `CLAUDE.md`'s *Commands* section, which gives the
`node --test 'std/core/tests/**/*.mjs'` line; [`TEST-3`](test-3.md), whose step 2 runs that line in
CI.

**Problem:** [*Testing a companion*](../spec/interop.md#testing-a-companion) and
[`DEC-14`](../decisions/dec-14.md) specify a companion's checks as a **test facade** under `tests/`
that declares each check as `Task (Result Failure ())`, beside a test module that exposes one
`Test` per check. [Decision 4](../decisions/dec-14.md#4--the-layout-is-adopted-before-anything-can-run-it)
put the `.mjs` files at their final path early and pointed `node --test` at them "until a Zelkova
one exists". Once the tickets above land, that runner exists, and the interim arrangement is the
second copy of a rule that `DEC-14` wanted to avoid.

**Approach:**

1. For each of the three files, rewrite every `test('…', () => {…})` as an exported function
   with an identifier name, the way the chapter's example does (`export function idivTruncates()`).
   Keep the `PINS`/`GUARD` label as a comment on each one: it records how the check was
   verified, and a Zelkova name cannot carry it. Declare each export in a sibling facade,
   `module foreign Js.BasicsChecks` and so on. Then add a test module (`Js/BasicsTests.zel` or
   similar) that exposes one `Test` per check through `Test.succeeds`, which prints a
   failing check's `Threw` description beside `FAIL`. The two roots [share module
   names](../spec/packages.md#source-roots), so none of these may be called `Js.Basics`.
2. **Resolve the import each check file makes of the companion under test.** They import
   `../../src/Js/Basics.mjs`, a path relative to the *source* tree, as
   [decision 3](../decisions/dec-14.md#3--the-test-companion-reaches-the-companion-under-test-as-a-module-of-its-target)
   allows. The build copies a companion to `build/test/js/<package>/<module path>.companion.mjs`,
   where that relative path no longer resolves. The ticket does not pick how to fix this. The
   options are: rewrite the specifier at copy time; place a test companion so that its source-relative
   imports still resolve; or have a test companion import its target by the path the build gives
   it (`./Basics.companion.mjs`). That last one is a rule the chapter would then have to state.
   Whichever is chosen, the chapter sentence lands first, in its own commit, if it changes what a
   companion author writes.
3. Delete the `node:test` scaffolding and the "interim harness" paragraphs from the three headers.
   Change `CLAUDE.md`'s *Commands* line and [`TEST-3`](test-3.md)'s step 2 so they no longer run
   `std/core/tests/**/*.mjs` with `node --test`. `runtime/js/tests/zelkovaChecks.mjs` is not a
   package's companion and keeps its `node --test` line.
4. Update [`DEC-14`](../decisions/dec-14.md)'s *What nothing checks* and the **Not implemented:**
   paragraph under *Testing a companion*. The facade `zel` block there is retagged `expect=ok` if
   it now compiles.

**Tests:** the ported checks are the tests. As with any port, see each one fail: break the
behaviour one `PINS` check names in the companion under test, confirm `zelkova test std/core` reports
exactly that check as `FAIL`, then restore. Do this for at least one check per file, and for one
that throws rather than returns.

**Acceptance:** `zelkova test std/core` runs all 66 checks and reports each one under its test
module's name. `std/core/tests/Js/` holds no `node:test` import. A deliberately broken companion
function makes the matching test fail and the run exit `1`. Nothing in `CLAUDE.md` or `TEST-3`
still points `node --test` at `std/core/tests/`.
