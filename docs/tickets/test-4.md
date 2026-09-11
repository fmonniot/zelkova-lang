# TEST-4 · A facade's `.mjs` companion test lives in the compiler repo, not in the package that ships the companion

**Sizing:** small-to-medium (a convention plus wiring to discover and run it; grows if it turns
out to need real package boundaries first — see **Approach**).

**Location:** `tests/js/Utils.test.mjs` on the open, unmerged `fix/bug-20-js-utils-guards`
branch — not yet on `main`, which has no `.test.mjs` file at all. That branch's `CLAUDE.md`
names `tests/js/*.test.mjs` as the invocation. [`docs/spec/packages.md`](../spec/packages.md)'s
*Source roots* section, which defines `src/` and `tests/` for `.zel` modules only — "every file
under a root whose name ends in `.zel` is a module of the package"; a `.mjs` file under either
root is not read at all. [`docs/spec/interop.md`](../spec/interop.md)'s note that a facade's
companion "sits next to it in the same directory" under `src/`.

**Found:** while working [`BUG-20`](bug-20.md) (still open). Its first commits added
`std/core/src/Js/Utils.test.mjs` — the location [`TEST-3`](test-3.md) was filed against — and a
later commit on the same branch moved it to `tests/js/Utils.test.mjs`, so the companion test now
lives under the compiler repository's own `tests/` tree rather than beside the facade it
exercises. Neither location has merged to `main` yet, but the branch's direction is the thing
worth getting right before it does.

**Problem:** `std/core/src/Js/Utils.mjs` is a file that ships as part of the `std/core`
package — [`docs/spec/packages.md`](../spec/packages.md) says `src/` "is what the package
*is*, and the only thing it ships." Its test, wherever `BUG-20` lands it, does not: `tests/js/`
sits in `zelkova-lang`'s own `tests/` directory, addressed by a path that exists because this
repository happens to also be the compiler's source tree, not because any package convention
says JavaScript-companion tests belong there. A third-party package that ships a
`foreign` facade with its own `.mjs` companion has nowhere prescribed to put that companion's
test at all — `docs/spec/packages.md`'s `tests/` root is walked for `.zel` files only, so a
`.test.mjs` dropped there is silently never read, and nothing in `docs/spec/interop.md` says
where a companion's own test should sit relative to it.

This also means the test would travel with the compiler, not with the package: distributing
`std/core` on its own (once [`LANG-13`](lang-13.md) and [`LANG-14`](lang-14.md) make that
possible) would leave `Utils.test.mjs` behind in `zelkova-lang`, so a consumer who patches the
companion has no test to run against their change, and CI for `std/core` alone (should it ever
exist independently of this repo) cannot see it either. [`TEST-3`](test-3.md) is about wiring
*a* harness for `.mjs` tests wherever they live; this ticket is about *where they live*
gaining a package-relative answer.

**Approach:** this ticket does not pick between the options; two are worth weighing side by
side rather than defaulting to whichever is closer to today's ad-hoc placement:

- **Beside the companion, under `src/`**, e.g. `std/core/src/Js/Utils.test.mjs` — where the
  file lived before `BUG-20`'s move. Matches "sits next to it in the same directory" from
  [`docs/spec/interop.md`](../spec/interop.md) and needs no new root or manifest field, but
  means a file ending in `.mjs` is *conventionally* a test only by a naming pattern
  (`*.test.mjs`) that the compiler's `src/`-walk (which only reads `.zel`) is indifferent to,
  and it ships inside `src/`, which the spec calls "the only thing it ships" — arguably correct
  for a companion's own test, arguably not.
- **Under `tests/`, package-relative** (e.g. `<package>/tests/js/Utils.test.mjs`), mirroring
  where `.zel` test modules live per [`docs/spec/packages.md`](../spec/packages.md). This keeps
  every kind of test under one root but requires deciding whether the walk that finds `.zel`
  test modules also needs to find `.mjs` ones, and needs [`LANG-15`](lang-15.md)'s package test
  root to exist first (today `tests/` has no meaning outside a hardcoded `.zel` walk).

Either option depends on there being a real package directory to place the file in at all —
today `std/core` is a source root passed by `src/main.rs`, not a package with a boundary
([`LANG-13`](lang-13.md), [`LANG-14`](lang-14.md)) — so this ticket may only be resolvable
choosing a convention now and relocating the one existing file once those land, rather than
implementing the final layout today.

**Acceptance:** `docs/spec/interop.md` or `docs/spec/packages.md` states, normatively, where a
`foreign` facade's per-target companion test file belongs relative to the package that ships
the companion. `Utils.test.mjs` (or its successor location) matches that rule, and
`CLAUDE.md`'s *Commands* section and [`TEST-3`](test-3.md)'s CI wiring (if landed by then) point
at the new path rather than `tests/js/`.
