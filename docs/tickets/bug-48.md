# BUG-48 · `Js.Utils`'s structural equality throws a `ReferenceError` on a value nested more than a hundred deep

**Severity:** medium (`==` on a value of an `Eq` type throws instead of answering, under ordinary
use: a user-defined list of a hundred and two elements is enough; nothing in `std/core` itself
compares a value that deep today).

**Location:** `std/core/src/Js/Utils.mjs` — `_Utils_eqHelp`'s `depth > 100` branch, which calls
`_Utils_Tuple2(x, y)`, and `eq`, whose loop reads the pushed pair back as `pair.a` and `pair.b`.
`std/core/src/Js/Utils.zel` — `equalInt`, `equalFloat`, `notEqualInt` and `notEqualFloat`, which
all forward to that code; `std/core/src/Basics.zel` — `eq`, the one a Zelkova `==` reaches.

**Problem:** `_Utils_eqHelp` keeps its recursion shallow by pushing the pair under comparison onto
an explicit stack once `depth` passes 100 and answering `true` for that level, and `eq` pops each
pair afterwards. The push is written `_Utils_Tuple2(x, y)`, which Elm's kernel defined. This file
defines `_Utils_Tuple2__PROD` and `_Utils_Tuple2__DEBUG` and no `_Utils_Tuple2`, so the first
value nested past 100 throws instead of answering. Reproduced against the `std/core` build
(`cargo run -- compile std/core`, then `Js/Utils.companion.mjs` under `build/out/js`), with
`equalInt` called on two equal values and on two that differ only at the innermost leaf:

```
records  {x: {x: … {x: 1n}}}        101 records deep: true, and false at a differing leaf
                                    102 records deep: ReferenceError, both ways
unions   {$:"Cons", a:1n, b: …}     100 Cons cells: true
                                    101 Cons cells: ReferenceError
```

and 150 deep throws as well; 50 deep answers correctly. The error is
`ReferenceError: _Utils_Tuple2 is not defined`, whatever the two values are, so the answer for
two unequal values is lost too. A union is the likelier victim: a recursive union such as a
list is as deep as it is long. A record reaches the same path since `GEN-25`, which pins `==` on
records and does not reach this depth.

Even with the name defined, the stack is read as `pair.a` and `pair.b`, so the pushed value has
to be an object of those two fields: `_Utils_Tuple2__PROD` builds exactly that, and
`_Utils_Tuple2__DEBUG` adds a `$` and is not wanted here.

[`LANG-42`](lang-42.md) step 5 moves structural comparison out of this companion and into derived
instances written in Zelkova; until it lands, `Basics.eq` forwards to this code.

**Fix:** push the pair as the object `eq` reads, `stack.push({ a: x, b: y })`, rather than a
helper this file does not define. The ticket does not pick between that and leaving the
walk to `LANG-42`, which would close this with it; the first is a one-line change, and the second
is not planned with a value this deep in mind.

**Acceptance:** `std/core/tests/Js/UtilsChecks.mjs` asserts that `equal` on two equal values
nested 150 deep answers `true`, and on two that differ at the innermost leaf answers `false`,
for objects shaped as a record and as a union, in a case that is seen to throw
`ReferenceError` before the change; `cargo run -- test std/core` passes. The new case's own
doc comment says which line it was seen red against.

**Found:** while reviewing `GEN-25`, whose first PR body described it as found, not fixed, and
whose reviewer reproduced it. Left unfixed there because the code is `std/core`'s companion and
not a record's emission. Neighbours: [`BUG-20`](bug-20.md), about the types the comparison
facades declare, and `BUG-24`, which closed the other helpers this file called and no file
defined.
