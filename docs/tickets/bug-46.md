# BUG-46 · `Js.Utils.compareInt` and `compareFloat` declare an `Int` result their companion returns as a number

**Severity:** low (every call aborts the program, but nothing in `std/core` calls either today;
`Basics.compare` is written without them).

**Location:** `std/core/src/Js/Utils.zel` — `compareInt : Int -> Int -> Int` and
`compareFloat : Float -> Float -> Int`; `std/core/src/Js/Utils.mjs` — `compare`, exported under
both names, which returns `_Utils_cmp`'s `-1`, `0` or `1` as JavaScript numbers.

**Problem:** an `Int` crosses the JavaScript boundary as a `bigint`
([Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)),
and since [`GEN-2`](README.md) the value an `unsafe` facade's companion returns is checked
against its declared type, aborting the program when it fails. `compare(1n, 2n)` returns the
number `-1`, so any call to either facade aborts with

```
`Js.Utils.compareInt`'s companion returned a value its declared type, `Int`, does not admit
```

Nothing reaches it yet: `Basics.compare` answers `EQ` without calling anything, and the body
meant to replace that, which calls `Js.Utils.compareInt`, is commented out until `let` parses
([`LANG-33`](lang-33.md)).
[`LANG-42`](lang-42.md)'s third point plans to call `compareInt` per `Comparable` instance, and
will meet this the first time it does.

`std/core/tests/Js/UtilsChecks.mjs` pins the mismatch rather than catching it: its `compare`
checks assert `compareInt` returns the numbers `-1`, `0` and `1`.

**Fix:** either return `BigInt(_Utils_cmp(a, b))` from the companion's `compare`, or change
both signatures to a result the companion honours as it is — `Float` is the only admitted type
a JavaScript number satisfies, and an `Order`-shaped union would need the companion to build
`{$: "LT"}` and so on. The first is the smallest change; the ticket does not pick.

**Acceptance:** the checks in `std/core/tests/Js/UtilsChecks.mjs` assert that `compareInt` and
`compareFloat` return a value their declared result admits, and
`node --test 'std/core/tests/**/*.mjs'` passes.

**Found:** by the first attempt at [`GEN-2`](README.md), while surveying which `std/core`
facades return what their signatures declare. Left unfixed there because nothing calls them.
