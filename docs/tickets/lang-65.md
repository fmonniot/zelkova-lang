# LANG-65 · Three more `std/core` JavaScript functions still read an `Int` as a number

**Sizing:** small-to-medium — one mechanical fix, and one that repeats a decision
[LANG-64](lang-64.md) is already making for a different pair of functions.

**Location:** `std/core/src/Js/Basics.mjs` — `toFloat`, `pow` — and `std/core/src/Js/Utils.mjs`
— `_Utils_isOrdered`, which backs `compare`, `lt`, `le`, `gt` and `ge`.

**Depends on:** [LANG-56](lang-56.md), which carries `std/core`'s two `Basics`/`Bitwise`
companions to the 64-bit `BigInt` representation of `Int` [DEC-16](../decisions/dec-16.md)
settled, but named its own scope as `add`, `sub`, `mul`, `idiv`, `remainderBy`, `modBy`,
`truncate`, `ceiling`, `floor`, `round` and `Js/Bitwise.mjs`. These three were left out.

**Found:** while implementing LANG-56 — noted at its site rather than fixed, to keep that
ticket's diff to its stated scope.

**Problem:** once `Int` crosses the JavaScript boundary as a `BigInt` rather than a `number`
(LANG-56), three more spots that assume the old representation start giving wrong answers or
throwing:

- **`toFloat`** (`Js/Basics.mjs`) is `return x`. `Basics.zel` types it `Int -> Float`, so it
  hands back a `BigInt` where the type says `Float`. Nothing downstream currently distinguishes
  the two at runtime — there is no code generator yet — but the value is wrong the moment one
  exists.
- **`pow`** (`Js/Basics.mjs`) is `export const pow = Math.pow`. `Basics.zel` types it `a -> a
  -> a`, the same shape `add`/`sub`/`mul` have, and LANG-56 gave those a runtime dispatch on the
  operand's JavaScript type because they back both `Int` and `Float` arithmetic. `pow` needs the
  same dispatch — `Math.pow` on two `BigInt`s throws (`Cannot convert a BigInt to a number`) —
  and it needs the `BigInt` half to use `**`, which itself throws on a negative exponent
  (`Exponent must be non-negative`). Whether `2 ^ -1` (`Int` base, negative `Int` exponent) has
  an `Int` answer at all is a real question with no `docs/spec/` or `DEC-16` answer today — the
  same shape of open decision [LANG-64](lang-64.md) is filing for a negative shift count, not a
  detail this ticket should quietly pick.
- **`_Utils_isOrdered`** (`Js/Utils.mjs`) admits `typeof v === 'number' | 'string' | 'boolean'`.
  An `Int` is now `typeof v === 'bigint'`, so `compare`, `lt`, `le`, `gt` and `ge` — all built on
  `_Utils_cmp`, which calls `_Utils_isOrdered` — refuse to order two `Int`s and fall through to
  the tuple-arity branch, ending in `_Utils_cmp`'s "can only compare" throw. `Basics.compare`,
  `Basics.<`, and friends would all throw on two `Int`s. `'bigint' < 'bigint'` and `===`
  already do the right thing in JavaScript — it's the type-name check that's stale, not the
  comparison itself.

None of the three is reachable at runtime today: there is no code generator (see
[`GEN-1`](gen-1.md)), so nothing calls a facade companion yet. This is why LANG-56 could leave
them and still close — but they block the first `GEN-*` ticket that actually runs emitted code
touching any of `toFloat`, `pow`, or ordering two `Int`s.

**Approach:**

- `toFloat`: `return Number(x)`. Mechanical — `Basics.zel`'s own doc example
  (`3.14 + toFloat (List.length [1,2,3]) == 6.14`) is unaffected either way, and `Number` on a
  `BigInt` in `Int`'s admitted range never loses precision differently than the `2^53` limit
  `DEC-16` decision 2 already accepted for `Float`.
- `pow`: dispatch on `typeof a` the way `add`/`sub`/`mul` do, keeping `Math.pow` for the `Float`
  case. The `Int` case needs [LANG-64](lang-64.md)'s sibling question answered first — what a
  negative `Int` exponent means — so this ticket should land the `Float` and non-negative-`Int`
  cases and either resolve the negative-exponent question itself (if it turns out simple) or
  split it into its own ticket the way LANG-64 did for shifts. Don't guess.
- `_Utils_isOrdered`: add `'bigint'` to the admitted `typeof` set. `_Utils_cmp`'s primitive
  branch (`x === y`, `x < y`) already does the right thing for two `BigInt`s with no further
  change, since JavaScript's `<`/`===` work across same-typed `BigInt` operands the same way
  they do for numbers.

**Acceptance:** `node -e` (or a `node --test` addition alongside `std/core/tests/Js/UtilsChecks.mjs`
and `BasicsChecks.mjs`) shows `toFloat(10n) === 6.14 - 3.14` is false but `toFloat(10n) === 10`
holds as a `Number`; `compare(3n, 5n)`, `lt(3n, 5n)`, `ge(5n, 5n)` etc. answer instead of
throwing; and `pow` either has a committed answer for a negative `Int` exponent with a test
pinning it, or this ticket closes with that case split into a new ticket the way LANG-64 was
split from LANG-56.
