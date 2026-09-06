# BUG-25 · Three of the four `Float -> Int` conversions never wrap, so `round nan` and `round 1.0e20` are not `Int`s

**Severity:** medium (wrong behaviour under normal use — three functions declared to return an
`Int` return a value outside the type for ordinary inputs). It is not high only because no code
generator emits calls into these files yet, so nothing runs them.

**Location:**

- `std/core/src/Js/Basics.mjs`, lines 42–44: `export const ceiling = Math.ceil;`,
  `export const floor = Math.floor;`, `export const round = Math.round;`.
- `std/core/src/Js/Basics.mjs`, line 39: `export function truncate(n) { return n | 0 }` — the
  one that is already right, and the model for the fix.
- The facade declaring their types is `std/core/src/Js/Basics.zel`, lines 26–29; `Basics.zel`
  re-exports all four (`round` at line 276, `floor` at 292, `ceiling` at 308, `truncate` at 324).

**Problem:** `round`, `floor`, `ceiling` and `truncate` are each declared `Float -> Int`, and
`Float` has more values than `Int` has room for. `nan`, both infinities and every finite value
outside the 32-bit range have no integer to convert to, and JavaScript's `Math.round`,
`Math.floor` and `Math.ceil` hand back a JavaScript number for all of them rather than an
integer in range:

| Call | Today | The language's answer |
|---|---|---|
| `round nan` | `nan` | `0` |
| `floor nan` | `nan` | `0` |
| `round (1.0 / 0.0)` | `Infinity` | `0` |
| `round 1.0e20` | `1.0e20` | the low 32 bits of `1.0e20`, as a signed integer |
| `truncate nan` | `0` | `0` |

[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *Numbers* — *Converting
a `Float` to an `Int`* — says a conversion rounds as its name says and then wraps into 32 bits,
the way `Int` arithmetic already wraps, and that `nan` and both infinities convert to `0`. Three
of the four do neither.

The consequence is not a wrong number, it is a value that escapes its type. `Int` is specified
as a 32-bit signed two's-complement integer on every target; `nan` and `Infinity` are not
integers at all, and `1.0e20` is not one of the 2³² that exist. Every `Int` operation downstream
of such a value is then computing on something the language says cannot be there — and because
the same facade is what a future code generator emits calls to, the bad value is produced at the
one place nothing later can check.

`truncate` is already correct: `n | 0` is JavaScript's conversion to a signed 32-bit integer, so
it truncates toward zero and then wraps, and it yields `0` for `nan` and for both infinities.
That is exactly the rule, which is why the fix is to give the other three the same final step.

Found while writing [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s
`Float` rules (`SPEC-17`).

**Fix:** apply `| 0` to the result of each of the three, keeping their rounding direction:

```js
export function ceiling(n) { return Math.ceil(n) | 0 }
export function floor(n) { return Math.floor(n) | 0 }
export function round(n) { return Math.round(n) | 0 }
```

They stop being `const` aliases for `Math`, which is the point: the wrap is the part the
language requires and `Math` does not do. Note the three are also the reason
[`docs/spec/js-interop.md`](../spec/js-interop.md)'s facade rules matter here — a companion is
an ordinary ES module and the compiler never rewrites what it returns, so nothing but this file
can add the wrap.

`truncate` needs no change. Do not "simplify" it to `Math.trunc(n)`, which does not wrap.

**Acceptance:** `round`, `floor` and `ceiling` in `std/core/src/Js/Basics.mjs` each return a
value in the 32-bit signed range for every `Float` input, `0` for `nan` and for both infinities.
The **Known gap:** paragraph in
[`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *Converting a `Float` to
an `Int`* is deleted. `cargo run` still prints `parsed 8 modules` and lists all eight as checked.

**Note — this ticket has no red test behind it**, for the same reason
[`BUG-24`](bug-24.md) does not: the repository has no JavaScript harness and `cargo test` never
loads a `.mjs` file, so no spec block goes red when the fix lands and none goes red while it is
outstanding. Until a harness exists — the one [`BUG-20`](bug-20.md)'s acceptance also waits
on — the check is reading the four exports. `BUG-24` is the sibling defect in the same file, on
the arithmetic side rather than the conversion side; the two are independent and can land in
either order.
