# DEC-21 · The unit value crosses as `undefined`

**Settled:** 2026-09-28, by the language owner.
**Status:** live.
**Where the rule lives:**
[Foreign interoperability — The unit value crosses as `undefined`](../spec/interop.md#the-unit-value-crosses-as-undefined),
and [the `()` row of the admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary).

[`LANG-72`](../tickets/README.md) made `()` a type, an expression and a pattern the front end
accepts; [`GEN-20`](../tickets/README.md) is what emits it. Neither settles what the value *is*
on JavaScript — [Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)
had said a `()` predicate decides "the value is the one value that type has" and never said what
that value was, because nothing needed one until a companion had to produce it: a
[test facade](../spec/interop.md#testing-a-companion) checks by side effect and returns nothing,
so `Task (Result Failure ())`'s payload is exactly what a `node:assert` function hands back —
`undefined`.

## 1 — `()` is `undefined`; a result is discarded and a parameter keeps its slot

**Every `()` Zelkova code produces is `undefined`.** It is the only value of that type any
Zelkova code ever sees, so equality (`Js/Utils.eq`'s `x === y`) and any future printer read one
value rather than choosing among several representations that would all have to compare equal.

**A result of `()` is discarded, not checked.** A facade whose result is `()` — `unsafe f : X ->
()`, or the payload of `Task (Result Failure ())` — ignores whatever its companion returns and
yields `undefined`. The value is *replaced*, not passed through: a predicate that let a
companion's `42` stand in for `()` would make `() == ()` answer `False` the day two call sites of
the same facade disagreed about what to return. Discarding is also cheaper to check: a facade
result needs no predicate at all where every other admitted type needs one.

**A parameter of `()` keeps its slot.** A companion is handed `undefined` where the signature has
a `()` parameter, rather than that argument being dropped from the call. This is what keeps [the
plain-parameter-list promise](../spec/interop.md#the-javascript-companion) — a companion's
positions match the signature's — true without a special case for `()`; dropping the slot would
make a two-argument Zelkova signature call a one-argument JavaScript function whenever its last
parameter happened to be `()`, which a companion author would have no way to predict from the
signature alone. It is also the one place JavaScript and WIT disagree: WIT has no parameter to
spell there at all, since a component's interface just omits it. An omitted trailing argument
reads as `undefined` in JavaScript, so a companion may leave a trailing `()` parameter
undeclared — the slot is kept in the call, not necessarily in the companion's own signature.

**Nested, the check stays strict.** A `()` inside a tuple, a record, a list or a union argument a
companion returns must be `undefined` (`v === undefined`), checked like any other field or
element. Replacing a nested `()` the way a result position does would mean walking into the
companion's own structure and rewriting part of what it returned, which is a cost the result-only
case does not have; and a nested `()` in a foreign type is rare enough that paying for the
general mechanism everywhere else buys little. A record field of type `()` must still be
present, as `{ f: undefined }`: the record predicate checks for exactly the record's fields, and
an absent field is a different shape from a present one holding `undefined`.

### What it was chosen over

**Strict `undefined` everywhere**, result position included — the uniform rule, and the first one
tried. It is right for a test facade, whose companion means to return nothing at all, and wrong
for an ordinary effectful one that happens to end on a call with a return value it does not mean:
`(a, x) => a.push(x)` returns the array's new length, `map.set(k, v)` returns the map, and
`appendChild` returns the node appended. Each of those is a perfectly correct companion for a
`Task (Result Failure ())` facade, and strict checking would make every one of them
`Err (Malformed ..)`.

**`null`.** JSON keeps a `null`, unlike `undefined`, which does not survive a round trip through
`JSON.stringify`. But `null` is not what an ordinary void computation reaches for: every `async`
companion and every function with no explicit `return` already answers with `undefined`, and
forcing `null` would mean every one of those needs an explicit `return null` it would otherwise
have no reason to write — including the chapter's own companion example
(`assert.equal(idiv(7, 2), 3);`), which would be wrong as written under this alternative.

**A frozen constant exported from the runtime** — the shape [`DEC-18` decision
4](dec-18.md#4--a-constructor-of-no-arguments-is-hoisted-to-one-module-level-constant) uses for a
nullary constructor, and what this ticket offered before the decision above. Unambiguous inside
one loaded copy of the runtime, and wrong everywhere the boundary actually has to work: every
companion would need to import the Zelkova runtime just to produce or compare `()`, the
predicate becomes an identity check that a bundler's duplicate copy or the dual-package hazard
can defeat, and structured clone — the mechanism that moves a value across a worker or a realm —
makes a new object on the other side, so the same constant would stop being the same value the
moment it crossed a boundary the runtime constant was supposed to make trivial.

**The 0-tuple, `[]`.** Consistent with how a tuple already crosses — as an array — but it either
allocates a fresh array on every crossing, or needs a shared constant to avoid that, which is the
previous alternative's identity problem again, one level down.

**Ecosystem precedent**, weighed rather than argued from: `undefined` is what Gleam's `Nil`,
ReScript's `unit`, PureScript's `unit`, TypeScript's `void` and wasm-bindgen's `()` all compile
to in JavaScript, and what a `Promise<void>` resolves to — which is exactly the shape a `Task
(Result Failure ())` unwraps into. Elm uses `0` (or `{ $: '#0' }` in a debug build) but never
hands it to foreign code, so it answers a question Elm never had to ask.

[`GEN-16`](../tickets/gen-16.md) and [`GEN-2`](../tickets/gen-2.md) are what make the discard
and the nested check real code: this entry settles what they build toward, not the wrapper or
the predicate emitter themselves.
