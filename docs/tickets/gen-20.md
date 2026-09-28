# GEN-20 · Emit `()`

**Sizing:** small. The IR already carries `()` and the decision tree already lowers its pattern,
so what is left is emitting the value, lifting the backend's refusal, and publishing the value's
JavaScript representation, which is now decided (below).

**Part of:** [Active work: effects](README.md#active-work-effects). This is the sibling
[`GEN-1` decision 7](gen-1.md) asks for: [`LANG-72`](README.md) made `()` parse and check, and
this ticket emits it.

**Depends on:** [`LANG-72`](README.md), now closed.

**Location:** `src/compiler/javascript.rs` — the expression emitter, the `unit_pattern` refusal at
the top of `Emitter::case_expression`, and `RESERVED`; `src/compiler/ir/` — `TypedTermKind::Unit`
and `TermPatternKind::Unit`, which `decision_tree` already lowers to no test and no binding;
`docs/spec/interop.md` — the `()` row of the admitted-type table. Until this lands,
`javascript::emit` refuses both with `Construct::Unit`.

**Problem:** `()` reaches the backend and `javascript::emit` refuses it. The
effects path needs it emitted in two places. `main`'s `Task ()` is completed with it. And every
[test facade](../spec/interop.md#testing-a-companion) `Task (Result Failure ())` is completed with
it, by a companion that does not know Zelkova exists.

That second use is why the representation is observable.
[The admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary) says `()`'s
JavaScript predicate decides "the value is the one value that type has", and never says what
that value is. A companion returning from a check has to produce it. A test companion as
[*Testing a companion*](../spec/interop.md#testing-a-companion) writes it
(`export function idivTruncates() { assert.equal(…); }`) returns `undefined`.

**Decided (2026-09-28, by the language owner):** `()` is **`undefined`** in JavaScript, and a
`()` in **result position is discarded, not checked**. In full:

- **Every `()` Zelkova code produces is `undefined`.** It is the only value of that type any
  Zelkova code ever sees, so equality (`Js/Utils.eq`'s `x === y`) and any future printer read one
  value.
- **Result position — no result.** A facade whose result is `()`, whether `unsafe f : X -> ()` or
  the payload of `Task (Result Failure ())`, ignores whatever its companion returns and yields
  `undefined`. The value is *replaced*, not passed through: letting a companion's `42` into Zelkova
  typed as `()` would make `() == ()` answer `False`. This is the JavaScript reading of the WIT
  column's "no result", so both targets say the same thing about the same row.
- **Parameter position — `undefined` in its slot.** A companion is handed `undefined` where the
  signature has a `()` parameter. The slot is kept rather than dropped, so the
  [plain parameter list](../spec/interop.md#the-javascript-companion) a companion is
  promised keeps its positions. An omitted trailing argument reads as `undefined` in JavaScript,
  so a companion may leave a trailing `()` parameter undeclared. This is where the two targets
  differ — WIT has no parameter at all — and the row says so.
- **Nested — strict.** A `()` inside a tuple, record, list or union argument that a companion
  returns must be `undefined` (`v === undefined`). Replacing it there would mean copying the
  companion's structure, and a nested `()` in a foreign type is rare. A record field of type `()`
  must be present, as `{ f: undefined }`: the record predicate checks for exactly the record's
  fields.

What it was chosen over, for the decision entry this ticket owes on close:

- **Strict `undefined` everywhere.** Right for a test companion, wrong for an effectful one that
  ends on a call with a return value it does not mean: `(a, x) => a.push(x)` returns a number,
  `map.set` the map, `appendChild` the node. Each would be `Err (Malformed ..)`.
- **`null`.** JSON keeps it, but every void and every `async` companion would need an explicit
  `return null`, and the chapter's own companion example would be wrong as written.
- **A frozen constant exported from the runtime** (what the ticket once offered, after
  [`DEC-18` decision 4](../decisions/dec-18.md#4--a-constructor-of-no-arguments-is-hoisted-to-one-module-level-constant)).
  Unambiguous, but every companion imports the Zelkova runtime, and the predicate becomes an
  identity check that fails when two copies of the runtime are loaded (a bundler's duplicate, the
  dual-package hazard) and across realms and workers, where structured clone makes a new object.
- **The 0-tuple, `[]`.** Consistent with tuples crossing as arrays, but it allocates, or needs a
  shared constant with the previous option's identity problem.
- **Ecosystem.** `undefined` is what Gleam's `Nil`, ReScript's `unit`, PureScript's `unit`,
  TypeScript's `void` and wasm-bindgen's `()` all are in JavaScript, and what a `Promise<void>`
  resolves to. Elm uses `0` (or `{ $: '#0' }` in debug) but never hands it to foreign code.

**Approach:**

1. **Publish it.** The `()` row's predicate cell says the value is `undefined`, that a `()` result
   is discarded and read as `()`, and that a `()` parameter keeps its slot where WIT has none.
   That is a spec change and lands in its own commit ahead of the emitter, per
   [the conventions](../spec/conventions.md#a-spec-change-and-a-semantics-change-do-not-share-a-diff).
2. **Add `undefined` to `RESERVED`** in `src/compiler/javascript.rs`. It is not an ECMAScript
   reserved word but a global a local binding can shadow, and `undefined` is a legal Zelkova
   name: without this, a binding called `undefined` changes what every `()` in its scope emits
   as. It then mangles to `$undefined` like any other reserved name, and `RESERVED`'s doc
   comment says why a non-keyword is on the list.
3. Emit the expression as `undefined`, and delete the `unit_pattern` refusal in
   `Emitter::case_expression` (and `unit_pattern` with it). Nothing else is owed to the pattern:
   `decision_tree` already lowers a `()` to no test and no binding, at any depth.

Enforcing the boundary half — discarding a `()` result, and the nested `=== undefined` check — is
not this ticket's: it lands with the call-site wrapper and the predicates, [`GEN-16`](gen-16.md)
and [`GEN-2`](gen-2.md), which read the row this ticket publishes.

**Tests:** `tests/javascript.rs` for the emitted text of `x = ()`, of `always () = On`, and of a
binding named `undefined` in scope of a `()`. A Zelkova test in `std/core/tests/` that goes
through both the expression and the pattern. That needs nothing from effects:
`Test.equal (always ()) On` is enough.

**Acceptance:** a module using `()` as an expression and as a pattern emits, and `zelkova test
std/core` runs a test that goes through both and passes. The `()` row of the interop table says
what the value is and how each position reads it. On close, the alternatives above are promoted
to a decision entry.
