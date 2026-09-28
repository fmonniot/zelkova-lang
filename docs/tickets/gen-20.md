# GEN-20 · Emit `()`

**Sizing:** small. One IR node and its emission in expression and pattern position, once the value's
JavaScript representation is chosen. The choice is the part that is not small, because a companion
sees it.

**Part of:** [Active work: effects](README.md#active-work-effects). This is the sibling
[`GEN-1` decision 7](gen-1.md) asks for: [`LANG-72`](lang-72.md) makes `()` parse and check, and
this ticket emits it.

**Depends on:** [`LANG-72`](lang-72.md).

**Location:** `src/compiler/javascript.rs` — the expression emitter and the decision-tree test a
pattern becomes; `src/compiler/ir/` — whatever node `LANG-72` gives `()`; `runtime/js/zelkova.mjs`,
if the value ends up living there.

**Problem:** once `LANG-72` lands, `()` reaches the backend and `javascript::emit` refuses it. The
effects path needs it emitted in two places. `main`'s `Task ()` is completed with it. And every
[test facade](../spec/interop.md#testing-a-companion) `Task (Result Failure ())` is completed with
it, by a companion that does not know Zelkova exists.

That second use is why the representation is observable.
[The admitted-type table](../spec/interop.md#which-types-may-cross-the-boundary) says `()`'s
JavaScript predicate decides "the value is the one value that type has", and never says what
that value is. A companion returning from a check has to produce it. A test companion as
[*Testing a companion*](../spec/interop.md#testing-a-companion) writes it
(`export function idivTruncates() { assert.equal(…); }`) returns `undefined`.

**Approach:**

1. **Choose the representation.** The ticket leans to `undefined`, because it is what a JavaScript
   function that returns nothing hands back, which makes the chapter's own companion example
   correct as written. The alternatives are `null`, or a frozen constant exported from the runtime
   in the way [`DEC-18` decision 4](../decisions/dec-18.md#4--a-constructor-of-no-arguments-is-hoisted-to-one-module-level-constant)
   hoists a nullary constructor. That constant would be unambiguous, but every companion would
   have to import it. Say which was chosen.
2. **Publish it.** It is part of the boundary, so it belongs in the admitted-type table's `()` row,
   the way [a union's encoding](../spec/interop.md#a-union-crosses-as-a-tagged-value) is published.
   That sentence is a spec change and lands in its own commit ahead of the emitter, per
   [the conventions](../spec/conventions.md#a-spec-change-and-a-semantics-change-do-not-share-a-diff).
3. Emit the expression. A `()` pattern tests nothing, since the type has one value, so the
   decision tree binds nothing and branches on nothing.

**Tests:** `tests/javascript.rs` for the emitted text of `x = ()` and of `always () = On`. A
Zelkova test in `std/core/tests/` that goes through both. That needs nothing from effects:
`Test.equal (always ()) On` is enough.

**Acceptance:** a module using `()` as an expression and as a pattern emits, and `zelkova test
std/core` runs a test that goes through both and passes. The `()` row of the interop table says
what the value is.
