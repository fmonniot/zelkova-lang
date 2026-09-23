# GEN-15 · The WebAssembly backend

**Sizing:** large, and **unscheduled**. Filed now so the constraints it places on the IR are on
record before the IR is written, not rediscovered afterwards. It is not part of
[`GEN-1`](gen-1.md)'s program and nothing in that program waits on it.

**Depends on:** all of [`GEN-1`](gen-1.md) — there is no backend to be the second of until the
first exists.

**Location:** a sibling of the JavaScript backend, `src/compiler/javascript.rs`, reading the same
`src/compiler/ir/`.

**Decided ([`docs/spec/interop.md`](../spec/interop.md)):** Zelkova compiles to more than one
target and [a facade is written once for all of them](../spec/interop.md#a-facade-names-a-boundary-not-a-backend):
what varies is the companion, one per target, and a build reads the companion for the target it
is building. So this backend adds no syntax, no rule and no signature — a module written above a
facade is compiled for every target unchanged.

The companion is [a component](../spec/interop.md#the-webassembly-companion) whose exported
interface is written in WIT, taking a plain parameter list, each export declared with the WIT
spelling of the Zelkova type in its signature. Unlike JavaScript's per-call predicate, **the
component's declared interface is checked against the facade once, when the component is
loaded, and nothing is checked per call**
([Which types may cross](../spec/interop.md#which-types-may-cross-the-boundary)) — the same
table gives every admitted type's WIT spelling, and
[a union is a `variant`](../spec/interop.md#a-union-crosses-as-a-tagged-value) with one case per
constructor, carrying its arguments as a payload.

**Problem:** nothing targets WebAssembly, which is the eventual target the project is for.

**What the IR already owes this backend**, and why `src/compiler/ir/` is shaped as it is:

- **Types on every node.** WebAssembly is statically typed. A representation class — i64, f64, a
  reference, and for a reference which layout — is read off a node's type, and there is nothing
  to read it off if the IR does not carry one. This is the decision
  [`GEN-1`](gen-1.md) settled first and the one that would have been impossible to add later.
- **Monomorphisation.** `identity : a -> a` has no single WebAssembly type. Either every value
  is boxed uniformly, or the program is specialised per instantiation — and specialisation is
  already what the language requires for
  [erasing class dictionaries](../decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed),
  so it is one pass serving two purposes rather than a cost this backend invents.
  [`LANG-40`](lang-40.md) is where the class half first needs it.
- **A constructor's index in its declaration**, not only its name, because a `variant` case is
  positional where the JavaScript `$` field is nominal.
- **Explicit arity and saturation**, which the plain parameter list needs on this target for the
  same reason it needs it on the other.

**Approach:** not decided. What has to be settled when this is picked up, none of which is a
question the JavaScript backend answered: whether values live in linear memory or in WasmGC
structs; how a function value — a partial application carrying its arguments — is represented
when there is no closure primitive; how the tail-call rewrite [`GEN-11`](gen-11.md) does with a
`while` loop is expressed; how a component is loaded and its interface checked against the
facade; and whether monomorphisation is whole-program, which decides what a separately-compiled
package can even be.

Each of those is plausibly its own ticket. This one is the placeholder for the decision session
that breaks them out, in the shape [`GEN-1`](gen-1.md) was broken out.

**Acceptance:** not written — a ticket sized this way is replaced by its children rather than
completed. What makes it closable is a `GEN-` program for this target with the questions above
answered, the way [`GEN-1`](gen-1.md) answered the JavaScript ones.
