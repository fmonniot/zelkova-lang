# GEN-12 · Emit an `unsafe` facade call, and place its companion

**Sizing:** medium. A facade's emitted module, the companion's placement, and a new error for a
target with no companion.

**Depends on:** `GEN-9`, closed — the emitter is `src/compiler/javascript.rs`. Sits with [`GEN-13`](gen-13.md), which decides the output
layout the companion is copied into — take them in either order, or together.

**Part of:** [`GEN-1`](gen-1.md).

**Location:** `src/compiler/javascript.rs`, whose `emit` answers `Error::Facade` for a facade
today.
`src/compiler/canonical/mod.rs` — `Module::binding_foreign`, which marks a facade, and
`Value::TypedValue`'s `marked_unsafe`, which nothing downstream reads yet.
`std/core/src/Js/Basics.zel`, `Js/Bitwise.zel` and `Js/Utils.zel` with their `.mjs` companions
are the three facades in the tree, and all three are `unsafe` throughout.

**Decided ([`docs/spec/interop.md`](../spec/interop.md)):**

- A facade's [companion](../spec/interop.md#a-facade-names-a-boundary-not-a-backend) is a file
  of the same base name beside it, one per target. A build reads the companion for the target it
  is building and ignores the others. **A facade with no companion for the target being built is
  an error, and the error names the facade and the target.**
- [The JavaScript companion](../spec/interop.md#the-javascript-companion)'s exports take a
  **plain parameter list**: a Zelkova function of two arguments is backed by a JavaScript
  function of two arguments, called directly. There is no curried-wrapper convention on the
  JavaScript side.
- [`unsafe`](../spec/interop.md#an-unsafe-facade) declares a function rather than an effect, and
  **no wrapper stands between the caller and the companion.**
- A [facade constant](../spec/interop.md#facade-constants) — a type with no arrow — is exported
  by the companion as the value itself, not as a function. The one exception is a constant whose
  type is a `Task`, which is [`GEN-16`](gen-16.md)'s and not reachable yet.

**Problem:** a facade has signatures and no bodies, so `src/compiler/javascript.rs` has nothing to emit
for one — and every value in `std/core` that actually computes something arrives through one.
`marked_unsafe` is recorded on the declaration and read nowhere.

**Approach:** emit a module for the facade like any other, so that an importer needs no special
case: it re-exports the companion's exports under the names the facade declares. Copy the
companion beside it, so the emitted `import` is the same relative shape the source layout
already implies.

A call site needs no wrapper. The signature's arrow count is the arity, and the companion takes
that many parameters, so a saturated call is a direct call and an unsaturated one goes through
[`GEN-8`](gen-8.md)'s `$curry` with the arity read off the signature — the same rule
`src/compiler/javascript.rs` applies to an ordinary declaration, which is the point of the
plain-parameter-list promise.

Add the missing-companion error, with a `message()` naming the facade and the target per
`CLAUDE.md`'s *An error has to describe itself*, and a label on the `module foreign` line.

**Two facade signatures in the tree name a type variable** — `Js.Basics.add : a -> a -> a` and
`Js.Utils.equal : a -> a -> Bool` — which
[a facade may not do](../spec/interop.md#what-a-facade-signature-may-not-name). Nothing rejects
them ([`LANG-43`](lang-43.md)) and [`BUG-20`](bug-20.md) is open on the `Js.Utils` half. This
ticket is not blocked by either: an arrow count is readable whatever the types are, and nothing
here inspects them. Do not add a workaround for it, and do not fix it here.

**Not in this ticket:** the [boundary predicate](../spec/interop.md#which-types-may-cross-the-boundary)
that checks what a companion hands back — that is [`GEN-2`](gen-2.md), which needs
[`LANG-43`](lang-43.md) first. And the wrapper an **effectful** facade's call site gets, which is
[`GEN-16`](gen-16.md) and is blocked on `Task` existing at all. Every facade in the tree is
`unsafe`, so neither is reachable today; read `marked_unsafe` and emit for the `unsafe` case,
and make the other case a visible refusal rather than silently emitting the `unsafe` shape for
it.

**Acceptance:** `cargo run` emits a module for each of the three `Js/*` facades with their
companions beside them. A test asserts the emitted text re-exports the companion's names and
that a saturated call to a two-parameter facade export is a direct two-argument call. A test
asserts a facade whose `.mjs` is absent reports the new error, naming the facade and the target,
and that the build writes no output. The behavioural half, through [`GEN-14`](gen-14.md)'s
harness, calls a facade export through the emitted module under `node` and asserts the value.
`cargo run` still prints `parsed 8 modules`, lists all eight and exits 0.
