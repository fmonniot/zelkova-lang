# GEN-2 · Emit the boundary predicate a facade signature promises

**Sizing:** medium. One predicate emitter per admitted type form, plus the call-site wiring that
runs it, plus the two destinations a failing check has. Bigger if it is taken before
[`GEN-1`](gen-1.md) has settled how a value is represented, because half of this ticket *is* that
representation read back.

**Depends on:** [`GEN-1`](gen-1.md) — there is no code generation phase to emit anything from
until it lands. Sibling rather than part of it: `GEN-1` is a whole phase and this is a bounded
piece of output that can be written, tested and reviewed on its own, against one chapter section.

**Location:** the backend module `GEN-1` creates under `src/compiler/`, at whatever it emits for a
`module javascript` facade's call site. `std/core/src/Js/*.mjs` are the companions the emitted
checks sit in front of.

**Problem:** [Foreign interoperability](../spec/interop.md#which-types-may-cross-the-boundary)
admits a type into a facade signature exactly when the compiler can emit a **predicate** for
it — a piece of
JavaScript deciding, from a value alone, whether that value belongs to that type — and says that
every value a companion `.mjs` hands back is run through the predicate of the type its signature
declares. Nothing emits one, because no code generation exists at all.

That leaves the boundary the chapter calls checked entirely unchecked. A facade's annotation has
no body behind it: the compiler never reads the JavaScript, so the annotation is a claim, and the
predicate is the whole of what turns the claim into something a running program keeps. Without
it a `.mjs` returning the wrong shape produces a Zelkova value that lies about its own type, and
every phase downstream of the boundary is entitled to believe it.

**Approach:** this ticket does not pick the details. What it has to settle:

1. **Routing a failing check to the right destination.** This is no longer open — [Which types
   may cross](../spec/interop.md#which-types-may-cross-the-boundary) settles it, and settles it
   two different ways depending on the facade. Out of an effectful facade a failing check is
   `Err (Malformed ..)`, a value the wrapper this ticket emits builds and hands to the caller; out
   of an [`unsafe`](../spec/interop.md#an-unsafe-facade) one it
   [aborts the program](../spec/evaluation-semantics.md#when-a-program-aborts), there being no
   result type to carry it. Both name the export whose companion returned the bad value, because
   the thing being reported is always a bug in a hand-written `.mjs`. Note that `unsafe` removes
   the `Task` and not the predicate, so this ticket emits a check for both kinds of facade and
   only the destination differs.
2. **Which direction is checked.** The chapter states the inbound direction — values a companion
   hands back. Whether an argument on its way *out* to JavaScript is also checked is a separate
   question: it is checked already, in the sense that the type checker proved it, so the case for
   spending time on it is weaker. Say which was chosen.
3. **The predicates themselves**, one per form the chapter admits: `Int` (a number, and a whole
   one the [32-bit range](../spec/evaluation-semantics.md#numbers) holds), `Float`, `Bool`,
   `Char`, `String`, a tuple, a record, a list, and a union type — the last reading the `$` field
   against the declaration's constructor set and checking each argument against the predicate of
   the type that constructor declares for it. The union encoding is
   [published in the chapter](../spec/interop.md#a-union-crosses-as-a-tagged-value) and this
   is the ticket that makes it true; the record and list encodings belong to
   [`SPEC-21`](spec-21.md) and [`SPEC-22`](spec-22.md) and to `GEN-1`, and this ticket inherits
   whatever they settle rather than deciding it.
4. **Recursion and cost.** A predicate for a recursive union is a recursive walk, and it
   terminates because a Zelkova value is immutable and holds no cycle. It costs the size of the
   value at each crossing, which the chapter states rather than hides; a first version should
   measure nothing and optimise nothing, and a later one may want to notice that a predicate for
   a type with no structure is a single `typeof`.

**Not in this ticket:** rejecting a signature whose type has no predicate. That is a front-end
check, it is [`LANG-43`](lang-43.md), and it lands first — this ticket may then assume every
facade signature it sees names only admitted types, which is what makes an exhaustive emitter
possible rather than one with a fallthrough case.

**Found:** filed by `SPEC-18`, which wrote the chapter section stating that the boundary is
checked. Not implemented there: a spec change and a semantics change do not share a diff
([conventions](../spec/conventions.md#a-spec-change-and-a-semantics-change-do-not-share-a-diff)),
and neither shares one with a codegen change.

**Acceptance:** a test in `tests/` compiles a module calling a facade whose companion `.mjs`
returns a value of the wrong shape — a string where the signature says `Int`, a union object
carrying a constructor name the type does not declare — runs it under `node`, and asserts the
boundary check reports it rather than the wrong value propagating. A second test asserts the
same facade called correctly returns its value unchanged, so the check is shown not to reject
what it should accept. `cargo run` still prints `parsed 8 modules` and lists all eight.
