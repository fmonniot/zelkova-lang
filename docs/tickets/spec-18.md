# SPEC-18 · "A subset of the Zelkova standard types" names no subset, and the compiler enforces none

**Sizing:** medium. Prose in one chapter, one design question that the chapter itself calls its
substance, and — depending on the answer — a `LANG-` ticket for the check nothing performs today.

**Location:** [`docs/spec/js-interop.md`](../spec/js-interop.md) — its opening paragraph, which
makes the claim, and *Open questions*, first entry. The absent enforcement is in
`src/compiler/canonical/mod.rs` — the `if source.binding_javascript` branch of `canonicalize`.

**Problem:** [JS interop](../spec/js-interop.md) opens by saying the boundary is narrow:

> The compiler also accepts only a subset of the Zelkova standard types as parameters and return
> types for these signatures; if you need more expressiveness, use a JSON data type.

and its own open question concedes that the subset is never named:

> The paragraph above says "a subset of the Zelkova standard types" without saying which, and
> that is the substance of this chapter rather than a detail of it. The rule wanted is one a
> runtime check can enforce; naming the subset needs the types chapter first.

The types chapter has since been written ([`docs/spec/types.md`](../spec/types.md), `SPEC-5`), so
the stated blocker is gone.

**Known gap:** the compiler enforces nothing. The facade branch of `canonicalize` checks three
things — no infix declarations, no type declarations, and every value carries an annotation and
no bindings — and then resolves the annotation with `Type::from_parser_type`, which accepts any
type that names something in scope. There is no subset. Any type a normal module may write, a
facade may write.

That is not a hypothetical. [`BUG-20`](bug-20.md) is the worked consequence: `Js.Utils`'s
comparison and append facades declare types their JavaScript cannot honour, and that ticket says
in as many words that there is no facade-level rule to appeal to. A boundary whose narrowness is
its whole justification currently has none.

**The question to settle:** which types may cross, and on what principle. The chapter states the
test it wants — "only things verifiable by the runtime are let through" — which is a stronger
constraint than it first looks, and it decides most of the cases:

- **Primitives.** `Int`, `Float`, `Bool`, `String` (once it exists), `Char`. A runtime check
  distinguishes these, modulo `Int` and `Float` both being JavaScript numbers, which is itself a
  case to rule on.
- **A bare type variable.** `add : a -> a -> a` is the chapter's own worked example and it
  passes nothing verifiable to the runtime at all. Either the subset admits variables — in which
  case "verifiable by the runtime" is not the rule — or `std/core`'s existing facades violate it,
  which is [`BUG-20`](bug-20.md) generalised. This is the hard case and it should be settled
  first, because the answer decides whether the rule is about types or about *what the JavaScript
  is allowed to do with* a value it cannot inspect.
- **Structured types** — tuples, union types declared in Zelkova, lists, records. Each is
  representable, but a facade receiving one has to know its runtime representation, which is
  exactly what [`docs/spec/js-interop.md`](../spec/js-interop.md) promises a hand-written `.mjs`
  never has to know. The chapter's own escape hatch is "use a JSON data type", which suggests the
  answer is no — but no JSON type exists, so saying no today leaves the escape hatch unbuilt.
- **Function types as arguments.** A callback into Zelkova from JavaScript needs currying
  knowledge on the JavaScript side, which the plain-parameter-list promise forbids. Likely
  excluded, and worth saying so rather than leaving it to be discovered.

This ticket does not pick. It does observe that the bare-type-variable case and the JSON escape
hatch are entangled: a strict subset with no JSON type to fall back on makes several `std/core`
facades unwritable.

**Approach:** follow `write-spec-chapter`, at the scale of a section. Settle the question with
the language owner first. Then:

1. [JS interop](../spec/js-interop.md) gains a section naming the subset and the principle behind
   it, with blocks tagged for what the compiler does today — which is accept everything, so a
   block showing a rejected type is `expect=ok` and goes red when the check lands.
2. The opening paragraph's "a subset of the Zelkova standard types" links to that section instead
   of gesturing at it.
3. The first entry in *Open questions* is deleted.
4. A `LANG-` ticket is filed for the check itself, against the facade branch of `canonicalize`,
   with a new `canonical::Error` variant carrying the offending annotation's span. Filed, not
   implemented: a spec change and a semantics change do not share a diff.

**What this is not.** Not a fix for [`BUG-20`](bug-20.md), which is about two specific facades
lying about what their JavaScript does and has a runtime-check approach available to it now. This
ticket is the general rule that would have prevented it, and the two are separately shippable.
Do not settle it by declaring the current behaviour correct and deleting the claim from the
chapter's opening paragraph — the narrow boundary is the design, and widening it to whatever the
code happens to allow would be deciding the question by not answering it.

**Acceptance:** [`docs/spec/js-interop.md`](../spec/js-interop.md) names which types may appear
in a facade signature and why, including a ruling on bare type variables. The first entry in its
*Open questions* is gone, and the opening paragraph links to the rule. A `LANG-` ticket exists
for the enforcement. `cargo test --test spec` green, with the new blocks tagged for present
behaviour and each one proven to fail when its tag is flipped.

**Sequencing:** [`SPEC-15`](spec-15.md) is nearby — whatever an effect value turns out to be has
to be something this boundary permits, and settling the subset first is cheaper than discovering
it excludes the answer. [`CLASS-2`](class-2.md) is unaffected: a facade may not carry a constraint
at all, which the chapter already settles.

**Found:** while auditing `docs/spec/` for open questions with no ticket attached, on 2026-09-04.
