# SPEC-20 · A facade constant is called unsettled by the chapter and shipped by `std/core`

**Sizing:** small. Prose in one chapter, and a decision that the tree has arguably already made.

**Location:** [`docs/spec/js-interop.md`](../spec/js-interop.md) — *Open questions*, third entry.
The behaviour is in `src/compiler/canonical/mod.rs` — the `if source.binding_javascript` branch of
`canonicalize`. The counter-examples are `std/core/src/Js/Basics.zel` — `e` and `pi` — and their
implementations in `std/core/src/Js/Basics.mjs`.

**Problem:** [JS interop](../spec/js-interop.md#open-questions) says:

> **Values, not just functions.** Only signatures are accepted today, and every worked example is
> a function. Whether a facade may declare a constant is unsettled.

Both halves of that are wrong about the tree as it stands.

Not every worked example is a function. `std/core/src/Js/Basics.zel` declares two constants and
exposes both:

```
e : Float

pi : Float
```

and `std/core/src/Js/Basics.mjs` implements them as constants on the JavaScript side too:

```
export const pi = Math.PI;
export const e = Math.E;
```

And it is not unsettled in the compiler. The facade branch of `canonicalize` requires an
annotation and no bindings, then builds a `Value::TypedValue` with `patterns: vec![]`. Nothing
there inspects the annotation's shape, so a nullary signature is accepted exactly as a function
signature is. `Js.Basics` is one of the eight modules `cargo run` compiles and it passes, so a
facade constant is a construct the standard library depends on today.

The gap is therefore not a missing feature but a chapter describing its own worked example
incorrectly — the failure mode `CLAUDE.md`'s *A doc comment describes what the code at that site
does* names, one level up. A reader who takes the open question at face value will believe the
two constants their standard library exposes are not a thing the language has.

**The question to settle** is smaller than the open question implies, because the default is
already "yes". What remains is whether that default is intended, and what it obliges:

- **Whether a facade constant is permitted at all.** Almost certainly yes, given `std/core`.
  Saying no means changing `Js/Basics` and losing `pi` and `e`, which nothing recommends.
- **What the JavaScript side must provide.** A function facade is backed by an export taking a
  plain parameter list; the constant case has no parameter list, so the rule is that the export
  is the value itself rather than a thunk. `Js/Basics.mjs` already does this and the chapter
  never says it. This is the substantive half of the ticket: the plain-parameter-list promise has
  a nullary case and it is unwritten.
- **When the constant is evaluated.** `export const pi = Math.PI` runs at module load. Whether
  the language promises that, permits laziness, or says nothing, interacts with
  [Evaluation semantics](../spec/evaluation-semantics.md) — Zelkova is strict, and a facade
  constant is the one value whose evaluation happens outside Zelkova's control. Worth a sentence
  even if the sentence is "nothing is promised".
- **Whether the type may be a bare variable.** `x : a` as a constant is meaningless in a way
  `add : a -> a -> a` is not, and whether the subset rule excludes it belongs to
  [`SPEC-18`](spec-18.md) rather than here.

**Approach:** follow `write-spec-chapter`, at the scale of a section. Then:

1. [JS interop](../spec/js-interop.md) states that a facade may declare a constant, and what its
   companion `.mjs` must export for one — the nullary case of the plain-parameter-list rule.
2. The chapter's worked example, or a second block beside it, shows a constant. `e` and `pi` are
   the ready-made ones and a block using them is `expect=ok` today, so it pins present behaviour
   rather than intent.
3. The third entry in *Open questions* is deleted.

**What this is not.** Not a change to `canonicalize`, which already does the right thing, and not
a reason to add a check: there is no bug here, only an unwritten rule. Do not resolve it by
declaring facade constants unsupported — that would be a `LANG-` against `std/core`, and the
ticket for it would have to explain what replaces `pi`.

**Acceptance:** [`docs/spec/js-interop.md`](../spec/js-interop.md) says whether a facade may
declare a constant and what its `.mjs` export must be, and no sentence in the chapter still says
every worked example is a function. The third entry in its *Open questions* is gone.
`cargo test --test spec` green with the new block tagged `expect=ok`, and `cargo run` still
reports all eight modules checked.

**Found:** while auditing `docs/spec/` for open questions with no ticket attached, on 2026-09-04.
The chapter's claim was checked against `std/core/src/Js/Basics.zel` and contradicted by it.
