# SPEC-19 · `javascript` is the only interop modifier, and the WebAssembly equivalent is undesigned

**Sizing:** medium, and it may reasonably close as "not yet" — see *What this is not*. The design
is small if the answer is a second modifier and large if it is a different mechanism.

**Location:** [`docs/spec/js-interop.md`](../spec/js-interop.md) — *Open questions*, second entry.
The modifier itself is `src/compiler/parser/grammar.lalrpop`'s module production,
`<js: "javascript"?>`, and `parser::Module`'s `binding_javascript: bool`.

**Problem:** [JS interop](../spec/js-interop.md#open-questions) says:

> `javascript` is the only interop modifier the language defines. What the equivalent declaration
> for a WebAssembly-backed module would look like — a second modifier, a different mechanism
> entirely — is undesigned.

WebAssembly is not incidental to this project. `CLAUDE.md` states that it is the eventual target,
with JavaScript as an intermediate one because it is easier to integrate with. So the language
already anticipates a backend for which `module javascript` is the wrong facade, and the shape
that facade takes is unwritten.

The cost of leaving it unwritten is not that a Wasm facade cannot be written today — nothing can
be generated for any backend, [`GEN-1`](gen-1.md) being unstarted — but that the *existing*
mechanism is shaped by an assumption nobody has tested. `binding_javascript` is a `bool` on both
ASTs, and every check that consults it is written as "is this a facade or a normal module"
rather than "which kind of facade is this". A second interop kind turns that `bool` into an enum
and touches the parser, both ASTs and the canonicalizer — a bigger change the longer the `bool`
stands, and one whose size nobody has priced because the question has never been asked as a
ticket.

**The question to settle:** whether a WebAssembly-backed module is a second modifier or a
different mechanism, and whether the answer is needed before [`GEN-1`](gen-1.md).

- **A second modifier** — `module webassembly Wasm.Foo exposing (…)`, parallel to `javascript` in
  every respect: annotations only, no bodies, no infixes, no types, a companion file of the same
  base name. Cheapest, and it reuses the whole facade design including
  [`SPEC-18`](spec-18.md)'s eventual subset rule. Against it: the two backends' boundaries are
  genuinely different — Wasm has no dynamic typing to verify against, and its numeric types are
  narrower than JavaScript's single number — so "parallel in every respect" may be a claim the
  types cannot support.
- **One modifier, parameterised** — `module foreign "js" …` or similar, with the target named
  rather than baked into a keyword. Keeps one production for any number of backends and makes
  `javascript` a value rather than a token, which is a breaking change to a keyword the tokenizer
  already reserves.
- **A different mechanism entirely** — the facade is a JavaScript one and a Wasm backend reaches
  its host through whatever JavaScript embeds it, so there is nothing new to design. This is the
  answer that costs nothing, and it is genuinely possible; it is also the one most likely to be
  chosen by default rather than on purpose, which is why it should be chosen on purpose.

This ticket does not pick. It notes that the third is a real answer and not an evasion, and that
if it wins the outcome is a paragraph in the chapter saying so, plus the deletion of the open
question — which is a better state than the current one either way.

**Approach:** follow `write-spec-chapter`, at the scale of a section. Settle the question with the
language owner. Then:

1. [JS interop](../spec/js-interop.md) says what a WebAssembly-backed module is — a second
   modifier with its own section, or a sentence explaining why there is nothing to design — and
   the chapter's title and opening are revisited if it is no longer only about JavaScript.
2. The second entry in *Open questions* is deleted.
3. If the answer is a second interop kind, a `LANG-` ticket is filed for turning
   `binding_javascript: bool` into an enum across `parser::Module`, `canonical::Module` and the
   facade branch of `canonicalize`. Filed, not implemented.

**What this is not.** Not a WebAssembly backend, and not a decision about how Zelkova compiles to
Wasm — that is [`GEN-1`](gen-1.md)'s successor and nothing here should constrain it beyond the
facade's shape. It is also entirely legitimate for this ticket to close by recording that the
question is deliberately deferred until a Wasm backend is real, *provided the chapter says so*:
an open question that has been considered and postponed with a reason is a different artefact
from one nobody has looked at, and only the first is honest. Do not, however, close it by
deleting the open question with no replacement text.

**Acceptance:** [`docs/spec/js-interop.md`](../spec/js-interop.md) says what interop with a
WebAssembly backend looks like, or says why the question is postponed and what would reopen it.
The second entry in its *Open questions* is gone. `cargo test --test spec` green; a new block, if
any, is tagged `expect=unimplemented` and proven to fail.

**Sequencing:** no dependency on [`GEN-1`](gen-1.md) in either direction, but the `bool`-to-enum
cost above only grows, and [`SPEC-18`](spec-18.md) should be settled first if the answer is a
second modifier — a subset rule written for one boundary and then retrofitted to two is the
expensive order.

**Found:** while auditing `docs/spec/` for open questions with no ticket attached, on 2026-09-04.
