# Zelkova — Language specification

This is the specification of Zelkova the language, as distinct from `docs/tickets/`
(the compiler's own work log) and `cargo doc` (the compiler's Rust API). One markdown
file per chapter, sibling files in this directory.

**The spec is normative.** It describes Zelkova as designed, including constructs the
compiler does not implement yet — it is not a description of today's binary. Where a
chapter documents something unimplemented, it says so and tags its examples
accordingly (see the vocabulary below). `CLAUDE.md`'s *Language notes* section has the
compiler's current implemented/not-implemented split; this directory is where that
split gets explained rather than just listed.

## Every example is checked

Every fenced ```` ```zel ```` block in a chapter carries an `expect=` tag in its info
string, and `cargo test --test spec` runs every one of them through the compiler. A
chapter claim the compiler disagrees with is a red test, not a stale sentence nobody
notices — which is the failure mode this directory exists to prevent (see
[`docs/tickets/spec-1.md`](../tickets/spec-1.md) for the history: the document this
replaced, `lang.md`, drifted from the compiler within one rename and nothing caught
it).

A `zel` block with no `expect=` tag, or with a tag the harness does not recognise, is
a hard test failure — never a silent skip. When you add an example, tag it.

### The `expect=` vocabulary

| Tag | Meaning |
|---|---|
| `expect=ok` | Parses and canonicalizes with no errors. |
| `expect=parse-error` | Fails somewhere in the parser (tokenizer, layout or grammar). Which specific error is not pinned. |
| `expect=canonical-error:VariantName` | Parses, then canonicalization returns a `Vec<canonical::Error>` containing at least one error of variant `VariantName` — matched against the real variant names in `src/compiler/canonical/mod.rs`'s `Error` enum. |
| `expect=unimplemented` | Must fail somewhere in parse-or-canonicalize, but deliberately does not pin *which* error: pinning would wire tokenizer/grammar internals into a prose document, and the tag's whole job is to go red the day the feature is actually implemented. On an expected failure the test run prints the error it observed, so a reviewer can eyeball that the block failed for the reason the chapter intends. |
| `expect=fragment` | An illustrative fragment, deliberately not executed. The only opt-out, and it must be written explicitly — there is no implicit skip. |

A fenced block whose info string's first token is not `zel` (` ```sh `, a bare
` ``` `, prose) is not touched by the harness at all.

Only a single module per block is supported today; a multi-module example (imports
across two chapters' worth of source) is a decision `SPEC-1` deliberately left open —
see that ticket's *Open, this ticket does not pick* section before adding one.

## Chapters

| Chapter | Status |
|---|---|
| [Layout (the offside rule)](layout.md) | written |
| [JS interop](js-interop.md) | written — migrated from the former `lang.md` |

Further chapters (multi-line function declarations with pattern matching, lexical
structure and the soft keywords, tuples' fixed arity, constrained type variables) are
each their own ticket; `docs/tickets/spec-1.md`'s *Follow-up chapters* section has the
list and the reasoning for the order.

Two rules the Layout chapter states are enforced by the compiler today but reported
badly — a `case` branch indented deeper than its siblings
([ERR-11](../tickets/err-11.md)) and leading indentation before `module`
([ERR-12](../tickets/err-12.md)). Both chapters' examples are tagged
`expect=parse-error`, which pins the rule rather than the message, so they stay green
across those fixes. That is the general pattern: tag the claim the chapter is making,
not the diagnostic the compiler currently happens to emit.
