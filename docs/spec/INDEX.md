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
notices — which is the failure mode this directory exists to prevent. The document this
replaced, a root-level `lang.md`, described a facade module as `Basics.Js` while the tree
had spelled it `Js.Basics` since March 2021. That contradiction sat there for five years,
because nothing read `lang.md` and nothing checked it. `SPEC-1` (closed, recoverable per
[`docs/tickets/INDEX.md`](../tickets/INDEX.md)) has the rest of that history.

A `zel` block with no `expect=` tag, or with a tag the harness does not recognise, is
a hard test failure — never a silent skip. When you add an example, tag it.

### The `expect=` vocabulary

| Tag | Meaning |
|---|---|
| `expect=ok` | Parses and canonicalizes with no errors. |
| `expect=parse-error` | Fails somewhere in the parser (tokenizer, layout or grammar). Which specific error is not pinned. Use when the chapter claims only that the source is rejected. |
| `expect=parse-error:Reason` | The same, and the reason must match. `Reason` is either the phase (`Tokenizer`, `Layout`) or a specific error (`IndentationError`, `TabError`, `LayoutError`, `UnexpectedToken`, `UnexpectedEOF`, `InvalidToken`, `ExtraToken`) — matched against the real enums in `src/compiler/parser/`. Use whenever the chapter's prose describes the error the reader will see. |
| `expect=canonical-error:VariantName` | Parses, then canonicalization returns a `Vec<canonical::Error>` containing at least one error of variant `VariantName` — matched against the real variant names in `src/compiler/canonical/mod.rs`'s `Error` enum. |
| `expect=unimplemented` | Must fail somewhere in parse-or-canonicalize, but deliberately does not pin *which* error: pinning would wire tokenizer/grammar internals into a prose document, and the tag's whole job is to go red the day the feature is actually implemented. On an expected failure the test run prints the error it observed, so a reviewer can eyeball that the block failed for the reason the chapter intends. |
| `expect=fragment` | An illustrative fragment, deliberately not executed. The only opt-out, and it must be written explicitly — there is no implicit skip. |

A fenced block whose info string's first token is not `zel` (` ```sh `, a bare
` ``` `, prose) is not touched by the harness at all.

### One module per block — undecided beyond that

A block holds a single module. The chapters on modules, imports and cross-module errors
will need two or three at once, and how to express that is **open**:
`canonicalize_standalone` takes one source string, so the options are adjacent fenced
blocks sharing one expectation; a block that names the module it defines and accumulates
into an implicit package; `canonicalize_with_interfaces` with a hand-built `Interface`; or
writing the example to a temp directory and calling `compile_package` the way
`tests/pipeline.rs`'s `fixture_package` drives `tests/fixtures/`.

Whichever chapter needs it first decides. `tests/spec.rs`'s dispatch was deliberately
written so adding this is not a rewrite; don't reshape it in a way that forecloses them.

## Chapters

| Chapter | Status |
|---|---|
| [Layout (the offside rule)](layout.md) | written |
| [JS interop](js-interop.md) | written — migrated from the former `lang.md` |
| Multi-line function declarations with pattern matching | planned |
| Lexical structure, including the soft keywords | planned |
| Tuples' fixed arity | planned |
| Constrained type variables | planned — records an open question, does not settle it |

The planned chapters are in rough priority order, and the order is not arbitrary: these
are the places Zelkova **cannot defer to Elm's documentation**, either because it diverges
or because Elm never wrote the rule down. Each is its own ticket, filed when it is picked
up rather than in bulk. What each has to cover:

- **Multi-line function declarations with pattern matching** — a deliberate divergence
  from Elm, so Elm's documentation is actively wrong here and there is nothing to defer to.
- **Lexical structure, including the soft keywords** — `left`, `right`, `non` and
  `javascript` each act as a keyword in one position and as an ordinary identifier in
  others, and the four do not behave alike today: `javascript` is reserved in identifier
  position and the other three are not. Whether that split is intended has to be settled
  before the chapter can state a rule.
- **Tuples' fixed arity** — Zelkova has 2-tuples and 3-tuples and no other size. A
  four-element tuple is a syntax error rather than a type error, which is a fact about the
  grammar of the language and belongs in the spec.
- **Constrained type variables** (`number`, `comparable`, `appendable`) — whether these
  become real type classes, compiler-known constraints, or nothing at all is undecided.
  The chapter records the question. A spec is a good place to hold an open design question
  and a bad place to settle one by accident.

### Tag every claim the chapter makes

A chapter that describes a known-bad diagnostic is making **two** claims, and they have
different lifetimes. The rule — *this source is rejected* — is permanent. The sentence
describing today's error is temporary, and it becomes false the moment someone improves
the diagnostic.

The Layout chapter has two of these: a `case` branch indented deeper than its siblings
([ERR-11](../tickets/err-11.md)) and leading indentation before `module`
([ERR-12](../tickets/err-12.md)). Rejection is the intended behaviour in both cases *and*
what happens today, so a bare `expect=parse-error` stays green straight through those
fixes — and the prose explaining that the grammar "trips on the second `->`" would quietly
become a lie.

So pin the reason: those blocks are tagged `expect=parse-error:UnexpectedToken`, naming
the wrong-but-current error deliberately. When ERR-11 lands and the error becomes a proper
`LayoutError`, that block goes **red**, and whoever fixed the diagnostic has to update the
paragraph describing it in the same change.

Two consequences, both deliberate. **First**, this reads as being in tension with *A spec
change and a semantics change do not share a diff*, immediately below — pinning the reason
guarantees that whoever fixes ERR-11 edits `docs/spec/layout.md` in the same PR to get
green. That is the wanted outcome and not the shape that rule is aimed at. What it forces
is a small, prose-only edit, written by the one person who has just read the code the
paragraph describes; what the rule forbids is deciding what the language *is* inside the
diff that changes what the compiler *does*. A red block is the mechanism that makes the
first happen; the rule is what stops it becoming the second.

**Second**, the guarantee is narrower than "the stale sentence cannot survive". The pin is
on the error *variant* only, so an ERR-11 fix that produced a different but still
`UnexpectedToken` error would leave the block green with the stale paragraph intact.
Pinning the token as well is possible and is deliberately not done: wiring that much
grammar detail into a prose document costs more than the residual risk is worth.

The general form: **tag every claim you make, at the granularity you make it.** Claim only
rejection, and use the bare tag. Describe the diagnostic, and pin it. There is no manual
verification step here on purpose — a convention that depends on someone remembering to
check something by hand is one that will be skipped, and a spec whose examples are checked
by ritual is the same document `lang.md` was.

### A spec change and a semantics change do not share a diff

Writing a chapter surfaces compiler behaviour nobody intended — that is much of the value.
When it does, file a ticket and specify the behaviour the language *should* have, tagging
the example for what the compiler does today. ERR-11 and ERR-12 were both found this way.
Fixing the compiler in the same diff that documents it makes the change unreviewable, and
it is unnecessary: a spec claim the compiler fails is a red test, which is a working
record rather than a lost one.
