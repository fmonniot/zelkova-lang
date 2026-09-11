# DEC-3 · What the spec harness checks, and what it declines to

**Settled:** 2026-09-05 (`SPEC-23`), which asked for the checks and left their scope open on
purpose.
**Status:** live. Extended by [DEC-4](dec-4.md#3--the-track-is-checked-for-links-and-not-for-examples),
which brings `docs/decisions/` under the same link check.
**Where the rule lives:** `tests/spec.rs` — `spec_cross_references_resolve` and
`spec_tag_vocabulary_is_documented` — and [`docs/tickets/README.md`](../tickets/README.md)'s
closing convention, which says what bill this lands and who pays it.

`cargo test --test spec` began as one thing: every ```` ```zel ```` block in a chapter must do
what its `expect=` tag says. `SPEC-23` added two siblings that hold the rest of the directory
to the same premise the tags hold its examples to — documentation nothing checks drifts from
what it describes, silently and indefinitely. `spec_cross_references_resolve` resolves every
inline markdown link a chapter writes; `spec_tag_vocabulary_is_documented` holds
[`conventions.md`](../spec/conventions.md#the-expect-vocabulary)'s tag table to the names the
harness really accepts.

Three scope questions came with them. None has a self-evident answer, each has a cost that
falls on somebody other than whoever wrote the check, and all three are cheap to re-open by
accident, which is why they are here.

## 1 — Links into `docs/tickets/` are checked

Along with everything else that is not an absolute URL. The chapters cite ticket files from
their **Known gap:** and **Not implemented:** paragraphs, and those citations are the spec's
own account of the distance between itself and the compiler; a citation of a file the ticket
process has since deleted is a claim about a gap that may no longer exist.

The cost is real and lands on whoever closes a cited ticket rather than on whoever wrote the
paragraph: the closing convention deletes the ticket file, so closing one a chapter cites
turns this test red until the citing paragraph is edited. That is the intended pressure — a
closed `LANG-` usually means the chapter's **Known gap:** paragraph is now false, and the
cases where the tagged block stays green across its own fix are exactly the ones nothing else
notices.

The alternative was to leave the chapters out of it and let a stale citation survive. It was
weighed and rejected on that argument.

## 2 — An anchor is checked in whatever file the link names

`docs/tickets/` files included, rather than only within `docs/spec/`. No such link existed
when this was decided; the uniform rule cost nothing then and does the right thing now that
[`docs/decisions/`](README.md) is a second directory the chapters link into.

## 3 — The tag table is read as a row, not searched name by name

`spec_tag_vocabulary_is_documented` parses the `expect=parse-error:Reason` row of
`conventions.md`'s table rather than searching the section for each name verbatim.

Parsing costs a coupling to one row's formatting — the reason names have to be backticked in
that row, and the count of specific errors spelled out as a word — and buys the reverse
direction, which a verbatim search cannot have: a name the table *invents*, or one left behind
by a rename, is caught rather than ignored. Only the forward direction was already guarded,
by `parse_error_reasons`'s explicit match, and the unguarded direction had already drifted
once — the table documented seven of the eleven specific errors while `UnrecognizedToken` was
in use at two blocks in `docs/spec/lexical-structure.md`.

## What is deliberately not checked

**The `canonical::Error` variant names.** `conventions.md` documents them by rule — "matched
against the real variant names in `src/compiler/canonical/mod.rs`'s `Error` enum" — and not by
list, so there is no prose enumeration to drift. A check would have to invent the list it then
verified.

**Anything outside a chapter's markdown.** The harness reads prose. It does not check that a
**Known gap:** paragraph is still true, only that the ticket it names still exists; a gap that
was fixed without closing its ticket stays documented as open. The `expect=` tag on the block
beside it is what covers that case, and it covers it as far as the type checker — no further,
so a claim about evaluation or about code generation has nothing behind it.
