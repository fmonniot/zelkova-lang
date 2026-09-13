# SPEC-30 · `unsafe` outside a facade is rejected, and no chapter says so

**Sizing:** small — one paragraph and one tagged example in an existing chapter; no compiler
change.

**Location:** [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s *Reserved
words* section, which is where `unsafe`'s soft-keyword row already lives.
`src/compiler/canonical/mod.rs` — `Error::UnsafeOutsideFacade`, its `message()`, `labels()` and
`notes()` arms, and the `canonicalize` call site (around the `.filter(|f| f.marked_unsafe)` at
line 1418) that constructs it for a `module` (non-`foreign`) declaration marked `unsafe`.

**Problem:** `lexical-structure.md`'s soft-keyword table describes `unsafe` as "a keyword before
a signature in an [`unsafe` facade]" and an ordinary identifier everywhere else — which reads as
a promise that `unsafe f : Int` in an ordinary module is the *name* reading, `unsafe` being bound
as an identifier and the line being read as two names where one is expected (so a parse error,
the shape every other soft keyword's off-position use gets: `left`, `right`, `non` and `foreign`
are all plain names outside their one keyword position, with no bespoke canonicalization error
for using them elsewhere).

The implementation does something more deliberate and better: `unsafe` is parsed as the modifier
reading in **every** module (`VarIdent`'s "unsafe" arm exists for name position, but `FunType`
takes the token as a modifier unconditionally), and `canonicalize` then rejects it with
`Error::UnsafeOutsideFacade` when the enclosing module is not `module foreign`. This is a
user-visible rejection — `unsafe f : Int` in an ordinary module is a canonicalization error, not
a parse error, and not accepted as a name — and nothing in `docs/spec/` says so:
`grep -rn "UnsafeOutsideFacade" docs/spec/` finds nothing, and `tests/spec.rs` already lists
`UnsafeOutsideFacade` among the variants an `expect=canonical-error:` tag may name (it is
generated from the real enum), so a chapter is free to cite it — none does yet.

[`docs/spec/conventions.md`](../spec/conventions.md)'s *A spec change and a semantics change do
not share a diff* is why this was not fixed inside `LANG-53`, which implemented the rejection:
the rule belongs in a chapter, and that chapter has to be written by someone reading the
committed behaviour, in its own diff.

**Approach:** add a short paragraph to *Reserved words*, next to the existing soft-keyword table
and the `unsafe`/`derived` sentence that already explains why they need the following token
rather than a bare reservation. State the rule as the language has it: `unsafe` before a
signature outside a `module foreign` facade is rejected, distinct from every other soft keyword,
because a facade modifier only means something where there is a companion for it to be a claim
about. Add an `expect=canonical-error:UnsafeOutsideFacade` block demonstrating it — an ordinary
module with an `unsafe f : Int` signature is enough; `tests/compiler/canonical.rs`'s existing
`UnsafeOutsideFacade` test (asserting on the name `"twice"`) is the model for a minimal repro.

**Acceptance:** *Reserved words* states that `unsafe` in signature position outside a `module
foreign` facade is rejected, and why that differs from the other soft keywords' plain-name
fallback. A new `expect=canonical-error:UnsafeOutsideFacade` block demonstrates it. `cargo test
--test spec` is green.
