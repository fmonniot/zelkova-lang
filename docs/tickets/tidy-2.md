# TIDY-2 · Replace the tokenizer's keyword `HashMap` with a `match`

**Sizing:** small — one function, one field, one lookup site.

**Location:** `src/compiler/parser/tokenizer.rs` — `get_keywords()`, the `keywords:
HashMap<String, Token>` field on `Tokenizer`, its initialisation in `Tokenizer::new`, and the
lookup that consults it. The `get_keywords` doc link in the identifier-scanning documentation
(around the "reserved list of keywords" note) will need repointing.

**Problem:** `get_keywords()` allocates a `HashMap` and inserts nineteen `String` keys — fifteen
keywords plus four soft keywords (`left`, `right`, `non`, `javascript`) — every time a
`Tokenizer` is constructed. The set is fixed at compile time and small enough that a `match`
on `&str` compiles to a length switch plus a handful of comparisons, which beats hashing a
freshly allocated `String`. The map also forces the lookup site to have a `String` (or to
allocate one) to query with, where `&str` would do.

This is not a measured bottleneck and should not be justified as one. It is simpler, allocates
nothing, and moves an invariant — "these nineteen words are reserved" — from runtime data into
code the compiler can check for exhaustiveness of intent.

**Approach:**

1. Replace `get_keywords()` with `fn keyword(s: &str) -> Option<Token>` containing a single
   `match s { "module" => Some(Token::Module), … , _ => None }`. Keep the soft keywords in the
   same match, with the existing comment marking where they start — that distinction is real
   and the comment is the only place it is written down.
2. Delete the `keywords` field from `Tokenizer` and its initialisation in `new`.
3. Update the lookup site to call `keyword(...)` on a `&str`, avoiding the allocation if the
   surrounding code permits it.
4. Drop the now-unused `use std::collections::HashMap;` if nothing else in the file needs it.
5. Repoint the doc-comment reference to `get_keywords()` at the new function. A dangling
   intra-doc link is a `cargo doc` warning, and `.github/workflows/rustdoc.yml` builds docs
   with `RUSTFLAGS: -D warnings`.

**Acceptance:** `get_keywords` no longer exists, `Tokenizer` has no `keywords` field, all
nineteen words still tokenize to the same `Token` as before, and `cargo test` passes — the
thirteen existing tokenizer tests are the regression net. `cargo doc --all --no-deps` produces
no new warnings.
