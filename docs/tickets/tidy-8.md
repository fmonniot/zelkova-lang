# TIDY-8 · Two tokenizer comments describe the `Int` width as unsettled and cite a closed ticket

**Sizing:** small — two comments, no code change.

**Location:** `src/compiler/parser/tokenizer.rs` — the doc comment on
`TokenizerErrorType::IntegerOverflow` (around `:161`), and the inline comment in
`consume_number`'s `i64::from_str` arm (around `:986`).

**Problem:** both describe a disagreement that no longer exists, and both cite `SPEC-28`, whose
file has been deleted.

The doc comment reads:

> `i64` is not a bound `docs/spec/` states. *Integers* guarantees `-2^31 .. 2^31 - 1` on every
> target and leaves the rest to the compilation target, and *Numbers* in
> `evaluation-semantics.md` says `Int` is 32-bit everywhere; the two disagree, and `SPEC-28` is
> where that is settled and this bound then revisited.

Every clause of that is now false. The chapters agree, neither says what it is quoted as saying,
and the bound was settled rather than revisited: [`Int` is a 64-bit signed two's-complement
integer](../spec/evaluation-semantics.md#numbers), so the `i64` the token carries a literal in
*is* the language's range, exactly. The inline comment makes the narrower version of the same
claim — "that is the carrier's bound rather than the language's" — which is the one sentence that
has been inverted by the decision.

This is the pleasant half of that decision: `consume_number` rejects what an `i64` cannot hold,
[Integers](../spec/lexical-structure.md#integers) now says an integer literal outside `Int`'s
range is an error, and the two coincide. The behaviour needs no change and the comments describe
it wrongly, which is the whole of the ticket.

**Approach:** rewrite both to say that the carrier's bound and `Int`'s range are the same 64-bit
range, and cite [the chapter](../spec/evaluation-semantics.md#numbers) —
[DEC-16](../decisions/dec-16.md) for the argument — in place of `SPEC-28`. Keep the `BUG-12`
citations: that ticket is closed too, but it is named as history for why the arm returns an error
instead of panicking, and a tombstone row keeps it resolvable.

Do not restate the range as a number in either comment. `CLAUDE.md`'s rule that a doc comment
describes what the code at that site does is the reason: the site enforces `i64::from_str`'s
bound, and it happens to coincide with a rule stated elsewhere. A comment naming both is two
records of one width, and this ticket exists because the copy nobody maintained was the one that
went stale.

**Acceptance:** `grep -rn "SPEC-28" src/` is empty. Neither comment claims the language leaves
the width to the target or that the two chapters disagree. `cargo test --workspace` is green,
and `cargo doc` renders the rewritten doc comment.
