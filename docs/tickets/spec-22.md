# SPEC-22 · Lists are part of the language and the chapter specifying them does not exist

**Sizing:** medium. A chapter for a construct with three spellings already promised by other
chapters, and no design that has been written down anywhere.

**Location:** a new chapter under [`docs/spec/`](../spec/README.md), plus its row in that file's
chapter table. The deferrals are
[`docs/spec/expressions.md`](../spec/expressions.md)'s opening table and its *Forms the compiler
does not have*, which says outright that "the list chapter is unwritten";
[`docs/spec/patterns.md`](../spec/patterns.md)'s form table, which gives `[]`, `[a, b]` and
`first :: rest` spellings with no chapter behind them;
[`docs/spec/types.md`](../spec/types.md)'s *Type expressions*; and
[`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s *Punctuation*.

**Problem:** the same gap as [`SPEC-21`](spec-21.md), one construct over.
[Expressions](../spec/expressions.md) names list literals as one of "three more forms … part of
the language and specified in their own chapters", and its *Forms the compiler does not have*
table then says of `[1, 2]` that "[Lexical structure] carries the brackets; the list chapter is
unwritten". [`docs/spec/README.md`](../spec/README.md)'s chapter table lists twelve chapters, all
`written`, and there is no list row — so, as with records, the deferral points at a chapter that
is not merely unwritten but unplanned, while the spec presents itself as complete.

Lists are further along than records in one respect and that makes the gap sharper rather than
softer. [Patterns](../spec/patterns.md)' form table already commits to three spellings — the
empty list `[]`, the fixed-length `[a, b]`, and the cons split `first :: rest` — and describes
what each matches. Those are claims about the language made in a chapter that then says the
construct is specified elsewhere. `::` in particular is an operator spelling that no `infix`
declaration introduces and no chapter defines, so a reader cannot find out what it is.

**Not implemented:** `[` and `]` are tokenized — `src/compiler/parser/tokenizer.rs` emits
`Token::LBracket` and `Token::RBracket` — but no production in
`src/compiler/parser/grammar.lalrpop` consumes them, which
[Lexical structure](../spec/lexical-structure.md) states accurately. There is no list type, no
literal, and no cons.

**The design to settle:**

- **The type.** How a list type is written — `List a`, `[a]`, or something else — and whether
  `List` is an ordinary type constructor declared in `std/core` or a construct the grammar knows.
  This one has a consequence beyond lists: an ordinary `List a` is a type constructor applied to a
  variable, which [Types](../spec/types.md) already supports, while `[a]` is new type syntax.
- **The literal.** `[1, 2]` and `[]`, and whether the empty literal needs an annotation to have a
  type.
- **Cons.** Whether `::` is an ordinary operator with an `infix` declaration in `std/core`, the
  way every other operator is, or a reserved spelling. If it is ordinary, then the *pattern*
  half is the problem: [Patterns](../spec/patterns.md) is explicit that a pattern never resolves
  an operator, so `first :: rest` as a pattern cannot be an operator application and has to be a
  pattern production — the same distinction [`SPEC-13`](spec-13.md) is settling for negative
  literals, and worth settling the same way.
- **What `std/core` provides.** Whether the chapter specifies a list *type* only and leaves the
  functions over it to the library, which is the split every other chapter uses.

This ticket does not pick any of them.

**Approach:** follow `write-spec-chapter`. Then:

1. A new chapter is written, with every block tagged `expect=unimplemented`, and its row added to
   [`docs/spec/README.md`](../spec/README.md)'s chapter table.
2. [Patterns](../spec/patterns.md)' list and cons rows link to it, and the chapter says which
   level consumes `::` in a pattern.
3. [Types](../spec/types.md), [Expressions](../spec/expressions.md) and
   [Lexical structure](../spec/lexical-structure.md) link to the new chapter rather than to one
   that does not exist.
4. `LANG-` tickets are filed for the implementation. Filed, not implemented.

**What this is not.** Not [`SPEC-21`](spec-21.md), which is the same gap for records; the two are
independent and neither blocks the other. Not an implementation, and not a `std/core` list
module — `std/core/src/` has no list module today and adding one is downstream of this.

**Acceptance:** `docs/spec/` has a list chapter, listed in
[`docs/spec/README.md`](../spec/README.md)'s table, specifying the list type, the literal, and
cons in both expression and pattern position. No chapter still says the list chapter is
unwritten, and [Patterns](../spec/patterns.md)' three list rows resolve to a section that exists.
`cargo test --test spec` green, with every new block tagged `expect=unimplemented` and proven to
fail.

**Found:** while grounding [`SPEC-21`](spec-21.md) during an audit of `docs/spec/` for open
questions with no ticket attached, on 2026-09-04. Like that one, it is stated inline rather than
in an *Open questions* section, which is why neither was in the count.
