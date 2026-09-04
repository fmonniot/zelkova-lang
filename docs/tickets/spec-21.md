# SPEC-21 · Records are part of the language and no chapter says what one looks like

**Sizing:** medium. A chapter's worth of design and prose for a construct with no syntax, no
grammar and no precedent in the tree — and four existing chapters that defer to it and have to
be revisited once it exists.

**Location:** a new chapter under [`docs/spec/`](../spec/README.md), plus its row in that file's
chapter table. The four deferrals are
[`docs/spec/patterns.md`](../spec/patterns.md)'s *Record patterns* and its form table,
[`docs/spec/types.md`](../spec/types.md)'s *Type expressions*,
[`docs/spec/expressions.md`](../spec/expressions.md)'s opening table and its *Forms the compiler
does not have*, and [`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s
*Punctuation*.

**Problem:** four chapters state that records are part of the language, and every one of them
defers the spelling to a chapter that does not exist and is not planned.

[Patterns](../spec/patterns.md)' *Record patterns* is the whole section:

> A record pattern names fields rather than positions. Its spelling is not settled, because
> record syntax itself is not: records are part of the language, and no chapter yet says what one
> looks like in a type, an expression or a pattern.

[Types](../spec/types.md) says records "are specified with the constructs they belong to, neither
of which exists yet". [Expressions](../spec/expressions.md) lists records and field access among
"three more forms … specified in their own chapters" and then, at *Forms the compiler does not
have*, admits the chapter is unwritten. [Lexical structure](../spec/lexical-structure.md) reserves
`{` and `}` for records and says their syntax "is specified in the chapters on those constructs
rather than here".

Every one of those deferrals points at nothing. [`docs/spec/README.md`](../spec/README.md)'s
chapter table lists twelve chapters, all marked `written`, and there is no records row — so the
chapter is not merely unwritten, it is not on the list. That is worse than an open question at
the foot of a chapter, because the spec reads as complete: a reader following any of the four
cross-references arrives nowhere, and nothing in the ticket index says anyone is going to write
it.

Records are also load-bearing for a decision already made. [Types](../spec/types.md)' *Tuples*
justifies the two-or-three limit by saying "a tuple of four is where a record belongs" — so the
language declines to grow tuples on the strength of a construct it has never specified.

**Not implemented:** none of it exists in the compiler. `{` and `}` are not tokens at all —
`src/compiler/parser/tokenizer.rs` recognises `{` only as the opening of a `{-` block comment,
and there is no `LBrace` beside the `LBracket`/`RBracket` it does have. So a records chapter
starts from nothing, and every block in it is `expect=unimplemented`.

**The design to settle** is a chapter's worth, not a single question. At minimum:

- **The type.** What a record type is written as, whether field order matters, and whether two
  record types with the same fields in different orders are the same type.
- **Construction and access.** `{ a = 1 }` and `r.name` are the spellings
  [Expressions](../spec/expressions.md) already names, along with the bare `.name` accessor as a
  function. Whether `.name` is a real expression form or sugar decides how it interacts with
  [name resolution](../spec/name-resolution.md), where `.` currently means qualification only.
- **Update.** `{ r | a = 2 }` is named in the same table. Whether update may change a field's
  type, and whether it may add a field, are the two questions that decide whether records are
  nominal-ish or structural.
- **Patterns.** What a record pattern looks like and whether it must name every field. This is
  the section [Patterns](../spec/patterns.md) is holding a place for.
- **Extensibility.** Whether a function may accept "any record with a `name` field" — Elm's
  extensible records. This is the big one, and it is where the cost lives: it needs row
  polymorphism in the type checker, and [Type classes](../spec/type-classes.md) deliberately has
  no higher-kinded variables, so it would be the first place the type system grows a second axis.
  A chapter that says records are *not* extensible is a legitimate and much cheaper answer.

This ticket does not pick any of them. It is filed to make the absence visible and to give the
four dangling cross-references somewhere to point.

**Approach:** follow `write-spec-chapter`. Settle the design above with the language owner —
extensibility first, since it decides the shape of everything else. Then:

1. A new chapter is written, with every block tagged `expect=unimplemented`, and its row added to
   [`docs/spec/README.md`](../spec/README.md)'s chapter table.
2. [Patterns](../spec/patterns.md)' *Record patterns* is written rather than deferring, and its
   form table gains the record row it currently omits.
3. [Types](../spec/types.md), [Expressions](../spec/expressions.md) and
   [Lexical structure](../spec/lexical-structure.md) link to the new chapter instead of to a
   chapter that does not exist.
4. `LANG-` tickets are filed for the implementation — at least the brace tokens, the grammar, both
   ASTs and the typer. Filed, not implemented: a spec change and a semantics change do not share
   a diff, and this is several diffs' worth.

**What this is not.** Not an implementation of records, and not a reason to touch the tokenizer
in this ticket. Also not the list chapter, which is the same shape of gap and is
[`SPEC-22`](spec-22.md) — the two are separately shippable and neither blocks the other, though
`[]` and `{}` are reserved by the same *Punctuation* section.

**Acceptance:** `docs/spec/` has a records chapter, listed in
[`docs/spec/README.md`](../spec/README.md)'s table, specifying records in a type, an expression,
an update and a pattern, and ruling on extensibility. No chapter still says record syntax is
unsettled, and every existing cross-reference to records resolves to a section that exists.
`cargo test --test spec` green, with every new block tagged `expect=unimplemented` and proven to
fail.

**Found:** while auditing `docs/spec/` for open questions with no ticket attached, on 2026-09-04.
This one is not in an *Open questions* section — it is stated inline in four chapters, which is
why it had escaped the count.
