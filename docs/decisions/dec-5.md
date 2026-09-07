# DEC-5 · A pattern's sign is the pattern grammar's, not the tokenizer's

**Settled:** 2026-09-06, by the language owner (`SPEC-13`).
**Status:** live.
**Where the rule lives:** [Patterns](../spec/patterns.md#literal-patterns) and
[Lexical structure](../spec/lexical-structure.md#integers).

`SPEC-13` found two chapters disagreeing about a pattern's negative literal. *Patterns* said
"the sign belongs to the literal", which reads as a claim about the token; *Lexical
structure*'s *Integers* said an integer token never has a leading `-`. Both cannot be true of
the same thing, and the chapter that owns tokens was the one telling the truth about them — so
the disagreement was really an unanswered question one level up: does a signed literal exist as
a **token**, produced by the tokenizer, or as a **pattern-grammar production**, where the token
stream is unchanged and the pattern grammar itself consumes a leading `-` before a literal?

## 1 — The pattern grammar consumes the sign; the tokenizer is unchanged

The token reading was the tempting one, because it looks like the smaller change: teach the
tokenizer to emit a signed `Integer`, and pattern matching falls out for free. It loses, and the
reason is not visible from *Patterns* or *Lexical structure* alone, because neither chapter is
allowed to argue against a rule it didn't choose
([a chapter says what the language is](../spec/conventions.md#a-chapter-says-what-the-language-is)).

A signed integer token does not stay confined to pattern position — the tokenizer has no notion
of position at all, so `Integer { value: -1 }` would come out of `f -1` in an *expression* just
as readily as out of a `case` branch. Something would then have to stop that from tokenizing as
`f` applied to `-1`, and the only thing available to stop it is adjacency: whether a space
separates the `-` from what follows. [Expressions](../spec/expressions.md#prefix-negation)
already rules that out, in the plainest possible terms — `g -n` is `g` minus `n`, not `g`
applied to a negative literal, because the language never lets meaning turn on whether a
character is present, and doubly never on whether a *space* is. Taking the token reading here
would mean reopening that rule, for every expression in the language, to answer a question that
only ever comes up in one.

The pattern-grammar reading has no such cost. A pattern is the one place syntax is closed —
[Patterns](../spec/patterns.md) already says a pattern never resolves an operator, because
nothing is available to apply one to — so it is also the one place a leading `-` has no operator
lookup to compete with. The tokenizer keeps emitting `-` as the same operator token it always
has, in every position; the pattern grammar's literal production is the only thing that ever
looks for one immediately before a literal. `-` never gains a second, position-dependent
meaning at the token level, and *Integers*' "no leading `-`" stays true of the token exactly as
written.

The same distinction recurs for `::` as a list-pattern separator rather than an operator
application — [`SPEC-22`](../tickets/spec-22.md) names it as the same question, worth settling
the same way, for the same reason: pattern syntax has nowhere to look an operator up, so a
pattern-grammar production is the reading that costs nothing outside patterns.
