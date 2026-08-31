# SPEC-13 · Whether a pattern's negative literal is a token or a pattern production is unsettled, and two chapters answer it differently

**Sizing:** small. Prose in two files, and one design question to settle before writing a word
of it. No compiler change — but see *What this is not*, because the tempting fix is the wrong
one.

**Location:** [`docs/spec/patterns.md`](../spec/patterns.md)'s *Literal patterns*;
[`docs/spec/lexical-structure.md`](../spec/lexical-structure.md)'s *Integers*.

**Problem:** the language has a negative literal in pattern position and not in expression
position, which is deliberate — a pattern never resolves an operator, so nothing is available
to apply. Two chapters describe the pattern half at different levels, and the descriptions
contradict each other.

[Patterns](../spec/patterns.md)' *Literal patterns* puts the sign on the literal:

> A pattern may hold a **negative** number. The sign belongs to the literal rather than being
> an operator applied to it: pattern syntax is closed, and `-` in an expression is a name bound
> by an `infix` declaration, which a pattern never looks up.

"The sign belongs to the literal" reads as a claim about the token.
[Lexical structure](../spec/lexical-structure.md)'s *Integers* denies exactly that claim:

> An integer literal is a run of ASCII digits, or `0x` followed by a run of ASCII hexadecimal
> digits. There are no digit separators, and no leading `-`: negation is an operator, described
> below.

Both cannot be true of the same thing. Either an integer literal may carry a sign or it may
not, and the chapter that owns tokens says it may not.

**The question to settle:** whether a signed literal is a **token** or a **pattern
production**. The two are not equivalent and the choice has consequences outside patterns:

- A **token** means the tokenizer emits `Integer { value: -1 }`, and something then has to stop
  `f -1` in an *expression* from tokenizing as `f` applied to `-1` rather than as subtraction.
  That rule would have to be about adjacency — about whether a space follows the `-` — which
  [Expressions](../spec/expressions.md#prefix-negation) explicitly rejects, on the grounds that
  a language whose meaning turns on invisible characters cannot be read aloud. Taking this
  branch means reopening that.
- A **pattern production** means the tokenizer is unchanged, `-` stays an operator token
  everywhere, and the sign is consumed by the pattern grammar — the one place with no operator
  lookup to compete with it. *Integers* then stays true exactly as written.

The second is almost certainly right and is what the rest of the design already implies. It
makes *Patterns*' "the sign belongs to the literal" a statement about the pattern grammar
rather than about the lexer, which is a sentence that needs rewriting rather than a rule that
needs changing.

Found while writing [`docs/spec/expressions.md`](../spec/expressions.md) (`SPEC-6`), which
specifies the expression half and made the disagreement about the pattern half visible.

**Approach:** settle the question with the language owner, then:

1. *Patterns*' *Literal patterns* says which level consumes the sign, keeping the reason it
   already gives — pattern syntax is closed, so there is no operator to look up — and stops
   implying the lexer produces a signed token if that is not the answer.
2. *Lexical structure*'s *Integers* keeps "no leading `-`", which is a claim about the token
   and true, and adds a clause noting that a pattern may carry a sign, linking to where that is
   specified. A reader who arrives at *Integers* asking what `-1` is should not leave with half
   the answer.

**What this is not.** Do not resolve the disagreement by giving expressions a negative literal
too. `-1` being negation applied to `1` in an expression is what keeps prefix negation a single
rule with no exceptions, and it is what
[Expressions](../spec/expressions.md#prefix-negation) relies on to explain why `g -n` is
subtraction rather than application. The asymmetry between the two positions is the design; only
the description of it is wrong.

**Acceptance:** *Integers* and *Literal patterns* agree on which level consumes the sign, and
each links to the other. No sentence in `docs/spec/` claims the language has, or lacks, a
negative literal without naming the position it means. `cargo test --test spec` stays green: no
block changes, since *Patterns*' `expect=unimplemented` block already pins the construct and
none of this alters what the compiler does with it.

**No block holds this to account**, and none can — a prose disagreement between two files is
not something a per-block expectation can observe. This is a read-and-fix ticket.

**Related, deliberately out of scope:** the grammar rejects a signed literal in a pattern
today, which is what *Patterns*' `expect=unimplemented` block records; implementing it is a
separate ticket nobody has filed, and it is gated on this question rather than the other way
round. [BUG-19](bug-19.md) is nearby — a line whose first token starts with `-` confuses
indentation measurement — and `src/compiler/parser/tokenizer.rs` carries a
`TODO Add support for negative number` on the `'-'` arm of its main loop, which is the
tokenizer-level reading this ticket most likely rules out. Leave that `TODO` alone until the
question is settled; if the pattern-production reading wins, it should be deleted rather than
implemented.
