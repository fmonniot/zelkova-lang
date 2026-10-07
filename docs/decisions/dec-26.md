# DEC-26 · A pattern is parenthesised in an argument position, and nowhere else: two decisions

**Settled:** 2026-10-06, by the language owner (`SPEC-38`).
**Status:** live.
**Where the rule lives:** [Patterns](../spec/patterns.md#where-a-pattern-is-parenthesised).

`SPEC-38` found *Patterns* stating two rules for one position. *Patterns nest* said "an applied
constructor written as a sub-pattern is parenthesised", with no exception. *List patterns*, and
[Lists](../spec/lists.md#lists-in-patterns) after it, wrote `Circle n :: rest`, an applied
constructor as the operand of a cons pattern, without parentheses. No test saw the
disagreement, because every block writing the second form is `expect=unimplemented`.

The grammar had taken the first statement literally. One production served a constructor's
argument, a parameter, a tuple's element and a record pattern's entry, so `(Just x, y)` and
`{ taken = Celsius t }` were syntax errors, and `((Just x), y)` was the spelling that compiled.

## 1 — Parentheses are required in an argument position only

An argument position is a constructor's argument or a parameter. A pattern that is not atomic
is parenthesised there and written bare in every other position: the left-hand side of a `case`
branch, a tuple's element, a record pattern's entry, a bracketed list pattern's element.

The rejected reading was the categorical one: every nested applied constructor is parenthesised,
cons operands and tuple elements included, and `Circle n :: rest` becomes `(Circle n) :: rest`.
It had one thing for it, that the grammar already agreed. It loses on three counts.

**The reason for the parentheses does not reach the other positions.** In `Wrapper Just x`,
juxtaposition is the only separator, and without parentheses the pattern is `Wrapper` applied
to two arguments. In `(Just x, y)` the comma separates, and there is exactly one reading. The
categorical rule would have the chapter require parentheses there with nothing to say about
why.

**[Expressions](../spec/expressions.md#application) already has this rule.** A non-atomic
expression is parenthesised in argument position and bare elsewhere, so `(Just x, y)` builds a
tuple. Under the categorical rule the pattern that matches that tuple is spelled differently
from the expression that built it. Under this one a pattern is written the way its value is,
and the reader carries one rule for both.

**The error it produced taught nothing.** `(Full x, y)` was reported as an unexpected `Comma`,
with a list of expected tokens and no hint that parentheses were the fix. Elm accepts the same
source: its parser reads a tuple's or a list's entry as a full pattern and a constructor's
argument as a term.

The same tiers answer the cons operand with no rule of its own.
[Lists](../spec/lists.md#the-cons-operator) has the standard library declare `::` as
`infix right 5`, and application binds tighter than any operator, so the expression
`Circle n :: rest` is `(Circle n) :: rest`. The pattern groups the same way, although `::` in a
pattern is a production and not that operator
([`DEC-7` decision 3](dec-7.md#3---is-an-ordinary-operator-in-an-expression-and-a-pattern-production-in-a-pattern)):
the grouping is fixed in the pattern grammar and does not follow an `infix` declaration a
module might write.

The cost is a grammar change. The single pattern production becomes the usual tiers — an atom,
an application, a cons, an as-pattern — and the production a `case` branch had to itself, which
existed to take an applied constructor bare, becomes the whole-pattern tier every delimited
position uses. [`LANG-89`](../tickets/lang-89.md) is that work, and
[`LANG-45`](../tickets/lang-45.md) writes its cons production into the same tiers.

## 2 — An as-pattern follows the same rule, as the loosest tier

*As-patterns* said "an as-pattern written as a sub-pattern is parenthesised", the same
categorical sentence one form over. It gets the same answer. `as` binds more loosely than `::`,
so `first :: rest as whole` names the whole list, an as-pattern is bare as a tuple's element, a
record pattern's entry or a list pattern's element, and it is parenthesised in an argument
position and as an operand of `::`. Elm groups `::` and `as` the same way.

Keeping the as-pattern categorical while freeing the applied constructor would have left the
chapter with two rules for where parentheses go, one per form, and the difference between them
would have been which ticket noticed first.

## What this does not settle

A `let` binding's left-hand side and a lambda's parameter are positions the language does not
have yet. A lambda's parameters are separated by juxtaposition, as a declaration's are, so the
rule as written covers them. Whether a `let` binding's left-hand side takes an applied
constructor bare is a question for whoever specifies `let`.
