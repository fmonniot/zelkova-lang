# SPEC-38 · `patterns.md` parenthesises every sub-pattern and also writes `Circle n :: rest` bare

**Sizing:** small. One rule, decided one way, and a few sentences or one grammar production
follow from it. It becomes larger if the answer is the second reading below, which is a grammar
change and so `CLAUDE.md`'s *A grammar change is never a one-file change*.

**Location:** `docs/spec/patterns.md` — *Patterns nest*, the sentence "An applied constructor
written as a sub-pattern is parenthesised." (line 524), and *List patterns*, "Both sides are
whole patterns, so `a :: b :: rest` matches a list of two or more, and `Circle n :: rest`
matches on the first element's shape." (lines 634-635); `docs/spec/lists.md` — *Lists in
patterns*, the same sentence (lines 184-185); [`LANG-45`](lang-45.md)'s *Approach*, which says
both "both of its sides are whole patterns, so `a :: b :: rest` and `Circle n :: rest` both
parse" and, two paragraphs later, that a cons pattern goes "parenthesised as a sub-pattern" and
that this is "the rule [Patterns](../spec/patterns.md#constructor-patterns) already states".

**Found while:** reviewing the PR for [`LANG-16`](README.md) (closed), which implemented the
first rule and so made it observable.

**Problem:** the two statements cannot both be the rule for the same position. `Circle n :: rest`
is an applied constructor as the left operand of `::`, which is a sub-pattern of the cons
pattern, and the first statement says an applied constructor in that position is parenthesised.
Nothing flags this today because every block that writes `Circle n :: rest` is
`expect=unimplemented` (`LANG-45` is not done), so the harness never compares the two.

The grammar follows the first statement and nothing else: one `Pattern` production serves a
constructor's argument, a tuple's element and a parameter, and it takes an applied constructor
only parenthesised. A tuple element is therefore a third position, with the same rule for a
different reason. Juxtaposition separates a constructor's arguments, so `Wrapper Just x` could
not be one pattern; commas separate a tuple's elements, so `(Just x, y)` has exactly one
reading, and it is rejected all the same. The reader gets

```
error: unexpected token: `Comma`
  ┌─ App.zel:8:12
8 │     (Full x, y) ->
  │            ^ unexpected token
  = we were expecting one of the following tokens: ["lo_ident", "up_ident", "integer", …, "(", ")", "_", …]
```

with no hint that parentheses are the fix, and `((Full x), y)` is the spelling that compiles.
Elm accepts both `(Just x, y)` and `Circle n :: rest`. (The error above is from a scratch
package at this branch's tip.)

**Approach:** the ticket does not choose; it is a question for the language owner, because it
settles what the language is. The two readings:

1. **The rule is categorical**: an applied constructor written as a sub-pattern is parenthesised
   wherever it appears, tuple elements and cons operands included. The grammar already agrees.
   *List patterns*, `lists.md` and `LANG-45`'s *Approach* then change: `Circle n :: rest` becomes
   `(Circle n) :: rest`, and `LANG-45` writes its cons production with that in mind. A worse
   message for `(Just x, y)` is worth an `ERR-` ticket on its own if this is the answer.
2. **The rule is about juxtaposed positions only**: a constructor's argument and a function's
   parameter. A tuple element and a cons operand admit a bare applied constructor, as in Elm. The
   first statement narrows to say so, *Patterns nest* and its three blocks gain a bare-form
   example, `Circle n :: rest` stands as written, and the grammar changes: a tuple alternative
   takes an element that may be an applied constructor without parentheses, and
   [`LANG-45`](lang-45.md)'s cons production takes one on its left. Whether the cons operand's
   precedence against a bare applied constructor needs a rule in the chapter is part of the
   work.

**Acceptance:** `patterns.md`, `lists.md` and `LANG-45` agree on every position, a reader of the
chapter can predict whether `(Just x, y)` and `Circle n :: rest` parse, and `cargo test --test
spec` is green. If the answer is the second reading, a parser test in
`crates/zelkova-syntax/tests/parser/patterns.rs` asserts `(Just x, y)` parses to a tuple whose
first element is `PatternKind::Constructor`, seen red against the current grammar.
