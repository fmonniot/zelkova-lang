# Layout — the offside rule

Zelkova is indentation-sensitive: where a line starts decides which construct it belongs
to, and there are no braces to fall back on. This chapter is the normative account of
that, because nothing else is — Elm, which Zelkova takes its surface from, never specified
its own offside rule, so "like Elm" is not an answer here.

Two passes share the work, and the split matters when reading an error message:

- The **tokenizer** (`src/compiler/parser/tokenizer.rs` — `handle_indentation`) looks at
  the whitespace at the start of each line and decides whether it is *well-formed* at all.
- The **layout pass** (`src/compiler/parser/layout.rs` — `layout`, `Layout::handle_next_token`)
  runs over the resulting tokens and decides what each column *means*, injecting the
  `OpenBlock` and `CloseBlock` tokens that let the grammar be written without any notion of
  indentation. Those two tokens are internal: they are not spellable in source, and seeing
  one named in an error message means a layout rule was violated rather than a token missed.

## Indentation is measured in two-space levels

Leading whitespace on a line must be an even number of spaces. One level is two spaces,
and there are no half levels.

```zel expect=ok
module Example exposing (describe)

type Flag
  = On
  | Off

describe f =
  case f of
    On ->
      1

    Off ->
      0
```

An odd number of leading spaces is rejected before layout ever sees the line:

```zel expect=parse-error:IndentationError
module Example exposing (f)

f x =
   1
```

**Tabs never indent.** A tab in a line's leading whitespace is an error in its own right,
not a width — Zelkova takes no position on how wide a tab is because no line may be
indented with one:

```zel expect=parse-error:TabError
module Example exposing (f)

f x =
	1
```

A tab *after* the first non-whitespace character of a line is ordinary whitespace and is
fine; the rule is about indentation only.

Whitespace-only lines are exempt from both rules. A "blank" line carrying three spaces, or
a tab, is still blank — `handle_indentation` resets its count and moves on when it reaches
the newline, so such a line neither breaks the even-width rule nor closes any block.

The blank line in the example below is not empty: it holds three spaces, which would be an
odd-width indentation error on any line that had a token on it. That whitespace is
load-bearing — it is what makes this block a test of the exemption rather than a
restatement of the previous one, and stripping it (an editor "cleaning" trailing
whitespace will) quietly turns this into an example that proves nothing.

```zel expect=ok
module Example exposing (f)
   
f x =
  1
```

## A file starts at column 1

The first token of a source file is the `module` keyword, and it sits in column 1. Any
space or tab before it is invalid.

```zel expect=parse-error:UnexpectedToken
  module Example exposing (f)

  f x =
    1
```

This is a real rule rather than an accident of the implementation, but the implementation
currently enforces it by accident and reports it badly — the caret lands on a later
declaration and the message asks for `close block`, an internal token the reader cannot
write. [`docs/tickets/err-12.md`](../tickets/err-12.md) tracks the diagnostic; the rule
above is what the language says either way.

The example is tagged `expect=parse-error:UnexpectedToken`, naming that wrong-but-current
error on purpose. When ERR-12 lands and the message becomes a real one, the block goes red
— which is the point: the paragraph you are reading describes a diagnostic that will stop
existing, and it should not be able to outlive it.

## Top-level declarations

Every top-level declaration — `module`, `import`, a type declaration, a type annotation, a
function definition — begins in column 1. A token in column 1 on a line after the current
declaration started is what ends that declaration and begins the next; a declaration's own
continuation lines must therefore be indented.

```zel expect=ok
module Example exposing (first, second)

first x =
  1

second x =
  2
```

There is no separator between declarations. Blank lines between them are conventional and
carry no meaning.

## `case … of`

This is where layout does its real work, and where most of the rules live.

### The scrutinee

The expression between `case` and `of` may span several lines. It opens a block of its
own, closed by the `of`:

```zel expect=ok
module Example exposing (describe)

type Flag
  = On
  | Off

describe f =
  case
    f
  of
    On -> 1
    Off -> 0
```

### The first branch fixes the column for all of them

When the block of branches opens, the first token after it sets the column that every
branch in that block must start on. The implementation records it as
`Context::CaseBlock(Some(column))` and reads it back through `Offside::min_indent`; there
is no fixed relationship to the column of the `case` keyword, only that it be deeper.

Branches may be written one per line:

```zel expect=ok
module Example exposing (describe)

type Flag
  = On
  | Off

describe f =
  case f of
    On -> 1
    Off -> 0
```

A branch that starts **left** of that column is an error:

```zel expect=parse-error:LayoutError
module Example exposing (describe)

type Flag
  = On
  | Off

describe f =
  case f of
      On -> 1
    Off -> 0
```

A branch that starts **right** of it is equally an error. All branches of one `case … of`
start on the same column; a deeper line that begins a new branch is a mistake, not a
continuation of the branch above it:

```zel expect=parse-error:UnexpectedToken
module Example exposing (describe)

type Flag
  = On
  | Off

describe f =
  case f of
    On -> 1
      Off -> 0
```

The compiler rejects that today, but for the wrong reason and with the wrong caret: layout
has no rule for the deeper line, so it is absorbed into the previous branch's body and the
grammar then trips on the second `->`. [`docs/tickets/err-11.md`](../tickets/err-11.md)
tracks the diagnostic. As with the column-1 rule above, the language's answer is unchanged
by that ticket, and the block pins today's `UnexpectedToken` so this paragraph goes red
along with it.

### A branch body is deeper than its pattern

A branch body may sit on the same line as its `->`, or on following lines indented past
the branch's own column. It may not start at the branch's column — that column belongs to
the next branch, so a body written there closes the branch and leaves it empty:

```zel expect=parse-error:UnexpectedToken
module Example exposing (describe)

type Flag
  = On
  | Off

describe f =
  case f of
    On ->
    1

    Off ->
      0
```

Written correctly, with the body one level deeper than the pattern:

```zel expect=ok
module Example exposing (describe)

type Flag
  = On
  | Off

describe f =
  case f of
    On ->
      1

    Off ->
      0
```

### Nesting

A `case … of` in a branch body follows the same rules relative to its own position, which
is what makes the deeply-nested shapes in `std/core/src/Maybe.zel` legal:

```zel expect=ok
module Example exposing (both)

type Flag
  = On
  | Off

both a b =
  case a of
    On ->
      case b of
        On ->
          1

        Off ->
          2

    Off ->
      0
```

## `let … in`

`let` blocks are designed and partially built, but not usable. `layout.rs` carries a
complete `Context::Let` — `let` opens a context at its own column plus one, and `in` closes
it — and the tokenizer produces both keywords. The grammar does not: `grammar.lalrpop`'s
`extern` token list has no `let` and no `in`, so nothing downstream can consume them and
the parse fails on the `let` itself.

```zel expect=unimplemented
module Example exposing (f)

f x =
  let
    y = 1
  in
    y
```

The layout rules recorded in `layout.rs` for when it does land: the bindings sit deeper
than the `let`, and `in` closes the block. Two questions its own `TODO`s leave open —
whether a `let` block should emit `OpenBlock`/`CloseBlock` the way `case … of` does, and
whether `in` must align with its `let` — are undecided, so this chapter does not answer
them either. That example is tagged `expect=unimplemented`: it will go red the day `let`
is implemented, which is the signal to come back and finish this section.

## What the compiler tracks

For anyone reading `layout.rs` alongside this chapter: the pass keeps a stack of
`Offside { context, indent, line }`, one entry per open block, and `Context` names the five
kinds — `TopLevelDeclaration`, `CaseExpression`, `CaseBlock`, `CaseBranch`, `Let`.
`Context::description()` is what turns one of those into the phrase a diagnostic shows the
user ("the branches of a `case … of`"), so an indentation error names the block it broke.

The pass **stops at its first error**. A layout violation is diagnosed without changing the
context stack, so there is no state from which the same input could be read differently;
the iterator therefore fuses rather than retrying and looping forever. One malformed source
file yields one layout error, never a list — unlike canonicalization and type checking,
which accumulate. `CLAUDE.md`'s standing invariant on `Result`-yielding iterators has the
history (`BUG-4`, `BUG-5`).
