# Layout — the offside rule

Zelkova is indentation-sensitive: where a line starts decides which construct it belongs
to, and there are no braces to fall back on. This chapter is the normative account of
that, because nothing else is — Elm, which Zelkova takes its surface from, never specified
its own offside rule, so "like Elm" is not an answer here.

Two kinds of thing can go wrong with a line, and the split matters when reading an error
message. The leading whitespace may be malformed in its own right — an odd number of
spaces, or a tab — in which case the error is about the whitespace and says so. Or the
whitespace may be well-formed and the *column* wrong for the construct the line belongs
to, in which case the error names the block that was broken: "the branches of a
`case … of`", "the expression of a `case … of`".

An error message may also name `open block` or `close block`. Those are not tokens you can
write. They are Zelkova's block structure made explicit so that a diagnostic has something
to point at, and seeing one in a message means a layout rule was violated rather than a
token missed.

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
a tab, is still blank: such a line neither breaks the even-width rule nor closes any block.

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
branch in that block must start on. There is no fixed relationship to the column of the
`case` keyword, only that it be deeper.

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

`let … in` is part of the language as designed and is not implemented yet. A `let` in a
source file is rejected today.

```zel expect=unimplemented
module Example exposing (f)

f x =
  let
    y = 1
  in
    y
```

The layout rules already decided for it: the bindings sit deeper than the `let`, and `in`
closes the block. Two questions are **open**, and this chapter deliberately does not answer
them — whether the bindings of a `let` form a block with the same column discipline as
`case … of` branches, so that the first binding fixes the column for all of them; and
whether `in` must align with its `let`. That example is tagged `expect=unimplemented`: it
will go red the day `let` is implemented, which is the signal to come back and finish this
section.

## One layout error at a time

A file with a layout mistake yields exactly one layout error, never a list — unlike
canonicalization and type checking, which report everything they find. Indentation is not
independent line by line: a wrong column changes what every column after it means, so a
second error reported past the first would be a guess about a file the reader has not
written yet.
