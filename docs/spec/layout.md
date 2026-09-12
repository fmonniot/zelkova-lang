# Layout — the offside rule

Zelkova is indentation-sensitive: where a line starts decides which construct it belongs
to, and there are no braces to fall back on.

Two kinds of thing can go wrong with a line. The leading whitespace may be malformed in
its own right — an odd number of spaces, or a tab — in which case the error is about the
whitespace and says so. Or the whitespace may be well-formed and the *column* wrong for
the construct the line belongs to, in which case the error names the block that was
broken: "the branches of a `case … of`", "the expression of a `case … of`".

An error message may also name `open block` or `close block`. Those are not tokens you can
write. They are Zelkova's block structure made explicit so that a diagnostic has something
to point at, and seeing one in a message means a layout rule was violated rather than a
token missed.

## Indentation is measured in two-space levels

Leading whitespace on a line must be an even number of spaces. One level is two spaces.

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

## Tabs are legal only inside a comment

A tab is an error anywhere in a source line except inside a comment. It does not matter
whether the tab is indenting the line, separating two tokens, or sitting alone on a line
that would otherwise be blank. Zelkova therefore takes no position on how wide a tab is,
because no position in a program can depend on one.

A tab in a line's leading whitespace:

```zel expect=parse-error:TabError
module Example exposing (f)

f x =
	1
```

A tab between two tokens, well after the line's first non-whitespace character, is the
same error:

```zel expect=parse-error:TabError
module Example exposing (f)

f x =
  1	+	2
```

So is a tab on a line that carries nothing else:

```zel expect=parse-error:TabError
module Example exposing (f)
	
f x =
  1
```

Inside a comment — from `--` to the end of the line, or between `{-` and `-}` — a tab is
ordinary text and is accepted:

```zel expect=ok
module Example exposing (f)

-- a	tab in a line comment
{- and a	tab in a block comment -}
f x =
  1
```

## A blank line is blank whatever spaces it holds

A line holding only spaces is blank whatever its width: it neither breaks the even-width
rule nor closes any block.

The blank line in the example below is not empty: it holds three spaces, which would be an
odd-width indentation error on any line that had a token on it. Stripping that whitespace,
as an editor "cleaning" trailing whitespace will, turns the block into an example that
proves nothing.

```zel expect=ok
module Example exposing (f)
   
f x =
  1
```

## A file starts at column 1

The first **token** of a source file is the `module` keyword, and it sits in column 1. A
comment may precede it — comments are consumed as part of the indentation scan, so they
are invisible to this rule:

```zel expect=ok
-- Comments before the header are fine.
module Example exposing (f)

f x =
  1
```

Whitespace before `module` is not. A leading space is invalid under this rule; a leading
tab is invalid too, but as a tab (above).

The compiler does not enforce this rule as stated. An indented file holding a single
declaration is accepted:

```zel expect=ok
  module Example exposing (f)
```

**Known gap:** that block is tagged `expect=ok` because that is what happens today, and it
is the gap [`docs/tickets/err-12.md`](../tickets/err-12.md) tracks made visible. The
language's answer is that this file is invalid.

An indented file with a *second* declaration is rejected, but for a reason one step removed
from the rule: the second declaration is what fails, not the indentation on line 1.

```zel expect=parse-error:UnexpectedToken
  module Example exposing (f)

  f x =
    1
```

**Known gap:** that block pins "an indented file with two declarations is rejected", not "a
file must start at column 1". The diagnostic is bad in the way ERR-12 describes — the caret
lands on the later declaration and the message asks for `close block`, which the reader
cannot write. It is tagged `expect=parse-error:UnexpectedToken`, naming that
wrong-but-current error on purpose: when ERR-12 lands and the message becomes a real one,
the block goes red.

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

A line written in column 1 where a continuation was meant closes the declaration, and the
parser is then handed the end of a declaration whose body never arrived.

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

f x =
1
```

There is no separator between declarations. Blank lines between them are conventional and
carry no meaning.

A declaration written as several clauses is no exception. Each clause begins in column 1
and so ends the one before it; what makes them one declaration is that they share a name
and stand together, which is [Declarations](declarations.md#the-clauses-stand-together)'
rule rather than a layout one. See also
[Patterns](patterns.md#a-pattern-that-can-fail-and-one-that-cannot).

## `case … of`

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
branch in that block must start on. That column must be strictly deeper than the column of
the `case` keyword itself.

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

A branch that starts **left** of the column the first branch established is an error:

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

A branch that starts **right** of it is equally an error: a deeper line that begins a new
branch is a mistake, not a continuation of the branch above it:

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

**Known gap:** the compiler rejects that today, but for the wrong reason and with the wrong
caret: layout has no rule for the deeper line, so it is absorbed into the previous branch's
body and the grammar then trips on the second `->`.
[`docs/tickets/err-11.md`](../tickets/err-11.md) tracks the diagnostic. The language's
answer is unchanged by that ticket, and the block pins today's `UnexpectedToken` so this
paragraph goes red along with it.

Even where a branch satisfies the enclosing block's own indentation, sitting level with
`case` — or left of it — is an error:

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

A `case` written on the same line as the `=` that introduces the body puts that floor deep
inside the line rather than at its start, so its branches have to follow it there:

```zel expect=ok
module Example exposing (describe)

type Flag
  = On
  | Off

describe f = case f of
              On -> 1
              Off -> 0
```

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

A `case … of` in a branch body follows the same rules relative to its own position:

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

`let … in` is part of the language as designed and is not implemented. A `let` in a source
file is rejected today:

```zel expect=unimplemented
module Example exposing (f)

f x =
  let
    y = 1
  in
    y
```

**Not implemented:** that example is tagged `expect=unimplemented`; it will go red the day
`let` is implemented.

**Not implemented:** the layout rules below are design intent, not something the compiler
has ever checked. The bindings sit deeper than the `let`, and `in` closes the block. Two
questions are **open** — whether the bindings of a `let` form a block with the same column
discipline as `case … of` branches, so that the first binding fixes the column for all of
them; and whether `in` must align with its `let`. [`LANG-33`](../tickets/lang-33.md)
answers both, as the first step of implementing `let … in`.
