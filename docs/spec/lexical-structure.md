# Lexical structure

A Zelkova source file is read in three stages. A **tokenizer** turns characters into tokens;
a **layout** pass inserts block markers by looking at where each line starts
([Layout](layout.md)); a **grammar** assembles tokens into declarations. This chapter is the
first stage: what the characters of a file mean, before any structure is imposed on them.

Zelkova takes its surface syntax from Elm, and this chapter is where that inheritance stops
being a deferral. Every rule below is stated here in full. Where Zelkova and Elm differ, or
where Elm never wrote a rule down, the text below is the answer — there is no other document
to consult.

## Source text

A source file is Unicode text, encoded as UTF-8, in a file whose name ends in `.zel`.

A line ends at a line feed (`U+000A`). A carriage return immediately followed by a line feed
(`U+000D U+000A`) is one line ending and means exactly the same thing, so a file written on
Windows and the same file written on Unix tokenize identically. A carriage return anywhere
else is not valid source text.

The space (`U+0020`) is the only whitespace character Zelkova recognises. It separates
tokens, and in a line's leading run it is what [Layout](layout.md) measures. A horizontal tab
is invalid anywhere outside a comment — that is Layout's rule, and the reasoning is there.

Tokens are formed by **maximal munch**: at each point the tokenizer takes the longest run of
characters that can form a token. This is why `1.5` is one float rather than `1`, `.`, `5`,
and why `<=` is one operator rather than `<` followed by `=`. Two tokens that would otherwise
run together are separated by a space.

## Comments

A **line comment** starts at `--` and runs to the end of the line.

```zel expect=ok
module Example exposing (f)

-- a line comment
f = 1
```

Because `--` always starts a comment, no operator can contain two consecutive hyphens, and
`a--a` is the single token `a` followed by a comment rather than anything arithmetic:

```zel expect=ok
module Example exposing (f)

f a =
  a--a
```

A **block comment** runs from `{-` to the matching `-}`. Block comments **nest**: an inner
`{-` must be closed by its own `-}` before the outer comment can end. Nesting is what makes
commenting out a region safe — a region that already contains a comment stays commented out
as a whole, rather than ending early and leaving the tail of it as code.

A block comment ends exactly at its closing `-}`. Whatever follows on that line is ordinary
source text. A block comment may appear anywhere a space may, which is what makes an inline
one possible at all.

```zel expect=parse-error:UnrecognizedToken
module Example exposing (f)

f = {- a note -} 1
```

**Known gap:** that block should be `expect=ok` — it is valid Zelkova. Block comments are
recognised today only in a line's leading whitespace, so the `{` here is not read as opening
a comment at all and the tokenizer rejects it as an unrecognised character.
[`docs/tickets/bug-13.md`](../tickets/bug-13.md) tracks this and the two below, which are the
same defect seen from other angles.

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

{- a note -} f =
  1
```

**Known gap:** that block should be `expect=ok` too. Here the comment *is* recognised, being
at the start of a line, but the closing `-}` discards the rest of the line along with itself,
so `f =` never reaches the parser and the `1` beneath it is left with nothing to belong to.
This is the worst of the three: source silently stops existing.

```zel expect=parse-error:IndentationError
module Example exposing (f)

{- outer
   {- inner -}
   still outer
-}
f = 1
```

**Known gap:** that block should be `expect=ok`. Comments do not nest today, so the inner
`-}` closes the whole comment; `still outer` is then read as source, and its three-space
indentation is what the tokenizer complains about. The reported error is pinned here
precisely because it is an accident of two other bugs, and should not survive their fix.

Reaching the end of a file inside a block comment is an error. A stray `{-` should not be
able to delete the rest of a file without saying so.

```zel expect=ok
module Example exposing (f)

f = 1
{- oops, never closed
```

**Known gap:** that block should be rejected. Today the tokenizer runs to end-of-file and
stops, accepting the file — the silent deletion this rule exists to prevent. Also
[`bug-13`](../tickets/bug-13.md).

A comment beginning `{-|` is an ordinary block comment. The convention that such a comment
documents the declaration below it, and the `@docs` markup used inside the module-level one,
belong to documentation tooling; the compiler discards them like any other comment and this
specification says nothing further about them.

```zel expect=ok
module Example exposing (f)

{-| Documentation for f. -}
f = 1
```

Tabs are legal inside a comment of either kind, and only there. See
[Layout](layout.md#tabs-are-legal-only-inside-a-comment).

## Identifiers

An identifier begins with an **uppercase or lowercase letter** and continues with any number
of letters, digits, and underscores. Precisely: the first character is in Unicode general
category `Lu` or `Ll`; each subsequent character is in `Lu`, `Ll`, `Lt`, `Lm`, `Lo`, `Nd` or
`Nl`, or is `_` (`U+005F`).

The first letter is not a formality — it decides what kind of thing the identifier names:

| First letter | Names |
|---|---|
| uppercase | a type, a type constructor, or a module |
| lowercase | a value, a function, a function parameter, or a type variable |

That is the whole reason the start rule is narrower than the continuation rule. A letter that
has no case cannot begin an identifier, because there would be nothing for the language to
read off it. Such letters are perfectly good *inside* a name, where nothing depends on their
case.

```zel expect=ok
module Example exposing (Форма, aire)

type Форма
  = Ronde

aire côté =
  côté
```

`Форма` begins with a Cyrillic capital, so it names a type; `aire` and `côté` begin with
lowercase letters, so they name a function and its parameter. A name may mix scripts freely
after its first character:

```zel expect=ok
module Example exposing (f)

fデータ = 1

f = fデータ
```

A name may not *begin* with an uncased letter:

```zel expect=parse-error:UnrecognizedToken
module Example exposing (f)

f x =
  データ
```

Titlecase letters (`Lt` — `ǅ`, `ǈ`, `ǋ`, `ǲ`) are a third case, and Zelkova reads only two.
They may not begin an identifier either, for the same reason, and are ordinary continuation
characters.

```zel expect=ok
module Example exposing (f)

ǅoo = 1

f = ǅoo
```

**Known gap:** that block should be rejected. Today an identifier may begin with any cased
letter, titlecase included, and a titlecase-initial name is classified as *lowercase* — so
`ǅoo` is accepted and names a value. [`docs/tickets/lang-3.md`](../tickets/lang-3.md) tracks
it.

### The underscore is not a letter

`_` may appear inside an identifier but may not begin one. On its own it is the wildcard
pattern, and that is what it stays when it is written in front of a name: `_x` is two tokens,
not one identifier.

This matters more than it sounds like it should, because in a function's parameter list two
tokens are two parameters:

```zel expect=canonical-error:BindingPatternsInvalidLen
module Example exposing (Flag, f)

type Flag
  = On

f : Flag -> Flag
f _x =
  On
```

The annotation promises one argument. `f _x` supplies two patterns — a wildcard and a
variable named `x` — and the mismatch is what the compiler reports. Zelkova has no convention
by which a leading underscore marks a parameter as deliberately unused; write `_` for that.

## Reserved words

Thirteen words are reserved and may never be used as identifiers:

```
as     case   else   exposing   if     import   in
infix  let    module of         then   type
```

`let` and `in` are reserved although the construct that uses them is not implemented; see
[Layout](layout.md#let--in).

Four further words are **soft keywords**. Each is a keyword in exactly one position and an
ordinary identifier everywhere else:

| Word | Keyword in |
|---|---|
| `left`, `right`, `non` | the associativity of an `infix` declaration |
| `javascript` | the header of a [JS interop](js-interop.md) module |

The distinction is deliberate. These four read as ordinary vocabulary — a tree module wants
`left` and `right`, and a project targeting the browser will want `javascript` — and each
appears in a position where nothing else could possibly be meant, so reserving the word
outright would take a useful name and give nothing back.

```zel expect=ok
module Example exposing (left, right, non)

left = 1

right = 2

non = 3
```

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

javascript = 1

f = javascript
```

**Known gap:** that block should be `expect=ok`. `javascript` is reserved outright today,
unlike its three siblings, which is the inconsistency
[`docs/tickets/lang-2.md`](../tickets/lang-2.md) resolves.

`true` and `false` are **not** reserved. Zelkova has no boolean literal syntax: `Bool` is an
ordinary union type and `True` and `False` are its constructors, resolved, imported and
shadowed like any other name.

```zel expect=ok
module Example exposing (Bool, not)

type Bool
  = True
  | False

not : Bool -> Bool
not b =
  case b of
    True ->
      False

    False ->
      True
```

So `true` is available as a variable name:

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

true = 1

f = true
```

**Known gap:** that block should be `expect=ok`. `true` and `false` are currently reserved
words that produce boolean literals, so `Bool` exists twice over — once as the keywords and
once as `Basics`' union type. [`docs/tickets/lang-1.md`](../tickets/lang-1.md) removes the
keywords.

## Literals

### Integers

An integer literal is a run of ASCII digits, or `0x` followed by a run of ASCII hexadecimal
digits. There are no digit separators, and no leading `-`: negation is an operator, described
below.

```zel expect=ok
module Example exposing (count)

count = 42
```

```zel expect=unimplemented
module Example exposing (mask)

mask = 0xFF
```

**Not implemented:** hexadecimal literals are not recognised. `0xFF` is currently read as the
literal `0` applied to a variable named `xFF`, which then fails to resolve — an accident, not
a diagnostic.

The language guarantees that every integer in `-2^31 .. 2^31 - 1` is representable on every
target. Beyond that range the compilation target decides: the JavaScript backend is exact to
`2^53`, and a future WebAssembly backend would use 64-bit two's-complement arithmetic with
the wraparound that implies. A program that stays inside the guaranteed range means the same
thing everywhere; one that does not is making a claim about its target.

### Floats

A float literal is a run of digits, a `.`, and a further run of digits — **at least one digit
on each side** — optionally followed by an exponent: `e` or `E`, an optional `+` or `-`, and
a run of digits. An exponent may also follow a bare run of digits with no `.` at all.

```zel expect=ok
module Example exposing (ratio)

ratio = 3.14
```

```zel expect=unimplemented
module Example exposing (avogadro)

avogadro = 6.022e23
```

**Not implemented:** exponents are not recognised. As with hexadecimal above, `6.022e23`
currently parses as an application of `6.022` to an unresolvable `e23`.

Requiring a digit on each side of the point is what keeps `.` usable as punctuation. Floats
are IEEE 754 double-precision.

```zel expect=ok
module Example exposing (f)

f = 1.
```

**Known gap:** that block should be rejected — `1.` has no digit after the point. The
tokenizer accepts a trailing point today. [`docs/tickets/lang-3.md`](../tickets/lang-3.md)
tracks it, along with the titlecase rule above.

### Characters

A character literal is a single character between single quotes. The character may be written
directly, or as one of these escape sequences:

| Escape | Means |
|---|---|
| `\n` | line feed |
| `\r` | carriage return |
| `\t` | horizontal tab |
| `\\` | backslash |
| `\'` | single quote |
| `\"` | double quote |
| `\u{H…}` | the character with the given hexadecimal code point |

```zel expect=ok
module Example exposing (letter)

letter = 'a'
```

```zel expect=unimplemented
module Example exposing (newline)

newline = '\n'
```

**Not implemented:** no escape sequence is recognised. A character literal must currently hold
exactly one character between the quotes, so `'\n'` is rejected as an unclosed literal.

A character literal holds one Unicode code point. `'\u{1F600}'` is a single character.

### Strings

A string literal is a run of characters between double quotes, using the same escapes as
character literals. It may not contain an unescaped line ending.

```zel expect=unimplemented
module Example exposing (greeting)

greeting = "hello"
```

A **multi-line string** is delimited by `"""` and may contain line endings and unescaped
double quotes. The same escapes apply.

```zel expect=unimplemented
module Example exposing (poem)

poem =
  """
  one
  two
  """
```

**Not implemented:** neither form is recognised; `"` is not a token the tokenizer knows.

## Operators

An operator is a run of one or more of these characters:

```
! # $ % & * + - . / < = > ? @ \ ^ | ~ :
```

Operators are subject to maximal munch like every other token, so `a<=b` is `a`, `<=`, `b`,
and an operator only ends where a character outside that set begins. Two consecutive hyphens
start a comment, so no operator may contain `--`.

Six spellings are punctuation rather than operator names, and may not be declared or
redefined:

| Spelling | Means |
|---|---|
| `=` | binds a declaration to its body |
| `:` | introduces a type annotation |
| `\|` | separates the variants of a `type` declaration |
| `->` | a function type, and a `case` branch |
| `.` | separates the parts of a qualified name |
| `..` | "everything", in an `exposing` list |

Every other spelling is available. An operator has no meaning of its own: it is a name, bound
by an `infix` declaration to an ordinary function, and its precedence and associativity are
declared there rather than built in.

```zel expect=ok
module Example exposing ((|+|), combine)

infix left 6 (|+|) = combine

combine a b =
  a
```

A reserved spelling cannot be given one:

```zel expect=parse-error:UnexpectedToken
module Example exposing ((|), f)

infix left 6 (|) = f

f a b = a
```

Note that `-` is *not* reserved: it is an ordinary operator name, and `Basics` binds it to
subtraction like any other.

### Prefix negation

A `-` written directly before an expression, where no left operand is available, is **prefix
negation**: `-e` means the negation of `e`, and has the type of `e`, which must be a numeric
one. It is not subtraction against an implied zero, and nothing about it is special-cased to
literals — `-x` negates the variable `x`, and `-1` negates the literal `1` rather than being a
literal of its own. A [pattern](patterns.md#literal-patterns) carries a sign differently. Where
a `-` has a left operand and where it does not is
[Expressions](expressions.md#prefix-negation)' subject.

```zel expect=ok
module Example exposing (opposite)

infix left 6 (-) = sub

sub a b =
  a

opposite n =
  -n
```

**Known gap:** prefix negation is currently desugared to `0 - e`, with `0` always an *integer*
literal, so `-3.14` is a subtraction that mixes an `Int` with a `Float`. The block above passes
either way — it pins the syntax, which is unchanged — and
[`docs/tickets/lang-4.md`](../tickets/lang-4.md) tracks the meaning.

## Punctuation

Beyond the six reserved operator spellings above, these characters are tokens in their own
right:

| Token | Used for |
|---|---|
| `(` `)` | grouping, tuples, and naming an operator |
| `,` | separating tuple elements and `exposing` entries |
| `_` | the wildcard pattern |
| `[` `]` | list literals |
| `{` `}` | records |

```zel expect=unimplemented
module Example exposing (f)

f = [1, 2]
```

```zel expect=unimplemented
module Example exposing (f)

f = { a = 1 }
```

**Not implemented:** lists and records are part of the language and neither is implemented.
Brackets are tokenized but no construct consumes them; braces are not tokenized at all. Their
syntax is specified in the chapters on those constructs rather than here — this section
claims only that the characters are spoken for and are not available as operator characters.
Neither of those chapters exists yet: [`SPEC-22`](../tickets/spec-22.md) is the list one and
[`SPEC-21`](../tickets/spec-21.md) the record one.

## Numeric literals the tokenizer cannot represent

Three inputs make the tokenizer panic today rather than report an error: an integer literal
too large for a 64-bit signed integer, a run of digits containing more than one `.`
(`1.2.3`), and a numeric literal continuing into a non-ASCII digit (`1١`). A fourth, an
`infix` precedence above 255, panics in the grammar.

All four are ordinary syntax errors under the rules above and must be reported as such.
[`docs/tickets/bug-12.md`](../tickets/bug-12.md) tracks them. They are described here rather
than shown, because a panic aborts the whole run of `cargo test --test spec` and would take
every other example in this directory with it.
