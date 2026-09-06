# Anchor targets

A fixture standing in for a chapter, holding the header shapes the slug rule has to get
right. Nothing here says anything about Zelkova — it exists so the anchor collector can
be pinned without renaming a real chapter's headers.

## `let … in`

Punctuation vanishes without leaving a separator, so the ellipsis contributes nothing
and each of the two spaces around it leaves a hyphen: this header's anchor is
`let--in`. Two real chapters write that header — `docs/spec/layout.md` and
`docs/spec/expressions.md` — and seven links across six chapters name exactly that
spelling.

## Resolution and `zelkova.lock`

A dot is punctuation too, so this one is `resolution-and-zelkovalock`.

## The annotation and the declaration's parameters

An apostrophe likewise, so `declaration's` contributes `declarations`.

## Two outcomes

The header below repeats this one. GitHub disambiguates the second by appending `-1`.

```toml
# Two outcomes
name = "a comment inside a fenced block is not a header"
```

## Two outcomes

## A line beginning with an inline code span

```` ```zel ```` at the start of a line is an inline code span, not a fence opener:
CommonMark forbids a backtick anywhere in a backtick fence's info string. The header
below is written after that line, and is still a header.

## Still a header
