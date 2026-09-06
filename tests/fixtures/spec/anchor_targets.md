# Anchor targets

A fixture standing in for a chapter, holding the header shapes the slug rule has to get
right. Nothing here says anything about Zelkova — it exists so the anchor collector can
be pinned without renaming a real chapter's headers.

## `let … in`

Punctuation vanishes without leaving a separator, so the ellipsis contributes nothing
and each of the two spaces around it leaves a hyphen: this header's anchor is
`let--in`. Four chapters link to exactly that spelling in `docs/spec/layout.md` and
`docs/spec/expressions.md`.

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
