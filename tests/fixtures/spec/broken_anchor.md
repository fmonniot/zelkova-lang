# Broken anchor

A fixture holding one link of every shape the cross-reference check has to judge, so a
red run can be produced without breaking a real chapter.

## A header this file has

Both of these resolve: [its own section](#a-header-this-file-has) and
[a section of the other fixture](anchor_targets.md#let--in).

Neither of these does: [a section this file does not have](#no-such-section) and
[a section the other fixture does not have](anchor_targets.md#no-such-header).

Nor does this: [a chapter that was never written](no_such_chapter.md).

A link inside a fenced block is not a link, and is not checked:

```markdown
[not a link](no_such_chapter.md#no-such-anchor)
```

## After an inline code span

```` ```zel ```` opens no fence, so the link that follows it is still a link and still
has to land: [a section this file does not have](#neither-does-this-one).
