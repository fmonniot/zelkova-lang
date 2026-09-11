# A block the type checker rejects

`double` is annotated as taking and returning a `Size`, and its body hands back the
integer literal it was given. Nothing here is a parse or a canonicalization problem:
every name resolves, the annotation is well-formed, and the module canonicalizes
cleanly. Only the type checker can tell that `Size` and an integer are not the same
thing.

```zel expect=type-error:UnificationFailed
module Example exposing (Size, double)

type Size
  = Small

double : Size -> Size
double s =
  1
```
