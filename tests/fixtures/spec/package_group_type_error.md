# A type error inside a `package=` group

`Widget` fails the type checker and `Main` imports it. `Main` still canonicalizes,
because the interface `Widget` published was built out of its canonical module and the
typer never touches it — a declared signature is what an interface carries, and
canonicalization is what validated it.

```zel expect=type-error:UnificationFailed package=fixture
module Widget exposing (Size, small, broken)

type Size
  = Small

small : Size
small = Small

broken : Size
broken = 1
```

```zel expect=ok package=fixture
module Main exposing (x)

import Widget

x : Widget.Size
x = Widget.small
```
