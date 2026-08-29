# Fixture: a `package=` group where one module fails its own `expect=ok`

Used by `tests/spec.rs::package_group_judges_each_block_separately`. The two blocks are
one package: `Widget` compiles, and `Main` imports a value `Widget` does not declare, so
it must be reported as a failure on its own line while `Widget` still passes. A harness
that judged a group as a single unit could not tell the two apart.

```zel expect=ok package=fixture
module Widget exposing (Size, label)

type Size
  = Small

label : Size
label = Small
```

```zel expect=ok package=fixture
module Main exposing (x)

import Widget exposing (missing)

x = 1
```
