# Fixture: a `package=` group where an `expect=ok` block imports from a block that fails

Used by `crates/zelkova-compiler/tests/spec.rs::an_ok_block_importing_a_failed_block_is_a_failure`.
`Failing` declares a union canonicalization rejects, so its interface is incomplete and the
errors `Importer` would raise against its names are dropped. `Importer` has no error of its own
and was still never checked whole, so `expect=ok` must not hold for it.

```zel expect=canonical-error:InvalidVariant package=fixture
module Failing exposing (T(..))

type T = MkT | (T, T)
```

```zel expect=ok package=fixture
module Importer exposing (b)

import Failing exposing (T(..))

b : Failing.T
b = MkT
```
