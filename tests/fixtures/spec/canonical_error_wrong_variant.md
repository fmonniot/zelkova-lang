# Fixture: `expect=canonical-error:X` failing with a different variant

Used by `tests/spec.rs::canonical_error_wrong_variant_is_a_failure`. `y` is an
undefined *value*, which canonicalization reports as `VariableNotFound` — this block
pins `VariantNotFound` (an undefined *constructor*) instead, on purpose, so the
harness must catch the mismatch rather than accept "canonicalization failed somehow".

```zel expect=canonical-error:VariantNotFound
module Test exposing (..)
x = y
```
