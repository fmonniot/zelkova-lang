# Fixture: an unrecognised `expect=` value

Used by `tests/spec.rs::unrecognised_expect_value_is_a_hard_failure`. `bogus` is not
in the vocabulary — the harness must reject it rather than guess.

```zel expect=bogus
module Test exposing (..)
x = 42
```
