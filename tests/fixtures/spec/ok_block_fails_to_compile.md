# Fixture: an `expect=ok` block that does not compile

Used by `tests/spec.rs::ok_block_that_fails_to_compile_is_a_failure`. `y` is never
declared, so this fails canonicalization — `expect=ok` must not let that slide.

```zel expect=ok
module Test exposing (..)
x = y
```
