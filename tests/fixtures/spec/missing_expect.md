# Fixture: a `zel` block with no `expect=`

Used by `tests/spec.rs::block_with_no_expect_is_a_hard_failure`. The module below is
otherwise perfectly fine — the point is that the harness must reject the block for
its missing tag before it ever gets to compiling anything.

```zel
module Test exposing (..)
x = 42
```
