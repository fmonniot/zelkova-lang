# Fixture: `expect=unimplemented` on a block that actually compiles

Used by `tests/spec.rs::unimplemented_block_that_compiles_is_a_failure`. The tag's
whole purpose is to go red the day a feature lands, so a block that parses and
canonicalizes cleanly must fail its `expect=unimplemented` tag rather than pass.

```zel expect=unimplemented
module Test exposing (..)
x = 42
```
