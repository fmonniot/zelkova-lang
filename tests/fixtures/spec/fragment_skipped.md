# Fixture: `expect=fragment` is never executed

Used by `tests/spec.rs::fragment_block_is_skipped_and_counted`. The block below is
not even syntactically valid Zelkova — if the harness ever ran it, it would fail with
a parse error. A green `Verdict::Fragment` here is only possible if the block was
genuinely skipped.

```zel expect=fragment
this is not valid Zelkova at all !!! ???
```
