# Fixture: `expect=parse-error:<reason>` failing for a different reason

Used by `tests/spec.rs::parse_error_wrong_reason_is_a_failure`. The block below is
indented by three spaces, which the tokenizer rejects as `IndentationError`, but it
pins `TabError` instead — on purpose. The harness must catch that mismatch rather than
accept "the parser rejected it somehow", because the whole point of pinning a reason is
to notice when the diagnostic changes.

```zel expect=parse-error:TabError
module Test exposing (f)

f x =
   1
```
