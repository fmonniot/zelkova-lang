# BUG-34 · A failed sub-pass in `canonicalize` reports as if it found nothing, cascading into spurious errors from every later pass that depended on it

**Severity:** low (the real, root-cause error is always still present and correctly reported;
the defect is noise stacked beside it, not a miscompile or a dropped diagnostic).

**Location:** `src/compiler/canonical/mod.rs` — the five `<pass>(...).unwrap_or_else(|err| {
errors.extend(err); HashMap::new() })` sites inside `canonicalize`'s module body (line numbers
drift; found via `grep -n "unwrap_or_else(|err| {" src/compiler/canonical/mod.rs`, currently):
`do_infixes` (~1456), `do_types` (~1461), `do_values` (~1474) and `do_exports` (~1484) in the
regular-module branch, and a `crate::utils::collect_accumulate(iter)` call (~1446) building
`values` in the `module javascript` facade branch. `src/utils.rs` — `collect_accumulate`, whose
signature is the reason none of the five can currently do better.

**Problem:** each of the five sites swallows a sub-pass's `Vec<Error>` into the shared `errors`
accumulator, then substitutes an **empty** `HashMap` and lets canonicalization continue as if
the pass had produced nothing at all — not the partial map of whichever declarations it *did*
resolve successfully. Every later pass in `canonicalize` that looks something up in that empty
map reports its own error for every entry it expected to find, alongside the one real error
that caused the map to be empty.

This is possible because `collect_accumulate` (`src/utils.rs`) itself has no partial-success
path: on any `Err` it returns only `Err(Vec<E>)`, discarding every `Ok` item the iterator
already produced. There is no way for a caller to ask `do_types` (or `do_infixes`, `do_values`)
for "everything that resolved, plus these errors" — the function's return type does not have a
slot for both.

Two concrete reproductions, run directly against `canonicalize_standalone`/
`canonicalize_with_interfaces` (see `tests/support/mod.rs`) and observed by printing the
returned `Vec<canonical::Error>`:

1. A single mistyped variant, sitting beside an otherwise-correct sibling `type`:

   ```zel
   module Example exposing (Size, Pair)

   type Size
     = Small

   type Pair
     = (Size, Size)
   ```

   produces three errors: `InvalidVariant(Tuple, ..)` on `Pair`'s tuple variant (the real
   error), then `ExportNotFound(Name("Size"), UnionPrivate, ..)` and
   `ExportNotFound(Name("Pair"), UnionPrivate, ..)`. Both `ExportNotFound`s are spurious —
   `Size` and `Pair` are both declared correctly — because `do_types` failed on `Pair` alone,
   returned `HashMap::new()` instead of `{Size: ...}`, and `do_exports` then found neither name
   in the (empty) type environment.

2. A pre-existing trigger, unrelated to variants or `InvalidVariant`, proving the defect
   predates [`BUG-18`](../tickets/README.md) and is not specific to its diagnostic: an imported
   type applied at the wrong arity inside a variant argument —

   ```zel
   module Example exposing (B)

   import Maybe exposing (Maybe)

   type B
     = MkB Maybe
   ```

   (against a `Maybe` interface of arity 1) produces `TypeArityMismatch(Name("Maybe"), 1, 0,
   ..)` (the real error) followed by a spurious `ExportNotFound(Name("B"), UnionPrivate, ..)`,
   same mechanism as case 1.

[`BUG-18`](../tickets/README.md)'s fix makes case 1's shape newly reachable by the single most
common bad input a `type` declaration can have — a mistyped or otherwise-invalid variant — where
before it required an import at the wrong arity. The mechanism itself is not new or worsened by
that PR; only how easily a user hits it is.

**Fix:** not decided, and this ticket does not pick. The shape of the fix depends on a design
choice `collect_accumulate` — and by extension every one of the four `do_*` functions built on
it — would need to make:

- Give `collect_accumulate` (and `do_infixes`/`do_types`/`do_values`/`do_exports`) a return
  type that carries both the partial `R` and the `Vec<E>` on failure, e.g. `Result<R, (R,
  Vec<E>)>` or a dedicated struct, so a caller in `canonicalize` can `.unwrap_or_else` into the
  partial map instead of an empty one. This changes a project-wide helper's signature and every
  call site that uses it — check for other callers of `collect_accumulate` beyond
  `canonical/mod.rs` before assuming the blast radius is limited to the five sites above.
- Or, narrower: leave `collect_accumulate` alone and give each of the four `do_*` functions its
  own partial-result path, duplicating the accumulation logic `collect_accumulate` currently
  centralises.

Either way, the four downstream sites (`do_types`, `do_infixes`, `do_values`, `do_exports`) need
their own review once partial results are available, since each may have its own assumptions
about a name being absent meaning "never declared" versus "declared, but its own resolution
failed" — `do_exports`'s `ExportNotFound` is the one this ticket demonstrates, but the other
three deserve the same check before trusting a partial map blindly.

**Acceptance:** case 1 above raises exactly one error (`InvalidVariant(Tuple, ..)`) rather than
three, in a canonicalization test in `tests/compiler/canonical.rs` — mutation-checked in the
usual way (reverting the fix must turn the test red, not just newly-written). A second test
covers case 2 the same way, raising only `TypeArityMismatch`. `cargo test --workspace` stays
green, and `cargo run` still prints `parsed 8 modules` with all eight checked.

**Related:** found during review of [`BUG-18`](../tickets/README.md)'s PR (#201), which made
case 1's shape reachable by a single mistyped constructor name rather than requiring an import
at the wrong arity. Following the precedent of [`BUG-31`](bug-31.md) and [`BUG-32`](bug-32.md),
filed as follow-ups out of closing `BUG-8` and `BUG-14`, rather than folded into that PR: the
cascade is a genuinely separate, pre-existing defect and fixing it was out of that PR's scope.
