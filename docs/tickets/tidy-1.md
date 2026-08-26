# TIDY-1 · Make `Name`'s inner `String` private

**Sizing:** small-to-medium — trivial in principle, but `Name(...)` is constructed positionally
in the grammar and in most test files, so the diff is wide.

**Location:** `src/compiler/name.rs` — `pub struct Name(pub String)`;
`src/compiler/parser/grammar.lalrpop` — seven construction sites in the `Name` productions
around the identifier and operator terminals; `src/compiler/dependencies.rs` — three in its
test helpers; `tests/compiler/parser/*.rs` and `tests/pipeline.rs` — many.

**Problem:** `Name(pub String)` publishes its representation. The module's own doc comment says
what that costs:

> In the future, if performance requires it, this module will probably also host the interner
> for qualified and unqualified names.

Interning is incompatible with a public `String` field: every `name.0` and every `Name(s)`
becomes a compile error the day it lands, in files that have nothing to do with names. The
methods that should be the interface — `qualify_with`, `qualify_with_name`, `starts_with`,
`to_qual`, `From<&str>`, `Display` — already exist and already cover the real uses.

This is a hygiene ticket, not a bug. Nothing is broken today; the point is to make the *next*
change cheap, and to do it while the blast radius is a few dozen lines instead of a few
hundred.

**Approach:**

1. Change the field to private: `pub struct Name(String);`.
2. Add whatever accessor the remaining call sites genuinely need. Prefer `as_str(&self) -> &str`
   over exposing the `String`; add `Name::new<S: Into<String>>(s: S)` for construction. Note
   `From<&str> for Name` already exists — most sites should use `"foo".into()` and need no new
   API at all.
3. Fix the fallout. Inside `name.rs` itself the field stays reachable, so `Name`'s own methods
   need no change. The grammar's `Name(<>)` productions become `Name::new(<>)`; the test files
   become `"Int".into()` where the type is inferable.
4. Do the same audit for `QualName` while in the file — its fields are already private, so
   check nothing has grown a `pub` since.

The `TODO`s already in `name.rs` (`Return QualName?`, `Rename to qualify_with_str`, `Remove the
Option`, `tests` on `starts_with`) are adjacent and tempting. **Leave them.** They are separate
decisions and folding them in makes this diff unreviewable.

**Acceptance:** `grep -n 'pub String' src/compiler/name.rs` returns nothing, no file outside
`src/compiler/name.rs` constructs `Name` positionally or reads `.0` from one, and `cargo test`
passes. No behaviour change, so no new tests — but if step 2 added an accessor, it needs one
unit test in `name.rs`'s existing `mod tests`.
