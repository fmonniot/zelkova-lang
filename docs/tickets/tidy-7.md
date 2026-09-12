# TIDY-7 · Four label/diagnostic messages in `Error::Tokenizer`'s match are still capitalized

**Sizing:** small (four string literals, no behaviour change).

**Location:** `src/compiler/parser/error.rs` — the `Error::Tokenizer(err) => { ... }` arm of
`Error::diagnostic`, specifically the `CharNotClosedError(None)`, `IndentationError`,
`TabError` and `UnrecognizedToken` sub-arms.

**Problem:** `BUG-6` and `BUG-7` established a lowercase style for this match's diagnostic and
label messages — `"char sequence opened but never closed"`, `"we were expecting a single quote
here"`, `"for the opening quote here"`, `"this string literal could not be read"`, `"the string
starts here"`, `"this unicode escape sequence could not be read"`, `"the escape sequence starts
here"` are all lowercase. Four sub-arms of the same match were left untouched by `BUG-7`
(reviewer thread on PR #190, comment id 3995110445) and still capitalize their first word:

1. `TokenizerErrorType::CharNotClosedError(None)`'s label message reads `"The char is declared
   here but not closed"`.
2. `TokenizerErrorType::IndentationError`'s diagnostic message reads `"Invalid indentation
   level"`. (Its label carries no `.with_message(...)` at all, so there is nothing to lowercase
   there.)
3. `TokenizerErrorType::TabError`'s diagnostic message reads `"Tab found"`. (Same as above: its
   label has no message.)
4. `TokenizerErrorType::UnrecognizedToken { tok }`'s diagnostic message reads `"Unexpected token
   found"`, and its label message reads `"Unrecognized token {} found"` — both capitalized.

The `.with_notes(...)` strings in these same arms (e.g. `"Zelkova use exclusively two spaces to
denote indentation..."`) are ordinary prose sentences, correctly capitalized, and are not part
of this inconsistency — leave them as they are. The `StringError` and `UnicodeError` sub-arms
are also already lowercase and consistent; leave them alone too.

**Fix:** lowercase the first word of the four strings above so every message and label in this
match reads as a clause continuing "error: ..." / "here, ...", matching the style the other
arms already use:

- `"The char is declared here but not closed"` → `"the char is declared here but not closed"`
- `"Invalid indentation level"` → `"invalid indentation level"`
- `"Tab found"` → `"tab found"`
- `"Unexpected token found"` → `"unexpected token found"`
- `"Unrecognized token {} found"` → `"unrecognized token {} found"`

While in the `UnrecognizedToken` arm, note it builds a fresh `Diagnostic::error()` instead of
reusing the arm's own `diag` binding the way every sibling arm does — harmless today since both
produce the same value, but worth folding into `diag.with_message(...)` if it falls out for
free alongside the wording fix. This ticket is about the casing, not about that refactor; don't
go looking for other cleanup in the same match beyond what's listed here.

**Acceptance:** the five string literals above read lowercase-first in
`src/compiler/parser/error.rs`, `cargo build` and `cargo test --workspace` stay green (no test
currently asserts on this arm's exact text — a `grep -n` for the old capitalized strings in the
file returns nothing after the change), and `cargo fmt --all --check` / `cargo clippy
--workspace --all-features` stay clean.
