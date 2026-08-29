# BUG-11 · The `Tokenizer` never terminates on a tab outside leading whitespace

**Severity:** low. `parser::parse` is not affected — `Layout` fuses on the first `Err`, so the
error is seen once and the pipeline stops — which is the only reason this is not the same
incident as BUG-4. Any other consumer of `Tokenizer` gets the identical error forever.

**Location:** `src/compiler/parser/tokenizer.rs:637` — the `'\t'` arm of the main token loop in
`process_next_tokens`. Its fixed sibling is the `Some('\t')` arm of `handle_indentation`, around
line 420.

**Found:** during review of the PR that landed `docs/spec/layout.md`, while checking the
chapter's claims about tabs against the compiler.

**Problem:** the arm returns without consuming the tab and without advancing the position:

```rust
'\t' => {
    return Err(TokenizerError::new(
        self.position.absolute,
        self.position.absolute + '\t'.len_utf8() as u32,
        TokenizerErrorType::TabError,
    ))
}
```

`Tokenizer::next` calls `process_next_tokens` and has no latch: on the next poll the lookahead
still holds the same tab, the same arm runs, and the same `TabError` — byte-identical span
included — comes back. Verified by polling `make_tokenizer("f x =\n  1\t+ 2\n")` past the error:
every subsequent item is `TabError` at the same `BytePos`, indefinitely.

This is exactly the shape BUG-5 fixed in `handle_indentation`, which now reads the span, calls
`next_char()`, and clears `at_line_start` before returning — and carries a comment saying why.
The main-loop arm was not part of that fix. It is also the class of defect `CLAUDE.md`'s
standing invariant *A `Result`-yielding iterator must advance or stop — never repeat one error*
exists to prevent; fully draining the one in BUG-4 consumed ~20GB before the OS killed it.

**Approach:** mirror the sibling arm. Read `self.position.absolute` into `start`, call
`self.next_char()`, read the new `self.position.absolute` into `end`, and return the error with
that span. The span moves from a computed `absolute + 1` to two observed positions, which is the
same value for a tab but stops depending on the assumption. `at_line_start` needs no touching
here: this arm only runs after `handle_indentation` has already cleared it, which is what makes
it the main-loop arm rather than the indentation one — say so in a comment, since the sibling
arm's comment will otherwise read as if it applied to both.

Note that this makes the tokenizer *advance* past the error, not stop. It does not make the
tokenizer stop at the first error the way `Layout` does; the invariant offers both, and matching
the sibling arm keeps the two tab sites consistent.

**Acceptance:** a test alongside the BUG-5 one in `tokenizer.rs`'s `mod tests` — the existing
one is the model — that drains a tokenizer over a source with a tab *after* the first
non-whitespace character of a line and asserts no two consecutive items are the same
`TabError`, and that exactly one is raised for one tab. Neutralise the fix and confirm the test
goes red before keeping it.

`docs/spec/layout.md`'s *Tabs are legal only inside a comment* section already pins the
user-visible behaviour through `parser::parse` and is unaffected: the rule and the error the
reader sees are the same before and after this fix.
