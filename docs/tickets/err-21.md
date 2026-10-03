# ERR-21 · A four-element tuple type at the front of an annotation is reported as a missing `=>`

**Sizing:** small-to-medium. The production to add is small; what makes it bigger is that LALRPOP may refuse to place it (see *Approach*), and that the answer may need a type-classes design call.

**Location:** `crates/zelkova-syntax/src/parser/grammar.lalrpop` — `ConstrainedType`'s four-or-more-constraints production (`"(" Type "," Type "," Type "," Type ("," Type)* ")"`), `AtomicType`'s tuple productions;
`crates/zelkova-compiler/tests/canonical.rs` — `tuple_type_of_four_is_a_parse_error`.

**Found:** while reviewing the PR for `LANG-71`, which gave a list of four or more constraints a production of its own. Left unfixed there because the diagnostic follows from the approach `LANG-71` and [DEC-24](../decisions/dec-24.md) chose, not from an implementation slip.

**Problem:** a context is parsed as a type, and a list of four or more constraints has its own production because `AtomicType` has no tuple of that size and must not grow one (adding a four-element production to `AtomicType` beside it is now a shift/reduce conflict on `"=>"`, so the build fails). From the fourth `,` on, the parser cannot know whether the `)` will be followed by `=>`, so a too-long tuple type at the *front* of an annotation is only rejected when the `=>` is missing. Before `LANG-71` it was rejected at the fourth `,` expecting `)`. With a scratch package depending on `std/core`:

```zel
f : (Int, Int, Int, Int)
f = 1
```

reports ``unexpected token: `CloseBlock` `` with the caret under the `f` of the next line, and the note that it was expecting `["=>"]`;

```zel
f : (Int, Int, Int, Int) -> Int
f = 1
```

reports ``unexpected token: `Arrow` `` under the `->`, again expecting `=>`. Both tell a user who wrote too long a tuple type to write a constraint arrow, and the first points at a different line. The rule itself still holds: `types.md`'s `quad : (Size, Size, Size, Size) -> Size` block is bare `expect=parse-error` and stays green. Only the diagnostic regressed. A four-element tuple in a non-leading position (`Int -> (Int, Int, Int, Int)`) is still rejected at the fourth `,`.

**Approach:** the ticket does not pick; this has not been probed in a build.
1. A sibling production for the same token run *not* followed by `=>`, with a fallible action (`=>?`) that returns a tuple-arity error at the fourth element. It may not be placeable without a conflict with the `=>` production, since the parser still has to decide at `)`; that is the first thing to try.
2. Rewrite the error after the fact: when the parse fails expecting `=>` and the tokens just consumed are a parenthesised run of four or more types, report a tuple-arity error at the fourth `,` instead. This touches the error path, not the grammar.
3. Accept the diagnostic and improve only its wording. If the answer changes how a context is parsed, it is a type-classes design question (DEC-24) and the ticket should stop and ask rather than decide.

**Acceptance:** for each of the two snippets above, `cargo run -- compile` on a scratch package reports an error whose caret is at or before the fourth `,` of the annotation and whose message does not suggest `=>`; a test in `crates/zelkova-compiler/tests/canonical.rs` pins each, and `tuple_type_of_four_is_a_parse_error` gains a leading-position twin. The `types.md` `expect=parse-error` block stays green, and `cargo test --workspace` passes.
