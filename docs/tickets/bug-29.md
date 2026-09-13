# BUG-29 · A top-level declaration whose first token is not at column 1 fails to parse

**Severity:** medium (wrong behaviour under normal use — the layout pass rejects otherwise
valid, correctly 2-space-indented source for no reason connected to its indentation).

**Location:** `src/compiler/parser/layout.rs` — `handle_next_token`'s handling of
`Context::TopLevelDeclaration`: the two `self.contexts.stack.last_mut()` / `self.contexts.last()`
`None` arms that push `Offside { context: Context::TopLevelDeclaration, indent: start.column,
line: start.line }` when the context stack is empty, and the implicit-close / `min_indent`
check that follows.

**Problem:** a top-level declaration's `TopLevelDeclaration` context is opened with `indent` set
to the **column of whichever token happens to be the first one seen** for it, not to a fixed
column 1. In the ordinary case that token is at column 1 (nothing precedes it on its line), so
this has gone unnoticed. But nothing enforces that assumption, and once the first token sits at
any other column the pass breaks. Reproduced with three unrelated ways of putting the first
token somewhere other than column 1 — none involving a corner case, just an otherwise-ordinary
declaration:

```
module Example exposing (f)

  f = 1
```

(the whole declaration indented by 2, correct indentation, no comment involved at all) fails
identically to:

```
module Example exposing (f)

{- a note -} f = 1
```

and to the same source with the body moved to a second line:

```
module Example exposing (f)

{- a note -} f =
  1
```

All three currently fail the same way — `parser::parse` returns `Err(UnexpectedToken { token:
LowerIdentifier("f"), expected: ["close block"] })` — confirmed against the real
tokenizer → layout → grammar pipeline (`src/compiler/parser/mod.rs`'s `parse`), not just the
tokenizer in isolation. The token blamed is the declaration's own first real token, reprocessed
by the layout pass itself: `handle_next_token`'s `None` arm pushes the new
`TopLevelDeclaration` context, emits a synthetic `OpenBlock`, and pushes the real token onto
`reprocess_tokens` to be read again — and something in the indent/min-indent bookkeeping that
follows treats that replayed token as a violation of the very context it just opened for it,
so the grammar sees an unexpected `f` where it expected the context to close.

**Found while working [BUG-13](README.md):** that ticket's own text, and
`docs/spec/lexical-structure.md`'s Comments section, expected `{- a note -} f =\n  1` to become
`expect=ok` once BUG-13's tokenizer fixes landed. It does not — confirmed by reproducing the
identical failure with no comment anywhere in the source (the plain 2-space-indented example
above). This is a distinct, pre-existing defect in `layout.rs`'s top-level-declaration handling,
unrelated to comments, and BUG-13 was scoped to the tokenizer's comment handling only, so it was
left unfixed there. `docs/spec/lexical-structure.md`'s comment block for `{- a note -} f =\n
1` keeps its current `expect=parse-error:UnexpectedToken` tag and cites this ticket in its
**Known gap:** paragraph instead of `bug-13`.

**Fix:** not decided here. Two shapes are visible from the location above, and this ticket does
not pick between them:

1. `TopLevelDeclaration`'s `indent` could be fixed at column 1 unconditionally, rejecting any
   top-level declaration whose first token is not there — matching Elm's actual rule, if that is
   the intended rule Zelkova wants (nothing in `docs/spec/` states it either way today).
2. The context's `indent` could keep tracking wherever the first token actually sits (as now),
   with the real defect being downstream in how the implicit-close / min-indent check treats the
   token that opened the context — in which case the fix is somewhere in `handle_next_token`'s
   reprocessing logic, not in what `indent` is set to.

Whichever is chosen, if it is (1), `docs/spec/lexical-structure.md`'s layout expectations should
be checked for a rule this pins down that the spec does not currently state.

**Acceptance:** a top-level declaration whose first token is not at column 1 either parses
successfully (if the fix is shape 2, or a variant of shape 1 that still allows this) or is
rejected with a diagnostic that actually names the indentation as the problem, rather than an
`UnexpectedToken` blaming the declaration's own name (if the fix is shape 1). A regression test
at the `parser::parse` (tokenizer + layout + grammar) level — not the tokenizer alone — pins
whichever behaviour is chosen, verified to fail first against today's `layout.rs`. Once this
lands, revisit `docs/spec/lexical-structure.md`'s `{- a note -} f =\n  1` block and retag it
`expect=ok` if the chosen fix makes it valid.
