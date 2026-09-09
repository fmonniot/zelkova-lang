# LANG-49 · There is no record pattern production

**Sizing:** medium. A grammar change, so `grammar.lalrpop`, the `parser` AST's `PatternKind` and
the canonical conversion land together.

**Location:** `src/compiler/parser/grammar.lalrpop`'s pattern productions;
`src/compiler/parser/mod.rs`'s `PatternKind`; `canonical::Pattern::from_parser_pattern`.

**Depends on:** [`LANG-47`](lang-47.md), hard — there is no brace token.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decision 5):** a
record pattern is `{ label = pattern, … }`, `{ label }` is shorthand for `{ label = label }`, and
a pattern names a **subset** of the record's fields.
[Records](../spec/records.md#record-patterns) is the rule, and
[Patterns](../spec/patterns.md#record-patterns) states the form beside the others.

**Not implemented:** the pattern grammar has no brace production, so every record-pattern block in
[Records](../spec/records.md#record-patterns) is rejected in the tokenizer.

**Approach:** one production at the atomic-pattern level, one or more entries, no trailing comma.
Each entry is `"lo_ident" "=" Pattern`, or a bare `"lo_ident"` desugaring to
`label = Variable(label)` — do the desugar in the grammar action so `PatternKind` carries one
shape and no later phase learns about the shorthand.

Sub-patterns are whole patterns, so nesting falls out with no extra work; a repeated label is
reported the way [`LANG-48`](lang-48.md) reports one in a record expression.

**Refutability is the part with a real consequence.** A record pattern is refutable exactly when
one of its sub-patterns is, so `{ x }` and `{ x, y }` are irrefutable and legal in a parameter
and a `let` binding, while `{ x = 0 }` is not. Whatever decides refutability for a tuple pattern
today is where this goes — a record pattern is not irrefutable by virtue of being a record, which
is the tempting shortcut.

**Acceptance:** every `expect=unimplemented` block in
[Records](../spec/records.md#record-patterns) goes red and is retagged; the same for
[Patterns](../spec/patterns.md#record-patterns)' worked examples, which are written when this
lands. A parser test asserts the desugared `PatternKind` for `{ x }` rather than only that it
parsed, and a canonicalization test asserts the bindings a nested record pattern produces.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
