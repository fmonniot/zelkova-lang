# LANG-49 · There is no record pattern production

**Sizing:** medium. A grammar change, so `grammar.lalrpop`, the `parser` AST's `PatternKind` and
the canonical conversion land together.

**Location:** `crates/zelkova-syntax/src/parser/grammar.lalrpop`'s pattern productions;
`crates/zelkova-syntax/src/parser/mod.rs`'s `PatternKind`; `canonical::Pattern::from_parser_pattern`.

**Depends on:** `LANG-16` (closed), hard for the acceptance below: the first record-pattern
block in [Records](../spec/records.md#record-patterns) and the one in
[Patterns](../spec/patterns.md#record-patterns) both write `{ taken = Celsius, … }`, a
constructor as a sub-pattern, which `Pattern` admits.

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
reported as `canonical::Error::RepeatedLabel`, the way one in a record expression is.

**Refutability needs no code here.** A record pattern is refutable exactly when one of its
sub-patterns is, so `{ x }` and `{ x, y }` can never fail and `{ x = 0 }` can. Nothing in the
compiler asks that question today: a parameter and a `case` branch both accept both kinds, and
what the language requires is that the patterns of a position cover the type, which is
[`LANG-19`](lang-19.md)'s stub. So this ticket writes no check. What it must not do is build
one in that treats a record pattern as irrefutable for being a record.

**Acceptance:** every `expect=unimplemented` block in
[Records](../spec/records.md#record-patterns) goes red and is retagged; the same for the block in
[Patterns](../spec/patterns.md#record-patterns), whose **Not implemented:** paragraph goes. Every
one of those blocks annotates with a record type, which parses. Typing a record pattern is not this ticket's: it is [`LANG-84`](lang-84.md)'s, and
until then the typer leaves a declaration holding one unchecked. A parser test asserts the
desugared `PatternKind` for `{ x }` rather than only that it parsed, and a canonicalization test
asserts the bindings a nested record pattern produces.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
