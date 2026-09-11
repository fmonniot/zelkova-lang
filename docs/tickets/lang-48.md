# LANG-48 · There is no record production, so a record type, a record and an update do not parse

**Sizing:** large. A grammar change, so `CLAUDE.md`'s *A grammar change is never a one-file
change* applies — `grammar.lalrpop`, the `parser` AST and the `canonical` conversion land
together — and it adds a form to three of the grammar's languages at once.

**Location:** `src/compiler/parser/grammar.lalrpop`; `src/compiler/parser/mod.rs`'s `TypeKind`
and `ExpressionKind`; `canonical::Type::from_parser_type` and
`canonical::Expression::from_parser_expression` in `src/compiler/canonical/`.

**Depends on:** [`LANG-47`](lang-47.md), hard. There is no brace token to consume.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decisions 1, 2 and
4):** a record type is `{ label : Type, … }`, a record is `{ label = expr, … }`, an update is
`{ expr | label = expr, … }`, each has at least one field, no field is repeated and no trailing
comma is permitted. [Records](../spec/records.md) is the rule.

**Not implemented:** no production consumes a brace in any position, so every record example in
[Records](../spec/records.md) is rejected in the tokenizer today.

**Approach:** three productions, all delimited on both sides and therefore all at the atomic
level — a record needs no parenthesising in argument position, the same argument
[`LANG-44`](lang-44.md) makes for a list literal.

- **`AtomicType`** gains `{ label : Type, … }`, one or more fields, no trailing comma.
- **`AtomicExpr`** gains `{ label = Expr, … }` and `{ Expr | label = Expr, … }`. The update's
  left operand is a full expression rather than a name, so LALRPOP resolves the two by the token
  after the first component — `=` against `|` — which needs the two written as one production
  with an optional update head, or the grammar reports a conflict.

`Token::Pipe` already exists for variant lists and is what the update reads.

**Both ASTs gain a record form, and it survives canonicalization.** Unlike a list, a record is
not sugar over constructors and there is nothing to desugar into: `TypeKind::Record` and
`ExpressionKind::Record`/`Update` reach `canonical::Module` and go on to the typer
([`LANG-51`](lang-51.md)). A record type is a **set** of fields — order-insensitive, per
[Records](../spec/records.md#a-record-type-is-a-set-of-fields) — so canonicalization is where the
field list becomes an order-independent representation and where a repeated label is reported. A
new `canonical::Error` variant carries that, with the caret under the repeated label rather than
under the record.

**Acceptance:** every `expect=unimplemented` block in [Records](../spec/records.md) that shows a
record type, a record or an update goes red — that tag's whole job — and is retagged `expect=ok`,
except those [Records](../spec/records.md#a-record-type-is-a-set-of-fields) marks as rejected by
the typer, which become `expect=ok` carrying a **Known gap:** naming [`LANG-51`](lang-51.md).
A repeated label is a `canonical-error:` block. The trailing-comma block in
[Building a record](../spec/records.md#building-a-record) stays `expect=parse-error` and its
**Not implemented:** paragraph goes, because after this it is rejected at the comma it
illustrates. Parser tests assert the `TypeKind`/`ExpressionKind` shape, not only that it parsed.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
