# LANG-50 · Field access `r.name` and the accessor `.name` do not parse

**Sizing:** medium. A grammar change with a whitespace-sensitive rule in it, which is the part
worth being careful about.

**Location:** `src/compiler/parser/grammar.lalrpop` — `AtomicExpr`, which carries a commented-out
`<expr: AtomicExpr> "." <id: Ident> => Expression::Projection(…)` from before any of this was
specified; `src/compiler/parser/mod.rs`'s `ExpressionKind`;
`canonical::Expression::from_parser_expression`.

**Depends on:** [`LANG-52`](lang-52.md), hard. A `.` with whitespace before it is an accessor, so
the qualification dot has to stop accepting whitespace before either form can be told from the
other.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decision 3):**
`r.name` reads a field, `.name` on its own is `\r -> r.name`, and whitespace before the `.` is
what separates the two. [Records](../spec/records.md#reading-a-field) is the rule.

**Not implemented:** `.` is consumed only by the qualified-name productions, both of which want an
uppercase identifier to its left. `f r = r.name` is `UnexpectedToken` at the `Dot`, and so is
`f = .name`.

**Approach:** two productions. Access is postfix on `AtomicExpr` and binds tighter than
application, so `f r.name` is `f (r.name)` and `r.centre.x` is `(r.centre).x`. The accessor is an
atomic expression of its own: `"." "lo_ident"`.

**The whitespace rule is the whole difficulty**, because the grammar cannot see whitespace. `f
.name` is an application of `f` to an accessor and `f.name` is an access, and the two token
streams are identical. Two ways out, and the choice is the implementer's:

- **Adjacency in the grammar action**, comparing the `@R` of the left operand against the `@L` of
  the `Dot`. Cheap, and keeps one token.
- **Two tokens from the tokenizer** — a `Dot` that was written against the previous token and one
  that was not — which moves the rule to where the whitespace actually is and makes the grammar
  unambiguous without position arithmetic.

A `.` opening an expression is an accessor under either. The accessor's own `.` is written against
its label with no space after it, which the same mechanism decides.

**A bare accessor has no type of its own.** Records are closed
([Records](../spec/records.md#records-are-closed)), so there is no "any record with a `name`
field" for `.name` to be polymorphic in: it is typed from where it is written and is an error
where nothing fixes the record type. That is the typer's half and belongs to
[`LANG-51`](lang-51.md); this ticket produces the AST node.

**Acceptance:** the field-access and accessor blocks in
[Records](../spec/records.md#reading-a-field) and [The accessor](../spec/records.md#the-accessor)
go red and are retagged, as does the `r.name` block in
[Expressions](../spec/expressions.md#forms-the-compiler-does-not-have), whose **Not implemented:**
paragraph goes with it. Parser tests assert that `f r.name` parses as one application with an
access argument, that `f .name` parses as an application to an accessor, and that `r.a.b` nests
left.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
