# LANG-50 · Field access `r.name` and the accessor `.name` do not parse

**Sizing:** medium. A grammar change with a whitespace-sensitive rule in it, which is the part
worth being careful about.

**Location:** `crates/zelkova-syntax/src/parser/grammar.lalrpop` — `AtomicExpr`, which carries a commented-out
`<expr: AtomicExpr> "." <id: Ident> => Expression::Projection(…)` from before any of this was
specified; `crates/zelkova-syntax/src/parser/mod.rs`'s `ExpressionKind`;
`canonical::Expression::from_parser_expression`.

**Depends on:** `LANG-52`, hard, and landed ([the index](README.md)). A `.` with whitespace before it
is an accessor, so the qualification dot had to stop accepting whitespace before either form could
be told from the other.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decision 3):**
`r.name` reads a field, `.name` on its own is `\r -> r.name`, and whitespace before the `.` is
what separates the two. [Records](../spec/records.md#reading-a-field) is the rule.

**Not implemented:** `.` is consumed only by the qualified-name productions, both of which want an
uppercase identifier to its left. `f r = r.name` is `UnexpectedToken` at the `Dot`, and `f =
.name` is `Error::SpacedDot` at the `SpacedDot`.

**Approach:** two productions. Access is postfix on `AtomicExpr` and binds tighter than
application, so `f r.name` is `f (r.name)` and `r.centre.x` is `(r.centre).x`. The accessor is an
atomic expression of its own, and it begins with the `SpacedDot` token: `"spaced dot" VarIdent`.

**The whitespace rule is the whole difficulty**, because the grammar cannot see whitespace. `f
.name` is an application of `f` to an accessor and `f.name` is an access, and a grammar fed one
token for both cannot tell them apart.

The choice of mechanism is made: `LANG-52` put the left-hand half in the tokenizer.
`consume_operator` in `crates/zelkova-syntax/src/parser/tokenizer.rs` reads a `.` as `Dot` only
when it is written against an operand on its left — an identifier, a literal, `)`, `]` or `}` —
and against the character after it. Every other `.` is `SpacedDot`, which no production consumes,
and that includes a `.` opening an expression after `(`, `[`, `{`, `,`, an operator or a keyword:
`(.name)` begins with `SpacedDot`, as `f .name` does. A `.` opening an expression is an accessor,
so every accessor starts with `SpacedDot` and needs no `Dot` production; the doc comment on
`consume_operator` has the reasons. A scratch `"spaced dot" VarIdent` production in `AtomicExpr`
builds without a conflict.

What is left to this ticket is the rest of the whitespace rule. The accessor's own `.` is written
against its label with no space after it, but `.name` and `. name` are both a `SpacedDot` followed
by a name, so telling them apart means looking at the spans in an action or splitting the token
further; which, is the implementer's to choose. And the access production: a scratch `AtomicExpr
"." VarIdent` reports a local ambiguity against `QualTypeIdent` after an uppercase name, so what
an access may take as its left operand is this ticket's to settle.

**`Just .name` becomes an application.** `LANG-52` landed first and rejects every detached `.`
after an uppercase name, `Widget .size` included, because no accessor exists yet.
Once one does, an uppercase name followed by a detached `.` and an attached label is that name
applied to an accessor — a constructor taking a function, or a canonicalization error where the
name is a module and no constructor. `Widget . size` and `Widget. size` stay parse errors, and
`LANG-52`'s parser test for `Widget .size` moves to the phase that now rejects it.

**A bare accessor has no type of its own.** Records are closed
([Records](../spec/records.md#records-are-closed)), so there is no "any record with a `name`
field" for `.name` to be polymorphic in: it is typed from where it is written and is an error
where nothing fixes the record type. That is the typer's half and belongs to
[`LANG-51`](lang-51.md); this ticket produces the AST node.

**Acceptance:** the field-access and accessor blocks in
[Records](../spec/records.md#reading-a-field) and [The accessor](../spec/records.md#the-accessor)
go red and are retagged, as does the `r.name` block in
[Expressions](../spec/expressions.md#forms-the-compiler-does-not-have), whose **Not implemented:**
paragraph goes with it. All three annotate with a record type, which parses, so they turn on
this ticket.
The unannotated block under
[A use does not decide a record's type](../spec/records.md#a-use-does-not-decide-a-records-type)
needs no brace and turns on this ticket alone: it becomes `expect=ok` under a **Known gap:**
naming [`LANG-51`](lang-51.md), which is what makes it an error. Parser tests assert that `f r.name` parses as one application with an
access argument, that `f .name` parses as an application to an accessor, and that `r.a.b` nests
left.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
