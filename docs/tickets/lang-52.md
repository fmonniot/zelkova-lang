# LANG-52 · Whitespace around a qualification dot is accepted, and records need it not to be

**Sizing:** small. One rule, in the tokenizer or in a grammar action, plus the tests.

**Location:** `src/compiler/parser/grammar.lalrpop`'s `QualVarIdent` and `QualTypeIdent`, which
read `"up_ident" "." QualVarIdent` with no adjacency requirement; or
`src/compiler/parser/tokenizer.rs`'s `consume_operator`, which is where the whitespace is still
visible.

**Decided (`SPEC-21`, by the language owner; [`DEC-8`](../decisions/dec-8.md) decision 3):** a `.`
written against the expression to its left is an access or a qualification; a `.` with whitespace
before it is a **record accessor**. [Records](../spec/records.md#whitespace-before-a--decides-which-form-it-is)
is the rule.

**Known gap:** `Widget . size` parses and canonicalizes today as the qualified name
`Widget.size`, and so does `Widget .size` and `Widget. size`. The language reads all three as
`Widget` applied to an accessor, which is an application of a module name and an error. The
package block in
[Records](../spec/records.md#whitespace-before-a--decides-which-form-it-is) is tagged `expect=ok`
and shows exactly that: it is green, and it should not be.

**Why this is a ticket at all.** The spacing is meaningless today, so nothing was wrong with
accepting it. Records give the spelling a second meaning, and the two cannot both be right:
`f .name` has to be an application to an accessor, and there is no reading of the language under
which `Widget . size` is a qualified name while `f .name` is not.

**Approach:** require the `.` of a qualified name to be adjacent to the identifiers on both
sides. If [`LANG-50`](lang-50.md) is being done at the same time, do it there — that ticket has
to distinguish an attached `.` from a detached one anyway, and one mechanism serves both. Doing
this one first, on its own, is also fine and is the cheaper order: it is a rejection with no new
form behind it.

The diagnostic matters more than usual, because the source looks reasonable. `Widget . size`
reports that a qualified name is written with no spaces around its `.`, and not that `Widget` is
not a function.

**Acceptance:** `Widget . size`, `Widget .size` and `Widget. size` are all rejected, in an
expression and in a type; `Widget.size` is unaffected. The package block in
[Records](../spec/records.md#whitespace-before-a--decides-which-form-it-is) goes red, is retagged
`expect=parse-error` with its reason pinned, and its **Known gap:** paragraph is rewritten to
state the rule without the gap. A parser test asserts the rejection in both languages.

**Found:** while writing [`docs/spec/records.md`](../spec/records.md) (`SPEC-21`).
