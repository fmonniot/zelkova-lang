# LANG-26 · A declaration's clauses need not stand together

**Sizing:** small.

**Location:** `src/compiler/parser/mod.rs`, `Module::from_declarations` — the
`HashMap<Name, Vec<Declaration>>` that gathers `Declaration::Function` and
`Declaration::FunctionType` by name, throwing their positions away.

**Decided by:** [`docs/spec/declarations.md`](../spec/declarations.md)'s *The clauses stand
together*.

**Problem:** the clauses of one declaration are consecutive, with nothing between them but
blank lines and comments. Today they are gathered by name from anywhere in the module, so
this is one declaration with two clauses and an unrelated one sitting in the middle:

```
invert On =
  Off

other =
  On

invert Off =
  On
```

Order among clauses decides which is tried first, so a clause a reader has to search for is
not a detail of layout — it changes what every clause above it matches.

This is the same lost-position defect as [`LANG-11`](lang-11.md), which is about an
annotation sitting anywhere among the declarations rather than directly above its body. Both
are `from_declarations` discarding the order it was handed, and whichever lands first should
keep the position that the other one then reads.

**Blocked by [`LANG-20`](lang-20.md)** in the sense that matters for the test: a declaration
may not have more than one clause at all today, so the chapter's block is rejected for that
reason and this rule is unobservable. Landing `LANG-20` without this one turns that block
green and the chapter wrong.

**Approach:** keep each declaration's index as `from_declarations` walks the list, and when a
name's declarations are assembled into a `Function`, check that its bindings' indices are
consecutive. A new `canonical::Error` variant — or a parser-level one, depending on where
`LANG-20` ends up putting the grouping — naming the clause that stands apart, with the
declaration's other clauses as secondary labels.

**Acceptance:** the `expect=canonical-error:MultipleBindingsUnsupported` block under *The
clauses stand together* in [`docs/spec/declarations.md`](../spec/declarations.md) still fails,
now for the new error — retag it with that variant and delete the **Known gap:** paragraph.
The multi-clause block under *Clauses*, whose clauses are adjacent, must be `expect=ok` once
`LANG-20` has landed. A test for each, seen to fail before the fix.
