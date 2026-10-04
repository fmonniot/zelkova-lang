# TIDY-15 · `insert_foreign_value` has a `todo!()` on a variable-table state its callers cannot produce

**Sizing:** small. One function, no behaviour change.

**Location:** `crates/zelkova-compiler/src/canonical/environment.rs` — `insert_foreign_value`, the
`_ => todo!("find out what to do in those cases")` arm of the `match env.variables.remove(&name)`.

**Found while:** working [LANG-39](README.md) (PR #317), whose diff adds callers of the function.
Left alone there, because it is not that ticket's change.

**Problem:** `CLAUDE.md`'s *Standing invariants* say no `todo!()` on a non-test path, and this is
one. The arm is what `match` is left with after `None`, `Some(ValueType::Foreign(..))` and
`Some(ValueType::Foreigns(..))` — that is, `Some(ValueType::Local)` and
`Some(ValueType::TopLevel)`. It cannot fire today: `new_environment` registers every import's
values through `process_import` before anything else touches the table, and `Local` and `TopLevel`
entries are only inserted afterwards, by `insert_top_level_value` and the scoped environments' own
inserts. So at every call the table holds only foreign entries, and the arm guards a state its
callers have already ruled out.

That is the shape worth fixing, the one [TIDY-9](tidy-9.md) describes for the parser. A `todo!()`
that cannot fire is invisible until someone calls `insert_foreign_value` from a place that runs
after the module's own declarations are registered, and then the compiler crashes on a user's file
instead of reporting a clash. `ERR-1` removed the reachable ones; this one survived because it is
not reachable.

**Approach:** the ticket does not pick between two. (a) Make the state unrepresentable: give the
imports' phase a table type whose values are only `Foreign` and `Foreigns`, and move into
`ValueType` only when the module's own names are added — more types, but the `match` has no
wildcard arm. (b) Make the arm do something defined: a `Local` or `TopLevel` entry means the
module's own value shadows or clashes with an import, and the arm would keep the existing entry
and drop the import's, or return an `EnvError` — which means deciding what the language says about
that clash, a question [`modules.md`](../spec/modules.md) may already answer; read it first. (a)
changes no behaviour; (b) is only a cleanup if the spec already settles the clash.

**Acceptance:** `grep -n 'todo!' crates/zelkova-compiler/src/canonical/environment.rs` finds
nothing outside `#[cfg(test)]`. `cargo test --workspace` is unchanged and green, and
`cargo run -- compile std/core` still prints `parsed 10 modules` and lists all ten as checked. No
new test is required for (a), since no behaviour changes; for (b), a test for the new error
asserted by variant, mutation-checked per `CLAUDE.md`.
