# TIDY-9 · `Module::from_declarations` has a `panic!` on a declaration kind its own bucketing rules out

**Sizing:** small. One function, no behaviour change.

**Location:** `src/compiler/parser/mod.rs` — `Module::from_declarations`, the
`_ => panic!("Invalid kind of declaration used in functions, report this error …")` arm in the
`.map(|(name, decls)| …)` that assembles each `Function`.

**Found while:** working [LANG-37](README.md), which added a line to that loop. Left alone
there, because it is not the ticket's change.

**Problem:** `CLAUDE.md`'s *Standing invariants* say no `panic!` on a non-test path, and this is
one. It cannot fire today: the first loop over `declarations` only inserts a
`Declaration::Function` or a `Declaration::FunctionType` into the `functions` map, and sends
imports, infixes and unions to their own vectors. So the panic guards a case the code above it
has already excluded, and it is there because the map's value type is `Vec<Declaration>` —
every variant of `Declaration` — rather than one that can only hold the two it does.

That is the shape worth fixing. A `panic!` that cannot fire is invisible until someone adds a
`Declaration` variant that should be bucketed by name, at which point the compiler stays quiet
and the parser crashes on a user's file. `ERR-1` removed the reachable ones; this one survived
because it is not reachable.

**Approach:** make the illegal case unrepresentable rather than reporting it. Bucket a name's
annotation and bindings separately as the first loop reads them — a small struct holding
`Option<FunType>` and `Vec<FunBinding>`, say — so the second step consumes typed values and the
`match` has no wildcard arm to panic in. The alternative is to return a `parser::Error` from the
wildcard arm; it is worse here because the case has no source position and no user could ever
cause it, so there is nothing to describe. Either way the function must keep producing the same
`Module` for every input it does today, in particular the merged `span` and the
`annotation_span` of a function with no body.

Do not touch the `// TODO Error if more than function type is defined` above the loop.
`LANG-11` owns a repeated annotation, and folding it in here widens a cleanup into a language
change.

**Acceptance:** `grep -n 'panic!' src/compiler/parser/mod.rs` finds nothing outside
`#[cfg(test)]`. `cargo test --workspace` is unchanged and green, and
`cargo run -- compile std/core` still prints `parsed 8 modules` and lists all eight as checked.
No new test is required, since no behaviour changes; if one is added, mutation-check it per
`CLAUDE.md`.
