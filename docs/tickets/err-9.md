# ERR-9 · Span `parser::Exposed`, so an exposing list can be underlined

**Sizing:** small.

**Depends on:** `ERR-3` (closed) — the `NodeSpan`/`SpanLabel` machinery this needs already
exists; only the `Exposed` productions were left out of it.

**Location:** `src/compiler/parser/grammar.lalrpop` (the `Exposed` and `Exposing` productions),
`src/compiler/parser/mod.rs` (`enum Exposed`), `src/compiler/canonical/environment.rs`
(`EnvError::ValueNotFound`, `UnionNotFound`, `InfixNotFound`, raised at lines 150/169/177),
`src/compiler/canonical/mod.rs` (`Error::ExportNotFound`, raised at line 1070).

**Problem:** `ERR-3` gave every declaration, expression, pattern and type a span, and
deliberately stopped at the exposing list. `enum Exposed` has three variants — `Lower(Name)`,
`Upper(Name, Privacy)`, `Operator(Name)` — and none carries a position, because the four
`Exposed` productions build it without capturing `@L`/`@R`. Four errors are caret-less as a
direct result:

* `EnvError::ValueNotFound` / `UnionNotFound` / `InfixNotFound` — raised while walking
  `import Foo exposing (bar)` when `Foo` exposes no `bar`.
* `canonical::Error::ExportNotFound` — raised while walking the `module … exposing (…)` header
  when the module exposes a name it does not declare.

Each of these is about a name **the user wrote in this file, on that line**. The rationale
comment on `process_import`'s `span` parameter (`environment.rs:88`) explains withholding the
`import` line's span from them by saying that pointing there "would underline text that is not
where the problem is". That is not quite the reason: the offending name *is* on the `import`
line. The real limitation is finer — the best caret currently available is the whole `import`
line rather than the offending name, and a whole-line caret on a four-name exposing list is
not worth much. Spanning `Exposed` removes the choice: the caret goes under `bar`.

**Approach:** carry a `NodeSpan` on each `Exposed` variant (or, if that gets noisy at the three
construction sites, wrap it as the AST already does for `Expression`/`Pattern`/`Type`: a `span`
field beside an `…ExposedKind`). Capture `@L`/`@R` in the four `Exposed` productions, thread
the span through `expose_*` in `environment.rs` and through the export walk in
`canonical/mod.rs`, and add it to the four error variants and their `labels` arms. Then reword
the `process_import` comment, which will no longer be describing the code.

Per the standing invariant, `grammar.lalrpop`, `parser/mod.rs` and the `from_parser*`
conversions in `canonical/mod.rs` move in one commit.

Once this lands, the "any error raised while walking a node the grammar does not span, such as
an `exposing` list" clause in `CLAUDE.md`'s *Standing invariants* is no longer true of this
node, and should be rewritten or dropped rather than left describing code that changed.

**Related:** `ERR-7` ("did you mean …?") will want this caret — a suggestion on
`ValueNotFound` needs a name to hang off.

**Acceptance:** a pipeline test asserting that `import Foo exposing (missing)` renders a label
whose range is `missing` alone, and one asserting the same for a `module … exposing` header
naming an undeclared value.
