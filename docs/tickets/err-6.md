# ERR-6 · A dependency cycle points at the `import` lines that form it

**Sizing:** small-to-medium — the graph work is done, this is reporting.

**Depends on:** `ERR-3` (`parser::Import` carries a span) and `ERR-5` (a label can name a file
other than the erroring module's — a cycle has no single file).

**Location:** `src/compiler/dependencies.rs` — `Error::CycleDetected` and its `PhaseError` impl.

**Problem:** `ModuleWalker::new` runs Tarjan's SCC and reports every cycle it finds, and the
diagnostic is a headline plus one note per cycle, written back to its start so it reads as the
loop it is:

```
error: 1 circular dependency between modules
   = cycle: A -> B -> A
```

That names the modules and stops. The user still has to open both files and find which `import`
made each edge — and in a cycle of four modules where one of them imports three others, that is
real work. rustc would underline each `import` in the loop, one label per file.

The information is all present: the SCC is computed from `source.imports`, so each edge in the
cycle corresponds to a specific `Import` node in a specific module.

**Approach:** carry the offending `Import`'s span (and the module it was written in) alongside
each edge of the cycle rather than only the module `Name`s, and emit one label per edge. Keep
the existing note — the `A -> B -> A` line is a good summary and stays useful when a cycle is
long enough that the labels scroll.

Note this error is deliberately *not* attached to a module: the `PhaseError` impl's doc comment
says "a dependency cycle belongs to the package, not to any one module", and
`CompilationError::DependenciesError` bypasses `phase_diagnostic` for that reason. That stays
true — it is why this needs `ERR-5` rather than the single-file path `ERR-3` builds.

**Acceptance:** a fixture package with a two-module cycle renders a diagnostic with one label
per edge, each in the importing module's file, asserted in `tests/pipeline.rs`.
