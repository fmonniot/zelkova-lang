# Code Improvements

Prioritized list of code quality improvements to the existing codebase.
Items are ordered by priority for the current focus: type checker integration.

## Do first

### 1. Replace `panic!`/`unwrap()` with proper error handling in non-test code

~15 `panic!`/`unwrap()` calls in production paths produce unhelpful crashes instead of
structured errors. Most critical in the typer (current focus area):

- `src/compiler/typer/unifier.rs:53,69` — `panic!` on unification failure and occurs check
- `src/compiler/canonical/mod.rs:222-227` — `unwrap()` in `Pattern::from_parser` for constructor lookup
- `src/compiler/canonical/mod.rs:145-147` — `panic!` on tuple sizes in `Type::from_parser_type`
- `src/compiler/typer/annotate.rs:26` — `panic!` on unbound identifier

All should return `Result` so callers can accumulate diagnostics.

- [ ] Convert typer panics to `Result<_, typer::Error>`
- [ ] Convert canonical panics/unwraps to `Result<_, canonical::Error>`

### 2. Unify error handling strategy across compiler phases

Each phase has its own error story and they compose awkwardly:

- `parser::Error`, `canonical::Error`, `typer::Error`, `exhaustiveness::Error`,
  `dependencies::Error`, `SourceFileError` — all separate types
- Some phases return `Vec<Error>`, some single errors, some `Result<_, Vec<Error>>`
- `CompilationError` has a `PlaceHolder` variant (admitted hack)
- `typer::Error` and `exhaustiveness::Error` are empty enums with `todo!()` in their `From` impls

Before integrating the type checker, define what `typer::Error` actually looks like
and remove the `PlaceHolder` / `todo!()` conversions.

- [ ] Define `typer::Error` variants
- [ ] Remove `CompilationError::PlaceHolder`
- [ ] Remove `todo!()` from `From<typer::Error>` and `From<exhaustiveness::Error>`

## Do second

### 3. Remove unnecessary `Box<Vec<_>>` throughout the parser AST

`Vec` is already heap-allocated; `Box<Vec<_>>` adds pointless indirection.
There are existing TODOs about this in `parser/mod.rs:51` and `parser/mod.rs:359`.

Affected types:
- `Type::Unqualified(Name, Box<Vec<Type>>)` → `Vec<Type>`
- `Type::Tuple(Box<Type>, Box<Vec<Type>>)` → needs rethinking (see item 4)
- `UnionType.variants: Vec<Type>` uses `Type` but only `Type::Unqualified` is valid

- [ ] Replace `Box<Vec<_>>` with `Vec<_>` in parser AST types
- [ ] Update grammar.lalrpop to match
- [ ] Update canonical `Type::from_parser_type` to match

### 4. Clean up Tuple representation inconsistency

Tuples are represented differently across ASTs, which will cause bugs during type checking:

- Parser `Pattern::Tuple(Box, Box, Vec)` — first two mandatory, rest in vec
- Canonical `Pattern::Tuple(Box, Box, Option<Box>)` — exactly 2 or 3
- Parser `Type::Tuple(Box, Box<Vec>)` — head + rest
- Canonical `Type::Tuple(Box, Box, Option<Box>)` — exactly 2 or 3

Pick one canonical shape and use it consistently. Enforce the 2-or-3 constraint at
parse time. Consider a `Tuple2`/`Tuple3` enum instead of optional third element.

- [ ] Decide on canonical tuple representation
- [ ] Update parser AST
- [ ] Update canonical AST
- [ ] Update grammar and conversion code

## Do when convenient

### 5. Reduce cloning in the Layout module

The author flagged this in `layout.rs:109-110`: too many `clone()` calls in the hot
loop. `Offside` and tokens are cloned repeatedly. Use references where possible and
restructure `handle_next_token` to avoid needing `&mut self` while reading `self.contexts`.

- [ ] Refactor Layout to reduce cloning

### 6. Make `Name` internals private

`Name(pub String)` exposes the inner string. Several places access `name.0` directly.
If name interning is ever introduced (mentioned in `name.rs` doc comments), every
one of those sites breaks. Make the field private and route through methods.

- [ ] Make `Name.0` private
- [ ] Add accessor methods
- [ ] Update all direct field accesses

### 7. Replace keyword `HashMap` in Tokenizer with `match`

`tokenizer.rs:71-97` builds a `HashMap` of keywords on every `Tokenizer` construction.
The keyword set is fixed and small (~18 entries). A `match` on the string would be
faster, simpler, and avoid the allocation.

- [ ] Replace `get_keywords()` HashMap with a match statement

### 8. Fix the `associativy` typo

`parser/mod.rs:279` — `associativy` should be `associativity`. Trivial rename
touching the parser AST, canonical AST, and the grammar.

- [ ] Rename `associativy` → `associativity` everywhere

### 9. Add integration tests running the full pipeline on `.zel` sources

No test currently runs `compile_package` or `check_module` on actual `.zel` source
strings end-to-end. Setting up the harness now gives a regression safety net for
type checker integration.

- [ ] Create integration test harness
- [ ] Add tests for modules that currently pass canonicalization
