# BUG-39 · A function parameter pattern other than a variable or `_` is never type checked

**Severity:** medium (wrong behaviour under normal use — a function whose parameter destructures
a tuple or a constructor is accepted, its whole body unchecked, and nothing says the declaration
was skipped).

**Location:** `src/compiler/typer/mod.rs` — `wrap_with_patterns`, whose `match` translates only
`canonical::PatternKind::Variable` and `canonical::PatternKind::Anything`, and whose `_ => None`
arm rejects every other pattern kind; the `Option<Vec<_>>` this feeds means one such parameter
makes the whole `collect()` `None`. `value_to_term_and_annotation`, whose
`wrap_with_patterns(pattern_iter, body_term)?` propagates that `None` up, and `type_check`
(`src/compiler/typer/mod.rs`), whose third pass records `Solved::Untranslatable` for the whole
value when `value_to_term_and_annotation` returns `None` — the same outcome `translate_pattern`
already produces for a `case` scrutinee's own unsupported nesting
(`translate_pattern`'s `_ => return None, // nested complex patterns not yet supported`, right
beside `PatternKind::Constructor`'s own field-binding loop).

**Problem:** `wrap_with_patterns` turns a function's parameter list into nested `TermKind::Fun`
bindings, each taking one plain name. A parameter pattern is not resolved against anything one
plain name away — it is not a lookup, unlike `BUG-36`'s gap — the translation for it simply does
not exist yet for a *parameter* position, though it does for a `case` scrutinee
(`translate_pattern`).

`std/core` has six declarations that hit this today, none of them touching an import:

- `Basics.never (JustOneMore nvr) = never nvr` (`std/core/src/Basics.zel`) — a constructor
  pattern; `Never`, and `JustOneMore`, are declared in `Basics` itself, so this is not `BUG-36`'s
  domain (an imported name), it is a pattern *shape* `wrap_with_patterns` cannot represent at
  all, local or imported.
- All five of `std/core/src/Tuple.zel`'s functions destructure a tuple parameter directly:
  `first (x,_) = x`, `second (_,y) = y`, `mapFirst func (x,y) = (func x, y)`,
  `mapSecond func (x,y) = (x, func y)`, `mapBoth funcA funcB (x,y) = (funcA x, funcB y)`.

Surfaced while scoping [`GEN-13`](gen-13.md) (write the build): `javascript::emit` refuses a
module if even one of its declarations was never type checked, so this gap alone keeps
`Basics` and `Tuple` both fully refused now that [`BUG-36`](README.md) (an imported constructor
or value) is closed.

**Fix:** give a parameter pattern the same case-arm translation `translate_pattern` already gives
a `case` scrutinee. The mechanism is not new — `translate_pattern` returns a `TermPattern` and
its bindings for exactly this shape — what has to be decided is how a `Fun` reaches it, since
`TermKind::Fun` only ever bound one plain name:

- Desugar each patterned parameter into a fresh plain name plus a `TermKind::Case` wrapping the
  rest of the body: `f (x,_) = x` becomes, in effect, `f p1 = case p1 of (x,_) -> x`, built with
  `translate_pattern` and a single-branch `Case`. The fresh name needs a source — `counter`
  already exists for fresh type variables and could mint term-level names too, but nothing today
  guarantees such a name cannot collide with a binding already in scope; that guarantee is part
  of what this ticket has to settle, not assume.
- Extend `TermKind::Fun` itself to carry a full pattern rather than a plain name, pushing the
  scrutinee-vs-binder distinction into every later reader of `Fun` (annotate/constraint,
  `ir::build`, `javascript::emit`) instead of resolving it once at translation. This is a wider
  change to a type every phase already shares, and whether it is worth that width instead of the
  desugaring above is this ticket's to decide.

Either fix has to account for `translate_pattern`'s own remaining gap, unaffected by this ticket:
its `_ => return None` for a pattern nested inside a constructor's arguments (`Just (x, y) ->`)
stays unsupported, since that is `translate_pattern`'s own limit and not a parameter-position
problem.

**Acceptance:** a test in `tests/typer.rs` (or `tests/compiler/`, matching whichever file already
covers `wrap_with_patterns`'s neighbours) for each shape:

- a tuple-pattern parameter, `f (x, _) = x`, is solved rather than `Untranslatable` — assert the
  declaration's `Solved` variant, not just that `type_check` returned `Ok`;
- a constructor-pattern parameter over a local union, mirroring `Basics.never`'s shape, is solved
  the same way;
- mutation-check by restoring `wrap_with_patterns`'s `_ => None` and watching both tests go red.

`cargo run` still prints `parsed 8 modules`, lists all eight as checked, and exits 0 —
`Basics.never` and all five of `Tuple`'s functions are newly checked, so a failure there is
either a real error in `std/core` or a regression, and has to be told apart before landing.
