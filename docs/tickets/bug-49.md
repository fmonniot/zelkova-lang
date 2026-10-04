# BUG-49 · A pattern variable that shadows an outer name drops the outer binder when its branch ends, so a valid declaration is left unchecked

**Severity:** medium (a valid program is silently not type checked: no error, no warning, and the
declaration is in `ir::Module::unchecked` with `reported: false`, which the JavaScript emitter
refuses, and a type error in it is never reported).

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `Types::add_binder` and
`Types::remove_binder`, which insert into and remove from one `HashMap` keyed by name;
`crates/zelkova-compiler/src/typer/annotate.rs` — `annotate`'s `TermKind::Case` arm, which adds a
pattern's bindings, annotates the branch, then removes each of them ("Restore scope");
`DerivationCheck::binding` in `typer/mod.rs`, which type checks the class's own derivation
bindings unrenamed and discards the `Solved` it gets back.

**Problem:** [*Patterns*](../spec/patterns.md) says a pattern variable shadows anything of the same name from
an enclosing scope. `annotate` removes the name from the environment when the branch ends instead of restoring
what it shadowed, so any later use of the outer name in a sibling branch is an unbound variable
there. `solve` answers an unbound variable with `Solved::UnboundName` and no error, since that answer
is meant for a name the typer's environment does not hold and not for a mistake in the source, so
the declaration goes unchecked. This
valid program is left as `Solved::UnboundName`, with `check_module` returning `Ok`:

```zel
type Pair = Pair Int

g : Bool -> Pair -> Int
g b n =
  case b of
    True ->
      case n of
        Pair n ->
          n

    False ->
      case n of
        Pair m ->
          m
```

The inner `Pair n` removes the outer `n` on leaving its branch, and `False`'s use of `n` is
unbound. Reproduced at the tip of `LANG-83`'s PR: `g` is in `ir.unchecked` with `reported: false`.

`LANG-83` works around it for the members it generates: every binder in a placed body gets a
fresh name (`$3$x`) and no source file can spell one, so no generated binder is ever inside the
scope of another with the same name. It does not reach the class's own bindings: `DerivationCheck`
type checks them as written, and discards the result. A derivation that shadows is therefore not
checked either, and an ill-typed one is accepted: in

```zel
class Eq a where
  eq : a -> a -> Bool

  derived eq
    matched = True
    differed _ _ = False
    combine x y =
      case x of
        True ->
          case y of
            y ->
              1

        False ->
          y
```

the `1` is a `Bool` mismatch and the module checks with no error.

**Fix:** make `remove_binder` restore the binding the name shadowed on scope exit (the
environment holds a stack per name, or `add_binder` returns what it replaced and the `Case` arm
puts it back). `DerivationCheck::binding` then needs no special case; it should also report a
`Solved::UnboundName` answer as unchecked rather than dropping it. Once restored, the renaming in
`Generated::bind_pattern` is no longer needed for the typer's sake, and its doc comment says
what it is for.

**Acceptance:** `crates/zelkova-compiler/tests/typer.rs` — the program above checks and `g` is
`Solved::Typed`; the class above is a `UnificationFailed` at the `1`. Both are seen red against the
current `remove_binder`. `cargo test --workspace` is green.

**Found:** while working `LANG-83`, whose PR body described it as found, not fixed, and whose
reviewer reproduced it and found the class-side case. Left unfixed there because the typer's scoping
is not what that ticket changes.
