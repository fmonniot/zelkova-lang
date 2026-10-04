# LANG-87 · A derivation binding written with more parameters than the walk supplies is rejected

**Sizing:** small-to-medium. The rule is already written; the work is canonical code that can
hold a parameter the walk does not supply. What could make it bigger is that every role
(`matched`, `differed`, `atConstructor`, `combine`) takes the extra parameters, and a generated
member then takes them as well.

**Part of:** *Active work: type classes* in [the index](README.md), after `LANG-83`. Found in the
review of `LANG-83`'s PR, which kept the rejection because canonical code has no lambda.

**Location:** `crates/zelkova-compiler/src/canonical/derivation.rs` — `class_derivations`, which
raises `Error::DerivationBindingTakesTooMany` when `Value::arity()` exceeds
`DerivationRole::parameters()`; `Generated::place` and `Generated::place_combine`, which place a
binding's body where the walk supplies its parameters and cannot place a parameter beyond them;
`Generated::member`, whose generated definition takes `$left` and `$right`, or `$value`, and no
more. `crates/zelkova-compiler/src/canonical/mod.rs` — `Error::DerivationBindingTakesTooMany`.
`docs/spec/type-classes.md` — [*A class says how it is
derived*](../spec/type-classes.md#a-class-says-how-it-is-derived), its **Known gap:** paragraph and
the block tagged `expect=canonical-error:DerivationBindingTakesTooMany`.

**Decided (`docs/spec/type-classes.md`, *A class says how it is derived*):** `matched` is an `R`,
`differed` a `Position -> Position -> R`, `atConstructor` a `Position -> R` and `combine` an
`R -> R -> R`, where the only condition on `R` is that it does not mention the class variable. `R`
may therefore be a function type, and a binding of that type may be written with parameters for it.

**Problem:** a member at `hashWith : a -> Int -> Int` has `R = Int -> Int`, and
`atConstructor p n = n` has exactly the type the chapter gives (`Position -> Int -> Int`). It is
rejected:

```
DerivationBindingTakesTooMany(atConstructor, hashWith, 1, …)
```

The same holds for `matched n = …`, `differed i j n = …` and `combine x y n = …` whenever `R` is a
function, so a derivation for a member shaped `a -> X -> R` can only be written point-free today.
The rejection exists because `place` binds each parameter the walk supplies by a one-branch `case`
and has nothing to bind a parameter beyond them to: placing the body where the binding was
called would need a lambda, and the language has none ([`LANG-34`](lang-34.md)).

**Approach:** the ticket does not pick between two ways out.

1. **Generate the parameters.** The member's signature says how many arrows `R` has. A generated
   member takes that many more parameters than the walk's (`$p1`, …, with the `$` no source file
   can write), every role is placed with them as extra arguments, and the walk's recursive calls
   stay partial applications. `place` already applies an argument beyond a binding's parameters
   to its body, so the work is on the other side: a binding with more parameters than the walk
   supplies is given the generated ones. `combine`'s second parameter, the rest of the walk, has
   to be applied to them too.
2. **Wait for a lambda** ([`LANG-34`](lang-34.md)), and place a binding with extra parameters as
   one. It changes nothing in the generated member's own parameters, and costs nothing until the
   language has one.

Whichever lands, the check at the class (`DerivationCheck` in the typer, which holds the binding
to the type the chapter gives it) is unchanged, since it types the written binding.

**Acceptance:** each seen red with what it pins neutralised.

In `crates/zelkova-compiler/tests/canonical.rs`, `a_derivation_binding_with_too_many_parameters_is_an_error`
is replaced by a test that a derivation for `hashWith : a -> Int -> Int` with `atConstructor p n =
n` canonicalizes, and by one that a binding with more parameters than `R` has arrows is still an
error naming the binding.

In `crates/zelkova-compiler/tests/ir.rs`, the member generated for it takes the extra parameter
and applies the placed body to it.

In `docs/spec/type-classes.md`, the block tagged
`expect=canonical-error:DerivationBindingTakesTooMany` is retagged `expect=ok` and the **Known
gap:** paragraph above it is deleted; `cargo test --test spec` is green.

`cargo test --workspace` is green and `cargo run -- compile std/core` still lists all ten modules
as checked.
