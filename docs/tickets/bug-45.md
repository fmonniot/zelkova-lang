# BUG-45 · A facade signature may name a union whose constructors hold a type no predicate decides

**Severity:** low (on the result side, code generation now refuses it; on the argument side the
program runs, and only the WebAssembly target, which does not exist yet, would have no spelling
for it).

**Location:** `crates/zelkova-compiler/src/canonical/mod.rs` — `check_facade_admitted_type`, whose
`Type::Type(_, args)` arm checks a union's type arguments and never the union's declaration.
`crates/zelkova-js/src/lib.rs` — `Error::NoPredicate`, which is where the result-side case is
caught today.

**Problem:** [Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)
admits "a union type, applied to admitted types", and a union's predicate checks each
constructor's arguments against the types that constructor declares. So a union is only
decidable when every type its constructors declare is — and
[What a facade signature may not name](../spec/interop.md#what-a-facade-signature-may-not-name)
rejects a function type "wherever it appears".

`check_facade_admitted_type` looks at the signature's own type expression only. Given

```zel
module Handler exposing (Handler(..))

type Handler
  = Handler (Int -> Int)
```

```zel
module foreign Test exposing (first, run)

import Handler exposing (Handler)

unsafe first : Int -> Handler
unsafe run : Handler -> Int
```

both signatures canonicalize. `first` is refused later, by `javascript::emit`, with
`Error::NoPredicate` naming the constructor `Handler.Handler` — that refusal is what
`crates/zelkova-js/tests/javascript.rs`' `a_union_holding_a_function_has_no_predicate` pins. `run` is accepted
all the way through and emitted, because only a result is checked on JavaScript. The same holds
for a constructor argument that is a type variable the union does not bind
([`LANG-31`](lang-31.md)).

**Fix:** extend the admitted-type walk to follow a union into its declaration's constructors,
substituting the union's arguments for its variables, and reject a function type or an unbound
variable found there with `FacadeTypeNotAdmitted`, pointing at the signature and naming the
constructor. The walk needs every union the signature reaches, including ones declared in other
modules and exposed without their constructors — an `Interface` does not carry those, so this
needs either the constructor types published in the interface or the check moved to where the
whole build is visible. The ticket does not pick. A recursive union needs a visited set.

**Acceptance:** a test in `crates/zelkova-compiler/tests/canonical.rs` asserts that both `first` and `run`
above are `FacadeTypeNotAdmitted`. `Error::NoPredicate`'s `Unpredicated::Function` and
`Unpredicated::Variable` cases become unreachable from a signature that canonicalized, and the
doc comment on `Error::NoPredicate` is updated to say so.

**Found:** by the first attempt at [`GEN-2`](README.md), while building the union predicate.
Left unfixed there because it is a front-end check, and `GEN-2` only had to refuse what it could
not emit.
