# PERF-2 · Every `Basics` operator is called through `$curry`, because its declaration has no parameters

**Sizing:** small once decided. The change is either a rewrite of `std/core/src/Basics.zel`'s
and `Bitwise.zel`'s forwarding declarations or one rule about a parameterless binding's arity.
Which one is a choice between a library change and a language rule.

**Location:** `std/core/src/Basics.zel` — `add = Js.Basics.addInt` and every declaration shaped
like it (`sub`, `mul`, `fdiv`, `idiv`, `pow`, `eq`, `neq`, `lt`, `gt`, `le`, `ge`, `and`, `or`,
`xor`, `append`, `modBy`, `remainderBy`, `atan2`); `std/core/src/Bitwise.zel`, which is written
the same way; `canonical::Module::emitted_arity` (`src/compiler/canonical/mod.rs`), which gives
each of them arity 0; the *Calls* section of `src/compiler/javascript.rs`'s module doc.

**Problem:** a call supplying every argument of a declaration whose arity is known is a direct
call ([`DEC-18` decision
3](../decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)),
and an importer reads that arity from the interface. `add`'s arity is 0, because it is written
with no parameters, so `n + n` in any module is emitted as

```javascript
zelkova_core$Basics$add(n)(n)
```

against `const add = $curry(zelkova_core$Js$Basics$addInt, 2);`. That is correct, and it goes
through `$curry` on every call: the first call allocates a closure holding `n`, and the second
reaches `addInt`. Every arithmetic, comparison and boolean operator in every program pays that
cost, which is the case decision 3's fast path was written for.

Found while fixing `BUG-43`, which made the cross-module call direct for a declaration written
with parameters. Left there because that ticket was a miscompile, this is a cost, and one of
the two ways to remove it is a language rule.

**Fix:** undecided. Two directions:

1. **Write the forwarding declarations with parameters**: `add a b = Js.Basics.addInt a b`.
   No language rule changes, and `add` then has arity 2 in its interface. Probed on
   2026-09-28: rewriting `add` and `idiv` that way, `cargo run -- compile std/core` still checks
   all eight modules. For `add : a -> a -> a` that is only because an annotation's type
   variable unifies with `Int` ([`LANG-12`](lang-12.md)), which is exactly as true of today's
   `add = Js.Basics.addInt`, so this direction is no worse placed than the current source when
   `LANG-12` lands. The comment above `add` in `Basics.zel`, which explains the choice of
   `addInt`, moves with it.
2. **Give a parameterless binding the arity of the function it names**: a binding whose body is
   a bare reference to a declaration of arity *n* is emitted as that declaration, or as an
   *n*-parameter function forwarding to it. That is eta-expansion. It is observable only
   through evaluation order and sharing, which [DEC-9](../decisions/dec-9.md) and
   [*A binding with no parameters is evaluated
   once*](../spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once)
   govern, so whether it is permitted is for the language owner to decide. It would reach a
   user's own forwarding bindings too, which the first direction does not.

**Acceptance:**

- A Rust test pins that `n + n`, compiled against `std/core`'s real `Basics`, is a direct call
  (`tests/pipeline.rs`'s `check_std_core` supplies the real interfaces), and the pin goes red
  when the fix is reverted.
- `cargo run -- test tests/fixtures/package_test_cross_module_calls` still reports all three
  tests passing.
- `cargo run -- compile std/core` still prints `parsed 8 modules`, lists all eight, and exits 0.
