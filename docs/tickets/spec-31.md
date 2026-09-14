# SPEC-31 · A facade has no legitimate way to name `Int`, and `BUG-16` is what hides it

**Sizing:** medium — the decision and a [`docs/decisions/`](../decisions/README.md) entry are
the bulk of it, plus the chapter paragraph that follows from whichever shape wins. The compiler
change is likely small once the shape is settled, but one of the candidates below moves
`Type::Literal`, and that one is not small.

**Location:** `std/core/src/Basics.zel` — the `import Js.Basics` and `import Js.Utils` lines,
and the `type Int`, `type Float` and `type Bool` declarations further down;
`std/core/src/Js/Basics.zel` and `std/core/src/Js/Utils.zel`, neither of which carries an
`import` line at all; `src/compiler/dependencies.rs` — `ModuleWalker::new`, which is what
rejects the cycle.

**Blocks:** [BUG-16](bug-16.md). Related to [BUG-26](bug-26.md), which is a different defect
about the same four type names.

**Problem:** the two facades `Basics` is built from name primitive types they cannot import.

`Basics` imports both of them:

```
std/core/src/Basics.zel:65:  import Js.Basics
std/core/src/Basics.zel:66:  import Js.Utils
```

and it is also where the primitive types are declared — `type Int` at `Basics.zel:123`, `type
Float` at `:146`, `type Bool` at `:458`. Both facades then write those names in their
signatures with no import to bring them into scope: `unsafe idiv : Int -> Int -> Int` and
`unsafe toFloat : Int -> Float` in `Js/Basics.zel`, `unsafe equal : a -> a -> Bool` and
`unsafe compare : a -> a -> Int` in `Js/Utils.zel`.

Writing the import out is rejected. Adding `import Basics exposing (Int, Float, Bool)` to
`Js/Basics.zel` and running `cargo run`:

```
   ┌─ Js/Basics.zel:10:1
10 │ import Basics exposing (Int, Float, Bool)
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ `Js.Basics` imports `Basics` here
   ┌─ Basics.zel:65:1
65 │ import Basics
   │ ^^^^^^^^^^^^^^^^ `Basics` imports `Js.Basics` here
   = cycle: Js.Basics -> Basics -> Js.Basics
```

The default imports do not supply it either, and deliberately: a module `Basics` depends on
is exactly the case [*The default imports*](../spec/modules.md#the-default-imports) withholds
`Basics` from, for this same cycle. So there is no spelling — written or implicit — that puts
`Int` in a facade's scope.

These two modules compile today only because an unresolved type name is invented rather than
reported, which is [`BUG-16`](bug-16.md). That makes this ticket a prerequisite rather than a
sibling: fixing `BUG-16` turns `Js/Basics.zel` and `Js/Utils.zel` red with no available fix,
so the shape below has to be chosen first.

Found while reviewing `LANG-8` (PR #206), which is where the withholding rule was written and
where the interaction became visible. It was deliberately left unfixed there — the default-import
mechanism is not what creates the cycle, and choosing where a primitive type lives is a language
decision rather than a change to one phase.

**Approach:** the ticket does not pick. Four shapes are worth weighing, and they differ in what
they cost rather than in whether they work:

1. **Make `Int`, `Float` and `Bool` compiler-known**, in scope everywhere without an import and
   without a declaration in `Basics`. This removes the cycle by removing the dependency, and it
   is closest to what the `NOTE: The compiler provides the real implementation.` comments on
   `Basics.zel:123` and `:146` already claim. It interacts with [`BUG-26`](bug-26.md), which is
   about the typer mapping these names to `Type::Literal` by spelling, and with
   [`LANG-41`](lang-41.md), which moves in the opposite direction by retiring `Type::Number`.
   Deciding this one probably decides `BUG-26` too.
2. **Exempt a `module foreign` facade from the cycle rule**, on the grounds that a facade has no
   body and so cannot actually consume what it imports. This is the smallest change and the
   easiest to get subtly wrong: `dependencies` uses the graph for check *order*, not only for
   cycle rejection, so an exempted edge still has to leave the interfaces available in the order
   canonicalization needs them.
3. **Split the primitive declarations into a module that imports nothing** — `Prim`, say — which
   `Basics` re-exports and the facades import directly. No cycle, no new compiler concept. The
   cost is a module that exists for the compiler's benefit and that the chapter then has to
   either explain or hide.
4. **Stop `Basics` importing the facades**, routing its `unsafe` bindings some other way. This
   is the largest change to `std/core`'s shape and the least obviously desirable.

Whichever wins, the argument for it belongs in a [`docs/decisions/`](../decisions/README.md)
entry, since the rejected three are the useful part of the record.

**Acceptance:** a decision entry states the chosen shape and what it was chosen over.
`std/core/src/Js/Basics.zel` and `std/core/src/Js/Utils.zel` name `Int`, `Float` and `Bool`
through whatever mechanism it settles on, with no invented type standing in — verifiable by
applying [`BUG-16`](bug-16.md)'s fix on top and finding both modules still check. `cargo run`
still prints `parsed 8 modules`, lists all eight as checked, and exits 0. If the shape changes
what [*The default imports*](../spec/modules.md#the-default-imports) or
[`docs/spec/interop.md`](../spec/interop.md) claims, those paragraphs move in the same change
and `cargo test --test spec` is green.
