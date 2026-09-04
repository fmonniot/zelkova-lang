# GEN-1 · Emit runnable JavaScript for a checked module

**Sizing:** large (a whole phase that does not exist, plus a decision about the intermediate
representation it consumes and a way to run its output). Bigger if it is asked to cover
constructs the front end already accepts but `std/core` does not yet use.

**Location:** `src/compiler/mod.rs` — `check_module`, which ends at `exhaustiveness::check` and
returns the `canonical::Module`; `compile_package`, which reports and drops it. A new module
under `src/compiler/` for the backend. `src/main.rs`, which prints the modules that checked and
writes nothing.

**Problem:** the pipeline stops after type checking. `check_module` runs canonicalization, the
typer and exhaustiveness and hands back a `canonical::Module`; nothing consumes it. There is no
code generation module under `src/compiler/`, so no Zelkova program has ever run.

That is the gap [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md) opens
with: the whole chapter specifies what evaluating an expression does, and the compiler neither
enforces nor implements any of it. It also caps what the spec harness can prove — a block tagged
`expect=ok` parses and canonicalizes, so a chapter can state what an expression computes but
nothing holds it to that.

**Approach:** this ticket does not pick the design. What it has to settle first:

1. **What the backend consumes.** `canonical::Module` is the only thing `check_module` produces,
   and it is shaped for name resolution rather than for emission — the typer already found it
   worth translating into its own `Term` language. Whether the backend reads the canonical AST
   directly or a new intermediate one is the first choice, and `check_module`'s existing comment
   asking the same question about the typer is the prior art.
2. **How a function value is represented.** Zelkova's application is curried and
   [`docs/spec/js-interop.md`](../spec/js-interop.md) promises that a `module javascript`
   companion's export takes a **plain parameter list**, not Elm's `F2`/`F3` wrappers. The
   generator therefore has to bridge the two at the facade boundary, and decide what it emits
   for a Zelkova function of two arguments applied to two arguments.
3. **The tail-call rewrite.** *Recursion and tail calls* promises that a self tail call runs in
   constant stack, compiled as a jump back to the top of the declaration. That is a rule about
   the generator, and it is the one rule in the chapter that cannot be met by emitting the
   obvious thing.
4. **Initialisation order.** *A binding with no parameters is evaluated once* puts a dependency
   order on parameterless top-level bindings, independent of the order they are written. Nothing
   computes that order today — `dependencies.rs` orders *modules*, not declarations within one —
   and [`LANG-35`](lang-35.md) is the sibling ticket for the cycle among them that nothing
   currently rejects.
5. **Where output goes.** There is no manifest and no build directory
   ([`LANG-13`](lang-13.md)); `cargo run` compiles `std/core/src` and prints. What file layout
   the emitted modules take, and how a companion `.mjs` is found and referenced from the module
   that imports its facade, is undecided.

Rules from [`evaluation-semantics.md`](../spec/evaluation-semantics.md) that constrain the
output rather than the design, and that a first version has to honour or say it does not:
subexpressions evaluate left to right; both operands of `&&` and `||` are evaluated; `Int` is
32-bit and wraps; `n // 0`, `modBy 0 n` and `remainderBy 0 n` are `0` (today
[`BUG-24`](bug-24.md)); equality is structural and comparing functions is not allowed.

One decision is inherited rather than made here: **a class dictionary is erased by
specialisation and never passed** ([`docs/spec/type-classes.md`](../spec/type-classes.md),
decision 7). It costs nothing while no class exists, but a backend that starts by passing
dictionaries is one that has to be unpicked when [`CLASS-4`](class-4.md) lands.

**Not in this ticket:** teaching the spec harness to check what a block *computes*.
[`TEST-2`](test-2.md) is the equivalent for type errors and stops at the typer; an
`expect=evaluates-to` tag needs a generator and a runtime to exist first, and is a follow-up.

**Acceptance:** `cargo run` emits JavaScript for `std/core/src`'s eight modules, and a test in
`tests/` compiles a small module, imports one of its exported values from the emitted output
under `node`, and asserts the value. (What entry point a *program* has is the open question at
the foot of [`packages.md`](../spec/packages.md); this ticket does not need it answered, because
an exported value can be called from a test harness.) At minimum the test covers one
self-recursive function in tail position, deep enough that a non-tail emission exhausts the
stack, and one call through a `module javascript` facade into its companion `.mjs`.
