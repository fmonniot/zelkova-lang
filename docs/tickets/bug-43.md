# BUG-43 · A call to another module's function of two or more parameters is emitted one argument at a time against a plain n-ary function

**Severity:** high (a miscompile. Compilation reports success, and the emitted program throws
the first time any imported function of two or more parameters is called. That includes `+`,
`==` and every other operator `Basics` declares).

**Location:** `src/compiler/javascript.rs` — `Emitter::application`, whose `_` arm is where an
application headed by a `ReferenceKind::Foreign` name lands and is called through
`Emitter::operand`; `Emitter::value`'s `ReferenceKind::Foreign` arm; and the *Calls* section of
the module doc, which says "a value another module declares has an arity this module cannot
see, so it is called one argument at a time". `src/compiler/ir/mod.rs` — `ReferenceKind::Foreign`,
which carries a name and a package and no arity. `std/core/src/Basics.zel` — `add`, declared
`add = Js.Basics.addInt`. [`DEC-18` decision
3](../decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper).

**Problem:** The *Calls* section of `javascript.rs`'s module doc makes one-argument-at-a-time
calls to a foreign value correct by requiring that "every function value the emitted code hands
around accepts being called one argument at a time", and says a declaration of two or more
parameters is `$curry(f, n)` wherever it is used as a value. An *export* is not wrapped that way. A module
exports its declaration as the plain `function` it emitted, and an importer, which cannot see
the arity, calls it as `f(a)(b)`. So the invariant the call rule leans on does not hold at the
one place it has to. This is not particular to `Basics`. On `main`, a package holding

```zel
module Lib exposing (pick, viaPick)

pick : Int -> Int -> Int
pick a b =
  a

viaPick : Int -> Int
viaPick n =
  pick n n
```

and a `Main` that calls `Lib.pick n n`, or writes `n + n`, or `n == n`, compiles and emits

```javascript
function pick(a, b) { return a; }
function viaPick(n) { return pick(n, n); }
export { pick, viaPick };
```

```javascript
function first(n)  { return arity_scratch$Lib$pick(n)(n); }
function double(n) { return zelkova_core$Basics$add(n)(n); }
function same(n)   { return zelkova_core$Basics$eq(n)(n); }
```

and under `node` (`double(2n)`, `same(2n)`, `first(2n)`):

```
double threw: Cannot mix BigInt and other types, use explicit conversions
same threw: zelkova_core$Basics$eq(...) is not a function
first threw: arity_scratch$Lib$pick(...) is not a function
```

`double` fails differently from the other two because `add(n)` runs `addInt(n, undefined)`
before the second call is reached. `Test.equal 1 1` fails at load with the same message as
`same`, so under `zelkova test` a test module using it is reported as errored:

```
ERROR EqTest.oneIsOne: module failed to load: zelkova_core$Basics$eq(...) is not a function
1 test: 0 passed, 0 failed, 1 errored
```

`Basics` has a second shape of the same fault. `add` is a parameterless binding whose value is
another module's function, so `const add = zelkova_core$Js$Basics$addInt` holds the raw
two-parameter function and is not curried even in the module that declares it.

Nothing had run emitted code before `zelkova test` existed, and the tests written against the
emitter pin its text, so nothing saw it. `tests/fixtures/package_test_run` uses no arithmetic and
no imported multi-parameter function, which is why its two tests pass.

Found while reviewing the PR for `LANG-69`, which added `zelkova test`. Left unfixed there
because it is a change to how the emitter and the interfaces agree on arity, not to running
tests. [`GEN-14`](gen-14.md) cannot be finished until this is fixed: its *Approach* checks a
facade call with `Test.equal (7 // 2) 3`, and `//` is an imported two-parameter function.

**Fix:** undecided. The two options below differ in what an importer may assume, and the
ticket does not pick one.

1. **Make the export honour the rule.** An exported declaration of two or more parameters, and
   the facade forwarding functions `Js.Basics` and its siblings emit, are exported as function
   values that accept one argument at a time (`$curry(f, n)`), so `const add = …addInt`
   becomes correct without the emitter knowing anything about a foreign arity. The cost is
   that a call across a module boundary can never be direct: every `n + n` in a user program,
   since `+` lives in `Basics`, allocates a closure per argument. That undoes the point of
   [`DEC-18` decision 3](../decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper),
   whose "fast path costs nothing" was written for a callee "whose arity is known".
2. **Make the arity visible to the importer.** An `Interface` carries each exported value's
   arity, `ReferenceKind::Foreign` carries it into the IR, `Saturation` is computed for it as
   it is for a `TopLevel` name, and the emitter calls `f(a, b)` directly and writes
   `$curry(f, n)` for a foreign function used as a value. That keeps the fast path across
   modules. It does not by itself answer `add`, whose *declared* arity is zero, since it is
   written `add = Js.Basics.addInt`: either the declaration is emitted curried (its aliased
   function's arity is known from the facade's signature), or a parameterless binding whose
   type is a function is given the arity of what it names, which is a further language-level
   decision about eta-expansion and is the language owner's.

Whichever is chosen, the *Calls* section of the module doc and `DEC-18` decision 3 are updated
in the same diff to state what an export is.

**Acceptance:**

- A fixture package under `tests/fixtures/` whose `tests/` root holds three `Test` values: one
  that calls a two-parameter function declared in another `src/` module, one that uses `n + n`,
  and `Test.equal 1 1`. `cargo run -- test tests/fixtures/<it>` reports all three passing and
  exits 0; paste the run into the PR. Reverting the fix makes it report all three errored.
- `tests/javascript.rs` pins the text of a cross-module call and of an exported
  multi-parameter function, and the pin goes red when the fix is reverted.
- `cargo run -- compile std/core` still prints `parsed 8 modules`, lists all eight, and exits 0.
- `cargo test --workspace` does not invoke `node`.
