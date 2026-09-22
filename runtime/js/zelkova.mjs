/*
The JavaScript runtime: the two things emitted code needs and cannot repeat at every call site
without duplicating itself in every module (`GEN-8`;
docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper).

A Zelkova declaration is emitted as a plain n-ary JavaScript function, and a saturated call at a
known callee is a direct call — the fast path, and this module is not on it. Everything else
goes through `$curry`:

  - an application that supplies fewer arguments than the callee's arity produces a function
    value (docs/spec/evaluation-semantics.md#function-values), so it accumulates what it has
    until there is enough;
  - an application that supplies more — over-application: `f 1 2` where `f` takes one argument
    and returns a function is two applications the emitted call site may not know apart — hands
    the surplus to whatever the first `arity` arguments produced, with a plain JavaScript call.
    That plain call is always correct by induction: every function value this runtime hands
    around is either a reference to a declaration, called elsewhere only when the call site
    knows it is saturated, or something `$curry` itself produced, which already knows how to
    accumulate or finish. Nothing a `$curry` result is applied to is a bare, arity-less closure.

`$abort` is the other piece the backend does not generate.
docs/spec/evaluation-semantics.md#when-a-program-aborts is when the runtime can no longer keep
the language's guarantees: it stops without producing a value and without running any more of
itself. A thrown JavaScript error is the obvious way to do that on Node — nothing in Zelkova
catches (docs/spec/evaluation-semantics.md#two-outcomes), so nothing here needs to be caught
either — and a throw can never be returned or bound the way a sentinel value could, so it cannot
be mistaken for one. The description says what caused the abort and is carried on the thrown
error's `message`.

Equality, comparison and arithmetic are deliberately absent from this file: those are ordinary
functions behind the `Js/Basics` and `Js/Utils` facades (`std/core/src/Js/`), and a second copy
here is how the two drift apart.
*/

// CURRYING

// `$curry(fn, arity)` wraps a plain n-ary function `fn` — the shape a declaration of `arity`
// parameters is emitted as — so that a call is total regardless of how many arguments it is
// given:
//
//   - fewer than `arity`: returns a function accumulating the rest. Any number of calls, each
//     supplying any number of arguments, works — `curried(1)(2, 3)`, `curried(1, 2)(3)` and
//     `curried(1, 2, 3)` all reach `fn` the same way.
//   - `arity` or more: calls `fn` with the first `arity` of them. Anything left over is
//     over-application and is applied to whatever `fn` returned, with a plain call — see this
//     module's header comment for why that call is always safe.
export function $curry(fn, arity) {
  return function accumulated(...args) {
    if (args.length < arity) {
      return $curry((...rest) => fn(...args, ...rest), arity - args.length);
    }

    const result = fn(...args.slice(0, arity));
    const surplus = args.slice(arity);
    return surplus.length === 0 ? result : result(...surplus);
  };
}

// ABORTING

// `$abort(description)` stops the program: it throws, and nothing in generated code — or in
// the language itself — catches. `description` says what caused the abort
// (docs/spec/evaluation-semantics.md#when-a-program-aborts) and is the thrown error's message.
export function $abort(description) {
  throw new Error(description);
}
