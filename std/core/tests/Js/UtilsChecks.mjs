// The JavaScript checks over std/core/src/Js/Utils.mjs, the companion behind
// the Js.Utils facade (Js/Utils.zel).
//
// This is the companion of the test facade Js.UtilsChecks
// (tests/Js/UtilsChecks.zel), laid out as docs/spec/interop.md's "Testing a
// companion" describes. Each export is one check, declared there as
// `Task (Result Failure ())`: it returns nothing when the check holds, and a
// failed assertion throws, which the facade's wrapper turns into
// `Err (Threw ..)`. Js.UtilsTests (tests/Js/UtilsTests.zel) exposes one
// `Test` per check, so `zelkova test std/core` runs them.
//
// `lt`/`le`/`gt`/`ge`/`compare` and `append` are declared `a -> a -> ...`
// (BUG-20), a type their JavaScript cannot honour: handed a value of a
// user-defined union type, `_Utils_cmp` and `append` read undefined tuple and
// list fields off it and returned a nonsense answer instead of failing. The
// fix is a guard that admits only the shapes this file can actually read and
// throws on everything else.
//
// Two kinds of check live below, each labelled in the comment above its
// export, where a Zelkova name could not carry it:
//
//   PINS  — verified red against the file as it stood before the guard: either
//           the unguarded original, or a guard keyed on the presence of a `$`
//           field alone. These are the fix.
//   GUARD — passes with and without the guard. These pin that the fix did not
//           narrow what the file used to accept; they prove nothing about the
//           fix itself, so do not read a green one as a pinned new behaviour.
//
// Values are shaped the way docs/spec/interop.md says they cross to
// JavaScript, or the way this file's own constructors build them, since code
// generation does not exist yet to produce either from real Zelkova source.
// Utils.mjs is imported as a module of the target rather than reached through
// its facade, which is what lets these ask what it does with a value Js.Utils
// would have refused to pass it.

import assert from 'node:assert/strict';
// `Utils.mjs` no longer exports these eight under their bare names:
// `LANG-43` split each into a monomorphic `*Int`/`*Float` pair
// (`Js/Utils.zel`), and `Basics.zel` itself picks the `Int` one for its own
// re-export. The two aliases share one underlying function (see
// `Utils.mjs`), so importing the `Int` name under its old bare spelling
// below still exercises exactly what these tests exercised before, whatever
// the operands' types.
import {
    compareInt as compare, ltInt as lt, leInt as le, gtInt as gt,
    geInt as ge, appendInt as append, equalInt as equal,
    notEqualInt as notEqual,
} from '../../src/Js/Utils.mjs';

// A stand-in for `Colour = Red | Blue`, encoded as
// docs/spec/interop.md#a-union-crosses-as-a-tagged-value specifies.
const Red = { $: 'Red' };
const Blue = { $: 'Blue' };
const Rgb = (r, g, b) => ({ $: 'Rgb', a: r, b: g, c: b });

// The two encodings `_Utils_Tuple2`/`_Utils_Tuple3` build in this file.
const prodPair = (a, b) => ({ a, b });
const debugPair = (a, b) => ({ $: '#2', a, b });
const prodTriple = (a, b, c) => ({ a, b, c });
const debugTriple = (a, b, c) => ({ $: '#3', a, b, c });

// A cons list in the encoding `__List_Cons` used to build, which `append`'s
// deleted walk was written for.
const nil = { $: 0 };
const cons = (h, t) => ({ $: 1, a: h, b: t });

const cmpError = /compare: can only compare /;
const appendError = /append: can only append two Strings/;

// COMPARE — what it accepts

// GUARD compare orders numbers
export function compareOrdersNumbers() {
    assert.equal(compare(1, 2), -1);
    assert.equal(compare(2, 2), 0);
    assert.equal(compare(3, 2), 1);
}

// GUARD compare orders strings
export function compareOrdersStrings() {
    assert.equal(compare('a', 'b'), -1);
    assert.equal(compare('b', 'b'), 0);
}

// GUARD compare orders tuples in the PROD encoding
export function compareOrdersProdTuples() {
    assert.equal(compare(prodPair(1, 2), prodPair(1, 3)), -1);
    assert.equal(compare(prodPair(1, 2), prodPair(1, 2)), 0);
    assert.equal(compare(prodTriple(1, 2, 3), prodTriple(1, 2, 2)), 1);
}

// PINS compare orders tuples in the DEBUG encoding
export function compareOrdersDebugTuples() {
    // _Utils_Tuple2__DEBUG stamps `$: '#2'` on a perfectly legitimate tuple.
    // A guard keyed on the mere presence of `$` rejects it.
    assert.equal(compare(debugPair(1, 2), debugPair(1, 3)), -1);
    assert.equal(compare(debugTriple(1, 2, 3), debugTriple(1, 2, 3)), 0);
}

// GUARD compare stops at a 2-tuple instead of reading a third field
export function compareStopsAtAPairsArity() {
    // The walk now stops at the arity rather than recursing into `x.c`/`y.c`,
    // which are both `undefined` on a pair. The old walk reached the same
    // answer the long way round, so this only pins that it still does.
    assert.equal(compare(prodPair(1, 2), prodPair(1, 2)), 0);
    assert.equal(compare(prodPair(2, 1), prodPair(1, 1)), 1);
}

// COMPARE — what it refuses

// PINS compare refuses a union value
export function compareRefusesAUnionValue() {
    assert.throws(() => compare(Red, Blue), cmpError);
    assert.throws(() => compare(Rgb(1, 2, 3), Rgb(1, 2, 4)), cmpError);
}

// PINS compare refuses a union against a primitive, either way round
export function compareRefusesAUnionAgainstAPrimitive() {
    // The old guard sat behind the primitive branch, so only one order threw.
    assert.throws(() => compare(Red, 1), cmpError);
    assert.throws(() => compare(1, Red), cmpError);
}

// PINS compare refuses an array
export function compareRefusesAnArray() {
    // docs/spec/interop.md says a tuple and a list each cross as an array.
    // This file reads the object encoding, so an array is unreadable here —
    // and untagged, so a `$`-keyed guard waves it through and answers EQ.
    assert.throws(() => compare([1, 2], [1, 3]), cmpError);
    assert.throws(() => compare([1, 2, 3], [1, 2, 4]), cmpError);
}

// PINS compare refuses a record
export function compareRefusesARecord() {
    // _Utils_update builds plain untagged objects of the record's own fields.
    assert.throws(() => compare({ x: 1, y: 2 }, { x: 1, y: 3 }), cmpError);
}

// PINS compare refuses a function
export function compareRefusesAFunction() {
    assert.throws(() => compare(() => 1, () => 2), cmpError);
}

// PINS compare refuses null and undefined
export function compareRefusesNullAndUndefined() {
    assert.throws(() => compare(null, null), cmpError);
    assert.throws(() => compare(undefined, undefined), cmpError);
}

// PINS compare refuses tuples of different sizes
export function compareRefusesTuplesOfDifferentSizes() {
    assert.throws(() => compare(prodPair(1, 2), prodTriple(1, 2, 3)), cmpError);
}

// PINS compare refuses a union nested inside a tuple
export function compareRefusesANestedUnion() {
    // Here it is the recursion that has to catch it, not the entry point.
    assert.throws(() => compare(prodPair(1, Red), prodPair(1, Blue)), cmpError);
    assert.throws(
        () => compare(prodTriple(1, 2, Red), prodTriple(1, 2, Blue)),
        cmpError,
    );
}

// PINS the comparison operators refuse what compare refuses
export function comparisonOperatorsRefuseWhatCompareRefuses() {
    for (const op of [lt, le, gt, ge]) {
        assert.throws(() => op(Red, Blue), cmpError);
        assert.throws(() => op([1, 2], [1, 3]), cmpError);
    }
}

// GUARD the comparison operators still answer on numbers
export function comparisonOperatorsAnswerOnNumbers() {
    assert.equal(lt(1, 2), true);
    assert.equal(le(2, 2), true);
    assert.equal(gt(1, 2), false);
    assert.equal(ge(2, 2), true);
}

// COMPARE — Ints (LANG-65)
//
// An `Int` crosses as a `bigint` (docs/spec/interop.md#which-types-may-cross-
// the-boundary), which `_Utils_isOrdered` did not admit: `compare`, `lt`,
// `le`, `gt` and `ge` fell through to the tuple-arity branch and threw
// `cmpError` on two `Int`s, the same throw a genuinely unorderable value
// gets.

// PINS compare orders two Ints instead of throwing
export function compareOrdersTwoInts() {
    assert.equal(compare(3n, 5n), -1);
    assert.equal(compare(5n, 5n), 0);
    assert.equal(compare(5n, 3n), 1);
}

// PINS the comparison operators answer on two Ints instead of throwing
export function comparisonOperatorsAnswerOnTwoInts() {
    assert.equal(lt(3n, 5n), true);
    assert.equal(le(5n, 5n), true);
    assert.equal(gt(5n, 3n), true);
    assert.equal(ge(5n, 5n), true);
    assert.equal(lt(5n, 3n), false);
}

// APPEND

// GUARD append concatenates two strings
export function appendConcatenatesTwoStrings() {
    assert.equal(append('ab', 'cd'), 'abcd');
    assert.equal(append('', ''), '');
}

// PINS append refuses a string and a non-string
export function appendRefusesAStringAndANonString() {
    assert.throws(() => append('ab', 1), appendError);
    assert.throws(() => append(1, 'ab'), appendError);
    assert.throws(() => append('ab', Red), appendError);
}

// PINS append refuses a union value
export function appendRefusesAUnionValue() {
    assert.throws(() => append(Red, Blue), appendError);
}

// PINS append refuses a cons list, and says lists are not implemented
export function appendRefusesAConsList() {
    // The deleted walk was written for exactly this encoding and called an
    // undefined `__List_Cons` (BUG-24). Whatever it did, it never concatenated
    // two lists — so the error has to name lists as absent rather than claim
    // they are supported.
    assert.throws(() => append(cons(1, nil), cons(2, nil)), appendError);
    assert.throws(() => append(nil, nil), appendError);
}

// PINS append refuses arrays rather than returning one of them
export function appendRefusesArrays() {
    // The array encoding docs/spec/interop.md gives a list. Untagged, so a
    // `$`-keyed guard passes it to the walk, which returned `ys` unchanged.
    assert.throws(() => append([1, 2], [3, 4]), appendError);
}

// PINS append refuses a record and a function
export function appendRefusesARecordAndAFunction() {
    assert.throws(() => append({ x: 1 }, { x: 2 }), appendError);
    assert.throws(() => append(() => 1, () => 2), appendError);
}

// EQUALITY — the function case (BUG-24)

// `Eq` has no instance for a function type, so comparing two functions does
// not type-check and this path is unreachable from well-typed source — but
// `Js.Utils.equal` is declared `a -> a -> Bool` today (BUG-20) and accepts
// anything, so it is reachable now. `_Utils_eqHelp` used to call an undefined
// crash helper here, a `ReferenceError`; it now answers `false` instead of
// inventing a failure mode equality does not have.
// PINS comparing two functions for equality answers false rather than throwing
export function equalityOnTwoFunctionsIsFalse() {
    const f = () => 1;
    const g = () => 2;
    assert.equal(equal(f, g), false);
    assert.equal(notEqual(f, g), true);
}

// GUARD a function is equal to itself by reference
export function aFunctionEqualsItself() {
    const f = () => 1;
    assert.equal(equal(f, f), true);
    assert.equal(notEqual(f, f), false);
}
