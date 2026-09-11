/*
Important reminder: this file has been copied as is from the Elm implementation.
It's very highly likely it won't work as if :)
*/

// EQUALITY

function eq(x, y) {
    for (
        var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
        isEqual && (pair = stack.pop());
        isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
    ) { }

    return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack) {
    if (x === y) {
        return true;
    }

    if (typeof x !== 'object' || x === null || y === null) {
        typeof x === 'function' && __Debug_crash(5);
        return false;
    }

    if (depth > 100) {
        stack.push(_Utils_Tuple2(x, y));
        return true;
    }

    /**__DEBUG/
    if (x.$ === 'Set_elm_builtin')
    {
        x = __Set_toList(x);
        y = __Set_toList(y);
    }
    if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
    {
        x = __Dict_toList(x);
        y = __Dict_toList(y);
    }
    //*/

    /**__PROD/
    if (x.$ < 0)
    {
        x = __Dict_toList(x);
        y = __Dict_toList(y);
    }
    //*/

    for (var key in x) {
        if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack)) {
            return false;
        }
    }
    return true;
}

export function equal(x, y) { return eq(x, y) }
export function notEqual(x, y) { return !eq(x, y) }



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

// `lt`/`le`/`gt`/`ge`/`compare` and `append` are declared over a bare type
// variable (BUG-20), so nothing stops them being handed a value this file has
// no way to read. What `_Utils_cmp` can actually read is what decides which
// values it may accept: its object branch reads `.a`, `.b` and `.c`, which is
// the tuple encoding the constructors further down this file build —
// `_Utils_Tuple2__PROD` builds `{a, b}` and `_Utils_Tuple2__DEBUG` builds
// `{$: '#2', a, b}`.
//
// Nothing else in the language is shaped that way. A union value carries its
// constructor's name in `$` with that constructor's arguments in those same
// `a`/`b`/`c` fields
// (docs/spec/interop.md#a-union-crosses-as-a-tagged-value), and a record is a
// plain object of the record's own field names; read either as a tuple and the
// answer is a comparison of fields that mean nothing, which is BUG-20.
//
// So the test below is an allowlist and not a blocklist: a value is comparable
// only if it is one of the two tuple shapes above, rather than everything
// carrying a `$` being rejected and the rest trusted. A blocklist would still
// let through every untagged object this file cannot read — a record, and an
// array in particular:
// docs/spec/interop.md#which-types-may-cross-the-boundary says a tuple and a
// list each cross the JavaScript boundary as an array, and code generation
// (GEN-2) has not chosen between that encoding and the object one this file
// reads. Until it does, an array is a value `_Utils_cmp` cannot compare, and
// saying so is the whole point of the guard.
//
// Returns the tuple's arity, or 0 for a value that is not a tuple.
function _Utils_tupleArity(v) {
    if (typeof v !== 'object' || v === null || Array.isArray(v)) {
        return 0;
    }

    var tag = null;
    var fields = [];
    for (var key of Object.keys(v)) {
        if (key === '$') {
            tag = v.$;
        } else {
            fields.push(key);
        }
    }

    var shape = fields.sort().join(',');
    if (shape === 'a,b' && (tag === null || tag === '#2')) {
        return 2;
    }
    if (shape === 'a,b,c' && (tag === null || tag === '#3')) {
        return 3;
    }
    return 0;
}

// `<` orders exactly these, and `_Utils_cmp`'s primitive branch is written for
// them. A function is not one of them: `f < g` compares two values that have
// no order and answers anyway, and `_Utils_eqHelp` above already refuses a
// function for equality.
function _Utils_isOrdered(v) {
    var t = typeof v;
    return t === 'number' || t === 'string' || t === 'boolean';
}

// Names a value the way the errors below talk about values, so a failure says
// what it was handed rather than only that it refused.
function _Utils_describe(v) {
    var arity = _Utils_tupleArity(v);
    if (arity !== 0) {
        return 'a ' + arity + '-tuple';
    }
    if (v === null) {
        return 'null';
    }
    if (Array.isArray(v)) {
        return 'an array';
    }
    if (typeof v === 'object') {
        return '$' in v
            ? 'a value of the user-defined constructor ' + String(v.$)
            : 'an object';
    }
    return 'a ' + typeof v;
}

function _Utils_cmp(x, y, ord) {
    // Both sides are tested, not just `x`. A guard reached only when `x` is an
    // object makes `compare(Red, 1)` throw while `compare(1, Red)` returns a
    // nonsense answer, and the nonsense answer is what BUG-20 is about.
    if (_Utils_isOrdered(x) && typeof y === typeof x) {
        return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
    }

    var arity = _Utils_tupleArity(x);
    if (arity !== 0 && _Utils_tupleArity(y) === arity) {
        ord = _Utils_cmp(x.a, y.a);
        if (ord !== 0) {
            return ord;
        }
        ord = _Utils_cmp(x.b, y.b);
        if (ord !== 0 || arity === 2) {
            return ord;
        }
        return _Utils_cmp(x.c, y.c);
    }

    throw new Error(
        'compare: can only compare two numbers, two characters, two strings, ' +
        'two booleans, or two tuples of the same size holding these, but was ' +
        'given ' + _Utils_describe(x) + ' and ' + _Utils_describe(y)
    );
}

// We expose compare as a way to implement Basics.compare. This one returns a number,
// so we don't want to expose it.
export function compare(a, b) {
    return _Utils_cmp(a, b); 
}
export function lt(a, b) {  return _Utils_cmp(a, b) < 0 }
export function le(a, b) {  return _Utils_cmp(a, b) < 1 }
export function gt(a, b) {  return _Utils_cmp(a, b) > 0 }
export function ge(a, b) {  return _Utils_cmp(a, b) >= 0 }


// APPEND

export function append(xs, ys) {
    if (typeof xs === 'string' && typeof ys === 'string') {
        return xs + ys;
    }

    // `append` backs Elm's `(++)`, which concatenates two Strings or two
    // Lists, and String is the only one of the two this file can honour:
    // Zelkova has no list type at all — no literal syntax for one (LANG-44)
    // and no encoding for one to cross the JavaScript boundary (GEN-2) — so
    // every value that is not a String is one `append` cannot concatenate.
    //
    // Elm's cons-cell walk used to stand here. It called `__List_Cons`, an
    // identifier no file in this repository defines, so it could only ever
    // have raised a `ReferenceError` naming something the reader cannot find.
    // A throw that names the absent feature is what BUG-24 asks for in its
    // place; BUG-24's remaining scope is the same substitution in
    // `Js/Basics.mjs` and in `_Utils_eqHelp` above.
    throw new Error(
        'append: can only append two Strings — Lists are not implemented yet ' +
        '— but was given ' + _Utils_describe(xs) + ' and ' +
        _Utils_describe(ys)
    );
}

// TODO Script below have not been modified yet, which isn't an issue given it doesn't export anything
// COMMON VALUES

var _Utils_Tuple0__PROD = 0;
var _Utils_Tuple0__DEBUG = { $: '#0' };

function _Utils_Tuple2__PROD(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2__DEBUG(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3__PROD(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3__DEBUG(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr__PROD(c) { return c; }
function _Utils_chr__DEBUG(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields) {
    var newRecord = {};

    for (var key in oldRecord) {
        newRecord[key] = oldRecord[key];
    }

    for (var key in updatedFields) {
        newRecord[key] = updatedFields[key];
    }

    return newRecord;
}
