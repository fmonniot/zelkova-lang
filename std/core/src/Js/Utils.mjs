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

function _Utils_cmp(x, y, ord) {
    if (typeof x !== 'object') {
        return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
    }

    return (ord = _Utils_cmp(x.a, y.a))
        ? ord
        : (ord = _Utils_cmp(x.b, y.b))
            ? ord
            : _Utils_cmp(x.c, y.c);
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
    // append Strings
    if (typeof xs === 'string') {
        return xs + ys;
    }

    // append Lists
    if (!xs.b) {
        return ys;
    }
    var root = __List_Cons(xs.a, ys);
    xs = xs.b
    for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
    {
        curr = curr.b = __List_Cons(xs.a, ys);
    }
    return root;
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
