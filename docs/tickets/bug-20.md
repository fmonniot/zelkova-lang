# BUG-20 · `Js.Utils`'s comparison and append facades declare a type the JavaScript cannot honour

**Severity:** high (miscompile — the type checker accepts a call whose runtime behaviour is
undefined, and there is no later phase that would catch it).

**Location:** `std/core/src/Js/Utils.zel` — the `lt`, `le`, `gt`, `ge`, `compare` and `append`
signatures; `std/core/src/Js/Utils.mjs` — `_Utils_cmp` and `append`, which are what those
signatures are facades for. `std/core/src/Basics.zel` re-exports every one of them under the
same type.

**Found:** while writing the Constrained type variables chapter (`SPEC-11`), which `SPEC-12`
has since superseded with [`docs/spec/type-classes.md`](../spec/type-classes.md).

**Problem:** the six facades accept any type. They did so before this ticket too — the
spellings were `comparable` and `appendable`, which the language gives no meaning to, so the
declared type has always been `a -> a -> Bool` — but the spellings read as a restriction and
hid it. `SPEC-11` rewrote them to `a` because that is what they mean, which makes the
over-promise structural rather than notational, and this ticket is the record of it.

The JavaScript underneath cannot honour that type. `_Utils_cmp` used to compare a non-object
with `<`, and otherwise assume the value is a tuple, reading `.a`, `.b` and `.c` off it:

```js
function _Utils_cmp(x, y, ord) {
    if (typeof x !== 'object') {
        return x === y ? 0 : x < y ? -1 : 1;
    }
    return (ord = _Utils_cmp(x.a, y.a)) ? ord : /* … */;
}
```

Handed a value of a user union type — an object that is not a tuple — it recursed into three
`undefined` fields and returned a comparison of nothing against nothing. `append` was the same
shape: it branched on `typeof xs === 'string'` and otherwise treated both arguments as lists.
That half is now fixed; see **Status** below. What is not fixed is the declared type, which is
what lets such a call be written at all.

So this checks clean and means nothing:

```zel
type Colour
  = Red
  | Blue

smaller : Colour
smaller =
  min Red Blue
```

Nothing in the compiler stands between that program and `_Utils_cmp`. Type checking accepts
it because the declared type genuinely does accept it, and code generation does not exist yet.
There is now a facade-level rule to appeal to —
[Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)
rejects a bare type variable in a facade signature, so all six of these are inadmissible — but
nothing enforces it: [`LANG-43`](lang-43.md) is that check, and rewriting these six into
monomorphic facades is part of its scope. It does not close this ticket, whose acceptance is
about the `.mjs` failing loudly rather than about the signatures.

**Approach:** this cannot be fixed by narrowing the annotation, because the language has no
way to write the restriction — that is the whole subject of
[`docs/spec/type-classes.md`](../spec/type-classes.md), and its answer is a class mechanism that
does not exist yet. Two things are separable, and only the first is available now:

- **Make the runtime say so.** `_Utils_cmp` and `append` should reject a value they cannot
  handle rather than returning a nonsense answer for it. That is a change to the `.mjs` files
  and needs no type-system work; it converts an undefined result into a diagnosable failure.
- **Make the type say so.** Blocked on the class mechanism. When it lands, these six get a
  real constraint and this ticket closes for the right reason.

Do the first and leave the ticket open, or wait for the second — but do not leave both undone
on the grounds that codegen has not started, because the ticket that starts codegen will not
be looking here.

**Status — the first is done and this ticket stays open for the second.**
`std/core/src/Js/Utils.mjs` now admits only what it can actually read: `_Utils_cmp` takes an
ordered primitive or an object shaped like one of the two tuple encodings that file's own
constructors build, and throws on anything else — a union value, a record, an array, a
function, `null` — naming what it was handed. `append` keeps the String case and throws
elsewhere, naming lists as not implemented. `tests/js/Utils.test.mjs` is the repository's first
JavaScript harness and pins all of it; run it with `node --test 'tests/js/*.test.mjs'`.

Two things that guard deliberately does *not* settle. It reads the object tuple encoding this
file was copied with, while
[Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)
says a tuple crosses as an array; [`GEN-2`](gen-2.md) is what chooses, and until it does, an
array is a value `_Utils_cmp` cannot compare and is rejected as one. And it is a runtime
failure, not a compile-time one — which is the half below that remains.

**Acceptance:** *(the first clause is met — see Status.)* `_Utils_cmp` and `append` fail loudly
on a value they cannot compare or concatenate, with a test in whatever harness covers the `.mjs`
files by then. `cargo run` still prints `parsed 8 modules` and lists all eight as checked. What
remains is the type half: the six signatures carry a real constraint, so `min Red Blue` is a
type error rather than a runtime throw. [`LANG-42`](lang-42.md) is that work and closes this
ticket.
