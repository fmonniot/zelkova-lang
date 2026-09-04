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

The JavaScript underneath cannot honour that type. `_Utils_cmp` compares a non-object with
`<`, and otherwise assumes the value is a tuple, reading `.a`, `.b` and `.c` off it:

```js
function _Utils_cmp(x, y, ord) {
    if (typeof x !== 'object') {
        return x === y ? 0 : x < y ? -1 : 1;
    }
    return (ord = _Utils_cmp(x.a, y.a)) ? ord : /* … */;
}
```

Handed a value of a user union type — an object that is not a tuple — it recurses into three
`undefined` fields and returns a comparison of nothing against nothing. `append` is the same
shape: it branches on `typeof xs === 'string'` and otherwise treats both arguments as lists.

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
it because the declared type genuinely does accept it; code generation does not exist yet;
and `docs/spec/js-interop.md` already records that *which types may cross the boundary* is
undesigned ([SPEC-18](spec-18.md)), so there is no facade-level rule to appeal to either.

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

**Acceptance:** `_Utils_cmp` and `append` fail loudly on a value they cannot compare or
concatenate, with a test in whatever harness covers the `.mjs` files by then. `cargo run`
still prints `parsed 8 modules` and lists all eight as checked.
