# JS interoperability

A user can mark a Zelkova module as being a JavaScript interface. This is done by
using the `javascript` modifier after the `module` keyword. When declaring a JS
module, only function signatures are accepted — no bodies, no infix declarations, no
type declarations. The compiler also accepts only a subset of the Zelkova standard
types as parameters and return types for these signatures; if you need more
expressiveness, use a JSON data type.

This is the **only** way into JavaScript. Zelkova has no privileged internal escape
hatch — no module the standard library may use and a user's package may not — so a
standard-library module that needs a JavaScript primitive declares a facade in exactly
the syntax written here. Ordinary code and `std/core` reach the runtime the same way,
which is the property this design exists to preserve.

The idea is close to TypeScript's type definitions, with the difference that Zelkova
is less permissive in what types the functions can use — by design, only things
verifiable by the runtime are let through, to a certain extent.

A `javascript module` is only usable from inside the package that declares it. The
module name is not exposed to any other package.

Each facade module is paired with a companion `.mjs` file of the same base name
(`Js/Basics.zel` next to `Js/Basics.mjs`) holding the actual implementation. That file
is an ES module, and its exports take a **plain parameter list**: a Zelkova function
of two arguments is backed by a JavaScript function of two arguments, called directly.
There is no curried-wrapper convention to observe on the JavaScript side — currying is
the compiler's business, and a hand-written interop file should not have to know how
it is done. `std/core/src/Js/Basics`, `Js/Utils` and `Js/Bitwise` are the worked
examples.

Example — a reduced version of `std/core/src/Js/Basics.zel`, keeping only the
declarations its `exposing` list names so it stands alone as a compiling module (the
real file exposes many more):

```zel expect=ok
module javascript Js.Basics exposing
  ( add
  , sub
  )

add : a -> a -> a
sub : a -> a -> a
```

`a` there is an ordinary type variable and constrains nothing; a facade's signature says which
types its JavaScript really handles only as far as the types it names.

**Not implemented:** a facade signature may never carry a class constraint, and that restriction
exists to protect the plain parameter list above. A class is normally implemented by passing a
function a hidden table of its class's operations, which is exactly the calling convention this
file is promised it will never see — so a constrained function lives in ordinary Zelkova and
calls a monomorphic facade underneath it. The table does not survive to runtime either: a
constrained function is specialised per type before code is generated.
[Type classes](type-classes.md#a-constrained-function-may-not-be-a-javascript-facade) is the
chapter.

## Open questions

- **Which types may cross the boundary.** The paragraph above says "a subset of the
  Zelkova standard types" without saying which, and that is the substance of this
  chapter rather than a detail of it. The rule wanted is one a runtime check can
  enforce; naming the subset needs the types chapter first.
- **WebAssembly modules.** `javascript` is the only interop modifier the language
  defines. What the equivalent declaration for a WebAssembly-backed module would look
  like — a second modifier, a different mechanism entirely — is undesigned.
- **Values, not just functions.** Only signatures are accepted today, and every worked
  example is a function. Whether a facade may declare a constant is unsettled.
