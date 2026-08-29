# JS interoperability

A user can mark a Zelkova module as being a JavaScript interface. This is done by
using the `javascript` modifier after the `module` keyword. When declaring a JS
module, only function signatures are accepted — no bodies, no infix declarations, no
type declarations (`CLAUDE.md`'s *Zelkova has no `Elm.Kernel.*`* standing invariant).
The compiler also accepts only a subset of the Zelkova standard types as parameters
and return types for these signatures; if you need more expressiveness, use a JSON
data type.

The idea is close to TypeScript's type definitions, with the difference that Zelkova
is less permissive in what types the functions can use — by design, only things
verifiable by the runtime are let through, to a certain extent.

A `javascript module` is only usable from inside the package that declares it. The
module name is not exposed to any other package.

Each facade module is paired with a companion `.mjs` file of the same base name
(`Js/Basics.zel` next to `Js/Basics.mjs`) holding the actual implementation. Unlike
Elm's kernel modules, that file's exports take a plain parameter list rather than
Elm's curried `F2`/`F3` wrappers — see `CLAUDE.md`'s standing invariant on this for the
worked examples (`Js/Basics`, `Js/Utils`, `Js/Bitwise`).

Example — a reduced version of `std/core/src/Js/Basics.zel`, keeping only the
declarations its `exposing` list names so it stands alone as a compiling module (the
real file exposes many more):

```zel expect=ok
module javascript Js.Basics exposing
  ( add
  , sub
  )

add : number -> number -> number
sub : number -> number -> number
```

## Open questions

- Reuse Elm's language definition and documentation where it applies, since Zelkova
  started as a fork of the language. `docs/spec/` is itself the beginning of that
  work, but how much of Elm's own JS-interop story (if any) should be pulled in here
  has not been decided.
- WebAssembly modules: `javascript` is the only interop modifier the grammar accepts
  today (`src/compiler/parser/tokenizer.rs` groups it with the other soft keywords).
  What the equivalent declaration for a WebAssembly-backed module would look like — a
  second modifier, a different mechanism entirely — is undesigned.
