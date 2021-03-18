# Zelkova Lang

TODO: Reuse Elm's language definition/documentation, as we are a fork of the language.

## JS/WebAssembly interopability

A user can mark a Zelkova module as being a javascript interface. This is done by using the `javascript` modifier after the `module` keyword. When declaring a JS module, only functions signature are accepted.
The compiler will also accept only a subset of the Zelkova standard types as acceptable parameters. If you
need more expressiveness, please use a JSON data type.

The idea is extremely familiar to Typescript's type definition. With the difference that Zelkova is less
permissive in what types the functions can use (by design, we only let things which are verifiable by the
runtime to go through, to a certain extent).

Note that a `javascript module` is only useable from inside the package which declare it.
The module name isn't exposed to any other packages.


Example: 

```
module javascript Basics.Js exposing
  ( add, sub, mul, fdiv, idiv, pow
  , toFloat, round, floor, ceiling, truncate
  , not, and, or, xor,
  , modBy, remainderBy, sqrt, log, e
  , pi, cos, sin, tan, acos, asin, atan, atan2
  , isNaN, isInfinite
  )


add : number -> number -> number

sub : number -> number -> number

-- ...
```

TODO: WebAssembly modules
