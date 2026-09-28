// The companion of `Boundary.zel`. Each `right…` export returns a value its signature
// admits; each `wrong…` export returns one it does not, for the boundary check the
// compiler emits in front of it to catch.

export function rightInt(n) {
  return n + 1n;
}

// A string where the signature says `Int`.
export function wrongIntString(n) {
  return String(n);
}

// A JavaScript number where the signature says `Int`, which crosses as a `bigint`.
export function wrongIntNumber(n) {
  return Number(n);
}

// A `bigint` past the 64-bit range an `Int` holds.
export function wrongIntTooWide(n) {
  return 2n ** 63n + n;
}

export function rightShape(n) {
  return { $: "Square", a: n };
}

// A constructor name `Shape` does not declare.
export function wrongShapeConstructor(n) {
  return { $: "Triangle", a: n };
}

// A constructor `Shape` declares, carrying a number where it declares an `Int`.
export function wrongShapeArgument(n) {
  return { $: "Circle", a: Number(n) };
}
