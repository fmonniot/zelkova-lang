// The companion of `Source.zel`. Each `right…` export returns a record its signature
// admits; each `wrong…` export returns a value it does not, for the boundary check the
// compiler emits in front of it to catch.

let ticks = 0n;

export function tick(_) {
  ticks += 1n;
  return ticks;
}

export function sumFields(point) {
  return point.x + point.y;
}

export function rightPoint(n) {
  return { x: n, y: n + 1n };
}

// A field the record type does not have.
export function wrongExtra(n) {
  return { x: n, y: n + 1n, z: n + 2n };
}

// A field the record type has, absent.
export function wrongMissing(n) {
  return { x: n };
}

// A number where the field's type says `Int`.
export function wrongType(n) {
  return { x: n, y: Number(n) };
}

export function wrongNull(_) {
  return null;
}

// An array carrying the right fields as properties: not a record.
export function wrongArray(n) {
  return Object.assign([], { x: n, y: n });
}

// An empty array has one own key, `length`, and a `Float` for it: the one count and the one
// field the record `{ length : Float }` asks for, so only the array test turns it away.
export function wrongArrayLength(_) {
  return [];
}

// A field of type `()` is present, holding `undefined`.
export function rightUnit(n) {
  return { done: undefined, n };
}

// A field of type `()` that is absent is not a present one, even with another field beside
// the fields it has to make the count right: `done` reads as `undefined` all the same.
export function wrongUnitMissing(n) {
  return { n, extra: n };
}

export function rightKeyed(n) {
  return { class: n, new: n + 1n, constructor: n + 2n, toString: n + 3n };
}

// `toString` is found on this object, as on any object, but through its prototype and not
// as an own property of its own: a check that reads inherited keys is satisfied by it.
export function wrongInherited(n) {
  return Object.assign(Object.create({ toString: n }), { other: n });
}

// The record's own field and a symbol-keyed one beside it.
export function wrongSymbol(n) {
  return { x: n, [Symbol("extra")]: n };
}

// The record's own field and a non-enumerable one beside it, which `Object.keys` does not
// list.
export function wrongHidden(n) {
  return Object.defineProperty({ x: n }, "hidden", { value: n, enumerable: false });
}

// A label that is an own property but not an enumerable one: `hasOwn` finds it and the field
// passes its type, but a spread would not copy it, so an update would lose it.
export function wrongHiddenLabel(n) {
  return Object.defineProperty({ x: n }, "y", { value: n + 1n, enumerable: false });
}

// Admitted, because only a value's own keys are asked of it: a frozen object, an instance of a
// class, and an object with no prototype at all, each with exactly the record's labels.
export function rightFrozen(n) {
  return Object.freeze({ x: n, y: n + 1n });
}

class Point {
  constructor(n) {
    this.x = n;
    this.y = n + 1n;
  }
}

export function rightInstance(n) {
  return new Point(n);
}

export function rightNullPrototype(n) {
  return Object.assign(Object.create(null), { x: n, y: n + 1n });
}

export function rightNested(n) {
  return { inner: { x: n }, pair: [n + 1n, { y: n + 2n }] };
}

// Wrong two levels down: a record in a tuple in a record, a field short.
export function wrongNested(n) {
  return { inner: { x: n }, pair: [n + 1n, {}] };
}

export function rightWrapped(n) {
  return { $: "Wrapper", a: { n } };
}

// A constructor's record argument with a field to spare.
export function wrongWrapped(n) {
  return { $: "Wrapper", a: { n, m: n } };
}

export function say(n) {
  console.log(`point ${n}`);
}
