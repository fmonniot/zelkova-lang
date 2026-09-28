// The JavaScript checks over the boundary check `javascript::emit` puts in front of an
// `unsafe` facade's companion (src/compiler/javascript.rs, "The boundary check"): a value
// the companion hands back is run through the predicate of its declared type, returned
// unchanged when it passes, and aborts the program, naming the export, when it does not
// (docs/spec/interop.md#which-types-may-cross-the-boundary).
//
// This is the behavioural half of that check. `cargo test` pins the text it emits as
// (tests/javascript.rs) and does not run `node`
// (docs/decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node),
// so this file compiles its fixture itself, with the compiler built from this checkout,
// and loads the output:
//
//   node --test 'tests/js/**/*.mjs'
//
// The fixture is tests/fixtures/package_boundary_check: `Boundary` is the facade, its
// companion `Boundary.mjs` answers right and wrong shapes on purpose, and `App` is
// ordinary Zelkova calling it.
//
// Mutation-checked in `Emitter::facade_declaration`: returning `$returned` in place of
// the checked expression turns every `aborts` test below red, and prefixing the `Int`
// predicate with `false &&` turns every `returns … unchanged` test red — `Shape`'s
// constructors each carry an `Int`.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { dirname, join } from "node:path";
import { before, describe, test } from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";

const repository = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const fixture = join(repository, "tests", "fixtures", "package_boundary_check");
const output = join(fixture, "build", "out", "js", "package-boundary-check");

let Boundary;
let App;

before(async () => {
  execFileSync("cargo", ["run", "--quiet", "--", "compile", fixture], {
    cwd: repository,
    stdio: ["ignore", "ignore", "inherit"],
  });
  Boundary = await import(pathToFileURL(join(output, "Boundary.mjs")));
  App = await import(pathToFileURL(join(output, "App.mjs")));
});

// What `$abort` throws for `export`, whose declared result type is `type`.
function aborted(exported, type) {
  return {
    message: `\`Boundary.${exported}\`'s companion returned a value its declared type, \`${type}\`, does not admit`,
  };
}

describe("a value the companion returns of its declared type", () => {
  test("returns an Int unchanged", () => {
    assert.equal(Boundary.rightInt(41n), 42n);
  });

  test("returns an Int unchanged through a Zelkova caller", () => {
    assert.equal(App.next(41n), 42n);
  });

  test("returns a union value unchanged through a Zelkova caller", () => {
    assert.deepEqual(App.shape(3n), { $: "Square", a: 3n });
  });
});

describe("a value the companion returns of the wrong shape", () => {
  test("aborts on a string where the signature says Int", () => {
    assert.throws(() => Boundary.wrongIntString(1n), aborted("wrongIntString", "Int"));
  });

  test("aborts on a number where the signature says Int", () => {
    assert.throws(() => Boundary.wrongIntNumber(1n), aborted("wrongIntNumber", "Int"));
  });

  test("aborts on a bigint past the 64-bit range", () => {
    assert.throws(() => Boundary.wrongIntTooWide(0n), aborted("wrongIntTooWide", "Int"));
  });

  test("aborts on a constructor name the union does not declare", () => {
    assert.throws(
      () => Boundary.wrongShapeConstructor(1n),
      aborted("wrongShapeConstructor", "Shape"),
    );
  });

  test("aborts on a declared constructor carrying an argument of the wrong type", () => {
    assert.throws(
      () => Boundary.wrongShapeArgument(1n),
      aborted("wrongShapeArgument", "Shape"),
    );
  });

  test("aborts rather than reaching the Zelkova caller", () => {
    assert.throws(() => App.broken(1n), aborted("wrongIntString", "Int"));
    assert.throws(() => App.brokenShape(1n), aborted("wrongShapeConstructor", "Shape"));
  });
});
