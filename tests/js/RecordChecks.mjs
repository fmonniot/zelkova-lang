// The JavaScript checks over what `zelkova_js::emit` makes of a record, an access, an update, an
// accessor and a record pattern, and of the predicate that decides a record where it crosses a
// facade (crates/zelkova-js/src/lib.rs, "Representations" and "The boundary check";
// docs/spec/interop.md#which-types-may-cross-the-boundary). `cargo test` pins the text each is
// emitted as (crates/zelkova-js/tests/javascript.rs) and does not run `node`
// (docs/decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node),
// so this file runs its fixture itself, with the compiler built from this checkout:
//
//   node --test 'tests/js/**/*.mjs'
//
// The fixture is tests/fixtures/package_records. `tests/RecordTests.zel` holds the Zelkova tests,
// each judged by the value it computes, and `zelkova test` is run on it first. `src/Records.zel`
// is the Zelkova the tests call and is loaded below, to look at the objects it builds
// rather than at what they compute; `src/Source.zel` is a facade whose companion, `Source.mjs`,
// returns a record of the declared shape from the exports named `right…` and a value of the wrong
// one from those named `wrong…`; `src/App.zel` is a program `zelkova run` runs.
//
// Mutation-checked in the emitter (crates/zelkova-js/src/lib.rs), each turning a test red:
//   - `Emitter::record_fields` emitting the fields in label order: "fields run in the order
//     written" and `zelkova test`'s `recordFieldsRunInTheOrderWritten`;
//   - the update's spread dropped, or written after the fields: "an update answers a new
//     object" and `updateEvaluatesItsRecordBeforeItsFields`;
//   - `Object.assign` in place of the spread: "an update answers a new object";
//   - `mangle` applied to a label in `key` and `property`: "a reserved word and an inherited
//     name are fields of the record" and `zelkova test`'s `reservedAndInheritedLabelsAreFields`;
//   - `Predicates::test` for a record with the `Reflect.ownKeys` count deleted: "an extra
//     field", "a symbol key" and "a non-enumerable property" abort tests;
//   - `Object.hasOwn` replaced by `in`: "a field only the prototype has";
//   - `Object.hasOwn` deleted: "a missing field of type ()" and the same abort test;
//   - the `!Array.isArray` and `!== null` tests deleted: the abort test "on null, and on an array".

import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import { dirname, join } from "node:path";
import { before, describe, test } from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";

const repository = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const fixture = join(repository, "tests", "fixtures", "package_records");
const output = join(fixture, "build", "out", "js", "package-records");

let tests;
let Records;
let Source;

before(async () => {
  tests = spawnSync("cargo", ["run", "--quiet", "--", "test", fixture], {
    cwd: repository,
    encoding: "utf8",
  });
  execFileSync("cargo", ["run", "--quiet", "--", "compile", fixture], {
    cwd: repository,
    stdio: ["ignore", "ignore", "inherit"],
  });
  Records = await import(pathToFileURL(join(output, "Records.mjs")));
  Source = await import(pathToFileURL(join(output, "Source.mjs")));
});

// What `$abort` throws for `export`, whose declared result type is `type`.
function aborted(exported, type) {
  return {
    message: `\`Source.${exported}\`'s companion returned a value its declared type, \`${type}\`, does not admit`,
  };
}

describe("zelkova test over the fixture", () => {
  test("passes every test, and runs at least one", () => {
    assert.match(tests.stdout, /^([1-9]\d*) tests: \1 passed, 0 failed, 0 errored$/m);
    assert.equal(tests.status, 0, tests.stderr);
  });
});

describe("a record, as the object it is", () => {
  test("is a plain object of its labels, with no tag", () => {
    const point = Records.point(1n, 2n);
    assert.deepEqual(point, { x: 1n, y: 2n });
    assert.equal(Object.getPrototypeOf(point), Object.prototype);
    assert.equal("$" in point, false);
  });

  test("holds its fields in the order they were written, and ran them in it", () => {
    const stamped = Records.stamped(0n);
    assert.deepEqual(Object.keys(stamped), ["b", "a"]);
    assert.ok(stamped.b < stamped.a, `b ran at ${stamped.b} and a at ${stamped.a}`);
  });

  test("is read by a property read, as an access, an accessor and a pattern read it", () => {
    const point = Records.point(7n, 8n);
    assert.equal(Records.getX(point), 7n);
    assert.equal(Records.readX(point), 7n);
    assert.equal(Records.parameterSum(point), 15n);
    assert.equal(Records.classify(point), 15n);
  });

  test("nests in a record and in a constructor", () => {
    const circle = Records.circle(1n, 2n, 3n);
    assert.deepEqual(circle, { $: "Circle", a: { centre: { x: 1n, y: 2n }, radius: 3n } });
    assert.equal(Records.centreX(circle.a), 1n);
  });
});

describe("an update", () => {
  test("answers a new object and leaves the old one alone", () => {
    const point = Records.point(1n, 2n);
    const moved = Records.moveX(10n, point);

    assert.notEqual(moved, point);
    assert.deepEqual(moved, { x: 11n, y: 2n });
    assert.deepEqual(point, { x: 1n, y: 2n });
  });

  test("evaluates its record before its fields, once, and its fields in the order written", () => {
    const stamped = Records.stampedUpdate(0n);
    assert.ok(stamped.c < stamped.b, "the record was evaluated first");
    assert.ok(stamped.b < stamped.a, "b was written before a");
    assert.equal(Records.updatedOnce(0n), 2n);
  });

  test("keeps a frozen record frozen and answers one that is not", () => {
    const frozen = Object.freeze({ x: 1n, y: 2n });
    const moved = Records.moveX(1n, frozen);
    assert.deepEqual(moved, { x: 2n, y: 2n });
    assert.equal(Object.isFrozen(moved), false);
    assert.deepEqual(frozen, { x: 1n, y: 2n });
  });
});

describe("a reserved word and an inherited name are fields of the record", () => {
  test("each is an own property, written as the label", () => {
    const keyed = Records.keyed(1n);
    assert.deepEqual(Object.keys(keyed), ["class", "new", "constructor", "toString"]);
    for (const label of ["class", "new", "constructor", "toString"]) {
      assert.ok(Object.hasOwn(keyed, label), `${label} is an own property`);
    }
    assert.equal(keyed.constructor, 3n);
    assert.equal(keyed.toString, 4n);
  });

  test("an access, an accessor, a pattern and an update reach the own property", () => {
    const keyed = Records.keyed(1n);
    assert.equal(Records.readKeyed(keyed), 4321n);
    assert.equal(Records.reachConstructor(keyed), 3n);
    assert.equal(Records.patternKeyed(keyed), 13n);

    const replaced = Records.replaceConstructor(keyed);
    assert.equal(replaced.constructor, 90n);
    assert.equal(keyed.constructor, 3n);
  });
});

describe("a record a companion returns of its declared type", () => {
  test("is returned unchanged", () => {
    assert.deepEqual(Source.rightPoint(3n), { x: 3n, y: 4n });
    assert.deepEqual(Source.rightNested(1n), { inner: { x: 1n }, pair: [2n, { y: 3n }] });
    assert.deepEqual(Source.rightWrapped(8n), { $: "Wrapper", a: { n: 8n } });
  });

  test("holds a field of type () as a property that is present", () => {
    const unit = Source.rightUnit(6n);
    assert.ok(Object.hasOwn(unit, "done"));
    assert.equal(unit.done, undefined);
  });

  test("may hold a reserved word and an inherited name", () => {
    assert.deepEqual(Source.rightKeyed(1n), { class: 1n, new: 2n, constructor: 3n, toString: 4n });
  });

  test("is handed to a companion as the plain object it is", () => {
    assert.equal(Source.sumFields(Records.point(3n, 4n)), 7n);
  });
});

describe("a value a companion returns that is not the record its signature declares", () => {
  const point = "{ x : Int, y : Int }";

  test("aborts on an extra field", () => {
    assert.throws(() => Source.wrongExtra(1n), aborted("wrongExtra", point));
  });

  test("aborts on a missing field", () => {
    assert.throws(() => Source.wrongMissing(1n), aborted("wrongMissing", point));
  });

  test("aborts on a field of the wrong type", () => {
    assert.throws(() => Source.wrongType(1n), aborted("wrongType", point));
  });

  test("aborts on null, and on an array", () => {
    assert.throws(() => Source.wrongNull(1n), aborted("wrongNull", point));
    assert.throws(() => Source.wrongArray(1n), aborted("wrongArray", point));
    assert.throws(() => Source.wrongArrayLength(1n), aborted("wrongArrayLength", "{ length : Float }"));
  });

  test("aborts on a missing field of type ()", () => {
    assert.throws(
      () => Source.wrongUnitMissing(1n),
      aborted("wrongUnitMissing", "{ done : (), n : Int }"),
    );
  });

  test("aborts on a field only the prototype has", () => {
    assert.throws(() => Source.wrongInherited(1n), aborted("wrongInherited", "{ toString : Int }"));
  });

  test("aborts on a symbol key beside the fields", () => {
    assert.throws(() => Source.wrongSymbol(1n), aborted("wrongSymbol", "{ x : Int }"));
  });

  test("aborts on a non-enumerable property beside the fields", () => {
    assert.throws(() => Source.wrongHidden(1n), aborted("wrongHidden", "{ x : Int }"));
  });

  test("aborts on a record that is wrong in a tuple in a record", () => {
    assert.throws(
      () => Source.wrongNested(1n),
      aborted("wrongNested", "{ inner : { x : Int }, pair : ( Int, { y : Int } ) }"),
    );
  });

  test("aborts on a record with a field to spare inside a constructor", () => {
    assert.throws(() => Source.wrongWrapped(1n), aborted("wrongWrapped", "Wrapper"));
  });
});

describe("zelkova run over a program that builds, updates and reads a record", () => {
  test("writes what it read", () => {
    const run = spawnSync("cargo", ["run", "--quiet", "--", "run", fixture], {
      cwd: repository,
      encoding: "utf8",
    });
    assert.match(run.stdout, /^point 41$/m);
    assert.equal(run.status, 0, run.stderr);
  });
});
