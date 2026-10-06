// The JavaScript checks over a `Float` and a `String` literal pattern
// (crates/zelkova-js/src/lib.rs, `test_condition`; docs/spec/patterns.md#literal-patterns).
// `cargo test` pins the text each is emitted as (crates/zelkova-js/tests/javascript.rs) and does
// not run `node`
// (docs/decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node),
// so this file runs its fixture itself, with the compiler built from this checkout:
//
//   node --test 'tests/js/**/*.mjs'
//
// The fixture is tests/fixtures/package_literal_patterns. `tests/LiteralTests.zel` holds the
// Zelkova tests and `zelkova test` is run on it first; `src/Literals.zel` is loaded below and
// called with the values a Zelkova program cannot spell, `-0` and `NaN` among them, which is
// where IEEE 754 equality (docs/spec/evaluation-semantics.md#what-structural-equality-computes)
// is told apart from `Object.is`.
//
// Mutation-checked in the emitter (crates/zelkova-js/src/lib.rs, `test_condition`), each turning
// this file red:
//   - the `Float` arm writing `Object.is(value, literal)` in place of `===`: "a positive zero
//     pattern matches negative zero" and `zelkova test`'s `positivePatternMatchesNegativeZero`
//     are the only ones to fail;
//   - the `Float` arm writing the literal with an `n` suffix, as an `Int` is, or the `String` arm
//     writing the text unescaped: the fixture's module is no longer valid JavaScript, so the
//     `before` hook fails and nothing in the file runs;
//   - `typer::translate_pattern` without its `Float` or its `String` arm: the fixture no longer
//     compiles, for the same reason.

import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import { dirname, join } from "node:path";
import { before, describe, test } from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";

const repository = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const fixture = join(repository, "tests", "fixtures", "package_literal_patterns");
const output = join(fixture, "build", "out", "js", "package-literal-patterns");

let tests;
let Literals;

before(async () => {
  tests = spawnSync("cargo", ["run", "--quiet", "--", "test", fixture], {
    cwd: repository,
    encoding: "utf8",
  });
  execFileSync("cargo", ["run", "--quiet", "--", "compile", fixture], {
    cwd: repository,
    stdio: ["ignore", "ignore", "inherit"],
  });
  Literals = await import(pathToFileURL(join(output, "Literals.mjs")));
});

describe("zelkova test over the fixture", () => {
  test("passes every test, and runs at least one", () => {
    assert.match(tests.stdout, /^([1-9]\d*) tests: \1 passed, 0 failed, 0 errored$/m);
    assert.equal(tests.status, 0, tests.stderr);
  });
});

describe("a Float pattern", () => {
  test("matches the number its literal spells", () => {
    assert.equal(Literals.classifyFloat(2.5), 3n);
    assert.equal(Literals.classifyFloat(1), 2n);
  });

  test("matches a literal with no exact binary64 value as the nearest one", () => {
    assert.equal(Literals.classifyFloat(0.1), 1n);
    assert.equal(Literals.classifyFloat(0.1 + 0.2), 99n);
  });

  test("falls through to the wildcard on any other number", () => {
    assert.equal(Literals.classifyFloat(3.5), 99n);
  });

  test("a positive zero pattern matches negative zero", () => {
    assert.ok(Object.is(-0, -0) && !Object.is(-0, 0), "the argument below is a negative zero");
    assert.equal(Literals.classifyFloat(-0), 0n);
    assert.equal(Literals.classifyFloat(0), 0n);
  });

  test("a NaN matches no literal, and reaches the wildcard", () => {
    assert.equal(Literals.classifyFloat(NaN), 99n);
  });

  test("a literal too large for binary64 is infinity", () => {
    assert.equal(Literals.classifyFloat(Infinity), 4n);
    assert.equal(Literals.classifyFloat(-Infinity), 99n);
    assert.equal(Literals.classifyFloat(Number.MAX_VALUE), 99n);
  });
});

describe("a String pattern", () => {
  test("matches the text its literal holds", () => {
    assert.equal(Literals.classifyString("hello"), 1n);
    assert.equal(Literals.classifyString(""), 0n);
  });

  test("matches a literal holding escapes", () => {
    assert.equal(Literals.classifyString('say "hi"\n'), 2n);
    assert.equal(Literals.classifyString('say "hi"'), 99n);
  });

  test("is case sensitive and exact", () => {
    assert.equal(Literals.classifyString("Hello"), 3n);
    assert.equal(Literals.classifyString("hello!"), 99n);
    assert.equal(Literals.classifyString("hell"), 99n);
  });
});
