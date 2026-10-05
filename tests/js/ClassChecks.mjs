// The JavaScript checks over what the compiler makes of a class, an instance and a constrained
// function (crates/zelkova-js/src/lib.rs, "Classes, instances and specialisations";
// crates/zelkova-compiler/src/ir/specialise.rs). `cargo test` pins the text each is emitted as
// (crates/zelkova-js/tests/javascript.rs) and does not run `node`
// (docs/decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node),
// so this file runs its fixture itself, with the compiler built from this checkout:
//
//   node --test 'tests/js/**/*.mjs'
//
// The fixture is tests/fixtures/package_classes. `tests/ClassTests.zel` holds the Zelkova tests,
// each judged by the value it computes, and `zelkova test` is run on it first. The rest of this
// file looks at what the tests cannot see: where each function is written, what it takes, what
// each module exports, and that two builds of one package write one text.
//
// Mutation-checked, each turning the test named red:
//   - `place_combine` (crates/zelkova-compiler/src/canonical/derivation.rs) substituting
//     `combine`'s first parameter for the expression that computes it, where it binds it once:
//     "zelkova test over the fixture" (the two chains of forty never finish, and the run is
//     stopped), and "a derived rank over two chains of forty links" for the same reason;
//   - the pass (crates/zelkova-compiler/src/ir/specialise.rs) placing each specialisation in the
//     module that declares the function: "a specialisation is in the module that uses it";
//   - `Emitter::binding` adding a parameter to a specialisation's function: "takes exactly the
//     parameters it was declared with";
//   - `exports` leaving out what `mentioned_by_copies` names: "a module exports what a copy of its
//     code names" (and `zelkova test` itself, which fails to link);
//   - `instance_member_name` leaving out the head: "an instance's member is one function of the
//     module that declares the instance";
//   - the pass reading a module's roots in a `HashMap`'s order: "two builds of one package write
//     one text".

import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import { readdirSync, readFileSync, rmSync, statSync } from "node:fs";
import { dirname, join, relative } from "node:path";
import { before, describe, test } from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";

const repository = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const fixture = join(repository, "tests", "fixtures", "package_classes");
const build = join(fixture, "build");
const output = join(build, "out", "js", "package-classes");
const testOutput = join(build, "test", "js", "package-classes");

// Long enough for a build and its tests, short enough that a chain that never finishes is
// stopped and reported rather than waited for.
const patience = 180_000;

let tests;
let Classes;
let Colour;
let Box;
let Shapes;
let Report;

function compile() {
  execFileSync("cargo", ["run", "--quiet", "--", "compile", fixture], {
    cwd: repository,
    stdio: ["ignore", "ignore", "inherit"],
  });
}

// Every file below `directory`, by its path relative to it.
function snapshot(directory) {
  const files = {};
  const walk = (current) => {
    for (const entry of readdirSync(current).sort()) {
      const path = join(current, entry);
      if (statSync(path).isDirectory()) {
        walk(path);
      } else {
        files[relative(directory, path)] = readFileSync(path, "utf8");
      }
    }
  };
  walk(directory);
  return files;
}

const text = (file) => readFileSync(join(output, file), "utf8");

// The one name in `names` that has `part` in it.
function exported(module, part) {
  const matching = Object.keys(module).filter((name) => name.includes(part));
  assert.equal(matching.length, 1, `exactly one export has ${part} in it: ${Object.keys(module)}`);
  return module[matching[0]];
}

before(async () => {
  tests = spawnSync("cargo", ["run", "--quiet", "--", "test", fixture], {
    cwd: repository,
    encoding: "utf8",
    timeout: patience,
  });
  compile();
  Classes = await import(pathToFileURL(join(output, "Classes.mjs")));
  Colour = await import(pathToFileURL(join(output, "Colour.mjs")));
  Box = await import(pathToFileURL(join(output, "Box.mjs")));
  Shapes = await import(pathToFileURL(join(output, "Shapes.mjs")));
  Report = await import(pathToFileURL(join(output, "Report.mjs")));
});

describe("zelkova test over the fixture", () => {
  test("passes every test, and runs at least one", () => {
    assert.match(tests.stdout ?? "", /^([1-9]\d*) tests: \1 passed, 0 failed, 0 errored$/m);
    assert.equal(tests.status, 0, `${tests.error ?? ""}${tests.stderr}`);
  });
});

describe("a specialisation", () => {
  test("is in the module that uses it, and not in the module that declares the function", () => {
    // `Classes` declares `smaller`; `Report` and the tests use it at `Colour`, which `Colour`
    // declares an instance for, in a module `Classes` could not import.
    assert.doesNotMatch(text("Classes.mjs"), /\$spec\$/);
    assert.doesNotMatch(text("Classes.mjs"), /Colour\.mjs|Report\.mjs|Box\.mjs|Shapes\.mjs/);
    assert.match(text("Report.mjs"), /function \$spec\$\d+\$smaller\(/);
    assert.match(readFileSync(join(testOutput, "ClassTests.mjs"), "utf8"), /function \$spec\$\d+\$smaller\(/);
  });

  test("is not exported under any name: a copy is the module's own", () => {
    assert.deepEqual(
      Object.keys(Report).sort(),
      ["boxedSameness", "boxedSmaller", "pickInts", "pickSmaller", "sameShapes", "smallestColour"],
    );
    assert.equal("smaller" in Classes, false, "a constrained declaration has no function of its own");
    assert.equal("smallest" in Classes, false);
  });

  test("takes exactly the parameters it was declared with, and no more", () => {
    const report = text("Report.mjs");
    assert.match(report, /function \$spec\$\d+\$smaller\(x, y\) \{/);
    assert.match(report, /function \$spec\$\d+\$smallest\(x, y, z\) \{/);
    assert.match(report, /function \$spec\$\d+\$sameWhenRanked\(x, y\) \{/);

    // Whatever instance a copy is at is not an argument: the functions the module exports take
    // the arguments the Zelkova declaration says.
    assert.equal(Report.pickSmaller.length, 2);
    assert.equal(Report.smallestColour.length, 3);
  });

  test("calls the instance's function directly, with no table of operations", () => {
    const report = text("Report.mjs");
    assert.doesNotMatch(report, /\$dict|\$instances|\$table/);
    assert.match(
      report,
      /\$instance\$package_classes\$Classes\$Ranked\$package_classes\$Colour\$Colour\$rank\(x, y\)/,
    );
  });

  test("computes what the source says, at each type it is used at", () => {
    const red = { $: "Red" };
    const green = { $: "Green" };
    const blue = { $: "Blue" };

    assert.deepEqual(Report.pickSmaller(blue, green), green);
    assert.deepEqual(Report.pickSmaller(red, blue), red);
    assert.equal(Report.pickInts(9n, 8n), 8n);
    assert.deepEqual(Report.smallestColour(blue, green, red), red);
    assert.deepEqual(Report.boxedSmaller({ $: "Box", a: blue }, { $: "Box", a: green }), { $: "Box", a: green });
  });
});

describe("an instance's member", () => {
  test("is one function of the module that declares the instance, exported under a name built from the class, the head and the member", () => {
    const same = exported(Colour, "$instance$package_classes$Classes$Same$package_classes$Colour$Colour$same");
    const rank = exported(Colour, "$instance$package_classes$Classes$Ranked$package_classes$Colour$Colour$rank");

    assert.equal(same.length, 2);
    assert.equal(same({ $: "Red" }, { $: "Red" }), true);
    assert.equal(same({ $: "Red" }, { $: "Blue" }), false);
    assert.deepEqual(rank({ $: "Green" }, { $: "Blue" }), { $: "LT" });

    // `Colour`'s instance is not copied into the modules that use it.
    assert.doesNotMatch(text("Report.mjs"), /function \$instance\$/);
  });

  test("of an instance with no context is a function of the class's module for each scalar", () => {
    const same = exported(Classes, "$instance$package_classes$Classes$Same$zelkova_core$Basics$Int$same");
    assert.equal(same(3n, 3n), true);
    assert.equal(same(3n, 4n), false);
  });

  test("of an instance with a context is not a function of its module, but a copy of the module that uses it", () => {
    assert.equal(
      Object.keys(Box).some((name) => name.startsWith("$instance$")),
      false,
      `Box exports ${Object.keys(Box)}`,
    );
    // `Report` uses `smaller` and `differs` at `Box Colour`, so it holds the box instance's `rank`
    // and `same` at `Colour`. Their parameters are the patterns the instance wrote.
    assert.match(text("Report.mjs"), /function \$spec\$\d+\$rank\(\$0, \$1\) \{/);
    assert.match(text("Report.mjs"), /function \$spec\$\d+\$same\(\$0, \$1\) \{/);
  });
});

describe("a module exports what a copy of its code names", () => {
  test("a function of the class's module that the header does not list", () => {
    assert.match(readFileSync(join(fixture, "src", "Classes.zel"), "utf8"), /^module Classes exposing \(Same, Ranked, \(===\), differs, smaller, smallest, sameWhenRanked\)$/m);
    assert.equal(typeof Classes.isGreater, "function");
    assert.equal(Classes.isGreater({ $: "GT" }), true);
    assert.equal(Classes.isGreater({ $: "LT" }), false);
  });

  test("a function of the type's module that its instance with a context names", () => {
    assert.match(readFileSync(join(fixture, "src", "Box.zel"), "utf8"), /^module Box exposing \(Box\(\.\.\), unbox\)$/m);
    assert.equal(typeof Box.confirmed, "function");
  });
});

describe("a derived rank over two chains of forty links", () => {
  test("answers each part once, so it returns", () => {
    // Run in a process of its own so that a derivation which answers each part twice, and takes
    // 2 to the 40th steps, is stopped here and reported.
    const script = `
      import { $instance$package_classes$Classes$Ranked$package_classes$Shapes$Chain$rank as rank } from ${JSON.stringify(
        pathToFileURL(join(output, "Shapes.mjs")).href,
      )};
      import { chain } from ${JSON.stringify(pathToFileURL(join(output, "Shapes.mjs")).href)};
      const answer = rank(chain(40n, 1n), chain(40n, 2n));
      console.log(answer.$);
    `;
    const result = spawnSync("node", ["--input-type=module", "-e", script], {
      encoding: "utf8",
      timeout: 20_000,
    });
    assert.equal(result.stdout.trim(), "LT", `${result.error ?? ""}${result.stderr}`);
    assert.equal(result.status, 0);
  });

  test("is reached through the exported instance function", () => {
    assert.equal(typeof exported(Shapes, "Ranked$package_classes$Shapes$Chain$rank"), "function");
  });
});

describe("two builds of one package", () => {
  test("write one text, in the same order and under the same names", () => {
    const out = join(build, "out");
    const first = snapshot(out);
    assert.ok(Object.keys(first).length > 4, `the build wrote ${Object.keys(first)}`);

    for (let again = 0; again < 3; again++) {
      rmSync(build, { recursive: true, force: true });
      compile();
      const next = snapshot(out);
      assert.deepEqual(Object.keys(next), Object.keys(first));
      assert.deepEqual(next, first);
    }
  });
});
