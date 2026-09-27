# DEC-20 · A test-dependency may depend on the package it tests

**Settled:** 2026-09-27, by the language owner (`SPEC-35`).
**Status:** live.
**Where the rule lives:**
[Packages — `test-dependencies`](../spec/packages.md#test-dependencies), and the pointer to it
from [One version of each](../spec/packages.md#one-version-of-each).

A test library is written against the package it tests. `zelkova-test` depends on
`zelkova-core`, and `zelkova-core` wants its own tests written with `zelkova-test`, so core's
`test-dependencies` name a package whose `dependencies` name core. Counted by package, that is
the chain `zelkova-core → zelkova-test → zelkova-core`, which the acyclicity rule rejects.

The chapter counts a package's two source roots as two nodes instead. The test-dependency's
edge back names the package's `src/`, which is compiled first; the test library is compiled
against it; the package's `tests/` is compiled last, against both. That graph has no cycle:
every package in it still has an order to be compiled in.

## Precedent

Cargo gives a `dev-dependency` that depends on its dependent this arrangement for its
integration tests: the library is built once, the dev-dependency is built against it, and the
tests under `tests/` are built against both. Its unit tests do not get it. They are a second
build of the library, one the dev-dependency never sees, so a type the dev-dependency names is
not the type the unit test holds. Cargo therefore gives the arrangement only to tests that see
the public API. Zelkova gives it to tests that reach the private modules too, because `src/` is
compiled once and `tests/` reaches its private modules from that same build.

Elm's `elm/core` has the same need, since it is tested with `elm-explorations/test`, which
depends on `elm/core`.

## Why only the root package

No package's `test-dependencies` are resolved except those of the package whose tests are being
run ([Tests](../spec/packages.md#tests)), so the exception has nowhere else to apply. A chain
that returns to the package through `dependencies` alone is still a cycle: the split into two
roots only helps an edge that starts at `tests/`.

## A separate test package was rejected

The other option needs no rule change: a third package, holding core's tests, depending on both
`zelkova-core` and `zelkova-test`. It was rejected because such a package sees only core's
public modules, and [Tests](../spec/packages.md#tests) holds that a package's tests must be able
to reach its private ones. A package whose internals could only be tested from outside would be
pushed into exposing them.
