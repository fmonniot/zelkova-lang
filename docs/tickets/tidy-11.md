# TIDY-11 · `BUG-20` and `Js/Utils.mjs` describe a tuple encoding and a test file that no longer match the tree

**Sizing:** small. Prose only, in one ticket file and two comments. What could make it bigger:
deciding, while rewording, that `_Utils_cmp` should read the array encoding — which is
[`BUG-20`](bug-20.md)'s or [`LANG-42`](lang-42.md)'s to do, not this ticket's.

**Location:** `docs/tickets/bug-20.md` — its **Status** paragraph, which cites
`tests/js/Utils.test.mjs` and `node --test 'tests/js/*.test.mjs'`, and the paragraph after it,
which says [`GEN-2`](README.md) "is what chooses" the tuple encoding. `std/core/src/Js/Utils.mjs` —
the comment above `_Utils_tupleArity`, which says code generation "(GEN-2) has not chosen
between that encoding and the object one this file reads", and the comment inside `append` that
names `GEN-2` as where a list's encoding comes from.

**Problem:** three statements the next reader will trust are no longer true.

- `tests/js/Utils.test.mjs` does not exist. The checks moved to
  `std/core/tests/Js/UtilsChecks.mjs` with [`TEST-4`](README.md), and CI runs them with
  `node --test 'std/core/tests/**/*.mjs'`. `tests/js/` exists again, holding `BoundaryChecks.mjs`,
  which makes the stale path look plausible.
- The tuple encoding is chosen. Code generation builds a tuple as a JavaScript array —
  `javascript.rs`' `TypedTermKind::Tuple` arm writes `[a, b]`, and a pattern reads `base[index]` —
  as [Which types may cross the boundary](../spec/interop.md#which-types-may-cross-the-boundary)
  says, and `GEN-2`, now closed, checks a returned tuple as an array of its length.
- A list's encoding is not `GEN-2`'s: it is unpublished, and belongs to whatever implements lists
  ([`LANG-44`](lang-44.md), [`LANG-46`](lang-46.md)).

**Approach:** point `bug-20.md` at `std/core/tests/Js/UtilsChecks.mjs` and its real command, and
reword its tuple paragraph to say that tuples cross as arrays and `_Utils_cmp` still reads only
the object encoding, which is why it throws on an emitted tuple today, describing it as an array.
Reword the two `Utils.mjs` comments to state the same facts without citing `GEN-2` as
undecided. Change no behaviour.

**Acceptance:** `git grep -n -e "tests/js/Utils.test.mjs" -e "(GEN-2)" -- docs/tickets/bug-20.md
std/core/src/Js/Utils.mjs` prints nothing, and
`node --test 'std/core/tests/**/*.mjs'` still passes.

**Found:** while closing [`GEN-2`](README.md), grepping for the references to repoint. Left
unfixed there because `Utils.mjs` is a companion whose behaviour that ticket did not touch, and
`BUG-20`'s account of `_Utils_cmp` deserves a reader who checks it rather than a
find-and-replace.
