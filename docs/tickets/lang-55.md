# LANG-55 · The `Char` and `String` default imports bring their modules but not their types

**Sizing:** small. Two fields of one const, and the tests that count what an entry brings.

**Location:** `src/compiler/default_imports.rs` — the `Char` and `String` entries of
`DEFAULT_IMPORTS` at :133-141, both `Unqualified::Nothing`.

**Decided ([`docs/spec/modules.md`](../spec/modules.md), *The default imports*, by the language
owner):** the list reads `import Char exposing (Char)` and `import String exposing (String)`,
so both types are writable unqualified in every module that keeps the entry.

**Problem:** the two entries are `Unqualified::Nothing`, the same treatment as `Tuple`, which
has no type to expose. So `Char.toUpper` and `String.length` are reachable and the types `Char`
and `String` are not: an annotation has to write `String.String`.

Every other entry whose module has a type exposes it — `List`, `Maybe`, `Result` and `Task` —
which is the chapter's own reason for the list existing at all: the types appearing in ordinary
type annotations are always writable. `Char` and `String` are also two of the five [scalar
types](../spec/types.md#scalar-types), and the other three arrive unqualified through `Basics`.

Nothing observes this today. `std/core` ships neither module — `Char.ignored` and
`String.ignored` — so both entries bring nothing at all, and there is no string literal to
annotate either. It becomes visible the day those modules compile, which is why it is filed
rather than left to be noticed then.

**Fix:** set both entries to `Unqualified::Type`, the variant `List` and `Task` already use.

**Acceptance:** a module that keeps both entries resolves `Char` and `String` as type names
with nothing written at the top of it, and still reaches `String.length` qualified — a test in
`src/compiler/default_imports.rs` or `canonical/environment.rs`, checked by reverting the two
fields and watching it go red. The clause naming this ticket in *The default imports*' final
`**Known gap:**` paragraph is deleted. `cargo run` still prints `parsed 8 modules`, lists all
eight as checked, and exits 0.
