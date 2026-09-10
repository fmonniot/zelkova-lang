# LANG-54 · The interop modifier is `foreign`, not `javascript`

**Sizing:** medium. One token, one grammar production and one field name, plus the three
`std/core` facades and the tests that name them. No new behaviour — every check that reads the
flag keeps reading it.

**Location:**

- `src/compiler/parser/tokenizer.rs` — `Token::Javascript` and the soft-keyword table entry
  `"javascript" => Some(Token::Javascript)`.
- `src/compiler/parser/grammar.lalrpop` — the terminal `"javascript" => Token::Javascript`, the
  `Module` production's `<js: "javascript"?>`, and the `VarIdent` production, which lists
  `"left"`, `"right"` and `"non"` as identifiers and must list the new word too.
- `src/compiler/parser/mod.rs` — `parser::Module::binding_javascript` and the
  `matches!(modifier, Some(tokenizer::Token::Javascript))` that computes it.
- `src/compiler/canonical/mod.rs` — `canonical::Module::binding_javascript`, the branch of
  `canonicalize` that reads it, and the three error messages that spell the modifier
  (`"a `module javascript` facade cannot declare an infix operator…"` and its two siblings).
- `std/core/src/Js/Basics.zel`, `Js/Bitwise.zel`, `Js/Utils.zel` — the three real facades.
- `tests/compiler/canonical.rs` and `tests/compiler/parser/modules.rs`, which each declare one.

**Decided ([Foreign interoperability](../spec/interop.md)):** a facade names a boundary and not
a backend. The modifier is `foreign`, one facade serves every compilation target, and which
target a build is for decides only which
[companion](../spec/interop.md#a-facade-names-a-boundary-not-a-backend) is read — `Prim.mjs` for
JavaScript, `Prim.wasm` for WebAssembly.

**Problem:** the modifier is spelled `javascript`, which claims a backend the chapter no longer
promises. Two consequences, and the second is why this is a `LANG-` rather than a rename.

A module written above a facade would have to name a target. `Basics` imports `Js.Basics`
today; if the Wasm-backed facade were a second module, `Basics` would name one of the two and
stop being compilable for both. The target-neutral modifier is what keeps every module above a
facade target-neutral for free.

And `javascript` is reserved outright — the tokenizer produces `Token::Javascript` and
`VarIdent` re-admits its three siblings and not it, so `javascript = 1` is a parse error whose
"expected" list names `left`, `right` and `non`. That is [`LANG-2`](lang-2.md), and this ticket
**subsumes it**: the word stops being a keyword in any position, so there is nothing left to
soften. `foreign` takes its place in `VarIdent`, on the same terms as the other three.

**Approach:** rename the token, the terminal, the two AST fields and the three error messages;
add the new word to `VarIdent`; rewrite the three `std/core` facades and the two tests. The flag
stays a `bool` — a facade has one kind, and the target is not a property of the declaration.

`LANG-2`'s grammar-conflict warning applies unchanged and is worth re-reading: `foreign` sits in
the `Module` production before an *upper* identifier while `VarIdent` yields a lower one, so the
two positions should not collide, but LALRPOP is the authority on that. If it conflicts, say so
in the PR rather than working around it.

**What this is not.** Not the check that holds a facade signature to the admitted types — that
is [`LANG-43`](lang-43.md), which this ticket must land before. Not a WebAssembly backend, and
not the emission of a `.wasm` companion's binding, which is [`GEN-2`](gen-2.md)'s successor.

**Acceptance:** `module foreign Core.Prim exposing (…)` parses and canonicalizes as a facade;
`module javascript X exposing (…)` does not; `javascript = 1`, `f javascript = javascript` and
`exposing (javascript)` all compile. `cargo run` still prints `parsed 8 modules` and lists all
eight as checked.

Seven tagged blocks change verdict, in three ways, and the PR has to walk all seven. Most
facade blocks in the spec fail on the modifier *and* on something else — `unsafe`
([`LANG-53`](lang-53.md)), a parenthesised type argument ([`LANG-9`](lang-9.md)), a constraint,
an undeclared `Task` — and those stay `expect=unimplemented` and stay passing.

**Three go green and are retagged `expect=ok`**: `Core.Colour` under [Which types may cross the
boundary](../spec/interop.md#which-types-may-cross-the-boundary), the `pi`/`e` block under
[Facade constants](../spec/interop.md#facade-constants), and `Core.Widget` under [What a package
exposes](../spec/packages.md#what-a-package-exposes).

**Two go red and stay `expect=unimplemented`**: `Core.Utils` and `Core.List` under [What a
facade signature may not name](../spec/interop.md#what-a-facade-signature-may-not-name). Both
name a type variable or a function type, which the language rejects and nothing checks, so they
canonicalize cleanly the moment they parse. They are `LANG-43`'s to turn green, and retagging
them `ok` here would delete the only record that the check is missing. Say so in the PR.

**One is regrouped**: `Core.Palette` under [A union crosses as a tagged
value](../spec/interop.md#a-union-crosses-as-a-tagged-value) imports the `Palette` block above
it, and stands alone today only because a `package=` group cannot hold a block that fails to
parse. Once it parses, tag both blocks `expect=ok package=union` — the label the chapter's
prose is written for.

**One goes red on its pin**: [Reserved words](../spec/lexical-structure.md#reserved-words)'
`javascript = 1` block is pinned `expect=parse-error:UnexpectedToken`. Retag it `expect=ok` and
delete the **Known gap:** paragraph under it. Its neighbour listing `foreign`, `derived` and
`unsafe` as ordinary identifiers must stay green: `foreign` becomes a keyword only in a position
that block does not use.

**Found:** while writing [Foreign interoperability](../spec/interop.md) (`SPEC-19`).
