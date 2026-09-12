# SITE-1 · Publish a landing page and the rendered spec alongside the rustdoc on GitHub Pages

**Sizing:** large (a Cargo workspace, two new crates, a markdown renderer that invents its own
`expect=` styling, a hand-written landing page, and a workflow restructure — none of which
exists today).

**Location:** `Cargo.toml`; new crates under `tools/spec-doc/` and `tools/spec-site/`;
`tests/spec.rs`, which gives up its scanning half; `.github/workflows/rustdoc.yml` and
`.github/workflows/rust.yml`; `CLAUDE.md`'s *Commands* block; `README.md`. The 17 files under
[`docs/spec/`](../spec/README.md) are the renderer's input and are **not edited** by this
ticket.

**Problem:** the only thing published to GitHub Pages today is `cargo doc`'s output, deployed
by `.github/workflows/rustdoc.yml` straight from `target/doc` with `force_orphan: true`. Three
consequences follow:

1. There is no landing page. The only way to reach the docs is to already know
   `https://francois.monniot.eu/zelkova-lang/zelkova_lang/` — `cargo doc`'s crate-root index,
   with no framing of what Zelkova is or where else to look.
2. `docs/spec/` — the normative language specification, whose 343 `zel` blocks are checked
   against the compiler by `cargo test --test spec` — is published nowhere. It exists only as
   markdown in the repo.
3. `force_orphan: true` with `publish_dir: ./target/doc` means the workflow can only ever
   publish exactly one directory tree, replaced wholesale on every push to `main`. A landing
   page and a rendered spec have to be assembled into a combined directory *before* the deploy
   step: two `peaceiris/actions-gh-pages` runs with `force_orphan: true` in one workflow would
   each wipe out what the other just published.

**Wanted:** a landing page at the site root that frames the project and links to the rustdoc and
the spec, and the spec's chapters rendered as HTML on the same site — each chapter's
```` ```zel ```` blocks styled by their `expect=` tag, so a reader can see at a glance which
examples the compiler accepts today and which are aspirational.

## Decided

The four design questions this ticket was filed with are settled. Do not re-open them.

**1 — The renderer is an in-repo Rust tool, not mdBook.** mdBook would need a `SUMMARY.md`
duplicating [`docs/spec/README.md`](../spec/README.md)'s chapter table, with nothing holding the
two in step, and the `expect=` badges would need a preprocessor binary written in Rust anyway.
Writing the renderer outright costs one dependency and keeps the badge rule, the anchor slugs
and the link rewriting under `cargo test`, which is where everything else in this repo that
could drift already lives.

**2 — The `expect=` grammar is written once, in a crate both readers share.** `tests/spec.rs`
holds the only implementation today and it is a test binary, so nothing outside the test crate
can call it. Its scanning half moves into a new zero-dependency crate. A second, drifting
implementation of the `expect=` grammar is the exact failure mode
[*Every example is checked*](../spec/README.md#every-example-is-checked) exists to prevent.

**3 — The site is `/` (landing), `/spec/` (chapters), `/api/` (rustdoc).** The bookmarked
`…/zelkova-lang/zelkova_lang/` becomes `…/zelkova-lang/api/zelkova_lang/`. Accepted: the root
has to serve a front door, and it cannot be a crate list.

**4 — Links out of `docs/spec/` become GitHub blob URLs.** The chapters make roughly 180 links
to `../tickets/*.md` and `../decisions/*.md`. The site renders neither directory — publishing
the compiler's work log is not what this ticket is for, and a closed ticket's page would vanish
from the site on the next deploy. `spec_cross_references_resolve` already guarantees every cited
file exists in-tree, so none of the rewritten links 404.

### Why the compiler crate does not move

A virtual workspace — `crates/zelkova-lang/` beside `tools/` — was considered and rejected *for
this ticket*. It buys one thing: bare `cargo test` covers every member, because cargo's default
for a virtual manifest is the whole workspace. With a root package it is the root package alone,
which was confirmed by experiment rather than assumed.

That is worth one flag, not a rename. `src/compiler/…` and `tests/*.rs` are named 232 times
across some 85 files — `CLAUDE.md`, four skills, five decision entries, `conventions.md`, and
nearly every open ticket. Rewriting all of them inside a PR about a website makes the PR
unreviewable and buries the part a reviewer needs to read. `cargo test --workspace` in
`rust.yml` and in `CLAUDE.md`'s *Commands* block costs two lines and is gated by CI.

The virtual workspace also breaks `cargo run`, which `CLAUDE.md` documents as the de-facto smoke
test: with two packages exposing a binary, cargo cannot pick one and stops. Under a root package
`cargo run` keeps working untouched.

If the split is wanted on its own merits, it is a `TIDY-` ticket of its own.

## Approach

### Step 1 — Workspace, and `tools/spec-doc`

Root `Cargo.toml` keeps its `[package]` and gains:

```toml
[workspace]
members = ["tools/spec-doc", "tools/spec-site"]
```

Create `tools/spec-doc`, a library crate with **no dependencies**, and move into it the half of
`tests/spec.rs` that reads markdown rather than running the compiler:

| Moves | What it is |
|---|---|
| `fence_open`, `fence_close` | what a fence is, including the no-backtick-in-the-info-string rule |
| `Expect`, `parse_expect`, `parse_info` | the `expect=` and `package=` grammar |
| `Block`, `extract_zel_blocks` | one `zel` block, and the scan that finds them |
| `slugify`, `header_anchors` | GitHub's header-anchor rule, repeat suffixes included |
| `prose_lines` | every line outside a fence |

Each carries its doc comment across unchanged — those comments are the account of why the rules
are what they are, and several name the chapter that forced them. The crate gets a module doc
comment saying it is shared by the spec harness and the site renderer, and why that sharing
exists. Everything moved becomes `pub`; `rustdoc.yml` sets `-D warnings -W unreachable-pub`, so
an undocumented or needlessly-public item fails the deploy.

`tests/spec.rs` then keeps only the evaluation half — `parse`, `canonicalize`, `type_check`,
`evaluate_group`, `parse_error_reasons`, `error_kind_names`, `variant_names`, and all 31
`#[test]` functions: the four that read the real chapters, and the 27 that pin the harness's own
failure modes against `tests/fixtures/spec/`. Add to the root `Cargo.toml`:

```toml
[dev-dependencies]
spec-doc = { path = "tools/spec-doc" }
```

A **dev**-dependency deliberately: the compiler does not depend on this at build time, and
`cargo doc -p zelkova-lang` does not publish it as part of the compiler's API.

Three things to get right here, each of which is a silent break rather than a compile error:

- `tests/spec.rs`'s module doc comment and the intra-doc links in it (`` [`slugify`] ``,
  `` [`parse_error_reasons`] ``, `` [`extract_zel_blocks`] ``) now point at items in another
  crate. Re-path them or the doc build warns, and `-D warnings` turns a warning into a failure.
- The fixture tests under `tests/fixtures/spec/` pin the scanner's failure modes —
  `header_anchors_follow_githubs_slug_rule`, `block_with_no_expect_is_a_hard_failure`,
  `unrecognised_expect_value_is_a_hard_failure`, and the one pinning `fence_open`'s
  no-backtick rule, `a_line_beginning_with_an_inline_code_span_is_not_a_fence`. They
  stay in `tests/spec.rs` and go on calling the moved functions from their new home.
- `chapter_files` locates `docs/spec/` through `CARGO_MANIFEST_DIR`. The root package does not
  move, so that path is still correct — leave it alone.

**This step changes no behaviour.** `cargo test` is green before and after, and the diff is a
move. Land it as its own commit within the PR so the renderer's diff is readable.

### Step 2 — `tools/spec-site`, the renderer

A binary crate depending on `spec-doc` and `pulldown-cmark` (0.13.4 at time of writing; use the
current release). It does **not** depend on `zelkova-lang` — the renderer never compiles Zelkova,
and keeping the compiler out of its build graph keeps a site build fast.

```
cargo run -p spec-site -- --out site
```

walks the 17 top-level `*.md` files in `docs/spec/`, renders each to `site/spec/<stem>.html`,
and copies its static assets to the site root. `README.md` renders to `site/spec/index.html`.

Enable `pulldown-cmark`'s GFM table option. Tables are the **only** extension beyond CommonMark
the chapters use, and that is checked rather than assumed: across `docs/spec/` there is no
strikethrough, no footnote and no raw HTML block. Then intercept three event kinds:

**Fenced code blocks.** Pass the info string to `spec_doc::parse_info`. A fence whose first token
is not `zel` renders as an ordinary `<pre><code>`. A `zel` fence renders inside a wrapper
carrying the badge:

```html
<div class="zel-block expect-accepted">
  <span class="zel-tag">expect=ok</span>
  <pre><code>module Main exposing (..)
…</code></pre>
</div>
```

The badge always shows the tag's **literal text**, so the distinction never rests on colour
alone. Three visual families, which is the whole point of the feature:

| `Expect` | Family | Reads as |
|---|---|---|
| `Ok` | `expect-accepted` — green left border, green badge | the compiler accepts this today |
| `ParseError`, `CanonicalError`, `TypeError`, `DependencyError` | `expect-rejected` — red left border, red badge | the compiler rejects this, which is the point of the example |
| `Unimplemented` | `expect-pending` — amber left border, amber badge | the language has this and the compiler does not yet |
| `Fragment` | `expect-unchecked` — grey **dashed** border, grey badge | illustrative; nothing is run, and the block makes no claim either way |

`Fragment` being visually distinct from both other outcomes is a requirement, not a preference: a
fragment carries no claim about what the compiler does with it, so styling it as either "works"
or "doesn't" misrepresents it. The dashed border is what carries that without a fourth colour.

Match **exhaustively** on `spec_doc::Expect` so that a new variant fails the renderer to compile
rather than rendering unstyled — the same guarantee `error_kind_names` and `variant_names` get in
`tests/spec.rs` from their explicit matches. Where `parse_info` returns `Err`, **abort the build
with a non-zero exit and the file and line**. An unrecognised tag is a hard failure in the
harness and it is a hard failure here; a silently-unstyled block is the drift this crate exists
to prevent. A `package=<label>` renders as a second, neutral chip beside the `expect=` badge, so
a reader can see which blocks of a chapter are one package.

Bare `expect=type-error` is currently used by no chapter, and `expect=ok` / `expect=fragment` /
`expect=dependency-error` are the only other tags whose spelling has no suffix. Handle all seven
variants regardless — the renderer follows the enum, not today's usage.

**Headers.** `pulldown-cmark` emits no `id`. Emit one, using `spec_doc::header_anchors`'
slug-and-repeat-suffix rule, which is GitHub's. Getting this wrong is the subtle failure in the
whole ticket: every intra-chapter link in `docs/spec/` is a GitHub slug, and
`spec_cross_references_resolve` validates them against that rule — so a renderer that slugs
differently produces a site of quietly broken anchors that no test sees. `slugify`'s doc comment
records the case worth knowing: `let … in` slugs to `let--in`, two hyphens, and seven links
across six chapters name exactly that.

**Links.** Rewrite the destination:

| Written in the chapter | On the site |
|---|---|
| `types.md`, `types.md#anchor` | `types.html`, `types.html#anchor` |
| `README.md` | `index.html` |
| `../tickets/lang-47.md`, `../decisions/dec-2.md#7--dictionaries…` | `https://github.com/fmonniot/zelkova-lang/blob/main/docs/tickets/lang-47.md`, same with the anchor carried through |
| `#anchor` alone | unchanged |
| `https://…` | unchanged |

Tests, in `tools/spec-site`, each one confirmed to go red with the behaviour it pins removed —
`CLAUDE.md`'s rule, and these are exactly the kind that pass both ways if you skip it:

- each of the seven `Expect` variants renders its own family class and its literal tag text;
- a header renders the `id` `header_anchors` would give it, `let … in` among the cases;
- each link row of the table above rewrites as stated, and an `#anchor`-only link and an
  absolute URL are left alone;
- a `zel` fence with an unrecognised tag exits non-zero and names the file and line;
- a non-`zel` fence renders plain, with no badge.

Syntax highlighting for `zel` is **not** in this ticket. No highlighter knows the language, and
the `expect=` badge is what the blocks are here to carry.

### Step 3 — Landing page and stylesheet

`tools/spec-site/assets/index.html` and `assets/style.css`, hand-written, copied to the site root
by the renderer. One stylesheet serves the landing page and every chapter; the badge families
above are classes in it.

The landing page says what `README.md` and `CLAUDE.md` already say, in a reader's order: Zelkova
is an Elm-inspired functional language compiling to WebAssembly by way of JavaScript, written in
Rust as a project about how compilers are built. Then three links — the spec at `/spec/`, the
rustdoc, and the repository at `https://github.com/fmonniot/zelkova-lang`. Say plainly that the
spec is normative and that every example in it is checked against the compiler, since that is the
most interesting true thing about this particular spec and it explains the badges before the
reader meets one.

Link the rustdoc as **`/api/zelkova_lang/`**, not `/api/`. `cargo doc --no-deps` writes no
root `index.html` — the current `gh-pages` tree has `crates.js`, `help.html`, `settings.html`,
`static.files` and `zelkova_lang/` at its root and no index — so `/api/` would 404. Have the
renderer also write `site/api/index.html` as a meta-refresh redirect to `zelkova_lang/index.html`
so a reader who trims the URL lands somewhere.

Keep the CSS small and legible: a readable measure on the prose, a monospace stack on code, and
`prefers-color-scheme` if it is cheap. Restraint here is worth more than a theme.

### Step 4 — The workflow

`rustdoc.yml` assembles one directory and deploys it once:

```yaml
- name: Build Documentation
  run: cargo doc -p zelkova-lang --no-deps

- name: Render the site
  run: cargo run -p spec-site -- --out site

- name: Mount the rustdoc
  run: cp -r target/doc site/api

- name: Deploy Docs
  uses: peaceiris/actions-gh-pages@v3
  with:
    github_token: ${{ secrets.GITHUB_TOKEN }}
    publish_branch: gh-pages
    publish_dir: ./site
    force_orphan: true
```

`cargo doc --all` becomes `cargo doc -p zelkova-lang`: `--all` is `--workspace`, which would now
document the two tool crates onto the public site as well.

One deploy step, as before. Leave `enable_jekyll` unset — the action writes `.nojekyll` by
default, which is what keeps rustdoc's `static.files` from being eaten, and the existing
`gh-pages` tree confirms it is being written today. Do **not** add a `CNAME` file: the custom
domain is configured in the repository's Pages settings and the current branch has no such file.

Add `/site` to `.gitignore`.

In `rust.yml`, add a job that runs `cargo run -p spec-site -- --out site` on pull requests
without deploying, so a chapter or a renderer change that breaks the build is caught before it
reaches `main` rather than on the deploy. Change `cargo test` to `cargo test --workspace` in the
test job, and clippy's `args` to `--workspace --all-features`. Neither `fmt` nor `clippy` gates —
both are `continue-on-error: true` — so run both locally.

### Step 5 — The documented commands

`cargo test` no longer means what `CLAUDE.md`'s *Commands* block says it means. Update that block
and the sentence under it:

- `cargo test --workspace` is the full suite. Bare `cargo test` runs the compiler's tests and
  silently skips the tool crates', which is worth one sentence of warning.
- `cargo clippy --workspace --all-features`.
- `cargo run` is unchanged and still compiles `std/core/src`.
- `cargo run -p spec-site -- --out site` renders the site locally; open `site/index.html`.
- `cargo fmt --all` is unchanged and already covers workspace members.

Add a line to `CLAUDE.md`'s routing on where the site is built from, in the *Where work is
tracked* neighbourhood. One sentence, pointing at `tools/spec-site/`. Add a *Documentation*
sentence to `README.md` giving the site's URL.

## What this is not

Not a change to any chapter. `docs/spec/` is input here, and a rendering problem that looks like
it wants a chapter edited is a rendering problem. `expect=fragment`, which the original filing
was unsure existed, is used five times already — `evaluation-semantics.md:600` and four blocks in
`type-classes.md` — so nothing needs adding to make the acceptance check possible.

Not a rename of the compiler crate; see
[*Why the compiler crate does not move*](#why-the-compiler-crate-does-not-move).

Not syntax highlighting, not full-text search, and not rendering `docs/tickets/` or
`docs/decisions/`.

## Acceptance

- `cargo test --workspace` is green, and `cargo test --test spec` still checks all 343 `zel`
  blocks — the move in Step 1 changed no behaviour.
- `cargo run -p spec-site -- --out site` exits 0 and writes `site/index.html`, `site/style.css`,
  `site/api/index.html` and 17 files under `site/spec/`.
- Deleting the `expect=` tag from any one block and re-running the renderer exits non-zero and
  names the file and line.
- Pushing to `main` deploys a `gh-pages` site whose root is the landing page, whose `/spec/`
  holds the rendered chapters, and whose `/api/zelkova_lang/` is the rustdoc, still reachable.
  A single `actions-gh-pages` step with a single `force_orphan`.
- **By eye, on three chapters**, which between them use every tag in service today:
  [`lexical-structure.md`](../spec/lexical-structure.md) (`ok`, `parse-error:IndentationError`,
  `parse-error:UnexpectedToken`, `parse-error:UnrecognizedToken`,
  `canonical-error:BindingPatternsInvalidLen`, `type-error:UnificationFailed`, `unimplemented`),
  [`type-classes.md`](../spec/type-classes.md) (`fragment` beside `ok` and `unimplemented`), and
  [`modules.md`](../spec/modules.md) (`dependency-error`, and 15 `package=` labels). Accepted,
  rejected, pending and unchecked are four looks a reader can tell apart, every badge shows its
  literal tag, and no anchor in any of the three is broken.
