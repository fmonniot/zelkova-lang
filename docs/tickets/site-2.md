# SITE-2 · An image reference in a chapter is not rewritten, and has nowhere to land

**Sizing:** small-to-medium (a copy step in `build_site`, plus a `Tag::Image` rewrite rule
distinct from `Tag::Link`'s — see **Approach** for why the two can't share code as written).

**Location:** `tools/spec-site/src/render.rs` — `render_chapter`'s event loop rewrites
`Tag::Link` via `rewrite_link` but has no arm for `Tag::Image`. `tools/spec-site/src/main.rs`
— `build_site` copies `tools/spec-site/assets/*` into the site root but never reads anything
out of `docs/spec/` besides the `.md` chapters themselves.

**Found:** while addressing review comments on [PR #188](https://github.com/fmonniot/zelkova-lang/pull/188)
(SITE-1). A reviewer note observed that `rewrite_link` is wired to `Tag::Link` only. Fixing it
by routing `Tag::Image` through the same function was tried and reverted: `rewrite_link`'s
sibling-chapter branch assumes a `.md` extension being replaced with `.html`, so calling it on
an image `dest_url` like `diagram.png` produces `diagram.png.html` — confirmed with a test
(`render("![a diagram](diagram.png)\n")` rendered `src="diagram.png.html"`). Left unfixed on
that PR because closing it needs a design decision SITE-1's acceptance didn't ask for.

**Problem:** there are no `![...]` images anywhere under `docs/spec/` today (checked with
`grep -r '!\[' docs/spec/`), so nothing is broken yet. But a chapter that adds a diagram would
hit two separate gaps at once:

- A same-directory image would need its `src` rewritten to wherever the site puts it, but
  `rewrite_link`'s `.md` → `.html` logic doesn't apply to an arbitrary image extension, so
  reusing it verbatim produces a broken path rather than a working one.
- Even with a correct rewrite rule, there is no step in `build_site` that copies the image
  file itself into the output directory — `docs/spec/` is read only for its `.md` files, so
  the image would 404 regardless of what its `src` says.

**Approach:** two decisions, neither of which this ticket makes:

- **Where a chapter image lives in the output tree.** Options include copying it next to its
  rendered chapter under `site/spec/`, or into a shared `site/spec/img/` regardless of which
  chapter references it. The choice affects both the copy step in `build_site` and the rewrite
  rule's output path.
- **What counts as an image to copy.** The simplest version walks `docs/spec/` for image
  extensions (`.png`, `.svg`, `.jpg`, …) the same pass that already lists `.md` chapters does;
  a more conservative version only copies images actually referenced by a `Tag::Image` event,
  which avoids shipping an orphaned file but means the copy step and the rewrite rule have to
  share information about what was found.

Once those are settled, the `Tag::Image` rewrite is a small addition to `render_chapter`'s
event loop, structurally the same as the existing `Tag::Link` arm but resolving against
wherever the image was copied rather than against a sibling `.html` page.

**Acceptance:** a chapter under `docs/spec/` referencing a same-directory image with
`![alt](diagram.png)` renders a page whose `<img src="...">` resolves to a file that exists in
the site's output directory, verified by `cargo run -p spec-site -- --out <dir>` followed by
checking the referenced path exists under `<dir>`. A unit test in `render.rs`'s test module
pins the rewritten `src`, following the pattern of `sibling_chapter_link_becomes_sibling_html`.
