# SPEC-10 · Write the Packages and source layout chapter

**Sizing:** medium. The manifest is entirely undesigned (see Problem), so part of this ticket's
work is a real design decision, not just documentation.

**Location:** `src/compiler/source/mod.rs` — `load_package_sources`;
`src/compiler/source/files.rs` — `SourceFile`, `SourceFile::load`/`load_private`,
`SourceFileId`; `src/compiler/mod.rs` — `PackageName` (`author`/`project`), `ModuleName`, and
its `// TODO Ultimately we will pass a manifest content instead of a raw path` comment.

**Problem:** the package directory, the `zelkova.json` manifest, and what a package boundary
means for visibility have never been written down — and the manifest does not exist at all
yet, beyond two `TODO`-flavoured comments in `src/compiler/mod.rs`. This chapter also touches
`LANG-6` (a module's declared name is unrelated to the file it lives in), which sits at the
boundary between this chapter and the already-written Modules chapter.

**Approach:** follow `write-spec-chapter`. Document what exists today (directory walk,
path-to-module-name derivation) as settled; bring the `zelkova.json` manifest shape and
package-boundary visibility to the language owner as design questions rather than assuming an
answer; cross-link `LANG-6` where the file/module-name relationship overlaps with the Modules
chapter.

**Acceptance:** `cargo test --test spec` green, `docs/spec/packages.md` contributing its blocks
with every block tagged and each tag proven to fail, `docs/spec/README.md`'s row for this
chapter moved to `written`.
