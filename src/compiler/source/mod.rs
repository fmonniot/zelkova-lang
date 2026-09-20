pub mod files;

pub use files::{SourceFile, SourceFiles, SourceRoot};

use super::{CompilationError, PackageName};
use files::SourceFileError;
use std::path::Path;
use walkdir::WalkDir;

/// Walk one of a package's source roots into a database of its own.
///
/// Nothing else can be in that database, so a file is named by its place in the
/// package and nothing more — see [`load_package_sources_into`] for the build case,
/// where that name is not unique.
// We don't support non-UTF8 characters in path
pub fn load_package_sources(
    package_dir: &Path,
    root: SourceRoot,
) -> Result<SourceFiles, CompilationError> {
    let mut sources = SourceFiles::new();
    load_package_sources_into(package_dir, root, None, &mut sources)?;
    Ok(sources)
}

/// Walk one source root of the package rooted at `package_dir` for `.zel` files, add
/// each one to `files`, and return the id each was given.
///
/// A package has two source roots and they are walked separately, because a module's
/// name is its path under *its own* root: `src/Model.zel` and `tests/Model.zel` are
/// both `Model` ([*Source
/// roots*](../../../../docs/spec/packages.md#source-roots)). Each file carries the root
/// it came from, so a diagnostic can tell the two apart.
///
/// A package must have a `src/`, and a walk of one that is not there is the error
/// `WalkDir` reports. A `tests/` that is not there is a package that holds no tests,
/// which is not an error and loads as no modules at all.
///
/// A build holds one file database for every package in it, because a
/// [`SourceFileId`](files::SourceFileId) travels inside an `Interface` and outlives the
/// package it came from (`ERR-5`), so the ids of two packages have to be drawn from one
/// sequence. This is the entry point that appends into that shared database;
/// [`load_package_sources`] is the same walk over a database of its own, which is what
/// a caller compiling exactly one source root wants.
///
/// `package` names the package `package_dir` holds, and every caller sharing a database
/// across packages has to pass it: a path relative to a package's own `src/` is not
/// unique in a build — two packages may each hold a `Size.zel` — so without it a
/// diagnostic cannot say which package it is about.
///
/// The ids come back rather than the files themselves: `files` is borrowed mutably for
/// the walk, and the caller needs it borrowed immutably afterwards to read the sources
/// back out.
pub fn load_package_sources_into(
    package_dir: &Path,
    root: SourceRoot,
    package: Option<&PackageName>,
    sources: &mut SourceFiles,
) -> Result<Vec<files::SourceFileId>, CompilationError> {
    let mut loaded = vec![];
    let mut errors = vec![];

    let root_dir = package_dir.join(root.directory());

    // A package that holds no tests has no `tests/` to walk, and that is not a failure
    // the way a missing `src/` is. Asking first is what keeps the walk's own
    // does-not-exist error meaning what it says everywhere else.
    if root == SourceRoot::Tests && !root_dir.exists() {
        return Ok(loaded);
    }

    // `WalkDir`'s iterator advances past every entry it yields — including an `Err`
    // one — and terminates on its own (a root that doesn't exist yields exactly one
    // `Err` and then ends), so this loop cannot spin on a single failing entry the
    // way the invariant in `CLAUDE.md` about `Result`-yielding iterators warns
    // against.
    for entry in WalkDir::new(&root_dir).follow_links(true) {
        match entry {
            Ok(entry) => {
                let path = entry.path().to_path_buf();

                match path.extension() {
                    Some(ext) if ext == "zel" => {}
                    _ => continue,
                }

                match SourceFile::load(path, &root_dir, root, package) {
                    Ok(src) => {
                        loaded.push(sources.add_file(src));
                    }
                    Err(err) => {
                        errors.push(err);
                    }
                }
            }
            Err(walk_err) => {
                errors.push(SourceFileError::from_walk_error(&root_dir, walk_err));
            }
        }
    }

    if errors.is_empty() {
        Ok(loaded)
    } else {
        Err(errors.into())
    }
}
