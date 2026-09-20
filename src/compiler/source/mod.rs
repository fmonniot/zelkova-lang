pub mod files;

pub use files::{SourceFile, SourceFiles};

use super::CompilationError;
use files::SourceFileError;
use std::path::Path;
use walkdir::WalkDir;

// We don't support non-UTF8 characters in path
pub fn load_package_sources(root: &Path) -> Result<SourceFiles, CompilationError> {
    let mut sources = SourceFiles::new();
    load_package_sources_into(root, &mut sources)?;
    Ok(sources)
}

/// Walk `root` for `.zel` files and add each one to `files`, returning the id each was
/// given.
///
/// A build holds one file database for every package in it, because a
/// [`SourceFileId`](files::SourceFileId) travels inside an `Interface` and outlives the
/// package it came from (`ERR-5`), so the ids of two packages have to be drawn from one
/// sequence. This is the entry point that appends into that shared database;
/// [`load_package_sources`] is the same walk over a database of its own, which is what
/// a caller compiling exactly one source root wants.
///
/// The ids come back rather than the files themselves: `files` is borrowed mutably for
/// the walk, and the caller needs it borrowed immutably afterwards to read the sources
/// back out.
pub fn load_package_sources_into(
    root: &Path,
    sources: &mut SourceFiles,
) -> Result<Vec<files::SourceFileId>, CompilationError> {
    let mut loaded = vec![];
    let mut errors = vec![];

    // `WalkDir`'s iterator advances past every entry it yields — including an `Err`
    // one — and terminates on its own (a root that doesn't exist yields exactly one
    // `Err` and then ends), so this loop cannot spin on a single failing entry the
    // way the invariant in `CLAUDE.md` about `Result`-yielding iterators warns
    // against.
    for entry in WalkDir::new(root).follow_links(true) {
        match entry {
            Ok(entry) => {
                let path = entry.path().to_path_buf();

                match path.extension() {
                    Some(ext) if ext == "zel" => {}
                    _ => continue,
                }

                match SourceFile::load(path, root) {
                    Ok(src) => {
                        loaded.push(sources.add_file(src));
                    }
                    Err(err) => {
                        errors.push(err);
                    }
                }
            }
            Err(walk_err) => {
                errors.push(SourceFileError::from_walk_error(root, walk_err));
            }
        }
    }

    if errors.is_empty() {
        Ok(loaded)
    } else {
        Err(errors.into())
    }
}
