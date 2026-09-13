pub mod files;

pub use files::{SourceFile, SourceFiles};

use super::CompilationError;
use files::SourceFileError;
use std::path::Path;
use walkdir::WalkDir;

// We don't support non-UTF8 characters in path
pub fn load_package_sources(root: &Path) -> Result<SourceFiles, CompilationError> {
    let mut sources = SourceFiles::new();
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
                        sources.add_file(src);
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
        Ok(sources)
    } else {
        Err(errors.into())
    }
}
