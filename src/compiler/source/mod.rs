pub mod files;

pub use files::{SourceFile, SourceFiles};

use super::CompilationError;
use std::path::Path;
use walkdir::WalkDir;

// We don't support non-UTF8 characters in path
pub fn load_package_sources(root: &Path) -> Result<SourceFiles, CompilationError> {
    let walk = WalkDir::new(root)
        .follow_links(true)
        .into_iter()
        .filter_map(|r| r.ok())
        .filter_map(|entry| {
            let path = entry.path().to_path_buf();
            let ext = path.extension();

            match ext {
                Some(os_str) if os_str == "zel" => Some(path),
                _ => None,
            }
        });

    let mut sources = SourceFiles::new();
    let mut errors = vec![];

    for abs_path in walk {
        match SourceFile::load(abs_path, root) {
            Ok(src) => {
                sources.add_file(src);
            }
            Err(err) => {
                errors.push(err);
            }
        };
    }

    if errors.is_empty() {
        Ok(sources)
    } else {
        Err(errors.into())
    }
}
