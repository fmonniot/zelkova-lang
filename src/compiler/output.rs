//! Writing a build's output to disk.
//!
//! What goes where is [`javascript`](super::javascript)'s — its *Paths* section is the
//! only place a path into the output tree is built. This module takes the files a
//! build produced, each with its path under `build/js/`, and writes them, reporting a
//! file it could not write as an [`Error`] rather than stopping at the first.
//!
//! It is only ever reached with a whole build's files, once every module of every
//! package has checked and emitted: `compile_package` calls it when its error vector is
//! empty and at no other time, so a build that failed anywhere writes nothing.

use std::path::{Path, PathBuf};

use super::PhaseError;

/// One file of a build's output.
#[derive(Debug)]
pub struct File {
    /// Where it goes, relative to `build/js/`.
    pub path: PathBuf,
    pub contents: Contents,
}

/// What a [`File`] holds.
#[derive(Debug)]
pub enum Contents {
    /// Text the compiler produced: an emitted module, or the runtime.
    Text(String),
    /// A file copied as it is, from this path: a facade's companion.
    Copy(PathBuf),
}

/// A file of the output that could not be written.
#[derive(Debug)]
pub struct Error {
    /// The file that was being written, in full.
    pub path: PathBuf,
    /// The companion it was being copied from, for a [`Contents::Copy`].
    pub from: Option<PathBuf>,
    pub error: std::io::Error,
}

impl PhaseError for Error {
    fn message(&self) -> String {
        match &self.from {
            Some(from) => format!(
                "could not copy the companion `{}` to `{}`: {}",
                from.display(),
                self.path.display(),
                self.error
            ),
            None => format!("could not write `{}`: {}", self.path.display(), self.error),
        }
    }
}

/// Write every one of `files` below `js_root`, creating the directories each needs.
///
/// A file that cannot be written does not stop the others: the answer is every file
/// that failed, and empty when all of them were written.
pub fn write(js_root: &Path, files: &[File]) -> Vec<Error> {
    files
        .iter()
        .filter_map(|file| write_one(&js_root.join(&file.path), &file.contents).err())
        .collect()
}

fn write_one(path: &Path, contents: &Contents) -> Result<(), Error> {
    let from = match contents {
        Contents::Copy(from) => Some(from.clone()),
        Contents::Text(_) => None,
    };
    let failed = |error| Error {
        path: path.to_path_buf(),
        from: from.clone(),
        error,
    };

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(failed)?;
    }

    match contents {
        Contents::Text(text) => std::fs::write(path, text).map_err(failed),
        Contents::Copy(source) => std::fs::copy(source, path).map(|_| ()).map_err(failed),
    }
}
