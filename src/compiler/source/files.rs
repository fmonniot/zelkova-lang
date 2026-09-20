use crate::compiler::{PackageName, PhaseError};
use codespan_reporting::files::{Error as FilesError, Files, SimpleFile};
use std::ops::Range;
use std::path::{Path, PathBuf};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SourceFileId(usize);

/// One of a package's two source roots, as
/// [*Source roots*](../../../../docs/spec/packages.md#source-roots) fixes them: `src/`
/// is what the package ships, `tests/` is compiled when this package's own tests are
/// run. Neither is configurable and there is no third.
///
/// Every [`SourceFile`] carries the one it was walked from, because the two roots share
/// one set of module names: `src/Model.zel` and `tests/Model.zel` are both `Model`, and
/// a diagnostic about that has to say which file it means.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SourceRoot {
    Src,
    Tests,
}

impl SourceRoot {
    /// The directory this root is, under the package directory holding `zelkova.toml`.
    pub fn directory(self) -> &'static str {
        match self {
            SourceRoot::Src => "src",
            SourceRoot::Tests => "tests",
        }
    }
}

impl std::fmt::Display for SourceRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.directory())
    }
}

// This should probably implements `Files` directly instead of relying on SimpleFile
#[derive(Debug)] // TODO Implement Debug manually (don't want to embedded the entire source code)
pub struct SourceFile {
    #[allow(dead_code)]
    module_name: String,
    /// Which of the package's two source roots this file was walked from, and its path
    /// under that root. Together they are [`SourceFile::package_path`], the file's place
    /// in its package.
    root: SourceRoot,
    relative_path: PathBuf,
    /// `Files` implementation. Its name is what a diagnostic's location line shows: the
    /// file's path within its package, prefixed with `package:` when the database holds
    /// a whole build (see [`SourceFile::load`]).
    file: SimpleFile<String, String>,
}

impl SourceFile {
    /// Load a `SourceFile` from the file system.
    ///
    /// `root_dir` is the directory the file was walked from and `root` says which of the
    /// package's two source roots that is; a module's name is its path under its own
    /// root, so the two roots are walked separately and each file is named relative to
    /// the one it came from.
    ///
    /// `package` is the name of the package `root_dir` belongs to, and is what the
    /// rendered file name is prefixed with. It is `Some` whenever the file joins a
    /// database that holds more than one package's files: a path relative to a
    /// package's own `src/` is not unique across a build — two packages may each hold a
    /// `Size.zel` — so without it a diagnostic cannot say which package it is about.
    /// `None` is for a database of exactly one package, where the prefix would be noise.
    pub fn load(
        abs_path: PathBuf,
        root_dir: &Path,
        root: SourceRoot,
        package: Option<&PackageName>,
    ) -> Result<SourceFile, SourceFileError> {
        SourceFile::load_private(&abs_path, root_dir, root, package)
            .map_err(|error| SourceFileError { error, abs_path })
    }

    fn load_private(
        abs_path: &PathBuf,
        root_dir: &Path,
        root: SourceRoot,
        package: Option<&PackageName>,
    ) -> Result<SourceFile, SourceFileErrorType> {
        let relative_path = abs_path.strip_prefix(root_dir)?.to_path_buf();

        let file_name = relative_path
            .to_str()
            .ok_or_else(|| SourceFileErrorType::NonUtf8Module(relative_path.clone()))?
            .to_owned();

        let module_name = file_name
            .trim_end_matches(".zel")
            .replace(std::path::MAIN_SEPARATOR, ".");

        let package_path = format!("{}/{}", root, file_name);

        // `package:path`, the same shape `ModuleName` renders a module in, so the two
        // halves of a diagnostic — the `[package:Module]` headline and the file its
        // labels point into — name the package the same way.
        let rendered_name = match package {
            Some(package) => format!("{}:{}", package, package_path),
            None => package_path,
        };

        let source = std::fs::read_to_string(abs_path)?;

        Ok(SourceFile {
            module_name,
            root,
            relative_path,
            file: SimpleFile::new(rendered_name, source),
        })
    }

    pub fn file(&self) -> &SimpleFile<String, String> {
        &self.file
    }

    /// Where this file sits in its package: the source root it was walked from and its
    /// path under that root, as `src/Model.zel`.
    ///
    /// This is the half of the rendered name that belongs to the package rather than to
    /// the build, and it is what a diagnostic about the package's own layout — two
    /// modules answering to one name — names each file by.
    pub fn package_path(&self) -> String {
        format!("{}/{}", self.root, self.relative_path.display())
    }
}

#[derive(Debug)]
pub struct SourceFileError {
    error: SourceFileErrorType,
    abs_path: PathBuf,
}

impl SourceFileError {
    /// Build a `SourceFileError` from a `walkdir` failure.
    ///
    /// `walkdir::Error::path` is `None` only for the handful of internal errors that
    /// aren't tied to a specific directory entry (see the crate's `Error::from_io`);
    /// `root` is the best path to blame in that case, since it is what was being
    /// walked. Everything else — a root that doesn't exist, a directory the process
    /// can't read, a symlink loop — carries its own path.
    pub(super) fn from_walk_error(root: &Path, error: walkdir::Error) -> SourceFileError {
        let abs_path = error
            .path()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| root.to_path_buf());

        SourceFileError {
            error: SourceFileErrorType::Walk(error),
            abs_path,
        }
    }
}

/// Loading is the one phase that has no module and no source text to point into —
/// it failed before either existed — so its errors name the path instead. It goes
/// through [`PhaseError`] anyway, so that `as_diagnostic` has exactly one way to
/// turn a phase error into text.
impl PhaseError for SourceFileError {
    fn message(&self) -> String {
        let detail = match &self.error {
            SourceFileErrorType::InvalidPathPrefix => {
                "the module path doesn't start with the package path"
            }
            SourceFileErrorType::Io(_) => "an I/O error occured while reading the module",
            SourceFileErrorType::NonUtf8Module(_) => {
                // Maybe we should add some details as to where the incorrect characters are ?
                "module names must be utf-8 encoded"
            }
            SourceFileErrorType::Walk(err) => {
                if err.loop_ancestor().is_some() {
                    "a symbolic link loop was found while walking the package sources"
                } else if err.io_error().map(std::io::Error::kind)
                    == Some(std::io::ErrorKind::NotFound)
                {
                    "this path does not exist"
                } else {
                    "an I/O error occured while walking the package sources"
                }
            }
        };

        format!("{}: {}", self.abs_path.display(), detail)
    }

    fn notes(&self) -> Vec<String> {
        match &self.error {
            SourceFileErrorType::InvalidPathPrefix => vec![],
            SourceFileErrorType::Io(err) => vec![format!("detailled error: {:?}", err)],
            SourceFileErrorType::NonUtf8Module(rel_path) => {
                // Maybe we should add some details as to where the incorrect characters are ?
                vec![format!("relative path: {}", rel_path.display())]
            }
            SourceFileErrorType::Walk(err) => {
                let mut notes = vec![];
                if let Some(ancestor) = err.loop_ancestor() {
                    notes.push(format!(
                        "the link back to {} was found here",
                        ancestor.display()
                    ));
                }
                if let Some(io_err) = err.io_error() {
                    notes.push(format!("detailled error: {:?}", io_err));
                }
                notes
            }
        }
    }
}

#[derive(Debug)]
pub enum SourceFileErrorType {
    InvalidPathPrefix,
    Io(std::io::Error),
    NonUtf8Module(PathBuf),
    /// An entry `walkdir` failed to yield while walking the package root — a root
    /// that doesn't exist, a directory the process can't read, or a symbolic-link
    /// loop it detected (see `WalkDir::follow_links` on why the walk can hit one).
    Walk(walkdir::Error),
}

impl From<std::path::StripPrefixError> for SourceFileErrorType {
    fn from(_err: std::path::StripPrefixError) -> Self {
        SourceFileErrorType::InvalidPathPrefix
    }
}

impl From<std::io::Error> for SourceFileErrorType {
    fn from(err: std::io::Error) -> Self {
        SourceFileErrorType::Io(err)
    }
}

/// A file database that can store multiple source files.
///
/// This is taking as is from `codespan_reporting::files::SimpleFiles` with
/// the added ability to iterate over the files
///
/// There are a few TODOs on this structure that may or may not be needed:
/// - Use a custom `SourceFile` structure instead of `SimpleFile`
/// - Use a deterministic `FileId` instead of using `usize`
pub struct SourceFiles {
    files: Vec<SourceFile>,
}

impl Default for SourceFiles {
    fn default() -> Self {
        Self::new()
    }
}

impl SourceFiles {
    /// Create a new files database.
    pub fn new() -> SourceFiles {
        SourceFiles { files: Vec::new() }
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again.
    pub fn add_file(&mut self, file: SourceFile) -> SourceFileId {
        let file_id = self.files.len();
        self.files.push(file);
        SourceFileId(file_id)
    }

    /// Get the file corresponding to the given id.
    pub fn get(&self, file_id: SourceFileId) -> Result<&SimpleFile<String, String>, FilesError> {
        self.files
            .get(file_id.0)
            .map(|f| &f.file)
            .ok_or(FilesError::FileMissing)
    }

    pub fn iter(&self) -> impl Iterator<Item = (SourceFileId, &'_ SourceFile)> + '_ {
        self.files
            .iter()
            .enumerate()
            .map(|(i, v)| (SourceFileId(i), v))
    }

    // Will probably need an accessor module name -> Option<SourceFileId>
}

impl<'a> Files<'a> for SourceFiles {
    type FileId = SourceFileId;
    type Name = String;
    type Source = &'a String;

    fn name(&self, file_id: Self::FileId) -> Result<String, FilesError> {
        Ok(self.get(file_id)?.name().clone())
    }

    fn source(&self, file_id: Self::FileId) -> Result<&String, FilesError> {
        Ok(self.get(file_id)?.source())
    }

    fn line_index(&self, file_id: Self::FileId, byte_index: usize) -> Result<usize, FilesError> {
        self.get(file_id)?.line_index((), byte_index)
    }

    fn line_range(
        &self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, FilesError> {
        self.get(file_id)?.line_range((), line_index)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_make_source_file() {
        let root_path = Path::new(
            "/Users/francoismonniot/Projects/github.com/fmonniot/zelkova-lang/std/core/src",
        );
        let abs_path = Path::new("/Users/francoismonniot/Projects/github.com/fmonniot/zelkova-lang/std/core/src/Platform/Cmd.zel");

        let res = SourceFile::load(abs_path.to_path_buf(), root_path, SourceRoot::Src, None);
        println!("{:?}", res);
    }
}
