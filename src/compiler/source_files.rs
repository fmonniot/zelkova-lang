use codespan_reporting::files::{Files, SimpleFile};
use std::ops::Range;
use std::path::{Path, PathBuf};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SourceFileId(usize);

// This should probably implements `Files` directly instead of relying on SimpleFile
#[derive(Debug)] // TODO Implement Debug manually (don't want to embedded the entire source code)
pub struct SourceFile {
    module_name: String,
    relative_path: PathBuf,
    /// `Files` implementation with the name being the relative path within the package
    file: SimpleFile<String, String>,
}

impl SourceFile {

    /// Load a `SourceFile` from the file system
    pub fn load(
        abs_path: PathBuf,
        root: &Path,
    ) -> Result<SourceFile, SourceFileError> {
        let relative_path = abs_path.strip_prefix(root)?.to_path_buf();

        let file_name = relative_path
            .to_str()
            .ok_or_else(|| SourceFileError::NonUtf8Module(relative_path.clone()))?
            .to_owned();
    
        let module_name = file_name
            .trim_end_matches(".zel")
            .replace(std::path::MAIN_SEPARATOR, ".");
    
        let source = std::fs::read_to_string(abs_path)?;
    
        Ok(SourceFile { module_name, relative_path, file: SimpleFile::new(file_name, source) })
    }

    pub fn file(&self) -> &SimpleFile<String, String> {
        &self.file
    }
}

#[derive(Debug)]
pub enum SourceFileError {
    PathPrefix(std::path::StripPrefixError),
    Io(std::io::Error),
    NonUtf8Module(PathBuf),
}

impl From<std::path::StripPrefixError> for SourceFileError {
    fn from(err: std::path::StripPrefixError) -> Self {
        SourceFileError::PathPrefix(err)
    }
}

impl From<std::io::Error> for SourceFileError {
    fn from(err: std::io::Error) -> Self {
        SourceFileError::Io(err)
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

impl<'a> SourceFiles {
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
    pub fn get(&self, file_id: SourceFileId) -> Option<&SimpleFile<String, String>> {
        self.files.get(file_id.0).map(|f| &f.file)
    }

    pub fn iter(&self) -> impl Iterator<Item=(SourceFileId, &'_ SourceFile)> + '_ {
        self.files.iter().enumerate().map(|(i, v)| (SourceFileId(i), v))
    }

    // Will probably need an accessor module name -> Option<SourceFileId>
}

impl<'a> Files<'a> for SourceFiles {
    type FileId = SourceFileId;
    type Name = String;
    type Source = &'a String;

    fn name(&self, file_id: Self::FileId) -> Option<String> {
        Some(self.get(file_id)?.name().clone())
    }

    fn source(&self, file_id: Self::FileId) -> Option<&String> {
        Some(self.get(file_id)?.source())
    }

    fn line_index(&self, file_id: Self::FileId, byte_index: usize) -> Option<usize> {
        self.get(file_id)?.line_index((), byte_index)
    }

    fn line_range(&self, file_id: Self::FileId, line_index: usize) -> Option<Range<usize>> {
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

        let res = SourceFile::load(abs_path.to_path_buf(), &root_path);
        println!("{:?}", res);
    }
}