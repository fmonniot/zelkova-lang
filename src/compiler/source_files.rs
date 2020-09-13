use codespan_reporting::files::{Files, SimpleFile};
use std::ops::Range;
use std::slice::Iter;

/// A file database that can store multiple source files.
///
/// This is taking as is from `codespan_reporting::files::SimpleFiles` with
/// the added ability to iterate over the files
///
/// There are a few TODOs on this structure that may or may not be needed:
/// - Use a custom `SourceFile` structure instead of `SimpleFile`
/// - Use a deterministic `FileId` instead of using `usize`
pub struct SourceFiles {
    files: Vec<SimpleFile<String, String>>,
}

impl<'a> SourceFiles {
    /// Create a new files database.
    pub fn new() -> SourceFiles {
        SourceFiles { files: Vec::new() }
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again.
    pub fn add(&mut self, name: String, source: String) -> usize {
        let file_id = self.files.len();
        self.files.push(SimpleFile::new(name, source));
        file_id
    }

    /// Get the file corresponding to the given id.
    pub fn get(&self, file_id: usize) -> Option<&SimpleFile<String, String>> {
        self.files.get(file_id)
    }

    // TODO Should probably return an Iterator instead of the Iter struct
    pub fn iter(&self) -> Iter<'_, SimpleFile<String, String>> {
        self.files.iter()
    }
}

impl<'a> Files<'a> for SourceFiles {
    type FileId = usize;
    type Name = String;
    type Source = &'a String;

    fn name(&self, file_id: usize) -> Option<String> {
        Some(self.get(file_id)?.name().clone())
    }

    fn source(&self, file_id: usize) -> Option<&String> {
        Some(self.get(file_id)?.source())
    }

    fn line_index(&self, file_id: usize, byte_index: usize) -> Option<usize> {
        self.get(file_id)?.line_index((), byte_index)
    }

    fn line_range(&self, file_id: usize, line_index: usize) -> Option<Range<usize>> {
        self.get(file_id)?.line_range((), line_index)
    }
}
