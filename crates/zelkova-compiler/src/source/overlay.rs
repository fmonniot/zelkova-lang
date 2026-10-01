use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// The text of the buffers an editor holds open, to be read in place of what is on disk.
///
/// A module's text normally comes from its file. A language server checks what the
/// editor holds, which differs from the disk until the user saves, so the checking entry
/// points ([`check_package`](crate::check_package) and
/// [`check_package_with_tests`](crate::check_package_with_tests)) take an
/// `Overlay` and every `.zel` file is read through it. The functions that write a build
/// take none: no build is ever written from the text of a buffer.
///
/// An `Overlay` is a concrete type and not a trait, because it is the only source of
/// text other than the disk anything wants. The functions that take it take an
/// `&Overlay` where a `&dyn` would go, so making it one later changes a type and no call
/// graph.
///
/// The rules, which outlive the ticket (`TOOL-2`) that set them:
///
/// - **A path is normalised before every comparison, on both sides.** [`insert`],
///   [`remove`] and every lookup canonicalize the deepest ancestor of the path that
///   exists on disk and append the rest unchanged, so a path that is not on disk yet
///   still normalises. The two sides are spelled differently otherwise: package roots are
///   canonicalized by `resolve`, so a walk yields paths under the canonical root, while a
///   caller's path is whatever its editor reported (`/tmp/..` against `/private/tmp/..`
///   on macOS). The walk follows symbolic links, so a file reached through one is matched
///   by where it points. Normalisation is for the lookup only: a module is still named
///   from the path the walk yielded.
/// - **The walk adds what only the overlay holds.** After the walk of a source root, every
///   key that ends in `.zel`, sits under the normalised form of that root and was not
///   matched by a walked file is loaded too, in path order, and named by its path under
///   the normalised root. A `tests/` that is not on disk does not stop that, but a `src/`
///   that is not on disk is the error it always is, whatever the overlay holds.
/// - **It only replaces and adds.** There is no way to say a file on disk is gone: an
///   editor that deletes a module deletes the file, and the next walk does not find it.
/// - **Only `.zel` sources go through it.** A key that does not end in `.zel` is ignored,
///   so a caller may hand over every open buffer unfiltered. The manifest is read from
///   disk, because one being typed is malformed most of the time and its error would
///   clear every `.zel` diagnostic of the package; a facade's companion `.mjs` is read
///   from disk too, by a build that writes, which takes no overlay.
/// - **The package has to exist on disk**, with its `zelkova.toml` and its `src/`, for the
///   overlay to apply to it. A package that exists only in memory is out of scope.
///
/// [`insert`]: Overlay::insert
/// [`remove`]: Overlay::remove
#[derive(Debug, Default, Clone)]
pub struct Overlay {
    buffers: HashMap<PathBuf, String>,
}

impl Overlay {
    /// An overlay holding no buffer: every file is read from disk.
    pub fn new() -> Overlay {
        Overlay::default()
    }

    /// Hold `text` as the contents of the file at `path`, replacing any earlier text for
    /// the same file.
    pub fn insert(&mut self, path: impl AsRef<Path>, text: String) {
        self.buffers.insert(normalise(path.as_ref()), text);
    }

    /// Stop holding the file at `path`, so it is read from disk again. Returns its text
    /// if it was held.
    pub fn remove(&mut self, path: impl AsRef<Path>) -> Option<String> {
        self.buffers.remove(&normalise(path.as_ref()))
    }

    /// The text held for the file at `path`, if any.
    pub(super) fn get(&self, path: &Path) -> Option<&str> {
        self.buffers.get(&normalise(path)).map(String::as_str)
    }

    /// Every key that ends in `.zel` and sits under `root`, in path order. `root` must
    /// already be normalised.
    pub(super) fn zel_files_under(&self, root: &Path) -> Vec<&Path> {
        let mut paths: Vec<&Path> = self
            .buffers
            .keys()
            .map(PathBuf::as_path)
            .filter(|path| path.starts_with(root))
            .filter(|path| path.extension().is_some_and(|ext| ext == "zel"))
            .collect();
        paths.sort();
        paths
    }
}

/// The one spelling of a path that both sides of a comparison are put in: the deepest
/// ancestor that exists on disk, canonicalized, with the components below it appended
/// unchanged. A path none of whose ancestors can be canonicalized (a relative path with no
/// existing prefix, or one whose last component is `..` below a missing directory) comes
/// back as it was.
pub(super) fn normalise(path: &Path) -> PathBuf {
    let mut rest: Vec<std::ffi::OsString> = Vec::new();
    let mut current = path.to_path_buf();
    loop {
        match current.canonicalize() {
            Ok(mut base) => {
                base.extend(rest.iter().rev());
                return base;
            }
            Err(_) => match (current.file_name(), current.parent()) {
                (Some(name), Some(parent)) => {
                    rest.push(name.to_owned());
                    let parent = parent.to_path_buf();
                    current = parent;
                }
                _ => return path.to_path_buf(),
            },
        }
    }
}
