//! Writing a build's output to disk.
//!
//! What goes where is [`javascript`](super::javascript)'s — its *Paths* section is the
//! only place a path into the output tree is built. This module takes the files a
//! build produced, each with its path under `build/out/js/`, and writes them: writing each
//! in turn, stopping at the first one that fails, and — only once every one of them has
//! landed — removing whatever else was already below `build/out/js/` that this build did
//! not just write.
//!
//! Pruning runs *after* writing, and only once writing succeeded in full, rather than
//! clearing the tree up front: clearing first would mean removing a previous good build
//! before the new one is known to succeed, so a write failure partway through would
//! leave nothing at all rather than the last good build.
//!
//! `write` also serializes itself process-wide with `WRITE_LOCK`. Write-then-prune
//! is not safe to run twice at once even so: two calls racing on the same directory can
//! still have one's prune remove a directory or file the other is mid-write into, which
//! is exactly the race this repository's own test suite hit once pruning was added —
//! several tests point `compile_package`/`compile_package_with_tests` at one shared
//! fixture directory (there is no per-test build directory unless a test asks for one
//! via `compile_package_into`) and `cargo test` runs them concurrently by default. A
//! single lock for every call is coarser than one keyed by `js_root` would be, but
//! nothing outside that test suite compiles more than one package at a time against the
//! same output directory, and the write-then-prune phase this guards is a small
//! fraction of a build's time next to checking and emitting.
//!
//! It is only ever reached with a whole build's files, once every module of every
//! package has checked and emitted: `compile_package` calls it when its error vector is
//! empty and at no other time. A build that failed to check or emit anywhere never
//! reaches this module at all, so `build/out/js/` is left exactly as an earlier successful
//! build (if any) wrote it.

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use super::PhaseError;

/// Serializes [`write`] process-wide — see this module's doc comment for why a call
/// cannot safely run concurrently with another.
static WRITE_LOCK: Mutex<()> = Mutex::new(());

/// One file of a build's output.
///
/// `Clone` because a test build's tree at `build/test/js/` starts as a copy of what
/// `compile` already emitted for `build/out/js/` — the runtime, the root's `src/` and every
/// plain dependency's modules — and adds its own extra files rather than emitting that
/// shared half a second time.
#[derive(Debug, Clone)]
pub struct File {
    /// Where it goes, relative to `build/out/js/`.
    pub path: PathBuf,
    pub contents: Contents,
}

/// What a [`File`] holds.
#[derive(Debug, Clone)]
pub enum Contents {
    /// Text the compiler produced: an emitted module, or the runtime.
    Text(String),
    /// A file copied as it is, from this path: a facade's companion.
    Copy(PathBuf),
    /// A file copied from `from` with each import [`javascript::rewrite_imports`] finds
    /// in `imports` replaced: the companion of a test facade, whose import of the
    /// companion it checks is spelled for the source tree
    /// ([`javascript::test_companion_import`]).
    ///
    /// [`javascript::rewrite_imports`]: super::javascript::rewrite_imports
    /// [`javascript::test_companion_import`]: super::javascript::test_companion_import
    Rewritten {
        from: PathBuf,
        imports: Vec<(String, String)>,
    },
}

/// A file of the output that could not be written, or a stale one that could not be
/// removed once every new file had been.
#[derive(Debug)]
pub struct Error {
    /// The file being written, or the stale file or directory being removed, in full.
    pub path: PathBuf,
    /// The companion it was being copied from, for a [`Contents::Copy`] or a
    /// [`Contents::Rewritten`].
    pub from: Option<PathBuf>,
    /// Set when the failure happened while removing a file or directory `js_root` no
    /// longer needs, not while writing one of `files`. Kept apart from `from` so the
    /// message names the step that actually failed rather than guessing it from which
    /// fields are `None`.
    pub pruning: bool,
    pub error: std::io::Error,
}

impl PhaseError for Error {
    fn message(&self) -> String {
        if self.pruning {
            return format!(
                "could not remove the stale `{}`: {}",
                self.path.display(),
                self.error
            );
        }
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

/// Write every one of `files` below `js_root`, creating the directories each needs, then
/// remove whatever else was already below `js_root` that is not one of `files` —
/// a module a previous build wrote that this one no longer holds, at any depth.
///
/// Writing stops at the first file that fails — the answer holds that one [`Error`] and
/// nothing past it is written, and nothing is pruned — rather than working through every
/// file the way loading or checking do; there is no diagnostic value in a second I/O
/// failure once the first has already made the build's output untrustworthy, and
/// continuing would make "which files actually landed" a question with no single
/// answer. Pruning runs only once every file wrote successfully, and stops at its own
/// first failure the same way. The answer is empty when every write and every removal
/// succeeded.
pub fn write(js_root: &Path, files: &[File]) -> Vec<Error> {
    // A poisoned lock (an earlier `write` panicked while holding it) leaves nothing
    // here that a fresh call cannot reestablish on its own — every step below either
    // fully succeeds or returns before touching more of the tree — so a later write
    // proceeds on the recovered guard rather than propagating the poison and refusing
    // to ever write again.
    let _guard = WRITE_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());

    for file in files {
        if let Err(error) = write_one(&js_root.join(&file.path), &file.contents) {
            return vec![error];
        }
    }

    if let Err(error) = prune(js_root, files) {
        return vec![error];
    }

    Vec::new()
}

fn write_one(path: &Path, contents: &Contents) -> Result<(), Error> {
    let from = match contents {
        Contents::Copy(from) | Contents::Rewritten { from, .. } => Some(from.clone()),
        Contents::Text(_) => None,
    };
    let failed = |error| Error {
        path: path.to_path_buf(),
        from: from.clone(),
        pruning: false,
        error,
    };

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(failed)?;
    }

    match contents {
        Contents::Text(text) => std::fs::write(path, text).map_err(failed),
        Contents::Copy(source) => std::fs::copy(source, path).map(|_| ()).map_err(failed),
        Contents::Rewritten { from, imports } => {
            let text = std::fs::read_to_string(from).map_err(failed)?;
            std::fs::write(path, super::javascript::rewrite_imports(&text, imports)).map_err(failed)
        }
    }
}

/// Remove every file below `js_root` whose path, relative to `js_root`, is not one of
/// `files`' — and, once its contents are gone, the directory that held it, when that
/// leaves the directory empty.
///
/// `js_root` not existing at all is not a failure — the first build a package ever gets
/// has nothing to prune — so [`std::io::ErrorKind::NotFound`] is swallowed like success
/// on the initial read and anything else is reported as an [`Error`].
fn prune(js_root: &Path, files: &[File]) -> Result<(), Error> {
    let keep: HashSet<&Path> = files.iter().map(|file| file.path.as_path()).collect();
    prune_dir(js_root, js_root, &keep)
}

fn pruning_failed(path: &Path, error: std::io::Error) -> Error {
    Error {
        path: path.to_path_buf(),
        from: None,
        pruning: true,
        error,
    }
}

fn prune_dir(js_root: &Path, dir: &Path, keep: &HashSet<&Path>) -> Result<(), Error> {
    let entries = match std::fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => return Err(pruning_failed(dir, error)),
    };

    for entry in entries {
        let entry = entry.map_err(|error| pruning_failed(dir, error))?;
        let path = entry.path();
        let file_type = entry
            .file_type()
            .map_err(|error| pruning_failed(&path, error))?;

        if file_type.is_dir() {
            prune_dir(js_root, &path, keep)?;
            // Best effort: a directory left empty by the removals above is untidy, not
            // a stale module, so failing to remove it — for instance because something
            // outside this build's knowledge is sitting in it — does not fail the build.
            let _ = std::fs::remove_dir(&path);
        } else {
            let relative = path.strip_prefix(js_root).unwrap_or(&path);
            if !keep.contains(relative) {
                std::fs::remove_file(&path).map_err(|error| pruning_failed(&path, error))?;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A directory under `std::env::temp_dir()`, unique per call and cleaned up on
    /// drop — the same small local helper `manifest`'s tests use, and for the same
    /// reason: `CARGO_TARGET_TMPDIR` is only set for integration test binaries, not for
    /// a unit test compiled into the library itself.
    struct TempDir(PathBuf);

    impl TempDir {
        fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    fn fresh_dir(test: &str) -> TempDir {
        let mut path = std::env::temp_dir();
        let unique = format!(
            "zelkova-output-test-{}-{}-{}",
            test,
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        );
        path.push(unique);
        TempDir(path)
    }

    fn files_under(dir: &Path) -> Vec<String> {
        fn walk(root: &Path, dir: &Path, out: &mut Vec<String>) {
            let Ok(entries) = std::fs::read_dir(dir) else {
                return;
            };
            for entry in entries {
                let path = entry.unwrap().path();
                if path.is_dir() {
                    walk(root, &path, out);
                } else {
                    out.push(
                        path.strip_prefix(root)
                            .unwrap()
                            .to_string_lossy()
                            .into_owned(),
                    );
                }
            }
        }
        let mut out = Vec::new();
        walk(dir, dir, &mut out);
        out.sort();
        out
    }

    /// A module dropped from one build to the next does not survive in `js_root`: `write`
    /// prunes whatever the new set of files does not need, rather than only adding to
    /// what is there.
    ///
    /// Mutation-checked by removing the `prune` call from `write`: `Old.mjs` then
    /// survives the second `write` and this goes red.
    #[test]
    fn write_removes_a_file_the_new_build_no_longer_has() {
        let dir = fresh_dir("write_removes_a_file_the_new_build_no_longer_has");
        let js_root = dir.path();

        let first = vec![File {
            path: "Old.mjs".into(),
            contents: Contents::Text("old".to_string()),
        }];
        assert!(write(js_root, &first).is_empty());
        assert_eq!(files_under(js_root), vec!["Old.mjs".to_string()]);

        let second = vec![File {
            path: "New.mjs".into(),
            contents: Contents::Text("new".to_string()),
        }];
        assert!(write(js_root, &second).is_empty());
        assert_eq!(
            files_under(js_root),
            vec!["New.mjs".to_string()],
            "`Old.mjs` from the first build must not survive the second"
        );
    }

    /// A stale module's directory is pruned too, once nothing inside it survives: a
    /// package dropped from the build does not leave an empty directory of its own name
    /// behind.
    ///
    /// Mutation-checked by commenting out the `std::fs::remove_dir` call in
    /// `prune_dir`: the empty `acme-widgets/` directory then survives and this goes red.
    #[test]
    fn write_removes_a_now_empty_package_directory() {
        let dir = fresh_dir("write_removes_a_now_empty_package_directory");
        let js_root = dir.path();

        let first = vec![File {
            path: Path::new("acme-widgets").join("Size.mjs"),
            contents: Contents::Text("old".to_string()),
        }];
        assert!(write(js_root, &first).is_empty());
        assert!(js_root.join("acme-widgets").is_dir());

        let second = vec![File {
            path: "App.mjs".into(),
            contents: Contents::Text("new".to_string()),
        }];
        assert!(write(js_root, &second).is_empty());
        assert!(
            !js_root.join("acme-widgets").exists(),
            "`acme-widgets` no longer holds any file this build wrote and must itself be gone"
        );
    }

    /// A file that cannot be written stops the write where it is: nothing after it in
    /// `files` is written, though a file already written earlier in the same call stays
    /// on disk — `write` does not roll a partial call back.
    ///
    /// Mutation-checked by reverting `write` to its previous `filter_map` shape, which
    /// keeps going after a failure: `After.mjs` is then written and this goes red.
    #[test]
    fn write_stops_at_the_first_failure() {
        let dir = fresh_dir("write_stops_at_the_first_failure");
        let js_root = dir.path();

        let files = vec![
            File {
                path: "Before.mjs".into(),
                contents: Contents::Text("before".to_string()),
            },
            // A companion copied from a source that does not exist — deterministic,
            // unlike a permissions failure, and exercises the same `write_one` error
            // path a real missing companion would.
            File {
                path: "Broken.companion.mjs".into(),
                contents: Contents::Copy(js_root.join("does-not-exist.mjs")),
            },
            File {
                path: "After.mjs".into(),
                contents: Contents::Text("after".to_string()),
            },
        ];

        let errors = write(js_root, &files);
        assert_eq!(errors.len(), 1, "got {:?}", errors);
        assert!(!errors[0].pruning);
        assert_eq!(errors[0].path, js_root.join("Broken.companion.mjs"));

        assert_eq!(
            files_under(js_root),
            vec!["Before.mjs".to_string()],
            "`Before.mjs` was already written when `Broken.companion.mjs` failed, and \
             `After.mjs` must never have been reached"
        );
    }
}
