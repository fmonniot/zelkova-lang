//! `zelkova.toml`, the package manifest.
//!
//! [`docs/spec/packages.md`](../../../docs/spec/packages.md#the-manifest) is the normative
//! shape: six fields, five of them required. This module reads the file, deserializes it
//! with `serde`/`toml`, and validates each field on its own terms — a legal package
//! [`name`](Manifest::name), a three-integer [`version`](Manifest::version), and each
//! `dependencies`/`test-dependencies` entry naming exactly one source. `private_modules` is
//! checked against the modules the package actually holds by [`compile_package`]'s caller,
//! once source loading has produced that list — this module only knows the manifest, never
//! the package's files, so [`load`] returns the names as written.
//!
//! Reading a manifest is where a build starts and not what it is:
//! [`resolve`](super::resolve) is what follows both dependency maps to the other packages
//! and decides what each module is called in each of them. `main` is validated here and read
//! nowhere, since there is no `Task` for a program's entry point to hold.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use super::name::Name;
use super::{PackageName, PhaseError};

/// The file every package directory must hold, beside `src/`.
pub const MANIFEST_FILE_NAME: &str = "zelkova.toml";

/// A validated `zelkova.toml`.
///
/// Built only by [`load`], which is the one place a [`RawManifest`] — the shape `serde`
/// deserializes directly, still full of unchecked strings — is turned into this one field by
/// field. `private_modules` holds the names exactly as written; whether each one names a
/// module the package actually holds is checked by the caller once it has loaded the
/// package's sources (see the module documentation).
#[derive(Debug, Clone)]
pub struct Manifest {
    pub name: PackageName,
    pub version: Version,
    pub main: Option<Name>,
    pub private_modules: Vec<Name>,
    pub dependencies: HashMap<PackageName, Dependency>,
    pub test_dependencies: HashMap<PackageName, Dependency>,
}

/// Three non-negative integers, with no pre-release suffix and no build metadata —
/// `docs/spec/packages.md`'s whole rule for `version`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

/// The three integers, dot-separated, exactly as a manifest writes them. A diagnostic
/// naming a package names the version it was resolved to beside it.
impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl Version {
    fn parse(raw: &str) -> Option<Version> {
        let mut parts = raw.split('.');
        let major = parts.next()?.parse().ok()?;
        let minor = parts.next()?.parse().ok()?;
        let patch = parts.next()?.parse().ok()?;
        if parts.next().is_some() {
            return None;
        }
        Some(Version {
            major,
            minor,
            patch,
        })
    }
}

/// One entry of `dependencies` or `test-dependencies`: a source, and whether this package
/// sees it wrapped (the default) or unwrapped.
#[derive(Debug, Clone)]
pub struct Dependency {
    pub source: Source,
    pub wrapped: bool,
}

/// Where a dependency comes from — exactly one of the two, per
/// `docs/spec/packages.md#where-a-dependency-comes-from`.
#[derive(Debug, Clone)]
pub enum Source {
    Git { url: String, pin: GitPin },
    Path { path: String },
}

/// A `git` source asks for a version, by way of the tag it was published under, or pins one
/// commit in full — never both, and never neither.
#[derive(Debug, Clone)]
pub enum GitPin {
    Version(String),
    Rev(String),
}

/// The shape `serde` deserializes `zelkova.toml` into directly, before any field has been
/// checked. `main` is the one field genuinely optional at this layer — every other field is
/// required by the spec, so its absence is itself a [`ManifestError`], which is what leaving
/// off `#[serde(default)]` on the others gets for free: a missing `dependencies` table fails
/// to deserialize with "missing field" rather than silently becoming empty.
///
/// `deny_unknown_fields` closes the same door from the other side. The spec's six fields are
/// the whole of a manifest, so a key that is not one of them is a misspelling (`mian = "App"`
/// would otherwise read as a library with no entry point) or a field a newer spec added and
/// this compiler cannot honour.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawManifest {
    name: String,
    version: String,
    #[serde(default)]
    main: Option<String>,
    #[serde(rename = "private-modules")]
    private_modules: Vec<String>,
    dependencies: HashMap<String, RawDependency>,
    #[serde(rename = "test-dependencies")]
    test_dependencies: HashMap<String, RawDependency>,
}

#[derive(Debug, Deserialize)]
struct RawDependency {
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    git: Option<String>,
    #[serde(default)]
    path: Option<String>,
    #[serde(default)]
    rev: Option<String>,
    #[serde(default = "wrapped_by_default")]
    wrapped: bool,
}

fn wrapped_by_default() -> bool {
    true
}

/// Every way reading a manifest can fail.
///
/// Like [`SourceFileError`](super::source::files::SourceFileError), a manifest error has no
/// [`Span`](super::position::Span) to render: the location it would want is a byte range in
/// `zelkova.toml`, which is not a file the [`Files`](super::source::files::Files) database
/// holds. Every variant therefore carries the `manifest_path` it was found in, so that its
/// [`message`](PhaseError::message) can name the file on its own — which is the whole of the
/// location a reader gets, and is why every variant but
/// [`PrivateModuleNotFound`](ManifestError::PrivateModuleNotFound) goes back to the caller
/// unrendered, the same way loading errors already do.
#[derive(Debug)]
pub enum ManifestError {
    /// No `zelkova.toml` at all beside `src/`.
    Missing { manifest_path: PathBuf },
    /// `zelkova.toml` exists but could not be read (permissions, not a regular file, …).
    Unreadable {
        manifest_path: PathBuf,
        message: String,
    },
    /// `zelkova.toml` is not well-formed TOML, or does not have the shape [`RawManifest`]
    /// expects — a missing required field among them.
    Malformed {
        manifest_path: PathBuf,
        message: String,
    },
    /// `name` is not ASCII lowercase letters, digits and hyphens, starting with a letter,
    /// with every hyphen followed by a letter.
    InvalidName {
        manifest_path: PathBuf,
        name: String,
    },
    /// `version` is not exactly three non-negative integers separated by dots.
    InvalidVersion {
        manifest_path: PathBuf,
        version: String,
    },
    /// A key of `dependencies` or `test-dependencies` is not a legal package name.
    InvalidDependencyName {
        manifest_path: PathBuf,
        table: &'static str,
        name: String,
    },
    /// An entry of `dependencies` or `test-dependencies` does not name exactly one source,
    /// or pairs that source with a field it does not take.
    InvalidDependencyEntry {
        manifest_path: PathBuf,
        table: &'static str,
        package: String,
        reason: String,
    },
    /// One package name in both `dependencies` and `test-dependencies` — the spec allows a
    /// name in at most one of the two.
    DependencyListedTwice {
        manifest_path: PathBuf,
        name: String,
    },
    /// `private-modules` names a module this package does not hold.
    ///
    /// Unlike every other variant here, this one cannot be raised by [`load`]: knowing what
    /// the package holds needs its sources loaded, which happens only after the manifest
    /// has already been accepted. `compile_package` builds this once it has parsed every
    /// module, and folds it into the normal per-package error accumulation rather than the
    /// unrendered path the rest of this enum takes.
    PrivateModuleNotFound { manifest_path: PathBuf, name: Name },
}

impl PhaseError for ManifestError {
    fn message(&self) -> String {
        match self {
            ManifestError::Missing { manifest_path } => {
                format!("`{}` has no manifest", manifest_path.display())
            }
            ManifestError::Unreadable {
                manifest_path,
                message,
            } => format!(
                "`{}` could not be read: {}",
                manifest_path.display(),
                message
            ),
            ManifestError::Malformed {
                manifest_path,
                message,
            } => format!(
                "`{}` is not a valid manifest: {}",
                manifest_path.display(),
                message
            ),
            ManifestError::InvalidName {
                manifest_path,
                name,
            } => format!(
                "`{}` declares the name `{}`, which is not a legal package name: a package name \
                 is ASCII lowercase letters, digits and hyphens, starting with a letter, with \
                 every hyphen followed by a letter",
                manifest_path.display(),
                name
            ),
            ManifestError::InvalidVersion {
                manifest_path,
                version,
            } => format!(
                "`{}` declares the version `{}`, which is not a legal version: a version is \
                 exactly three non-negative integers separated by dots",
                manifest_path.display(),
                version
            ),
            ManifestError::InvalidDependencyName {
                manifest_path,
                table,
                name,
            } => format!(
                "`{}` in `[{}]` of `{}` is not a legal package name",
                name,
                table,
                manifest_path.display()
            ),
            ManifestError::InvalidDependencyEntry {
                manifest_path,
                table,
                package,
                reason,
            } => format!(
                "`{}` in `[{}]` of `{}` {}",
                package,
                table,
                manifest_path.display(),
                reason
            ),
            ManifestError::DependencyListedTwice {
                manifest_path,
                name,
            } => format!(
                "`{}` lists `{}` in both `dependencies` and `test-dependencies`",
                manifest_path.display(),
                name
            ),
            ManifestError::PrivateModuleNotFound {
                manifest_path,
                name,
            } => format!(
                "`private-modules` in `{}` names `{}`, which this package does not hold",
                manifest_path.display(),
                name
            ),
        }
    }
}

/// Read and validate the manifest of the package rooted at `package_dir`.
///
/// Every failure this can produce is raised before `compile_package` builds the file
/// database `load_package_sources` needs, which is why this returns a plain `Vec` rather
/// than going through a rendered `Diagnostic` — there is nothing yet to render one against.
pub fn load(package_dir: &Path) -> Result<Manifest, Vec<ManifestError>> {
    let manifest_path = package_dir.join(MANIFEST_FILE_NAME);

    let text = match std::fs::read_to_string(&manifest_path) {
        Ok(text) => text,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Err(vec![ManifestError::Missing { manifest_path }]);
        }
        Err(err) => {
            return Err(vec![ManifestError::Unreadable {
                manifest_path,
                message: err.to_string(),
            }]);
        }
    };

    let raw: RawManifest = match toml::from_str(&text) {
        Ok(raw) => raw,
        Err(err) => {
            return Err(vec![ManifestError::Malformed {
                manifest_path,
                message: err.to_string(),
            }]);
        }
    };

    let mut errors = Vec::new();

    let name = PackageName::new(raw.name.clone());
    if name.is_err() {
        errors.push(ManifestError::InvalidName {
            manifest_path: manifest_path.clone(),
            name: raw.name,
        });
    }

    let version = Version::parse(&raw.version);
    if version.is_none() {
        errors.push(ManifestError::InvalidVersion {
            manifest_path: manifest_path.clone(),
            version: raw.version,
        });
    }

    let private_modules: Vec<Name> = raw.private_modules.iter().map(Name::new).collect();
    let main = raw.main.as_deref().map(Name::new);

    let mut dependencies = HashMap::new();
    for (package, raw_dep) in raw.dependencies {
        match validate_dependency(&manifest_path, "dependencies", package, raw_dep) {
            Ok((name, dep)) => {
                dependencies.insert(name, dep);
            }
            Err(err) => errors.push(err),
        }
    }

    let mut test_dependencies = HashMap::new();
    for (package, raw_dep) in raw.test_dependencies {
        match validate_dependency(&manifest_path, "test-dependencies", package, raw_dep) {
            Ok((name, dep)) => {
                test_dependencies.insert(name, dep);
            }
            Err(err) => errors.push(err),
        }
    }

    for shared in dependencies.keys() {
        if test_dependencies.contains_key(shared) {
            errors.push(ManifestError::DependencyListedTwice {
                manifest_path: manifest_path.clone(),
                name: shared.as_str().to_string(),
            });
        }
    }

    match (name, version) {
        (Ok(name), Some(version)) if errors.is_empty() => Ok(Manifest {
            name,
            version,
            main,
            private_modules,
            dependencies,
            test_dependencies,
        }),
        _ => Err(errors),
    }
}

/// Turn one raw `dependencies`/`test-dependencies` entry into a [`Dependency`], or the one
/// [`ManifestError`] naming why it is not one. `table` names which of the two maps `package`
/// came from, purely so the message can say where to look.
fn validate_dependency(
    manifest_path: &Path,
    table: &'static str,
    package: String,
    raw: RawDependency,
) -> Result<(PackageName, Dependency), ManifestError> {
    let name =
        PackageName::new(package.clone()).map_err(|_| ManifestError::InvalidDependencyName {
            manifest_path: manifest_path.to_path_buf(),
            table,
            name: package.clone(),
        })?;

    let entry_error = |reason: &str| ManifestError::InvalidDependencyEntry {
        manifest_path: manifest_path.to_path_buf(),
        table,
        package: package.clone(),
        reason: reason.to_string(),
    };

    let dependency = match (raw.git, raw.path) {
        (Some(_), Some(_)) => {
            return Err(entry_error(
                "names both `git` and `path`; an entry must name exactly one source",
            ))
        }
        (None, None) => {
            return Err(entry_error(
                "names neither `git` nor `path`; a bare version constraint names no place a \
                 package could be found",
            ))
        }
        (Some(url), None) => match (raw.version, raw.rev) {
            (Some(version), None) => Dependency {
                source: Source::Git {
                    url,
                    pin: GitPin::Version(version),
                },
                wrapped: raw.wrapped,
            },
            (None, Some(rev)) => Dependency {
                source: Source::Git {
                    url,
                    pin: GitPin::Rev(rev),
                },
                wrapped: raw.wrapped,
            },
            (None, None) => {
                return Err(entry_error(
                    "names a `git` source with neither a `version` nor a `rev`",
                ))
            }
            (Some(_), Some(_)) => {
                return Err(entry_error(
                    "names a `git` source with both a `version` and a `rev`; a `git` entry \
                     asks for one or the other",
                ))
            }
        },
        (None, Some(path)) => {
            if raw.version.is_some() || raw.rev.is_some() {
                return Err(entry_error(
                    "names a `path` source alongside a `version` or a `rev`; a `path` source \
                     carries neither",
                ));
            }
            Dependency {
                source: Source::Path { path },
                wrapped: raw.wrapped,
            }
        }
    };

    Ok((name, dependency))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_manifest(dir: &Path, contents: &str) {
        std::fs::write(dir.join(MANIFEST_FILE_NAME), contents).unwrap();
    }

    /// A missing manifest names the file it looked for, not just the directory.
    #[test]
    fn missing_manifest_names_the_file() {
        let dir = tempdir();

        let errors = load(dir.path()).expect_err("no zelkova.toml was written");

        assert_eq!(errors.len(), 1, "got {:?}", errors);
        match &errors[0] {
            ManifestError::Missing { manifest_path } => {
                assert_eq!(manifest_path, &dir.path().join("zelkova.toml"));
            }
            other => panic!("expected Missing, got {:?}", other),
        }
    }

    /// An illegal `name` is reported as `InvalidName`, naming the offending string, even
    /// when every other field is well-formed.
    #[test]
    fn illegal_name_is_rejected() {
        let dir = tempdir();
        write_manifest(
            dir.path(),
            r#"
                name = "Not_Legal"
                version = "0.1.0"
                private-modules = []

                [dependencies]

                [test-dependencies]
            "#,
        );

        let errors = load(dir.path()).expect_err("`Not_Legal` is not a legal package name");

        assert_eq!(errors.len(), 1, "got {:?}", errors);
        match &errors[0] {
            ManifestError::InvalidName {
                manifest_path,
                name,
            } => {
                assert_eq!(name, "Not_Legal");
                assert_eq!(manifest_path, &dir.path().join(MANIFEST_FILE_NAME));
            }
            other => panic!("expected InvalidName, got {:?}", other),
        }

        // The sentence the user is shown has to carry the file as well as the field, since
        // it is the only location a manifest error ever gets.
        let message = errors[0].message();
        assert!(
            message.contains("Not_Legal") && message.contains(MANIFEST_FILE_NAME),
            "expected the name and the manifest path in the message, got {:?}",
            message
        );
    }

    /// A key that is not one of the manifest's six fields is reported rather than ignored:
    /// `mian = "App"` is a misspelt `main`, and a manifest reader that shrugs at it reads the
    /// package as a library.
    #[test]
    fn an_unknown_key_is_rejected() {
        let dir = tempdir();
        write_manifest(
            dir.path(),
            r#"
                name = "todo"
                version = "0.1.0"
                mian = "App"
                private-modules = []

                [dependencies]

                [test-dependencies]
            "#,
        );

        let errors = load(dir.path()).expect_err("`mian` is not a manifest field");

        assert_eq!(errors.len(), 1, "got {:?}", errors);
        match &errors[0] {
            ManifestError::Malformed { message, .. } => assert!(
                message.contains("mian"),
                "expected the unknown key to be named, got {:?}",
                message
            ),
            other => panic!("expected Malformed, got {:?}", other),
        }
    }

    /// A well-formed manifest is read into every field, dependencies included.
    #[test]
    fn a_well_formed_manifest_loads() {
        let dir = tempdir();
        write_manifest(
            dir.path(),
            r#"
                name = "todo"
                version = "0.4.1"
                main = "App"
                private-modules = ["Model.Internal"]

                [dependencies.acme-widgets]
                version = "^1.2.0"
                git = "https://github.com/acme/widgets"

                [dependencies.acme-parser]
                path = "../acme-parser"
                wrapped = false

                [test-dependencies.acme-expect]
                version = "^2.0.0"
                git = "https://github.com/acme/expect"
            "#,
        );

        let manifest = load(dir.path()).expect("manifest is well-formed");

        assert_eq!(manifest.name.as_str(), "todo");
        assert_eq!(
            manifest.version,
            Version {
                major: 0,
                minor: 4,
                patch: 1
            }
        );
        assert_eq!(manifest.main, Some(Name::new("App")));
        assert_eq!(manifest.private_modules, vec![Name::new("Model.Internal")]);
        assert_eq!(manifest.dependencies.len(), 2);
        assert_eq!(manifest.test_dependencies.len(), 1);

        let widgets = &manifest.dependencies[&PackageName::new("acme-widgets").unwrap()];
        assert!(widgets.wrapped);
        match &widgets.source {
            Source::Git {
                url,
                pin: GitPin::Version(v),
            } => {
                assert_eq!(url, "https://github.com/acme/widgets");
                assert_eq!(v, "^1.2.0");
            }
            other => panic!("expected a Git/Version source, got {:?}", other),
        }

        let parser = &manifest.dependencies[&PackageName::new("acme-parser").unwrap()];
        assert!(!parser.wrapped);
        match &parser.source {
            Source::Path { path } => assert_eq!(path, "../acme-parser"),
            other => panic!("expected a Path source, got {:?}", other),
        }
    }

    /// A dependency entry naming neither `git` nor `path` is rejected — the ticket's
    /// "a bare version string is invalid" case, spelled as an entry with no source at all
    /// (a bare `acme-widgets = "^1.2.0"` does not even deserialize as a table and is caught
    /// as `Malformed` instead; this is the same rule applied to a table that still forgot
    /// the source).
    #[test]
    fn a_dependency_with_no_source_is_rejected() {
        let dir = tempdir();
        write_manifest(
            dir.path(),
            r#"
                name = "todo"
                version = "0.1.0"
                private-modules = []

                [dependencies.acme-widgets]
                version = "^1.2.0"

                [test-dependencies]
            "#,
        );

        let errors = load(dir.path()).expect_err("a dependency with no source must be rejected");

        assert_eq!(errors.len(), 1, "got {:?}", errors);
        match &errors[0] {
            ManifestError::InvalidDependencyEntry { table, package, .. } => {
                assert_eq!(*table, "dependencies");
                assert_eq!(package, "acme-widgets");
            }
            other => panic!("expected InvalidDependencyEntry, got {:?}", other),
        }
    }

    /// A bare version string — `acme-widgets = "^1.2.0"` rather than a table — is not a
    /// legal entry at all, and is caught as a malformed manifest.
    #[test]
    fn a_bare_version_string_is_malformed() {
        let dir = tempdir();
        write_manifest(
            dir.path(),
            r#"
                name = "todo"
                version = "0.1.0"
                private-modules = []

                [dependencies]
                acme-widgets = "^1.2.0"

                [test-dependencies]
            "#,
        );

        let errors = load(dir.path()).expect_err("a bare version string is not a legal entry");

        assert_eq!(errors.len(), 1, "got {:?}", errors);
        assert!(
            matches!(errors[0], ManifestError::Malformed { .. }),
            "expected Malformed, got {:?}",
            errors[0]
        );
    }

    /// A directory under `std::env::temp_dir()`, unique per call and cleaned up on drop —
    /// this module's own tests are the only thing in the tree that needs an on-disk
    /// manifest, so a tiny local helper is enough and avoids a new dev-dependency.
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

    fn tempdir() -> TempDir {
        let mut path = std::env::temp_dir();
        let unique = format!(
            "zelkova-manifest-test-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        );
        path.push(unique);
        std::fs::create_dir_all(&path).unwrap();
        TempDir(path)
    }
}
