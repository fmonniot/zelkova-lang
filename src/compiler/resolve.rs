//! The packages a build is made from, and the module names each of them sees.
//!
//! A package is compiled against its own modules and the public modules of the packages
//! its manifest lists — [*Imports across a package
//! boundary*](../../../docs/spec/packages.md#imports-across-a-package-boundary). This
//! module answers the two questions that has to be settled before any module of any
//! package is compiled:
//!
//! 1. **Which packages are in the build**, and in what order they can be compiled.
//!    [`resolve`] walks the manifests from the root package outwards and hands back the
//!    set it reached, dependencies first.
//! 2. **What each module is called** inside the package that imports it.
//!    [`visible_modules`] builds that one map — the package's own modules, plus each
//!    direct dependency's public ones under [the namespace](PackageName::namespace) or,
//!    for an unwrapped dependency, under their own names — and reports a name two
//!    modules both answer to.
//!
//! The collision check is here, rather than in the phase that would trip over the
//! ambiguity, because the manifest is what creates it and the manifest is what has to
//! change. A package with no coherent answer for a name is stopped before any of its
//! modules is canonicalized — its files are read and parsed first, since the map is
//! keyed by the names the module headers declare.
//!
//! # What this obtains, and what it does not
//!
//! A `path` dependency is read where it lies and compiled from source like any other
//! package. A `git` dependency is not obtained at all — fetching a source, caching it
//! and recording the result in `zelkova.lock` is [the toolchain
//! appendix](../../../docs/spec/toolchain.md#where-a-dependency-comes-from)'s, and none
//! of it is written yet — so an entry naming one is
//! [`Error::UnsupportedSource`](Error::UnsupportedSource) rather than a package quietly
//! missing from the build.
//!
//! `zelkova-core` is a dependency of every package and [is not written in
//! `dependencies`](../../../docs/spec/packages.md#zelkova-core-is-a-dependency-of-every-package);
//! nothing here supplies it, because a compiler that ships its own core has to know
//! where that copy sits and nothing yet says where. What is honoured is the other half
//! of that rule: a package named `zelkova-core` is seen unwrapped whatever the entry
//! naming it says, so `Basics` is `Basics` — which is what
//! [`scalars`](super::scalars) relies on, since it knows a scalar by the bare qualified
//! name `Basics.Int` ([`DEC-15` decision
//! 1](../../../docs/decisions/dec-15.md#1--a-scalar-type-is-known-by-its-qualified-name)).

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::manifest::{Manifest, ManifestError, Source, Version, MANIFEST_FILE_NAME};
use super::name::Name;
use super::{PackageName, PhaseError};

/// The package every other package is seen through, and the one package name the
/// compiler knows on its own.
///
/// It is [seen unwrapped in every
/// package](../../../docs/spec/packages.md#zelkova-core-is-a-dependency-of-every-package),
/// which is what makes `Basics` `Basics` everywhere and keeps `Basics.Int` — the name
/// [`scalars`](super::scalars) recognises a scalar by — pointing at one declaration.
pub const CORE_PACKAGE: &str = "zelkova-core";

/// One package of the resolved build: its name, the directory holding its
/// `zelkova.toml`, and that manifest.
#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    pub name: PackageName,
    pub root: PathBuf,
    pub manifest: Manifest,
}

impl ResolvedPackage {
    /// The path of this package's manifest, which is the location every error about it
    /// names.
    pub fn manifest_path(&self) -> PathBuf {
        self.root.join(MANIFEST_FILE_NAME)
    }

    /// Whether this package's modules are seen by their own names in a package that
    /// depends on it, given what that package's entry for it says.
    ///
    /// `wrapped` is the entry's own flag; `zelkova-core` overrides it, since it is seen
    /// unwrapped in every package whatever any manifest says.
    fn seen_unwrapped(&self, wrapped: bool) -> bool {
        !wrapped || self.name.as_str() == CORE_PACKAGE
    }
}

/// How a module came to be visible in the package being compiled, which is the whole of
/// what a collision diagnostic has to say about each of the two modules claiming one
/// name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OriginKind {
    /// Declared by the package being compiled, in the file named — `src/Model.zel`.
    ///
    /// The file is part of the answer because a package's two source roots share one
    /// set of module names, so naming the package is not enough to say which of two
    /// local modules a collision is about.
    Local { file: String },
    /// A public module of a dependency seen unwrapped, so it is named by its own name.
    Unwrapped,
    /// A public module of a wrapped dependency, named through that package's namespace.
    Namespaced,
}

/// One entry of the map of names a package can import: which package the module belongs
/// to, what that package calls it, and how it came to be visible here.
///
/// `package` and `module` together are the module's identity — the pair an
/// [`Interface`](super::Interface) is found by, and `module` is always the module's
/// name *within its own package*. The spelling this entry is keyed under is a different
/// thing, and belongs to the importing package alone.
#[derive(Debug, Clone)]
pub struct ModuleOrigin {
    pub package: PackageName,
    pub version: Version,
    pub module: Name,
    pub kind: OriginKind,
}

impl ModuleOrigin {
    /// This origin as one line of a collision diagnostic — the package it is in, that
    /// package's version, and how it reached the package being compiled.
    ///
    /// The namespaced case names the module as its own package calls it, because that
    /// is the one thing the collision's headline — which quotes the spelling — does not
    /// already say.
    fn describe(&self) -> String {
        let how = match &self.kind {
            OriginKind::Local { file } => format!("`{}`, a module of this package", file),
            OriginKind::Unwrapped => "imported unwrapped".to_string(),
            OriginKind::Namespaced => {
                format!("module `{}`, imported under its namespace", self.module)
            }
        };

        format!("{} {}, {}", self.package, self.version, how)
    }
}

/// A dependency as the package being compiled sees it: the resolved package, whether
/// this package sees it wrapped, and the modules it offers.
///
/// `modules` holds the dependency's **public** modules only, by their names within it —
/// [what a package exposes](../../../docs/spec/packages.md#what-a-package-exposes) is
/// every module of `src/` except the ones `private-modules` names and every `module
/// foreign` facade. A name that would have reached one of those is absent from the map
/// this builds, so it fails as a module that does not exist rather than as one that is
/// refused.
pub struct DependencyModules<'a> {
    pub package: &'a ResolvedPackage,
    pub wrapped: bool,
    pub modules: Vec<Name>,
}

/// One module the package being compiled declares: the name its header gives it, and
/// the file it was written in as [`SourceFile::package_path`](super::source::SourceFile::package_path)
/// renders it — `src/Model.zel`.
///
/// The file travels with the name because the package's two source roots share one set
/// of names: `src/Model.zel` and `tests/Model.zel` are both `Model`, and that is the
/// same error as declaring one name twice under one root. Saying which files claimed
/// the name is the whole of what makes that error actionable.
#[derive(Debug, Clone)]
pub struct LocalModule {
    pub name: Name,
    pub file: String,
}

/// Every way resolving a build, or naming the modules in one, can fail.
///
/// Like a [`ManifestError`](super::manifest::ManifestError), none of these has a span:
/// what each is about is a `zelkova.toml`, which is not a file the
/// [`Files`](super::source::files::Files) database holds, so every variant names the
/// manifest it was found in inside its own message.
#[derive(Debug)]
pub enum Error {
    /// A dependency's own manifest is missing, malformed, or fails one of its rules.
    ///
    /// The root package's manifest never reaches here — `compile_package` reads it
    /// before resolution starts and reports it as
    /// [`CompilationError::Manifest`](super::CompilationError::Manifest).
    Manifest {
        /// The dependency entry that led here, so the error names a package the reader
        /// wrote rather than only a path they may never have seen.
        package: PackageName,
        error: ManifestError,
    },
    /// A dependency entry names a source this compiler cannot obtain.
    UnsupportedSource {
        manifest_path: PathBuf,
        package: PackageName,
    },
    /// A `path` source names a directory that could not be reached.
    SourceUnreachable {
        manifest_path: PathBuf,
        package: PackageName,
        path: PathBuf,
        message: String,
    },
    /// The package found at a source declares a name other than the one the entry asked
    /// for. A dependent derives a namespace from the key it wrote, so a package
    /// answering to another name would have that namespace put on modules it has never
    /// heard of.
    NameMismatch {
        manifest_path: PathBuf,
        expected: PackageName,
        declared: PackageName,
        path: PathBuf,
    },
    /// One package name given two different directories in one build. A name is a
    /// package's identity for the whole build, not a label local to the manifest that
    /// writes it.
    ConflictingSources {
        package: PackageName,
        first: PathBuf,
        second: PathBuf,
    },
    /// A package depends on itself, directly or through a chain. The members of a cycle
    /// have no order they could be compiled in.
    Cycle(Vec<PackageName>),
    /// A package was not compiled because one of its dependencies was not.
    ///
    /// This never stands alone: whatever went wrong in the dependency is already
    /// reported, and this says which package was left uncompiled as a result — so a
    /// user reading a dependency's diagnostics knows why nothing was said about the
    /// package they pointed the compiler at.
    DependencyNotCompiled {
        package: PackageName,
        dependency: PackageName,
    },
    /// Two modules answer to one name in one package.
    ///
    /// Reported when the build is resolved, before any module of that package is
    /// compiled, naming both modules and the packages they come from — [*Two modules
    /// under one
    /// name*](../../../docs/spec/packages.md#two-modules-under-one-name-is-an-error).
    ModuleNameCollision {
        package: PackageName,
        name: Name,
        first: ModuleOrigin,
        second: ModuleOrigin,
    },
}

impl PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::Manifest { package, error } => {
                format!("`{}` could not be read: {}", package, error.message())
            }
            Error::UnsupportedSource {
                manifest_path,
                package,
            } => format!(
                "`{}` in `{}` names a `git` source, which this compiler cannot obtain",
                package,
                manifest_path.display()
            ),
            Error::SourceUnreachable {
                manifest_path,
                package,
                path,
                message,
            } => format!(
                "`{}` in `{}` names the directory `{}`, which could not be read: {}",
                package,
                manifest_path.display(),
                path.display(),
                message
            ),
            Error::NameMismatch {
                manifest_path,
                expected,
                declared,
                path,
            } => format!(
                "the dependency `{}` in `{}` resolves to `{}`, which declares the name `{}`",
                expected,
                manifest_path.display(),
                path.display(),
                declared
            ),
            Error::ConflictingSources { package, .. } => {
                format!("`{}` is given two different sources in this build", package)
            }
            Error::Cycle(packages) => format!(
                "{} package{} depend on each other in a circle",
                packages.len(),
                if packages.len() == 1 { "" } else { "s" }
            ),
            Error::ModuleNameCollision { package, name, .. } => {
                format!("two modules are named `{}` in package `{}`", name, package)
            }
            Error::DependencyNotCompiled {
                package,
                dependency,
            } => format!(
                "`{}` was not compiled, because its dependency `{}` was not",
                package, dependency
            ),
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Error::Manifest { error, .. } => error.notes(),
            Error::UnsupportedSource { .. } => vec![
                "a `git` dependency is fetched by the toolchain, which is not written yet; \
                 a `path` dependency is read where it lies and needs no fetching"
                    .to_string(),
            ],
            Error::ConflictingSources { first, second, .. } => vec![
                format!("one entry resolves to `{}`", first.display()),
                format!("another resolves to `{}`", second.display()),
            ],
            Error::Cycle(packages) => {
                let mut path: Vec<String> = packages.iter().map(|p| p.to_string()).collect();
                if let Some(first) = path.first().cloned() {
                    path.push(first);
                }
                vec![format!("cycle: {}", path.join(" -> "))]
            }
            Error::ModuleNameCollision { first, second, .. } => {
                vec![
                    first.describe(),
                    second.describe(),
                    "wrap one of the two, or rename this package's own module".to_string(),
                ]
            }
            _ => Vec::new(),
        }
    }
}

/// Walk the build's manifests, starting from the root package, and hand back every
/// package in it — each one after the packages it depends on, so they can be compiled
/// in that order.
///
/// `root` is the directory holding `manifest`; `manifest` is already read and validated
/// by the caller, which is why the root package's own manifest errors are not among
/// what this can report.
///
/// The root package's `test-dependencies` are part of the build, and no other
/// package's are: a package listed there is available to that package's `tests/` and is
/// [not resolved by anyone depending on
/// it](../../../docs/spec/packages.md#test-dependencies). The rest of the rules apply to
/// the union of the two maps, so one version of each package and an acyclic graph are
/// settled once for the whole build rather than again when the tests are run.
///
/// Every failure is collected rather than returned at the first one: a build whose
/// manifests name three missing directories says so once.
pub fn resolve(root: &Path, manifest: Manifest) -> Result<Vec<ResolvedPackage>, Vec<Error>> {
    let mut resolver = Resolver {
        order: Vec::new(),
        resolved: HashMap::new(),
        errors: Vec::new(),
    };

    let root_package = ResolvedPackage {
        name: manifest.name.clone(),
        root: canonical_dir(root),
        manifest,
    };

    resolver.visit(root_package, true, &mut Vec::new());

    if resolver.errors.is_empty() {
        Ok(resolver.order)
    } else {
        Err(resolver.errors)
    }
}

/// The directory as it will be compared against every other directory in the build.
///
/// Two entries reaching one package by different relative paths are the same package,
/// and `..` segments are what makes that common: `../acme-parser` from two siblings is
/// one directory. Canonicalizing is what makes them compare equal — and a path that
/// cannot be canonicalized (it does not exist yet) is kept as written, so the failure
/// is reported where the directory is actually read rather than here.
fn canonical_dir(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

struct Resolver {
    /// Every package resolved so far, in the order they can be compiled: a package is
    /// pushed only once every package it depends on already is.
    order: Vec<ResolvedPackage>,
    /// The directory each resolved package name was found in, which is what a second
    /// entry for that name is checked against.
    resolved: HashMap<PackageName, PathBuf>,
    errors: Vec<Error>,
}

impl Resolver {
    /// Resolve `package` and everything it depends on, appending each to `order`.
    ///
    /// `stack` is the chain of packages currently being visited, each with the
    /// directory it was found in. Meeting a name already on it is what stops the
    /// recursion — either because the chain closes on the same package, which is a
    /// cycle, or because it closes on a *different* package of the same name, which is
    /// one name given two sources. Either way the walk stops here rather than following
    /// the chain round forever.
    ///
    /// `with_test_dependencies` is true for the root package alone, because only that
    /// package's tests are ever compiled. A dependency's `test-dependencies` are none of
    /// this build's business and are not followed.
    fn visit(
        &mut self,
        package: ResolvedPackage,
        with_test_dependencies: bool,
        stack: &mut Vec<(PackageName, PathBuf)>,
    ) {
        if let Some(at) = stack.iter().position(|(name, _)| name == &package.name) {
            if stack[at].1 == package.root {
                self.errors.push(Error::Cycle(
                    stack[at..].iter().map(|(name, _)| name.clone()).collect(),
                ));
            } else {
                self.errors.push(Error::ConflictingSources {
                    package: package.name.clone(),
                    first: stack[at].1.clone(),
                    second: package.root.clone(),
                });
            }
            return;
        }

        if let Some(previous) = self.resolved.get(&package.name) {
            if previous != &package.root {
                self.errors.push(Error::ConflictingSources {
                    package: package.name.clone(),
                    first: previous.clone(),
                    second: package.root.clone(),
                });
            }
            return;
        }

        stack.push((package.name.clone(), package.root.clone()));

        // Sorted, so that a build with two broken entries reports them in the same
        // order every run — a `HashMap`'s iteration order is not one a user should see.
        let mut entries: Vec<(&PackageName, &super::manifest::Dependency)> =
            package.manifest.dependencies.iter().collect();
        if with_test_dependencies {
            entries.extend(package.manifest.test_dependencies.iter());
        }
        entries.sort_by(|left, right| left.0.as_str().cmp(right.0.as_str()));

        for (name, dependency) in entries {
            if let Some(dependency) = self.obtain(&package, name, &dependency.source) {
                self.visit(dependency, false, stack);
            }
        }

        stack.pop();

        self.resolved
            .insert(package.name.clone(), package.root.clone());
        self.order.push(package);
    }

    /// Read the package one dependency entry names, or report why it could not be.
    ///
    /// `None` means the entry produced an error, which is already recorded: resolution
    /// carries on with the rest of the build rather than stopping, so one unreachable
    /// directory does not hide the next.
    fn obtain(
        &mut self,
        dependent: &ResolvedPackage,
        name: &PackageName,
        source: &Source,
    ) -> Option<ResolvedPackage> {
        let manifest_path = dependent.manifest_path();

        let path = match source {
            // A `path` is relative to the manifest that names it
            // (`docs/spec/packages.md#where-a-dependency-comes-from`).
            Source::Path { path } => dependent.root.join(path),
            Source::Git { .. } => {
                self.errors.push(Error::UnsupportedSource {
                    manifest_path,
                    package: name.clone(),
                });
                return None;
            }
        };

        let path = match std::fs::canonicalize(&path) {
            Ok(path) => path,
            Err(err) => {
                self.errors.push(Error::SourceUnreachable {
                    manifest_path,
                    package: name.clone(),
                    path,
                    message: err.to_string(),
                });
                return None;
            }
        };

        let manifest = match super::manifest::load(&path) {
            Ok(manifest) => manifest,
            Err(errors) => {
                self.errors
                    .extend(errors.into_iter().map(|error| Error::Manifest {
                        package: name.clone(),
                        error,
                    }));
                return None;
            }
        };

        // The key is the package's identity for the whole build, so a package answering
        // to another name is refused rather than being taken under the name that was
        // asked for — the namespace a dependent derives from the key would otherwise
        // name modules this package has never heard of.
        if &manifest.name != name {
            self.errors.push(Error::NameMismatch {
                manifest_path,
                expected: name.clone(),
                declared: manifest.name.clone(),
                path,
            });
            return None;
        }

        Some(ResolvedPackage {
            name: manifest.name.clone(),
            root: path,
            manifest,
        })
    }
}

/// Every module name the package being compiled can import, and what each one names.
///
/// The map holds one entry per *spelling*: the package's own modules under their own
/// names, a wrapped dependency's under `<Namespace>.<module>`, and an unwrapped
/// dependency's under their own names. That is the whole of what an `import` can
/// reach — [*Imports across a package
/// boundary*](../../../docs/spec/packages.md#imports-across-a-package-boundary) — and
/// a module has exactly one spelling in it, never two.
///
/// A name claimed twice is an error and no map comes back, because there is no
/// coherent answer to give the modules about to be compiled. Every collision is
/// reported, not just the first.
pub fn visible_modules(
    package: &ResolvedPackage,
    local_modules: &[LocalModule],
    dependencies: &[DependencyModules<'_>],
) -> Result<HashMap<Name, ModuleOrigin>, Vec<Error>> {
    let mut names: HashMap<Name, ModuleOrigin> = HashMap::new();
    let mut errors = Vec::new();

    let mut claim =
        |name: Name, origin: ModuleOrigin, errors: &mut Vec<Error>| match names.get(&name) {
            Some(first) => errors.push(Error::ModuleNameCollision {
                package: package.name.clone(),
                name,
                first: first.clone(),
                second: origin,
            }),
            None => {
                names.insert(name, origin);
            }
        };

    // The package's own modules first, so that a collision between one of them and a
    // dependency's reads with the local module as the name already claimed. The sort is
    // stable, so two modules answering to one name keep the order they were given —
    // `src/` before `tests/`, which is the order a collision between the two roots reads
    // best in.
    let mut local: Vec<&LocalModule> = local_modules.iter().collect();
    local.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
    for module in local {
        claim(
            module.name.clone(),
            ModuleOrigin {
                package: package.name.clone(),
                version: package.manifest.version.clone(),
                module: module.name.clone(),
                kind: OriginKind::Local {
                    file: module.file.clone(),
                },
            },
            &mut errors,
        );
    }

    let mut dependencies: Vec<&DependencyModules<'_>> = dependencies.iter().collect();
    dependencies.sort_by(|left, right| left.package.name.as_str().cmp(right.package.name.as_str()));

    for dependency in dependencies {
        let unwrapped = dependency.package.seen_unwrapped(dependency.wrapped);
        let namespace = dependency.package.name.namespace();

        let mut modules: Vec<&Name> = dependency.modules.iter().collect();
        modules.sort_by(|left, right| left.as_str().cmp(right.as_str()));

        for module in modules {
            let (spelling, kind) = if unwrapped {
                (module.clone(), OriginKind::Unwrapped)
            } else {
                (
                    module
                        .qualify_with_name(&namespace)
                        .map(|qualified| qualified.to_name())
                        .unwrap_or_else(|| module.clone()),
                    OriginKind::Namespaced,
                )
            };

            claim(
                spelling,
                ModuleOrigin {
                    package: dependency.package.name.clone(),
                    version: dependency.package.manifest.version.clone(),
                    module: module.clone(),
                    kind,
                },
                &mut errors,
            );
        }
    }

    if errors.is_empty() {
        Ok(names)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::manifest::Dependency;

    fn version(major: u64) -> Version {
        Version {
            major,
            minor: 0,
            patch: 0,
        }
    }

    fn package(name: &str, dependencies: Vec<(&str, bool)>) -> ResolvedPackage {
        let dependencies = dependencies
            .into_iter()
            .map(|(name, wrapped)| {
                (
                    PackageName::new(name).unwrap(),
                    Dependency {
                        source: Source::Path {
                            path: format!("../{}", name),
                        },
                        wrapped,
                    },
                )
            })
            .collect();

        ResolvedPackage {
            name: PackageName::new(name).unwrap(),
            root: PathBuf::from("/tmp").join(name),
            manifest: Manifest {
                name: PackageName::new(name).unwrap(),
                version: version(1),
                main: None,
                private_modules: vec![],
                dependencies,
                test_dependencies: HashMap::new(),
            },
        }
    }

    /// The package's own modules, as `visible_modules` takes them: a name and the file
    /// it was declared in. The file is only ever read back out of a collision note.
    fn local(modules: &[&str]) -> Vec<LocalModule> {
        modules
            .iter()
            .map(|name| LocalModule {
                name: Name::new(*name),
                file: format!("src/{}.zel", name),
            })
            .collect()
    }

    fn dependency<'a>(
        package: &'a ResolvedPackage,
        wrapped: bool,
        modules: &[&str],
    ) -> DependencyModules<'a> {
        DependencyModules {
            package,
            wrapped,
            modules: modules.iter().map(|m| Name::new(*m)).collect(),
        }
    }

    /// A wrapped dependency's modules are reachable only under its namespace, and an
    /// unwrapped one's only under their own names. Both halves matter: a module has
    /// exactly one spelling in any file.
    ///
    /// Mutation-checked by ignoring `unwrapped` in `visible_modules` and always
    /// qualifying: the `Size` lookup for the unwrapped dependency goes red.
    #[test]
    fn a_dependency_is_named_through_its_namespace_unless_unwrapped() {
        let widgets = package("acme-widgets", vec![]);
        let todo = package("todo", vec![]);

        let wrapped = visible_modules(
            &todo,
            &local(&["Model"]),
            &[dependency(&widgets, true, &["Size", "Style.Dark"])],
        )
        .expect("no collision");

        assert!(wrapped.contains_key(&Name::new("AcmeWidgets.Size")));
        assert!(wrapped.contains_key(&Name::new("AcmeWidgets.Style.Dark")));
        assert!(!wrapped.contains_key(&Name::new("Size")));
        assert!(wrapped.contains_key(&Name::new("Model")));

        let unwrapped = visible_modules(
            &todo,
            &local(&["Model"]),
            &[dependency(&widgets, false, &["Size", "Style.Dark"])],
        )
        .expect("no collision");

        assert!(unwrapped.contains_key(&Name::new("Size")));
        assert!(unwrapped.contains_key(&Name::new("Style.Dark")));
        assert!(!unwrapped.contains_key(&Name::new("AcmeWidgets.Size")));
    }

    /// `zelkova-core` is seen unwrapped whatever the entry naming it says, which is
    /// what keeps `Basics` spelled `Basics`.
    ///
    /// Mutation-checked by dropping the `CORE_PACKAGE` arm of `seen_unwrapped`:
    /// `Basics` is then only reachable as `ZelkovaCore.Basics`.
    #[test]
    fn core_is_unwrapped_whatever_its_entry_says() {
        let core = package("zelkova-core", vec![]);
        let todo = package("todo", vec![]);

        let names = visible_modules(&todo, &[], &[dependency(&core, true, &["Basics"])])
            .expect("no collision");

        assert!(names.contains_key(&Name::new("Basics")));
        assert!(!names.contains_key(&Name::new("ZelkovaCore.Basics")));
    }

    /// Two unwrapped dependencies claiming one name is an error naming both packages,
    /// and so is an unwrapped dependency claiming a local module's name.
    ///
    /// Mutation-checked by making `claim` overwrite instead of reporting: both
    /// `expect_err` calls then come back `Ok`.
    #[test]
    fn a_name_claimed_twice_names_both_packages() {
        let widgets = package("acme-widgets", vec![]);
        let ui = package("fmonniot-ui", vec![]);
        let todo = package("todo", vec![]);

        let errors = visible_modules(
            &todo,
            &[],
            &[
                dependency(&widgets, false, &["Size"]),
                dependency(&ui, false, &["Size"]),
            ],
        )
        .expect_err("both dependencies claim `Size`");

        assert_eq!(errors.len(), 1, "got {:?}", errors);
        let notes = errors[0].notes();
        assert!(
            errors[0].message().contains("`Size`") && errors[0].message().contains("`todo`"),
            "got {:?}",
            errors[0].message()
        );
        assert!(
            notes.iter().any(|n| n.contains("acme-widgets"))
                && notes.iter().any(|n| n.contains("fmonniot-ui")),
            "both packages must be named, got {:?}",
            notes
        );

        let errors = visible_modules(
            &todo,
            &local(&["Size"]),
            &[dependency(&widgets, false, &["Size"])],
        )
        .expect_err("the local module and the dependency both claim `Size`");

        assert_eq!(errors.len(), 1, "got {:?}", errors);
        let notes = errors[0].notes();
        assert!(
            notes.iter().any(|n| n.contains("a module of this package")),
            "the local module must be named as one, got {:?}",
            notes
        );
    }

    /// Two *wrapped* dependencies cannot collide, whatever they contain — that is what
    /// the namespace is for.
    #[test]
    fn two_wrapped_dependencies_never_collide() {
        let widgets = package("acme-widgets", vec![]);
        let ui = package("fmonniot-ui", vec![]);
        let todo = package("todo", vec![]);

        let names = visible_modules(
            &todo,
            &local(&["Size"]),
            &[
                dependency(&widgets, true, &["Size"]),
                dependency(&ui, true, &["Size"]),
            ],
        )
        .expect("wrapped dependencies keep out of each other's way");

        assert!(names.contains_key(&Name::new("Size")));
        assert!(names.contains_key(&Name::new("AcmeWidgets.Size")));
        assert!(names.contains_key(&Name::new("FmonniotUi.Size")));
    }
}
