//! Building a package: the half of `zelkova compile`, `zelkova test` and `zelkova run`
//! that comes after the check.
//!
//! [`zelkova_compiler::check_package`] and [`zelkova_compiler::check_package_with_tests`] check a package
//! and every package it depends on, and print, write and render nothing. This module
//! takes what they hand back and does the rest: it prints each status line to stderr,
//! emits a build that checked and writes it, and renders every error to stderr.
//!
//! # Emitting and writing
//!
//! Once every module of every package has checked, the specialisations of the whole build
//! are found (`zelkova_compiler::ir::specialise`, over the modules of both trees at once), and
//! each module is emitted as JavaScript
//! (`zelkova_js::emit`) and, if that failed nowhere either, the build is written to
//! `build/out/js/` (`output::write`). A build with any error writes nothing. A build that
//! also compiled the tests writes a second, complete tree at `build/test/js/` — the
//! runtime, then one directory per package, holding every package of the build (a
//! test-only one included) and the root's `tests/` modules beside its `src/` ones — so
//! that tree alone is what a test runner ever reads from.
//!
//! Which file is a facade's companion is this module's to find, not the check's: a
//! checked module comes back as a [`CheckedSource`] naming the source root it was read
//! under, and [`zelkova_js::module_file`] names the companion below it ([*A facade names
//! a boundary, not a
//! backend*](../docs/spec/interop.md#a-facade-names-a-boundary-not-a-backend)).
//!
//! Every error is rendered through `as_diagnostic`, and the status lines are printed
//! before any of them.

use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};

use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::termcolor::{Color, ColorChoice, ColorSpec, StandardStream};
use codespan_reporting::term::{self};
use log::debug;

use codespan_reporting::diagnostic::Diagnostic;
use zelkova_compiler::ir::SpecialiseError;
use zelkova_compiler::name::Name;
use zelkova_compiler::source::files::SourceFileId;
use zelkova_compiler::source::Overlay;

use zelkova_compiler::{
    phase_diagnostic, plain_diagnostic, CheckedModule, CheckedSource, CompilationError, Interface,
    ModuleName, PackageCheck, PhaseError, Status,
};
use zelkova_js::output;

// Public because `BuildError::ProgramRun` carries its `Error`, and `zelkova run` calls `run`.
pub mod program_runner;

/// Which of the root package's source roots a build compiles: [`zelkova_compiler::check_package`]'s
/// `src/` alone, or [`zelkova_compiler::check_package_with_tests`]'s `src/` and `tests/`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TestRoot {
    /// `src/` alone.
    Skipped,
    /// Both roots.
    Compiled,
}

/// Every way building a package can fail: its check, or emitting, writing or running what
/// checked.
///
/// The check's own errors are [`CompilationError`]s, each wrapped in
/// [`Check`](BuildError::Check); the other variants are what only a build can raise.
#[derive(Debug)]
pub enum BuildError {
    /// An error of the check. On its own — returned rather than accumulated — it is one
    /// raised before there was a build to check, the root's manifest or the resolution of
    /// its dependencies, and is unrendered.
    Check(CompilationError),
    /// The named module checked, and the specialisations the build needs could not be found
    /// for it: a constrained function whose specialisations never end
    /// ([`SpecialiseError::Unbounded`]), or one of the errors that can only mean the type
    /// checker accepted what it should not have. Raised once every module of the build has
    /// checked, and the build writes nothing.
    Specialise(Vec<SpecialiseError>, Name),
    /// The named module checked and could not be emitted as JavaScript.
    Emit(Vec<zelkova_js::Error>, Name),
    /// A file of the build's output could not be written. Raised only once every module
    /// of the build has checked and emitted, since nothing is written before that.
    Output(output::Error),
    /// A package's tests were compiled and could not be run: `zelkova test` could not
    /// write its entry point or could not run `node`. A test that ran and did not pass is
    /// not this; it is the exit code of the run.
    TestRun(zelkova_test_runner::Error),
    /// A package's program could not be run: `zelkova run` was pointed at a package with no
    /// `main`, or could not write its entry point or could not run `node`. A program that
    /// ran and aborted is not this; it is the exit code of the run.
    ProgramRun(program_runner::Error),
    /// An [`Emit`](BuildError::Emit) or a [`Specialise`](BuildError::Specialise) together
    /// with the file its module was read from, so that its labels have a file to point into.
    /// It wraps nothing else: an error of the check carries its own file inside
    /// [`Check`](BuildError::Check), as a [`CompilationError::InFile`].
    InFile(Box<BuildError>, SourceFileId),
    /// Every error a build accumulated, each already rendered to stderr.
    ///
    /// Flat: each error of the check is one [`Check`](BuildError::Check) member, and a
    /// [`CompilationError::Many`] is never wrapped — its members are. A build does not
    /// stop on the first failure, so that one broken module cannot hide the diagnostics
    /// of the others; this is how that accumulation becomes a failure again at its end,
    /// with the typed errors still intact for the caller to inspect.
    Many(Vec<BuildError>),
}

impl BuildError {
    /// Turn this error into the diagnostic the user reads.
    ///
    /// A [`Check`](BuildError::Check) renders through [`CompilationError::as_diagnostic`];
    /// every other variant through [`phase_diagnostic`] or [`plain_diagnostic`], the two
    /// functions [`CompilationError::as_diagnostic`] shares with this one.
    pub fn as_diagnostic(&self) -> Diagnostic<SourceFileId> {
        self.as_diagnostic_in(None)
    }

    /// The name of the module this error belongs to, when it has one.
    pub fn module(&self) -> Option<&Name> {
        match self {
            BuildError::Emit(_, module) | BuildError::Specialise(_, module) => Some(module),
            BuildError::Check(error) => error.module(),
            BuildError::InFile(inner, _) => inner.module(),
            _ => None,
        }
    }

    /// `as_diagnostic`, carrying the file the error's module was read from, which only an
    /// [`InFile`](BuildError::InFile) wrapper supplies.
    fn as_diagnostic_in(&self, file: Option<SourceFileId>) -> Diagnostic<SourceFileId> {
        match self {
            BuildError::Check(error) => error.as_diagnostic(),
            BuildError::InFile(inner, id) => inner.as_diagnostic_in(Some(*id)),
            BuildError::Specialise(errors, module) => {
                phase_diagnostic(module, "specialisation", errors, file)
            }
            BuildError::Emit(errors, module) => {
                phase_diagnostic(module, "code generation", errors, file)
            }
            // A path on disk, not a place in any source, so there is nothing to label.
            BuildError::Output(error) => plain_diagnostic(error),
            // Like `Output`, about the machine and not about any source.
            BuildError::TestRun(error) => plain_diagnostic(error),
            BuildError::ProgramRun(error) => plain_diagnostic(error),
            // Each member was rendered with its own carets before this was built, so this
            // summarises rather than flattening their labels: drawing every caret of the
            // build a second time under one headline is what flattening would do.
            BuildError::Many(errors) => plain_diagnostic(&Summary { errors, file }),
        }
    }
}

/// What a [`BuildError::Many`] renders as: one headline counting its members, and each
/// member's own headline as a note.
struct Summary<'a> {
    errors: &'a [BuildError],
    file: Option<SourceFileId>,
}

impl PhaseError for Summary<'_> {
    fn message(&self) -> String {
        format!(
            "compilation failed with {} error{}",
            self.errors.len(),
            if self.errors.len() == 1 { "" } else { "s" }
        )
    }

    fn notes(&self) -> Vec<String> {
        self.errors
            .iter()
            .map(|e| e.as_diagnostic_in(self.file).message)
            .collect()
    }
}

/// What `?` goes through for an error raised before a build's accumulator exists — the
/// root's manifest, the resolution — which comes back bare and unrendered.
impl From<CompilationError> for BuildError {
    fn from(error: CompilationError) -> Self {
        BuildError::Check(error)
    }
}

/// Compile the package rooted at `package_dir` — a directory holding a `zelkova.toml`
/// manifest beside a `src/` source root, per
/// [`docs/spec/packages.md`](../docs/spec/packages.md#what-a-package-is) — and every
/// package it depends on.
///
/// Its `tests/` root is not compiled; [`compile_package_with_tests`] is that build. A
/// package is compiled from source the same way whether it is the one asked for or a
/// dependency of it, and the whole build shares one file database and one error
/// accumulator: an error in any package of it makes this return `Err`.
///
/// A build that checks writes its JavaScript to `build/out/js/` beside `package_dir`'s
/// manifest — see [`compile_package_into`] for what that tree holds. One that emitted
/// any error writes nothing.
///
/// The check itself is [`zelkova_compiler::check_package`]'s. This prints its status lines to stderr, emits
/// and writes the build, and renders every error to stderr before returning it inside
/// [`BuildError::Many`]. An error raised before any source is read — the manifest,
/// the resolution — is returned bare and unrendered.
pub fn compile_package(package_dir: &Path) -> Result<(), BuildError> {
    compile(
        package_dir,
        TestRoot::Skipped,
        &package_dir.join(BUILD_DIRECTORY),
        &Overlay::new(),
    )
    .map(|_| ())
}

/// The directory a build's output goes to, beside the root package's manifest and never
/// beside a source it read ([*The compiler's
/// interface*](../docs/spec/toolchain.md#the-compilers-interface)).
pub const BUILD_DIRECTORY: &str = "build";

/// The tree a build that compiled the tests writes below `build_dir`: `build_dir/test/js/`,
/// laid out like `build_dir/out/js/`. Both the write of that tree and [`test()`], which hands
/// it to [`zelkova_test_runner::run`], name it through here.
pub fn test_tree(build_dir: &Path) -> PathBuf {
    build_dir.join("test").join("js")
}

/// [`compile_package`], writing its output below `build_dir` rather than below the
/// package's own `build/`.
///
/// The output is one tree, `<build_dir>/out/js/`: the runtime at its root, then one
/// directory per package of the build holding one `.mjs` file per module of that
/// package, named after the module within its own package, and each facade's companion
/// beside the facade ([`zelkova_js`]'s *Paths* section has the names, [`DEC-18` decision
/// 5](../docs/decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest)
/// the reasons). Nothing is written until every module of every package has checked and
/// emitted.
pub fn compile_package_into(package_dir: &Path, build_dir: &Path) -> Result<(), BuildError> {
    compile(package_dir, TestRoot::Skipped, build_dir, &Overlay::new()).map(|_| ())
}

/// [`compile_package`], compiling the package's `tests/` root as well as its `src/`.
///
/// This is what running a package's own tests needs, and the only thing that ever reads
/// a `tests/` root: the tests of the build's other packages are not compiled, because
/// nothing outside a package reads its tests
/// ([*Running a package's tests*](../docs/spec/toolchain.md#running-a-packages-tests)).
/// A package holding no `tests/` at all compiles exactly as it does through
/// [`compile_package`].
///
/// It compiles the tests and does not run them: what makes a declaration a test is
/// [its type](../docs/spec/packages.md#what-a-test-is), and running one is
/// [`test()`]'s. What it hands back on success is the `Interface` of each of
/// the root's own `tests/` modules that checked — never a test-only package's, and never
/// `src/`'s — so a caller can find which of their exposed values are tests without a phase
/// dropping the checked modules once they are emitted. `test_collection::collect` is that
/// pass, and `zelkova_test_runner::run` is its caller. Empty when the package holds no `tests/` at
/// all.
///
/// A test module and every `test-dependency`'s modules are checked and, unlike a plain
/// build, written — to a tree of their own, `build/test/js/`, laid out exactly like
/// `build/out/js/` and holding every package of the build (a test-only one included) plus the
/// root's `tests/` modules beside its `src/` ones. `build/out/js/` itself is left exactly as
/// [`compile_package`] would have written it: a test module never turns up there, so a
/// plain build run afterwards never finds one left behind by a run that also compiled the
/// tests ([`GEN-18`](../docs/tickets/README.md)).
pub fn compile_package_with_tests(package_dir: &Path) -> Result<Vec<Interface>, BuildError> {
    compile_package_with_tests_into(package_dir, &package_dir.join(BUILD_DIRECTORY))
}

/// [`compile_package_with_tests`], writing below `build_dir` rather than below the
/// package's own `build/` — the same relationship [`compile_package_into`] has to
/// [`compile_package`].
pub fn compile_package_with_tests_into(
    package_dir: &Path,
    build_dir: &Path,
) -> Result<Vec<Interface>, BuildError> {
    compile(package_dir, TestRoot::Compiled, build_dir, &Overlay::new())
}

/// Compile the package rooted at `package_dir` with its tests
/// ([`compile_package_with_tests`]), run every test it holds under `node`
/// ([`zelkova_test_runner::run`], from the tree [`test_tree`] names), and answer the exit code the
/// process should end with.
///
/// The answer is `Ok(0)` when every test passed and when the package holds none. It is
/// `node`'s own exit code otherwise, which is non-zero exactly when a test did not pass or
/// `node` itself failed. Anything that stops the tests being run — a build that did not
/// compile, `node` missing — is an `Err`, and a build that did not compile never reaches
/// `node`.
pub fn test(package_dir: &Path) -> Result<i32, BuildError> {
    let interfaces = compile_package_with_tests(package_dir)?;
    zelkova_test_runner::run(&test_tree(&package_dir.join(BUILD_DIRECTORY)), &interfaces)
        .map_err(BuildError::TestRun)
}

/// Print one [`Status`] line to `writer`. Failing to write a status line is not itself a
/// compilation failure, so the write results are deliberately discarded rather than
/// unwrapped.
fn print_status(writer: &mut StandardStream, status: &Status) {
    let (color, label) = if status.success {
        (Color::Green, "success")
    } else {
        (Color::Red, "failure")
    };
    let _ = writer.set_color(ColorSpec::new().set_bold(true).set_fg(Some(color)));
    let _ = write!(writer, "{}", label);
    let _ = writer.reset();
    let _ = writeln!(writer, " {}", status.text);
}

/// Each error of the check as one [`BuildError::Check`], with a
/// [`CompilationError::Many`] replaced by its members, at any depth. This is what keeps
/// [`BuildError::Many`] flat by construction rather than by `check` never building a `Many`.
fn check_errors(errors: Vec<CompilationError>) -> Vec<BuildError> {
    errors
        .into_iter()
        .flat_map(|error| match error {
            CompilationError::Many(members) => check_errors(members),
            other => vec![BuildError::Check(other)],
        })
        .collect()
}

/// The CLI half of every `compile_package` variant: check the package
/// ([`zelkova_compiler::check_package`] or [`zelkova_compiler::check_package_with_tests`], as `tests`
/// says), print its status lines, emit and write a build that checked, and render every error to stderr.
///
/// The status lines are printed once the check has finished rather than as each phase
/// ends. For a check that returns, the bytes `compile_package` itself writes to stderr are
/// the same as before, in the same order: every status line, then every diagnostic. Log
/// output (`RUST_LOG`) and a panic's message no longer interleave with them, and a panic
/// during checking loses the status lines already recorded.
fn compile(
    package_dir: &Path,
    tests: TestRoot,
    build_dir: &Path,
    overlay: &Overlay,
) -> Result<Vec<Interface>, BuildError> {
    let PackageCheck {
        package: root_package,
        errors,
        sources,
        modules,
        test_dependency_modules,
        test_modules,
        // A build emits only what checked; these are for a reader of a module's tree.
        failing: _,
        status,
    } = match tests {
        TestRoot::Skipped => zelkova_compiler::check_package(package_dir, overlay)?,
        TestRoot::Compiled => zelkova_compiler::check_package_with_tests(package_dir, overlay)?,
    };

    // The build's one accumulator: each error of the check on its own, then whatever
    // emitting and writing the build adds. An empty one is what makes this return `Ok`.
    let mut errors: Vec<BuildError> = check_errors(errors);

    // Error reporter
    let mut writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config {
        tab_width: 2,
        ..codespan_reporting::term::Config::default()
    };

    for line in &status {
        print_status(&mut writer, line);
    }

    // What `compile_package_with_tests` hands back: the `Interface` of each of the
    // root's own `tests/` modules that checked — never a test-only package's, and
    // never `src/`'s. This is what lets a caller find a package's tests
    // (`test_collection::collect` is that pass) without a phase dropping the checked
    // modules on the floor once they have been emitted. Stays empty for a build that
    // did not ask for the tests, or whose root `tests/` did not check.
    let mut root_test_interfaces: Vec<Interface> = Vec::new();

    // Generate code, only for a build in which nothing failed. Every module of
    // both trees is emitted before anything is written, so a module that cannot be
    // emitted — in `src/` or in `tests/` — also leaves the build with no output at all.
    if errors.is_empty() {
        let mut checked: Vec<ModuleToEmit> = modules.into_iter().map(to_module_to_emit).collect();
        let mut root_tests_checked: Vec<ModuleToEmit> =
            test_modules.into_iter().map(to_module_to_emit).collect();

        // A test companion imports the companion it checks by its path in the source
        // tree, across the two roots, which reaches nothing in the build
        // ([*Testing a companion*](../docs/spec/interop.md#testing-a-companion)). Each
        // companion of the root's `src/` is one it may check, and `checked` holds every
        // one of them.
        let targets: Vec<&Name> = checked
            .iter()
            .filter(|to_emit| {
                to_emit.companion.is_some()
                    && to_emit.module.canonical.name.package() == &root_package
            })
            .map(|to_emit| to_emit.module.canonical.name.name())
            .collect();
        for to_emit in root_tests_checked
            .iter_mut()
            .filter(|to_emit| to_emit.companion.is_some())
        {
            let checks = to_emit.module.canonical.name.name();
            to_emit.companion_imports = targets
                .iter()
                .map(|target| zelkova_js::test_companion_import(checks, target))
                .collect();
        }
        // Built from the same `ModuleToEmit`s `test_tree_modules` is about to take,
        // before that move: an `Interface` is cheap to clone off a `CheckedModule`
        // that is otherwise about to be consumed by emission.
        root_test_interfaces = root_tests_checked
            .iter()
            .map(|to_emit| to_emit.module.to_interface(to_emit.file))
            .collect();

        // Every module the *test* tree (`build/test/js/`) needs beyond what `checked`
        // already holds: each test-only package's modules, and the root's `tests/` modules.
        // Empty for a build that did not ask for the tests.
        let mut test_tree_modules: Vec<ModuleToEmit> = test_dependency_modules
            .into_iter()
            .map(to_module_to_emit)
            .collect();
        test_tree_modules.extend(root_tests_checked);

        debug!("phase: specialise");
        let specialised = specialise_build(&mut checked, &mut test_tree_modules, &mut errors);

        debug!("phase: codegen");
        // Every union of the build, read by a facade's boundary checks: a facade may name
        // a union any module of the build declares, and a test-only package's or the
        // root's `tests/` modules are part of the build their facades see.
        let unions = zelkova_js::Unions::of(
            checked
                .iter()
                .chain(test_tree_modules.iter())
                .map(|to_emit| &to_emit.module),
        );
        // A build whose specialisations could not be found has references that still ask
        // for an instance, and emitting it would only add an error for each.
        let files = if specialised {
            emit_build(checked, &unions, &mut errors)
        } else {
            Vec::new()
        };

        // A build that also compiled the tests writes a second, complete tree at
        // `<build_dir>/test/js/`, laid out exactly like `<build_dir>/out/js/` — the runtime,
        // then one directory per package — but holding every package of the build (a
        // test-only one included) and the root's `tests/` modules beside its `src/` ones
        // (`docs/decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest`).
        // `files` already holds everything a plain build would have written — the
        // runtime, the root's `src/` and every plain dependency's modules — so the test
        // tree reuses it rather than emitting those modules a second time, and only
        // `test_tree_modules` (the test-only packages' and the root's `tests/`) is new
        // work. That work happens here, before `out/js/` is written, so a module of the test
        // tree that cannot be emitted blocks both writes.
        let test_files = (tests == TestRoot::Compiled).then(|| {
            debug!("phase: codegen (tests)");
            let mut test_files = files.clone();
            if specialised {
                test_files.extend(emit_modules(test_tree_modules, &unions, &mut errors));
            }
            test_files
        });

        if errors.is_empty() {
            debug!("phase: write the build");
            errors.extend(
                output::write(&build_dir.join("out").join("js"), &files)
                    .into_iter()
                    .map(BuildError::Output),
            );
        }

        // Gated on `errors.is_empty()` a second time so a plain build's own failure to
        // write `out/js/` — an unlikely I/O error, not a checking one — does not also
        // attempt the test tree.
        if let Some(test_files) = test_files.filter(|_| errors.is_empty()) {
            debug!("phase: write the test build");
            errors.extend(
                output::write(&test_tree(build_dir), &test_files)
                    .into_iter()
                    .map(BuildError::Output),
            );
        }
    }

    // Report everything we accumulated, then let that accumulation decide the
    // return value. Rendering the errors and returning `Ok` regardless was `BUG-1`.
    for error in &errors {
        // A rendering failure must not mask the compilation failure we are about to
        // return, and there is nowhere left to report it to, so it is dropped.
        let _ = term::emit_to_write_style(
            &mut writer.lock(),
            &config,
            &sources,
            &error.as_diagnostic(),
        );
    }

    if errors.is_empty() {
        Ok(root_test_interfaces)
    } else {
        Err(BuildError::Many(errors))
    }
}

/// Find the specialisations of the whole build ([`zelkova_compiler::ir::specialise`]) and
/// resolve every reference that asks for an instance, in the modules of both trees at once:
/// `checked`, which the plain tree is, and `test_tree_modules`, which the test tree holds as
/// well. A module's specialisations are made for it alone, so what the plain tree's modules
/// hold does not depend on the test tree having been read with them; the pass is run once
/// because both trees are emitted from these modules.
///
/// An error is pushed onto `errors`, tagged with the file the module it is written in was read
/// from, and `false` answers that the modules are not specialised.
fn specialise_build(
    checked: &mut [ModuleToEmit],
    test_tree_modules: &mut [ModuleToEmit],
    errors: &mut Vec<BuildError>,
) -> bool {
    let files: HashMap<ModuleName, Option<SourceFileId>> = checked
        .iter()
        .chain(test_tree_modules.iter())
        .map(|to_emit| (to_emit.module.canonical.name.clone(), to_emit.file))
        .collect();

    let mut modules: Vec<&mut CheckedModule> = checked
        .iter_mut()
        .chain(test_tree_modules.iter_mut())
        .map(|to_emit| &mut to_emit.module)
        .collect();

    match zelkova_compiler::ir::specialise(&mut modules) {
        Ok(()) => true,
        Err(failures) => {
            for failure in failures {
                let file = files.get(&failure.module).copied().flatten();
                let error = BuildError::Specialise(failure.errors, failure.module.name().clone());
                errors.push(match file {
                    Some(id) => BuildError::InFile(Box::new(error), id),
                    None => error,
                });
            }
            false
        }
    }
}

/// A module that checked, with what emitting and writing it needs beyond the module
/// itself.
struct ModuleToEmit {
    module: CheckedModule,
    /// The file it was read from, which an emission error's labels point into.
    file: Option<SourceFileId>,
    /// For a facade, its JavaScript companion when one sits beside its `.zel` source —
    /// a file of the same base name in the same directory
    /// ([*A facade names a boundary, not a
    /// backend*](../docs/spec/interop.md#a-facade-names-a-boundary-not-a-backend)).
    /// `None` for every other module, and for a facade with no companion, which
    /// [`zelkova_js::emit`] refuses.
    companion: Option<std::path::PathBuf>,
    /// The imports its companion spells for the source tree, each beside the specifier
    /// that replaces it in the build ([`zelkova_js::test_companion_import`]). Empty for
    /// every module but a facade of the root package's `tests/` with a companion, which
    /// [`compile`] fills in once it knows the companions of `src/` that one may import.
    companion_imports: Vec<(String, String)>,
}

/// What emitting and writing one checked module needs: its companion when it is a facade
/// with one sitting beside its source under its `root_dir`, so a facade under `tests/`
/// finds its companion there rather than under `src/`
/// ([*Testing a companion*](../docs/spec/interop.md#testing-a-companion)).
fn to_module_to_emit(checked: CheckedSource) -> ModuleToEmit {
    let CheckedSource {
        module,
        file,
        root_dir,
    } = checked;
    let companion = Some(root_dir.join(zelkova_js::module_file(module.canonical.name.name())))
        .filter(|path| module.ir.foreign && path.is_file());
    ModuleToEmit {
        file,
        companion,
        companion_imports: Vec::new(),
        module,
    }
}

/// Every file a build writes: the runtime, and each module of `checked` as the text
/// [`zelkova_js::emit`] gives it, with its facade's companion beside it.
///
/// A module that cannot be emitted pushes its errors onto `errors`, tagged with the
/// file it came from, and every other module is still emitted so that one refusal
/// cannot hide the next. The caller writes the files only when `errors` stays empty.
fn emit_build(
    checked: Vec<ModuleToEmit>,
    unions: &zelkova_js::Unions,
    errors: &mut Vec<BuildError>,
) -> Vec<output::File> {
    let mut files = vec![output::File {
        path: zelkova_js::RUNTIME_FILE.into(),
        contents: output::Contents::Text(zelkova_js::RUNTIME.to_string()),
    }];
    files.extend(emit_modules(checked, unions, errors));
    files
}

/// [`emit_build`], without the runtime file at the front.
///
/// [`emit_build`] calls it for the plain build's own modules. A test build calls it a
/// second time for the modules a plain build never emits — each test-only package's,
/// and the root's `tests/` — which is why it is factored out: the test tree already has
/// the runtime, since it starts from a clone of the plain build's own files, and those
/// extra modules must not emit a second runtime file to sit unused beside the first.
fn emit_modules(
    checked: Vec<ModuleToEmit>,
    unions: &zelkova_js::Unions,
    errors: &mut Vec<BuildError>,
) -> Vec<output::File> {
    let mut files = Vec::new();

    for ModuleToEmit {
        module,
        file,
        companion,
        companion_imports,
    } in checked
    {
        let name = module.canonical.name.clone();
        let package_dir = std::path::PathBuf::from(name.package().as_str());

        match zelkova_js::emit(&module, companion.is_some(), unions) {
            Ok(text) => {
                files.push(output::File {
                    path: package_dir.join(zelkova_js::module_file(name.name())),
                    contents: output::Contents::Text(text),
                });

                if let Some(companion) = companion {
                    let contents = if companion_imports.is_empty() {
                        output::Contents::Copy(companion)
                    } else {
                        output::Contents::Rewritten {
                            from: companion,
                            imports: companion_imports,
                        }
                    };
                    files.push(output::File {
                        path: package_dir.join(zelkova_js::companion_file(name.name())),
                        contents,
                    });
                }
            }
            Err(emit_errors) => {
                let error = BuildError::Emit(emit_errors, name.name().clone());
                errors.push(match file {
                    Some(id) => BuildError::InFile(Box::new(error), id),
                    None => error,
                });
            }
        }
    }

    files
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A `CompilationError::Many` in the check's errors becomes one `Check` per member,
    /// however deeply it was nested, and a `Many` is never itself wrapped. A plain error
    /// beside it stays as it was, in order.
    ///
    /// Mutation-checked by turning `check_errors` back into a plain
    /// `.map(BuildError::Check)`: the first assertion sees one `Check` where three are
    /// expected and this goes red.
    #[test]
    fn a_many_of_the_check_is_flattened_into_its_members() {
        let leaf = || CompilationError::Manifest(vec![]);
        let nested = CompilationError::Many(vec![
            leaf(),
            CompilationError::Many(vec![CompilationError::Resolution(vec![])]),
        ]);

        let flat = check_errors(vec![nested, leaf()]);

        assert_eq!(flat.len(), 3);
        assert!(matches!(
            flat[0],
            BuildError::Check(CompilationError::Manifest(_))
        ));
        assert!(matches!(
            flat[1],
            BuildError::Check(CompilationError::Resolution(_))
        ));
        assert!(matches!(
            flat[2],
            BuildError::Check(CompilationError::Manifest(_))
        ));
        assert!(!flat
            .iter()
            .any(|e| matches!(e, BuildError::Check(CompilationError::Many(_)))));
    }

    /// `module` is `Some` for an `Emit` and for a `Check` of one module, `None` for an
    /// error that is about no one module.
    ///
    /// Mutation-checked by making the `Emit` arm return `None`: the first assertion
    /// goes red.
    #[test]
    fn module_names_the_module_of_an_emit_error() {
        let emit = BuildError::Emit(vec![], Name::new("Main"));
        assert_eq!(emit.module(), Some(&Name::new("Main")));

        let check = BuildError::Check(CompilationError::Canonical(vec![], Name::new("Other")));
        assert_eq!(check.module(), Some(&Name::new("Other")));

        assert_eq!(BuildError::Many(vec![]).module(), None);
    }
}
