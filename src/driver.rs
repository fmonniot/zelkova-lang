//! Building a package: the half of `zelkova compile`, `zelkova test` and `zelkova run`
//! that comes after the check.
//!
//! [`compiler::check_package`] and [`compiler::check_package_with_tests`] check a package
//! and every package it depends on, and print, write and render nothing. This module
//! takes what they hand back and does the rest: it prints each status line to stderr,
//! emits a build that checked and writes it, and renders every error to stderr.
//!
//! # Emitting and writing
//!
//! Once every module of every package has checked, each one is emitted as JavaScript
//! (`javascript::emit`) and, if that failed nowhere either, the build is written to
//! `build/out/js/` (`output::write`). A build with any error writes nothing. A build that
//! also compiled the tests writes a second, complete tree at `build/test/js/` — the
//! runtime, then one directory per package, holding every package of the build (a
//! test-only one included) and the root's `tests/` modules beside its `src/` ones — so
//! that tree alone is what a test runner ever reads from.
//!
//! Which file is a facade's companion is this module's to find, not the check's: a
//! checked module comes back as a [`CheckedSource`] naming the source root it was read
//! under, and [`javascript::module_file`] names the companion below it ([*A facade names
//! a boundary, not a
//! backend*](../../docs/spec/interop.md#a-facade-names-a-boundary-not-a-backend)).
//!
//! Every error is rendered through `as_diagnostic`, and the status lines are printed
//! before any of them.

use std::io::Write;
use std::path::{Path, PathBuf};

use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::termcolor::{Color, ColorChoice, ColorSpec, StandardStream};
use codespan_reporting::term::{self};
use log::debug;

use crate::compiler::name::Name;
use crate::compiler::source::files::SourceFileId;
use crate::compiler::source::Overlay;
use crate::compiler::{
    self, javascript, output, CheckedModule, CheckedSource, CompilationError, Interface,
    PackageCheck, Status,
};

/// Which of the root package's source roots a build compiles: [`compiler::check_package`]'s
/// `src/` alone, or [`compiler::check_package_with_tests`]'s `src/` and `tests/`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TestRoot {
    /// `src/` alone.
    Skipped,
    /// Both roots.
    Compiled,
}

/// Compile the package rooted at `package_dir` — a directory holding a `zelkova.toml`
/// manifest beside a `src/` source root, per
/// [`docs/spec/packages.md`](../../docs/spec/packages.md#what-a-package-is) — and every
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
/// The check itself is [`compiler::check_package`]'s. This prints its status lines to stderr, emits
/// and writes the build, and renders every error to stderr before returning it inside
/// [`CompilationError::Many`]. An error raised before any source is read — the manifest,
/// the resolution — is returned bare and unrendered.
pub fn compile_package(package_dir: &Path) -> Result<(), CompilationError> {
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
/// interface*](../../docs/spec/toolchain.md#the-compilers-interface)).
pub const BUILD_DIRECTORY: &str = "build";

/// The tree a build that compiled the tests writes below `build_dir`: `build_dir/test/js/`,
/// laid out like `build_dir/out/js/`. Both the write of that tree and the entry point
/// [`compiler::test_runner::run`] puts in it name it through here.
pub fn test_tree(build_dir: &Path) -> PathBuf {
    build_dir.join("test").join("js")
}

/// [`compile_package`], writing its output below `build_dir` rather than below the
/// package's own `build/`.
///
/// The output is one tree, `<build_dir>/out/js/`: the runtime at its root, then one
/// directory per package of the build holding one `.mjs` file per module of that
/// package, named after the module within its own package, and each facade's companion
/// beside the facade ([`javascript`]'s *Paths* section has the names, [`DEC-18` decision
/// 5](../../docs/decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest)
/// the reasons). Nothing is written until every module of every package has checked and
/// emitted.
pub fn compile_package_into(package_dir: &Path, build_dir: &Path) -> Result<(), CompilationError> {
    compile(package_dir, TestRoot::Skipped, build_dir, &Overlay::new()).map(|_| ())
}

/// [`compile_package`], compiling the package's `tests/` root as well as its `src/`.
///
/// This is what running a package's own tests needs, and the only thing that ever reads
/// a `tests/` root: the tests of the build's other packages are not compiled, because
/// nothing outside a package reads its tests
/// ([*Running a package's tests*](../../docs/spec/toolchain.md#running-a-packages-tests)).
/// A package holding no `tests/` at all compiles exactly as it does through
/// [`compile_package`].
///
/// It compiles the tests and does not run them: what makes a declaration a test is
/// [its type](../../docs/spec/packages.md#what-a-test-is), and running one is
/// [`compiler::test_runner::run`]'s. What it hands back on success is the `Interface` of each of
/// the root's own `tests/` modules that checked — never a test-only package's, and never
/// `src/`'s — so a caller can find which of their exposed values are tests without a phase
/// dropping the checked modules once they are emitted. `test_collection::collect` is that
/// pass, and `test_runner::run` is its caller. Empty when the package holds no `tests/` at
/// all.
///
/// A test module and every `test-dependency`'s modules are checked and, unlike a plain
/// build, written — to a tree of their own, `build/test/js/`, laid out exactly like
/// `build/out/js/` and holding every package of the build (a test-only one included) plus the
/// root's `tests/` modules beside its `src/` ones. `build/out/js/` itself is left exactly as
/// [`compile_package`] would have written it: a test module never turns up there, so a
/// plain build run afterwards never finds one left behind by a run that also compiled the
/// tests ([`GEN-18`](../../docs/tickets/README.md)).
pub fn compile_package_with_tests(package_dir: &Path) -> Result<Vec<Interface>, CompilationError> {
    compile_package_with_tests_into(package_dir, &package_dir.join(BUILD_DIRECTORY))
}

/// [`compile_package_with_tests`], writing below `build_dir` rather than below the
/// package's own `build/` — the same relationship [`compile_package_into`] has to
/// [`compile_package`].
pub fn compile_package_with_tests_into(
    package_dir: &Path,
    build_dir: &Path,
) -> Result<Vec<Interface>, CompilationError> {
    compile(package_dir, TestRoot::Compiled, build_dir, &Overlay::new())
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

/// The CLI half of every `compile_package` variant: check the package
/// ([`compiler::check_package`] or [`compiler::check_package_with_tests`], as `tests`
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
) -> Result<Vec<Interface>, CompilationError> {
    let PackageCheck {
        package: root_package,
        mut errors,
        sources,
        modules,
        test_dependency_modules,
        test_modules,
        status,
    } = match tests {
        TestRoot::Skipped => compiler::check_package(package_dir, overlay)?,
        TestRoot::Compiled => compiler::check_package_with_tests(package_dir, overlay)?,
    };

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
        let checked: Vec<ModuleToEmit> = modules.into_iter().map(to_module_to_emit).collect();
        let mut root_tests_checked: Vec<ModuleToEmit> =
            test_modules.into_iter().map(to_module_to_emit).collect();

        // A test companion imports the companion it checks by its path in the source
        // tree, across the two roots, which reaches nothing in the build
        // ([*Testing a companion*](../../docs/spec/interop.md#testing-a-companion)). Each
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
                .map(|target| javascript::test_companion_import(checks, target))
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

        debug!("phase: codegen");
        // Every union of the build, read by a facade's boundary checks: a facade may name
        // a union any module of the build declares, and a test-only package's or the
        // root's `tests/` modules are part of the build their facades see.
        let unions = javascript::Unions::of(
            checked
                .iter()
                .chain(test_tree_modules.iter())
                .map(|to_emit| &to_emit.module),
        );
        let files = emit_build(checked, &unions, &mut errors);

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
            test_files.extend(emit_modules(test_tree_modules, &unions, &mut errors));
            test_files
        });

        if errors.is_empty() {
            debug!("phase: write the build");
            errors.extend(
                output::write(&build_dir.join("out").join("js"), &files)
                    .into_iter()
                    .map(CompilationError::Output),
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
                    .map(CompilationError::Output),
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
        Err(CompilationError::Many(errors))
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
    /// backend*](../../docs/spec/interop.md#a-facade-names-a-boundary-not-a-backend)).
    /// `None` for every other module, and for a facade with no companion, which
    /// [`javascript::emit`] refuses.
    companion: Option<std::path::PathBuf>,
    /// The imports its companion spells for the source tree, each beside the specifier
    /// that replaces it in the build ([`javascript::test_companion_import`]). Empty for
    /// every module but a facade of the root package's `tests/` with a companion, which
    /// [`compile`] fills in once it knows the companions of `src/` that one may import.
    companion_imports: Vec<(String, String)>,
}

/// What emitting and writing one checked module needs: its companion when it is a facade
/// with one sitting beside its source under its `root_dir`, so a facade under `tests/`
/// finds its companion there rather than under `src/`
/// ([*Testing a companion*](../../docs/spec/interop.md#testing-a-companion)).
fn to_module_to_emit(checked: CheckedSource) -> ModuleToEmit {
    let CheckedSource {
        module,
        file,
        root_dir,
    } = checked;
    let companion = Some(root_dir.join(javascript::module_file(module.canonical.name.name())))
        .filter(|path| module.ir.foreign && path.is_file());
    ModuleToEmit {
        file,
        companion,
        companion_imports: Vec::new(),
        module,
    }
}

/// Every file a build writes: the runtime, and each module of `checked` as the text
/// [`javascript::emit`] gives it, with its facade's companion beside it.
///
/// A module that cannot be emitted pushes its errors onto `errors`, tagged with the
/// file it came from, and every other module is still emitted so that one refusal
/// cannot hide the next. The caller writes the files only when `errors` stays empty.
fn emit_build(
    checked: Vec<ModuleToEmit>,
    unions: &javascript::Unions,
    errors: &mut Vec<CompilationError>,
) -> Vec<output::File> {
    let mut files = vec![output::File {
        path: javascript::RUNTIME_FILE.into(),
        contents: output::Contents::Text(javascript::RUNTIME.to_string()),
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
    unions: &javascript::Unions,
    errors: &mut Vec<CompilationError>,
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

        match javascript::emit(&module, companion.is_some(), unions) {
            Ok(text) => {
                files.push(output::File {
                    path: package_dir.join(javascript::module_file(name.name())),
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
                        path: package_dir.join(javascript::companion_file(name.name())),
                        contents,
                    });
                }
            }
            Err(emit_errors) => {
                let error = CompilationError::Emit(emit_errors, name.name().clone());
                errors.push(match file {
                    Some(id) => CompilationError::InFile(Box::new(error), id),
                    None => error,
                });
            }
        }
    }

    files
}
