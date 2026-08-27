//! The Zelkova compiler
//!
//!
//!# How to compile a package ?
//!
//! Note: We don't manage interfaces and external modules. We try to keep them
//! in mind, so they can be relatively easily added later on.
//!
//! 1. Start at the src/ folder. We name it root. (later on defined in a `zelkova.json` manifest)
//! 2. Collect all `*.zelkova` files with their path name relatives to the root.
//! 3. Create a `SourceFiles` mapping from `ModuleName` to `parser::Module`.
//!     1. module names are deduced from file name
//!     2. parsing is done through `parser::parse`
//!     3. Verify that `parser::Module.name` match the one from the file system
//! 4. Build a dependency graphs from the modules import
//!     1. build it
//!     2. Verify there is no cyclic relation between modules
//! 5. Following the deps graph,
//!     1. canonicalize each modules
//!     1. check each module (type check, exhaustiveness, etc…)
//!     1. Bonus point to parallelize the tree branches which are not dependent on each others
//! 6. Once we have a module with all checks passing, create its interface and emit AST/interface
//! 7. Report. Each phase returns every error it found, `check_module` tags those with
//!    the module they came from, and `compile_package` accumulates them across modules
//!    and renders them all through `CompilationError::as_diagnostic` — the one place a
//!    `Diagnostic` is built. See the `PhaseError` trait for what a phase error owes it.
//!

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::termcolor::{Color, ColorChoice, ColorSpec, StandardStream};
use codespan_reporting::term::{self};
use log::debug;
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;

pub mod canonical;
// Public so that `tests/pipeline.rs` can drive `ModuleWalker::check_in_order` with the
// real `check_module`, which is the only seam that observes the modules that checked
// successfully alongside the ones that failed (`BUG-2`) — `compile_package` only reports
// them to stderr. `dependencies::Error` was already reachable from the public
// `CompilationError::DependenciesError`, so this names an existing part of the API
// rather than widening it.
pub mod dependencies;
// Public for the same reason as `dependencies`: `exhaustiveness::Error` is
// reachable from the public `CompilationError::Exhaustiveness`, so the module
// that defines it has to be nameable. It also puts the last phase module on the
// same footing as `canonical`, `typer` and `parser`.
pub mod exhaustiveness;
pub mod name;
pub mod parser;
pub mod position;
pub mod source;
pub mod tuple;
pub mod typer;

use name::{Name, QualName};
use source::files::{SourceFileError, SourceFileId};

// TODO Move PackageName and ModuleName into the name module
/// A package name is composed of an author and project name and is written as `author/project`.
#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct PackageName {
    author: String,
    project: String,
}

impl PackageName {
    pub fn new<S: Into<String>>(author: S, project: S) -> PackageName {
        PackageName {
            author: author.into(),
            project: project.into(),
        }
    }
}

/// A module name represent
#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct ModuleName {
    package: PackageName,
    name: Name, // including dots
}

impl ModuleName {
    pub fn new(package: PackageName, name: Name) -> ModuleName {
        ModuleName { package, name }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    /// Simple shortcut to qualify a given name with this module's name
    pub fn qualify_name(&self, name: &Name) -> QualName {
        name.qualify_with_name(&self.name).unwrap()
    }

    fn as_human_string(&self) -> String {
        format!(
            "{}/{}:{}",
            self.package.author, self.package.project, self.name
        )
    }
}

/// An interface is trim down version of a module.
///
/// We use it when translating a local source AST into its canonical form as
/// an optimization technique. Instead of parsing every source files on each
/// file compilation, we save the publicly exposed information of a successfully
/// parsed module and only load this information on module depending on it.
///
/// All `Interface` indices are using non-qualified names. To get the qualified
/// version, simply use `I.module_name.qualify_name(&name)`.
// TODO Union types will need a way to reflect that some type constructor are private
#[derive(Debug)]
pub struct Interface {
    pub module_name: ModuleName,
    pub values: HashMap<Name, canonical::Type>,
    pub unions: HashMap<Name, canonical::UnionType>,
    // TODO type aliases
    //aliases: HashMap<Name, >
    /// infixes is a map from the operator symbol to its information
    pub infixes: HashMap<Name, canonical::Infix>,
}

/// What a compiler phase owes the diagnostic reporter.
///
/// Each phase keeps its own error type — one enum for the whole compiler would make
/// every phase depend on the vocabulary of every other. What they have to share is
/// the ability to describe themselves in the user's terms, because
/// [`CompilationError::as_diagnostic`] is the only place a `Diagnostic` is ever
/// built and it has no phase-specific knowledge to fall back on. Dumping
/// `format!("{:?}", e)` into a note is exactly what this trait replaces: a `Debug`
/// dump names Rust types, not source constructs.
///
/// # Why there is no span here
///
/// The reporter would rather have a `Span<BytePos>` and the [`SourceFileId`] it
/// belongs to, so it could point at the offending source. Only `parser::Error` can
/// supply one, and it does — it builds its own labelled `Diagnostic` through
/// `parser::Error::diagnostic` and deliberately does *not* go through this trait.
///
/// Every phase after parsing reads an AST with no positions in it at all:
/// `grammar.lalrpop` never captures `@L`/`@R`, so no `parser::Module` node carries a
/// span, and nothing canonicalization derives from it does either. A canonical or
/// typer error therefore has nothing to point at, whatever shape its error type is
/// given — adding a `span` field would only move the problem to the construction
/// site, which has no span to hand it. Making that possible means giving both ASTs
/// spans, which is a grammar-wide change; it is tracked separately as `ERR-3` (see
/// `docs/tickets/INDEX.md`). Until it lands, phases after parsing render as a
/// message plus notes, with no label.
///
/// The `SourceFileId` half is settled and stays settled: a phase never knows it. It
/// is attached by `compile_package`, the only place that knows which file a module
/// was read from — the way `CompilationError::Source` already works.
pub trait PhaseError {
    /// One line naming what went wrong, in the vocabulary of the user's source.
    ///
    /// This is rendered as the diagnostic's headline, so it has to read on its own:
    /// no `{:?}`, no Rust type names.
    fn message(&self) -> String;

    /// Supporting detail, one string per rendered note. Empty by default.
    fn notes(&self) -> Vec<String> {
        Vec::new()
    }

    /// This error's message followed by its notes.
    ///
    /// A `Diagnostic` has one headline, so an error that ends up inside a group —
    /// several errors from one phase, or a variant like `canonical::Error::Many`
    /// that wraps others — has to give up its headline and become notes. This is
    /// that demotion, in one place, so a group cannot silently drop the message of
    /// a member it swallowed.
    fn message_and_notes(&self) -> Vec<String> {
        std::iter::once(self.message())
            .chain(self.notes())
            .collect()
    }
}

/// Render the errors one phase produced for one module.
///
/// A `Diagnostic` has room for exactly one headline, so a lone error gets to be that
/// headline and a group is summarised instead, with every message demoted to a note.
/// `phase` names the phase in that summary line ("canonical", "type", …).
fn phase_diagnostic<E: PhaseError>(
    module: &Name,
    phase: &str,
    errors: &[E],
) -> Diagnostic<SourceFileId> {
    match errors {
        [only] => Diagnostic::error()
            .with_message(format!("[{}] {}", module, only.message()))
            .with_notes(only.notes()),
        many => Diagnostic::error()
            .with_message(format!(
                "[{}] {} {} error{}",
                module,
                many.len(),
                phase,
                if many.len() == 1 { "" } else { "s" }
            ))
            .with_notes(many.iter().flat_map(|e| e.message_and_notes()).collect()),
    }
}

/// Every way compiling a package can fail, tagged with the phase that failed.
///
/// Each phase-carrying variant holds *all* the errors that phase produced for one
/// module rather than only the first, plus the module's [`Name`], which is what
/// `as_diagnostic` puts in front of the message. Rendering goes through
/// [`PhaseError`]; see that trait for why no variant here carries a span.
#[derive(Debug)]
pub enum CompilationError {
    LoadingFiles(Vec<SourceFileError>),
    Source(parser::Error, SourceFileId),
    Canonical(Vec<canonical::Error>, Name),
    /// Type checking failed for the named module.
    Type(Vec<typer::Error>, Name),
    /// Exhaustiveness checking failed for the named module. Unreachable while
    /// `exhaustiveness::check` is a stub, but rendered like any other phase.
    Exhaustiveness(Vec<exhaustiveness::Error>, Name),
    DependenciesError(dependencies::Error),

    /// Every error accumulated over one compilation pass.
    ///
    /// `compile_package` does not stop on the first failure: it keeps going so that
    /// one broken module cannot hide the diagnostics of the others. This variant is
    /// how that accumulation becomes a failure again at the end of the pass, with the
    /// typed errors still intact for the caller to inspect.
    Many(Vec<CompilationError>),
}

impl CompilationError {
    /// Turn this error into the diagnostic the user reads.
    ///
    /// This is the compiler's single rendering point — `compile_package` calls it
    /// and nothing else builds a `Diagnostic` from a phase error. It is public so
    /// that a test can assert on what the user is actually shown, rather than on
    /// `is_err()`: what a failure *says* is the behaviour this method exists for.
    pub fn as_diagnostic(&self) -> Diagnostic<SourceFileId> {
        match self {
            // The one phase that carries spans renders its own labelled diagnostic.
            CompilationError::Source(err, file_id) => err.diagnostic(*file_id),
            // Loading failures are not attached to a module — there is no module yet,
            // and each error names its own file instead.
            CompilationError::LoadingFiles(errors) => Diagnostic::error()
                .with_message("Error while loading the package files")
                .with_notes(errors.iter().flat_map(|e| e.message_and_notes()).collect()),
            CompilationError::Canonical(errors, module) => {
                phase_diagnostic(module, "canonical", errors)
            }
            CompilationError::Type(errors, module) => phase_diagnostic(module, "type", errors),
            CompilationError::Exhaustiveness(errors, module) => {
                phase_diagnostic(module, "exhaustiveness", errors)
            }
            // A dependency cycle belongs to the package, not to any one module, so it
            // does not go through `phase_diagnostic`.
            CompilationError::DependenciesError(err) => Diagnostic::error()
                .with_message(err.message())
                .with_notes(err.notes()),
            // `compile_package` renders each accumulated error individually rather than
            // wrapping first, so this arm only fires when a `Many` is rendered as a
            // whole. It summarises rather than repeating what those diagnostics said.
            CompilationError::Many(errors) => Diagnostic::error()
                .with_message(format!(
                    "compilation failed with {} error{}",
                    errors.len(),
                    if errors.len() == 1 { "" } else { "s" }
                ))
                .with_notes(errors.iter().map(|e| e.as_diagnostic().message).collect()),
        }
    }

    fn from(err: parser::Error, source_id: SourceFileId) -> Self {
        CompilationError::Source(err, source_id)
    }
}

// There is deliberately no `From<typer::Error>` or `From<exhaustiveness::Error>`
// here. Both used to exist and neither could be written honestly: a `CompilationError`
// needs the name of the module its error belongs to, and a phase error does not know
// it. `From` has nowhere to get it from, so the two impls lost information instead —
// one discarded the error and the other panicked. `check_module` is the one place that
// knows both halves, so that is where the conversion happens (see `ERR-2` in
// `docs/tickets/INDEX.md`).

impl From<dependencies::Error> for CompilationError {
    fn from(err: dependencies::Error) -> Self {
        CompilationError::DependenciesError(err)
    }
}

impl From<Vec<SourceFileError>> for CompilationError {
    fn from(errors: Vec<SourceFileError>) -> Self {
        CompilationError::LoadingFiles(errors)
    }
}

// TODO Ultimately we will pass a manifest content instead of a raw path
// (eg. something akin to elm.json or package.json)
pub fn compile_package(package_path: &Path) -> Result<(), CompilationError> {
    // Error reporter
    let mut writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config {
        tab_width: 2,
        ..codespan_reporting::term::Config::default()
    };

    // Reports the outcome of one phase on stderr. Failing to write a status line is
    // not itself a compilation failure, so the write results are deliberately
    // discarded rather than unwrapped.
    let mut print_status = |success: bool, text: String| {
        let (color, label) = if success {
            (Color::Green, "success")
        } else {
            (Color::Red, "failure")
        };
        let _ = writer.set_color(ColorSpec::new().set_bold(true).set_fg(Some(color)));
        let _ = write!(&mut writer, "{}", label);
        let _ = writer.reset();
        let _ = writeln!(&mut writer, " {}", text);
    };

    // Step 1: package_path parameter

    // Step 2 and 3.a
    debug!("phase: load package sources");
    // Loading is the one phase whose errors cannot be deferred: without the loaded
    // files there is no `Files` database to render any diagnostic against, this one
    // included. It is returned unrendered and the caller reports it.
    let sources = source::load_package_sources(package_path)?;

    // Further steps will produce errors. We aggregate them here and report them at the
    // end of the compilation phase, rather than stopping on the first one, so that a
    // single broken module doesn't hide the diagnostics of every other module.
    //
    // They are kept as typed `CompilationError`s and not as already-rendered
    // `Diagnostic`s for two reasons: `as_diagnostic` stays the single rendering point,
    // and the accumulation is still meaningful as a return value — an empty vector is
    // what makes this function return `Ok`.
    let mut errors: Vec<CompilationError> = vec![];

    // Step 3.b
    debug!("phase: parse package sources");
    let mut modules: Vec<parser::Module> = vec![];
    let mut parse_failures = 0;
    for (id, file) in sources.iter() {
        match parser::parse(file.file()) {
            Ok(module) => modules.push(module),
            Err(err) => {
                parse_failures += 1;
                errors.push(CompilationError::from(err, id));
            }
        }
    }

    if parse_failures == 0 {
        print_status(true, format!("parsed {} modules", modules.len()));
    } else {
        print_status(
            false,
            format!(
                "parsed {} modules, {} failed to parse",
                modules.len(),
                parse_failures
            ),
        );
    }

    // Step 3.c
    // TODO Verify modules name match file system.
    // TODO Include this into the parser::parse() function (w/ module name as argument) ?

    debug!("phase: Build module dependency graph");
    // Step 4
    // A cycle leaves us with no order to check the modules in, so the check phase is
    // skipped — but the error goes through the same reporting path as the others
    // instead of returning early unrendered.
    let walker = match dependencies::ModuleWalker::new(&modules) {
        Ok(walker) => Some(walker),
        Err(err) => {
            errors.push(err.into());
            None
        }
    };

    // TODO Load those information from somewhere
    let package_name = PackageName::new("zelkova", "core");
    let mut interfaces = std::collections::HashMap::new();

    debug!("phase: Check modules");

    // Step 5: Follow graph and call check_module on each
    if let Some(walker) = walker {
        // `check_in_order` checks every module regardless of earlier failures and
        // hands back both halves: the modules that checked, and the errors from the
        // ones that didn't (see `docs/tickets/INDEX.md`, `BUG-2`). Both are reported
        // here, and the errors still flow into `errors` below so a failing module
        // keeps making this function return `Err` — only the previously-discarded
        // successes are new.
        let (can_mods, check_errors) =
            walker.check_in_order(&package_name, &mut interfaces, check_module);

        if check_errors.is_empty() {
            print_status(
                true,
                format!(
                    "checked modules: {:#?}",
                    can_mods
                        .iter()
                        .map(|m| m.name.as_human_string())
                        .collect::<Vec<_>>()
                ),
            );
        } else {
            print_status(
                false,
                format!(
                    "checked modules: {:#?} ({} failed to check)",
                    can_mods
                        .iter()
                        .map(|m| m.name.as_human_string())
                        .collect::<Vec<_>>(),
                    check_errors.len()
                ),
            );
            errors.extend(check_errors);
        }
    }

    // Step 6
    // emit interfaces and generate code
    debug!("phase: codegen");

    // Step 7: report everything we accumulated, then let that accumulation decide the
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
        Ok(())
    } else {
        Err(CompilationError::Many(errors))
    }
}

/// Take a parsed module file within the ecosystem and apply all checks to it
///
/// TODO canonicalization must happens before checkings, because type check (at least)
/// will require access to other modules canonical representation.
/// That probably mean moving the `canonical::canonicalize` call out of this function
pub fn check_module(
    package: &PackageName,
    interfaces: &HashMap<Name, Interface>,
    source: &parser::Module,
) -> Result<canonical::Module, CompilationError> {
    // - desugar ~?~ *!*
    // Should I have an intermediate AST before type checking ?
    // This could actually be useful to have something optimized for
    // the type checker. It would also be something that can be used
    // as an information dump for dependencies (keep types solved as
    // a result and don't type checks those modules more than once).
    //
    // Each phase accumulates its own errors and hands back all of them; this is where
    // they are tagged with the module they came from, because a phase only ever sees
    // one module and has no reason to carry its name around.
    let canonical = canonical::canonicalize(package, interfaces, source)
        .map_err(|errors| CompilationError::Canonical(errors, source.name.clone()))?;

    // - type checking and inference
    // TODO Here either type checks return the new types, or it take a mutable canonical
    // representation and "fill the blank" directly on the canonical AST.
    typer::type_check(&canonical)
        .map_err(|errors| CompilationError::Type(errors, source.name.clone()))?;

    // verify in pattern matching branches that all variants are covered
    exhaustiveness::check(&canonical)
        .map_err(|errors| CompilationError::Exhaustiveness(errors, source.name.clone()))?;

    Ok(canonical)
}

#[cfg(test)]
mod tests {
    use super::*;
    use codespan_reporting::diagnostic::Severity;

    /// Canonicalization failures are errors, and used to be rendered as warnings.
    ///
    /// The severity is part of how a failure reaches the user, so it is pinned
    /// here rather than left to the eye. Mutation-checked by putting
    /// `Diagnostic::warning()` back in the `Canonical` arm of `as_diagnostic`.
    #[test]
    fn canonical_errors_render_as_errors() {
        let error = CompilationError::Canonical(vec![canonical::Error::NoBindings], "Test".into());

        assert_eq!(error.as_diagnostic().severity, Severity::Error);
    }

    /// Same as above for dependency errors — a module cycle is not a warning.
    ///
    /// Mutation-checked by putting `Diagnostic::warning()` back in the
    /// `DependenciesError` arm of `as_diagnostic`.
    #[test]
    fn dependency_errors_render_as_errors() {
        let error = CompilationError::DependenciesError(dependencies::Error::CycleDetected(vec![
            vec!["A".into(), "B".into()],
        ]));

        assert_eq!(error.as_diagnostic().severity, Severity::Error);
    }

    /// A dependency cycle names the modules in it, and names them as a loop.
    ///
    /// This arm used to say "Dependencies error messages are not implemented yet"
    /// with `format!("{:?}", err)` in a note, so the module names only ever reached
    /// the user inside a `Debug` dump. Mutation-checked by dropping the
    /// write-back-to-the-start in `dependencies::Error::notes`, which turns the
    /// trailing `-> A` assertion red.
    #[test]
    fn dependency_cycle_notes_spell_out_the_loop() {
        let error = CompilationError::DependenciesError(dependencies::Error::CycleDetected(vec![
            vec!["A".into(), "B".into()],
        ]));

        let diagnostic = error.as_diagnostic();

        assert_eq!(diagnostic.message, "1 circular dependency between modules");
        assert_eq!(diagnostic.notes, vec!["cycle: A -> B -> A".to_string()]);
    }

    /// `ERR-2`: `exhaustiveness::Error` was `pub enum Error {}` — uninhabited — and
    /// `From<exhaustiveness::Error> for CompilationError` was `todo!()`. The
    /// conversion could not fire only because the error could not be built; the
    /// first error the real checker reported would have panicked the compiler.
    ///
    /// Nothing constructs this variant on a non-test path yet, so this test is what
    /// establishes the phase can report at all. Mutation-checked by collapsing
    /// `exhaustiveness::Error::message` to a constant string, which drops both names
    /// out of the headline and turns the message assertion red.
    #[test]
    fn exhaustiveness_errors_render_as_errors() {
        let error = CompilationError::Exhaustiveness(
            vec![exhaustiveness::Error::NonExhaustiveMatch {
                value: "describe".into(),
                tpe: "Shape".into(),
                missing: vec!["Square".into(), "Triangle".into()],
            }],
            "Test".into(),
        );

        let diagnostic = error.as_diagnostic();

        assert_eq!(diagnostic.severity, Severity::Error);
        assert_eq!(
            diagnostic.message,
            "[Test] the `case` expression in `describe` does not cover every variant of `Shape`"
        );
        assert_eq!(
            diagnostic.notes,
            vec!["no branch matches: Square, Triangle".to_string()]
        );
    }

    /// A phase that reported several errors renders all of them, not just the first.
    ///
    /// `Diagnostic` has one headline, so a group is summarised and every message
    /// becomes a note. Mutation-checked by making `phase_diagnostic` render only
    /// `errors[0]`, which drops the second note.
    #[test]
    fn several_phase_errors_all_reach_the_notes() {
        let error = CompilationError::Canonical(
            vec![
                canonical::Error::NoBindings,
                canonical::Error::TypeDeclared("Shape".into()),
            ],
            "Test".into(),
        );

        let diagnostic = error.as_diagnostic();

        assert_eq!(diagnostic.message, "[Test] 2 canonical errors");
        assert_eq!(diagnostic.notes.len(), 2, "notes: {:?}", diagnostic.notes);
        assert!(
            diagnostic.notes[1].contains("Shape"),
            "the second error must survive, got {:?}",
            diagnostic.notes
        );
    }
}
