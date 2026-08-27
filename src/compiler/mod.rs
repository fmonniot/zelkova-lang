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
//! 7. TODO Try a way to weave error management in each of those passes :)
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
mod dependencies;
mod exhaustiveness;
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

// We may be able to not list all errors by asking a trait
// AsDiagnostic instead. Maybe. Or just a Diagnostic.
#[derive(Debug)]
pub enum CompilationError {
    LoadingFiles(Vec<SourceFileError>),
    Source(parser::Error, SourceFileId),
    Canonical(Vec<canonical::Error>, Name),
    DependenciesError(dependencies::Error),

    /// Every error accumulated over one compilation pass.
    ///
    /// `compile_package` does not stop on the first failure: it keeps going so that
    /// one broken module cannot hide the diagnostics of the others. This variant is
    /// how that accumulation becomes a failure again at the end of the pass, with the
    /// typed errors still intact for the caller to inspect.
    Many(Vec<CompilationError>),

    /// Not an error, but something I use until I get to implement the actual error.
    /// Ultimately, this error should be removed from the code base
    PlaceHolder,
}

impl CompilationError {
    fn as_diagnostic(&self) -> Diagnostic<SourceFileId> {
        match self {
            CompilationError::Source(err, file_id) => err.diagnostic(*file_id),
            CompilationError::LoadingFiles(errors) => {
                let notes = errors
                    .iter()
                    .map(|error| {
                        format!(
                            "{}\n{}\n{}",
                            error.file_name(),
                            error.message(),
                            error.note().unwrap_or_else(|| "".to_owned())
                        )
                    })
                    .collect();

                Diagnostic::error()
                    .with_message("Error while loading the package files")
                    .with_notes(notes)
            }
            CompilationError::Canonical(errors, module_name) => Diagnostic::error()
                .with_message(format!(
                    "[{}] Canonical error messages are not implemented yet",
                    module_name
                ))
                .with_notes(errors.iter().map(|e| format!("{:?}", e)).collect()),
            CompilationError::PlaceHolder => {
                Diagnostic::bug().with_message("A non implemented error message have been emitted")
            }
            CompilationError::DependenciesError(err) => Diagnostic::error()
                .with_message("Dependencies error messages are not implemented yet")
                .with_notes(vec![format!("{:?}", err)]),
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

    fn canonical(errors: Vec<canonical::Error>, module: Name) -> Self {
        CompilationError::Canonical(errors, module)
    }
}

impl From<typer::Error> for CompilationError {
    fn from(_err: typer::Error) -> Self {
        CompilationError::PlaceHolder
    }
}

impl From<exhaustiveness::Error> for CompilationError {
    fn from(_err: exhaustiveness::Error) -> Self {
        todo!()
    }
}

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
    let canonical = canonical::canonicalize(package, interfaces, source)
        .map_err(|errors| CompilationError::canonical(errors, source.name.clone()))?;

    // - type checking and inference
    // TODO Here either type checks return the new types, or it take a mutable canonical
    // representation and "fill the blank" directly on the canonical AST.
    typer::type_check(&canonical)?;

    // verify in pattern matching branches that all variants are covered
    exhaustiveness::check(&canonical)?;

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
}
