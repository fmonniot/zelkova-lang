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
use codespan_reporting::term::termcolor::{Color, ColorSpec, StandardStream};
use codespan_reporting::term::{self, ColorArg};
use log::debug;
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::str::FromStr;

pub mod canonical;
mod dependencies;
mod exhaustiveness;
pub mod name;
pub mod parser;
pub mod position;
pub mod source;
pub mod typer;

use name::{Name, QualName};
use source::files::{SourceFileError, SourceFileId};

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

    // TODO tests
    pub fn qualify_name(&self, name: &Name) -> QualName {
        if name.starts_with(&self.name) {
            name.to_qual()
        } else {
            name.qualify_with_name(&self.name).to_qual()
        }
    }
}

/// An interface is trim down version of a module.
///
/// We use it when translating a local source AST into its canonical form as
/// an optimization technique. Instead of parsing every source files on each
/// file compilation, we save the publicly exposed information of a successfully
/// parsed module and only load this information on module depending on it.
///
/// TODO Decide if the Name in the maps are fully qualified or not
pub struct Interface {
    module_name: ModuleName,
    values: HashMap<Name, canonical::Type>,
    unions: HashMap<Name, canonical::UnionType>,
    // TODO type aliases
    //aliases: HashMap<Name, >
    /// infixes is a map from the operator symbol to its information
    infixes: HashMap<Name, canonical::Infix>,
}

// We may be able to not list all errors by asking a trait
// AsDiagnostic instead. Maybe. Or just a Diagnostic.
#[derive(Debug)]
pub enum CompilationError {
    LoadingFiles(Vec<SourceFileError>),
    Source(parser::Error, SourceFileId),
    Canonical(Vec<canonical::Error>),
    DependenciesError(dependencies::Error),

    /// Not an error, but something I use until I get to implement the actual error.
    /// Ultimately, this error should be removed from the code base
    PlaceHolder,
}

impl<'a> CompilationError {
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
            CompilationError::Canonical(errors) => Diagnostic::warning()
                .with_message("Canonical error messages are not implemented yet")
                .with_notes(errors.iter().map(|e| format!("{:?}", e)).collect()),
            CompilationError::PlaceHolder => {
                Diagnostic::bug().with_message("A non implemented error message have been emitted")
            }
            CompilationError::DependenciesError(err) => Diagnostic::warning()
                .with_message("Dependencies error messages are not implemented yet")
                .with_notes(vec![format!("{:?}", err)]),
        }
    }

    fn from(err: parser::Error, source_id: SourceFileId) -> Self {
        CompilationError::Source(err, source_id)
    }
}

impl From<Vec<canonical::Error>> for CompilationError {
    fn from(errors: Vec<canonical::Error>) -> Self {
        CompilationError::Canonical(errors)
    }
}

impl From<typer::Error> for CompilationError {
    fn from(_err: typer::Error) -> Self {
        todo!()
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
    let mut writer = StandardStream::stderr(ColorArg::from_str("auto").unwrap().into());
    let config = codespan_reporting::term::Config {
        tab_width: 2,
        ..codespan_reporting::term::Config::default()
    };

    let mut print_success = |text: String| {
        writer
            .set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Green)))
            .unwrap();
        write!(&mut writer, "success").unwrap();
        writer.reset().unwrap();
        writeln!(&mut writer, " {}", text).unwrap();
    };

    // Step 1: package_path parameter

    // Step 2 and 3.a
    debug!("phase: load package sources");
    let sources = source::load_package_sources(&package_path)?;

    // Further steps will produces errors. We aggregates them here and will report them at
    // the end of the compilation phase.
    let mut diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<SourceFileId>> = vec![];

    // Step 3.b
    debug!("phase: parse package sources");
    let modules: Vec<_> = {
        let (oks, fails): (Vec<_>, Vec<Result<_, CompilationError>>) = sources
            .iter()
            .map(|(id, file)| {
                parser::parse(file.file()).map_err(|err| CompilationError::from(err, id))
            })
            .partition(Result::is_ok);

        diagnostics.extend(
            fails
                .into_iter()
                .map(Result::unwrap_err)
                .map(|e| e.as_diagnostic()),
        );

        oks.into_iter().map(Result::unwrap).collect()
    };

    print_success(format!("parsed {} modules", modules.len()));

    // Step 3.c
    // TODO Verify modules name match file system.
    // TODO Include this into the parser::parse() function (w/ module name as argument) ?

    debug!("phase: Build module dependency graph");
    // Step 4
    let walker = dependencies::ModuleWalker::new(&modules)?;

    // TODO Load those information from somewhere
    let package_name = PackageName::new("zelkova", "core");
    let mut interfaces = std::collections::HashMap::new();

    debug!("phase: Checks modules");

    // Step 5: Follow graph and call check_module on each
    let can_mods = walker
        .check_in_order(&package_name, &mut interfaces, check_module)
        .unwrap_or_else(|errors| {
            diagnostics.extend(errors.into_iter().map(|e| e.as_diagnostic()));

            vec![]
        });
    print_success(format!("checked modules: {:#?}", can_mods));

    debug!("phase: codegen");

    // Step 7
    for err in diagnostics {
        term::emit(&mut writer.lock(), &config, &sources, &err).unwrap();
    }

    Ok(())
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
    let canonical = canonical::canonicalize(package, interfaces, source)?;

    // - type checking and inference
    // TODO Here either type checks return the new types, or it take a mutable canonical
    // representation and "fill the blank" directly on the canonical AST.
    typer::type_check(&canonical)?;

    // verify in pattern matching branches that all variants are covered
    exhaustiveness::check(&canonical)?;

    Ok(canonical)
}
