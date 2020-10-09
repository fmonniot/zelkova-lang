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
//! 3. Create a `SourceFiles` mapping from `ModuleName` to `source::Module`.
//!     a. module names are deduced from file name
//!     b. parsing is done through `source::parse`
//!     c. Verify that source::Module.name match the one from the file system
//! 4. Build a dependency graphs from the modules import
//!     a. build it
//!     b. Verify there is no cyclic relation between modules
//! 5. Following the deps graph, compile each module (checking type, exhaustiveness, etc…)
//!     a. do it
//!     b. Bonus point to parallelize the tree branches which are not dependent on each others
//! 6. Once we have a module with all checks passing, create its interface and emit AST/interface
//! 7. TODO Try a way to weave error management in each of those passes :)

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::termcolor::{Color, ColorSpec, StandardStream};
use codespan_reporting::term::{self, ColorArg};
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::str::FromStr;
use walkdir::WalkDir;

pub mod canonical;
mod exhaustiveness;
pub mod position;
pub mod source;
pub mod source_files;
pub mod typing;

use source_files::{SourceFile, SourceFileId, SourceFiles};

/// A package name is composed of an author and project name and is written as `author/project`.
#[derive(Eq, PartialEq, Hash)]
pub struct PackageName {
    author: String,
    project: String,
}

/// A module name represent
#[derive(Eq, PartialEq, Hash)]
pub struct ModuleName {
    package: PackageName,
    name: String, // including dots
}

/// An interface is trim down version of a module.
///
/// We use it when translating a local source AST into its canonical form as
/// an optimization technique. Instead of parsing every source files on each
/// file compilation, we save the publicly exposed information of a successfully
/// parsed module and only load this information on module depending on it.
pub struct Interface {
    package: PackageName,
    values: HashMap<source::Name, source::Type>,
    types: HashMap<source::Name, source::UnionType>,
    // TODO type aliases
    //aliases: HashMap<source::Name, >
    infixes: HashMap<source::Name, source::Infix>,
}

#[derive(Debug)]
pub enum CompilationError {
    LoadingFiles(Vec<source_files::SourceFileError>),
    Source(source::Error, SourceFileId),
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
        }
    }

    fn from(err: source::Error, source_id: SourceFileId) -> Self {
        CompilationError::Source(err, source_id)
    }
}

impl From<canonical::Error> for CompilationError {
    fn from(err: canonical::Error) -> Self {
        todo!()
    }
}

impl From<typing::Error> for CompilationError {
    fn from(err: typing::Error) -> Self {
        todo!()
    }
}

impl From<exhaustiveness::Error> for CompilationError {
    fn from(err: exhaustiveness::Error) -> Self {
        todo!()
    }
}

impl From<Vec<source_files::SourceFileError>> for CompilationError {
    fn from(errors: Vec<source_files::SourceFileError>) -> Self {
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
    let sources = load_package_sources(&package_path)?;

    // Further steps will produces errors. We aggregates them here and will report them at
    // the end of the compilation phase.
    let mut diagnostics: Vec<codespan_reporting::diagnostic::Diagnostic<SourceFileId>> = vec![];

    // Step 3.b
    let modules: Vec<_> = {
        let (oks, fails): (Vec<_>, Vec<Result<_, CompilationError>>) = sources
            .iter()
            .map(|(id, file)| {
                source::parse(file.file()).map_err(|err| CompilationError::from(err, id))
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
    // TODO Include this into the source::parse() function (w/ module name as argument) ?

    // Step 4
    // TODO Build dependency graphs between modules

    // Step 5
    // TODO Follow graph and call check_module on each

    // Step 7
    for err in diagnostics {
        term::emit(&mut writer.lock(), &config, &sources, &err).unwrap();
    }

    Ok(())
}

// We don't support non-UTF8 characters in path
fn load_package_sources(root: &Path) -> Result<SourceFiles, CompilationError> {
    let walk = WalkDir::new(root)
        .follow_links(true)
        .into_iter()
        .filter_map(|r| r.ok())
        .filter_map(|entry| {
            let path = entry.path().to_path_buf();
            let ext = path.extension();

            match ext {
                Some(os_str) if os_str == "zel" => Some(path),
                _ => None,
            }
        });

    let mut sources = SourceFiles::new();
    let mut errors = vec![];

    for abs_path in walk {
        match SourceFile::load(abs_path, root) {
            Ok(src) => {
                sources.add_file(src);
            }
            Err(err) => {
                errors.push(err);
            }
        };
    }

    if errors.is_empty() {
        Ok(sources)
    } else {
        Err(errors.into())
    }
}

/// Take a parsed module file within the ecosystem and apply all checks to it
pub fn check_module(
    package: PackageName,
    interfaces: HashMap<ModuleName, Interface>,
    source: source::Module,
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
    typing::type_check(&canonical)?;

    // verify in pattern matching branches that all variants are covered
    exhaustiveness::check(&canonical)?;

    Ok(canonical)
}

