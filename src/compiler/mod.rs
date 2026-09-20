//! The Zelkova compiler
//!
//!
//!# How to compile a package ?
//!
//! Note: an interface is built for every module that checks and kept for whatever
//! imports it, within this package and in the packages that depend on it. What is
//! still missing is emitting one, so a dependency is compiled from source on every
//! build.
//!
//! 1. Read the package's `zelkova.toml` manifest, and resolve the build: every package
//!    reachable through `dependencies`, plus the root package's `test-dependencies`, each
//!    ordered after the packages it depends on. Steps 2 to 6 then run once per package,
//!    over the two source roots — `src/` and, for the root package when its tests were
//!    asked for, `tests/` — beside its own manifest. Before a package's modules are
//!    checked, the whole map of module names it can import is built — its own, plus each
//!    direct dependency's public modules under that package's namespace or, unwrapped,
//!    under their own names — and a name two modules both answer to stops that package
//!    there.
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

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::termcolor::{Color, ColorChoice, ColorSpec, StandardStream};
use codespan_reporting::term::{self};
use log::debug;
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;

pub mod canonical;
/// The imports every module gets without writing them. Public because it is a rule
/// about the language rather than an implementation detail of one phase, and
/// because two phases read it: canonicalization synthesises the imports, and
/// `dependencies` puts the matching edges in the import graph.
pub mod default_imports;
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
/// `zelkova.toml`: reading it, and the shape it has to have. Public for the same
/// reason as `source` and `dependencies` — `manifest::ManifestError` is reachable
/// from the public `CompilationError::Manifest`.
pub mod manifest;
pub mod name;
pub mod parser;
pub mod position;
/// Which packages a build is made from, and what each module is called inside the
/// package that imports it. Public for the same reason as `manifest`:
/// `resolve::Error` is reachable from the public `CompilationError::Resolution`.
pub mod resolve;
/// The five type names the compiler knows. Public for the same reason as
/// `default_imports`: it is a rule about the language rather than an implementation
/// detail of one phase.
pub mod scalars;
pub mod source;
pub mod tuple;
pub mod typer;

use name::{Name, QualName};
use position::{BytePos, NodeSpan, Span};
use source::files::{SourceFileError, SourceFileId};
use source::SourceFiles;

// TODO Move PackageName and ModuleName into the name module
/// A package name: one flat identifier, ASCII lowercase letters, digits and hyphens,
/// starting with a letter, with every hyphen followed by a letter —
/// [`docs/spec/packages.md`](../../docs/spec/packages.md#the-manifest)'s whole rule for
/// `name`. That shape is what keeps a package name and a module name from ever being
/// confused (one is lowercase-with-hyphens, the other uppercase-with-dots), and what makes
/// the namespace a package's name derives unambiguous.
///
/// The only way to build one is [`PackageName::new`], which rejects anything that does not
/// match the rule — there is no way to hold a `PackageName` that manifest validation would
/// have refused.
#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct PackageName(String);

/// `PackageName::new` refused this string; it names the input so a caller can report it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvalidPackageName(pub String);

impl PackageName {
    /// Build a `PackageName`, or say why `name` is not one.
    pub fn new<S: Into<String>>(name: S) -> Result<PackageName, InvalidPackageName> {
        let name = name.into();
        if is_legal_package_name(&name) {
            Ok(PackageName(name))
        } else {
            Err(InvalidPackageName(name))
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// The prefix this package's modules are named through from outside it: the name
    /// split at its hyphens, each piece capitalised, joined —
    /// [*The namespace*](../../docs/spec/packages.md#the-namespace). `acme-widgets` is
    /// `AcmeWidgets`, `todo` is `Todo`.
    ///
    /// It is a [`Name`] and not a [`PackageName`]: what comes back is a module-name
    /// prefix, and the whole point of the shape rule above is that the two are never
    /// confusable. Distinct package names give distinct namespaces, which is what lets
    /// two wrapped dependencies keep out of each other's way with nothing checked.
    ///
    /// A package never writes its own namespace, so nothing inside the package being
    /// compiled goes through here: [`resolve::visible_modules`] is the one caller, and
    /// it applies this only to a *dependency*'s modules.
    pub fn namespace(&self) -> Name {
        let mut namespace = String::with_capacity(self.0.len());

        for piece in self.0.split('-') {
            let mut characters = piece.chars();
            if let Some(first) = characters.next() {
                namespace.extend(first.to_uppercase());
                namespace.push_str(characters.as_str());
            }
        }

        Name::new(namespace)
    }
}

impl std::fmt::Display for PackageName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// ASCII lowercase letters, digits and hyphens; starts with a letter; every hyphen is
/// immediately followed by a letter (so no leading, trailing or doubled hyphen, and no
/// hyphen before a digit).
fn is_legal_package_name(name: &str) -> bool {
    match name.chars().next() {
        Some(c) if c.is_ascii_lowercase() => {}
        _ => return false,
    }

    for (i, c) in name.char_indices() {
        if c == '-' {
            match name[i + 1..].chars().next() {
                Some(next) if next.is_ascii_lowercase() => {}
                _ => return false,
            }
        } else if !(c.is_ascii_lowercase() || c.is_ascii_digit()) {
            return false;
        }
    }

    true
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
        format!("{}:{}", self.package, self.name)
    }
}

/// A byte span paired with the file it was written in.
///
/// [`NodeSpan`] is enough for an error about the module a phase is currently
/// checking, because [`compile_package`] supplies that one file id for the whole
/// diagnostic at render time (see [`PhaseError`] and [`SpanLabel`]). A span pulled
/// out of an [`Interface`] cannot rely on that: it was written in the *exporting*
/// module's file, which is not necessarily the file being rendered, so the id has
/// to travel with the span instead of being attached later. This is that pair —
/// [`Interface::source_span`] is how one gets built.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceSpan {
    pub file: SourceFileId,
    pub span: Span<BytePos>,
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
//
// `Clone` because one interface reaches more than one place: a public module of a
// dependency is inserted into the environment of every package that imports it, under
// whichever spelling that package names it by (`resolve::visible_modules`). The
// interface itself is the same either way — a spelling is the importer's, and what an
// `Interface` carries is the module's own name.
#[derive(Debug, Clone)]
pub struct Interface {
    pub module_name: ModuleName,
    /// Each value's type, paired with where its declaration (annotation and body
    /// together) was written — [`canonical::Value::span`] — so a diagnostic about a
    /// name found here can point at the declaration, not just name it.
    pub values: HashMap<Name, (NodeSpan, canonical::Type)>,
    pub unions: HashMap<Name, canonical::UnionType>,
    // TODO type aliases
    //aliases: HashMap<Name, >
    /// infixes is a map from the operator symbol to its information
    pub infixes: HashMap<Name, canonical::Infix>,
    /// The type of an exposed infix's own backing function, keyed by that
    /// function's unqualified name — populated only when the function is *not*
    /// separately present in [`values`](Self::values), i.e. the header exposes
    /// the operator but not the function by name (`infix left 6 (+) = add`,
    /// header exposing `(+)` and not `add`, is `std/core`'s own shape for every
    /// operator it declares).
    ///
    /// `canonical::environment::imported_infix` is what needs it: an operator is
    /// resolved through its `infix` declaration, and the declaration alone names
    /// the backing function without saying anything about its type. Whichever of
    /// the two maps holds it, that is where the type comes from — and for an
    /// operator whose function is not separately exposed, this is the only one
    /// that can.
    ///
    /// Nothing else reads it, and in particular nothing inserts it into an
    /// importing module's scope under its own name: a function that backs an
    /// exposed operator is importable by name only when the header says so too
    /// (`BUG-9`), and then it is [`values`](Self::values) that carries it.
    pub infix_functions: HashMap<Name, (NodeSpan, canonical::Type)>,
    /// The file this interface's module was read from, when the caller knows it.
    ///
    /// It is what makes a diagnostic about an imported name able to underline that
    /// name's *own* source rather than merely naming its module. Nothing inside a
    /// module check can supply it: a phase only ever sees one module and never
    /// learns its [`SourceFileId`] (see [`PhaseError`]). Driver code does, so
    /// [`canonical::Module::to_interface`] takes it as an argument, and its one
    /// caller — [`dependencies::ModuleWalker::check_in_order`], which like
    /// [`compile_package`] is a driver loop and already holds the file each module
    /// came from — passes it there. A hand-built interface, as every test below
    /// `check_module` constructs, passes `None`; [`Interface::source_span`] then has
    /// no file to pair a span with and returns `None`, so the diagnostic degrades to
    /// no secondary label rather than a wrong one.
    pub file: Option<SourceFileId>,
}

impl Interface {
    /// Pair a [`NodeSpan`] taken from this interface's own data — a value's
    /// declaration, a [`canonical::UnionType`]'s or [`canonical::Infix`]'s `span`
    /// field — with the file it was written in, when both halves are known.
    ///
    /// Both halves are needed: a `NodeSpan` with nothing behind it (a hand-built
    /// test value) or an interface with no attached file (same) leaves nothing
    /// honest to build, and `None` is that answer — the caller renders no label
    /// rather than one pointing at byte 0 of the wrong file.
    pub fn source_span(&self, span: NodeSpan) -> Option<SourceSpan> {
        match (self.file, span.span()) {
            (Some(file), Some(span)) => Some(SourceSpan { file, span }),
            _ => None,
        }
    }
}

/// One underlined region of the user's source, and what to say about it.
///
/// # `file`: when a label is not about the module under check
///
/// A [`codespan_reporting::Label`] needs a file id as well as a byte range. Most
/// labels don't carry one themselves: a phase only ever sees one module, so its
/// spans belong to the file that module was read from, and [`compile_package`] —
/// the only place that knows which file that is — supplies it for the whole
/// diagnostic at render time. `file: None` is that case, and it covers everything
/// built before `ERR-5`.
///
/// `Some` is the escape hatch: a label built from a [`SourceSpan`] cloned out of an
/// [`Interface`] (via [`Interface::source_span`]) already carries its own file,
/// generally a *different* one from the module being checked — "the annotation you
/// are contradicting is over there, in `Basics`" — and that id is used verbatim
/// instead of the diagnostic's.
///
/// `primary` is the rustc distinction, and it applies independently of `file`: the
/// caret under the thing that is wrong is primary even when it is local, and
/// "defined here" in another file is secondary even though it is the only label
/// pointing into that file.
#[derive(Debug, Clone, PartialEq)]
pub struct SpanLabel {
    pub span: Span<BytePos>,
    pub message: String,
    pub primary: bool,
    pub file: Option<SourceFileId>,
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
/// # How much a span can say, and where that stops
///
/// Every production in `grammar.lalrpop` that builds a node captures `@L`/`@R`, so
/// declarations, expressions, patterns and types all know where they were written,
/// and canonicalization copies that onto the node it builds. An error raised from one
/// of those construction sites returns a [`SpanLabel`] from
/// [`labels`](PhaseError::labels) and gets a caret under the text it is about — an
/// unresolvable name is underlined at the identifier, not across the declaration
/// containing it.
///
/// Not every error can. `labels` defaults to empty and that is a real answer, not a
/// stub: an error raised while walking a node the grammar does not span — an
/// `exposing` list, say — has nowhere to point, and renders as message-plus-notes
/// with no caret, exactly as every phase after parsing used to. An error that groups
/// others (`canonical::Error::Many`, `EnvironmentErrors`) has no position of its own
/// and flattens its members' labels instead, the way it already flattens their
/// messages.
///
/// The typer is the phase that has to work for this, and it does: it does not check
/// the canonical AST but a term language of its own, so it carries each canonical
/// node's span into that language, each constraint records the term and the *reason*
/// it came from, and `unify` reports the origin of the constraint it failed on. A
/// type error therefore underlines the sub-expression that disagrees and adds a
/// secondary label under the annotation that says what was expected (`ERR-4`).
///
/// `parser::Error` is still the one phase error that does not go through this trait
/// at all — it builds its own labelled `Diagnostic` through `parser::Error::diagnostic`.
///
/// # The two ways a label learns its `SourceFileId`
///
/// A phase still never knows the id of the file the module it is checking came
/// from — that half is unchanged, and stays attached by `compile_package`, the only
/// place that knows it, the way `CompilationError::Source` already works. A
/// [`SpanLabel`] with `file: None` — everything built before `ERR-5` — means
/// exactly that: "in the module under check", resolved at render time.
///
/// That is right for an error entirely about one module and cannot cover a
/// diagnostic that also names something written elsewhere — "the annotation you
/// are contradicting is over there, in `Basics`". For that, `file` carries its own
/// [`SourceFileId`] instead, taken from a [`SourceSpan`] cloned out of an
/// [`Interface`] via [`Interface::source_span`]. An `Interface` can offer that
/// because it is not a phase: it is built by driver code
/// (`dependencies::ModuleWalker::check_in_order`) that already has the file the
/// module it just checked came from, and stamps it onto the `Interface` before
/// handing it to whoever imports next. `canonical::Type` still carries no span of
/// its own — seeing why is worth a look at that type's own documentation.
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

    /// The regions of the user's source this error is about, if it knows any.
    ///
    /// Empty — the default — means this error has no position to point at, and it
    /// renders as message-plus-notes with no caret. See the trait's documentation.
    fn labels(&self) -> Vec<SpanLabel> {
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

/// Turn `SpanLabel`s into the `codespan_reporting::Label`s a `Diagnostic` renders.
///
/// `fallback` is the file a label with none of its own resolves to — the module
/// currently being checked, when the caller has one. So `fallback` is a fallback
/// rather than a gate: a label needs *some* file, because a byte range on its own
/// does not say which file to underline, but a [`SpanLabel`] carrying its own
/// [`SpanLabel::file`] already has one and renders whether or not `fallback` is
/// `Some`. Only a label with neither is a byte range nobody can place, and that one
/// is dropped rather than guessed at.
///
/// This is the one place that distinction is applied; both [`phase_diagnostic`] (a
/// fallback file, from the module under check) and the dependency-cycle arm of
/// `CompilationError::as_diagnostic_in` (no fallback — a cycle has no single
/// module) go through it.
fn spans_to_labels(
    labels: Vec<SpanLabel>,
    fallback: Option<SourceFileId>,
) -> Vec<Label<SourceFileId>> {
    labels
        .into_iter()
        .filter_map(|l| {
            // A label with its own file — a `SourceSpan` cloned out of an
            // `Interface`, or (`ERR-6`) a `CycleEdge`'s file — points into that
            // file instead of falling back. That is the whole mechanism `ERR-5`
            // adds: everything built before it left `file` `None` and relied on
            // `fallback` here.
            let label_file = l.file.or(fallback)?;
            let range = l.span.to_range();
            let label = if l.primary {
                Label::primary(label_file, range)
            } else {
                Label::secondary(label_file, range)
            };
            Some(label.with_message(l.message))
        })
        .collect()
}

/// Render the errors one phase produced for one module.
///
/// A `Diagnostic` has room for exactly one headline, so a lone error gets to be that
/// headline and a group is summarised instead, with every message demoted to a note.
/// `phase` names the phase in that summary line ("canonical", "type", …).
///
/// `file` is the module's source file when the caller knows it, and it is handed
/// straight to [`spans_to_labels`] as the fallback that document describes: a label
/// of its own — the cross-module ones `ERR-5` added — renders regardless, so a
/// `CompilationError` built by hand, as the tests in this module do, loses only the
/// labels about the module under check. `compile_package` always wraps in
/// [`CompilationError::InFile`] and so loses none.
///
/// Both branches attach labels: only the headline demotion differs between one error
/// and a group, and an error that got swallowed into a note still knows where it was.
fn phase_diagnostic<E: PhaseError>(
    module: &Name,
    phase: &str,
    errors: &[E],
    file: Option<SourceFileId>,
) -> Diagnostic<SourceFileId> {
    let labels =
        |errors: &[E]| spans_to_labels(errors.iter().flat_map(|e| e.labels()).collect(), file);

    match errors {
        [only] => Diagnostic::error()
            .with_message(format!("[{}] {}", module, only.message()))
            .with_labels(labels(errors))
            .with_notes(only.notes()),
        many => Diagnostic::error()
            .with_message(format!(
                "[{}] {} {} error{}",
                module,
                many.len(),
                phase,
                if many.len() == 1 { "" } else { "s" }
            ))
            .with_labels(labels(many))
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
    /// The package's `zelkova.toml` is missing, malformed, or fails one of its own
    /// field-level rules (an illegal `name`, a dependency entry naming no source, …).
    ///
    /// Raised before [`LoadingFiles`](CompilationError::LoadingFiles) even runs — a
    /// package's source root is derived from its manifest, so there is nothing to walk
    /// until the manifest is known good — with one exception:
    /// [`PrivateModuleNotFound`](manifest::ManifestError::PrivateModuleNotFound) needs the
    /// package's parsed modules and is pushed after them.
    ///
    /// It carries no [`SourceFileId`] either way. A manifest error's location is a byte
    /// range in `zelkova.toml`, and that file is never read into the database, so each
    /// error names its manifest in its own message instead.
    Manifest(Vec<manifest::ManifestError>),
    /// The build could not be resolved: a dependency that could not be obtained or
    /// read, a circular package graph, or two modules answering to one name in one
    /// package.
    ///
    /// Like [`Manifest`](CompilationError::Manifest) it carries no
    /// [`SourceFileId`]: what each of these errors is about is a `zelkova.toml`, which
    /// is not a file the database holds, so each names its manifest in its own message.
    /// The graph half is raised before any source is loaded and goes back to the caller
    /// unrendered; the name-collision half is pushed onto `compile_package`'s
    /// accumulator once the packages' modules are known, and stops that package being
    /// compiled.
    Resolution(Vec<resolve::Error>),
    LoadingFiles(Vec<SourceFileError>),
    Source(parser::Error, SourceFileId),
    Canonical(Vec<canonical::Error>, Name),
    /// Type checking failed for the named module.
    Type(Vec<typer::Error>, Name),
    /// Exhaustiveness checking failed for the named module. Unreachable while
    /// `exhaustiveness::check` is a stub, but rendered like any other phase.
    Exhaustiveness(Vec<exhaustiveness::Error>, Name),
    DependenciesError(dependencies::Error),

    /// An error together with the file the module it belongs to was read from.
    ///
    /// A phase never knows its [`SourceFileId`], so the labels its errors produce are
    /// byte ranges with no file attached. This is where the two halves are put
    /// together, and it has exactly one constructor: [`compile_package`], which is
    /// the only place that knows which file a module was parsed from. Nothing else
    /// should build it — an id guessed anywhere else would underline the wrong file.
    InFile(Box<CompilationError>, SourceFileId),

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
        self.as_diagnostic_in(None)
    }

    /// The name of the module this error belongs to, when it has one.
    ///
    /// `compile_package` uses it to look up the file the module was read from, which
    /// is how an [`InFile`](CompilationError::InFile) wrapper gets its id.
    pub fn module(&self) -> Option<&Name> {
        match self {
            CompilationError::Canonical(_, module)
            | CompilationError::Type(_, module)
            | CompilationError::Exhaustiveness(_, module) => Some(module),
            CompilationError::InFile(inner, _) => inner.module(),
            _ => None,
        }
    }

    /// `as_diagnostic`, carrying the file the error's module was read from.
    ///
    /// `file` is `None` until an [`InFile`](CompilationError::InFile) wrapper supplies
    /// one, which only `compile_package` builds. Everything downstream of that
    /// distinction is in `phase_diagnostic`, where it serves as the fallback file for
    /// labels that do not name one themselves.
    fn as_diagnostic_in(&self, file: Option<SourceFileId>) -> Diagnostic<SourceFileId> {
        match self {
            // The one arm that changes `file`, and the only one that can: it is the
            // only variant that carries an id.
            CompilationError::InFile(inner, id) => inner.as_diagnostic_in(Some(*id)),
            // The one phase that carries spans renders its own labelled diagnostic.
            CompilationError::Source(err, file_id) => err.diagnostic(*file_id),
            // Only `PrivateModuleNotFound` ever reaches this arm: every other
            // `ManifestError` is raised before the file database exists and goes back to
            // the caller unrendered. That one is pushed after parsing, so a database does
            // exist here — but the location this error wants is a byte range in
            // `zelkova.toml`, which is not a file that database holds, so it renders with
            // no label and each error names itself and its manifest in a note.
            CompilationError::Manifest(errors) => Diagnostic::error()
                .with_message("Error in the package manifest")
                .with_notes(errors.iter().flat_map(|e| e.message_and_notes()).collect()),
            // A resolution error is about a manifest, the same as the arm above, and
            // renders the same way: no label, because `zelkova.toml` is not in the file
            // database. A lone error keeps its own headline — the collision message
            // names both the module and the package, and burying it under a summary
            // line would cost the user the one sentence that says what to change.
            CompilationError::Resolution(errors) => match errors.as_slice() {
                [only] => Diagnostic::error()
                    .with_message(only.message())
                    .with_notes(only.notes()),
                many => Diagnostic::error()
                    .with_message(format!(
                        "{} error{} while resolving the build",
                        many.len(),
                        if many.len() == 1 { "" } else { "s" }
                    ))
                    .with_notes(many.iter().flat_map(|e| e.message_and_notes()).collect()),
            },
            // Loading failures are not attached to a module — there is no module yet,
            // and each error names its own file instead.
            CompilationError::LoadingFiles(errors) => Diagnostic::error()
                .with_message("Error while loading the package files")
                .with_notes(errors.iter().flat_map(|e| e.message_and_notes()).collect()),
            CompilationError::Canonical(errors, module) => {
                phase_diagnostic(module, "canonical", errors, file)
            }
            CompilationError::Type(errors, module) => {
                phase_diagnostic(module, "type", errors, file)
            }
            CompilationError::Exhaustiveness(errors, module) => {
                phase_diagnostic(module, "exhaustiveness", errors, file)
            }
            // A dependency cycle belongs to the package, not to any one module, so it
            // does not go through `phase_diagnostic` — there is no module to supply
            // the fallback file that helper takes. Its labels don't need one: each
            // `CycleEdge` (`ERR-6`) carries the file the `import` forming it was
            // written in, so `spans_to_labels` is called with `None` and a label
            // with no file of its own (an edge `ModuleWalker::new` couldn't map back
            // to a file) is dropped rather than guessed at.
            CompilationError::DependenciesError(err) => Diagnostic::error()
                .with_message(err.message())
                .with_labels(spans_to_labels(err.labels(), None))
                .with_notes(err.notes()),
            // `compile_package` renders each accumulated error individually rather than
            // wrapping first, so this arm only fires when a `Many` is rendered as a
            // whole. It summarises rather than repeating what those diagnostics said.
            //
            // This is the one group that deliberately does not flatten its members'
            // labels. The phase-error groups that do — `canonical::Error::Many`,
            // `EnvironmentErrors` — hold errors from a single module, so their labels
            // all belong to one file and the group is the *only* thing rendered. This
            // one spans the whole package: its members are `InFile` wrappers naming
            // different files, and each has already been rendered with its own carets
            // by the time this summary is built. Flattening here would draw every
            // caret in the package a second time under one headline.
            CompilationError::Many(errors) => Diagnostic::error()
                .with_message(format!(
                    "compilation failed with {} error{}",
                    errors.len(),
                    if errors.len() == 1 { "" } else { "s" }
                ))
                .with_notes(
                    errors
                        .iter()
                        .map(|e| e.as_diagnostic_in(file).message)
                        .collect(),
                ),
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
// `docs/tickets/README.md`).

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

impl From<Vec<manifest::ManifestError>> for CompilationError {
    fn from(errors: Vec<manifest::ManifestError>) -> Self {
        CompilationError::Manifest(errors)
    }
}

impl From<Vec<resolve::Error>> for CompilationError {
    fn from(errors: Vec<resolve::Error>) -> Self {
        CompilationError::Resolution(errors)
    }
}

/// Which of the root package's source roots a build compiles.
///
/// A package has two, `src/` and `tests/`, and only the first is ever compiled for a
/// package that is being depended on: a dependency's `tests/` is not read, not resolved
/// and not observable ([*Tests*](../../docs/spec/packages.md#tests)). So this is a
/// property of the build rather than of a package, and it applies to the package the
/// compiler was pointed at.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TestRoot {
    /// `src/` alone.
    Skipped,
    /// Both roots. The modules under `tests/` are checked against the package's own
    /// modules — the private ones included — and the public modules of both dependency
    /// maps.
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
pub fn compile_package(package_dir: &Path) -> Result<(), CompilationError> {
    compile(package_dir, TestRoot::Skipped)
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
/// [its type](../../docs/spec/packages.md#what-a-test-is), and there is no runner.
pub fn compile_package_with_tests(package_dir: &Path) -> Result<(), CompilationError> {
    compile(package_dir, TestRoot::Compiled)
}

fn compile(package_dir: &Path, tests: TestRoot) -> Result<(), CompilationError> {
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

    // Step 1: read and validate the root package's manifest. This is the one failure
    // that cannot be deferred to the usual accumulate-and-render path, because it
    // happens before that path exists: the source root itself is derived from the
    // manifest, so there is no build to walk, no accumulator and no file database yet.
    // It goes back to the caller unrendered. Every failure after the accumulator below
    // is created goes onto it instead, this package's source loading included.
    debug!("phase: read package manifest");
    let manifest = manifest::load(package_dir)?;
    // The package the compiler was pointed at, which is the one whose `tests/` root
    // this build compiles — every other package in the build is here because something
    // depends on it, and a dependency's tests are never compiled.
    let root_package = manifest.name.clone();

    // Step 1b: resolve the build. Every package reachable from this one's two dependency
    // maps, each ordered after the packages it depends on, so an
    // `Interface` a package needs always exists by the time that package is
    // compiled. The union is resolved whether or not the tests are being compiled, so
    // that one version of each package and an acyclic graph are settled once for the
    // build. A dependency's own manifest is read here, so this is raised before any
    // source is loaded and goes back unrendered for the same reason the manifest above
    // does.
    debug!("phase: resolve the build");
    let build = resolve::resolve(package_dir, manifest)?;

    // Further steps will produce errors. We aggregate them here and report them at the
    // end of the compilation phase, rather than stopping on the first one, so that a
    // single broken module doesn't hide the diagnostics of every other module.
    //
    // They are kept as typed `CompilationError`s and not as already-rendered
    // `Diagnostic`s for two reasons: `as_diagnostic` stays the single rendering point,
    // and the accumulation is still meaningful as a return value — an empty vector is
    // what makes this function return `Ok`.
    let mut errors: Vec<CompilationError> = vec![];

    // One file database for the whole build, so that a diagnostic about any module of
    // any package renders against the file it was written in. `SourceFileId` is an
    // index into this one database, and it travels inside `Interface`s that outlive
    // the package they came from (`ERR-5`), which is exactly why there is one
    // database and not one per package.
    let mut sources = SourceFiles::new();

    // What each compiled package offers its dependents: its public modules'
    // `Interface`s, keyed by the module's name *within* that package. The spelling a
    // depending package reaches one by — `AcmeWidgets.Size`, or `Size` when it
    // unwraps — belongs to that package alone and is applied by `compile_in_build`.
    let mut published: HashMap<PackageName, HashMap<Name, Interface>> = HashMap::new();

    for package in &build {
        debug!("phase: compile package {}", package.name);

        let tests = if package.name == root_package {
            tests
        } else {
            TestRoot::Skipped
        };

        if let Some(public) = compile_in_build(
            package,
            &build,
            &published,
            tests,
            &mut sources,
            &mut errors,
            &mut print_status,
        ) {
            published.insert(package.name.clone(), public);
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

/// Compile one package of a resolved build, and hand back what it offers its
/// dependents.
///
/// `published` holds what every package compiled before this one offers — this
/// package's dependencies among them, since [`resolve::resolve`] orders a package
/// after everything it depends on. `sources` and `errors` are the build's, not this
/// package's: every module of every package renders against one file database, and
/// every error from any package makes the build fail.
///
/// `tests` says whether this package's `tests/` root is compiled beside its `src/`. It
/// is [`TestRoot::Compiled`] for the package the compiler was pointed at, when the
/// caller asked for tests, and for no other package in the build. The two roots are
/// walked, parsed and checked separately, because they are two environments: a module
/// under `tests/` sees the package's own modules, the private ones included, and the
/// public modules of both dependency maps, while a module under `src/` sees neither a
/// test module nor a `test-dependency`'s — an import naming one is a module that does
/// not exist, the same as a private module of another package.
///
/// `None` means nothing here was compiled and this package publishes nothing. It
/// always comes with at least one error already pushed onto `errors` — a dependency
/// that did not compile, sources that could not be read, a name claimed twice, or a
/// failure in one of this package's own modules — so a package that publishes nothing
/// can never be read as one that compiled. A package whose modules failed publishes
/// nothing for the same reason: its dependents would otherwise be checked against half
/// an interface and report errors belonging to a module they never wrote.
///
/// There is deliberately no error return. Every way this can fail is a diagnostic about
/// one package of a build whose other packages may already have accumulated diagnostics
/// of their own, and a `Result` here is an invitation to `?` those out of
/// [`compile_package`] past its reporting loop — which is the "nothing is rendered and
/// then dropped" the accumulator exists to prevent.
fn compile_in_build(
    package: &resolve::ResolvedPackage,
    build: &[resolve::ResolvedPackage],
    published: &HashMap<PackageName, HashMap<Name, Interface>>,
    tests: TestRoot,
    sources: &mut SourceFiles,
    errors: &mut Vec<CompilationError>,
    print_status: &mut impl FnMut(bool, String),
) -> Option<HashMap<Name, Interface>> {
    let errors_before = errors.len();

    // Step 1: what this package is compiled against. Only its *direct* dependencies:
    // a package listed in a dependency's manifest and not in this one's is in the
    // build and is not importable here
    // (`docs/spec/packages.md#only-direct-dependencies-are-usable`).
    //
    // `test-dependencies` join them when this package's tests are compiled, and are
    // resolved into the build whether or not they are
    // (`docs/spec/packages.md#test-dependencies`). What keeps one out of `src/` is not
    // that it is missing from the build but that its modules are held back from the
    // environment `src/` is checked against, a few steps below.
    //
    // Sorted so that a build reports two broken entries in the same order every run.
    let mut entries: Vec<(&PackageName, &manifest::Dependency)> =
        package.manifest.dependencies.iter().collect();
    if tests == TestRoot::Compiled {
        entries.extend(package.manifest.test_dependencies.iter());
    }
    entries.sort_by(|left, right| left.0.as_str().cmp(right.0.as_str()));

    let mut dependencies: Vec<resolve::DependencyModules<'_>> = Vec::new();
    for (name, entry) in entries {
        let resolved = build.iter().find(|candidate| &candidate.name == name);
        let public = published.get(name);

        match (resolved, public) {
            (Some(resolved), Some(public)) => dependencies.push(resolve::DependencyModules {
                package: resolved,
                wrapped: entry.wrapped,
                modules: public.keys().cloned().collect(),
            }),
            // The dependency is in the build — resolution succeeded, or we would not
            // be here — and did not compile. Its own diagnostics say why; this one
            // says which package was left uncompiled because of it, so a user reading
            // a wall of errors from a dependency knows why nothing was said about the
            // package they asked for.
            //
            // No status line goes with it. A status line reports a phase this package
            // got through; a package abandoned before its first phase has none, and the
            // diagnostic already says the same sentence in the same words.
            _ => {
                errors.push(CompilationError::Resolution(vec![
                    resolve::Error::DependencyNotCompiled {
                        package: package.name.clone(),
                        dependency: name.clone(),
                    },
                ]));
                return None;
            }
        }
    }

    // Step 2 and 3.a
    debug!("phase: load package sources");
    // A package whose sources cannot be read is not compiled, and nothing of it is
    // published — but the failure is accumulated like any other rather than returned,
    // because by the time we get here other packages of the build may already have
    // pushed diagnostics onto `errors` and returning would carry this one past the
    // reporting loop and drop theirs ("nothing is rendered and then dropped").
    //
    // A loading error names a path and has no span, so it renders against the shared
    // database whether or not this package contributed a single file to it.
    //
    // The two roots are walked separately, because a module's name is its path under
    // its own root. `tests/` is walked only for the package whose tests are being
    // compiled, and a package that holds no `tests/` directory at all is a package with
    // no tests rather than a failure — unlike `src/`, which every package has.
    let load_root = |root: source::SourceRoot, sources: &mut SourceFiles| {
        source::load_package_sources_into(&package.root, root, Some(&package.name), sources)
    };

    let src_ids = match load_root(source::SourceRoot::Src, sources) {
        Ok(file_ids) => file_ids,
        Err(error) => {
            errors.push(error);
            return None;
        }
    };
    let test_ids = match tests {
        TestRoot::Skipped => Vec::new(),
        TestRoot::Compiled => match load_root(source::SourceRoot::Tests, sources) {
            Ok(file_ids) => file_ids,
            Err(error) => {
                errors.push(error);
                return None;
            }
        },
    };

    // Step 3.b
    debug!("phase: parse package sources");
    let mut modules: Vec<parser::Module> = vec![];
    let mut test_modules: Vec<parser::Module> = vec![];
    // Which file each module was parsed from. This is the only point in the compiler
    // where both halves are in scope at once — a phase is handed a module and never
    // learns where it came from — so the mapping is recorded here and used below to
    // wrap the check errors in `InFile`, which is what lets their labels render.
    let mut module_files: HashMap<Name, SourceFileId> = HashMap::new();
    // Every module this package declares, with the file that declared it. A name two
    // files both answer to loses one of them in the map above, so the collision check
    // is given this list instead, in the order the roots were walked: `src/` first.
    let mut local_modules: Vec<resolve::LocalModule> = vec![];
    let mut parse_failures = 0;
    for (id, file) in sources
        .iter()
        .filter(|(id, _)| src_ids.contains(id) || test_ids.contains(id))
    {
        match parser::parse(file.file()) {
            Ok(module) => {
                module_files.insert(module.name.clone(), id);
                local_modules.push(resolve::LocalModule {
                    name: module.name.clone(),
                    file: file.package_path(),
                });
                if test_ids.contains(&id) {
                    test_modules.push(module);
                } else {
                    modules.push(module);
                }
            }
            Err(err) => {
                parse_failures += 1;
                errors.push(CompilationError::from(err, id));
            }
        }
    }

    let parsed = modules.len() + test_modules.len();
    if parse_failures == 0 {
        print_status(true, format!("parsed {} modules", parsed));
    } else {
        print_status(
            false,
            format!(
                "parsed {} modules, {} failed to parse",
                parsed, parse_failures
            ),
        );
    }

    // Step 3.c
    // TODO Verify modules name match file system.
    // TODO Include this into the parser::parse() function (w/ module name as argument) ?

    // `private-modules` can only be checked against the package's real modules once they
    // are parsed, which is why this lives here rather than inside `manifest::load` — every
    // other manifest error is known from the manifest text alone. What the list is *for*
    // is a few lines down: a module it names is kept out of what this package publishes.
    //
    // It is checked against `src/` alone. The list says what the package does not ship,
    // and a test module is not shipped by anything, so naming one there names a module
    // this package does not hold — the same answer whether or not the tests are being
    // compiled.
    //
    // A file that failed to parse contributes no module, so with any parse failure the list
    // of modules the package holds is known to be short. Checking against it then blames the
    // manifest for the parser's failure, and the parse error is already on its way to the
    // user, so the check is skipped entirely rather than run on a list it cannot trust.
    if parse_failures == 0 {
        let held_modules: std::collections::HashSet<&Name> =
            modules.iter().map(|m| &m.name).collect();
        let missing_private_modules: Vec<manifest::ManifestError> = package
            .manifest
            .private_modules
            .iter()
            .filter(|name| !held_modules.contains(name))
            .cloned()
            .map(|name| manifest::ManifestError::PrivateModuleNotFound {
                manifest_path: package.manifest_path(),
                name,
            })
            .collect();
        if !missing_private_modules.is_empty() {
            errors.push(CompilationError::Manifest(missing_private_modules));
        }
    }

    // Step 3.d: the whole map of module names this package can import, built before
    // anything is canonicalized. A name two modules both answer to is the manifest's
    // doing, not any one file's, so it is reported here and no module of the package is
    // canonicalized, type checked or checked for exhaustiveness
    // (`docs/spec/packages.md#two-modules-under-one-name-is-an-error`).
    //
    // The files have been read and parsed by this point, because `local_modules` is
    // taken from the parsed module headers. `SourceFile` derives a module name from the
    // relative path alone, so this could run one step earlier, against the walk; the
    // reason it does not is that the parsed header is what every other phase calls a
    // module by.
    //
    // Both roots are in that list, because they share one set of names: `src/Model.zel`
    // and `tests/Model.zel` are both `Model`, and that is the same collision as two
    // modules of one root answering to one name
    // (`docs/spec/packages.md#source-roots`).
    let visible = match resolve::visible_modules(package, &local_modules, &dependencies) {
        Ok(visible) => visible,
        // As with the uncompiled-dependency arm above, the diagnostic is the whole
        // report: a second, shorter copy on the status line said nothing it does not.
        Err(collisions) => {
            errors.push(CompilationError::Resolution(collisions));
            return None;
        }
    };

    // Each dependency's public modules enter the environment under the spelling this
    // package names them by, and under no other: a wrapped dependency's `Size` is
    // `AcmeWidgets.Size` here and nothing else, an unwrapped one's is `Size` and
    // nothing else. The `Interface` is the same either way — it carries the module's
    // own name, never the spelling — which is what keeps two packages that spell one
    // dependency differently agreeing about every type in it.
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    // A `test-dependency`'s modules are available to `tests/` and to nothing else, so
    // they are held back here and added to the environment for the tests pass alone. A
    // module of `src/` naming one finds no module of that name.
    let mut test_interfaces: HashMap<Name, Interface> = HashMap::new();
    let test_only: std::collections::HashSet<&PackageName> = match tests {
        TestRoot::Skipped => std::collections::HashSet::new(),
        TestRoot::Compiled => package.manifest.test_dependencies.keys().collect(),
    };

    for (spelling, origin) in &visible {
        if origin.package == package.name {
            // This package's own modules are inserted by `check_in_order` as each one
            // is checked, under the name it declares.
            continue;
        }

        if let Some(interface) = published
            .get(&origin.package)
            .and_then(|modules| modules.get(&origin.module))
        {
            if test_only.contains(&origin.package) {
                test_interfaces.insert(spelling.clone(), interface.clone());
            } else {
                interfaces.insert(spelling.clone(), interface.clone());
            }
        }
    }

    // Whether this package declares one of the eight default imports, and so receives
    // none of them. It is a question about the package, and each root holds only part
    // of one, so it is asked here — over both — rather than by each root's walker.
    let package_declares_a_default = default_imports::declares_a_default(
        modules.iter().chain(test_modules.iter()).map(|m| &m.name),
    );

    // Steps 4 and 5, once per source root. Two passes rather than one walk over both,
    // because the two roots are two environments: `src/` is checked knowing nothing of
    // `tests/`, and `tests/` is checked against everything `src/` published to this
    // package — the private modules included, since `interfaces` holds every module of
    // the package and the `private-modules` filter applies only to what is published.
    let mut roots: Vec<(source::SourceRoot, &[parser::Module])> =
        vec![(source::SourceRoot::Src, &modules)];
    if tests == TestRoot::Compiled {
        roots.push((source::SourceRoot::Tests, &test_modules));
    }

    for (root, root_modules) in roots {
        if root == source::SourceRoot::Tests {
            // A package whose own modules did not check cannot say anything true about
            // its tests: every one of them would be blamed for a type the package never
            // managed to declare.
            if errors.len() != errors_before {
                break;
            }

            interfaces.extend(std::mem::take(&mut test_interfaces));
        }

        debug!("phase: Build module dependency graph ({})", root);
        // A cycle leaves us with no order to check the modules in, so the check phase is
        // skipped — but the error goes through the same reporting path as the others
        // instead of returning early unrendered.
        let walker = match dependencies::ModuleWalker::new_for_root(
            root_modules,
            &module_files,
            package_declares_a_default,
        ) {
            Ok(walker) => Some(walker),
            Err(err) => {
                errors.push(err.into());
                None
            }
        };

        debug!("phase: Check modules ({})", root);

        // Step 5: Follow graph and call check_module on each
        if let Some(walker) = walker {
            // `check_in_order` checks every module regardless of earlier failures and
            // hands back both halves: the modules that checked, and the errors from the
            // ones that didn't (see `docs/tickets/README.md`, `BUG-2`). Both are reported
            // here, and the errors still flow into `errors` below so a failing module
            // keeps making this function return `Err` — only the previously-discarded
            // successes are new.
            //
            // `module_files` is also how each checked module's `Interface` learns which
            // file it came from (`Interface::file`, `ERR-5`): this is the one place that
            // knows both the module and its file, so `check_in_order` takes the map and
            // stamps it onto every interface it inserts as it goes.
            let (can_mods, check_errors) =
                walker.check_in_order(&package.name, &mut interfaces, &module_files, check_module);

            let what = match root {
                source::SourceRoot::Src => "modules",
                source::SourceRoot::Tests => "test modules",
            };

            if check_errors.is_empty() {
                print_status(
                    true,
                    format!(
                        "checked {}: {:#?}",
                        what,
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
                        "checked {}: {:#?} ({} failed to check)",
                        what,
                        can_mods
                            .iter()
                            .map(|m| m.name.as_human_string())
                            .collect::<Vec<_>>(),
                        check_errors.len()
                    ),
                );
                // Tag each error with the file its module was read from, so the spans its
                // phase produced have something to point into. A module with no entry —
                // there is none today, since only a module that parsed can be checked —
                // stays unwrapped and renders exactly as it did before spans existed.
                errors.extend(check_errors.into_iter().map(|error| {
                    match error.module().and_then(|name| module_files.get(name)) {
                        Some(id) => CompilationError::InFile(Box::new(error), *id),
                        None => error,
                    }
                }));
            }
        }
    }

    if errors.len() != errors_before {
        return None;
    }

    // Step 6: what this package offers whoever depends on it. Every module of `src/`
    // except the ones `private-modules` names and every `module foreign` facade, which
    // is package-internal by its own declaration whatever the manifest says
    // (`docs/spec/packages.md#what-a-package-exposes`, `docs/spec/interop.md`). A name
    // that would have reached one of those is simply absent from a dependent's map, so
    // it fails there as a module that does not exist.
    let facades: std::collections::HashSet<&Name> = modules
        .iter()
        .filter(|module| module.binding_foreign)
        .map(|module| &module.name)
        .collect();
    let private: std::collections::HashSet<&Name> =
        package.manifest.private_modules.iter().collect();
    // A module of `tests/` is not part of what the package ships, and nothing outside
    // the package can observe that it exists at all.
    let test_module_names: std::collections::HashSet<&Name> =
        test_modules.iter().map(|module| &module.name).collect();

    let public = interfaces
        .into_iter()
        // The map still holds the dependencies' interfaces seeded above; a package
        // publishes its own modules and nothing else, which is what stops it
        // re-exporting a dependency's module as one of its own
        // (`docs/spec/packages.md#what-a-package-boundary-cannot-rename`).
        .filter(|(name, interface)| {
            interface.module_name.package == package.name
                && !facades.contains(name)
                && !private.contains(name)
                && !test_module_names.contains(name)
        })
        .collect();

    Some(public)
}

/// Take a parsed module file within the ecosystem and apply all checks to it
///
/// TODO canonicalization must happens before checkings, because type check (at least)
/// will require access to other modules canonical representation.
/// That probably mean moving the `canonical::canonicalize` call out of this function
///
/// `package_declares_a_default` says whether the package `source` belongs to
/// declares one of the eight [default
/// imports](default_imports) — see [`default_imports::declares_a_default`].
/// `dependencies::ModuleWalker` computes it once, from the same module list it
/// builds the import graph from, and hands it to every module it checks through
/// this same parameter, which is why `check` in
/// [`ModuleWalker::check_in_order`](dependencies::ModuleWalker::check_in_order)
/// carries it too: `check_module` is normally reached only as that `fn` pointer.
pub fn check_module(
    package: &PackageName,
    interfaces: &HashMap<Name, Interface>,
    source: &parser::Module,
    package_declares_a_default: bool,
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
    let canonical =
        canonical::canonicalize(package, interfaces, source, package_declares_a_default)
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

    /// Every clause of the package-name rule, accepted and rejected side by side.
    ///
    /// The hyphen half is the reason this table exists: a name is read left to right and
    /// each hyphen has to be followed by an ASCII lowercase letter, which is what keeps
    /// `acme-widgets` from colliding with a package called `acme` holding a `widgets`
    /// something. `Not_Legal`, the only rejection the pipeline tests reach, fails on its
    /// first character and never enters the loop at all.
    ///
    /// Mutation-checked by dropping the `is_ascii_lowercase()` guard on the character
    /// after a hyphen (`a-`, `a--b` and `a-1` go green) and, separately, by starting the
    /// first-character check from `is_ascii_alphanumeric()` (`1a` goes green).
    #[test]
    fn a_package_name_is_lowercase_ascii_with_hyphens_between_letters() {
        let cases = [
            ("a", true),
            ("core", true),
            ("zelkova-core", true),
            ("a-b-c", true),
            ("html2", true),
            ("a1-b2", true),
            ("", false),
            ("-a", false),
            ("a-", false),
            ("a--b", false),
            ("a-1", false),
            ("1a", false),
            ("Core", false),
            ("Not_Legal", false),
            ("with space", false),
            ("zelkøva", false),
        ];

        for (name, legal) in cases {
            assert_eq!(
                is_legal_package_name(name),
                legal,
                "{:?} should {} a legal package name",
                name,
                if legal { "be" } else { "not be" }
            );
        }
    }

    /// Canonicalization failures are errors, and used to be rendered as warnings.
    ///
    /// The severity is part of how a failure reaches the user, so it is pinned
    /// here rather than left to the eye. Mutation-checked by putting
    /// `Diagnostic::warning()` back in the `Canonical` arm of `as_diagnostic`.
    #[test]
    fn canonical_errors_render_as_errors() {
        let error = CompilationError::Canonical(
            vec![canonical::Error::NoBindings(position::NodeSpan::none())],
            "Test".into(),
        );

        assert_eq!(error.as_diagnostic().severity, Severity::Error);
    }

    /// Same as above for dependency errors — a module cycle is not a warning.
    ///
    /// Mutation-checked by putting `Diagnostic::warning()` back in the
    /// `DependenciesError` arm of `as_diagnostic`.
    #[test]
    fn dependency_errors_render_as_errors() {
        let error = CompilationError::DependenciesError(dependencies::Error::CycleDetected(vec![
            dependencies::Cycle {
                path: vec!["A".into(), "B".into()],
                others: vec![],
                edges: vec![],
            },
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
            dependencies::Cycle {
                path: vec!["A".into(), "B".into()],
                others: vec![],
                edges: vec![],
            },
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
                canonical::Error::NoBindings(position::NodeSpan::none()),
                canonical::Error::TypeDeclared("Shape".into(), position::NodeSpan::none()),
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
