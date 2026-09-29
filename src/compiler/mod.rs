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
//!    ordered after the packages it depends on. Steps 2 to 6 then run once per package —
//!    bar the ones only `test-dependencies` reaches, which a build that did not ask for
//!    the tests resolves and leaves uncompiled — over the two source roots: `src/` and,
//!    for the root package when its tests were asked for, `tests/`, beside its own
//!    manifest. The root's `tests/` is compiled last, after the packages only
//!    `test-dependencies` reach, since one of those may depend on the root's `src/`.
//!    Before a package's modules are checked, the whole map of module names it can
//!    import is built — its own, plus each
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
//!     1. check each module (type check, exhaustiveness, etc…), and build the `ir::Module`
//!        a backend reads out of the canonical module and what the typer solved
//!     1. Bonus point to parallelize the tree branches which are not dependent on each others
//! 6. Once every module of every package has checked, emit each one as JavaScript
//!    (`javascript::emit`) and, if that failed nowhere either, write the build to
//!    `build/out/js/` (`output::write`). A build with any error writes nothing. A build that
//!    also compiled the tests (step 1's exception) writes a second, complete tree at
//!    `build/test/js/` — the runtime, then one directory per package, holding every
//!    package of the build (a test-only one included) and the root's `tests/` modules
//!    beside its `src/` ones — so that tree alone is what a test runner ever reads from.
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
use std::path::{Path, PathBuf};

pub mod canonical;
// Public because it is a rule about the language rather than an implementation detail of
// one phase, and because two phases read it: canonicalization synthesises the imports, and
// `dependencies` puts the matching edges in the import graph.
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
// Public because it is the compiler's hand-off to a backend and `check_module` returns one.
pub mod ir;
// Public so that its tests reach `emit` directly, on a module no package holds.
pub mod javascript;
// Public for the same reason as `source` and `dependencies` — `manifest::ManifestError` is
// reachable from the public `CompilationError::Manifest`.
pub mod manifest;
pub mod name;
// Public for the same reason as `manifest`: `output::Error` is reachable from the public
// `CompilationError::Output`.
pub mod output;
pub mod parser;
pub mod position;
// Public for the same reason as `manifest`: `program::Error` is reachable from the public
// `CompilationError::Program`.
pub mod program;
// Public for the same reason as `manifest`: `resolve::Error` is reachable from the public
// `CompilationError::Resolution`.
pub mod resolve;
// Public for the same reason as `default_imports`: it is a rule about the language rather
// than an implementation detail of one phase.
pub mod scalars;
pub mod source;
// Public because `test_runner`, and `zelkova test` through it, is its caller, the way
// `scalars` and `default_imports` are public for the phase that reads them.
pub mod test_collection;
// Public because `CompilationError::ProgramRun` carries its `Error`, and `zelkova run` calls `run`.
pub mod program_runner;
// Public because `CompilationError::TestRun` carries its `Error`, and `zelkova test` calls `run`.
pub mod test_runner;
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

    /// `zelkova-core`, [`resolve::CORE_PACKAGE`]: the one package the compiler names
    /// on its own, because it is where the [scalars] are declared.
    ///
    /// Built without going through [`PackageName::new`]'s check, so that naming a scalar
    /// needs no `unwrap`; that the constant passes the check is a unit test.
    pub fn core() -> PackageName {
        PackageName(resolve::CORE_PACKAGE.to_string())
    }

    /// `zelkova-test`, [`test_collection::TEST_PACKAGE`]: the package that declares
    /// `Test`, the type [*what a test
    /// is*](../../docs/spec/packages.md#what-a-test-is) finds a value's test-ness by.
    ///
    /// Built the same way [`PackageName::core`] is, without going through
    /// [`PackageName::new`]'s check, so that naming it needs no `unwrap`; that the
    /// constant passes the check is a unit test.
    pub fn test_package() -> PackageName {
        PackageName(test_collection::TEST_PACKAGE.to_string())
    }

    /// Whether this is `zelkova-core`, the one package the default imports and the
    /// scalar seeding they are replaced by both key themselves on
    /// ([`DEC-17`](../../docs/decisions/dec-17.md) decision 4) — and, since
    /// [`resolve::visible_modules`] rejects any other package that declares one of
    /// the eight, the only package that can be shaped like it.
    pub fn is_core(&self) -> bool {
        self.0 == resolve::CORE_PACKAGE
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

    /// The package that declares this module.
    pub fn package(&self) -> &PackageName {
        &self.package
    }

    /// The name `name`, as declared by this module of this package.
    pub fn qualify_name(&self, name: &Name) -> QualName {
        QualName::in_module(self.package.clone(), self.name.as_str(), name.as_str())
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
    /// How many parameters each value in [`values`](Self::values) and
    /// [`infix_functions`](Self::infix_functions) is emitted with, keyed the same way:
    /// [`canonical::Module::emitted_arity`].
    ///
    /// An importer reads it for the same reason the module declaring the value reads its
    /// own declarations' arities: a call supplying that many arguments is a direct call,
    /// and a partial application, or a use as a value of one taking two or more, goes
    /// through the runtime's `$curry`
    /// ([`DEC-18` decision
    /// 3](../../docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)).
    /// The typer's translation is where it is read, into [`ir::ReferenceKind::Foreign`].
    ///
    /// [`canonical::Module::to_interface`] records one for every value either map holds.
    /// A value missing from it — which only a hand-built interface in a test can leave
    /// out — is read as arity 0, the arity of a parameterless binding.
    pub arities: HashMap<Name, usize>,
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
/// A [`codespan_reporting::diagnostic::Label`] needs a file id as well as a byte range. Most
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
/// `import`'s `exposing` list, say — has nowhere to point, and renders as message-plus-notes
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
    /// package's parsed modules and is pushed after them, and
    /// [`MainModuleNotFound`](manifest::ManifestError::MainModuleNotFound) needs its checked
    /// ones and is pushed after those.
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
    /// The named module is its package's `main`, checked, and is not fit to be a
    /// program's entry point ([`program`]).
    Program(Vec<program::Error>, Name),
    DependenciesError(dependencies::Error),
    /// The named module checked and could not be emitted as JavaScript.
    Emit(Vec<javascript::Error>, Name),
    /// A file of the build's output could not be written. Raised only once every module
    /// of the build has checked and emitted, since nothing is written before that.
    Output(output::Error),
    /// A package's tests were compiled and could not be run: `zelkova test` could not
    /// write its entry point or could not run `node`. A test that ran and did not pass is
    /// not this; it is the exit code of the run.
    TestRun(test_runner::Error),
    /// A package's program could not be run: `zelkova run` was pointed at a package with no
    /// `main`, or could not write its entry point or could not run `node`. A program that
    /// ran and aborted is not this; it is the exit code of the run.
    ProgramRun(program_runner::Error),

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
            | CompilationError::Exhaustiveness(_, module)
            | CompilationError::Program(_, module)
            | CompilationError::Emit(_, module) => Some(module),
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
            // Only `PrivateModuleNotFound` and `MainModuleNotFound` ever reach this arm:
            // every other `ManifestError` is raised before the file database exists and
            // goes back to the caller unrendered. Those two are pushed after parsing, so a
            // database does exist here — but the location they want is a byte range in
            // `zelkova.toml`, which is not a file that database holds, so they render with
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
            CompilationError::Program(errors, module) => {
                phase_diagnostic(module, "program", errors, file)
            }
            CompilationError::Emit(errors, module) => {
                phase_diagnostic(module, "code generation", errors, file)
            }
            // A path on disk, not a place in any source, so there is nothing to label.
            CompilationError::Output(error) => Diagnostic::error()
                .with_message(error.message())
                .with_notes(error.notes()),
            // Like `Output`, about the machine and not about any source.
            CompilationError::TestRun(error) => Diagnostic::error()
                .with_message(error.message())
                .with_notes(error.notes()),
            CompilationError::ProgramRun(error) => Diagnostic::error()
                .with_message(error.message())
                .with_notes(error.notes()),
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
///
/// A build that checks writes its JavaScript to `build/out/js/` beside `package_dir`'s
/// manifest — see [`compile_package_into`] for what that tree holds. One that emitted
/// any error writes nothing.
pub fn compile_package(package_dir: &Path) -> Result<(), CompilationError> {
    compile(
        package_dir,
        TestRoot::Skipped,
        &package_dir.join(BUILD_DIRECTORY),
    )
    .map(|_| ())
}

/// The directory a build's output goes to, beside the root package's manifest and never
/// beside a source it read ([*The compiler's
/// interface*](../../docs/spec/toolchain.md#the-compilers-interface)).
pub const BUILD_DIRECTORY: &str = "build";

/// The tree a build that compiled the tests writes below `build_dir`: `build_dir/test/js/`,
/// laid out like `build_dir/out/js/`. Both the write of that tree and the entry point
/// [`test_runner::run`] puts in it name it through here.
pub(crate) fn test_tree(build_dir: &Path) -> PathBuf {
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
    compile(package_dir, TestRoot::Skipped, build_dir).map(|_| ())
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
/// [`test_runner::run`]'s. What it hands back on success is the `Interface` of each of
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
    compile(package_dir, TestRoot::Compiled, build_dir)
}

fn compile(
    package_dir: &Path,
    tests: TestRoot,
    build_dir: &Path,
) -> Result<Vec<Interface>, CompilationError> {
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

    // Every package this build reaches only through `test-dependencies` — never through
    // the plain `dependencies` graph. None of them is compiled by the first loop below.
    //
    // A build that did not ask for the tests has no use for a `test-dependency` at all:
    // no module here can import one, since a `test-dependency`'s modules are held out of
    // the environment `src/` is checked against. Compiling it anyway would parse and
    // check a package the user never reached for, print its status lines, and fail this
    // build on an error inside it. One that did ask for the tests compiles it in the
    // second loop — the tests need its interface — but does not write it.
    let test_only = resolve::test_only_packages(&build, &root_package);

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

    // Every module that checked, from every package that belongs in the build's output,
    // held until the whole build is known to have checked: a module cannot be written
    // while another may still fail. A `test-dependency`-only package is compiled when
    // the tests ask for it, but its modules never land here — only the first loop below
    // extends it.
    let mut checked: Vec<ModuleToEmit> = Vec::new();

    // Every module the *test* tree (`build/test/js/`) needs beyond what `checked` already
    // holds: each test-only package's modules, and the root's `tests/` modules. Stays
    // empty — and unread — for a build that did not ask for the tests, since the codegen
    // step below only reaches for it when `tests == TestRoot::Compiled`.
    let mut test_tree_modules: Vec<ModuleToEmit> = Vec::new();

    // What `compile_package_with_tests` hands back: the `Interface` of each of the
    // root's own `tests/` modules that checked — never a test-only package's, and
    // never `src/`'s. This is what lets a caller find a package's tests
    // (`test_collection::collect` is that pass) without a phase dropping the checked
    // modules on the floor once they have been emitted. Stays empty for a build that
    // did not ask for the tests, or whose root `tests/` did not check.
    let mut root_test_interfaces: Vec<Interface> = Vec::new();

    // The root package's `tests/` is compiled apart from its `src/`, after every
    // test-only package, because a test-only package may depend on the root: that edge
    // names the root's `src/`, and the root's `tests/` in turn needs the test-only
    // package ([*`test-dependencies`*](../../docs/spec/packages.md#test-dependencies)).
    // So the build runs in three steps: every package the plain graph reaches, the root's
    // `src/` last among them; then each test-only package, which sees the root through
    // `published` like any other dependency; then the root's `tests/`, against what its
    // `src/` left here.
    let mut root_tests: Option<(&resolve::ResolvedPackage, TestsEnvironment)> = None;

    for package in &build {
        if test_only.contains(&package.name) {
            debug!(
                "phase: defer package {} — reached through `test-dependencies` alone",
                package.name
            );
            continue;
        }

        debug!("phase: compile package {}", package.name);

        let tests = if package.name == root_package {
            tests
        } else {
            TestRoot::Skipped
        };

        if let Some(compiled) = compile_in_build(
            package,
            &build,
            &published,
            tests,
            &mut sources,
            &mut errors,
            &mut print_status,
        ) {
            published.insert(package.name.clone(), compiled.public);
            checked.extend(compiled.modules);
            if let Some(environment) = compiled.tests {
                root_tests = Some((package, environment));
            }
        }
    }

    // `None` unless the tests were asked for and the root's `src/` checked, and in the
    // second case the failure is already reported: its tests would only be blamed for a
    // type the package never managed to declare. A test-only package is in the build for
    // the tests alone, so it is not compiled either — one that depends on the root would
    // only add a `DependencyNotCompiled` saying what the root's own errors already say.
    if let Some((root, environment)) = root_tests {
        for package in build.iter().filter(|p| test_only.contains(&p.name)) {
            debug!("phase: compile package {} for the tests", package.name);

            // Compiled and published so the tests can be checked against it. Nothing
            // outside a package's own tests reads its modules, so they never join
            // `checked` — the plain build's output tree — but they do join
            // `test_tree_modules`, which only the test tree reads.
            if let Some(compiled) = compile_in_build(
                package,
                &build,
                &published,
                TestRoot::Skipped,
                &mut sources,
                &mut errors,
                &mut print_status,
            ) {
                published.insert(package.name.clone(), compiled.public);
                test_tree_modules.extend(compiled.modules);
            }
        }

        debug!("phase: compile the tests of package {}", root.name);
        let mut root_tests_checked = compile_tests(
            root,
            &build,
            &published,
            environment,
            &mut errors,
            &mut print_status,
        );

        // A test companion imports the companion it checks by its path in the source
        // tree, across the two roots, which reaches nothing in the build
        // ([*Testing a companion*](../../docs/spec/interop.md#testing-a-companion)). Each
        // companion of the root's `src/` is one it may check, and `checked` holds every
        // one of them by now.
        let targets: Vec<&Name> = checked
            .iter()
            .filter(|to_emit| {
                to_emit.companion.is_some() && to_emit.module.canonical.name.package() == &root.name
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
        test_tree_modules.extend(root_tests_checked);
    }

    // Step 6: generate code, only for a build in which nothing failed. Every module of
    // both trees is emitted before anything is written, so a module that cannot be
    // emitted — in `src/` or in `tests/` — also leaves the build with no output at all.
    if errors.is_empty() {
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

/// Pair each checked module of one source root with what emitting and writing it needs:
/// the file it was read from, and its companion when it is a facade with one sitting
/// beside its source under `root_dir` — the source root's own directory, `src/` or
/// `tests/`, so a facade under `tests/` finds its companion there rather than under
/// `src/` ([*Testing a companion*](../../docs/spec/interop.md#testing-a-companion)).
///
/// Shared by [`compile_in_build`] (over `src/`) and [`compile_tests`] (over `tests/`),
/// which differ only in which root's directory and file map they pass.
fn to_modules_to_emit(
    checked: Vec<CheckedModule>,
    root_dir: &Path,
    module_files: &HashMap<Name, SourceFileId>,
) -> Vec<ModuleToEmit> {
    checked
        .into_iter()
        .map(|module| {
            let name = module.canonical.name.name();
            let companion = Some(root_dir.join(javascript::module_file(name)))
                .filter(|path| module.ir.foreign && path.is_file());
            ModuleToEmit {
                file: module_files.get(name).copied(),
                companion,
                companion_imports: Vec::new(),
                module,
            }
        })
        .collect()
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

/// What [`compile_in_build`] hands back for a package whose every module checked.
struct CompiledPackage {
    /// What it offers its dependents: its public modules' `Interface`s, keyed by each
    /// module's name within the package.
    public: HashMap<Name, Interface>,
    /// Every module of it that was compiled, to be emitted once the whole build is known
    /// to have checked.
    modules: Vec<ModuleToEmit>,
    /// What [`compile_tests`] needs to check this package's `tests/` later. `Some` only
    /// when [`compile_in_build`] was handed [`TestRoot::Compiled`].
    tests: Option<TestsEnvironment>,
}

/// What [`compile_in_build`] leaves behind for a package's `tests/`, which is parsed
/// alongside `src/` and checked in a later step of the build ([`compile_tests`]) once
/// every test-only package is compiled.
struct TestsEnvironment {
    /// Every interface `src/` was checked against or produced: the package's own modules,
    /// the private ones and the facades included, and its `dependencies`' public modules
    /// under the spelling this package names them by. A test module sees all of it.
    interfaces: HashMap<Name, Interface>,
    /// The modules of `tests/`, as the parser left them.
    modules: Vec<parser::Module>,
    /// The modules both roots declare, `src/` first, each with its file. They were
    /// already checked against each other and against the plain dependencies; the tests'
    /// own collision check runs them again with the `test-dependencies` added.
    local_modules: Vec<resolve::LocalModule>,
    /// The file each module of either root was parsed from.
    module_files: HashMap<Name, SourceFileId>,
}

/// Compile the `src/` root of one package of a resolved build, and hand back what it
/// offers its dependents and the modules it compiled.
///
/// `published` holds what every package compiled before this one offers — this
/// package's dependencies among them, since `compile` calls this for a package only
/// once everything it depends on has had its turn. `sources` and `errors` are the
/// build's, not this package's: every module of every package renders against one file
/// database, and every error from any package makes the build fail.
///
/// `tests` says whether this package's `tests/` root is compiled later in the build. It
/// is [`TestRoot::Compiled`] for the package the compiler was pointed at, when the
/// caller asked for tests, and for no other package in the build. With
/// [`TestRoot::Compiled`] this function reads and parses `tests/`, so that a module name
/// the two roots share is reported before `src/` is checked, but checks none of it: it
/// hands back the parsed modules in the [`TestsEnvironment`] that [`compile_tests`]
/// checks them against. A test module that fails to parse fails this package, the same
/// as one of `src/` would. The two roots are
/// two environments: a module under `src/` sees neither a test module nor a
/// `test-dependency`'s — an import naming one is a module that does not exist, the same
/// as a private module of another package.
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
) -> Option<CompiledPackage> {
    let errors_before = errors.len();

    // Step 1: what this package is compiled against. Only its *direct* dependencies:
    // a package listed in a dependency's manifest and not in this one's is in the
    // build and is not importable here
    // (`docs/spec/packages.md#only-direct-dependencies-are-usable`).
    //
    // `test-dependencies` are not among them. They are resolved into the build whether or
    // not the tests are compiled (`docs/spec/packages.md#test-dependencies`), and join
    // the environment in `compile_tests`, which checks `tests/` alone.
    let dependencies = direct_dependencies(
        package,
        package.manifest.dependencies.iter(),
        build,
        published,
        errors,
    )?;

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
    let src_ids = match source::load_package_sources_into(
        &package.root,
        source::SourceRoot::Src,
        Some(&package.name),
        sources,
    ) {
        Ok(file_ids) => file_ids,
        Err(error) => {
            errors.push(error);
            return None;
        }
    };

    // Step 3.b
    debug!("phase: parse package sources");
    let parsed = parse_root(&src_ids, sources, errors);
    print_parse_status(&parsed, "modules", print_status);

    // `tests/` is read and parsed here, ahead of anything in `src/` being checked, even
    // though it is checked last in the build (`compile_tests`): the two roots share one
    // set of module names, and a collision between them has to be reported before any
    // module of the package is compiled (step 3.d below). A package that holds no
    // `tests/` directory at all is a package with no tests rather than a failure —
    // unlike `src/`, which every package has.
    let parsed_tests = match tests {
        TestRoot::Skipped => None,
        TestRoot::Compiled => {
            debug!("phase: load package test sources");
            let test_ids = match source::load_package_sources_into(
                &package.root,
                source::SourceRoot::Tests,
                Some(&package.name),
                sources,
            ) {
                Ok(file_ids) => file_ids,
                Err(error) => {
                    errors.push(error);
                    return None;
                }
            };

            debug!("phase: parse package test sources");
            let parsed_tests = parse_root(&test_ids, sources, errors);
            print_parse_status(&parsed_tests, "test modules", print_status);
            Some(parsed_tests)
        }
    };

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
    if parsed.failures == 0 {
        let held_modules: std::collections::HashSet<&Name> =
            parsed.modules.iter().map(|m| &m.name).collect();
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
    // Whichever roots this build read are in the list, `src/` first — the order a
    // collision between the two reads best in — because the two share one set of names:
    // `src/Model.zel` and `tests/Model.zel` are both `Model`, and the chapter calls that
    // the same error as two modules of one root answering to one name
    // (`docs/spec/packages.md#source-roots`). A `tests/` root is read [when this
    // package's own tests are run and at no other
    // time](../../docs/spec/packages.md#tests), so an ordinary build has not read the
    // second file and has nothing to report.
    //
    // The dependencies here are the plain ones alone. A collision with a
    // `test-dependency`'s module is found by `compile_tests`, because a test-dependency
    // may depend on this package and so is compiled after it: what it publishes is not
    // known yet. That half is reported late, which the chapter records as a known gap
    // (`docs/tickets/bug-42.md`).
    let mut local_modules = parsed.local_modules.clone();
    if let Some(parsed_tests) = &parsed_tests {
        local_modules.extend(parsed_tests.local_modules.iter().cloned());
    }
    let visible = match resolve::visible_modules(package, &local_modules, &dependencies) {
        Ok(visible) => visible,
        // As with the uncompiled-dependency case in `direct_dependencies`, the diagnostic
        // is the whole report: a second, shorter copy on the status line said nothing it
        // does not.
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
    insert_dependency_interfaces(package, &visible, published, |_| true, &mut interfaces);

    // Whether this package is exempt from the default imports, and so receives none
    // of them, is `package.name.is_core()` — a property of the package's own name,
    // not of which modules either root happens to hold. So it cannot differ between
    // `src/` and `tests/`, or between a build that compiles the tests and one that
    // does not: both roots, and both walkers, are handed `&package.name` directly
    // rather than a flag computed once and threaded through.

    // Steps 4 and 5.
    let can_mods = check_root(
        package,
        source::SourceRoot::Src,
        &parsed.modules,
        &parsed.module_files,
        &mut interfaces,
        errors,
        print_status,
    );

    // A companion sits beside its facade's source, under the same path below the
    // source root that the facade's own emitted module has below its package's output
    // directory.
    //
    // Only `src/` feeds `checked` here — the plain build's output tree. A module of
    // `tests/` checks like any other but is not this package's, so it is not among
    // these: it joins the test tree's own modules later, in `compile_tests`.
    let root_dir = package.root.join(source::SourceRoot::Src.directory());
    let checked: Vec<ModuleToEmit> = to_modules_to_emit(can_mods, &root_dir, &parsed.module_files);

    if errors.len() != errors_before {
        return None;
    }

    // Step 5b: a program's entry point ([`program`]). It runs only once every module of
    // `src/` has checked, since the check reads the type inference solved for `main`, and
    // for every package of the build that declares `main` — the root and any dependency
    // alike; `program`'s documentation says why.
    //
    // A failure here fails the build, and the package is still published: its interfaces
    // are sound, and a dependent checked against them reports only its own errors.
    if let Some(main) = &package.manifest.main {
        check_main(package, main, &checked, errors);
    }

    // Step 6: what this package offers whoever depends on it. Every module of `src/`
    // except the ones `private-modules` names and every `module foreign` facade, which
    // is package-internal by its own declaration whatever the manifest says
    // (`docs/spec/packages.md#what-a-package-exposes`, `docs/spec/interop.md`). A name
    // that would have reached one of those is simply absent from a dependent's map, so
    // it fails there as a module that does not exist.
    let facades: std::collections::HashSet<&Name> = parsed
        .modules
        .iter()
        .filter(|module| module.binding_foreign)
        .map(|module| &module.name)
        .collect();
    let private: std::collections::HashSet<&Name> =
        package.manifest.private_modules.iter().collect();

    let public = interfaces
        .iter()
        // The map still holds the dependencies' interfaces seeded above; a package
        // publishes its own modules and nothing else, which is what stops it
        // re-exporting a dependency's module as one of its own
        // (`docs/spec/packages.md#what-a-package-boundary-cannot-rename`).
        .filter(|(name, interface)| {
            interface.module_name.package == package.name
                && !facades.contains(name)
                && !private.contains(name)
        })
        .map(|(name, interface)| (name.clone(), interface.clone()))
        .collect();

    // `tests/` is checked against everything `src/` was checked against or produced —
    // the private modules included, since `interfaces` holds every module of the
    // package and the `private-modules` filter applies only to what is published.
    let tests = parsed_tests.map(|parsed_tests| {
        let mut module_files = parsed.module_files;
        module_files.extend(parsed_tests.module_files);
        TestsEnvironment {
            interfaces,
            modules: parsed_tests.modules,
            local_modules,
            module_files,
        }
    });

    Some(CompiledPackage {
        public,
        modules: checked,
        tests,
    })
}

/// Check the module the manifest's `main` names among `checked`, the modules of
/// `package`'s `src/` that checked, and push what is wrong with it onto `errors`.
///
/// A name matching no module of `src/` is the manifest's error, not a module's, so it is a
/// [`ManifestError::MainModuleNotFound`](manifest::ManifestError::MainModuleNotFound) and
/// renders with no location. `checked` holds `src/` alone, which is what keeps a module
/// under `tests/` from counting. Anything wrong with the module it finds is a
/// [`CompilationError::Program`] carrying that module's file, so its labels point into it.
fn check_main(
    package: &resolve::ResolvedPackage,
    main: &Name,
    checked: &[ModuleToEmit],
    errors: &mut Vec<CompilationError>,
) {
    let Some(entry) = checked
        .iter()
        .find(|to_emit| to_emit.module.canonical.name.name() == main)
    else {
        errors.push(CompilationError::Manifest(vec![
            manifest::ManifestError::MainModuleNotFound {
                manifest_path: package.manifest_path(),
                name: main.clone(),
            },
        ]));
        return;
    };

    if let Err(error) = program::check(&entry.module) {
        let error = CompilationError::Program(vec![error], main.clone());
        errors.push(match entry.file {
            Some(file) => CompilationError::InFile(Box::new(error), file),
            None => error,
        });
    }
}

/// Check the `tests/` root of the package the compiler was pointed at, which
/// [`compile_in_build`] already parsed into `environment`, against what its `src/` left
/// there and the public modules of both its dependency maps.
///
/// `compile` calls this after the package's `src/` and after every test-only package,
/// because a test-only package may depend on this one
/// ([*`test-dependencies`*](../../docs/spec/packages.md#test-dependencies)): `published`
/// then holds every package `tests/` can name. Only for a package whose `src/` checked —
/// a package whose own modules did not check cannot say anything true about its tests,
/// since every one of them would be blamed for a type the package never managed to
/// declare.
///
/// Every `tests/` module that checked, paired with what emitting and writing it needs —
/// [`ModuleToEmit`], the same shape [`compile_in_build`] hands back for `src/` — so the
/// test tree ([`compile_package_with_tests`]) can write them beside it. Empty, with
/// nothing published or emitted, for a package whose collision check or dependency
/// resolution failed before any module was checked. A test module is checked like any
/// other and a failure in one fails the build, but nothing outside the package's own
/// tests reads it, so it is never published to a dependent.
fn compile_tests(
    package: &resolve::ResolvedPackage,
    build: &[resolve::ResolvedPackage],
    published: &HashMap<PackageName, HashMap<Name, Interface>>,
    environment: TestsEnvironment,
    errors: &mut Vec<CompilationError>,
    print_status: &mut impl FnMut(bool, String),
) -> Vec<ModuleToEmit> {
    let TestsEnvironment {
        mut interfaces,
        modules,
        local_modules,
        module_files,
    } = environment;

    // Both dependency maps, the plain one included: the collision check below is over
    // everything `tests/` can name, and a plain dependency's module can collide with a
    // `test-dependency`'s.
    let Some(dependencies) = direct_dependencies(
        package,
        package
            .manifest
            .dependencies
            .iter()
            .chain(package.manifest.test_dependencies.iter()),
        build,
        published,
        errors,
    ) else {
        return Vec::new();
    };

    // The map is built again over both roots and both dependency maps. `compile_in_build`
    // already reported every collision among the two roots and the plain dependencies,
    // before `src/` was checked, and stopped there if it found one; what this can still
    // find is a collision with a `test-dependency`'s module, which could not be seen
    // until that package was compiled.
    let visible = match resolve::visible_modules(package, &local_modules, &dependencies) {
        Ok(visible) => visible,
        Err(collisions) => {
            errors.push(CompilationError::Resolution(collisions));
            return Vec::new();
        }
    };

    // A `test-dependency`'s modules are available to `tests/` and to nothing else, so
    // they join the environment here and never in `compile_in_build`. The plain
    // dependencies' are already in it, from `src/`.
    let test_dependencies: std::collections::HashSet<&PackageName> =
        package.manifest.test_dependencies.keys().collect();
    insert_dependency_interfaces(
        package,
        &visible,
        published,
        |origin| test_dependencies.contains(origin),
        &mut interfaces,
    );

    let can_mods = check_root(
        package,
        source::SourceRoot::Tests,
        &modules,
        &module_files,
        &mut interfaces,
        errors,
        print_status,
    );

    // A facade under `tests/` places its companion the way any other facade does — beside
    // its own source — under `tests/` here rather than `src/`
    // (`docs/spec/interop.md#testing-a-companion`).
    let root_dir = package.root.join(source::SourceRoot::Tests.directory());
    to_modules_to_emit(can_mods, &root_dir, &module_files)
}

/// The direct dependencies named by `entries`, each as the package being compiled sees
/// it, sorted by name so that a build reports two broken entries in the same order
/// every run.
///
/// `None` when one of them is in the build and did not compile — resolution succeeded,
/// or we would not be here. Its own diagnostics say why; the
/// [`DependencyNotCompiled`](resolve::Error::DependencyNotCompiled) this pushes says
/// which package was left uncompiled because of it, so a user reading a wall of errors
/// from a dependency knows why nothing was said about the package they asked for.
///
/// No status line goes with it. A status line reports a phase this package got through;
/// a package abandoned before its first phase has none, and the diagnostic already says
/// the same sentence in the same words.
fn direct_dependencies<'b, 'e>(
    package: &resolve::ResolvedPackage,
    entries: impl Iterator<Item = (&'e PackageName, &'e manifest::Dependency)>,
    build: &'b [resolve::ResolvedPackage],
    published: &HashMap<PackageName, HashMap<Name, Interface>>,
    errors: &mut Vec<CompilationError>,
) -> Option<Vec<resolve::DependencyModules<'b>>> {
    let mut entries: Vec<(&PackageName, &manifest::Dependency)> = entries.collect();
    entries.sort_by(|left, right| left.0.as_str().cmp(right.0.as_str()));

    let mut dependencies: Vec<resolve::DependencyModules<'b>> = Vec::new();
    for (name, entry) in entries {
        let resolved = build.iter().find(|candidate| &candidate.name == name);
        let public = published.get(name);

        match (resolved, public) {
            (Some(resolved), Some(public)) => dependencies.push(resolve::DependencyModules {
                package: resolved,
                wrapped: entry.wrapped,
                modules: public.keys().cloned().collect(),
            }),
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

    Some(dependencies)
}

/// Add to `interfaces` every module of `visible` that belongs to a dependency whose name
/// `include` accepts, under the spelling `visible` keys it by.
///
/// This package's own modules are skipped: `check_root` inserts each one as it is
/// checked, under the name it declares.
fn insert_dependency_interfaces(
    package: &resolve::ResolvedPackage,
    visible: &HashMap<Name, resolve::ModuleOrigin>,
    published: &HashMap<PackageName, HashMap<Name, Interface>>,
    include: impl Fn(&PackageName) -> bool,
    interfaces: &mut HashMap<Name, Interface>,
) {
    for (spelling, origin) in visible {
        if origin.package == package.name || !include(&origin.package) {
            continue;
        }

        if let Some(interface) = published
            .get(&origin.package)
            .and_then(|modules| modules.get(&origin.module))
        {
            interfaces.insert(spelling.clone(), interface.clone());
        }
    }
}

/// The modules of one source root as the parser left them.
struct ParsedRoot {
    modules: Vec<parser::Module>,
    /// Which file each module was parsed from. This is the only point in the compiler
    /// where both halves are in scope at once — a phase is handed a module and never
    /// learns where it came from — so the mapping is recorded here and used by
    /// `check_root` to wrap the check errors in `InFile`, which is what lets their labels
    /// render.
    module_files: HashMap<Name, SourceFileId>,
    /// Every module the root declares, with the file that declared it. A name two files
    /// both answer to loses one of them in `module_files`, so the collision check is
    /// given this list instead.
    local_modules: Vec<resolve::LocalModule>,
    /// How many files failed to parse, each already reported.
    failures: usize,
}

/// Parse every file of `ids`, pushing a parse error onto `errors` for each that fails.
fn parse_root(
    ids: &[SourceFileId],
    sources: &SourceFiles,
    errors: &mut Vec<CompilationError>,
) -> ParsedRoot {
    let mut parsed = ParsedRoot {
        modules: vec![],
        module_files: HashMap::new(),
        local_modules: vec![],
        failures: 0,
    };

    for (id, file) in sources.iter().filter(|(id, _)| ids.contains(id)) {
        match parser::parse(file.file()) {
            Ok(module) => {
                parsed.module_files.insert(module.name.clone(), id);
                parsed.local_modules.push(resolve::LocalModule {
                    name: module.name.clone(),
                    file: file.package_path(),
                });
                parsed.modules.push(module);
            }
            Err(err) => {
                parsed.failures += 1;
                errors.push(CompilationError::from(err, id));
            }
        }
    }

    parsed
}

/// The status line for one root's parse: `parsed 8 modules`, or how many failed.
fn print_parse_status(
    parsed: &ParsedRoot,
    what: &str,
    print_status: &mut impl FnMut(bool, String),
) {
    let count = parsed.modules.len();
    if parsed.failures == 0 {
        print_status(true, format!("parsed {} {}", count, what));
    } else {
        print_status(
            false,
            format!(
                "parsed {} {}, {} failed to parse",
                count, what, parsed.failures
            ),
        );
    }
}

/// Order the modules of one source root by their imports and check each one, against
/// `interfaces` and every module of the root checked before it.
///
/// Every module that checked is inserted into `interfaces` and handed back; every error
/// goes onto `errors`, tagged with the file its module was read from.
fn check_root(
    package: &resolve::ResolvedPackage,
    root: source::SourceRoot,
    modules: &[parser::Module],
    module_files: &HashMap<Name, SourceFileId>,
    interfaces: &mut HashMap<Name, Interface>,
    errors: &mut Vec<CompilationError>,
    print_status: &mut impl FnMut(bool, String),
) -> Vec<CheckedModule> {
    debug!("phase: Build module dependency graph ({})", root);
    // A cycle leaves us with no order to check the modules in, so the check phase is
    // skipped — but the error goes through the same reporting path as the others
    // instead of returning early unrendered.
    let walker =
        match dependencies::ModuleWalker::new_for_root(modules, module_files, &package.name) {
            Ok(walker) => walker,
            Err(err) => {
                errors.push(err.into());
                return Vec::new();
            }
        };

    debug!("phase: Check modules ({})", root);

    // Step 5: Follow graph and call check_module on each
    //
    // `check_in_order` checks every module regardless of earlier failures and hands back
    // both halves: the modules that checked, and the errors from the ones that didn't
    // (see `docs/tickets/README.md`, `BUG-2`). Both are reported here, and the errors
    // still flow into `errors` below so a failing module keeps making the build return
    // `Err`.
    //
    // `module_files` is also how each checked module's `Interface` learns which file it
    // came from (`Interface::file`, `ERR-5`): this is the one place that knows both the
    // module and its file, so `check_in_order` takes the map and stamps it onto every
    // interface it inserts as it goes.
    let (can_mods, check_errors) =
        walker.check_in_order(&package.name, interfaces, module_files, check_module);

    let what = match root {
        source::SourceRoot::Src => "modules",
        source::SourceRoot::Tests => "test modules",
    };
    let names: Vec<String> = can_mods
        .iter()
        .map(|m| m.canonical.name.as_human_string())
        .collect();

    if check_errors.is_empty() {
        print_status(true, format!("checked {}: {:#?}", what, names));
    } else {
        print_status(
            false,
            format!(
                "checked {}: {:#?} ({} failed to check)",
                what,
                names,
                check_errors.len()
            ),
        );
        // Tag each error with the file its module was read from, so the spans its phase
        // produced have something to point into. A module with no entry — there is none
        // today, since only a module that parsed can be checked — stays unwrapped and
        // renders exactly as it did before spans existed.
        errors.extend(check_errors.into_iter().map(|error| {
            match error.module().and_then(|name| module_files.get(name)) {
                Some(id) => CompilationError::InFile(Box::new(error), *id),
                None => error,
            }
        }));
    }

    can_mods
}

/// Take a parsed module file within the ecosystem and apply all checks to it
///
/// TODO canonicalization must happens before checkings, because type check (at least)
/// will require access to other modules canonical representation.
/// That probably mean moving the `canonical::canonicalize` call out of this function
///
/// Whether `source`'s package is exempt from the default imports is not decided
/// here: `canonical::canonicalize` derives it from `package` itself
/// ([`PackageName::is_core`]), so this function needs nothing beyond the package it
/// already receives — which is why `check` in
/// [`ModuleWalker::check_in_order`](dependencies::ModuleWalker::check_in_order) can
/// carry `check_module` as a plain `fn` pointer over the package alone.
pub fn check_module(
    package: &PackageName,
    interfaces: &HashMap<Name, Interface>,
    source: &parser::Module,
) -> Result<CheckedModule, CompilationError> {
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
    //
    // The typer answers with the types it solved — one `ir::Solved` per declaration,
    // carrying a type on every node of the ones it could type and saying why for the
    // ones it could not. Not logged on the way out: `infer_annotated` already dumps each
    // declaration's term under `debug`, and `typer::type_check` is `pub`, so a test that
    // wants the map calls it directly. It reads the same interfaces canonicalization
    // resolved this module's imports against, for the types of what they declare.
    let solved = typer::type_check(&canonical, interfaces)
        .map_err(|errors| CompilationError::Type(errors, source.name.clone()))?;

    // verify in pattern matching branches that all variants are covered
    exhaustiveness::check(&canonical)
        .map_err(|errors| CompilationError::Exhaustiveness(errors, source.name.clone()))?;

    // - the shape a backend reads. Built here because this is the last frame holding
    // both the canonical module and what the typer solved from it, and nothing after
    // this point needs either half separately.
    let ir = ir::build(&canonical, solved);

    Ok(CheckedModule { canonical, ir })
}

/// One module that passed every check, in both the forms the compiler still needs it.
///
/// The two halves answer different questions and neither is derivable from the other.
/// [`canonical`](Self::canonical) is what an [`Interface`] is built from, so the modules
/// that import this one are checked against it; [`ir`](Self::ir) is what a backend
/// reads, and carries the solved types, the name kinds, the arities and the constructor
/// positions that emission needs and name resolution never did.
#[derive(Debug)]
pub struct CheckedModule {
    pub canonical: canonical::Module,
    pub ir: ir::Module,
}

impl CheckedModule {
    /// This module's [`Interface`] — [`canonical::Module::to_interface`], which is the
    /// half of a checked module that crosses a module boundary.
    ///
    /// Inherent so that a caller holding a `CheckedModule` reaches it without importing
    /// [`Checked`]; the body is that trait's, so there is only one to keep correct.
    pub fn to_interface(&self, file: Option<SourceFileId>) -> Interface {
        Checked::to_interface(self, file)
    }
}

/// What [`dependencies::ModuleWalker::check_in_order`] needs of whatever its checker
/// hands back: a name to key the module by, and the interface the modules after it are
/// checked against.
///
/// It is a trait because the walker drives more than one checker. The compiler's is
/// [`check_module`], which answers with a [`CheckedModule`]; `tests/spec.rs` drives the
/// same walker with a checker that only canonicalizes, because a spec example is judged
/// on the errors each phase reports and there is nothing to emit from it.
pub trait Checked {
    /// The module's own name, package included.
    fn name(&self) -> &ModuleName;

    /// The trimmed-down view the modules that import this one are checked against.
    ///
    /// `file` is where this module was read from, when the caller knows it — see
    /// [`Interface::file`].
    fn to_interface(&self, file: Option<SourceFileId>) -> Interface;
}

impl Checked for canonical::Module {
    fn name(&self) -> &ModuleName {
        &self.name
    }

    fn to_interface(&self, file: Option<SourceFileId>) -> Interface {
        canonical::Module::to_interface(self, file)
    }
}

impl Checked for CheckedModule {
    fn name(&self) -> &ModuleName {
        &self.canonical.name
    }

    fn to_interface(&self, file: Option<SourceFileId>) -> Interface {
        self.canonical.to_interface(file)
    }
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

    /// `PackageName::core` skips `PackageName::new`'s check, so the check is run here
    /// instead: the one package name the compiler builds on its own is a legal one.
    #[test]
    fn the_core_package_name_is_a_legal_one() {
        assert_eq!(
            PackageName::new(resolve::CORE_PACKAGE),
            Ok(PackageName::core())
        );
    }

    /// `PackageName::test_package` skips the check the same way `core` does, so it is
    /// run here too.
    #[test]
    fn the_test_package_name_is_a_legal_one() {
        assert_eq!(
            PackageName::new(test_collection::TEST_PACKAGE),
            Ok(PackageName::test_package())
        );
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
