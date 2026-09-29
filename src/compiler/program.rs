//! A program's entry point: the module a manifest's `main` names, held to
//! [*Programs*](../../../docs/spec/packages.md#programs).
//!
//! That section asks three things of a package whose manifest has `main`: the name is a
//! module under `src/`, the module exposes a value called `main`, and that value has type
//! `Task ()`. The first is about the manifest and has no place in any source to point at,
//! so it is [`ManifestError::MainModuleNotFound`](super::manifest::ManifestError::MainModuleNotFound).
//! The other two are about the module, and are this module's [`Error`].
//!
//! [`check`] reads a module that already checked. The type it compares is the one
//! inference solved for `main` — [`ir::Declaration::tpe`](super::ir::Declaration::tpe) —
//! and `Task` is recognised by the qualified name of its declaration in `zelkova-core`,
//! never by spelling, so a union some other module calls `Task` is not it.
//!
//! # Which packages are checked
//!
//! Every package of the build whose manifest has `main`, not only the one the compiler was
//! pointed at: `compile_in_build` runs the check, and it runs for each package of the
//! build alike. A package is a program because its own manifest says
//! so, and whether that manifest is right cannot depend on which package started the
//! build: a dependency whose `main` names nothing is as broken as a root whose `main`
//! names nothing, and would fail the moment it was compiled on its own. Checking it only
//! as the root would make one package valid in one build and invalid in another.

use super::name::{Name, QualName};
use super::position::NodeSpan;
use super::typer::Type;
use super::{CheckedModule, PhaseError, SpanLabel};

/// The name a program's entry point has in the module the manifest names.
const MAIN: &str = "main";

/// The name of `zelkova-core`'s module declaring `Task`, and of the type itself.
const TASK: &str = "Task";

/// What is wrong with the module a manifest's `main` names.
#[derive(Debug)]
pub enum Error {
    /// The module exposes no value called `main`.
    MainNotExposed {
        /// The module's `exposing (...)`, which is what has to change.
        exposing: NodeSpan,
        /// Where the module declares `main` without exposing it. `None` when it declares
        /// no `main` at all.
        declared: Option<NodeSpan>,
    },
    /// The module exposes `main`, and its type is not `Task ()`.
    MainNotTask {
        /// The type inference solved for `main`. `None` when inference did not reach it —
        /// the typer marks a declaration it cannot yet model as unchecked rather than
        /// failing it — so nothing is known about its type, and it is not accepted.
        found: Option<Type>,
        /// `main`'s annotation, which every exposed value has
        /// ([*An exposed declaration must be
        /// annotated*](../../../docs/spec/types.md#an-exposed-declaration-must-be-annotated)).
        annotation: NodeSpan,
    },
}

/// Check that `module` is fit to be a program's entry point: it exposes `main`, and
/// `main`'s solved type is `Task.Task ()` as `zelkova-core` declares it.
pub fn check(module: &CheckedModule) -> Result<(), Error> {
    let main = Name::new(MAIN);
    let canonical = &module.canonical;

    // `values` holds every declaration, exposed or not; only the interface is trimmed to
    // what the header exposes, so that is what says whether `main` is reachable.
    let declared = canonical.values.get(&main);
    let interface = canonical.to_interface(None);
    let Some(value) = declared.filter(|_| interface.values.contains_key(&main)) else {
        return Err(Error::MainNotExposed {
            exposing: canonical.exposing_span,
            declared: declared.map(|value| value.span()),
        });
    };

    // An exposed `main` that the typer could not check is in `unchecked` rather than
    // `declarations`, and has no solved type. It is not accepted: nothing is known about
    // its type.
    let found = module
        .ir
        .declarations
        .iter()
        .find(|declaration| declaration.name == main)
        .map(|declaration| declaration.tpe.clone());

    match found {
        Some(tpe) if is_task_of_unit(&tpe) => Ok(()),
        found => Err(Error::MainNotTask {
            found,
            annotation: match value {
                super::canonical::Value::TypedValue {
                    annotation_span, ..
                } => *annotation_span,
                other => other.span(),
            },
        }),
    }
}

/// Whether `tpe` is exactly `Task.Task ()`, the `Task` `zelkova-core` declares.
fn is_task_of_unit(tpe: &Type) -> bool {
    matches!(tpe, Type::Adt(name, args) if is_core_task(name) && matches!(args.as_slice(), [Type::Unit]))
}

/// Whether `name` is the declaration of `Task` in `zelkova-core`'s `Task` module.
fn is_core_task(name: &QualName) -> bool {
    name.package().is_core()
        && name.module_name().as_str() == TASK
        && name.unqualified_name().as_str() == TASK
}

/// The union at the head of `tpe`, when it is spelled `Task` and is not `zelkova-core`'s.
fn other_task(tpe: &Type) -> Option<&QualName> {
    match tpe {
        Type::Adt(name, _) if name.unqualified_name().as_str() == TASK && !is_core_task(name) => {
            Some(name)
        }
        _ => None,
    }
}

impl PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::MainNotExposed { .. } => {
                "this module is the package's `main`, and it exposes no value called `main`"
                    .to_string()
            }
            Error::MainNotTask {
                found: Some(found), ..
            } => match other_task(found) {
                // Written out, because the two types print alike: `Task ()` either way.
                Some(name) => format!(
                    "`main` has type `{}` where `Task` is `{}`, and a program's `main` must \
                     have type `Task ()` with `zelkova-core`'s `Task`",
                    found,
                    name.to_name()
                ),
                None => format!(
                    "`main` has type `{}`, and a program's `main` must have type `Task ()`",
                    found
                ),
            },
            Error::MainNotTask { found: None, .. } => {
                "the type of `main` could not be inferred, and a program's `main` must have \
                 type `Task ()`"
                    .to_string()
            }
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Error::MainNotExposed { declared: None, .. } => vec![
                "the manifest's `main` names this module, so it has to declare `main : Task ()` \
                 and expose it"
                    .to_string(),
            ],
            Error::MainNotExposed {
                declared: Some(_), ..
            } => vec!["add `main` to the module's `exposing` list".to_string()],
            Error::MainNotTask { .. } => Vec::new(),
        }
    }

    fn labels(&self) -> Vec<SpanLabel> {
        let label = |span: &NodeSpan, message: &str, primary: bool| {
            span.span().map(|span| SpanLabel {
                span,
                message: message.to_string(),
                primary,
                file: None,
            })
        };
        match self {
            Error::MainNotExposed { exposing, declared } => label(
                exposing,
                "`main` is not among what this module exposes",
                true,
            )
            .into_iter()
            .chain(
                declared
                    .as_ref()
                    .and_then(|span| label(span, "`main` is declared here", false)),
            )
            .collect(),
            Error::MainNotTask { found, annotation } => {
                let message = match found {
                    Some(found) => format!("`main` is declared as `{}`", found),
                    None => "`main` is declared here".to_string(),
                };
                label(annotation, &message, true).into_iter().collect()
            }
        }
    }
}
