//! The imports every module gets without writing them.
//!
//! Zelkova's standard library is reachable from a module that imports nothing:
//! `Int`, `Bool`, `True`, `+` and `<|` resolve in a file whose only declaration is
//! `x = 1 + 2`, and `Maybe`, `Just`, `List.map` and `String.length` resolve there
//! too. [*The default
//! imports*](../../../docs/spec/modules.md#the-default-imports) is the rule; this
//! module is the list it names, plus the two questions asked of that list.
//!
//! # Why the list lives here and not in canonicalization
//!
//! Two phases need it, for two halves of one behaviour.
//! [`canonical::environment::new_environment`](crate::compiler::canonical::environment::new_environment)
//! turns each entry into a [`parser::Import`] and runs it through `process_import`
//! ahead of the module's own, so a name arriving implicitly is indistinguishable
//! from one written by hand. [`dependencies::ModuleWalker::new`](crate::compiler::dependencies::ModuleWalker::new)
//! adds the matching edge to the import graph, because a module can only resolve
//! against an [`Interface`](crate::compiler::Interface) that already exists — so
//! `Basics` has to be *checked* before the module that never named it, and nothing
//! but the dependency graph decides that order.
//!
//! Neither phase owns the other, and a copy of the list in each would be two lists.
//! So it sits beside them both, in a module that is the list and nothing else.
//!
//! # Where the cycle does not happen
//!
//! `Basics` cannot implicitly import `Basics`, and `Maybe` implicitly importing
//! `Result` importing `Maybe` is exactly the loop
//! [`dependencies`](crate::compiler::dependencies) exists to reject. Two rules keep
//! the graph acyclic, and both are enforced here and in `ModuleWalker::new`:
//!
//! 1. **A module named on the list receives no implicit imports** — [`is_default`].
//!    Each of the eight writes the imports it needs, which is what `std/core`'s
//!    `Basics.zel`, `Maybe.zel` and `Result.zel` already do.
//! 2. **An implicit edge that would close a loop is not added.** `Basics` imports
//!    the `Js.Basics` facade, so `Js.Basics` does not implicitly import `Basics`
//!    back: the modules a default import is *built from* do not receive it.
//!    `ModuleWalker::new` tests that edge by edge against the graph it has built so
//!    far, so no addition can ever introduce a cycle that was not already written.
//!
//! Rule 2 reaches further than "built from", because the graph it consults includes
//! the edges it has itself just added. A module no default import names can still
//! lose an entry, when an implicit edge allocated for an *earlier* entry put it
//! downstream of that one. `add_default_import_edges` allocates target by target in
//! `DEFAULT_IMPORTS` order for exactly that reason — so a collision is decided by
//! the list's priority rather than by what the modules are called — and its doc
//! comment carries the argument that nothing beyond that priority is left to
//! chance.
//!
//! The two rules agree with the availability check below rather than duplicating
//! it. When rule 2 drops an edge `m → d`, it is because `d` already depends on `m`,
//! which means `m` is checked *first* and `d`'s interface is not yet available when
//! `m` is canonicalized — so [`implicit_imports`] skips the same import on its own,
//! for its own reason. Whichever way a pair falls, both phases fall the same way.
//!
//! # An implicit import is never a diagnostic
//!
//! A synthesised import has no source text behind it, so its span is
//! [`NodeSpan::none`] and any error raised from it would render with no caret — a
//! message about an `import` line the user never wrote. [`implicit_imports`]
//! therefore applies an entry only when the package genuinely provides what that
//! entry asks for: the module has to have been checked already, and a `Maybe(..)`
//! or `List` entry additionally has to find that type declared in the interface.
//! Anything missing is left out, and the names it would have brought fail at the
//! place they are *used*, with a caret under them.
//!
//! That is what makes the list land in stages without a second change: `List`,
//! `Char`, `String` and `Task` are `.ignored` files under `std/core/src` today, so
//! their entries do nothing. Each starts working on the day its module compiles.

use super::name::Name;
use super::parser;
use super::position::NodeSpan;
use super::Interface;
use std::collections::HashMap;

/// What a default import makes available **unqualified**, beyond the qualified
/// spelling that every import provides.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unqualified {
    /// `exposing (..)` — everything the module exposes.
    Everything,
    /// `exposing (<Module>)` — the type the module is named after, without its
    /// constructors.
    Type,
    /// `exposing (<Module>(..))` — that type together with its constructors.
    TypeAndVariants,
    /// No `exposing` clause at all: the module's names are reachable only under
    /// its own prefix.
    Nothing,
}

/// One entry of the default import list.
///
/// [`Unqualified::Type`] and [`Unqualified::TypeAndVariants`] name no type of
/// their own because every entry using them exposes a type spelled exactly like
/// its module — `List` from `List`, `Maybe` from `Maybe`, `Result` from `Result`,
/// `Task` from `Task`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefaultImport {
    pub module: &'static str,
    pub unqualified: Unqualified,
}

/// The eight modules every Zelkova module behaves as though it began by importing.
///
/// This is [*The default
/// imports*](../../../docs/spec/modules.md#the-default-imports) verbatim, in the
/// order the chapter writes them. `Maybe` and `Result` bring their constructors
/// because matching on them is the ordinary way to use them; `List` and `Task`
/// come as bare types because their modules' functions read better qualified.
pub const DEFAULT_IMPORTS: &[DefaultImport] = &[
    DefaultImport {
        module: "Basics",
        unqualified: Unqualified::Everything,
    },
    DefaultImport {
        module: "List",
        unqualified: Unqualified::Type,
    },
    DefaultImport {
        module: "Maybe",
        unqualified: Unqualified::TypeAndVariants,
    },
    DefaultImport {
        module: "Result",
        unqualified: Unqualified::TypeAndVariants,
    },
    DefaultImport {
        module: "Task",
        unqualified: Unqualified::Type,
    },
    DefaultImport {
        module: "Char",
        unqualified: Unqualified::Nothing,
    },
    DefaultImport {
        module: "String",
        unqualified: Unqualified::Nothing,
    },
    DefaultImport {
        module: "Tuple",
        unqualified: Unqualified::Nothing,
    },
];

impl DefaultImport {
    /// This entry's module name.
    pub fn name(&self) -> Name {
        Name::new(self.module)
    }

    /// The `import` this entry stands for, as though it had been written at the
    /// top of the file.
    ///
    /// The span is [`NodeSpan::none`]: there is no source text behind it, and a
    /// diagnostic that would have underlined this line renders without a caret
    /// rather than pointing at an arbitrary one.
    pub fn to_import(&self) -> parser::Import {
        let name = self.name();
        let exposed = |privacy| {
            parser::Exposing::Explicit(vec![parser::Exposed::bare(parser::ExposedKind::Upper(
                name.clone(),
                privacy,
            ))])
        };

        let exposing = match self.unqualified {
            Unqualified::Everything => parser::Exposing::Open,
            Unqualified::Type => exposed(parser::Privacy::Private),
            Unqualified::TypeAndVariants => exposed(parser::Privacy::Public),
            // What the grammar builds for an `import` with no `exposing` clause.
            Unqualified::Nothing => parser::Exposing::Explicit(vec![]),
        };

        parser::Import {
            name,
            alias: None,
            exposing,
            span: NodeSpan::none(),
        }
    }

    /// Whether `interface` provides what this entry asks of it.
    ///
    /// An `exposing (Maybe(..))` entry against a module that declares no `Maybe`
    /// would raise [`EnvError::UnionNotFound`](crate::compiler::canonical::environment::EnvError::UnionNotFound)
    /// from an `import` nobody wrote. Rather than report that, the entry is
    /// dropped — see this module's documentation.
    ///
    /// The test for a [`Unqualified::Type`] or [`Unqualified::TypeAndVariants`]
    /// entry is that the interface declares a **union** of the module's own name.
    /// That is what `Maybe`, `Result` and `List` are. It is an assumption about
    /// [`Task`](../../../docs/spec/evaluation-semantics.md#effects), which is not
    /// ported yet and which the chapter does not oblige to be a union: if `Task`
    /// arrives as anything else — a type alias, or a type the compiler knows
    /// without a declaration — this entry is silently dropped and nothing here goes
    /// red. Whoever ports `Task` has to widen this test rather than trust it.
    fn satisfied_by(&self, interface: &Interface) -> bool {
        match self.unqualified {
            Unqualified::Everything | Unqualified::Nothing => true,
            Unqualified::Type | Unqualified::TypeAndVariants => {
                interface.unions.contains_key(&self.name())
            }
        }
    }
}

/// Whether `module` is one of the modules the default imports name.
///
/// Such a module receives none of them: see rule 1 in this module's
/// documentation.
pub fn is_default(module: &Name) -> bool {
    DEFAULT_IMPORTS
        .iter()
        .any(|default| default.module == module.as_str())
}

/// The imports `module` gets without writing them, given what it *did* write and
/// which interfaces are available to it.
///
/// Empty for a module on the list itself. Otherwise one [`parser::Import`] per
/// entry that `written` does not already name and that `interfaces` can satisfy —
/// a written `import` of a default module **replaces** the implicit one, so
/// `import Maybe as M` means `M.map` and nothing else.
pub fn implicit_imports(
    module: &Name,
    written: &[parser::Import],
    interfaces: &HashMap<Name, Interface>,
) -> Vec<parser::Import> {
    if is_default(module) {
        return Vec::new();
    }

    DEFAULT_IMPORTS
        .iter()
        .filter(|default| {
            let name = default.name();
            if written.iter().any(|import| import.name == name) {
                return false;
            }
            interfaces
                .get(&name)
                .is_some_and(|interface| default.satisfied_by(interface))
        })
        .map(DefaultImport::to_import)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::canonical::{Type, TypeConstructor, UnionType};
    use crate::compiler::{ModuleName, PackageName};

    /// An interface named `module`, declaring a union of the same name when
    /// `with_union` — which is what a `Maybe(..)`-shaped entry needs to find.
    fn interface(module: &str, with_union: bool) -> (Name, Interface) {
        let mut unions = HashMap::new();
        if with_union {
            unions.insert(
                Name::new(module),
                UnionType {
                    span: NodeSpan::none(),
                    variables: vec![],
                    variants: vec![TypeConstructor {
                        name: "Just".into(),
                        type_parameters: vec![Type::Variable("a".into())],
                        tpe: Name::new(module),
                    }],
                },
            );
        }

        (
            Name::new(module),
            Interface {
                module_name: ModuleName::new(
                    PackageName::new("zelkova", "core"),
                    Name::new(module),
                ),
                values: HashMap::new(),
                unions,
                infixes: HashMap::new(),
                infix_functions: HashMap::new(),
                file: None,
            },
        )
    }

    fn interfaces(entries: Vec<(Name, Interface)>) -> HashMap<Name, Interface> {
        entries.into_iter().collect()
    }

    fn names(imports: &[parser::Import]) -> Vec<String> {
        imports.iter().map(|i| i.name.to_string()).collect()
    }

    /// Only the entries the package actually provides are applied.
    ///
    /// Mutation-checked by dropping the `interfaces.get(..)` filter in
    /// `implicit_imports`: every one of the eight comes back instead of the two.
    #[test]
    fn only_available_modules_are_implicitly_imported() {
        let available = interfaces(vec![interface("Basics", false), interface("Tuple", false)]);

        assert_eq!(
            names(&implicit_imports(&"Main".into(), &[], &available)),
            vec!["Basics".to_string(), "Tuple".to_string()]
        );
    }

    /// A module on the list receives none of them — rule 1, which is what keeps
    /// `Maybe` and `Result` from importing each other.
    ///
    /// Mutation-checked by dropping the `is_default` guard: `Maybe` then comes
    /// back with `Basics` and `Result` implicitly imported.
    #[test]
    fn a_default_module_receives_no_implicit_imports() {
        let available = interfaces(vec![
            interface("Basics", false),
            interface("Maybe", true),
            interface("Result", true),
        ]);

        assert!(implicit_imports(&"Maybe".into(), &[], &available).is_empty());
        assert!(implicit_imports(&"Basics".into(), &[], &available).is_empty());
        assert!(!implicit_imports(&"Widget".into(), &[], &available).is_empty());
    }

    /// A written import of a default module replaces the implicit one, so the two
    /// cannot both register the same names and turn every use into an
    /// `AmbiguousVariables`.
    ///
    /// Mutation-checked by dropping the `written.iter().any(..)` filter: `Basics`
    /// comes back alongside the one the module wrote.
    #[test]
    fn a_written_import_replaces_the_implicit_one() {
        let available = interfaces(vec![interface("Basics", false), interface("Tuple", false)]);
        let written = vec![parser::Import {
            name: "Basics".into(),
            alias: None,
            exposing: parser::Exposing::Open,
            span: NodeSpan::none(),
        }];

        assert_eq!(
            names(&implicit_imports(&"Main".into(), &written, &available)),
            vec!["Tuple".to_string()]
        );
    }

    /// A module that happens to be named `Maybe` but declares no `Maybe` type
    /// cannot satisfy `exposing (Maybe(..))`, and the entry is dropped rather than
    /// reported from an `import` line nobody wrote.
    ///
    /// Mutation-checked by making `satisfied_by` return `true` unconditionally:
    /// `Maybe` reappears in the list, and `process_import` would then raise a
    /// caret-less `UnionNotFound`.
    #[test]
    fn an_entry_the_module_cannot_satisfy_is_dropped() {
        let with_type = interfaces(vec![interface("Maybe", true)]);
        let without_type = interfaces(vec![interface("Maybe", false)]);

        assert_eq!(
            names(&implicit_imports(&"Main".into(), &[], &with_type)),
            vec!["Maybe".to_string()]
        );
        assert!(implicit_imports(&"Main".into(), &[], &without_type).is_empty());
    }

    /// The shape each entry is turned into: `Basics` open, `Maybe` with its
    /// constructors, `List` without, `Tuple` qualified only.
    #[test]
    fn each_entry_builds_the_import_the_chapter_writes() {
        let of = |module: &str| {
            DEFAULT_IMPORTS
                .iter()
                .find(|d| d.module == module)
                .unwrap_or_else(|| panic!("`{}` is on the default import list", module))
                .to_import()
        };

        assert_eq!(of("Basics").exposing, parser::Exposing::Open);
        assert_eq!(
            of("Maybe").exposing,
            parser::Exposing::Explicit(vec![parser::Exposed::bare(parser::ExposedKind::Upper(
                "Maybe".into(),
                parser::Privacy::Public
            ))])
        );
        assert_eq!(
            of("List").exposing,
            parser::Exposing::Explicit(vec![parser::Exposed::bare(parser::ExposedKind::Upper(
                "List".into(),
                parser::Privacy::Private
            ))])
        );
        assert_eq!(of("Tuple").exposing, parser::Exposing::Explicit(vec![]));
        assert!(of("Tuple").alias.is_none());
        // A synthesised import has no source text behind it.
        assert!(of("Tuple").span.span().is_none());
    }
}
