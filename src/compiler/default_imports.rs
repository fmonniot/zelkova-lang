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
//! [`dependencies`](crate::compiler::dependencies) exists to reject. What keeps the
//! graph acyclic is a property of the *package*, not of any one entry: `zelkova-core`
//! — the package the eight belong to — is the exception, and it is all-or-nothing.
//! No module of it receives any of the eight, not the eight themselves and not the
//! facades they are built from, so nothing in the package can ever close a loop
//! through an implicit edge. [`declares_a_default`] is that question, asked of a
//! package by its module names — `PackageName` cannot yet answer it, since
//! [`compile_package`](crate::compiler::compile_package) hardcodes one for every
//! package it compiles. Every module of every other package receives all eight,
//! whatever it imports and whatever imports it. [*The default
//! imports*](../../../docs/spec/modules.md#the-default-imports) is the rule in full,
//! and [`DEC-17`](../../../docs/decisions/dec-17.md) is why it is scoped to the
//! package rather than judged from the import graph.
//!
//! `zelkova-core`'s own modules write every import they use — `Basics.zel`,
//! `Maybe.zel`, `Result.zel` and `Bitwise.zel` already do — which is what makes the
//! exception affordable: nothing in the package spends an entry it does not receive.
//!
//! [`implicit_imports`] and `dependencies::add_default_import_edges` are asked the
//! same question rather than each deriving their own answer: both take a
//! `package_declares_a_default` argument computed once, from the same module names,
//! so a package is exempt to `new_environment` exactly when it is to
//! `ModuleWalker::new`.
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
/// Such a module receives none of them — as a corollary of its package receiving
/// none, since a package containing a module of this name is the one the list
/// belongs to (see [`declares_a_default`] and this module's documentation).
pub fn is_default(module: &Name) -> bool {
    DEFAULT_IMPORTS
        .iter()
        .any(|default| default.module == module.as_str())
}

/// Whether a package declares one of the eight, given the names of every module
/// it contains.
///
/// A package this is true for is `zelkova-core`, the package the eight belong to,
/// and none of its modules receives any of them ([`implicit_imports`]) — the
/// package is told apart by what it declares because
/// [`compile_package`](crate::compiler::compile_package) hardcodes a
/// `PackageName` for every package it compiles and cannot yet tell one from
/// another by name.
pub fn declares_a_default<'a>(names: impl IntoIterator<Item = &'a Name>) -> bool {
    names.into_iter().any(is_default)
}

/// The imports a module gets without writing them, given what it *did* write and
/// which interfaces are available to it.
///
/// Empty whenever `package_declares_a_default` is set — a package that declares
/// one of the eight gets none of them for any of its modules, including the ones
/// named on the list themselves. That is why this no longer takes the module's
/// own name: a module named on the list is exactly what makes its own package
/// the exception ([`declares_a_default`]), so the caller's package-level answer
/// already covers it and there is nothing left for a per-module check to add.
/// Otherwise this returns one [`parser::Import`] per entry that `written` does
/// not already name and that `interfaces` can satisfy — a written `import` of a
/// default module **replaces** the implicit one, so `import Maybe as M` means
/// `M.map` and nothing else.
pub fn implicit_imports(
    written: &[parser::Import],
    interfaces: &HashMap<Name, Interface>,
    package_declares_a_default: bool,
) -> Vec<parser::Import> {
    if package_declares_a_default {
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
    use crate::compiler::name::QualName;
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
                        // The union shares its module's name, so the declaration
                        // it names is `Maybe.Maybe`.
                        tpe: QualName::parse(format!("{}.{}", module, module)).unwrap(),
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
            names(&implicit_imports(&[], &available, false)),
            vec!["Basics".to_string(), "Tuple".to_string()]
        );
    }

    /// `LANG-57`: a module of a package that declares one of the eight gets none
    /// of them — not only the modules named on the list, but every module beside
    /// them too — while an otherwise identical module of a package that declares
    /// none of the eight still gets everything it can satisfy.
    ///
    /// Mutation-checked by dropping the `package_declares_a_default` guard in
    /// `implicit_imports`: the `true` case then comes back with `Basics` (and
    /// anything else `available` can satisfy) instead of nothing.
    #[test]
    fn a_package_declaring_a_default_gets_no_implicit_imports() {
        let available = interfaces(vec![
            interface("Basics", false),
            interface("Maybe", true),
            interface("Result", true),
        ]);

        assert!(implicit_imports(&[], &available, true).is_empty());
        assert!(!implicit_imports(&[], &available, false).is_empty());
    }

    /// `declares_a_default` is a question about a package's whole set of module
    /// names, true as soon as any one of them is on the list.
    #[test]
    fn declares_a_default_asks_about_the_whole_package() {
        let core_shaped = [Name::new("Bitwise"), Name::new("Basics")];
        let ordinary = [Name::new("Widget"), Name::new("Gadget")];

        assert!(declares_a_default(core_shaped.iter()));
        assert!(!declares_a_default(ordinary.iter()));
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
            names(&implicit_imports(&written, &available, false)),
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
            names(&implicit_imports(&[], &with_type, false)),
            vec!["Maybe".to_string()]
        );
        assert!(implicit_imports(&[], &without_type, false).is_empty());
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
