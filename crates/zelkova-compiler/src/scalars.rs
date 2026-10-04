//! The five type names the compiler knows as scalars, and the one it knows beside them.
//!
//! `Int`, `Float`, `Bool`, `Char` and `String` are **scalar**: each is declared in
//! Zelkova like any other type, and the compiler additionally knows the
//! representation each target gives it. [*Scalar
//! types*](../../docs/spec/types.md#scalar-types) is the rule; this module is the
//! list it names.
//!
//! A scalar is known by the **qualified name of its declaration** — package, module and
//! name — and never by its spelling ([`DEC-15` decision
//! 1](../../docs/decisions/dec-15.md#1--a-scalar-type-is-known-by-its-qualified-name)).
//! A module declaring its own `Int` declares an ordinary type that shares three letters
//! with a scalar, and every phase treats it as one — which is why [`Scalar::declares`]
//! takes a [`QualName`] and there is no entry point taking a bare
//! [`Name`](super::name::Name). The same goes for a whole package: a dependency's own
//! `Basics` declaring `type Int = Int` declares `Basics.Int` of *that* package, and the
//! five scalars are declared in [`zelkova-core`](super::resolve::CORE_PACKAGE) only.
//!
//! # Why the list lives here and not in the typer
//!
//! The typer is the only reader today: `typer::canonical_type_to_typer_type` maps
//! four of the five onto its own literal types, and `BOOL` names the union an `if`
//! condition is checked against. The list is nonetheless a fact about
//! the language rather than about type inference, and the same five names answer
//! questions the typer never asks — which names a module underneath `Basics` receives
//! without an import, and which declarations may name nothing but themselves. It sits
//! beside the phases for the same reason
//! [`default_imports`](super::default_imports) does.

use super::name::QualName;
use super::resolve::CORE_PACKAGE;
use super::PackageName;

/// One scalar type: the module of `zelkova-core` its declaration lives in, and the name
/// that declaration writes.
///
/// The module is written out with its dots, as a `module` header writes it, so a
/// nested module name is spelled the one way everywhere. The package is not a field:
/// it is [`CORE_PACKAGE`] for all five.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Scalar {
    pub module: &'static str,
    pub name: &'static str,
}

impl Scalar {
    /// Whether `name` is this scalar's declaration.
    ///
    /// All three parts have to match: `Example.Int` is not [`INT`], and neither is a
    /// `Basics.Int` declared by any package but [`CORE_PACKAGE`]. An `Int` that
    /// resolves to nothing never reaches here at all — canonicalization rejects
    /// the name rather than building a type out of it.
    pub fn declares(&self, name: &QualName) -> bool {
        name.package().as_str() == CORE_PACKAGE
            && name.unqualified_name().as_str() == self.name
            && name.module_name().as_str() == self.module
    }

    /// This scalar's declaration, as the qualified name every phase after
    /// canonicalization spells a type with.
    ///
    /// The inverse of [`Scalar::declares`], and what a phase reaches for when it needs
    /// to *name* a scalar rather than recognise one — the typer builds the type of an
    /// `if` condition out of [`BOOL`] this way.
    pub fn qual_name(&self) -> QualName {
        QualName::in_module(PackageName::core(), self.module, self.name)
    }
}

/// `Basics.Int`.
pub const INT: Scalar = Scalar {
    module: "Basics",
    name: "Int",
};

/// `Basics.Float`.
pub const FLOAT: Scalar = Scalar {
    module: "Basics",
    name: "Float",
};

/// `Basics.Bool`, which is [a scalar and an ordinary union at
/// once](../../docs/spec/types.md#scalar-types): the compiler knows its
/// representation and nothing about its structure, so `True` and `False` stay
/// constructors like any others.
pub const BOOL: Scalar = Scalar {
    module: "Basics",
    name: "Bool",
};

/// `Char.Char`.
pub const CHAR: Scalar = Scalar {
    module: "Char",
    name: "Char",
};

/// `String.String`.
pub const STRING: Scalar = Scalar {
    module: "String",
    name: "String",
};

/// `Basics.Position`: the type a derivation hands `differed` and `atConstructor` for a
/// constructor's place in its declaration.
///
/// Known by name for the reason a scalar is — the compiler has to give those parameters a
/// type before any class has been read — and **not a scalar**: it is declared in Zelkova
/// like any other type, has no representation the compiler knows, and is in neither
/// [`SCALARS`] nor [`OPAQUE_SCALARS`]. Its one constructor, `Position`, holds the
/// zero-based place as an `Int` and is exposed to no module, so a derived instance names
/// it by this declaration ([`Scalar::qual_name`]) and never through scope.
pub const POSITION: Scalar = Scalar {
    module: "Basics",
    name: "Position",
};

/// The five scalar types, in the order [*Scalar
/// types*](../../docs/spec/types.md#scalar-types) lists them.
pub const SCALARS: &[Scalar] = &[INT, FLOAT, BOOL, CHAR, STRING];

/// The scalar `name` declares, if it declares one.
pub fn scalar_of(name: &QualName) -> Option<Scalar> {
    SCALARS.iter().copied().find(|s| s.declares(name))
}

/// The four scalars whose declaration is **opaque** ([`DEC-15` decision
/// 2](../../docs/decisions/dec-15.md#2--a-scalar-type-is-declared-in-zelkova-and-an-opaque-ones-declaration-names-itself)):
/// nothing in the language constructs or inspects a value of one, so each declares
/// only its own name and contributes no constructor.
///
/// [`BOOL`] is deliberately absent. It is a scalar — the compiler knows its
/// representation — but its declaration is `Basics`' genuine `type Bool = True |
/// False`, and the self-naming rule does not reach it ([`DEC-15` decision
/// 5](../../docs/decisions/dec-15.md#5--bool-is-a-scalar-and-an-ordinary-union-and-both-at-once)).
pub const OPAQUE_SCALARS: &[Scalar] = &[INT, FLOAT, CHAR, STRING];

/// The opaque scalar `name` declares, if it declares one — `None` for [`BOOL`] even
/// though [`scalar_of`] would answer it, since `BOOL` is not in [`OPAQUE_SCALARS`].
pub fn opaque_scalar_of(name: &QualName) -> Option<Scalar> {
    OPAQUE_SCALARS.iter().copied().find(|s| s.declares(name))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn qual(s: &str) -> QualName {
        QualName::parse(PackageName::core(), s).unwrap()
    }

    /// `Position` is known by name and is not a scalar: no list of scalars holds it, and
    /// `scalar_of` does not answer it.
    #[test]
    fn position_is_known_by_name_and_is_not_a_scalar() {
        assert!(POSITION.declares(&qual("Basics.Position")));
        assert_eq!(POSITION.qual_name(), qual("Basics.Position"));
        assert!(!SCALARS.contains(&POSITION));
        assert!(!OPAQUE_SCALARS.contains(&POSITION));
        assert_eq!(scalar_of(&qual("Basics.Position")), None);
    }

    #[test]
    fn a_scalar_is_its_declaration() {
        assert_eq!(scalar_of(&qual("Basics.Int")), Some(INT));
        assert_eq!(scalar_of(&qual("Basics.Float")), Some(FLOAT));
        assert_eq!(scalar_of(&qual("Basics.Bool")), Some(BOOL));
        assert_eq!(scalar_of(&qual("Char.Char")), Some(CHAR));
        assert_eq!(scalar_of(&qual("String.String")), Some(STRING));
    }

    /// The two directions agree: the name a scalar writes is the one it recognises.
    ///
    /// Mutation-checked by giving [`Scalar::qual_name`] the wrong module
    /// (`QualName::in_module(PackageName::core(), "Example", self.name)`): `declares`
    /// then rejects every scalar's own name and the loop goes red.
    #[test]
    fn a_scalar_recognises_the_name_it_writes() {
        for scalar in SCALARS {
            assert!(
                scalar.declares(&scalar.qual_name()),
                "{:?} does not recognise its own qualified name",
                scalar
            );
        }

        assert_eq!(BOOL.qual_name(), qual("Basics.Bool"));
        assert_eq!(CHAR.qual_name(), qual("Char.Char"));
    }

    /// The spelling alone decides nothing: the same four letters declared elsewhere
    /// is an ordinary type.
    #[test]
    fn another_modules_int_is_not_the_scalar() {
        assert_eq!(scalar_of(&qual("Example.Int")), None);
        assert_eq!(scalar_of(&qual("Js.Basics.Int")), None);
        assert_eq!(scalar_of(&qual("Basics.Char")), None);
        assert_eq!(scalar_of(&qual("Char.String")), None);
    }

    /// The package is part of the identity: `Basics.Int` declared by a package other
    /// than `zelkova-core` is an ordinary type, however it is spelled.
    ///
    /// Mutation-checked by dropping the package comparison from [`Scalar::declares`]:
    /// every assertion below then answers the scalar and goes red.
    #[test]
    fn another_packages_basics_int_is_not_the_scalar() {
        let acme = PackageName::new("acme-basics").unwrap();

        for scalar in SCALARS {
            let elsewhere = QualName::in_module(acme.clone(), scalar.module, scalar.name);
            assert_eq!(scalar_of(&elsewhere), None, "{:?} of acme-basics", scalar);
        }
    }

    /// The four opaque scalars answer `opaque_scalar_of`; `Bool` is a scalar and
    /// does not.
    #[test]
    fn bool_is_a_scalar_but_not_an_opaque_one() {
        assert_eq!(opaque_scalar_of(&qual("Basics.Int")), Some(INT));
        assert_eq!(opaque_scalar_of(&qual("Basics.Float")), Some(FLOAT));
        assert_eq!(opaque_scalar_of(&qual("Char.Char")), Some(CHAR));
        assert_eq!(opaque_scalar_of(&qual("String.String")), Some(STRING));

        assert_eq!(scalar_of(&qual("Basics.Bool")), Some(BOOL));
        assert_eq!(opaque_scalar_of(&qual("Basics.Bool")), None);
    }
}
