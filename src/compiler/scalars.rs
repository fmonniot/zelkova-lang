//! The five type names the compiler knows.
//!
//! `Int`, `Float`, `Bool`, `Char` and `String` are **scalar**: each is declared in
//! Zelkova like any other type, and the compiler additionally knows the
//! representation each target gives it. [*Scalar
//! types*](../../../docs/spec/types.md#scalar-types) is the rule; this module is the
//! list it names.
//!
//! A scalar is known by the **qualified name of its declaration** and never by its
//! spelling ([`DEC-15` decision
//! 1](../../../docs/decisions/dec-15.md)). A module declaring its own `Int` declares
//! an ordinary type that shares three letters with a scalar, and every phase treats it
//! as one — which is why [`Scalar::declares`] takes a [`QualName`] and there is no
//! entry point taking a bare [`Name`](super::name::Name).
//!
//! # Why the list lives here and not in the typer
//!
//! The typer is the only reader today: [`typer::canonical_type_to_typer_type`] maps
//! four of the five onto its own literal types. The list is nonetheless a fact about
//! the language rather than about type inference, and the same five names answer
//! questions the typer never asks — which names a module underneath `Basics` receives
//! without an import, and which declarations may name nothing but themselves. It sits
//! beside the phases for the same reason
//! [`default_imports`](super::default_imports) does.

use super::name::QualName;

/// One scalar type: the module its declaration lives in, and the name that
/// declaration writes.
///
/// The module is written out with its dots, as a `module` header writes it, so a
/// nested module name is spelled the one way everywhere.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Scalar {
    pub module: &'static str,
    pub name: &'static str,
}

impl Scalar {
    /// Whether `name` is this scalar's declaration.
    ///
    /// Both halves have to match. `Example.Int` is not [`INT`], and neither is a
    /// name fabricated for an `Int` that resolved to nothing
    /// ([`BUG-16`](../../../docs/tickets/bug-16.md)) in any module but `Basics`.
    pub fn declares(&self, name: &QualName) -> bool {
        name.unqualified_name().as_str() == self.name && name.module_name().as_str() == self.module
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
/// once](../../../docs/spec/types.md#scalar-types): the compiler knows its
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

/// The five scalar types, in the order [*Scalar
/// types*](../../../docs/spec/types.md#scalar-types) lists them.
pub const SCALARS: &[Scalar] = &[INT, FLOAT, BOOL, CHAR, STRING];

/// The scalar `name` declares, if it declares one.
pub fn scalar_of(name: &QualName) -> Option<Scalar> {
    SCALARS.iter().copied().find(|s| s.declares(name))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn qual(s: &str) -> QualName {
        QualName::parse(s).unwrap()
    }

    #[test]
    fn a_scalar_is_its_declaration() {
        assert_eq!(scalar_of(&qual("Basics.Int")), Some(INT));
        assert_eq!(scalar_of(&qual("Basics.Float")), Some(FLOAT));
        assert_eq!(scalar_of(&qual("Basics.Bool")), Some(BOOL));
        assert_eq!(scalar_of(&qual("Char.Char")), Some(CHAR));
        assert_eq!(scalar_of(&qual("String.String")), Some(STRING));
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
}
