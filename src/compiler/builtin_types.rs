//! The type names the compiler supplies, in scope in every module.
//!
//! [*Built-in type names*](../../../docs/spec/types.md#built-in-type-names) is the
//! rule; this module is the list it names, and [`DEC-15`](../../../docs/decisions/dec-15.md)
//! is why the list exists at all.
//!
//! # Why a name is here rather than in a module
//!
//! `Basics` is built from [facades](../../../docs/spec/interop.md) — `Js.Basics` and
//! `Js.Utils` — whose signatures name `Int`, `Float` and `Bool`. Those two cannot
//! import `Basics`: `Basics` imports *them*, so the import back is [a
//! cycle](../../../docs/spec/modules.md#imports-may-not-form-a-cycle) whether it is
//! written by hand or supplied as [a default
//! import](crate::compiler::default_imports), and the default-import machinery drops
//! the entry for exactly that reason. A module below `Basics` therefore has no
//! spelling that puts `Int` in its scope, and the two facades compiled only because
//! an unresolved type name was invented rather than reported (`BUG-16`).
//!
//! The five names are the [scalar types a facade signature may
//! name](../../../docs/spec/interop.md#which-types-may-cross-the-boundary) — every
//! other admitted form is either syntax (a tuple, a record, a list) or declared by a
//! module that can be imported. A facade is the boundary of the language, so the
//! types crossing it are the ones no module can be required to provide.
//!
//! # A built-in name is the weakest entry in a scope
//!
//! [`new_environment`](crate::compiler::canonical::environment::new_environment)
//! seeds these names *before* it processes a module's imports, so anything the
//! module imports or declares overwrites the entry. A module declaring `type Int
//! = Zero | Succ Int` gets its own `Int` and every other module still gets this one,
//! which is what keeps [`Bool` an ordinary union
//! type](../../../docs/spec/lexical-structure.md#reserved-words) rather than a
//! reserved word.
//!
//! # What this does not decide
//!
//! Type identity is the unqualified name throughout the canonical AST and the typer —
//! `canonical::Type::Type` carries a [`Name`] — so a built-in `Int` and the `type Int
//! = Int` that `std/core/src/Basics.zel` still declares are the same type by
//! spelling, not because anything here says so. That is the arrangement `BUG-26`
//! describes from the typer's side, and it is what a qualified type identity would
//! have to address; this module supplies a name to resolve against and no more.

use super::name::Name;

/// The type names every module resolves without importing anything.
///
/// Each takes no arguments: a written `Int a` is an arity error, the same as it is
/// for a declared nullary type.
pub const BUILTIN_TYPES: &[&str] = &["Int", "Float", "Bool", "Char", "String"];

/// The same list as [`Name`]s, which is what a scope is keyed by.
pub fn names() -> impl Iterator<Item = Name> {
    BUILTIN_TYPES.iter().map(|builtin| Name::new(*builtin))
}

/// Whether `name` is one of the built-in type names.
pub fn is_builtin(name: &Name) -> bool {
    BUILTIN_TYPES.contains(&name.as_str())
}
