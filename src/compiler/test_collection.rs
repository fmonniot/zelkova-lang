//! Collecting a package's tests.
//!
//! **A test is a value a module under `tests/` exposes whose type is `zelkova-test`'s
//! `Test`** — [*What a test is*](../../../docs/spec/packages.md#what-a-test-is). This
//! module is the pass that finds them: given the [`Interface`]s
//! [`compile_package_with_tests`](super::compile_package_with_tests) hands back for a
//! package's `tests/` root, [`collect`] returns, per module, the sorted names of the
//! values that have that type.
//!
//! It does not run anything, and does not decide how a runner reports what it finds —
//! that is [`LANG-69`](../../../docs/tickets/lang-69.md)'s pass, the only caller this
//! one is written for.
//!
//! # Identified by type, never by spelling
//!
//! `Test` is found by its full [`QualName`] — package, module and name — the same way
//! a [`Scalar`](super::scalars::Scalar) is
//! ([`DEC-15`](../../../docs/decisions/dec-15.md) decision 1 is that precedent). A
//! package that declares its own `Test` type does not get its values run:
//! `MyPackage.Test` and `zelkova-test:Test.Test` are two different declarations that
//! happen to share a spelling, and only a whole-`QualName` comparison tells them apart.
//! A package that does not depend on `zelkova-test` at all simply has nothing that can
//! carry the type, so [`collect`] finds nothing in it and raises no error over that.

use super::canonical::Type;
use super::name::{Name, QualName};
use super::{Interface, ModuleName, PackageName};

/// The package that declares `Test`. [`PackageName::test_package`] is the checked,
/// legal-by-construction form of this string; this constant is what that method (and
/// [`test_type`]) is built from.
pub const TEST_PACKAGE: &str = "zelkova-test";

/// `zelkova-test`'s `Test`: the module and the name its one declaration writes.
fn test_type() -> QualName {
    QualName::in_module(PackageName::test_package(), "Test", "Test")
}

/// Whether `tpe` is exactly `Test`, applied to no arguments.
///
/// A type variable, an arrow, a tuple, `Test` applied to an argument, and a type of
/// any of those shapes declared under a different name all fail this — only
/// [`Type::Type`] naming the exact declaration [`test_type`] returns, with an empty
/// argument list, is a test. `Test` has none of its own since it is not generic, so a
/// value could otherwise only reach this by naming a different, unrelated type that
/// happens to unify with none of the above; canonicalization already rules that out
/// for anything that isn't `Type::Type`.
fn is_test(tpe: &Type, test_type: &QualName) -> bool {
    matches!(tpe, Type::Type(name, args) if name == test_type && args.is_empty())
}

/// One test module's collected tests: its own name, and the sorted names of the
/// values it exposes whose type is `Test`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleTests {
    pub module: ModuleName,
    /// Sorted by name, so a runner built on this reports tests in one order run to
    /// run rather than whatever order a `HashMap` happened to iterate in.
    pub tests: Vec<Name>,
}

/// The tests each of `modules` exposes.
///
/// `modules` is what [`compile_package_with_tests`](super::compile_package_with_tests)
/// hands back for a package's `tests/` root: one checked [`Interface`] per module. One
/// [`ModuleTests`] comes back per module, in the same order, even when its `tests` is
/// empty — a module that exposes no `Test` is still a module the build held, and
/// dropping it here would leave a caller unable to tell "no tests" from "not
/// compiled".
///
/// Every `tests` is empty, for every module, when `zelkova-test` is not among the
/// packages this build reached: nothing can have declared a value of a type nothing
/// in the build can name, so nothing is found. That is not this pass's error to
/// raise — a package is free to hold no tests at all
/// ([*What a test is*](../../../docs/spec/packages.md#what-a-test-is)).
pub fn collect(modules: &[Interface]) -> Vec<ModuleTests> {
    let test_type = test_type();

    modules
        .iter()
        .map(|interface| {
            let mut tests: Vec<Name> = interface
                .values
                .iter()
                .filter(|(_, (_, tpe))| is_test(tpe, &test_type))
                .map(|(name, _)| name.clone())
                .collect();
            tests.sort_by(|a, b| a.as_str().cmp(b.as_str()));

            ModuleTests {
                module: interface.module_name.clone(),
                tests,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::position::NodeSpan;
    use std::collections::HashMap;

    fn interface(package: &str, module: &str, values: Vec<(&str, Type)>) -> Interface {
        Interface {
            module_name: ModuleName::new(PackageName::new(package).unwrap(), Name::new(module)),
            values: values
                .into_iter()
                .map(|(name, tpe)| (Name::new(name), (NodeSpan::none(), tpe)))
                .collect(),
            unions: HashMap::new(),
            infixes: HashMap::new(),
            infix_functions: HashMap::new(),
            file: None,
        }
    }

    fn real_test() -> Type {
        Type::Type(test_type(), vec![])
    }

    fn int() -> Type {
        Type::Type(
            QualName::in_module(PackageName::core(), "Basics", "Int"),
            vec![],
        )
    }

    /// The shape the ticket's acceptance test pins end to end
    /// (`tests/pipeline.rs::a_test_is_found_by_its_qualname_not_its_spelling`), at the
    /// unit level: a real `Test`, an unrelated `Int`, and a value of some other
    /// package's own type spelled `Test` too — only the first is collected, and the
    /// unrelated one never enters the sort.
    ///
    /// Mutation-checked by comparing `name.unqualified_name()` to `test_type`'s
    /// instead of the whole `QualName` in `is_test`: the decoy then joins the real
    /// test and this goes red.
    #[test]
    fn only_the_real_test_type_is_collected() {
        let decoy_type = Type::Type(
            QualName::in_module(PackageName::new("acme").unwrap(), "AppTest", "Test"),
            vec![],
        );
        let modules = vec![interface(
            "acme",
            "AppTest",
            vec![
                ("addsUp", real_test()),
                ("helper", int()),
                ("decoyTest", decoy_type),
            ],
        )];

        let collected = collect(&modules);

        assert_eq!(collected.len(), 1);
        assert_eq!(
            collected[0].tests,
            vec![Name::new("addsUp")],
            "only the exposed value of the real `Test` type must be collected"
        );
    }

    /// Two tests of one module come back sorted, not in whatever order the
    /// `HashMap` they were read from happened to iterate.
    ///
    /// Mutation-checked by dropping the `sort_by` call: this is flaky rather than
    /// reliably red, since a two-entry `HashMap` may already iterate in order — the
    /// doc comment records that rather than leaving a mutation nobody could confirm.
    #[test]
    fn tests_of_one_module_are_sorted_by_name() {
        let modules = vec![interface(
            "acme",
            "AppTest",
            vec![("zLast", real_test()), ("aFirst", real_test())],
        )];

        let collected = collect(&modules);

        assert_eq!(
            collected[0].tests,
            vec![Name::new("aFirst"), Name::new("zLast")]
        );
    }

    /// A module with no `Test`-typed value still gets an entry, with an empty list —
    /// dropping it would leave a caller unable to tell "no tests" from "not
    /// compiled".
    ///
    /// Mutation-checked by turning `map` into `filter_map` returning `None` for an
    /// empty `tests`: the module then disappears from `collected` and the length
    /// assertion goes red.
    #[test]
    fn a_module_with_no_tests_still_gets_an_entry() {
        let modules = vec![interface("acme", "AppTest", vec![("helper", int())])];

        let collected = collect(&modules);

        assert_eq!(collected.len(), 1);
        assert!(collected[0].tests.is_empty());
    }
}
