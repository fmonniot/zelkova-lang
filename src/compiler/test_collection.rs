//! Collecting a package's tests.
//!
//! **A test is a value a module under `tests/` exposes whose type is `zelkova-test`'s
//! `Test`** — [*What a test is*](../../../docs/spec/packages.md#what-a-test-is). This
//! module is the pass that finds them: given the [`Interface`]s of the modules of a
//! package's `tests/` root that checked, [`collect`] returns, per module, the sorted names
//! of the values that have that type.
//!
//! It does not run anything, and does not decide how a runner reports what it finds —
//! that is [`test_runner`](super::test_runner)'s, the only caller this one is written for.
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
use super::{Interface, ModuleName};

/// The package that declares `Test`. Nothing outside the test modules names it: the
/// checking modules do not know `zelkova-test` exists.
pub const TEST_PACKAGE: &str = "zelkova-test";

/// The module of [`TEST_PACKAGE`] that declares `Test`, and the name of the type it
/// declares: both are spelled `Test`.
const TEST: &str = "Test";

/// Whether `tpe` is exactly `Test`, applied to no arguments.
///
/// A type variable, an arrow, a tuple, `Test` applied to an argument, and a type of
/// any of those shapes declared under a different name all fail this — only
/// [`Type::Type`] naming the exact declaration [`is_test_type`] accepts, with an empty
/// argument list, is a test. `Test` has none of its own since it is not generic, so a
/// value could otherwise only reach this by naming a different, unrelated type that
/// happens to unify with none of the above; canonicalization already rules that out
/// for anything that isn't `Type::Type`.
fn is_test(tpe: &Type) -> bool {
    matches!(tpe, Type::Type(name, args) if is_test_type(name) && args.is_empty())
}

/// Whether `name` is the declaration of `Test` in `zelkova-test`'s `Test` module, compared
/// field by field: the package, the module and the name, all three.
fn is_test_type(name: &QualName) -> bool {
    name.package().as_str() == TEST_PACKAGE
        && name.module_name().as_str() == TEST
        && name.unqualified_name().as_str() == TEST
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
/// `modules` is one checked [`Interface`] per module of a package's `tests/` root, which
/// is what a build that compiled the tests hands back. One
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
    modules
        .iter()
        .map(|interface| {
            let mut tests: Vec<Name> = interface
                .values
                .iter()
                .filter(|(_, (_, tpe))| is_test(tpe))
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
    use crate::compiler::PackageName;
    use std::collections::HashMap;
    use zelkova_syntax::position::NodeSpan;

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
            arities: HashMap::new(),
            file: None,
        }
    }

    fn real_test() -> Type {
        Type::Type(
            QualName::in_module(PackageName::new(TEST_PACKAGE).unwrap(), "Test", "Test"),
            vec![],
        )
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
    /// Each comparison of `is_test_type` has a decoy that differs from the real type in
    /// that field alone: `acme:Test.Test` (package), `zelkova-test:Other.Test` (module) and
    /// `zelkova-test:Test.Other` (name); `acme:AppTest.Test` differs in two.
    ///
    /// Mutation-checked one comparison at a time, replacing each with `true`: every one
    /// lets its own decoy in and goes red.
    #[test]
    fn only_the_real_test_type_is_collected() {
        let decoy_type = Type::Type(
            QualName::in_module(PackageName::new("acme").unwrap(), "AppTest", "Test"),
            vec![],
        );
        let wrong_package = Type::Type(
            QualName::in_module(PackageName::new("acme").unwrap(), "Test", "Test"),
            vec![],
        );
        let wrong_module = Type::Type(
            QualName::in_module(PackageName::new(TEST_PACKAGE).unwrap(), "Other", "Test"),
            vec![],
        );
        let wrong_name = Type::Type(
            QualName::in_module(PackageName::new(TEST_PACKAGE).unwrap(), "Test", "Other"),
            vec![],
        );
        let modules = vec![interface(
            "acme",
            "AppTest",
            vec![
                ("addsUp", real_test()),
                ("helper", int()),
                ("decoyTest", decoy_type),
                ("wrongPackage", wrong_package),
                ("wrongModule", wrong_module),
                ("wrongName", wrong_name),
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

    /// Four tests of one module come back sorted, not in whatever order the
    /// `HashMap` they were read from happened to iterate.
    ///
    /// Mutation-checked by dropping the `sort_by` call: with only two entries this was
    /// flaky rather than reliably red, since a two-entry `HashMap` may already iterate
    /// in order by chance — dropping the call went green in 4 of 10 runs. Four entries,
    /// inserted out of order, went red in 10 of 10 runs of the same check.
    #[test]
    fn tests_of_one_module_are_sorted_by_name() {
        let modules = vec![interface(
            "acme",
            "AppTest",
            vec![
                ("zLast", real_test()),
                ("mSecond", real_test()),
                ("bThird", real_test()),
                ("aFirst", real_test()),
            ],
        )];

        let collected = collect(&modules);

        assert_eq!(
            collected[0].tests,
            vec![
                Name::new("aFirst"),
                Name::new("bThird"),
                Name::new("mSecond"),
                Name::new("zLast"),
            ]
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

    /// [`TEST_PACKAGE`] is compared against a package name's spelling rather than built
    /// into a `PackageName`, so nothing on the way checks it; this does. A constant that
    /// broke the package-name rule would name a package no manifest can declare, and no
    /// value would ever be a test.
    ///
    /// Mutation-checked by spelling the constant `zelkova_test`: `new` rejects the
    /// underscore and this goes red.
    #[test]
    fn the_test_package_name_is_a_legal_one() {
        assert!(PackageName::new(TEST_PACKAGE).is_ok());
    }
}
