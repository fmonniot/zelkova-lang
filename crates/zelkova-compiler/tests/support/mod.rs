//! Shared helpers for integration tests
//!
//! This file is compiled once per top-level integration test binary that includes
//! it (`crates/zelkova-compiler/tests/`'s binaries directly via `mod support;`;
//! `crates/zelkova-js/tests/javascript.rs` and `crates/zelkova/tests/pipeline.rs` through
//! `#[path = "../../zelkova-compiler/tests/support/mod.rs"] mod support;`). Each binary only
//! calls the subset of helpers it needs, so `dead_code` would fire in whichever binary
//! doesn't happen to use a given one — allow it here instead of per binary.
#![allow(dead_code)]

use codespan_reporting::files::SimpleFile;
use std::collections::HashMap;
use zelkova_compiler::canonical;
use zelkova_compiler::dependencies::{ModuleWalker, Outcome};
use zelkova_compiler::name::{Name, QualName};
use zelkova_compiler::{
    check_module_recovering, CheckedModule, CompilationError, Interface, ModuleName, PackageName,
};
use zelkova_syntax::parser;
use zelkova_syntax::position::NodeSpan;

/// The package every module the helpers below parse or canonicalize belongs to.
pub fn test_package() -> PackageName {
    PackageName::new("test-project").unwrap()
}

/// A [`QualName`] from its dotted spelling, declared in `package`, for hand-built
/// canonical types and expected values.
///
/// A qualified name carries the package that declares it, so a test spells the package
/// too — through this or one of the two shorthands below rather than by hand.
pub fn qual_in(package: &PackageName, name: &str) -> QualName {
    QualName::parse(package.clone(), name).expect("a qualified name needs a module prefix")
}

/// A name declared by a module of [`test_package`] — the module under test's own.
pub fn test_qual(name: &str) -> QualName {
    qual_in(&test_package(), name)
}

/// A name declared by `zelkova-core`: a scalar, or a union of one of the hand-built
/// interfaces below, which all stand in for `zelkova-core`'s modules.
pub fn core_qual(name: &str) -> QualName {
    qual_in(&PackageName::core(), name)
}

pub fn parse_source(source: &str) -> parser::Module {
    let file = SimpleFile::new("Test.zel".to_string(), source.to_string());
    parser::parse(&file).expect("parse should succeed")
}

/// What `ModuleWalker::check_in_order` handed back, reduced the way `check_module` reduces
/// one module: the modules that checked, and every error. A module that came back with
/// errors contributes its errors and not the module.
pub fn checked_and_errors<M, E>(outcomes: Vec<Outcome<M, E>>) -> (Vec<M>, Vec<E>) {
    let mut checked = Vec::new();
    let mut errors = Vec::new();
    for outcome in outcomes {
        match outcome {
            Outcome::Module(module, module_errors) if module_errors.is_empty() => {
                checked.push(module)
            }
            Outcome::Module(_, module_errors) => errors.extend(module_errors),
            Outcome::Failed(error) => errors.push(error),
        }
    }
    (checked, errors)
}

pub fn canonicalize_standalone(source: &str) -> Result<canonical::Module, Vec<canonical::Error>> {
    let parsed = parse_source(source);
    let interfaces = HashMap::new();
    canonical::canonicalize(&test_package(), &interfaces, &parsed)
}

/// Canonicalize `source` as a module of `zelkova-core` — the one package exempt
/// from [the default imports](../../docs/spec/modules.md#the-default-imports)
/// (`DEC-17`) — with no interfaces available at all.
///
/// The empty interface map is the point, for two kinds of caller: a test
/// declaring one of [the scalars](../../docs/spec/types.md#scalar-types) needs no
/// interface, since a scalar is declared in `zelkova-core` and nowhere else; and a
/// test of the scalar *seeding* (`LANG-58`) needs the map empty to tell a module
/// that resolves `Int`/`Float`/`Bool` by seeding apart from one that resolves them
/// by importing `Basics`, since the second would have nothing here to import from.
pub fn canonicalize_exempt_package(
    source: &str,
) -> Result<canonical::Module, Vec<canonical::Error>> {
    let parsed = parse_source(source);
    let interfaces = HashMap::new();
    canonical::canonicalize(&PackageName::core(), &interfaces, &parsed)
}

pub fn canonicalize_with_interfaces(
    source: &str,
    interfaces: &HashMap<Name, Interface>,
) -> Result<canonical::Module, Vec<canonical::Error>> {
    let parsed = parse_source(source);
    canonical::canonicalize(&test_package(), interfaces, &parsed)
}

/// [`canonicalize_with_interfaces`] through `canonicalize_recovering`: the module
/// canonicalization built beside every error it reported.
pub fn canonicalize_recovering_with_interfaces(
    source: &str,
    interfaces: &HashMap<Name, Interface>,
) -> canonical::Canonicalized {
    let parsed = parse_source(source);
    canonical::canonicalize_recovering(&test_package(), interfaces, &parsed)
}

/// Build a minimal Maybe interface for use in tests that need it.
/// Mirrors the `maybe_interface()` helper in environment.rs tests.
pub fn maybe_interface() -> (Name, Interface) {
    let type_var = |name: &str| canonical::Type::Variable(name.into());
    // A canonical type names its declaration in full, so the `Maybe` this
    // interface exports is `Maybe.Maybe`.
    let type_hk = |name: &str, params| canonical::Type::Type(core_qual(name), params);
    let type_fun = |t1, t2| canonical::Type::Arrow(Box::new(t1), Box::new(t2));

    let mut values = HashMap::new();
    // andThen : (a -> Maybe b) -> Maybe a -> Maybe b
    values.insert(
        "andThen".into(),
        canonical::ValueSignature::unconstrained(
            // Hand-built, not canonicalized from source: no position behind it.
            NodeSpan::none(),
            type_fun(
                type_fun(type_var("a"), type_hk("Maybe.Maybe", vec![type_var("b")])),
                type_fun(
                    type_hk("Maybe.Maybe", vec![type_var("a")]),
                    type_hk("Maybe.Maybe", vec![type_var("b")]),
                ),
            ),
        ),
    );
    // map : (a -> b) -> Maybe a -> Maybe b
    values.insert(
        "map".into(),
        canonical::ValueSignature::unconstrained(
            NodeSpan::none(),
            type_fun(
                type_fun(type_var("a"), type_var("b")),
                type_fun(
                    type_hk("Maybe.Maybe", vec![type_var("a")]),
                    type_hk("Maybe.Maybe", vec![type_var("b")]),
                ),
            ),
        ),
    );
    // withDefault : a -> Maybe a -> a
    values.insert(
        "withDefault".into(),
        canonical::ValueSignature::unconstrained(
            NodeSpan::none(),
            type_fun(
                type_var("a"),
                type_fun(type_hk("Maybe.Maybe", vec![type_var("a")]), type_var("a")),
            ),
        ),
    );

    let mut unions = HashMap::new();
    unions.insert(
        "Maybe".into(),
        canonical::UnionType {
            // Hand-built, not canonicalized from source: no position behind it.
            span: NodeSpan::none(),
            variables: vec!["a".into()],
            variants: vec![
                canonical::TypeConstructor {
                    name: "Just".into(),
                    type_parameters: vec![canonical::Type::Variable("a".into())],
                    tpe: core_qual("Maybe.Maybe"),
                },
                canonical::TypeConstructor {
                    name: "Nothing".into(),
                    type_parameters: vec![],
                    tpe: core_qual("Maybe.Maybe"),
                },
            ],
        },
    );

    let interface = Interface {
        module_name: ModuleName::new(PackageName::core(), "Maybe".into()),
        values,
        unions,
        opaque_unions: Default::default(),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
    };

    ("Maybe".into(), interface)
}

/// Build a minimal `Basics` interface declaring the three scalars it owns —
/// `Basics.Int`, `Basics.Float` and `Basics.Bool` — and the opaque `Basics.Position` a
/// derivation hands `differed`, and nothing else.
///
/// A scalar is known by the qualified name of its declaration, so a bare `Int` is the
/// scalar only in a module whose `Int` resolves to `Basics.Int`. In a real compile
/// the default imports give every module that; a standalone module handed to
/// `check_module` has no `Basics` to import, and this is what stands in for it. Put
/// it in the interface map and the implicit `import Basics exposing (..)` applies on
/// its own.
///
/// `Int` and `Float` are opaque in `std/core`'s `Basics`, so they carry no
/// constructors here and are listed in `opaque_unions`, which is what makes
/// `import Basics exposing (Int(..))` an error as it is against the real module;
/// `Bool` carries `True` and `False`.
pub fn basics_interface() -> (Name, Interface) {
    let union = |name: &str, variants: &[&str]| canonical::UnionType {
        // Hand-built, not canonicalized from source: no position behind it.
        span: NodeSpan::none(),
        variables: vec![],
        variants: variants
            .iter()
            .map(|variant| canonical::TypeConstructor {
                name: (*variant).into(),
                type_parameters: vec![],
                tpe: core_qual(&format!("Basics.{}", name)),
            })
            .collect(),
    };

    let mut unions = HashMap::new();
    unions.insert("Int".into(), union("Int", &[]));
    unions.insert("Float".into(), union("Float", &[]));
    unions.insert("Bool".into(), union("Bool", &["True", "False"]));
    // `Position` is declared here, opaque, with no constructor in the interface: the
    // compiler knows the type, and the one constructor behind it, by name
    // (`scalars::POSITION`), and no module is handed it.
    unions.insert("Position".into(), union("Position", &[]));

    let interface = Interface {
        module_name: ModuleName::new(PackageName::core(), "Basics".into()),
        values: HashMap::new(),
        unions,
        opaque_unions: std::collections::HashSet::from([
            "Int".into(),
            "Float".into(),
            "Position".into(),
        ]),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
    };

    ("Basics".into(), interface)
}

/// Build a minimal `Char` interface declaring the scalar `Char.Char`, opaque, for the
/// same reason as [`basics_interface`]: the default imports bring `Char` unqualified,
/// and a standalone module has no `Char` module to bring it from.
pub fn char_interface() -> (Name, Interface) {
    let mut unions = HashMap::new();
    unions.insert(
        "Char".into(),
        canonical::UnionType {
            // Hand-built, not canonicalized from source: no position behind it.
            span: NodeSpan::none(),
            variables: vec![],
            variants: vec![],
        },
    );

    let interface = Interface {
        module_name: ModuleName::new(PackageName::core(), "Char".into()),
        values: HashMap::new(),
        unions,
        opaque_unions: std::collections::HashSet::from(["Char".into()]),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
    };

    ("Char".into(), interface)
}

/// Build a minimal `String` interface declaring the scalar `String.String`,
/// opaque, for the same reason as [`char_interface`].
pub fn string_interface() -> (Name, Interface) {
    let mut unions = HashMap::new();
    unions.insert(
        "String".into(),
        canonical::UnionType {
            // Hand-built, not canonicalized from source: no position behind it.
            span: NodeSpan::none(),
            variables: vec![],
            variants: vec![],
        },
    );

    let interface = Interface {
        module_name: ModuleName::new(PackageName::core(), "String".into()),
        values: HashMap::new(),
        unions,
        opaque_unions: std::collections::HashSet::from(["String".into()]),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
    };

    ("String".into(), interface)
}

/// Build a minimal `Result` interface declaring `Result.Result`, with its two
/// constructors — `Ok` and `Err`, matching [the default
/// imports](../../docs/spec/modules.md#the-default-imports) — for a fixture that
/// needs the `Result Failure a` payload an unmarked facade's result carries
/// ([`LANG-68`](../../docs/tickets/README.md)).
pub fn result_interface() -> (Name, Interface) {
    let mut unions = HashMap::new();
    unions.insert(
        "Result".into(),
        canonical::UnionType {
            // Hand-built, not canonicalized from source: no position behind it.
            span: NodeSpan::none(),
            variables: vec!["e".into(), "a".into()],
            variants: vec![
                canonical::TypeConstructor {
                    name: "Ok".into(),
                    type_parameters: vec![canonical::Type::Variable("a".into())],
                    tpe: core_qual("Result.Result"),
                },
                canonical::TypeConstructor {
                    name: "Err".into(),
                    type_parameters: vec![canonical::Type::Variable("e".into())],
                    tpe: core_qual("Result.Result"),
                },
            ],
        },
    );

    let interface = Interface {
        module_name: ModuleName::new(PackageName::core(), "Result".into()),
        values: HashMap::new(),
        unions,
        opaque_unions: Default::default(),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
    };

    ("Result".into(), interface)
}

/// Build a minimal `Task` interface declaring `Task.Task` — opaque, exposed
/// without its constructors like the real one
/// (`docs/spec/evaluation-semantics.md#effects`) — and `Task.Failure`, with its
/// two constructors `Threw` and `Malformed`
/// (`docs/spec/evaluation-semantics.md#an-effect-that-can-fail`). Neither
/// constructor's argument is built as a `String`: what the constructors hold
/// is irrelevant to the shape a facade's result type is checked against
/// ([`LANG-68`](../../docs/tickets/README.md)), which only ever looks at
/// `Task.Failure`'s name.
///
/// `std/core/src/Task.zel` declares the real one. This stands in for it the way
/// [`maybe_interface`] and [`basics_interface`] do for their modules, so a test that
/// only needs the names does not have to check `std/core` first. Nothing reads
/// `Task.zel` to build it, so only the names a facade's result is checked against
/// are kept in step.
pub fn task_interface() -> (Name, Interface) {
    let mut unions = HashMap::new();
    unions.insert(
        "Task".into(),
        canonical::UnionType {
            // Hand-built, not canonicalized from source: no position behind it.
            span: NodeSpan::none(),
            variables: vec!["a".into()],
            variants: vec![],
        },
    );
    unions.insert(
        "Failure".into(),
        canonical::UnionType {
            // Hand-built, not canonicalized from source: no position behind it.
            span: NodeSpan::none(),
            variables: vec![],
            variants: vec![
                canonical::TypeConstructor {
                    name: "Threw".into(),
                    type_parameters: vec![],
                    tpe: core_qual("Task.Failure"),
                },
                canonical::TypeConstructor {
                    name: "Malformed".into(),
                    type_parameters: vec![],
                    tpe: core_qual("Task.Failure"),
                },
            ],
        },
    );

    let interface = Interface {
        module_name: ModuleName::new(PackageName::core(), "Task".into()),
        values: HashMap::new(),
        unions,
        opaque_unions: std::collections::HashSet::from(["Task".into()]),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
    };

    ("Task".into(), interface)
}

/// `std/core`'s `Js.Basics`, `Js.Utils` and `Basics`, each checked from its source as a module
/// of `zelkova-core`, in the order they check in, for a test that checks another module of that
/// package on its own. `Task`, `Maybe` and `Result` each import `Basics` for the class they
/// declare an instance of, and `Basics` is not something a hand-built double like
/// [`basics_interface`] can stand in for there: the class is the thing imported.
pub fn core_basics_modules() -> Vec<CheckedModule> {
    let core = PackageName::core();
    let mut interfaces = HashMap::new();
    let mut modules = Vec::new();
    for (name, source) in [
        (
            "Js.Basics",
            include_str!("../../../../std/core/src/Js/Basics.zel"),
        ),
        (
            "Js.Utils",
            include_str!("../../../../std/core/src/Js/Utils.zel"),
        ),
        (
            "Basics",
            include_str!("../../../../std/core/src/Basics.zel"),
        ),
    ] {
        let module = zelkova_compiler::check_module(&core, &interfaces, &parse_source(source))
            .unwrap_or_else(|error| panic!("expected {}.zel to check, got {:?}", name, error));
        interfaces.insert(
            module.canonical.name.name().clone(),
            module.to_interface(None),
        );
        modules.push(module);
    }
    modules
}

/// The interfaces of [`core_basics_modules`], keyed by module name.
pub fn core_basics_interfaces() -> HashMap<Name, Interface> {
    core_basics_modules()
        .iter()
        .map(|module| {
            (
                module.canonical.name.name().clone(),
                module.to_interface(None),
            )
        })
        .collect()
}

/// Check a package of `sources`, each one module, in dependency order with the real
/// checker, and return what came back for the module named `name`: its errors, or the
/// checked module.
pub fn check_package_module(
    sources: &[&str],
    name: &str,
) -> Result<CheckedModule, Vec<CompilationError>> {
    let modules: Vec<_> = sources.iter().map(|source| parse_source(source)).collect();
    let package = test_package();
    let module_files = HashMap::new();
    let walker = ModuleWalker::new(&modules, &module_files, &package).expect("no import cycle");
    let mut interfaces = HashMap::from([basics_interface(), char_interface()]);

    let mut result = None;
    for outcome in walker.check_in_order(
        &package,
        &mut interfaces,
        &module_files,
        check_module_recovering,
    ) {
        match outcome {
            Outcome::Module(module, errors) if module.canonical.name.name().as_str() == name => {
                result = Some(if errors.is_empty() {
                    Ok(module)
                } else {
                    Err(errors)
                });
            }
            Outcome::Failed(error) => panic!("a module failed outright: {:?}", error),
            Outcome::Module(..) => (),
        }
    }

    result.unwrap_or_else(|| panic!("no module named `{}` came back", name))
}

/// Every module of a package of `sources`, each one module, in dependency order, each checked
/// with the real checker. A module that does not check is a panic naming its errors: a test
/// that wants one that fails reads it with [`check_package_module`].
pub fn check_package_modules(sources: &[&str]) -> Vec<CheckedModule> {
    let modules: Vec<_> = sources.iter().map(|source| parse_source(source)).collect();
    let package = test_package();
    let module_files = HashMap::new();
    let walker = ModuleWalker::new(&modules, &module_files, &package).expect("no import cycle");
    let mut interfaces = HashMap::from([basics_interface(), char_interface()]);

    let mut checked = Vec::new();
    for outcome in walker.check_in_order(
        &package,
        &mut interfaces,
        &module_files,
        check_module_recovering,
    ) {
        match outcome {
            Outcome::Module(module, errors) if errors.is_empty() => checked.push(module),
            Outcome::Module(module, errors) => panic!(
                "expected `{}` to check, got {:?}",
                module.canonical.name.name(),
                errors
            ),
            Outcome::Failed(error) => panic!("a module failed outright: {:?}", error),
        }
    }
    checked
}

/// `zelkova_compiler::ir::specialise` over `modules`, which it changes in place.
pub fn specialise_all(
    modules: &mut [CheckedModule],
) -> Result<(), Vec<zelkova_compiler::ir::ModuleErrors>> {
    let mut references: Vec<&mut CheckedModule> = modules.iter_mut().collect();
    zelkova_compiler::ir::specialise(&mut references)
}

/// [`check_package_modules`], then [`specialise_all`], insisting that it finds every
/// specialisation.
pub fn specialised_package(sources: &[&str]) -> Vec<CheckedModule> {
    let mut modules = check_package_modules(sources);
    specialise_all(&mut modules).unwrap_or_else(|errors| {
        panic!(
            "expected the build to specialise, got {:?}",
            errors
                .iter()
                .flat_map(|module| &module.errors)
                .map(zelkova_compiler::PhaseError::message)
                .collect::<Vec<_>>()
        )
    });
    modules
}

/// The module of `modules` named `name`, or a panic naming the ones there are.
pub fn module_named<'a>(modules: &'a [CheckedModule], name: &str) -> &'a CheckedModule {
    modules
        .iter()
        .find(|module| module.canonical.name.name().as_str() == name)
        .unwrap_or_else(|| {
            panic!(
                "no module named `{}`; the build has {:?}",
                name,
                modules
                    .iter()
                    .map(|module| module.canonical.name.name().as_str())
                    .collect::<Vec<_>>()
            )
        })
}
