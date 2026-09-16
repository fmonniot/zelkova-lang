//! Shared helpers for integration tests
//!
//! This file is compiled once per top-level integration test binary that includes
//! it (`tests/typer.rs` and `tests/pipeline.rs` directly via `mod support;`;
//! `tests/compiler_tests.rs` indirectly, through `tests/compiler/canonical.rs`'s
//! `#[path = "../support/mod.rs"] mod support;`). Each binary only calls the subset
//! of helpers it needs, so `dead_code` would fire in whichever binary doesn't happen
//! to use a given one — allow it here instead of per binary.
#![allow(dead_code)]

use codespan_reporting::files::SimpleFile;
use std::collections::HashMap;
use zelkova_lang::compiler::canonical;
use zelkova_lang::compiler::name::{Name, QualName};
use zelkova_lang::compiler::parser;
use zelkova_lang::compiler::position::NodeSpan;
use zelkova_lang::compiler::{Interface, ModuleName, PackageName};

/// A [`QualName`] from its dotted spelling, for hand-built canonical types.
pub fn qual(name: &str) -> QualName {
    QualName::parse(name).expect("a qualified name needs a module prefix")
}

pub fn test_package() -> PackageName {
    PackageName::new("test", "project")
}

pub fn parse_source(source: &str) -> parser::Module {
    let file = SimpleFile::new("Test.zel".to_string(), source.to_string());
    parser::parse(&file).expect("parse should succeed")
}

pub fn canonicalize_standalone(source: &str) -> Result<canonical::Module, Vec<canonical::Error>> {
    let parsed = parse_source(source);
    let interfaces = HashMap::new();
    canonical::canonicalize(&test_package(), &interfaces, &parsed)
}

pub fn canonicalize_with_interfaces(
    source: &str,
    interfaces: &HashMap<Name, Interface>,
) -> Result<canonical::Module, Vec<canonical::Error>> {
    let parsed = parse_source(source);
    canonical::canonicalize(&test_package(), interfaces, &parsed)
}

/// Build a minimal Maybe interface for use in tests that need it.
/// Mirrors the `maybe_interface()` helper in environment.rs tests.
pub fn maybe_interface() -> (Name, Interface) {
    let type_var = |name: &str| canonical::Type::Variable(name.into());
    // A canonical type names its declaration in full, so the `Maybe` this
    // interface exports is `Maybe.Maybe`.
    let type_hk = |name: &str, params| canonical::Type::Type(qual(name), params);
    let type_fun = |t1, t2| canonical::Type::Arrow(Box::new(t1), Box::new(t2));

    let mut values = HashMap::new();
    // andThen : (a -> Maybe b) -> Maybe a -> Maybe b
    values.insert(
        "andThen".into(),
        (
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
        (
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
        (
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
                    tpe: qual("Maybe.Maybe"),
                },
                canonical::TypeConstructor {
                    name: "Nothing".into(),
                    type_parameters: vec![],
                    tpe: qual("Maybe.Maybe"),
                },
            ],
        },
    );

    let interface = Interface {
        module_name: ModuleName::new(PackageName::new("zelkova", "core"), "Maybe".into()),
        values,
        unions,
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        file: None,
    };

    ("Maybe".into(), interface)
}

/// Build a minimal `Basics` interface declaring the three scalars it owns —
/// `Basics.Int`, `Basics.Float` and `Basics.Bool` — and nothing else.
///
/// A scalar is known by the qualified name of its declaration, so a bare `Int` is the
/// scalar only in a module whose `Int` resolves to `Basics.Int`. In a real compile
/// the default imports give every module that; a standalone module handed to
/// `check_module` has no `Basics` to import, and this is what stands in for it. Put
/// it in the interface map and the implicit `import Basics exposing (..)` applies on
/// its own.
///
/// `Int` and `Float` are opaque in `std/core`'s `Basics`, so they carry no
/// constructors here; `Bool` carries `True` and `False`.
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
                tpe: qual(&format!("Basics.{}", name)),
            })
            .collect(),
    };

    let mut unions = HashMap::new();
    unions.insert("Int".into(), union("Int", &[]));
    unions.insert("Float".into(), union("Float", &[]));
    unions.insert("Bool".into(), union("Bool", &["True", "False"]));

    let interface = Interface {
        module_name: ModuleName::new(PackageName::new("zelkova", "core"), "Basics".into()),
        values: HashMap::new(),
        unions,
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        file: None,
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
        module_name: ModuleName::new(PackageName::new("zelkova", "core"), "Char".into()),
        values: HashMap::new(),
        unions,
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        file: None,
    };

    ("Char".into(), interface)
}
