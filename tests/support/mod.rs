//! Shared helpers for integration tests

use codespan_reporting::files::SimpleFile;
use std::collections::HashMap;
use zelkova_lang::compiler::canonical;
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::parser;
use zelkova_lang::compiler::{Interface, ModuleName, PackageName};

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
    let type_hk = |name: &str, params| canonical::Type::Type(name.into(), params);
    let type_fun = |t1, t2| canonical::Type::Arrow(Box::new(t1), Box::new(t2));

    let mut values = HashMap::new();
    // andThen : (a -> Maybe b) -> Maybe a -> Maybe b
    values.insert(
        "andThen".into(),
        type_fun(
            type_fun(type_var("a"), type_hk("Maybe", vec![type_var("b")])),
            type_fun(
                type_hk("Maybe", vec![type_var("a")]),
                type_hk("Maybe", vec![type_var("b")]),
            ),
        ),
    );
    // map : (a -> b) -> Maybe a -> Maybe b
    values.insert(
        "map".into(),
        type_fun(
            type_fun(type_var("a"), type_var("b")),
            type_fun(
                type_hk("Maybe", vec![type_var("a")]),
                type_hk("Maybe", vec![type_var("b")]),
            ),
        ),
    );
    // withDefault : a -> Maybe a -> a
    values.insert(
        "withDefault".into(),
        type_fun(
            type_var("a"),
            type_fun(type_hk("Maybe", vec![type_var("a")]), type_var("a")),
        ),
    );

    let mut unions = HashMap::new();
    unions.insert(
        "Maybe".into(),
        canonical::UnionType {
            variables: vec!["a".into()],
            variants: vec![
                canonical::TypeConstructor {
                    name: "Just".into(),
                    type_parameters: vec![canonical::Type::Variable("a".into())],
                    tpe: "Maybe".into(),
                },
                canonical::TypeConstructor {
                    name: "Nothing".into(),
                    type_parameters: vec![],
                    tpe: "Maybe".into(),
                },
            ],
        },
    );

    let interface = Interface {
        module_name: ModuleName::new(PackageName::new("zelkova", "core"), "Maybe".into()),
        values,
        unions,
        infixes: HashMap::new(),
    };

    ("Maybe".into(), interface)
}
