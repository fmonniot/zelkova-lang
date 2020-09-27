use crate::compiler::frontend::support::*;
use zelkova_lang::compiler::frontend::*;

// Let's simplify how we build module for our type tests
fn module(tpe: UnionType) -> Module {
    Module {
        name: name("Main"),
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![],
        types: vec![tpe],
        functions: vec![],
    }
}


test_parse_ok!(
    custom_types_simple_union,
    r#"
    module Main exposing (..)

    type UserStatus = Regular | Visitor
    "#,
    module(UnionType {
        name: name("UserStatus"),
        type_arguments: vec![],
        variants: vec![
            Type::unqualified(name("Regular")),
            Type::unqualified(name("Visitor")),
        ],
    })
);

