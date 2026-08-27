use super::support::*;
use zelkova_lang::compiler::parser::*;

// Let's simplify how we build module for our type tests
fn module_custom_type(tpe: UnionType) -> Module {
    Module {
        name: name("Main"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![],
        types: vec![tpe],
        functions: vec![],
    }
}

fn module_function_type(tpe: Type) -> Module {
    Module {
        name: name("Main"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![],
        types: vec![],
        functions: vec![Function {
            name: "main".into(),
            tpe: Some(tpe),
            bindings: vec![],
            span: no_span(),
        }],
    }
}

// TODO At the moment we only have positive cases, we really
// need to test the invalid cases (they are a lot of them
// not covered !)

test_parse_ok!(
    custom_types_simple_union,
    r#"
    module Main exposing (..)

    type UserStatus = Regular | Visitor
    "#,
    module_custom_type(UnionType {
        span: no_span(),
        name: name("UserStatus"),
        type_arguments: vec![],
        variants: vec![
            type_unqualified(name("Regular")),
            type_unqualified(name("Visitor")),
        ],
    })
);

test_parse_ok!(
    custom_types_product_union,
    r#"
    module Main exposing (..)

    type User
        = Regular String Int
        | Visitor String
        | Anonymous
    "#,
    module_custom_type(UnionType {
        span: no_span(),
        name: name("User"),
        type_arguments: vec![],
        variants: vec![
            type_unqualified_with(
                name("Regular"),
                vec![
                    type_unqualified(name("String")),
                    type_unqualified(name("Int")),
                ],
            ),
            type_unqualified_with(name("Visitor"), vec![type_unqualified(name("String"))],),
            type_unqualified(name("Anonymous")),
        ],
    })
);

test_parse_ok!(
    custom_types_generic_union,
    r#"
    module Main exposing (..)

    type Maybe a
        = Just a
        | Nothing
    "#,
    module_custom_type(UnionType {
        span: no_span(),
        name: name("Maybe"),
        type_arguments: vec![name("a")],
        variants: vec![
            type_unqualified_with(name("Just"), vec![type_variable(name("a"))]),
            type_unqualified(name("Nothing")),
        ],
    })
);

/* TODO Once we have support for records
    type Msg = ReceivedMessage { user : User, message : String }
*/

test_parse_ok!(
    custom_types_simple_product,
    r#"
    module Main exposing (..)

    type Product = Product Int String
    "#,
    module_custom_type(UnionType {
        span: no_span(),
        name: name("Product"),
        type_arguments: vec![],
        variants: vec![type_unqualified_with(
            name("Product"),
            vec![
                type_unqualified(name("Int")),
                type_unqualified(name("String")),
            ]
        ),],
    })
);

test_parse_ok!(
    type_annotation_constant,
    r#"
    module Main exposing (..)

    main : Int
    "#,
    module_function_type(type_unqualified("Int".into()))
);

test_parse_ok!(
    type_annotation_function,
    r#"
    module Main exposing (..)

    main : String -> Int
    "#,
    module_function_type(type_arrow(
        type_unqualified("String".into()),
        type_unqualified("Int".into()),
    ))
);

test_parse_ok!(
    type_annotation_tuple_function,
    r#"
    module Main exposing (..)

    main : (a -> b, b -> c) -> a
    "#,
    module_function_type(type_arrow(
        type_tuple2(
            type_arrow(type_variable("a".into()), type_variable("b".into()),),
            type_arrow(type_variable("b".into()), type_variable("c".into()),),
        ),
        type_variable("a".into()),
    ))
);

test_parse_ok!(
    type_annotation_higher_function,
    r#"
    module Main exposing (..)

    main : (String -> Int) -> String -> Int
    "#,
    module_function_type(type_arrow(
        type_arrow(
            type_unqualified("String".into()),
            type_unqualified("Int".into()),
        ),
        type_arrow(
            type_unqualified("String".into()),
            type_unqualified("Int".into()),
        ),
    ))
);

test_parse_ok!(
    type_annotation_higher_two_function,
    r#"
    module Main exposing (..)

    main : (String -> Int) -> (Int -> String) -> Int
    "#,
    module_function_type(type_arrow(
        type_arrow(
            type_unqualified("String".into()),
            type_unqualified("Int".into()),
        ),
        type_arrow(
            type_arrow(
                type_unqualified("Int".into()),
                type_unqualified("String".into()),
            ),
            type_unqualified("Int".into()),
        ),
    ))
);

test_parse_ok!(
    type_annotation_polymorphic_function,
    r#"
    module Main exposing (..)

    main : a -> Maybe a -> a
    "#,
    module_function_type(type_arrow(
        type_variable(name("a")),
        type_arrow(
            type_unqualified_with(name("Maybe"), vec![type_variable(name("a"))]),
            type_variable(name("a")),
        ),
    ))
);
