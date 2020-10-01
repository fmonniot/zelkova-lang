use crate::compiler::frontend::support::*;
use zelkova_lang::compiler::frontend::*;

// Let's simplify how we build module for our type tests
fn module_custom_type(tpe: UnionType) -> Module {
    Module {
        name: name("Main"),
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
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![],
        types: vec![],
        functions: vec![Function {
            name: Name("main".to_string()),
            tpe: Some(tpe),
            bindings: vec![],
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
        name: name("UserStatus"),
        type_arguments: vec![],
        variants: vec![
            Type::unqualified(name("Regular")),
            Type::unqualified(name("Visitor")),
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
        name: name("User"),
        type_arguments: vec![],
        variants: vec![
            Type::unqualified_with(
                name("Regular"),
                vec![
                    Type::unqualified(name("String")),
                    Type::unqualified(name("Int")),
                ],
            ),
            Type::unqualified_with(name("Visitor"), vec![Type::unqualified(name("String"))],),
            Type::unqualified(name("Anonymous")),
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
        name: name("Maybe"),
        type_arguments: vec![name("a")],
        variants: vec![
            Type::unqualified_with(name("Just"), vec![Type::Variable(name("a"))]),
            Type::unqualified(name("Nothing")),
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
        name: name("Product"),
        type_arguments: vec![],
        variants: vec![Type::unqualified_with(
            name("Product"),
            vec![
                Type::unqualified(name("Int")),
                Type::unqualified(name("String")),
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
    module_function_type(Type::unqualified(Name("Int".to_string())))
);

test_parse_ok!(
    type_annotation_function,
    r#"
    module Main exposing (..)

    main : String -> Int
    "#,
    module_function_type(Type::Arrow(
        Box::new(Type::unqualified(Name("String".to_string()))),
        Box::new(Type::unqualified(Name("Int".to_string()))),
    ))
);

test_parse_ok!(
    type_annotation_tuple_function,
    r#"
    module Main exposing (..)

    main : (a -> b, b -> c) -> a
    "#,
    module_function_type(Type::Arrow(
        Box::new(Type::Tuple(
            Box::new(Type::Arrow(
                Box::new(Type::Variable(Name("a".to_string()))),
                Box::new(Type::Variable(Name("b".to_string()))),
            )),
            Box::new(vec![Type::Arrow(
                Box::new(Type::Variable(Name("b".to_string()))),
                Box::new(Type::Variable(Name("c".to_string()))),
            )]),
        )),
        Box::new(Type::Variable(Name("a".to_string()))),
    ))
);

test_parse_ok!(
    type_annotation_higher_function,
    r#"
    module Main exposing (..)

    main : (String -> Int) -> String -> Int
    "#,
    module_function_type(Type::Arrow(
        Box::new(Type::Arrow(
            Box::new(Type::Arrow(
                Box::new(Type::unqualified(Name("String".to_string()))),
                Box::new(Type::unqualified(Name("Int".to_string()))),
            )),
            Box::new(Type::unqualified(Name("String".to_string()))),
        )),
        Box::new(Type::unqualified(Name("Int".to_string()))),
    ))
);

test_parse_ok!(
    type_annotation_higher_two_function,
    r#"
    module Main exposing (..)

    main : (String -> Int) -> (Int -> String) -> Int
    "#,
    module_function_type(Type::Arrow(
        Box::new(Type::Arrow(
            Box::new(Type::Arrow(
                Box::new(Type::unqualified(Name("String".to_string()))),
                Box::new(Type::unqualified(Name("Int".to_string()))),
            )),
            Box::new(Type::Arrow(
                Box::new(Type::unqualified(Name("Int".to_string()))),
                Box::new(Type::unqualified(Name("String".to_string()))),
            )),
        )),
        Box::new(Type::unqualified(Name("Int".to_string()))),
    ))
);

test_parse_ok!(
    type_annotation_polymorphic_function,
    r#"
    module Main exposing (..)

    main : a -> Maybe a -> a
    "#,
    module_function_type(Type::Arrow(
        Box::new(Type::Arrow(
            Box::new(Type::Variable(name("a"))),
            Box::new(Type::unqualified_with(
                name("Maybe"),
                vec![Type::Variable(name("a"))],
            )),
        )),
        Box::new(Type::Variable(name("a"))),
    ))
);
