use super::support::*;
use zelkova_syntax::parser::*;

// Let's simplify how we build module for our type tests
fn module_custom_type(tpe: UnionType) -> Module {
    Module {
        name: name("Main"),
        binding_foreign: false,
        exposing: Exposing::Open,
        exposing_span: no_span(),
        imports: vec![],
        infixes: vec![],
        types: vec![tpe],
        functions: vec![],
    }
}

fn module_function_type(tpe: Type) -> Module {
    module_constrained_function_type("main", None, tpe)
}

fn module_constrained_function_type(function: &str, context: Option<Type>, tpe: Type) -> Module {
    Module {
        name: name("Main"),
        binding_foreign: false,
        exposing: Exposing::Open,
        exposing_span: no_span(),
        imports: vec![],
        infixes: vec![],
        types: vec![],
        functions: vec![Function {
            name: function.into(),
            tpe: Some(tpe),
            context,
            marked_unsafe: false,
            bindings: vec![],
            span: no_span(),
            annotation_span: no_span(),
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

// A parenthesised type is an argument like any other (`LANG-9`). Each of the tests
// below is a syntax error when `AtomicType` has no parenthesised alternative: they
// were mutation-checked by restoring the grammar's previous shape — the three
// parenthesised productions in `Type`, none in `AtomicType` — and every one of them
// went red with an `UnexpectedToken` at the `(`.

test_parse_ok!(
    type_annotation_nested_application,
    r#"
    module Main exposing (..)

    main : Maybe (Maybe Int)
    "#,
    module_function_type(type_unqualified_with(
        name("Maybe"),
        vec![type_unqualified_with(
            name("Maybe"),
            vec![type_unqualified(name("Int"))],
        )],
    ))
);

test_parse_ok!(
    type_annotation_function_argument,
    r#"
    module Main exposing (..)

    main : Box (Int -> Int)
    "#,
    module_function_type(type_unqualified_with(
        name("Box"),
        vec![type_arrow(
            type_unqualified(name("Int")),
            type_unqualified(name("Int")),
        )],
    ))
);

test_parse_ok!(
    type_annotation_tuple_argument,
    r#"
    module Main exposing (..)

    main : Maybe (Int, Char)
    "#,
    module_function_type(type_unqualified_with(
        name("Maybe"),
        vec![type_tuple2(
            type_unqualified(name("Int")),
            type_unqualified(name("Char")),
        )],
    ))
);

// Nesting has no depth limit, and an application whose last argument is
// parenthesised is still an ordinary left operand of an arrow.
test_parse_ok!(
    type_annotation_deeply_nested_application_then_arrow,
    r#"
    module Main exposing (..)

    main : Result e (Maybe (Maybe a)) -> a
    "#,
    module_function_type(type_arrow(
        type_unqualified_with(
            name("Result"),
            vec![
                type_variable(name("e")),
                type_unqualified_with(
                    name("Maybe"),
                    vec![type_unqualified_with(
                        name("Maybe"),
                        vec![type_variable(name("a"))],
                    )],
                ),
            ],
        ),
        type_variable(name("a")),
    ))
);

test_parse_ok!(
    custom_types_recursive_variant,
    r#"
    module Main exposing (..)

    type Tree a
        = Node (Tree a) (Tree a)
        | Leaf a
    "#,
    module_custom_type(UnionType {
        span: no_span(),
        name: name("Tree"),
        type_arguments: vec![name("a")],
        variants: vec![
            type_unqualified_with(
                name("Node"),
                vec![
                    type_unqualified_with(name("Tree"), vec![type_variable(name("a"))]),
                    type_unqualified_with(name("Tree"), vec![type_variable(name("a"))]),
                ],
            ),
            type_unqualified_with(name("Leaf"), vec![type_variable(name("a"))]),
        ],
    })
);

// Constraint contexts (`Class a =>`)
//
// The context is parsed as a type and carried beside the annotation's type on
// `FunType::context`; nothing here checks that it is shaped like a constraint,
// which is canonicalization's job. Every unconstrained annotation above pins the
// other half: `module_function_type` expects `context: None`.
//
// Verified to fail by making `ConstrainedType`'s `=>` alternative return
// `(None, t)`: both tests then see `context: None`. Deleting that alternative
// instead turns both into an `UnexpectedToken` at `=>`.

test_parse_ok!(
    type_annotation_single_constraint,
    r#"
    module Main exposing (..)

    min : Comparable a => a -> a -> a
    "#,
    module_constrained_function_type(
        "min",
        Some(type_unqualified_with(
            name("Comparable"),
            vec![type_variable(name("a"))],
        )),
        type_arrow(
            type_variable(name("a")),
            type_arrow(type_variable(name("a")), type_variable(name("a"))),
        ),
    )
);

test_parse_ok!(
    type_annotation_constraint_list,
    r#"
    module Main exposing (..)

    lookup : (Comparable k, Eq v) => k -> v -> Bool
    "#,
    module_constrained_function_type(
        "lookup",
        Some(type_tuple2(
            type_unqualified_with(name("Comparable"), vec![type_variable(name("k"))]),
            type_unqualified_with(name("Eq"), vec![type_variable(name("v"))]),
        )),
        type_arrow(
            type_variable(name("k")),
            type_arrow(type_variable(name("v")), type_unqualified(name("Bool"))),
        ),
    )
);
