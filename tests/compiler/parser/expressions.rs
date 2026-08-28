use super::support::*;
use zelkova_lang::compiler::parser::*;
use zelkova_lang::compiler::tuple::Tuple;

fn module(body: Expression) -> Module {
    Module {
        name: name("Main"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![],
        types: vec![],
        functions: vec![Function {
            name: name("main"),
            tpe: None,
            bindings: vec![Match {
                patterns: vec![],
                body,
                span: no_span(),
            }],
            span: no_span(),
            annotation_span: no_span(),
        }],
    }
}

test_parse_ok!(
    constant,
    r#"
    module Main exposing (..)

    main = 42
    "#,
    module(expr_lit(Literal::Int(42)))
);

test_parse_ok!(
    single_variable,
    r#"
    module Main exposing (..)

    main = myvar
    "#,
    module(expr_var(name("myvar")))
);

test_parse_ok!(
    function_application,
    r#"
    module Main exposing (..)

    main = map myfunction 2
    "#,
    module(expr_app(
        Box::new(expr_app(
            Box::new(expr_var("map".into())),
            Box::new(expr_var("myfunction".into())),
        )),
        Box::new(expr_lit(Literal::Int(2))),
    ))
);

test_parse_ok!(
    function_application_parenthesis,
    r#"
    module Main exposing (..)

    main = map (myfunction 2)
    "#,
    module(expr_app(
        Box::new(expr_var("map".into())),
        Box::new(expr_app(
            Box::new(expr_var("myfunction".into())),
            Box::new(expr_lit(Literal::Int(2))),
        )),
    ))
);

// This will probably change once we have to intregrate infix
// configuration, because here we don't know yet how to build
// the application expression.
test_parse_ok!(
    operator_application,
    r#"
    module Main exposing (..)

    main = 2 + 3
    "#,
    module(expr_app(
        // map: (a -> b) -> a -> b
        // first application result in: a -> b
        // second application result in: b
        Box::new(expr_app(
            Box::new(expr_var("+".into())),
            Box::new(expr_lit(Literal::Int(2))),
        )),
        Box::new(expr_lit(Literal::Int(3))),
    ))
);

test_parse_ok!(
    tuple_declaration,
    r#"
    module Main exposing (..)

    main = (2, 3)
    "#,
    module(expr_tuple(Tuple::two(
        expr_lit(Literal::Int(2)),
        expr_lit(Literal::Int(3)),
    )))
);

test_parse_ok!(
    simple_if,
    r#"
    module Main exposing (..)

    main = if true then 2 else 3
    "#,
    module(expr_if(
        Box::new(expr_lit(Literal::Bool(true))),
        Box::new(expr_lit(Literal::Int(2))),
        Box::new(expr_lit(Literal::Int(3))),
    ))
);

test_parse_ok!(
    if_else_if_else,
    r#"
    module Main exposing (..)

    main = if false then 2 else if true then 3 else 4
    "#,
    module(expr_if(
        Box::new(expr_lit(Literal::Bool(false))),
        Box::new(expr_lit(Literal::Int(2))),
        Box::new(expr_if(
            Box::new(expr_lit(Literal::Bool(true))),
            Box::new(expr_lit(Literal::Int(3))),
            Box::new(expr_lit(Literal::Int(4))),
        )),
    ))
);
