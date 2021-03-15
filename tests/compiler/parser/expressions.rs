use super::support::*;
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::parser::*;

fn module(body: Expression) -> Module {
    Module {
        name: name("Main"),
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
            }],
        }],
    }
}

test_parse_ok!(
    constant,
    r#"
    module Main exposing (..)

    main = 42
    "#,
    module(Expression::Lit(Literal::Int(42)))
);

test_parse_ok!(
    single_variable,
    r#"
    module Main exposing (..)

    main = myvar
    "#,
    module(Expression::Variable(name("myvar")))
);

test_parse_ok!(
    function_application,
    r#"
    module Main exposing (..)

    main = map myfunction 2
    "#,
    module(Expression::Application(
        Box::new(Expression::Application(
            Box::new(Expression::Variable(Name("map".to_string()))),
            Box::new(Expression::Variable(Name("myfunction".to_string()))),
        )),
        Box::new(Expression::Lit(Literal::Int(2))),
    ))
);

test_parse_ok!(
    function_application_parenthesis,
    r#"
    module Main exposing (..)

    main = map (myfunction 2)
    "#,
    module(Expression::Application(
        Box::new(Expression::Variable(Name("map".to_string()))),
        Box::new(Expression::Application(
            Box::new(Expression::Variable(Name("myfunction".to_string()))),
            Box::new(Expression::Lit(Literal::Int(2))),
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
    module(Expression::Application(
        // map: (a -> b) -> a -> b
        // first application result in: a -> b
        // second application result in: b
        Box::new(Expression::Application(
            Box::new(Expression::Variable(Name("+".to_string()))),
            Box::new(Expression::Lit(Literal::Int(2))),
        )),
        Box::new(Expression::Lit(Literal::Int(3))),
    ))
);

test_parse_ok!(
    tuple_declaration,
    r#"
    module Main exposing (..)

    main = (2, 3)
    "#,
    module(Expression::Tuple(vec![
        Expression::Lit(Literal::Int(2)),
        Expression::Lit(Literal::Int(3)),
    ]))
);

test_parse_ok!(
    simple_if,
    r#"
    module Main exposing (..)

    main = if true then 2 else 3
    "#,
    module(Expression::If(
        Box::new(Expression::Lit(Literal::Bool(true))),
        Box::new(Expression::Lit(Literal::Int(2))),
        Box::new(Expression::Lit(Literal::Int(3))),
    ))
);

test_parse_ok!(
    if_else_if_else,
    r#"
    module Main exposing (..)

    main = if false then 2 else if true then 3 else 4
    "#,
    module(Expression::If(
        Box::new(Expression::Lit(Literal::Bool(false))),
        Box::new(Expression::Lit(Literal::Int(2))),
        Box::new(Expression::If(
            Box::new(Expression::Lit(Literal::Bool(true))),
            Box::new(Expression::Lit(Literal::Int(3))),
            Box::new(Expression::Lit(Literal::Int(4))),
        )),
    ))
);
