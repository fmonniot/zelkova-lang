use super::support::*;
use zelkova_lang::compiler::parser::*;
use zelkova_lang::compiler::tuple::Tuple;

fn module(body: Expression) -> Module {
    Module {
        name: name("Main"),
        binding_foreign: false,
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![],
        types: vec![],
        functions: vec![Function {
            name: name("main"),
            tpe: None,
            marked_unsafe: false,
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

// The grammar has no operator table (an operator's `infix` declaration may
// live in another module), so `2 + 3` parses to a flat `InfixChain` rather
// than a nested `Application` — canonicalization re-associates it once the
// infix environment is in hand. See `ExpressionKind::InfixChain` and
// `canonical::reassociate_infix_chain`.
test_parse_ok!(
    operator_application,
    r#"
    module Main exposing (..)

    main = 2 + 3
    "#,
    module(expr_infix_chain(
        Box::new(expr_lit(Literal::Int(2))),
        vec![(name("+"), expr_lit(Literal::Int(3)))],
    ))
);

test_parse_ok!(
    operator_application_chain,
    r#"
    module Main exposing (..)

    main = 2 + 3 * 4
    "#,
    module(expr_infix_chain(
        Box::new(expr_lit(Literal::Int(2))),
        vec![
            (name("+"), expr_lit(Literal::Int(3))),
            (name("*"), expr_lit(Literal::Int(4))),
        ],
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

// `BUG-23`: a `case` in the `then` arm of an `if` is closed by its `else`.
// Before the fix, the layout pass never emitted the `CloseBlock`s this needs
// (see `else_closes_case_block_opened_in_a_then_arm` in `layout.rs`'s own
// tests) and this source was rejected with a `LayoutError` on `Else` instead
// of parsing to the `If` node below.
test_parse_ok!(
    case_in_then_arm_closed_by_else,
    r#"
    module Main exposing (..)

    main =
      if v then
        case v of
          On ->
            Off

          Off ->
            On
      else
        On
    "#,
    module(expr_if(
        Box::new(expr_var(name("v"))),
        Box::new(expr_case(
            Box::new(expr_var(name("v"))),
            vec![
                case_branch(pattern_ctor(name("On")), expr_ctor(name("Off"))),
                case_branch(pattern_ctor(name("Off")), expr_ctor(name("On"))),
            ],
        )),
        Box::new(expr_ctor(name("On"))),
    ))
);

// Regression for a review finding on `BUG-23`'s first fix: the mirror shape
// of `case_in_then_arm_closed_by_else` above, where the `if` is nested
// *inside* a `case` branch's body instead of the `case` being nested inside
// the `if`'s `then` arm. The nested `if`'s own `else` must not be mistaken
// for something that closes the enclosing `case`'s branch — see
// `nested_if_else_inside_case_branch_does_not_close_case_block` in
// `layout.rs`'s own tests for the layout-only account of why the first,
// unconditional fix got this wrong.
test_parse_ok!(
    case_branch_with_nested_if_else_not_closed_by_inner_else,
    r#"
    module Main exposing (..)

    main =
      case v of
        On ->
          if w then
            Off
          else
            On

        Off ->
          On
    "#,
    module(expr_case(
        Box::new(expr_var(name("v"))),
        vec![
            case_branch(
                pattern_ctor(name("On")),
                expr_if(
                    Box::new(expr_var(name("w"))),
                    Box::new(expr_ctor(name("Off"))),
                    Box::new(expr_ctor(name("On"))),
                ),
            ),
            case_branch(pattern_ctor(name("Off")), expr_ctor(name("On"))),
        ],
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
