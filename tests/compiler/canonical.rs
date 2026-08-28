//! Integration tests for the canonicalization phase.
//!
//! Each test parses a source string, runs it through `canonical::canonicalize`,
//! and then asserts on the exact structure of the resulting `canonical::Module` —
//! the pattern bindings, expression bodies, and types — not just that the value
//! key is present.
use std::collections::HashMap;

use zelkova_lang::compiler::canonical;
use zelkova_lang::compiler::name::QualName;
use zelkova_lang::compiler::position::NodeSpan;
use zelkova_lang::compiler::tuple::Tuple;

#[path = "../support/mod.rs"]
mod support;

use support::*;

// ── Helpers ──────────────────────────────────────────────────────────────────

/// `Type::Type("Int", [])` — the canonical representation of an unresolved `Int`
/// (no Basics import in these tests).
fn int_t() -> canonical::Type {
    canonical::Type::Type("Int".into(), vec![])
}

// `canonical::Expression` and `canonical::Pattern` are each a `NodeSpan` beside a
// `…Kind`, so a hand-built literal would otherwise read
// `Expression::bare(ExpressionKind::Int(42))` at every node. One function per
// variant keeps the whole-value comparisons below readable. They all use
// `NodeSpan::none()`, which compares equal to the span the canonicalizer computed —
// see `NodeSpan`'s documentation for that trade and its cost.

fn c_int(i: i64) -> canonical::Expression {
    canonical::Expression::bare(canonical::ExpressionKind::Int(i))
}

fn c_char(c: char) -> canonical::Expression {
    canonical::Expression::bare(canonical::ExpressionKind::Char(c))
}

fn c_bool(b: bool) -> canonical::Expression {
    canonical::Expression::bare(canonical::ExpressionKind::Bool(b))
}

fn c_var_local(name: &str) -> canonical::Expression {
    canonical::Expression::bare(canonical::ExpressionKind::VarLocal(name.into()))
}

fn c_var_ctor(name: QualName, tpe: canonical::Type) -> canonical::Expression {
    canonical::Expression::bare(canonical::ExpressionKind::VarConstructor(name, tpe))
}

fn c_if(
    cond: canonical::Expression,
    then: canonical::Expression,
    els: canonical::Expression,
) -> canonical::Expression {
    canonical::Expression::bare(canonical::ExpressionKind::If(
        Box::new(cond),
        Box::new(then),
        Box::new(els),
    ))
}

fn c_tuple(tuple: Tuple<canonical::Expression>) -> canonical::Expression {
    canonical::Expression::bare(canonical::ExpressionKind::Tuple(tuple))
}

fn p_var(name: &str) -> canonical::Pattern {
    canonical::Pattern::bare(canonical::PatternKind::Variable(name.into()))
}

fn p_tuple(tuple: Tuple<canonical::Pattern>) -> canonical::Pattern {
    canonical::Pattern::bare(canonical::PatternKind::Tuple(tuple))
}

fn p_ctor(ctor: canonical::TypeConstructor, args: Vec<canonical::Pattern>) -> canonical::Pattern {
    canonical::Pattern::bare(canonical::PatternKind::Constructor { ctor, args })
}

// ── Scenario 1: Simple constant, no type annotation ─────────────────────────

#[test]
fn simple_constant_no_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer = 42
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"answer".into()).unwrap(),
        &canonical::Value::Value {
            span: NodeSpan::none(),
            name: "answer".into(),
            patterns: vec![],
            body: c_int(42),
        }
    );
}

// ── Scenario 2: Typed function with single parameter ────────────────────────

#[test]
fn typed_identity_function() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        identity : a -> a
        identity x = x
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"identity".into()).unwrap(),
        &canonical::Value::TypedValue {
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "identity".into(),
            // Pattern `x` is paired with the first arrow-arm type `a`
            patterns: vec![(p_var("x"), canonical::Type::Variable("a".into()),)],
            // The body `x` is a reference to the local binding
            body: c_var_local("x"),
            tpe: canonical::Type::Arrow(
                Box::new(canonical::Type::Variable("a".into())),
                Box::new(canonical::Type::Variable("a".into())),
            ),
        }
    );
}

// ── Scenario 3: Function with multiple parameters ────────────────────────────

#[test]
fn function_multiple_parameters() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        add : Int -> Int -> Int
        add a b = a
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"add".into()).unwrap(),
        &canonical::Value::TypedValue {
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "add".into(),
            patterns: vec![(p_var("a"), int_t()), (p_var("b"), int_t()),],
            // Body refers to the first pattern binding `a`
            body: c_var_local("a"),
            tpe: canonical::Type::Arrow(
                Box::new(int_t()),
                Box::new(canonical::Type::Arrow(Box::new(int_t()), Box::new(int_t()))),
            ),
        }
    );
}

// ── Scenario 4: Union type definition + constructor usage ────────────────────

#[test]
fn union_type_definition_and_constructor() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Color = Red | Green | Blue
        favorite : Color
        favorite = Red
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    // ── Union type structure ────────────────────────────────────────────────
    let color = module.types.get(&"Color".into()).unwrap();
    assert_eq!(
        color.variables,
        Vec::<zelkova_lang::compiler::name::Name>::new()
    );
    assert_eq!(color.variants.len(), 3);

    let variant_names: Vec<_> = color.variants.iter().map(|v| v.name.clone()).collect();
    assert_eq!(
        variant_names,
        vec!["Red".into(), "Green".into(), "Blue".into()]
    );

    for v in &color.variants {
        assert_eq!(v.type_parameters, vec![], "Color variants take no params");
        assert_eq!(v.tpe, "Color".into(), "variant tpe points back to Color");
    }

    // ── Value using the constructor ─────────────────────────────────────────
    // `Color` is in env so `Type::from_parser_type` returns
    // `Type::Type("Color", [])` directly for the annotation.
    let color_t = canonical::Type::Type("Color".into(), vec![]);

    // `Red` as a TypeConstructor expression:
    //   - no type params → tpe = Type::Type("Color", [])
    //   - unqualified name → falls back to env.module_name().qualify_name("Red")
    //     = QualName { module: ["Test"], name: "Red" }
    assert_eq!(
        module.values.get(&"favorite".into()).unwrap(),
        &canonical::Value::TypedValue {
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "favorite".into(),
            patterns: vec![],
            body: c_var_ctor(QualName::from("Test.Red"), color_t.clone()),
            tpe: color_t,
        }
    );
}

// ── Scenario 5: Case expression with locally-defined Maybe ───────────────────

#[test]
fn case_expression_local_maybe() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a = Just a | Nothing
        isJust : Maybe a -> Maybe a
        isJust maybe =
          case maybe of
            Just x -> Just x
            Nothing -> Nothing
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    // `Maybe a` in the env after do_types:
    //   insert_union_type inserts Type::Type("Maybe", [Variable("a")])
    //   Type::from_parser_type finds it and returns it verbatim.
    let maybe_a =
        canonical::Type::Type("Maybe".into(), vec![canonical::Type::Variable("a".into())]);

    let value = module.values.get(&"isJust".into()).unwrap();
    let (patterns, body) = match value {
        canonical::Value::TypedValue {
            patterns,
            body,
            tpe,
            ..
        } => {
            assert_eq!(
                tpe,
                &canonical::Type::Arrow(Box::new(maybe_a.clone()), Box::new(maybe_a.clone()))
            );
            (patterns, body)
        }
        other => panic!("expected TypedValue, got {:?}", other),
    };

    // Single pattern `maybe` bound to the first arrow arm
    assert_eq!(patterns, &vec![(p_var("maybe"), maybe_a.clone())]);

    // Body is a case expression on VarLocal("maybe")
    let (scrutinee, branches) = match &body.kind {
        canonical::ExpressionKind::Case(s, b) => (s.as_ref(), b),
        other => panic!("expected Case, got {:?}", other),
    };
    assert_eq!(scrutinee, &c_var_local("maybe"));
    assert_eq!(branches.len(), 2);

    // Branch 0: `Just x` pattern — Constructor with one Variable arg
    let just_ctor = canonical::TypeConstructor {
        name: "Just".into(),
        type_parameters: vec![canonical::Type::Variable("a".into())],
        tpe: "Maybe".into(),
    };
    assert_eq!(branches[0].pattern, p_ctor(just_ctor, vec![p_var("x")]));
    // Expression is Apply(VarConstructor("Test.Just", _), VarLocal("x"))
    assert!(
        matches!(
            &branches[0].expression.kind,
            canonical::ExpressionKind::Apply(_, _)
        ),
        "Just x branch expression should be Apply, got {:?}",
        branches[0].expression
    );

    // Branch 1: `Nothing` pattern — Constructor with no args
    let nothing_ctor = canonical::TypeConstructor {
        name: "Nothing".into(),
        type_parameters: vec![],
        tpe: "Maybe".into(),
    };
    assert_eq!(branches[1].pattern, p_ctor(nothing_ctor, vec![]));
    // Expression is VarConstructor("Test.Nothing", _)
    assert!(
        matches!(
            &branches[1].expression.kind,
            canonical::ExpressionKind::VarConstructor(_, _)
        ),
        "Nothing branch expression should be VarConstructor, got {:?}",
        branches[1].expression
    );
}

// ── Scenario 6: If/then/else expression ─────────────────────────────────────

#[test]
fn if_then_else_expression() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        max : Int -> Int -> Int
        max a b = if true then a else b
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"max".into()).unwrap(),
        &canonical::Value::TypedValue {
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "max".into(),
            patterns: vec![(p_var("a"), int_t()), (p_var("b"), int_t()),],
            body: c_if(c_bool(true), c_var_local("a"), c_var_local("b")),
            tpe: canonical::Type::Arrow(
                Box::new(int_t()),
                Box::new(canonical::Type::Arrow(Box::new(int_t()), Box::new(int_t()))),
            ),
        }
    );
}

// ── Scenario 7: Export validation — exporting a name that doesn't exist ──────

#[test]
fn export_nonexistent_name_is_error() {
    // NOTE: The canonicalizer only validates infix operators in exports today,
    // not lower-case names, so this currently succeeds.  This test documents
    // that current (incomplete) behaviour; flip to `is_err()` once
    // export validation is tightened.
    let source = indoc::indoc! {r#"
        module Test exposing (nonexistent)
        x = 42
    "#};
    let _ = canonicalize_standalone(source); // Ok or Err both acceptable today
}

// ── Scenario 8: JS binding module ────────────────────────────────────────────

#[test]
fn javascript_binding_module() {
    let source = indoc::indoc! {r#"
        module javascript Test exposing (add)
        add : Int -> Int -> Int
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    // JS binding values get a placeholder body of Bool(true) (see TODO in
    // canonical/mod.rs — the compiler doesn't yet have a dedicated binding
    // expression variant).
    assert_eq!(
        module.values.get(&"add".into()).unwrap(),
        &canonical::Value::TypedValue {
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "add".into(),
            patterns: vec![],
            body: c_bool(true),
            tpe: canonical::Type::Arrow(
                Box::new(int_t()),
                Box::new(canonical::Type::Arrow(Box::new(int_t()), Box::new(int_t()))),
            ),
        }
    );
}

// ── Scenario 9: Unknown constructor in a pattern is a diagnostic, not a panic ─

#[test]
fn unknown_constructor_pattern_is_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Color = Red | Green | Blue
        isRed : Color -> Bool
        isRed c =
          case c of
            Purple -> true
            _ -> false
    "#};

    let errors = canonicalize_standalone(source).expect_err("unknown constructor should error");
    // The error is nested in `Error::Many` because it originates inside a
    // `collect_accumulate` over case branches.
    assert!(
        format!("{:?}", errors).contains("VariantNotFound"),
        "expected a VariantNotFound error, got {:?}",
        errors
    );
}

// ── Scenario 10: Multi-clause functions are a diagnostic, not a panic ────────

#[test]
fn multiple_bindings_is_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        isZero : Int -> Bool
        isZero 0 = true
        isZero n = false
    "#};

    let errors = canonicalize_standalone(source).expect_err("multi-clause fn should error");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, canonical::Error::MultipleBindingsUnsupported(..))),
        "expected a MultipleBindingsUnsupported error, got {:?}",
        errors
    );
}

// ── Scenario 11: Tuples ──────────────────────────────────────────────────────
//
// `Tuple` is the single representation of a tuple in both ASTs and the grammar
// has one production per arity, so these tests cover the whole rule: the two
// legal sizes survive canonicalization through all three sites (type,
// expression, pattern), and any other size is rejected by the parser at each of
// those three sites.

/// Verified by mutating the two-element `AtomicExpr` production in
/// `grammar.lalrpop` to `Tuple::two(b, a)` and the two-element `Type`
/// production to `Tuple::two(b, a)` — each turns this test red.
#[test]
fn tuple_of_two_canonicalizes() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        pair : (Int, Char)
        pair = (1, 'a')
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"pair".into()).unwrap(),
        &canonical::Value::TypedValue {
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "pair".into(),
            patterns: vec![],
            body: c_tuple(Tuple::two(c_int(1), c_char('a'),)),
            tpe: canonical::Type::Tuple(Tuple::two(
                int_t(),
                canonical::Type::Type("Char".into(), vec![]),
            )),
        }
    );
}

/// The three type elements are all distinct so that the assertion pins their
/// order: `(Int, Char, Int)` would be a palindrome and survive a reversal.
///
/// Verified by mutating the three-element `AtomicExpr` production in
/// `grammar.lalrpop` to `Tuple::three(c, b, a)` and the three-element `Type`
/// production to `Tuple::three(c, b, a)` — each turns this test red.
#[test]
fn tuple_of_three_canonicalizes() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        triple : (Int, Char, Bool)
        triple = (1, 'a', 3)
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"triple".into()).unwrap(),
        &canonical::Value::TypedValue {
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "triple".into(),
            patterns: vec![],
            body: c_tuple(Tuple::three(c_int(1), c_char('a'), c_int(3),)),
            tpe: canonical::Type::Tuple(Tuple::three(
                int_t(),
                canonical::Type::Type("Char".into(), vec![]),
                canonical::Type::Type("Bool".into(), vec![]),
            )),
        }
    );
}

/// The pattern conversion used to read the third element with `c.first()` on a
/// rest-vector, silently dropping anything past it; `Tuple` removes the vector.
///
/// Verified by mutating the two-element `Pattern` production in
/// `grammar.lalrpop` to `Tuple::two(b, a)` — the test goes red.
#[test]
fn tuple_pattern_canonicalizes() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        first : (Int, Char) -> Int
        first (a, b) = a
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    let value = module.values.get(&"first".into()).unwrap();
    let patterns = match value {
        canonical::Value::TypedValue { patterns, .. } => patterns,
        other => panic!("expected TypedValue, got {:?}", other),
    };

    assert_eq!(
        patterns,
        &vec![(
            p_tuple(Tuple::two(p_var("a"), p_var("b"),)),
            canonical::Type::Tuple(Tuple::two(
                int_t(),
                canonical::Type::Type("Char".into(), vec![]),
            )),
        )]
    );
}

/// The three-element `Pattern` production is the one the ticket was filed
/// against: the old conversion read the third element off a rest-vector with
/// `c.first()` and truncated anything past it. The three annotated types are
/// distinct so the assertion pins element order on both the `Pattern` and the
/// `Type` side.
///
/// Verified by mutating the three-element `Pattern` production in
/// `grammar.lalrpop` to `Tuple::three(c, b, a)`, and separately by deleting
/// that production outright — the first turns this test red, the second makes
/// the source stop parsing.
#[test]
fn tuple_pattern_of_three_canonicalizes() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        first : (Int, Char, Bool) -> Int
        first (a, b, c) = a
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    let value = module.values.get(&"first".into()).unwrap();
    let patterns = match value {
        canonical::Value::TypedValue { patterns, .. } => patterns,
        other => panic!("expected TypedValue, got {:?}", other),
    };

    assert_eq!(
        patterns,
        &vec![(
            p_tuple(Tuple::three(p_var("a"), p_var("b"), p_var("c"),)),
            canonical::Type::Tuple(Tuple::three(
                int_t(),
                canonical::Type::Type("Char".into(), vec![]),
                canonical::Type::Type("Bool".into(), vec![]),
            )),
        )]
    );
}

/// Parses `source` and returns the `parser::Error` it must fail with.
///
/// These cases go through `parser::parse` directly because
/// `canonicalize_standalone` expects the parse to succeed.
fn expect_parse_error(source: &str, why: &str) -> zelkova_lang::compiler::parser::Error {
    use codespan_reporting::files::SimpleFile;
    use zelkova_lang::compiler::parser;

    let file = SimpleFile::new("Test.zel".to_string(), source.to_string());

    parser::parse(&file).expect_err(why)
}

/// Asserts `error` is an `UnexpectedToken` on `expected_token`.
fn assert_rejected_token(
    error: zelkova_lang::compiler::parser::Error,
    expected_token: zelkova_lang::compiler::parser::tokenizer::Token,
    why: &str,
) {
    use zelkova_lang::compiler::parser;

    match error {
        parser::Error::UnexpectedToken { token, .. } => {
            assert_eq!(token.value, expected_token, "{}", why);
        }
        other => panic!("expected an UnexpectedToken error, got {:?}", other),
    }
}

/// A four-element tuple is rejected by the grammar, not by
/// `canonical::Error::InvalidTupleSize` — the arity rule lives in exactly one
/// place now, and that place is upstream of canonicalization.
///
/// Verified by adding a four-element production to `AtomicExpr` in
/// `grammar.lalrpop`, which makes the parse succeed and the test go red.
#[test]
fn tuple_of_four_is_a_parse_error() {
    use zelkova_lang::compiler::parser::tokenizer::Token;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        quad = (1, 2, 3, 4)
    "#};

    let error = expect_parse_error(source, "a four-element tuple should not parse");

    assert_rejected_token(
        error,
        Token::Comma,
        "the comma introducing the fourth element is what the parser rejects",
    );
}

/// The arity rule moved into three grammar sites; this pins the `Pattern` one.
///
/// Verified by adding a four-element production to `Pattern` in
/// `grammar.lalrpop`, which makes the parse succeed and the test go red.
#[test]
fn tuple_pattern_of_four_is_a_parse_error() {
    use zelkova_lang::compiler::parser::tokenizer::Token;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        f (a, b, c, d) = a
    "#};

    let error = expect_parse_error(source, "a four-element tuple pattern should not parse");

    assert_rejected_token(
        error,
        Token::Comma,
        "the comma introducing the fourth element is what the parser rejects",
    );
}

/// The arity rule moved into three grammar sites; this pins the `Type` one.
///
/// Verified by adding a four-element production to `Type` in
/// `grammar.lalrpop`, which makes the parse succeed and the test go red.
#[test]
fn tuple_type_of_four_is_a_parse_error() {
    use zelkova_lang::compiler::parser::tokenizer::Token;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        f : (Int, Int, Int, Int)
        f = 1
    "#};

    let error = expect_parse_error(source, "a four-element tuple type should not parse");

    assert_rejected_token(
        error,
        Token::Comma,
        "the comma introducing the fourth element is what the parser rejects",
    );
}

/// Dropping `Comma<T>` from the tuple productions also dropped trailing-comma
/// support. Elm rejects `(1, 2,)` too and nothing under `std/core/src` used it,
/// so this pins the narrowing rather than treating it as a regression.
///
/// Verified by adding a `"(" <a:Expr> "," <b:Expr> "," ")"` production to
/// `AtomicExpr` in `grammar.lalrpop`, which makes the parse succeed and the
/// test go red.
#[test]
fn tuple_with_a_trailing_comma_is_a_parse_error() {
    use zelkova_lang::compiler::parser::tokenizer::Token;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        f = (1, 2,)
    "#};

    let error = expect_parse_error(source, "a trailing comma in a tuple should not parse");

    assert_rejected_token(
        error,
        Token::RPar,
        "the closing parenthesis after the trailing comma is what the parser rejects",
    );
}

// ── Extra: Module with imported Maybe interface ───────────────────────────────

#[test]
fn module_using_imported_maybe() {
    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Maybe exposing (Maybe(..))
        safeHead : Maybe a -> Maybe a
        safeHead m =
          case m of
            Just x -> Just x
            Nothing -> Nothing
    "#};
    let module = canonicalize_with_interfaces(source, &interfaces).expect("should canonicalize");

    // The imported `Maybe` interface stores the type as `Type::Type("Maybe", [])`
    // (insert_foreign_union_type uses an empty param list).  The annotation
    // `Maybe a` resolves via env.find_type → returns that stored value verbatim,
    // ignoring the `a` parameter (a known simplification).
    let maybe_t = canonical::Type::Type("Maybe".into(), vec![]);

    let value = module.values.get(&"safeHead".into()).unwrap();
    let (patterns, body) = match value {
        canonical::Value::TypedValue {
            patterns,
            body,
            tpe,
            ..
        } => {
            assert_eq!(
                tpe,
                &canonical::Type::Arrow(Box::new(maybe_t.clone()), Box::new(maybe_t.clone()))
            );
            (patterns, body)
        }
        other => panic!("expected TypedValue, got {:?}", other),
    };

    // Single parameter `m` bound to the first arrow-arm type
    assert_eq!(patterns, &vec![(p_var("m"), maybe_t)]);

    // Body is `case m of ...`
    let (scrutinee, branches) = match &body.kind {
        canonical::ExpressionKind::Case(s, b) => (s.as_ref(), b),
        other => panic!("expected Case, got {:?}", other),
    };
    assert_eq!(scrutinee, &c_var_local("m"));
    assert_eq!(branches.len(), 2);

    // Patterns come from the imported interface's TypeConstructor records
    let just_ctor = canonical::TypeConstructor {
        name: "Just".into(),
        type_parameters: vec![canonical::Type::Variable("a".into())],
        tpe: "Maybe".into(),
    };
    assert_eq!(branches[0].pattern, p_ctor(just_ctor, vec![p_var("x")]));

    let nothing_ctor = canonical::TypeConstructor {
        name: "Nothing".into(),
        type_parameters: vec![],
        tpe: "Maybe".into(),
    };
    assert_eq!(branches[1].pattern, p_ctor(nothing_ctor, vec![]));
}

// ── Extra: an annotation with no body points at the annotation ───────────────

/// `ERR-3`: `NoBindings` renders a caret under the annotation it is about.
///
/// "This declaration has a type annotation but no body" is precisely the message
/// where the reader needs to know *which* annotation, and the construction site in
/// `do_values` has `function.span` in hand — it is the same span the sibling
/// `BindingPatternsInvalidLen` uses three lines above. The range is asserted rather
/// than mere non-emptiness, for the usual reason: a span taken around the layout
/// pass's zero-width block tokens would satisfy `!labels.is_empty()` while pointing
/// at nothing.
///
/// Mutation-checked by dropping the `NoBindings` arm from `canonical::Error::labels`
/// so it falls through to the catch-all: `labels` comes back empty.
#[test]
fn annotation_without_a_body_labels_the_annotation() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer : Int
    "#};

    let errors =
        canonicalize_standalone(source).expect_err("an annotation with no body is an error");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let annotation = "answer : Int";
    let start = source.find(annotation).expect("source declares `answer`");

    match &errors[0] {
        canonical::Error::NoBindings(_) => (),
        other => panic!("expected NoBindings, got {:?}", other),
    }

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + annotation.len()),
        "the caret must sit under the annotation"
    );
}
