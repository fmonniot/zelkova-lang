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
use zelkova_lang::compiler::Interface;

#[path = "../support/mod.rs"]
mod support;

use support::*;

// ── Helpers ──────────────────────────────────────────────────────────────────

/// `Type::Type("Basics.Int", [])` — a bare `Int` in a module that can see
/// `Basics`.
///
/// A canonical type names the module that declared it, so the three scalar
/// helpers here are only what a source spelling `Int`, `Char` or `Bool`
/// canonicalizes to when something in scope declares that name. That is what
/// [`canonicalize_with_scalars`] arranges; a module handed an empty interface
/// map has no `Int` at all and is rejected for naming one.
fn int_t() -> canonical::Type {
    canonical::Type::Type(qual("Basics.Int"), vec![])
}

fn char_t() -> canonical::Type {
    canonical::Type::Type(qual("Char.Char"), vec![])
}

fn bool_t() -> canonical::Type {
    canonical::Type::Type(qual("Basics.Bool"), vec![])
}

/// Canonicalize `source` with `Basics` and `Char` available, so a bare `Int`,
/// `Float`, `Bool` or `Char` in it resolves to the scalar it names.
///
/// Neither module is imported by the sources below: the default imports bring
/// both in unqualified as soon as their interfaces exist, which is exactly what
/// a module of a real package gets.
fn canonicalize_with_scalars(source: &str) -> Result<canonical::Module, Vec<canonical::Error>> {
    canonicalize_with_interfaces(source, &scalar_interfaces())
}

/// The interface map [`canonicalize_with_scalars`] uses, for a test that needs
/// to add an interface of its own beside it.
fn scalar_interfaces() -> HashMap<zelkova_lang::compiler::name::Name, Interface> {
    let mut interfaces = HashMap::new();
    let (name, interface) = basics_interface();
    interfaces.insert(name, interface);
    let (name, interface) = char_interface();
    interfaces.insert(name, interface);
    interfaces
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
        module Test exposing ()
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
            marked_unsafe: false,
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
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"add".into()).unwrap(),
        &canonical::Value::TypedValue {
            marked_unsafe: false,
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
        assert_eq!(
            v.tpe,
            qual("Test.Color"),
            "variant tpe points back to Test's Color"
        );
    }

    // ── Value using the constructor ─────────────────────────────────────────
    // `Color` is in env so `Type::from_parser_type` returns
    // `Type::Type("Test.Color", [])` for the annotation — the head names the
    // module that declared it.
    let color_t = canonical::Type::Type(qual("Test.Color"), vec![]);

    // `Red` as a TypeConstructor expression:
    //   - no type params → tpe = Type::Type("Test.Color", [])
    //   - unqualified name → falls back to env.module_name().qualify_name("Red")
    //     = QualName { module: ["Test"], name: "Red" }
    assert_eq!(
        module.values.get(&"favorite".into()).unwrap(),
        &canonical::Value::TypedValue {
            marked_unsafe: false,
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

    // `Maybe a` in the env after do_types: `insert_union_type` records the
    // declaration as `Test.Maybe`, and `Type::from_parser_type` applies the
    // written argument to that head.
    let maybe_a = canonical::Type::Type(
        qual("Test.Maybe"),
        vec![canonical::Type::Variable("a".into())],
    );

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
        tpe: qual("Test.Maybe"),
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
        tpe: qual("Test.Maybe"),
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
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"max".into()).unwrap(),
        &canonical::Value::TypedValue {
            marked_unsafe: false,
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

/// `BUG-8`: a module's own `exposing (...)` header naming a value it never
/// declares is `Error::ExportNotFound`, underlined at the exposed name alone —
/// not the whole header.
///
/// Mutation-checked by reverting the `Lower` arm of `do_exports` to accept the
/// name unconditionally (the pre-fix behaviour): this test goes red because
/// `canonicalize_standalone` starts returning `Ok` again.
#[test]
fn export_nonexistent_value_is_error() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (nonexistent)
        x = 42
    "#};

    let errors =
        canonicalize_standalone(source).expect_err("exposing an undeclared value must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::ExportNotFound(name, kind, _) => {
            assert_eq!(name.as_str(), "nonexistent");
            assert_eq!(*kind, canonical::ExportType::Value);
        }
        other => panic!("expected ExportNotFound, got {:?}", other),
    }

    let identifier = "nonexistent";
    let start = source
        .find(identifier)
        .expect("source names it in the header");

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + identifier.len()),
        "the caret must sit under the exposed name alone, not the whole header"
    );
}

/// The same check for a type named in `exposing (...)` that the module never
/// declares. `Upper`'s two `Privacy` arms both check existence with
/// `find_type` — this covers the private (bare) spelling.
///
/// Mutation-checked by reverting the `Upper` arms of `do_exports` to accept
/// the name unconditionally: this test goes red because `canonicalize_standalone`
/// starts returning `Ok` again.
#[test]
fn export_nonexistent_type_is_error() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (Missing)
        x = 42
    "#};

    let errors =
        canonicalize_standalone(source).expect_err("exposing an undeclared type must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::ExportNotFound(name, kind, _) => {
            assert_eq!(name.as_str(), "Missing");
            assert_eq!(*kind, canonical::ExportType::UnionPrivate);
        }
        other => panic!("expected ExportNotFound, got {:?}", other),
    }

    let identifier = "Missing";
    let start = source
        .find(identifier)
        .expect("source names it in the header");

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + identifier.len()),
        "the caret must sit under the exposed name alone, not the whole header"
    );
}

// ── Scenario 7b: an exposed value with no annotation (`BUG-14`) ─────────────

/// `SPEC-5`: a value named in the `exposing` list must carry a type
/// annotation. `label` has none, so exposing it is rejected at the
/// declaration rather than silently dropped from the interface.
///
/// Mutation-checked: dropping the `values.get(name)` match arm in
/// `do_exports`'s `Lower` case (falling straight through to
/// `Ok((name.clone(), ExportType::Value))`, the pre-fix behaviour) turns this
/// red — `canonicalize_standalone` starts returning `Ok` again.
#[test]
fn exposed_value_without_annotation_is_error() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (label)
        label = 1
    "#};

    let errors = canonicalize_standalone(source)
        .expect_err("exposing an unannotated value must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::ExportedValueNotAnnotated(name, _, _) => {
            assert_eq!(name.as_str(), "label");
        }
        other => panic!("expected ExportedValueNotAnnotated, got {:?}", other),
    }

    // Primary label under the name in the `exposing` list; secondary under
    // the declaration itself, which is `Value::span()` — the whole
    // `label = 1` line, annotation and body together (there is none of the
    // former here), not just the name.
    let identifier = "label";
    let header_start = source
        .find(identifier)
        .expect("source names it in the header");
    let declaration = "label = 1";
    let declaration_start = source
        .find(declaration)
        .expect("source declares it on its own line");

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 2, "expected two labels, got {:?}", labels);
    assert!(labels[0].primary, "the first label must be the primary one");
    assert_eq!(
        labels[0].span.to_range(),
        header_start..(header_start + identifier.len()),
        "the primary caret must sit under the name in the exposing list"
    );
    assert!(!labels[1].primary, "the second label must be secondary");
    assert_eq!(
        labels[1].span.to_range(),
        declaration_start..(declaration_start + declaration.len()),
        "the secondary caret must sit under the declaration itself"
    );
}

/// The same declaration, kept private: `SPEC-5` only constrains what crosses
/// the module boundary, so an unannotated value that nothing exposes still
/// compiles.
#[test]
fn private_value_without_annotation_compiles() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        label = 1
    "#};

    let module = canonicalize_standalone(source)
        .expect("a private, unannotated declaration must still compile");
    assert_eq!(
        module.values.get(&"label".into()).unwrap(),
        &canonical::Value::Value {
            span: NodeSpan::none(),
            name: "label".into(),
            patterns: vec![],
            body: c_int(1),
        }
    );
}

/// `exposing (..)` exposes every top-level declaration, so `SPEC-5` applies
/// to all of them — not just a name written out individually. There is no
/// per-name span to blame here (`Exposing::Open` carries none), so the
/// declaration itself is the error's only label.
///
/// Mutation-checked: replacing the `Open` arm's `collect_accumulate` check
/// with a bare `Ok(Exports::Everything)` (the pre-fix behaviour) turns this
/// red.
#[test]
fn open_exposing_with_unannotated_declaration_is_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        label = 1
    "#};

    let errors = canonicalize_standalone(source)
        .expect_err("exposing (..) over an unannotated declaration must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::ExportedValueNotAnnotated(name, exposed_span, declared_span) => {
            assert_eq!(name.as_str(), "label");
            assert_eq!(
                exposed_span.span(),
                None,
                "exposing (..) names nothing individually, so there is no exposing-list span"
            );
            assert!(
                declared_span.span().is_some(),
                "the declaration's own span must still be real"
            );
        }
        other => panic!("expected ExportedValueNotAnnotated, got {:?}", other),
    }
}

// ── Scenario 8: `module foreign` facade ──────────────────────────────────────

#[test]
fn foreign_facade_module() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (add)
        add : Int -> Int -> Int
    "#};
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    // A facade's values get a placeholder body of Bool(true) (see TODO in
    // canonical/mod.rs — the compiler doesn't yet have a dedicated binding
    // expression variant).
    assert_eq!(
        module.values.get(&"add".into()).unwrap(),
        &canonical::Value::TypedValue {
            marked_unsafe: false,
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
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"pair".into()).unwrap(),
        &canonical::Value::TypedValue {
            marked_unsafe: false,
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "pair".into(),
            patterns: vec![],
            body: c_tuple(Tuple::two(c_int(1), c_char('a'),)),
            tpe: canonical::Type::Tuple(Tuple::two(int_t(), char_t())),
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
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"triple".into()).unwrap(),
        &canonical::Value::TypedValue {
            marked_unsafe: false,
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "triple".into(),
            patterns: vec![],
            body: c_tuple(Tuple::three(c_int(1), c_char('a'), c_int(3),)),
            tpe: canonical::Type::Tuple(Tuple::three(int_t(), char_t(), bool_t())),
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
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    let value = module.values.get(&"first".into()).unwrap();
    let patterns = match value {
        canonical::Value::TypedValue { patterns, .. } => patterns,
        other => panic!("expected TypedValue, got {:?}", other),
    };

    assert_eq!(
        patterns,
        &vec![(
            p_tuple(Tuple::two(p_var("a"), p_var("b"),)),
            canonical::Type::Tuple(Tuple::two(int_t(), char_t())),
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
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    let value = module.values.get(&"first".into()).unwrap();
    let patterns = match value {
        canonical::Value::TypedValue { patterns, .. } => patterns,
        other => panic!("expected TypedValue, got {:?}", other),
    };

    assert_eq!(
        patterns,
        &vec![(
            p_tuple(Tuple::three(p_var("a"), p_var("b"), p_var("c"),)),
            canonical::Type::Tuple(Tuple::three(int_t(), char_t(), bool_t())),
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

    // `Maybe`'s interface declares one type variable (`maybe_interface`'s `unions`
    // entry), so `Maybe a` resolves to a one-argument application — `BUG-17` — with
    // the written argument (`a`, the variable in this annotation) surviving rather
    // than being replaced by the declaration's own.
    let maybe_t = canonical::Type::Type(
        qual("Maybe.Maybe"),
        vec![canonical::Type::Variable("a".into())],
    );

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
        tpe: qual("Maybe.Maybe"),
    };
    assert_eq!(branches[0].pattern, p_ctor(just_ctor, vec![p_var("x")]));

    let nothing_ctor = canonical::TypeConstructor {
        name: "Nothing".into(),
        type_parameters: vec![],
        tpe: qual("Maybe.Maybe"),
    };
    assert_eq!(branches[1].pattern, p_ctor(nothing_ctor, vec![]));
}

// ── Extra: a type application's arity is checked ──────────────────────────────
//
// `BUG-17`: once `Type::from_parser_type`'s `Some` arm applies the written
// arguments instead of discarding them, there is something to count them against
// — the declaration's own arity — and a mismatch is an error rather than silently
// accepted.

/// `Maybe` takes exactly one argument; writing none is rejected.
///
/// Mutation-checked by reverting the `Some` arm of `Type::from_parser_type` to
/// `Ok(t.clone())` (the pre-fix behaviour, which returns the environment's stored
/// `Maybe a` verbatim and never looks at `args`): this test goes red because
/// `canonicalize_standalone` starts returning `Ok` again.
#[test]
fn type_application_with_too_few_arguments_is_an_arity_error() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a = Just a | Nothing
        bare : Maybe
        bare = Nothing
    "#};

    let errors =
        canonicalize_standalone(source).expect_err("Maybe with no argument should be an error");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::TypeArityMismatch(name, declared, written, _) => {
            assert_eq!(name.as_str(), "Maybe");
            assert_eq!(*declared, 1, "Maybe declares one type variable");
            assert_eq!(*written, 0, "bare : Maybe supplies none");
        }
        other => panic!("expected TypeArityMismatch, got {:?}", other),
    }

    let annotation = "bare : Maybe";
    let start = source.find(annotation).expect("source declares `bare`") + "bare : ".len();

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + "Maybe".len()),
        "the caret must sit under the application, not the whole annotation"
    );
}

/// `Maybe` takes exactly one argument; writing two is rejected the same way as
/// writing none.
#[test]
fn type_application_with_too_many_arguments_is_an_arity_error() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a = Just a | Nothing
        type Size = Small
        tooMany : Maybe Size Size
        tooMany = Nothing
    "#};

    let errors = canonicalize_standalone(source)
        .expect_err("Maybe applied to two arguments should be an error");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::TypeArityMismatch(name, declared, written, _) => {
            assert_eq!(name.as_str(), "Maybe");
            assert_eq!(*declared, 1, "Maybe declares one type variable");
            assert_eq!(*written, 2, "Maybe Size Size supplies two");
        }
        other => panic!("expected TypeArityMismatch, got {:?}", other),
    }

    let application = "Maybe Size Size";
    let start = source
        .find(application)
        .expect("source annotates `tooMany` with it");

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + application.len()),
        "the caret must cover the whole application, both extra arguments included"
    );
}

/// An *opaque* import — the type without its constructors — still carries the
/// declaration's arity, so applying it to the argument it declares is not an error.
///
/// `import Maybe exposing (Maybe)` goes through the `Privacy::Private` arm of
/// `process_import`, which is the only arm that does not read the union out of the
/// interface. Recording zero variables there would make every later use of the name
/// be measured against arity 0, and `Maybe a` — a perfectly ordinary annotation —
/// would be rejected as an arity mismatch.
///
/// Mutation-checked by putting `variables: vec![]` back in the `TypeArity` that
/// arm builds: this test goes red with `TypeArityMismatch(Maybe, 0, 1)`.
#[test]
fn opaque_import_of_a_parameterised_type_keeps_its_arity() {
    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Maybe exposing (Maybe)
        f : Maybe a -> Maybe a
        f m = m
    "#};

    let module = canonicalize_with_interfaces(source, &interfaces)
        .expect("an opaque `Maybe` applied to one argument should canonicalize");

    let maybe_a = canonical::Type::Type(
        qual("Maybe.Maybe"),
        vec![canonical::Type::Variable("a".into())],
    );

    match module.values.get(&"f".into()).unwrap() {
        canonical::Value::TypedValue { tpe, .. } => assert_eq!(
            *tpe,
            canonical::Type::Arrow(Box::new(maybe_a.clone()), Box::new(maybe_a))
        ),
        other => panic!("expected a TypedValue, got {:?}", other),
    }
}

/// A qualified and an unqualified spelling of one type are one type.
///
/// The environment inserts a union under every name an import makes available for
/// it — `Maybe.Maybe` and `Maybe` here — and `Type::from_parser_type` builds the
/// canonical head out of the *declaration's* name rather than the one written. Were
/// it to keep the written name, `Maybe.Maybe Int` and `Maybe Int` would be two
/// distinct types and would not unify downstream.
///
/// Mutation-checked by building the head out of the written name — the
/// `name.to_qual().unwrap_or_else(|| env.module_name().qualify_name(name))` the
/// `None` arm uses — instead of `declared.name`: this test goes red, the two arms
/// coming back as `Maybe.Maybe` and `Test.Maybe`.
#[test]
fn qualified_and_unqualified_spellings_canonicalize_to_one_head() {
    let mut interfaces = scalar_interfaces();
    let (iface_name, iface) = maybe_interface();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Maybe exposing (Maybe(..))
        f : Maybe.Maybe Int -> Maybe Int
        f m = m
    "#};

    let module = canonicalize_with_interfaces(source, &interfaces)
        .expect("both spellings of `Maybe` should canonicalize");

    let maybe_int = canonical::Type::Type(qual("Maybe.Maybe"), vec![int_t()]);

    match module.values.get(&"f".into()).unwrap() {
        canonical::Value::TypedValue { tpe, .. } => assert_eq!(
            *tpe,
            canonical::Type::Arrow(Box::new(maybe_int.clone()), Box::new(maybe_int)),
            "the qualified spelling must normalize to the declaration's own name"
        ),
        other => panic!("expected a TypedValue, got {:?}", other),
    }
}

// ── AST-4: a canonical type names the module that declared it ────────────────

/// A type reached through an import alias records the module that declared it.
///
/// `import Maybe as M` makes `M.Maybe` a spelling; the declaration behind it is
/// still `Maybe`'s, so that is the head. An alias names a route to a declaration
/// rather than a second declaration, which is the rule name resolution already
/// states for values.
///
/// Mutation-checked by having `insert_foreign_union_type` record
/// `env.module_name.qualify_name(union_name)` — the *importing* module — in place
/// of the declaring one: this test goes red with a head of `Test.Maybe`.
#[test]
fn a_type_imported_under_an_alias_records_the_declaring_module() {
    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Maybe as M
        f : M.Maybe a -> M.Maybe a
        f m = m
    "#};

    let module = canonicalize_with_interfaces(source, &interfaces)
        .expect("an aliased `Maybe` should canonicalize");

    let maybe_a = canonical::Type::Type(
        qual("Maybe.Maybe"),
        vec![canonical::Type::Variable("a".into())],
    );

    match module.values.get(&"f".into()).unwrap() {
        canonical::Value::TypedValue { tpe, .. } => assert_eq!(
            *tpe,
            canonical::Type::Arrow(Box::new(maybe_a.clone()), Box::new(maybe_a)),
            "the alias `M` names a route to `Maybe`, not a module of its own"
        ),
        other => panic!("expected a TypedValue, got {:?}", other),
    }
}

/// Two modules declaring one type name declare two types.
///
/// `Widget.Size` and the module under check's own `Size` share four letters and
/// nothing else: each `type` declaration introduces a genuinely new type, and a
/// canonical type that held only the spelling made these one value. The interface
/// here is built by canonicalizing `Widget` for real, so what is asserted is that
/// the declaring module survives the interface boundary as well as the annotation.
///
/// Mutation-checked by throwing the resolved module away again in
/// `Type::from_parser_type`'s `Some` arm —
/// `env.module_name().qualify_name(&declared.name.unqualified_name())`, which is
/// the old bare-`Name` behaviour spelled in the new type: both sides of the arrow
/// come back `Test.Size` and this test goes red.
#[test]
fn two_modules_declaring_one_type_name_are_two_types() {
    let widget = canonicalize_standalone(indoc::indoc! {r#"
        module Widget exposing (Size(..))
        type Size = Small
    "#})
    .expect("Widget should canonicalize");

    let mut interfaces = HashMap::new();
    interfaces.insert(widget.name.name().clone(), widget.to_interface(None));

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Widget
        type Size = Big
        resize : Size -> Widget.Size
        resize s = s
    "#};

    let module = canonicalize_with_interfaces(source, &interfaces)
        .expect("a module declaring its own `Size` alongside `Widget`'s should canonicalize");

    let own = canonical::Type::Type(qual("Test.Size"), vec![]);
    let widgets = canonical::Type::Type(qual("Widget.Size"), vec![]);

    match module.values.get(&"resize".into()).unwrap() {
        canonical::Value::TypedValue { tpe, .. } => assert_eq!(
            *tpe,
            canonical::Type::Arrow(Box::new(own), Box::new(widgets)),
            "each side of the arrow names the module that declared its type"
        ),
        other => panic!("expected a TypedValue, got {:?}", other),
    }
}

/// A qualified type name that resolves to nothing is reported, and the name the
/// error carries is the whole written spelling.
///
/// `import Widget as W` followed by `W.Thing` names no declaration — `Widget`
/// declares no `Thing` — and `W` is a route rather than a module, so there is no
/// module half to peel off and report separately. Quoting the spelling back is
/// what lets the reader find it in their own source.
///
/// Mutation-checked by restoring the `None` arm's
/// `Ok(Type::Type(env.module_name().qualify_name(name), args))`: the module
/// canonicalizes cleanly again and `expect_err` panics.
#[test]
fn an_unresolved_qualified_type_name_is_reported_as_written() {
    let widget = canonicalize_standalone(indoc::indoc! {r#"
        module Widget exposing (Size(..))
        type Size = Small
    "#})
    .expect("Widget should canonicalize");

    let mut interfaces = HashMap::new();
    interfaces.insert(widget.name.name().clone(), widget.to_interface(None));

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Widget as W
        f : W.Thing -> W.Thing
        f x = x
    "#};

    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("`W.Thing` names no declaration");
    // `from_parser_type` walks the annotation with `?`, so one annotation
    // yields one error however many times it names the type.
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::TypeNotFound(name, _) => assert_eq!(
            name.as_str(),
            "W.Thing",
            "the alias is part of the spelling, not a module to be peeled off"
        ),
        other => panic!("expected TypeNotFound, got {:?}", other),
    }
}

// ── BUG-16: a type name that resolves to nothing is an error ─────────────────

/// An annotation naming a type nothing in scope declares is rejected, and the
/// caret sits under the name.
///
/// Mutation-checked by restoring the `None` arm's
/// `Ok(Type::Type(env.module_name().qualify_name(name), args))`: the module
/// canonicalizes cleanly again and `expect_err` panics.
#[test]
fn an_undeclared_type_name_in_an_annotation_is_error() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (label)
        label : Nope
        label = 1
    "#};

    let errors = canonicalize_standalone(source).expect_err("`Nope` is declared nowhere");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::TypeNotFound(name, _) => assert_eq!(name.as_str(), "Nope"),
        other => panic!("expected TypeNotFound, got {:?}", other),
    }

    let start = source.find("Nope").expect("source names `Nope`");
    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + "Nope".len()),
        "the caret must sit under the name that resolved to nothing"
    );
}

/// A type *variable* resolves through nothing and is not an unresolved type
/// name: it is bound by the annotation it appears in.
///
/// Mutation-checked by raising `TypeNotFound` from `from_parser_type`'s
/// `TypeKind::Variable` arm as well: this test goes red and
/// `typed_identity_function` with it.
#[test]
fn a_type_variable_is_not_an_undeclared_type_name() {
    let source = indoc::indoc! {r#"
        module Test exposing (label)
        label : a -> a
        label x = x
    "#};

    canonicalize_standalone(source).expect("a type variable resolves to nothing by design");
}

/// A `type` declaration may name any type its own module declares, itself
/// included — `type Never = JustOneMore Never` is `Basics`' own.
///
/// Mutation-checked by deleting the `insert_declared_type` loop `canonicalize`
/// runs before `do_types`: both declarations are then canonicalized against an
/// environment that has not heard of either, and each variant's argument is a
/// `TypeNotFound`.
#[test]
fn a_type_declaration_may_name_its_own_module_s_types() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Never = JustOneMore Never
        type Chain = Link Never
    "#};

    let module = canonicalize_standalone(source)
        .expect("a declaration names the types its own module declares");

    let never = canonical::Type::Type(qual("Test.Never"), vec![]);

    let argument_of = |type_name: &str, variant: usize| {
        module.types.get(&type_name.into()).unwrap().variants[variant]
            .type_parameters
            .clone()
    };

    assert_eq!(
        argument_of("Never", 0),
        vec![never.clone()],
        "a declaration names itself"
    );
    assert_eq!(
        argument_of("Chain", 0),
        vec![never],
        "a declaration names a sibling written above it"
    );
}

/// The same, for a sibling declared *below* the one naming it — a file's
/// declarations are one scope rather than a sequence.
///
/// Mutation-checked the same way: with the pre-registration loop gone, `Flag`
/// is unknown at the point `Holder` is canonicalized and this goes red while
/// the test above keeps passing on its `Chain` half only by accident of order.
#[test]
fn a_type_declaration_may_name_a_type_declared_below_it() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Holder = Holds Flag
        type Flag = Up | Down
    "#};

    let module =
        canonicalize_standalone(source).expect("a declaration names a sibling written below it");

    assert_eq!(
        module.types.get(&"Holder".into()).unwrap().variants[0].type_parameters,
        vec![canonical::Type::Type(qual("Test.Flag"), vec![])]
    );
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

// ── ERR-7: "did you mean …?" on unresolved names ─────────────────────────────
//
// A typo'd name that is one edit away from something in scope gets a suggestion
// hung off the same label the caret already sits under (`labels[0].message`); a
// name resembling nothing in scope gets none — a wrong suggestion is worse than
// silence, since it sends the reader to check something irrelevant.

/// `Error::VariableNotFound`: a one-character typo of an unqualified, explicitly
/// exposed import suggests the name it is one edit away from.
///
/// Mutation-checked by reverting `Expression::from_parser`'s `Variable` arm to
/// build `Error::VariableNotFound(.., e.span, None)` unconditionally (skipping the
/// `suggest_name` call): this test goes red because `suggestion` becomes `None`.
#[test]
fn unresolved_variable_suggests_a_near_miss() {
    use zelkova_lang::compiler::PhaseError;

    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Maybe exposing (withDefault)
        answer = widthDefault
    "#};

    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("a typo'd variable should not resolve");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::VariableNotFound(name, _, suggestion) => {
            assert_eq!(name.unqualified_name(), "widthDefault".into());
            assert_eq!(suggestion.as_ref().map(|n| n.as_str()), Some("withDefault"));
        }
        other => panic!("expected VariableNotFound, got {:?}", other),
    }

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert!(
        labels[0].message.contains("did you mean `withDefault`?"),
        "the label should carry the suggestion, got {:?}",
        labels[0].message
    );
}

/// `Error::VariableNotFound`: a name resembling nothing in scope — not the
/// imported `withDefault`, not the module's own `answer` — gets no suggestion.
///
/// Mutation-checked by widening `utils::suggest`'s threshold to always accept
/// (e.g. `usize::MAX`): this test goes red because `suggestion` stops being
/// `None`.
#[test]
fn unresolved_variable_with_no_near_miss_has_no_suggestion() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer = 42
        mystery = zzzzzzzzzzzz
    "#};

    let errors = canonicalize_standalone(source).expect_err("an undefined name should not resolve");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::VariableNotFound(_, _, suggestion) => {
            assert_eq!(
                *suggestion, None,
                "an unrelated name must not produce a suggestion"
            );
        }
        other => panic!("expected VariableNotFound, got {:?}", other),
    }
}

/// `Error::VariantNotFound`: a one-character typo of a locally declared
/// constructor suggests the name it is one edit away from.
///
/// Mutation-checked by reverting `Pattern::from_parser`'s `Constructor` arm to
/// build `Error::VariantNotFound(.., p.span, None)` unconditionally: this test
/// goes red because `suggestion` becomes `None`.
#[test]
fn unresolved_constructor_suggests_a_near_miss() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Color = Red | Green | Blue
        isRed Reed = true
    "#};

    let errors = canonicalize_standalone(source)
        .expect_err("a typo'd constructor pattern should not resolve");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::VariantNotFound(name, _, suggestion) => {
            assert_eq!(name.unqualified_name(), "Reed".into());
            assert_eq!(suggestion.as_ref().map(|n| n.as_str()), Some("Red"));
        }
        other => panic!("expected VariantNotFound, got {:?}", other),
    }
}

/// `Error::VariantNotFound`: a constructor name resembling none of `Red`,
/// `Green` or `Blue` gets no suggestion.
#[test]
fn unresolved_constructor_with_no_near_miss_has_no_suggestion() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Color = Red | Green | Blue
        isRed Zzzzzzzzzzzz = true
    "#};

    let errors = canonicalize_standalone(source)
        .expect_err("an undeclared constructor pattern should not resolve");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::VariantNotFound(_, _, suggestion) => {
            assert_eq!(
                *suggestion, None,
                "an unrelated name must not produce a suggestion"
            );
        }
        other => panic!("expected VariantNotFound, got {:?}", other),
    }
}

/// `EnvError::InterfaceNotFound`, reached through `canonicalize` rather than
/// `new_environment` directly, so the label carries a real span and the
/// suggestion is asserted the same way as the parser-backed tests above.
#[test]
fn unresolved_import_module_suggests_a_near_miss() {
    use zelkova_lang::compiler::PhaseError;

    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Mabye
        answer = 1
    "#};

    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("an unknown module import should not resolve");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert!(
        labels[0].message.contains("did you mean `Maybe`?"),
        "expected a suggestion naming `Maybe`, got {:?}",
        labels[0].message
    );
}

/// `EnvError::ValueNotFound`, through `canonicalize`: an exposing-list typo of a
/// value the imported module does declare.
#[test]
fn unresolved_import_exposed_value_suggests_a_near_miss() {
    use zelkova_lang::compiler::PhaseError;

    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Maybe exposing (widthDefault)
        answer = 1
    "#};

    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("an unknown exposed value should not resolve");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert!(
        labels[0].message.contains("did you mean `withDefault`?"),
        "expected a suggestion naming `withDefault`, got {:?}",
        labels[0].message
    );
}

/// `EnvError::UnionNotFound`, through `canonicalize`, for a *bare* type entry —
/// the opaque form, which asks for the type without its constructors (`BUG-16`).
///
/// `Size` and `Size(..)` are one entry differing only in whether the constructors
/// come along, so both check the name against the interface and both point the
/// caret at the entry rather than at the `import` line (`ERR-9`).
///
/// Mutation-checked by restoring the arm's old body in `process_import` — reading
/// the variables with `.map(..).unwrap_or_default()` and inserting regardless:
/// the module then canonicalizes and `expect_err` panics.
#[test]
fn unresolved_opaque_import_exposed_type_suggests_a_near_miss() {
    use zelkova_lang::compiler::PhaseError;

    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Maybe exposing (Mayeb)
        answer = 1
    "#};

    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("an unknown opaquely exposed type should not resolve");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    assert_eq!(
        errors[0].message(),
        "the imported module does not expose a type named `Mayeb`"
    );

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert!(
        labels[0].message.contains("did you mean `Maybe`?"),
        "expected a suggestion naming `Maybe`, got {:?}",
        labels[0].message
    );

    // The caret covers `Mayeb` alone, not the `import` line it sits on.
    let start = source.find("Mayeb").unwrap();
    assert_eq!(labels[0].span.to_range(), start..(start + "Mayeb".len()));
}

// ── Scenario 12: Infix re-association (BUG-22) ───────────────────────────────
//
// `InfixExpr` parses a flat run of operator applications (`a * b + c` is one
// node, not a tree); canonicalization re-associates it into nested
// `Application` nodes using each operator's declared precedence and
// associativity. The precedences and associativities below mirror
// `Basics.zel`'s own: `*` at 7 binds tighter than `+`/`-` at 6, `+`/`-` are
// `infix left`, `++` is `infix right`, `==` is `infix non`.

/// A trimmed-down view of a canonicalized infix expression, asserting nesting
/// only — which grouping re-association produced is what BUG-22 is about. `Op`
/// recognizes the `Apply(Apply(operator, lhs), rhs)` shape re-association
/// builds for one step, and carries the name in that operator position: the
/// *function* the operator's `infix` declaration points at (`sub` for `-`), not
/// the symbol, since that is what an operator resolves to as a value. `Var` is
/// a `VarLocal` leaf — every operand in these tests is a bare function
/// parameter — and `Lit` is an integer leaf, which only the `0` prefix negation
/// desugars to ever produces.
#[derive(Debug, PartialEq)]
enum Shape {
    Var(String),
    Lit(i64),
    Op(String, Box<Shape>, Box<Shape>),
}

fn infix_shape(e: &canonical::Expression) -> Shape {
    match &e.kind {
        canonical::ExpressionKind::VarLocal(n) => Shape::Var(n.to_string()),
        canonical::ExpressionKind::Int(i) => Shape::Lit(*i),
        canonical::ExpressionKind::Apply(f, rhs) => match &f.kind {
            canonical::ExpressionKind::Apply(op, lhs) => {
                let op_name = match &op.kind {
                    canonical::ExpressionKind::VarTopLevel(q) => q.unqualified_name().to_string(),
                    canonical::ExpressionKind::VarLocal(n) => n.to_string(),
                    other => panic!(
                        "expected the operator to resolve to a value, got {:?}",
                        other
                    ),
                };
                Shape::Op(op_name, Box::new(infix_shape(lhs)), Box::new(infix_shape(rhs)))
            }
            other => panic!(
                "expected a partial application (the operator applied to its left operand), got {:?}",
                other
            ),
        },
        other => panic!("expected a variable or an infix application, got {:?}", other),
    }
}

fn op(name: &str, lhs: Shape, rhs: Shape) -> Shape {
    Shape::Op(name.to_string(), Box::new(lhs), Box::new(rhs))
}

fn var(name: &str) -> Shape {
    Shape::Var(name.to_string())
}

/// `-x`, as the grammar desugars it: subtraction from a literal `0`. Named for
/// `ADD_SUB_MUL`'s backing function, which is what the `-` position resolves to.
fn neg(operand: Shape) -> Shape {
    op("sub", Shape::Lit(0), operand)
}

/// Canonicalizes `chain a b c = <chain_body>` against the `infix` declarations
/// in `preamble`, and returns the re-associated shape of `chain`'s body.
fn infix_chain_shape(preamble: &str, chain_body: &str) -> Shape {
    // `exposing ()`, not `(..)`: every function these preambles declare (the
    // infix-backing ones, `chain` itself) is left without a type annotation on
    // purpose, to keep `chain_shape`'s `Value::Value` match below meaningful —
    // `exposing (..)` would expose them unannotated, which `SPEC-5` now rejects.
    let source = format!(
        "module Test exposing ()\n{}\nchain a b c =\n  {}\n",
        preamble, chain_body
    );

    chain_shape(&source)
}

/// The same, for a body that has to be written on the declaration's own line.
fn infix_chain_shape_one_line(preamble: &str, chain_body: &str) -> Shape {
    let source = format!(
        "module Test exposing ()\n{}\nchain a b c = {}\n",
        preamble, chain_body
    );

    chain_shape(&source)
}

/// The re-associated shape of `chain`'s body in an already-assembled module.
fn chain_shape(source: &str) -> Shape {
    let module = canonicalize_standalone(source).expect("should canonicalize");
    let body = match module.values.get(&"chain".into()).unwrap() {
        canonical::Value::Value { body, .. } => body,
        other => panic!("expected an untyped Value, got {:?}", other),
    };
    infix_shape(body)
}

/// `+`/`-` at precedence 6, `*` at precedence 7, all `infix left` — matching
/// `Basics.zel`.
const ADD_SUB_MUL: &str = indoc::indoc! {r#"
    infix left 6 (+) = add
    infix left 6 (-) = sub
    infix left 7 (*) = mul

    add a b = a
    sub a b = a
    mul a b = a
"#};

#[test]
fn higher_precedence_groups_first() {
    // `*` (7) binds tighter than `+` (6): `a * b + c` is `(a * b) + c`, not
    // `a * (b + c)` — the exact grouping `BUG-22` describes as wrong.
    //
    // Mutation-checked: making `reassociate_infix_chain`'s "strictly lower
    // precedence" arm (`next_op.infix.precedence < op.infix.precedence`)
    // return `Some(0)` instead of `None` — folding `+` into `*`'s right operand
    // regardless, the pre-fix behaviour — turned this red, producing
    // `a * (b + c)`.
    assert_eq!(
        infix_chain_shape(ADD_SUB_MUL, "a * b + c"),
        op("add", op("mul", var("a"), var("b")), var("c")),
    );
}

#[test]
fn lower_precedence_on_the_left_still_yields_to_the_higher_one_on_the_right() {
    // The same table, mixed the other way: `a + b * c` is `a + (b * c)`, not
    // `(a + b) * c`. `higher_precedence_groups_first` alone cannot catch a
    // mutation that always folds left-to-right — that mutation happens to
    // produce the right shape for `a * b + c` — so this is the one that does.
    //
    // Mutation-checked: forcing `reassociate_infix_chain`'s inner loop to
    // always `break` (never recurse into a tighter-binding `rhs`) turned this
    // red, producing `(a + b) * c` instead.
    assert_eq!(
        infix_chain_shape(ADD_SUB_MUL, "a + b * c"),
        op("add", var("a"), op("mul", var("b"), var("c"))),
    );
}

#[test]
fn infix_left_groups_leftward() {
    // `-` is `infix left`: `a - b - c` is `(a - b) - c`, not `a - (b - c)`.
    //
    // Mutation-checked: changing the `(Left, Left)` arm of the equal-precedence
    // match to return `Some(op.infix.precedence)` (the `(Right, Right)`
    // treatment) instead of `None` turned this red, producing `a - (b - c)`.
    assert_eq!(
        infix_chain_shape(ADD_SUB_MUL, "a - b - c"),
        op("sub", op("sub", var("a"), var("b")), var("c")),
    );
}

#[test]
fn infix_right_groups_rightward() {
    // `++` is `infix right`: `a ++ b ++ c` is `a ++ (b ++ c)`, not
    // `(a ++ b) ++ c`.
    //
    // Mutation-checked: changing the `(Right, Right)` arm to return `None` (the
    // `(Left, Left)` treatment) turned this red, producing `(a ++ b) ++ c`.
    let preamble = indoc::indoc! {r#"
        infix right 5 (++) = append

        append a b = a
    "#};
    assert_eq!(
        infix_chain_shape(preamble, "a ++ b ++ c"),
        op("append", var("a"), op("append", var("b"), var("c"))),
    );
}

#[test]
fn infix_non_chained_with_itself_is_an_ambiguous_precedence_error() {
    // `==` is `infix non`: two of them in a row, `a == b == c`, has no
    // unambiguous grouping and is rejected rather than guessed at.
    //
    // Mutation-checked: adding a `(Associativity::None, Associativity::None) =>
    // None` arm ahead of the catch-all (falling back to `(Left, Left)`'s
    // "just fold left" treatment) turned this green when it should stay red —
    // confirming the catch-all, not a missing case, is what rejects this.
    let source = indoc::indoc! {r#"
        module Test exposing (..)

        infix non 4 (==) = eq

        eq a b = a

        chain a b c =
          a == b == c
    "#};

    assert_ambiguous_pair(source, "==", "==");
}

/// Canonicalizes `source`, and asserts it was rejected with exactly one
/// `AmbiguousOperatorPrecedence` naming `left` and `right` in that order.
fn assert_ambiguous_pair(source: &str, left: &str, right: &str) {
    let errors = canonicalize_standalone(source).expect_err("should reject");
    assert_eq!(errors.len(), 1, "got {:?}", errors);
    match &errors[0] {
        canonical::Error::AmbiguousOperatorPrecedence(l, r, _) => {
            assert_eq!(l.name, zelkova_lang::compiler::name::Name::from(left));
            assert_eq!(r.name, zelkova_lang::compiler::name::Name::from(right));
        }
        other => panic!("expected AmbiguousOperatorPrecedence, got {:?}", other),
    }
}

#[test]
fn infix_left_against_infix_right_at_equal_precedence_is_rejected() {
    // The case the *Equal precedence, disagreeing associativity* rule is
    // actually named after, and the one reachable from `Basics`: `<<` is
    // `infix left 9` and `>>` is `infix right 9`, so `a << b >> c` groups
    // neither way and is rejected rather than guessed at.
    //
    // This is a different arm of the catch-all from
    // `infix_non_chained_with_itself_is_an_ambiguous_precedence_error`, which
    // only reaches `(None, None)`. Mutation-checked: adding an
    // `(Associativity::Left, Associativity::Right) => None` arm ahead of the
    // catch-all — the "fold left and carry on" guess — turned this red while
    // leaving the `infix non` test green, which is exactly the mutation the
    // `non` test alone cannot see.
    let source = indoc::indoc! {r#"
        module Test exposing (..)

        infix left 9 (<<) = composeL

        infix right 9 (>>) = composeR

        composeL a b = a

        composeR a b = a

        chain a b c =
          a << b >> c
    "#};

    assert_ambiguous_pair(source, "<<", ">>");
}

#[test]
fn two_different_infix_non_operators_are_rejected_and_say_why() {
    // `Basics` declares `<`, `>`, `==`, `/=`, `<=` and `>=` all at `infix non
    // 4`, so `a < b > c` is the everyday way to reach this error — two
    // *different* operators that agree completely (neither chains) rather than
    // disagreeing about which side groups first.
    //
    // Mutation-checked on the message: restoring the single `left != right`
    // branch ("have the same precedence but disagree on which side groups
    // first") turned the message assertion red.
    let source = indoc::indoc! {r#"
        module Test exposing (..)

        infix non 4 (<) = lt

        infix non 4 (>) = gt

        lt a b = a

        gt a b = a

        chain a b c =
          a < b > c
    "#};

    use zelkova_lang::compiler::PhaseError;

    assert_ambiguous_pair(source, "<", ">");

    let errors = canonicalize_standalone(source).expect_err("should reject");
    let message = errors[0].message();
    assert!(
        message.contains("both declared `infix non`"),
        "the message must say neither operator chains, not that they disagree; got {:?}",
        message
    );
    assert!(
        !message.contains("disagree"),
        "`<` and `>` do not disagree — both are `infix non`; got {:?}",
        message
    );
}

#[test]
fn prefix_negation_does_not_swallow_the_rest_of_the_chain() {
    // `-a + b` is `(-a) + b`, not `-(a + b)`. The two are different values —
    // `0 - (a + b)` is `-a - b` — so this is a miscompile and not a matter of
    // taste.
    //
    // Prefix negation desugars to `0 - e` in the grammar, which used to be built
    // over a whole `Expr`: the negation therefore took the entire run to its
    // right as its operand and never became part of the `InfixChain`, so
    // re-association never saw it. It is now an operand of the chain
    // (`OperandExpr`), which is what makes it bind tighter than `+`.
    //
    // Mutation-checked: moving the `"-" <e>` production back onto `Expr` (taking
    // `Expr` rather than `OperandExpr` as its operand) turns this red with
    // `-(a + b)`.
    assert_eq!(
        infix_chain_shape(ADD_SUB_MUL, "-a + b"),
        op("add", neg(var("a")), var("b")),
    );
}

#[test]
fn prefix_negation_binds_tighter_than_any_operator() {
    // The same rule where it is visible against a *higher* precedence: `*` is
    // `infix left 7`, above `-`'s 6, and negation still takes only `a`.
    // `-a * b` is `(-a) * b`.
    //
    // Mutation-checked alongside the test above: with the production back on
    // `Expr` this reads `-(a * b)`.
    assert_eq!(
        infix_chain_shape(ADD_SUB_MUL, "-a * b"),
        op("mul", neg(var("a")), var("b")),
    );
}

#[test]
fn repeated_prefix_negation_nests() {
    // `- -a` is the negation of the negation, `0 - (0 - a)`, and not the chain
    // `0 - 0 - a` that splicing a leading `-` into the run would produce — that
    // would group as `(0 - 0) - a`, i.e. `-a`, dropping one of the two
    // negations. Nothing above would catch it: both `-a + b` tests have a single
    // `-`.
    assert_eq!(
        infix_chain_shape_one_line(ADD_SUB_MUL, "- -a + b"),
        op("add", neg(neg(var("a"))), var("b")),
    );
}

// ── A variant is a constructor name and its arguments ─────────────────────────
//
// `BUG-18`: the grammar parses a `type` declaration's variants with the general
// `Type` production, so a tuple, an arrow or a bare name all reach `do_types`.
// Every one of the four tests below asserts on the `InvalidVariantKind` and on
// the caret's range, because "it is an error" is the cheap half of the claim —
// the message the user reads and the text it underlines are the other half.
//
// All four are mutation-checked the same way: put the pre-fix `filter_map` back
// in `do_types` — the one that kept `TypeKind::Unqualified` and answered `None`
// for everything else — and each goes red on `expect_err`, because the
// declaration canonicalizes with the variant gone. The fifth test guards the
// complement, so it needs the opposite mutation; its own comment says which.

/// The span a single-variant declaration's variant occupies in `source`, as the
/// byte range a label under it must have.
fn variant_range(source: &str, variant: &str) -> std::ops::Range<usize> {
    let start = source
        .find(variant)
        .unwrap_or_else(|| panic!("source should contain `{}`", variant));
    start..(start + variant.len())
}

/// The one label an `InvalidVariant` renders, for a source with exactly one error.
fn only_invalid_variant_label(errors: &[canonical::Error]) -> zelkova_lang::compiler::SpanLabel {
    use zelkova_lang::compiler::PhaseError;

    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);
    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    labels.into_iter().next().unwrap()
}

/// A lowercase name in variant position — the mistyped constructor — is rejected,
/// and says that a constructor name is capitalised.
#[test]
fn lowercase_name_in_variant_position_is_rejected() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Colour
          = red
    "#};

    let errors = canonicalize_standalone(source).expect_err("`red` is not a constructor name");

    match &errors[0] {
        canonical::Error::InvalidVariant(canonical::InvalidVariantKind::LowercaseName(n), _) => {
            assert_eq!(n.as_str(), "red")
        }
        other => panic!("expected InvalidVariant(LowercaseName), got {:?}", other),
    }

    assert!(
        errors[0].message().contains("uppercase"),
        "a lowercase name deserves the capitalisation message, got {:?}",
        errors[0].message()
    );

    assert_eq!(
        only_invalid_variant_label(&errors).span.to_range(),
        variant_range(source, "red"),
        "the caret must sit under the variant, not the whole declaration"
    );
}

/// A type variable in variant position is the same failure as any other lowercase
/// name, even when the declaration does bind that variable.
#[test]
fn type_variable_in_variant_position_is_rejected() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Bad a
          = a
    "#};

    let errors =
        canonicalize_standalone(source).expect_err("`a` is a type variable, not a variant");

    match &errors[0] {
        canonical::Error::InvalidVariant(canonical::InvalidVariantKind::LowercaseName(n), _) => {
            assert_eq!(n.as_str(), "a")
        }
        other => panic!("expected InvalidVariant(LowercaseName), got {:?}", other),
    }

    // `a` alone appears in `type Bad a` first, so the search anchors on the `=`.
    let start = source.find("= a").expect("source declares `Bad`") + "= ".len();
    assert_eq!(
        only_invalid_variant_label(&errors).span.to_range(),
        start..(start + "a".len()),
        "the caret must sit under the variant"
    );
}

/// A tuple type in variant position is rejected, with the caret over the
/// parenthesised type.
#[test]
fn tuple_in_variant_position_is_rejected() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Pair
          = (Int, Int)
    "#};

    let errors = canonicalize_standalone(source).expect_err("a tuple is not a variant");

    match &errors[0] {
        canonical::Error::InvalidVariant(canonical::InvalidVariantKind::Tuple, _) => (),
        other => panic!("expected InvalidVariant(Tuple), got {:?}", other),
    }

    assert_eq!(
        only_invalid_variant_label(&errors).span.to_range(),
        variant_range(source, "(Int, Int)"),
        "the caret must cover the whole tuple"
    );
}

/// A function type in variant position is rejected, and the caret covers the
/// constructor on the arrow's left too: `Wrap Int -> Int` is one `Arrow` node with
/// `Wrap Int` as its left operand, so there is no variant here to keep.
#[test]
fn function_type_in_variant_position_is_rejected() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Wrapper
          = Wrap Int -> Int
    "#};

    let errors = canonicalize_standalone(source).expect_err("an arrow is not a variant");

    match &errors[0] {
        canonical::Error::InvalidVariant(canonical::InvalidVariantKind::Arrow, _) => (),
        other => panic!("expected InvalidVariant(Arrow), got {:?}", other),
    }

    assert_eq!(
        only_invalid_variant_label(&errors).span.to_range(),
        variant_range(source, "Wrap Int -> Int"),
        "the caret must cover the whole function type, `Wrap` included"
    );
}

/// A declaration whose variants are constructor applications still canonicalizes:
/// the check rejects the other shapes and leaves this one alone.
///
/// Mutation-checked in the other direction from the four above — the pre-fix
/// `filter_map` keeps this green. Making the `TypeKind::Unqualified` arm of
/// `do_types` return an `InvalidVariant` too turns it red on `Just`.
#[test]
fn constructor_applications_remain_valid_variants() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a
          = Just a
          | Nothing
    "#};

    let module =
        canonicalize_standalone(source).expect("constructor applications are valid variants");

    let union = module
        .types
        .get(&"Maybe".into())
        .expect("Maybe should be declared");
    let names: Vec<_> = union
        .variants
        .iter()
        .map(|v| v.name.as_str().to_owned())
        .collect();
    assert_eq!(names, vec!["Just".to_owned(), "Nothing".to_owned()]);
}

// ── Scenario 13: `unsafe` on a facade signature ──────────────────────────────

/// The modifier survives the parser and canonicalization, landing on the value
/// the facade declares.
///
/// Verified to fail by pinning `marked_unsafe: false` at the facade branch's
/// `Value::TypedValue` in `canonical/mod.rs`.
#[test]
fn unsafe_facade_signature_is_marked() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (idiv, fdiv)
        unsafe idiv : Int -> Int -> Int
        fdiv : Int -> Int -> Int
    "#};
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    let marked = |name: &str| match module.values.get(&name.into()) {
        Some(canonical::Value::TypedValue { marked_unsafe, .. }) => *marked_unsafe,
        other => panic!("expected a TypedValue for `{}`, got {:?}", name, other),
    };

    assert!(marked("idiv"), "`unsafe idiv` declares a plain function");
    assert!(
        !marked("fdiv"),
        "an unmarked signature declares the effect a facade declares by default"
    );
}

/// The word only means something in front of a facade signature, so one written
/// on an ordinary declaration is rejected rather than ignored. The caret starts
/// on `unsafe` itself, which is why `FunType`'s span is taken from the modifier.
///
/// Verified to fail by deleting the `!source.binding_foreign` guard in
/// `canonicalize`: the module then canonicalizes cleanly and `expect_err` panics.
#[test]
fn unsafe_outside_a_facade_is_error() {
    use zelkova_lang::compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (twice)
        unsafe twice : Int -> Int
        twice x = x
    "#};

    let errors = canonicalize_with_scalars(source)
        .expect_err("`unsafe` outside a `module foreign` facade must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::UnsafeOutsideFacade(name, _) => assert_eq!(name.as_str(), "twice"),
        other => panic!("expected UnsafeOutsideFacade, got {:?}", other),
    }

    let annotation = "unsafe twice : Int -> Int";
    let start = source.find(annotation).expect("source has the annotation");

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + annotation.len()),
        "the caret must start on the `unsafe` that is being rejected"
    );
}

/// `unsafe : Int` names a facade constant; it is not a modifier with its name
/// missing, and it is not rejected as a stray `unsafe`.
///
/// Verified to fail by making `FunType`'s `"unsafe" ":" Type` alternative set
/// `marked_unsafe: true`, which turns the value into a marked one.
#[test]
fn unsafe_is_a_facade_constant_name() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (unsafe)
        unsafe : Int
    "#};
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    match module.values.get(&"unsafe".into()) {
        Some(canonical::Value::TypedValue {
            name,
            marked_unsafe,
            ..
        }) => {
            assert_eq!(name.as_str(), "unsafe");
            assert!(!marked_unsafe, "the word is the constant's name here");
        }
        other => panic!("expected a TypedValue for `unsafe`, got {:?}", other),
    }
}

// ── LANG-59: an opaque scalar's declaration is not an ordinary union ─────────
//
// `Basics.Int`, `Basics.Float`, `Char.Char` and `String.String` are opaque
// (`DEC-15` decision 2): each is declared in Zelkova, but nothing in the
// language builds or inspects a value of one, so the declaration writes only
// the type's own name and contributes no constructor. `scalars::opaque_scalar_of`
// recognises the four by qualified name, so these sources all declare `Basics`
// — the one module whose `Int` and `Float` are the scalars rather than ordinary
// types that share the spelling (`BUG-26`).
//
// All three below are mutation-checked by commenting out the
// `if let Some(scalar) = scalars::opaque_scalar_of(...)` block `do_types` adds:
// with it gone, `Int`'s declaration goes back to being an ordinary one-variant
// union, and each assertion's comment says what that produces instead.

/// `type Int = Int` in `Basics` registers the type but no `Int` constructor, so a
/// declaration that writes `Int` as a value fails to resolve it.
///
/// Without the check, `Int` is an ordinary constructor and `useInt` canonicalizes
/// cleanly — this goes green on the mutation described above.
#[test]
fn opaque_scalar_int_is_not_a_value_in_basics() {
    let source = indoc::indoc! {r#"
        module Basics exposing (Int, useInt)

        type Int = Int

        useInt : Int
        useInt = Int
    "#};

    let errors =
        canonicalize_standalone(source).expect_err("`Int` is a type, not a value, in `Basics`");

    match errors.as_slice() {
        [canonical::Error::VariantNotFound(name, _, _)] => {
            assert_eq!(name.unqualified_name().as_str(), "Int");
        }
        other => panic!("expected one VariantNotFound, got {:?}", other),
    }
}

/// `type Int = I32` in `Basics` is rejected: an opaque scalar's body must be
/// exactly its own name, and `I32` is not `Int`.
///
/// Without the check, `I32` is read as an ordinary constructor — a variant's own
/// name is never resolved as a type — so the module canonicalizes cleanly and
/// this test's `expect_err` starts panicking on the mutation described above.
#[test]
fn opaque_scalar_int_rejects_a_body_other_than_itself() {
    let source = indoc::indoc! {r#"
        module Basics exposing (..)

        type Int = I32
    "#};

    let errors = canonicalize_standalone(source).expect_err("`Int`'s body must be exactly `Int`");

    match errors.as_slice() {
        [canonical::Error::InvalidScalarDeclaration(name, span)] => {
            assert_eq!(name.to_name().as_str(), "Basics.Int");
            let decl = "type Int = I32";
            let start = source.find(decl).expect("source has the declaration");
            assert_eq!(
                span.to_range(),
                Some(start..(start + decl.len())),
                "the caret must cover the whole declaration"
            );
        }
        other => panic!("expected one InvalidScalarDeclaration, got {:?}", other),
    }
}

/// The check is keyed on the qualified name, not the spelling: a module that is
/// not `Basics` declaring its own `Int` is an ordinary union, whose `Int` is a
/// genuine constructor and a genuine value (`BUG-26`).
///
/// Without the check this would still pass — it pins the *complement*, not the
/// fix itself, so it is mutation-checked differently: giving `opaque_scalar_of`
/// `Example` instead of `Basics` as `INT`'s module turns it red, because `Int`
/// then resolves to nothing here instead of to the constructor.
#[test]
fn a_non_basics_modules_int_is_an_ordinary_union() {
    let source = indoc::indoc! {r#"
        module Example exposing (Int, zero)

        type Int = Int

        zero : Int
        zero = Int
    "#};

    let module = canonicalize_standalone(source)
        .expect("Example's `Int` is an ordinary union, its constructor included");

    let union = module
        .types
        .get(&"Int".into())
        .expect("Int should be declared");
    let names: Vec<_> = union.variants.iter().map(|v| v.name.as_str()).collect();
    assert_eq!(
        names,
        vec!["Int"],
        "Example's Int keeps its own constructor"
    );
}

// ── LANG-58: scalar names reach a module of an exempt package ────────────────
//
// `zelkova-core` — the package `Basics` belongs to — receives none of the
// eight default imports (`LANG-57`), and a facade underneath `Basics` such as
// `Js/Basics.zel` has no import that could reach `Basics`' own declaration of
// `Int`, `Float` and `Bool` without closing a cycle. The compiler seeds the
// five scalar names directly instead, bound to the qualified names
// `scalars::SCALARS` holds (`DEC-15` decision 3, re-scoped by `DEC-17`).
//
// Both tests go through `canonicalize_exempt_package`, whose interfaces map is
// empty — proof that seeding does not consult `Basics`' `Interface`.

/// `Int`, `Float` and `Bool` resolve to `Basics.Int`, `Basics.Float` and
/// `Basics.Bool` in a module with no import at all, as long as its package is
/// exempt from the default imports.
///
/// Mutation-checked by removing the scalar-seeding block from
/// `new_environment`: with nothing declaring any of the three, each becomes a
/// `TypeNotFound` and `expect` panics.
#[test]
fn scalar_types_resolve_in_an_exempt_package_with_no_import() {
    let source = indoc::indoc! {r#"
        module Js.Basics exposing (equal)

        equal : Int -> Float -> Bool
        equal a b =
          a
    "#};

    let module =
        canonicalize_exempt_package(source).expect("the seeded scalar names should resolve");

    match module.values.get(&"equal".into()) {
        Some(canonical::Value::TypedValue { tpe, .. }) => {
            assert_eq!(
                tpe,
                &canonical::Type::Arrow(
                    Box::new(canonical::Type::Type(qual("Basics.Int"), vec![])),
                    Box::new(canonical::Type::Arrow(
                        Box::new(canonical::Type::Type(qual("Basics.Float"), vec![])),
                        Box::new(canonical::Type::Type(qual("Basics.Bool"), vec![])),
                    )),
                )
            );
        }
        other => panic!("expected a TypedValue for `equal`, got {:?}", other),
    }
}

/// The seeded names are type names only: a module of an exempt package can
/// annotate a `Bool` and cannot write a `True` — the scalar seeding brings no
/// constructor and no value ([`DEC-15` decision
/// 4](../../docs/decisions/dec-15.md#4--the-scalar-names-arrive-without-their-values)).
///
/// Mutation-checked by also seeding `Basics`' constructors in
/// `new_environment` (inserting `True`/`False` into `env.constructors`
/// alongside the type names): `canonicalize_exempt_package` then succeeds and
/// this `expect_err` panics.
#[test]
fn exempt_package_seeding_brings_no_constructor() {
    let source = indoc::indoc! {r#"
        module Js.Basics exposing (yes)

        yes : Bool
        yes = True
    "#};

    let errors = canonicalize_exempt_package(source)
        .expect_err("`True` should not resolve: only the type name is seeded");

    match errors.as_slice() {
        [canonical::Error::VariantNotFound(name, _, _)] => {
            assert_eq!(name.unqualified_name().as_str(), "True");
        }
        other => panic!("expected one VariantNotFound, got {:?}", other),
    }
}

/// A module of an exempt package that writes its own `import Basics exposing (Int)`
/// is unaffected: `Int` resolves the same way the written import alone would produce,
/// and the two scalars this module never imports — `Float` and `Bool` — still reach
/// `Basics.Float` and `Basics.Bool` through the seed. This is the ticket's Acceptance
/// clause "a module that keeps its `Basics` entry is unaffected", otherwise only
/// exercised through the full `std/core` pipeline (`Bitwise.zel` keeps its own
/// `import Basics exposing (Int)`) — here with a real `Basics` interface in scope,
/// unlike the two tests above.
///
/// Mutation-checked by removing the scalar-seeding block from `new_environment`:
/// `Int` still resolves correctly (the written import supplies it on its own), but
/// `Float` and `Bool` — never imported here — become `TypeNotFound` and `expect`
/// panics.
#[test]
fn a_written_basics_import_coexists_with_the_seed() {
    let source = indoc::indoc! {r#"
        module Js.Basics exposing (compare)

        import Basics exposing (Int)

        compare : Int -> Float -> Bool
        compare a b =
          a
    "#};

    let mut interfaces = HashMap::new();
    let (name, interface) = basics_interface();
    interfaces.insert(name, interface);

    let parsed = parse_source(source);
    let module = canonical::canonicalize(&test_package(), &interfaces, &parsed, true)
        .expect("the written import should not collide with the seed");

    match module.values.get(&"compare".into()) {
        Some(canonical::Value::TypedValue { tpe, .. }) => {
            assert_eq!(
                tpe,
                &canonical::Type::Arrow(
                    Box::new(canonical::Type::Type(qual("Basics.Int"), vec![])),
                    Box::new(canonical::Type::Arrow(
                        Box::new(canonical::Type::Type(qual("Basics.Float"), vec![])),
                        Box::new(canonical::Type::Type(qual("Basics.Bool"), vec![])),
                    )),
                ),
                "Int (seeded and imported), Float and Bool (seeded only) all resolve to Basics"
            );
        }
        other => panic!("expected a TypedValue for `compare`, got {:?}", other),
    }
}
