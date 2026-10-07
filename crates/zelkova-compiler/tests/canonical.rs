//! Integration tests for the canonicalization phase.
//!
//! Each test parses a source string, runs it through `canonical::canonicalize`,
//! and then asserts on the exact structure of the resulting `canonical::Module` —
//! the pattern bindings, expression bodies, and types — not just that the value
//! key is present.
use std::collections::HashMap;

use zelkova_compiler::canonical;
use zelkova_compiler::name::QualName;
use zelkova_compiler::{Interface, PackageName};
use zelkova_syntax::position::NodeSpan;
use zelkova_syntax::tuple::Tuple;

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
    canonical::Type::Type(core_qual("Basics.Int"), vec![])
}

fn char_t() -> canonical::Type {
    canonical::Type::Type(core_qual("Char.Char"), vec![])
}

fn bool_t() -> canonical::Type {
    canonical::Type::Type(core_qual("Basics.Bool"), vec![])
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
fn scalar_interfaces() -> HashMap<zelkova_compiler::name::Name, Interface> {
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
            context: vec![],
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
            context: vec![],
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
    assert_eq!(color.variables, Vec::<zelkova_compiler::name::Name>::new());
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
            test_qual("Test.Color"),
            "variant tpe points back to Test's Color"
        );
    }

    // ── Value using the constructor ─────────────────────────────────────────
    // `Color` is in env so `Type::from_parser_type` returns
    // `Type::Type("Test.Color", [])` for the annotation — the head names the
    // module that declared it.
    let color_t = canonical::Type::Type(test_qual("Test.Color"), vec![]);

    // `Red` as a TypeConstructor expression:
    //   - no type params → tpe = Type::Type("Test.Color", [])
    //   - unqualified name → falls back to env.module_name().qualify_name("Red")
    //     = QualName { module: ["Test"], name: "Red" }
    assert_eq!(
        module.values.get(&"favorite".into()).unwrap(),
        &canonical::Value::TypedValue {
            context: vec![],
            marked_unsafe: false,
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "favorite".into(),
            patterns: vec![],
            body: c_var_ctor(test_qual("Test.Red"), color_t.clone()),
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
        test_qual("Test.Maybe"),
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
        tpe: test_qual("Test.Maybe"),
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
        tpe: test_qual("Test.Maybe"),
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
        max a b = if True then a else b
    "#};
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"max".into()).unwrap(),
        &canonical::Value::TypedValue {
            context: vec![],
            marked_unsafe: false,
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "max".into(),
            patterns: vec![(p_var("a"), int_t()), (p_var("b"), int_t()),],
            body: c_if(
                c_var_ctor(core_qual("Basics.True"), bool_t()),
                c_var_local("a"),
                c_var_local("b"),
            ),
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
    use zelkova_compiler::PhaseError;

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
    use zelkova_compiler::PhaseError;

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
    use zelkova_compiler::PhaseError;

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
    // `add` is `unsafe` — LANG-68 holds an *unmarked* facade to a
    // `Task (Result Failure a)` result, and this scenario is about the
    // placeholder `Value::TypedValue` a facade builds, not that check.
    let source = indoc::indoc! {r#"
        module foreign Test exposing (add)
        unsafe add : Int -> Int -> Int
    "#};
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    // A facade's values get a placeholder body of `Unit` (see TODO in
    // canonical/mod.rs — the compiler doesn't yet have a dedicated binding
    // expression variant).
    assert_eq!(
        module.values.get(&"add".into()).unwrap(),
        &canonical::Value::TypedValue {
            context: vec![],
            marked_unsafe: true,
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "add".into(),
            patterns: vec![],
            body: canonical::Expression::bare(canonical::ExpressionKind::Unit),
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
    // It is one error among several: nothing is imported, so `Bool`, `true` and `false`
    // do not resolve either.
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
            context: vec![],
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
/// `grammar.lalrpop` to `Tuple::three(c, b, a)` and the three-element
/// `AtomicType` production to `Tuple::three(c, b, a)` — each turns this test red.
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
            context: vec![],
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
fn expect_parse_error(source: &str, why: &str) -> zelkova_syntax::parser::Error {
    use codespan_reporting::files::SimpleFile;
    use zelkova_syntax::parser;

    let file = SimpleFile::new("Test.zel".to_string(), source.to_string());

    parser::parse(&file).expect_err(why)
}

/// Asserts `error` is an `UnexpectedToken` on `expected_token`.
fn assert_rejected_token(
    error: zelkova_syntax::parser::Error,
    expected_token: zelkova_syntax::parser::tokenizer::Token,
    why: &str,
) {
    use zelkova_syntax::parser;

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
    use zelkova_syntax::parser::tokenizer::Token;

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
    use zelkova_syntax::parser::tokenizer::Token;

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
/// The tuple is a function's result rather than the whole annotation: at the
/// front of an annotation, `(Int, Int, Int, Int)` is also the start of a
/// four-constraint context, which `ConstrainedType` accepts, so the parser only
/// fails at the missing `=>` there and not at the fourth `,`.
///
/// Adding a four-element tuple production to `AtomicType` no longer guards this:
/// beside `ConstrainedType`'s four-or-more production it is a shift/reduce conflict
/// on `"=>"`, so the grammar does not build and this test never runs. Verified by
/// adding `<tpe1:ArgType> "->" "(" <a:Type> "," <b:Type> "," <c:Type> "," <t:Type> ")"`
/// to `Type`'s arrow productions, which builds, makes the parse succeed and turns
/// this test red.
#[test]
fn tuple_type_of_four_is_a_parse_error() {
    use zelkova_syntax::parser::tokenizer::Token;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        f : Int -> (Int, Int, Int, Int)
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
    use zelkova_syntax::parser::tokenizer::Token;

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
        core_qual("Maybe.Maybe"),
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
        tpe: core_qual("Maybe.Maybe"),
    };
    assert_eq!(branches[0].pattern, p_ctor(just_ctor, vec![p_var("x")]));

    let nothing_ctor = canonical::TypeConstructor {
        name: "Nothing".into(),
        type_parameters: vec![],
        tpe: core_qual("Maybe.Maybe"),
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
    use zelkova_compiler::PhaseError;

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
    use zelkova_compiler::PhaseError;

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
        core_qual("Maybe.Maybe"),
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

    let maybe_int = canonical::Type::Type(core_qual("Maybe.Maybe"), vec![int_t()]);

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
        core_qual("Maybe.Maybe"),
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

    let own = canonical::Type::Type(test_qual("Test.Size"), vec![]);
    let widgets = canonical::Type::Type(test_qual("Widget.Size"), vec![]);

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
    use zelkova_compiler::PhaseError;

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

    let never = canonical::Type::Type(test_qual("Test.Never"), vec![]);

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
        vec![canonical::Type::Type(test_qual("Test.Flag"), vec![])]
    );
}

// ── Extra: an annotation with no body points at the annotation ───────────────

/// `ERR-3`: `NoBindings` renders a caret under the annotation it is about.
///
/// "This declaration has a type annotation but no body" is precisely the message
/// where the reader needs to know *which* annotation, and the construction site in
/// `value_body` has `function.span` in hand — it is the same span the sibling
/// `BindingPatternsInvalidLen` uses just above it. The range is asserted rather
/// than mere non-emptiness, for the usual reason: a span taken around the layout
/// pass's zero-width block tokens would satisfy `!labels.is_empty()` while pointing
/// at nothing.
///
/// Mutation-checked by dropping the `NoBindings` arm from `canonical::Error::labels`
/// so it falls through to the catch-all: `labels` comes back empty.
#[test]
fn annotation_without_a_body_labels_the_annotation() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer : Int
    "#};

    // `Int` has to resolve, or the annotation's own `TypeNotFound` is reported too.
    let errors = canonicalize_with_interfaces(source, &HashMap::from([basics_interface()]))
        .expect_err("an annotation with no body is an error");
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
    use zelkova_compiler::PhaseError;

    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing ()
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
    // Nothing is exposed, so that the unannotated declarations are not also reported
    // as exposed without an annotation.
    let source = indoc::indoc! {r#"
        module Test exposing ()
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
        module Test exposing ()
        type Color = Red | Green | Blue
        isRed Reed = 1
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
        module Test exposing ()
        type Color = Red | Green | Blue
        isRed Zzzzzzzzzzzz = 1
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
    use zelkova_compiler::PhaseError;

    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing ()
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
    use zelkova_compiler::PhaseError;

    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing ()
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
    use zelkova_compiler::PhaseError;

    let (iface_name, iface) = maybe_interface();
    let mut interfaces = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing ()
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

/// `Lib`, canonicalized for real so its interface comes out of `to_interface`: it
/// exposes `Opaque` bare and `Clear` with its constructors.
fn opaque_and_clear_lib() -> HashMap<zelkova_compiler::name::Name, Interface> {
    let lib = canonicalize_standalone(indoc::indoc! {r#"
        module Lib exposing (Opaque, Clear(..), wrap)
        type Opaque a = Wrapped a
        type Clear = Shown
        wrap : a -> Opaque a
        wrap x = Wrapped x
    "#})
    .expect("Lib should canonicalize");

    let mut interfaces = HashMap::new();
    interfaces.insert(lib.name.name().clone(), lib.to_interface(None));
    interfaces
}

/// A `Opaque(..)` import entry for a type its module exposes only as a bare
/// `Opaque` is an error at that entry, not a constructor-less type that surfaces
/// later as an unresolved constructor.
///
/// Mutation-checked by deleting the `opaque_unions.contains` early return in
/// `process_import`'s `Privacy::Public` arm: the import then succeeds and
/// `expect_err` panics. Deleting only the `opaque_unions.insert` in
/// `Module::to_interface` does the same.
#[test]
fn a_constructor_entry_for_an_opaquely_exposed_type_is_rejected() {
    use zelkova_compiler::PhaseError;

    let interfaces = opaque_and_clear_lib();
    let source = indoc::indoc! {r#"
        module Main exposing ()
        import Lib exposing (Opaque(..))
        answer = 1
    "#};

    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("asking for constructors `Lib` does not expose should not resolve");
    assert_eq!(errors.len(), 1, "got {:?}", errors);
    assert!(
        matches!(errors[0], canonical::Error::EnvironmentErrors(..)),
        "got {:?}",
        errors[0]
    );

    assert_eq!(
        errors[0].message(),
        "`Lib` exposes the type `Opaque` but not its constructors"
    );
    assert_eq!(
        errors[0].notes(),
        vec!["write `Opaque` without `(..)` to import the type alone".to_string()]
    );

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert!(labels[0].primary);
    assert_eq!(
        labels[0].message,
        "`Opaque(..)` asks for constructors `Lib` does not expose"
    );

    // The caret covers the whole `Opaque(..)` entry, not the `import` line.
    let start = source.find("Opaque(..)").unwrap();
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + "Opaque(..)".len())
    );
}

/// The legitimate cases beside the one above stay accepted: `Clear(..)` for a
/// type `Lib` exposes with its constructors brings `Shown` into scope, and a bare
/// `Opaque` asks for no constructors, so the same opacity does not reject it.
///
/// Mutation-checked by making that early return unconditional, rejecting every
/// `(..)` entry: `Clear(..)` is then refused and `expect` panics.
#[test]
fn a_constructor_entry_for_a_transparently_exposed_type_still_resolves() {
    let interfaces = opaque_and_clear_lib();
    let source = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (Clear(..), Opaque)
        shown : Clear
        shown = Shown
        wrapped : Opaque Clear
        wrapped = Lib.wrap Shown
    "#};

    let module = canonicalize_with_interfaces(source, &interfaces)
        .expect("`Clear(..)` and a bare `Opaque` should both resolve");

    assert!(module.values.contains_key(&"shown".into()));
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
        module Test exposing ()

        infix non 4 (==) = eq

        eq a b = a

        chain a b c =
          a == b == c
    "#};

    assert_ambiguous_pair(source, "==", "==");
}

/// Canonicalizes `source`, and asserts it was rejected with exactly one
/// `AmbiguousOperatorPrecedence` naming `left` and `right` in that order.
///
/// Each `source` exposes nothing, so that its unannotated declarations are not also
/// reported as exposed without an annotation.
fn assert_ambiguous_pair(source: &str, left: &str, right: &str) {
    let errors = canonicalize_standalone(source).expect_err("should reject");
    assert_eq!(errors.len(), 1, "got {:?}", errors);
    match &errors[0] {
        canonical::Error::AmbiguousOperatorPrecedence(l, r, _) => {
            assert_eq!(l.name, zelkova_compiler::name::Name::from(left));
            assert_eq!(r.name, zelkova_compiler::name::Name::from(right));
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
        module Test exposing ()

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
        module Test exposing ()

        infix non 4 (<) = lt

        infix non 4 (>) = gt

        lt a b = a

        gt a b = a

        chain a b c =
          a < b > c
    "#};

    use zelkova_compiler::PhaseError;

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
fn only_invalid_variant_label(errors: &[canonical::Error]) -> zelkova_compiler::SpanLabel {
    use zelkova_compiler::PhaseError;

    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);
    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    labels.into_iter().next().unwrap()
}

/// A lowercase name in variant position — the mistyped constructor — is rejected,
/// and says that a constructor name is capitalised.
#[test]
fn lowercase_name_in_variant_position_is_rejected() {
    use zelkova_compiler::PhaseError;

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
/// the facade declares. `fdiv` is unmarked, so its result is `Task (Result
/// Failure Int)` — the shape LANG-68 holds an unmarked signature to — rather
/// than the plain `Int -> Int -> Int` this scenario predates that check with.
///
/// Verified to fail by pinning `marked_unsafe: false` at the facade branch's
/// `Value::TypedValue` in `canonical/mod.rs`.
#[test]
fn unsafe_facade_signature_is_marked() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (idiv, fdiv)

        import Task exposing (Task, Failure)

        unsafe idiv : Int -> Int -> Int
        fdiv : Int -> Int -> Task (Result Failure Int)
    "#};
    let module = canonicalize_with_effects(source).expect("should canonicalize");

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
    use zelkova_compiler::PhaseError;

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
/// missing, and it is not rejected as a stray `unsafe`. The constant is
/// unmarked, so its type is `Task (Result Failure Int)` rather than the bare
/// `Int` this scenario predates LANG-68's effect-shape check with.
///
/// Verified to fail by making `FunType`'s `"unsafe" ":" Type` alternative set
/// `marked_unsafe: true`, which turns the value into a marked one.
#[test]
fn unsafe_is_a_facade_constant_name() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (unsafe)

        import Task exposing (Task, Failure)

        unsafe : Task (Result Failure Int)
    "#};
    let module = canonicalize_with_effects(source).expect("should canonicalize");

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

// ── Scenario 14: a facade signature naming an inadmissible type ──────────────
//
// `docs/spec/interop.md#which-types-may-cross-the-boundary` admits the
// primitives, tuples, records and union types applied to admitted types, and rejects a
// bare type variable and a function type wherever either appears
// (`docs/spec/interop.md#what-a-facade-signature-may-not-name`, `LANG-43`).

/// A type variable at the top of a facade signature — the simplest shape the
/// walk rejects.
///
/// Verified to fail by neutralising `check_facade_admitted_type` to always
/// return `Ok(())`: the module then canonicalizes cleanly and `expect_err`
/// panics.
#[test]
fn facade_signature_over_bare_variable_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module foreign Test exposing (equal)
        equal : a -> a -> Bool
    "#};

    let errors = canonicalize_with_scalars(source)
        .expect_err("a facade signature over a bare type variable must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::FacadeTypeNotAdmitted(name, kind, _) => {
            assert_eq!(name.as_str(), "equal");
            assert_eq!(*kind, canonical::FacadeRejectedKind::Variable);
        }
        other => panic!("expected FacadeTypeNotAdmitted, got {:?}", other),
    }

    let annotation = "equal : a -> a -> Bool";
    let start = source.find(annotation).expect("source has the annotation");

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + annotation.len()),
        "the caret must cover the whole annotation, the finest span in hand"
    );
}

/// A function type taken as a parameter, nested under the top-level arrows a
/// facade's own parameter list contributes — the shape the ticket's own
/// example (`count`) names, and the one that proves the walk descends past
/// the first arrow rather than stopping at it.
///
/// Verified to fail by neutralising `check_facade_admitted_type`'s
/// `Type::Arrow` arm to `Ok(())` instead of `Err(FacadeRejectedKind::Function)`:
/// the module then canonicalizes cleanly and `expect_err` panics.
#[test]
fn facade_signature_over_function_type_is_rejected() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (count)
        count : (Int -> Bool) -> Int -> Int
    "#};

    let errors = canonicalize_with_scalars(source)
        .expect_err("a facade signature taking a function must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::FacadeTypeNotAdmitted(name, kind, _) => {
            assert_eq!(name.as_str(), "count");
            assert_eq!(*kind, canonical::FacadeRejectedKind::Function);
        }
        other => panic!("expected FacadeTypeNotAdmitted, got {:?}", other),
    }
}

/// A tuple of admitted types is not what either rejection is about, so it
/// canonicalizes cleanly — the same `rgb` signature
/// `docs/spec/interop.md#which-types-may-cross-the-boundary` shows as
/// `expect=ok`. This is the counterpart the mutation check above needs: a
/// walk that rejected everything would also make the two tests above pass.
#[test]
fn facade_signature_over_admitted_tuple_is_accepted() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (rgb)
        unsafe rgb : Int -> (Int, Int, Int)
    "#};

    canonicalize_with_scalars(source).expect("a tuple of admitted types must canonicalize");
}

// ── LANG-68: an unmarked facade's result must be `Task (Result Failure a)`,
//    and `Task` may appear nowhere else ────────────────────────────────────
//
// `docs/spec/interop.md#an-effectful-facade` settles the shape: a facade
// declares an effect by default, so an unmarked signature's result must be
// exactly `Task (Result Failure a)`; `unsafe` is what removes that
// requirement (`DEC-12` decisions 1 and 7). The same chapter section confines
// `Task` to that one position — never an argument, never nested inside
// another type — whether or not the signature is `unsafe`.
//
// These fixtures are checked without `std/core`, so every fixture below hands
// `canonicalize_with_interfaces` the synthetic `task_interface`/`result_interface`
// built for exactly this, beside the scalars `canonicalize_with_scalars` already
// supplies.

/// The interface map every test below uses: [`scalar_interfaces`] plus
/// `Task`, `Result` and `String` — the three `zelkova-core` modules an
/// unmarked facade's required result shape can name.
fn effect_interfaces() -> HashMap<zelkova_compiler::name::Name, Interface> {
    let mut interfaces = scalar_interfaces();
    for (name, interface) in [task_interface(), result_interface(), string_interface()] {
        interfaces.insert(name, interface);
    }
    interfaces
}

fn canonicalize_with_effects(source: &str) -> Result<canonical::Module, Vec<canonical::Error>> {
    canonicalize_with_interfaces(source, &effect_interfaces())
}

/// An interface for a module named `Widgets`, declaring its own union spelled
/// `Task` — the fixture [`facade_naming_a_user_declared_task_is_an_ordinary_union`]
/// needs to show that `Task` is recognised by the qualified name of its
/// declaration and never by spelling: `Widgets.Task` shares four letters with
/// `Task.Task` and nothing else.
fn widgets_task_interface() -> (zelkova_compiler::name::Name, Interface) {
    let mut unions = HashMap::new();
    unions.insert(
        "Task".into(),
        canonical::UnionType {
            span: NodeSpan::none(),
            variables: vec![],
            variants: vec![canonical::TypeConstructor {
                name: "Noop".into(),
                type_parameters: vec![],
                tpe: qual_in(&test_package(), "Widgets.Task"),
            }],
        },
    );

    let interface = Interface {
        module_name: zelkova_compiler::ModuleName::new(test_package(), "Widgets".into()),
        values: HashMap::new(),
        unions,
        opaque_unions: Default::default(),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
    };

    ("Widgets".into(), interface)
}

/// `Task (Result Failure String)`, unmarked — the shape
/// [`docs/spec/interop.md`](../../../docs/spec/interop.md#an-effectful-facade)'s
/// own `read` example writes, and the first of the two payload types the
/// ticket's test list asks for.
///
/// Verified to fail by neutralising the new `is_task_applied(result)` branch
/// to always fall through to the `else` arm: this then reports
/// `FacadeResultNotEffect` instead of canonicalizing, and `expect` panics.
#[test]
fn unmarked_facade_over_task_result_failure_string_is_accepted() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (read)

        import Task exposing (Task, Failure)

        read : String -> Task (Result Failure String)
    "#};

    canonicalize_with_effects(source)
        .expect("`Task (Result Failure String)` is exactly the required shape");
}

/// `Task (Result Failure Int)`, unmarked — the second payload type the
/// ticket's test list asks for, and a facade constant rather than a function.
#[test]
fn unmarked_facade_over_task_result_failure_int_is_accepted() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (now)

        import Task exposing (Task, Failure)

        now : Task (Result Failure Int)
    "#};

    canonicalize_with_effects(source)
        .expect("`Task (Result Failure Int)` is exactly the required shape");
}

/// A bare `Int` result on an unmarked facade — no `Task` in sight — is the
/// simplest shape mismatch there is.
///
/// Verified to fail by neutralising the new checks — commenting out the
/// `else if is_task_applied(result) ... else if contains_task(result) ...
/// else` chain that follows the existing `FacadeTypeNotAdmitted` check: the
/// module then canonicalizes cleanly and `expect_err` panics.
#[test]
fn unmarked_facade_over_bare_int_is_rejected() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (now)

        now : Int
    "#};

    let errors = canonicalize_with_effects(source)
        .expect_err("an unmarked facade's result must be `Task (Result Failure a)`");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::FacadeResultNotEffect(name, _) => {
            assert_eq!(name.as_str(), "now");
        }
        other => panic!("expected FacadeResultNotEffect, got {:?}", other),
    }

    let annotation = "now : Int";
    let start = source.find(annotation).expect("source has the annotation");

    use zelkova_compiler::PhaseError;
    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + annotation.len()),
        "the caret must cover the whole annotation, the finest span in hand"
    );
}

/// `Task Int`, unmarked — `Task` is in the one position it may occupy, but
/// what it wraps is not `Result Failure a`. This is what tells
/// `is_task_applied` apart from a plain shape check: the outer shape is
/// right and the inner one is wrong, which is still `FacadeResultNotEffect`
/// and not `FacadeTaskMisplaced`.
#[test]
fn unmarked_facade_over_task_int_is_rejected() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (now)

        now : Task Int
    "#};

    let errors =
        canonicalize_with_effects(source).expect_err("`Task Int` is not `Task (Result Failure a)`");

    match errors.as_slice() {
        [canonical::Error::FacadeResultNotEffect(name, _)] => {
            assert_eq!(name.as_str(), "now");
        }
        other => panic!("expected one FacadeResultNotEffect, got {:?}", other),
    }
}

/// `unsafe` removes the effect-shape requirement (`DEC-12` decision 7): the
/// same bare `Int` that [`unmarked_facade_over_bare_int_is_rejected`] rejects
/// canonicalizes cleanly once the signature says `unsafe`.
///
/// Verified to fail by neutralising the `function.marked_unsafe` guard to
/// always take the unmarked branch: this then reports `FacadeResultNotEffect`
/// and `expect` panics.
#[test]
fn unsafe_facade_over_bare_int_is_accepted() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (magic)

        unsafe magic : Int
    "#};

    canonicalize_with_effects(source).expect("`unsafe` is not held to the effect result shape");
}

/// `Task` named as an argument — never admitted, marked or not.
///
/// Verified to fail by neutralising the `parameters.iter().copied().any(contains_task)`
/// check: the module then canonicalizes cleanly and `expect_err` panics.
#[test]
fn task_as_an_argument_is_rejected() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (run)

        import Task exposing (Task, Failure)

        run : Task Int -> Task (Result Failure Int)
    "#};

    let errors = canonicalize_with_effects(source)
        .expect_err("`Task` may not be named as a facade's argument");

    match errors.as_slice() {
        [canonical::Error::FacadeTaskMisplaced(name, _)] => {
            assert_eq!(name.as_str(), "run");
        }
        other => panic!("expected one FacadeTaskMisplaced, got {:?}", other),
    }
}

/// `Maybe (Task Int)` as a result — `Task` is present, but not at the top, so
/// this is `FacadeTaskMisplaced` rather than `FacadeResultNotEffect`, unlike
/// the bare-`Int` and `Task Int` mismatches above that never mention `Task`
/// in the wrong place.
#[test]
fn task_nested_in_another_type_as_a_result_is_rejected() {
    let mut interfaces = effect_interfaces();
    let (name, interface) = maybe_interface();
    interfaces.insert(name, interface);

    let source = indoc::indoc! {r#"
        module foreign Test exposing (run)

        run : Int -> Maybe (Task Int)
    "#};

    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("`Task` nested inside `Maybe` is not the required result shape");

    match errors.as_slice() {
        [canonical::Error::FacadeTaskMisplaced(name, _)] => {
            assert_eq!(name.as_str(), "run");
        }
        other => panic!("expected one FacadeTaskMisplaced, got {:?}", other),
    }
}

/// `Task (Result Failure (Task Int))` — the outer three levels are exactly
/// the required shape, and `Task` reappears inside the payload `a`.
///
/// Verified to fail by neutralising the `contains_task(payload)` check inside
/// the `is_task_applied` branch: this then accepts the signature and
/// `expect_err` panics.
#[test]
fn task_nested_in_the_payload_is_rejected() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (run)

        import Task exposing (Task, Failure)

        run : Task (Result Failure (Task Int))
    "#};

    let errors = canonicalize_with_effects(source)
        .expect_err("`Task` inside the payload `a` is not admitted");

    match errors.as_slice() {
        [canonical::Error::FacadeTaskMisplaced(name, _)] => {
            assert_eq!(name.as_str(), "run");
        }
        other => panic!("expected one FacadeTaskMisplaced, got {:?}", other),
    }
}

/// An `unsafe` facade returning `Task Int` — `unsafe` only removes the
/// effect-shape *requirement*; it grants no exemption from the "`Task`
/// nowhere else" rule, so `Task` at the top of an `unsafe` result is rejected
/// exactly like `Task` anywhere else would be.
///
/// Verified to fail by neutralising the `if function.marked_unsafe { if
/// contains_task(result) ... }` branch: this then accepts the signature and
/// `expect_err` panics.
#[test]
fn unsafe_facade_returning_task_is_rejected() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (run)

        unsafe run : Int -> Task Int
    "#};

    let errors =
        canonicalize_with_effects(source).expect_err("`unsafe` does not admit `Task` as a result");

    match errors.as_slice() {
        [canonical::Error::FacadeTaskMisplaced(name, _)] => {
            assert_eq!(name.as_str(), "run");
        }
        other => panic!("expected one FacadeTaskMisplaced, got {:?}", other),
    }
}

/// An `unsafe` facade naming `Task` as an argument — `unsafe` only removes
/// the effect-shape requirement on the *result*; the parameter-position check
/// runs unconditionally, before the `marked_unsafe` branch is even reached,
/// so `Task` is rejected as an argument whether or not the signature is
/// `unsafe`, exactly as
/// [`task_as_an_argument_is_rejected`] shows for an unmarked one.
///
/// Verified to fail by neutralising the `parameters.iter().copied().any(contains_task)`
/// check: the module then canonicalizes cleanly and `expect_err` panics.
#[test]
fn unsafe_facade_task_as_an_argument_is_rejected() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (run)

        unsafe run : Task Int -> Int
    "#};

    let errors = canonicalize_with_effects(source)
        .expect_err("`unsafe` grants no exemption from `Task` as a facade's argument");

    match errors.as_slice() {
        [canonical::Error::FacadeTaskMisplaced(name, _)] => {
            assert_eq!(name.as_str(), "run");
        }
        other => panic!("expected one FacadeTaskMisplaced, got {:?}", other),
    }
}

/// A facade naming a user-declared `Task` — from a module named `Widgets`,
/// not `Task` — canonicalizes as an ordinary union, with no
/// `FacadeTaskMisplaced` in sight: `Task` is recognised by the qualified name
/// of its declaration and never by spelling.
///
/// Verified to fail by neutralising `is_task_declaration` to compare only
/// `name.unqualified_name()` against `"Task"`, dropping the module and
/// package: `Widgets.Task` is then misread as the real one and this starts
/// reporting `FacadeTaskMisplaced` instead of canonicalizing.
#[test]
fn facade_naming_a_user_declared_task_is_an_ordinary_union() {
    let mut interfaces = scalar_interfaces();
    let (name, interface) = widgets_task_interface();
    interfaces.insert(name, interface);

    let source = indoc::indoc! {r#"
        module foreign Test exposing (run)

        import Widgets exposing (Task)

        unsafe run : Task -> Int
    "#};

    canonicalize_with_interfaces(source, &interfaces)
        .expect("a module's own `Task` is an ordinary union, not the effect type");
}

// ── LANG-59: an opaque scalar's declaration is not an ordinary union ─────────
//
// `Basics.Int`, `Basics.Float`, `Char.Char` and `String.String` are opaque
// (`DEC-15` decision 2): each is declared in Zelkova, but nothing in the
// language builds or inspects a value of one, so the declaration writes only
// the type's own name and contributes no constructor. `scalars::opaque_scalar_of`
// recognises the four by qualified name, so these sources all declare `Basics`
// of `zelkova-core` — the one module whose `Int` and `Float` are the scalars
// rather than ordinary types that share the spelling (`BUG-26`).
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
        canonicalize_exempt_package(source).expect_err("`Int` is a type, not a value, in `Basics`");

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

    let errors =
        canonicalize_exempt_package(source).expect_err("`Int`'s body must be exactly `Int`");

    match errors.as_slice() {
        [canonical::Error::InvalidScalarDeclaration(name, span)] => {
            assert_eq!(*name, core_qual("Basics.Int"));
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

/// The check is keyed on the package as well as the module: a `Basics` of any
/// package but `zelkova-core` declares an ordinary `Int`, whose body is an ordinary
/// variant list.
///
/// Mutation-checked by dropping the package comparison from `Scalar::declares`: the
/// declaration is then taken for the scalar and rejected as
/// `InvalidScalarDeclaration`.
#[test]
fn another_packages_basics_declares_an_ordinary_int() {
    let source = indoc::indoc! {r#"
        module Basics exposing (..)

        type Int = I32
    "#};

    let module = canonicalize_standalone(source)
        .expect("`Basics` of a package other than `zelkova-core` declares no scalar");

    let int = module.types.get(&"Int".into()).expect("`Int` is declared");
    assert_eq!(
        int.variants
            .iter()
            .map(|variant| variant.name.as_str())
            .collect::<Vec<_>>(),
        vec!["I32"]
    );
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
                    Box::new(canonical::Type::Type(core_qual("Basics.Int"), vec![])),
                    Box::new(canonical::Type::Arrow(
                        Box::new(canonical::Type::Type(core_qual("Basics.Float"), vec![])),
                        Box::new(canonical::Type::Type(core_qual("Basics.Bool"), vec![])),
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
    let module = canonical::canonicalize(&PackageName::core(), &interfaces, &parsed)
        .expect("the written import should not collide with the seed");

    match module.values.get(&"compare".into()) {
        Some(canonical::Value::TypedValue { tpe, .. }) => {
            assert_eq!(
                tpe,
                &canonical::Type::Arrow(
                    Box::new(canonical::Type::Type(core_qual("Basics.Int"), vec![])),
                    Box::new(canonical::Type::Arrow(
                        Box::new(canonical::Type::Type(core_qual("Basics.Float"), vec![])),
                        Box::new(canonical::Type::Type(core_qual("Basics.Bool"), vec![])),
                    )),
                ),
                "Int (seeded and imported), Float and Bool (seeded only) all resolve to Basics"
            );
        }
        other => panic!("expected a TypedValue for `compare`, got {:?}", other),
    }
}

// ── Scenario 14: A parameterless binding may not depend on itself (LANG-35) ──
//
// `docs/spec/evaluation-semantics.md`'s *A binding may not depend on itself*:
// a parameterless binding is evaluated once, before the program runs, after
// everything it depends on — what it reaches by following mentions, through
// functions as well as other bindings — so a cycle holding a parameterless
// binding, one declaration long or several, describes no such order. A cycle
// of functions only is untouched: a function's value exists before its body
// runs.

/// `x = x`: the shortest possible cycle, and the one
/// [`canonical::Error::SelfDependency`]'s message special-cases.
///
/// Mutation-checked by short-circuiting `check_self_dependency` to always
/// return `Ok(())`: this test goes red, `canonicalize_standalone` starts
/// returning `Ok`.
#[test]
fn self_reference_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing ()
        x = x
    "#};

    let errors =
        canonicalize_standalone(source).expect_err("a binding cannot depend on its own value");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::SelfDependency(path) => {
            assert_eq!(
                path.iter().map(|m| m.name.as_str()).collect::<Vec<_>>(),
                vec!["x"],
                "a one-binding cycle names only that binding"
            );
        }
        other => panic!("expected SelfDependency, got {:?}", other),
    }

    let declaration = "x = x";
    let start = source
        .find(declaration)
        .expect("source declares it on its own line");

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert!(labels[0].primary, "the only label must be the primary one");
    assert_eq!(
        labels[0].span.to_range(),
        start..(start + declaration.len()),
        "the caret must sit under the whole binding, not just one occurrence of `x`"
    );
}

/// `a = b` beside `b = a`: a cycle running through two bindings rather than
/// one. `path` is reported in a name-sorted order so the test does not depend
/// on `values`' `HashMap` iteration order — see `check_self_dependency`'s doc
/// comment.
///
/// Mutation-checked by making `check_self_dependency` count a component as a
/// cycle only once it has three or more members: this test goes red,
/// `canonicalize_standalone` starts returning `Ok`.
#[test]
fn mutual_dependency_between_two_bindings_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing ()
        a = b
        b = a
    "#};

    let errors = canonicalize_standalone(source)
        .expect_err("two bindings that depend on each other have no order to evaluate them in");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::SelfDependency(path) => {
            assert_eq!(
                path.iter().map(|m| m.name.as_str()).collect::<Vec<_>>(),
                vec!["a", "b"]
            );
        }
        other => panic!("expected SelfDependency, got {:?}", other),
    }

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 2, "expected two labels, got {:?}", labels);
    assert!(labels[0].primary, "the first label must be the primary one");
    assert!(!labels[1].primary, "the second label must be secondary");

    let a_decl = "a = b";
    let a_start = source
        .find(a_decl)
        .expect("source declares it on its own line");
    assert_eq!(labels[0].span.to_range(), a_start..(a_start + a_decl.len()));

    let b_decl = "b = a";
    let b_start = source
        .find(b_decl)
        .expect("source declares it on its own line");
    assert_eq!(labels[1].span.to_range(), b_start..(b_start + b_decl.len()));
}

/// `a = (a, b)` beside `b = a`: `a` has a self-loop (it names itself in its
/// own tuple) *and* is part of the two-member `{a, b}` cycle (`a` reaches
/// `b`, `b` reaches `a`). The two used to be reported as separate
/// `SelfDependency` errors — a length-1 one for `a`'s self-loop and a
/// length-2 one for the `{a, b}` cycle — even though they describe the same
/// underlying cycle. Only the length-2 report should survive:
/// `check_self_dependency` reports per strongly-connected component, and asks
/// about an edge to itself only for a component of one.
///
/// Mutation-checked by adding a second pass to `check_self_dependency` that
/// reports every node with an edge to itself on its own, beside the
/// per-component one: this test goes red, `errors.len()` back to 2.
#[test]
fn self_loop_inside_a_larger_cycle_is_reported_once() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        a = (a, b)
        b = a
    "#};

    let errors = canonicalize_standalone(source)
        .expect_err("a still has no value before the cycle it is part of resolves");
    assert_eq!(
        errors.len(),
        1,
        "a's self-loop and its membership in the {{a, b}} cycle are the same \
         defect and must be reported once, got {:?}",
        errors
    );

    match &errors[0] {
        canonical::Error::SelfDependency(path) => {
            assert_eq!(
                path.iter().map(|m| m.name.as_str()).collect::<Vec<_>>(),
                vec!["a", "b"]
            );
        }
        other => panic!("expected SelfDependency, got {:?}", other),
    }
}

/// `y = f y`: `y` depends on `f`, but `f`'s body mentions nothing, so `f` is
/// not part of any cycle — while `y` also names itself in the same
/// application, and that occurrence is a self-loop regardless of what else
/// the expression does.
///
/// Mutation-checked by dropping the `Apply` arm from `collect_top_level_refs`
/// (so only the outermost expression node is ever inspected): this test goes
/// red, since `y`'s own reference to itself is nested one `Apply` deep and
/// would never be visited.
#[test]
fn self_dependency_through_a_function_argument_is_rejected() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        f n = n
        y = f y
    "#};

    let errors =
        canonicalize_standalone(source).expect_err("y still has no value before it needs its own");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::SelfDependency(path) => {
            assert_eq!(
                path.iter().map(|m| m.name.as_str()).collect::<Vec<_>>(),
                vec!["y"],
                "f mentions nothing, so it is never part of the cycle"
            );
        }
        other => panic!("expected SelfDependency, got {:?}", other),
    }
}

/// `f n = f n`: a function may call itself, because its body only runs once
/// applied — must stay accepted.
///
/// Mutation-checked by dropping the `holds_binding` condition in
/// `check_self_dependency` (reporting every cycle, whatever its members):
/// this test goes red on the self-loop `f` forms.
#[test]
fn self_recursive_function_is_accepted() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        f n = f n
    "#};

    canonicalize_standalone(source)
        .expect("a function may call itself: its body only runs when applied");
}

/// Mutual recursion between two function bindings must stay accepted, for the
/// same reason a single self-recursive function does.
///
/// Mutation-checked by dropping the `holds_binding` condition in
/// `check_self_dependency`: the `{isEven, isOdd}` component is then reported.
#[test]
fn mutual_recursion_between_two_functions_is_accepted() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        isEven n = isOdd n
        isOdd n = isEven n
    "#};

    canonicalize_standalone(source).expect("two functions may call each other freely");
}

/// A parameterless binding that mentions a recursive function, which never
/// mentions it back, must stay accepted: `mentionsRecursive` depends on `f`,
/// and `f` on itself, but the only cycle is `f`'s, and it holds no
/// parameterless binding.
#[test]
fn mentioning_a_recursive_function_is_accepted() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        f n = f n
        mentionsRecursive = f
    "#};

    canonicalize_standalone(source).expect(
        "mentioning a recursive function is not itself a cycle among parameterless bindings",
    );
}

/// `a = f 1` beside `f x = a`: a cycle that runs through a function. `a` depends on
/// `f`, and `f`'s body mentions `a`, so initialising `a` would call `f`, which reads
/// `a` before it has a value. The error names both members, `a` first, marks `f` as
/// the function it is, and its message does not call `f` a parameterless binding.
///
/// Mutation-checked by making `canonical::dependency_graph` add a node for the
/// parameterless declarations only, as it once did: `f` is no longer a node, the
/// cycle disappears, and the module is accepted.
#[test]
fn a_cycle_through_a_function_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (a)

        a : Int
        a =
          f 1

        f : Int -> Int
        f x =
          a
    "#};

    let errors = canonicalize_with_interfaces(source, &HashMap::from([basics_interface()]))
        .expect_err("initialising `a` calls `f`, which reads `a`");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let canonical::Error::SelfDependency(members) = &errors[0] else {
        panic!("expected SelfDependency, got {:?}", errors[0]);
    };
    assert_eq!(
        members
            .iter()
            .map(|m| (m.name.as_str(), m.function))
            .collect::<Vec<_>>(),
        vec![("a", false), ("f", true)],
        "both members are named, the binding first and the function marked as one"
    );

    let message = errors[0].message();
    assert_eq!(
        message,
        "`a` needs its own value before it has one: `a` and the function `f` depend on each other"
    );

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 2, "expected two labels, got {:?}", labels);
    assert!(labels[0].primary, "the binding carries the primary label");
    assert!(
        labels[1].message.contains("the function `f`"),
        "the label on `f` says it is a function, got {:?}",
        labels[1].message
    );
}

/// `a = f` beside `f x = a`: `a` mentions `f` without calling it, so initialising it
/// would never run `f`'s body — and it is rejected all the same. *Depends on* is
/// transitive mention, not a guess at which mentioned code runs
/// ([`DEC-19`](../../docs/decisions/dec-19.md)), so this pins the over-approximation
/// as the rule rather than an accident of the implementation.
///
/// Mutation-checked the same way as `a_cycle_through_a_function_is_rejected`: with
/// only parameterless declarations as nodes, the module is accepted.
#[test]
fn mentioning_a_function_that_mentions_the_binding_back_is_rejected() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        a =
          f

        f x =
          a
    "#};

    let errors = canonicalize_standalone(source)
        .expect_err("`a` depends on `f`, and `f` on `a`, whether or not `a` calls it");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let canonical::Error::SelfDependency(members) = &errors[0] else {
        panic!("expected SelfDependency, got {:?}", errors[0]);
    };
    assert_eq!(
        members
            .iter()
            .map(|m| (m.name.as_str(), m.function))
            .collect::<Vec<_>>(),
        vec![("a", false), ("f", true)]
    );
}

// ── An imported name is named by the module that declared it ────────────────

/// The name at the head of `name`'s body, past every argument applied to it.
fn head_of(module: &canonical::Module, name: &str) -> String {
    let value = module
        .values
        .get(&name.into())
        .unwrap_or_else(|| panic!("no declaration `{}`", name));
    let canonical::Value::TypedValue { body, .. } = value else {
        panic!("`{}` should be annotated", name);
    };

    let mut expr = body;
    while let canonical::ExpressionKind::Apply(fun, _) = &expr.kind {
        expr = fun;
    }

    match &expr.kind {
        canonical::ExpressionKind::VarConstructor(name, _)
        | canonical::ExpressionKind::VarForeign(name, _, _) => name.to_name().as_str().to_string(),
        other => panic!("expected an imported name at the head, got {:?}", other),
    }
}

/// An imported constructor and an imported value are each named by the module that
/// declared them, however the importing source spells them: bare because exposed,
/// qualified by the module, or qualified by an alias.
///
/// `BUG-36`'s second root cause: an exposed constructor was named by the importing
/// module (`Main.Just`), an aliased one kept the alias (`M.Just`), and a qualified
/// value kept its whole spelling as its own name (`Maybe.Maybe.withDefault`).
///
/// Mutation-checked two ways in `Expression::from_parser`: naming a `VarConstructor`
/// by its written spelling fails the test with `Main.Just`, and qualifying a
/// `VarForeign` with the whole written spelling fails it with
/// `Maybe.Maybe.withDefault`.
#[test]
fn an_imported_name_is_named_by_the_module_that_declared_it() {
    let mut interfaces = scalar_interfaces();
    let (name, interface) = maybe_interface();
    interfaces.insert(name, interface);

    let canonicalize = |source| {
        canonicalize_with_interfaces(source, &interfaces)
            .unwrap_or_else(|e| panic!("should canonicalize, got {:?}", e))
    };

    // `Maybe` as the default imports bring it in: its constructors exposed.
    let module = canonicalize(indoc::indoc! {r#"
        module Main exposing (..)

        exposed : Maybe Int
        exposed = Just 1

        qualified : Maybe Int
        qualified = Maybe.Just 1

        value : Int
        value = Maybe.withDefault 0 Nothing
    "#});

    assert_eq!(head_of(&module, "exposed"), "Maybe.Just");
    assert_eq!(head_of(&module, "qualified"), "Maybe.Just");
    assert_eq!(head_of(&module, "value"), "Maybe.withDefault");

    // `Maybe` imported under an alias, which replaces the default import.
    let module = canonicalize(indoc::indoc! {r#"
        module Main exposing (..)

        import Maybe as M exposing (Maybe(..))

        aliased : Maybe Int
        aliased = M.Just 1

        aliasedValue : Int
        aliasedValue = M.withDefault 0 Nothing
    "#});

    assert_eq!(head_of(&module, "aliased"), "Maybe.Just");
    assert_eq!(head_of(&module, "aliasedValue"), "Maybe.withDefault");
}

// ── Scenario 15: a constraint context in front of an annotation (LANG-37) ─────
//
// The grammar parses what precedes `=>` as a type, so canonicalization is what
// decides that it is one constraint or a parenthesised list of them
// (`docs/spec/type-classes.md#a-constraint-belongs-to-a-signature-not-to-a-type`),
// and that no facade signature carries one
// (`docs/spec/type-classes.md#a-constrained-function-may-not-be-a-foreign-facade`).

/// The byte range of `needle`'s only occurrence in `source`.
fn range_of(source: &str, needle: &str) -> std::ops::Range<usize> {
    let start = source.find(needle).expect("source contains the needle");
    assert_eq!(
        source.rfind(needle),
        Some(start),
        "`{}` must occur once in the source for its range to be unambiguous",
        needle
    );
    start..start + needle.len()
}

/// A well-formed context canonicalizes, and the value's type is the one after
/// `=>`: the context is kept beside the type, and nothing about it leaks into the
/// canonical `Type`.
///
/// Verified to fail by making `validate_context` reject every constraint (its
/// first arm never matching, so an applied name falls through to `Unapplied`):
/// the module is then rejected and `expect` panics.
#[test]
fn well_formed_constraint_context_is_accepted() {
    let source = indoc::indoc! {r#"
        module Test exposing (lookup)

        class Eq a where
          eq : a -> a -> Bool

        class Comparable a where
          lt : a -> a -> Bool

        lookup : (Comparable k, Eq v) => k -> v -> Bool
        lookup key value =
          True
    "#};
    let module = canonicalize_with_scalars(source).expect("should canonicalize");

    match module.values.get(&"lookup".into()) {
        Some(canonical::Value::TypedValue { tpe, .. }) => assert_eq!(
            *tpe,
            canonical::Type::Arrow(
                Box::new(canonical::Type::Variable("k".into())),
                Box::new(canonical::Type::Arrow(
                    Box::new(canonical::Type::Variable("v".into())),
                    Box::new(bool_t()),
                )),
            )
        ),
        other => panic!("expected a TypedValue for `lookup`, got {:?}", other),
    }
}

/// `Int -> Int => a -> a` parses — the grammar cannot tell a context from a
/// type — and is rejected here, naming what was written and putting the caret
/// under the function type alone rather than the whole annotation.
///
/// Verified to fail by deleting the `validate_context` call in
/// `classes::constraints` (returning no constraint in its place): the module then
/// canonicalizes cleanly and `expect_err` panics.
#[test]
fn function_type_as_constraint_context_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (f)
        f : Int -> Int => a -> a
        f x =
          x
    "#};

    let errors = canonicalize_with_scalars(source)
        .expect_err("a function type in front of `=>` must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::InvalidConstraint(kind, _) => {
            assert_eq!(*kind, canonical::InvalidConstraintKind::Arrow)
        }
        other => panic!("expected InvalidConstraint, got {:?}", other),
    }

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(labels[0].span.to_range(), range_of(source, "Int -> Int"));
}

/// Each member of a parenthesised list is checked on its own, and each bad
/// one is reported at its own span: `(Int, Char)` is a perfectly good tuple
/// type and two errors as a context.
///
/// Verified to fail by making `parser::Context::from_type` keep a tuple whole
/// instead of splitting it into the list: one `InvalidConstraint(Tuple, …)`
/// spanning the whole list is then reported instead of two.
#[test]
fn every_malformed_constraint_of_a_list_is_reported_at_its_own_span() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (f)
        f : (Int, Char) => a -> a
        f x =
          x
    "#};

    let errors = canonicalize_with_scalars(source)
        .expect_err("a context of two bare type names must not compile");
    assert_eq!(errors.len(), 2, "got {:?}", errors);

    for (error, written) in errors.iter().zip(["Int", "Char"]) {
        match error {
            canonical::Error::InvalidConstraint(
                canonical::InvalidConstraintKind::Unapplied(n),
                _,
            ) => {
                assert_eq!(n.as_str(), written)
            }
            other => panic!("expected InvalidConstraint(Unapplied), got {:?}", other),
        }

        let labels = error.labels();
        assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
        assert_eq!(labels[0].span.to_range(), range_of(source, written));
    }
}

/// A list of four constraints is checked element by element like a shorter one,
/// and a malformed fourth is reported with its caret under the fourth alone.
///
/// Verified to fail by deleting `ConstrainedType`'s four-or-more production: the
/// module then fails to parse and `canonicalize_with_scalars` panics.
#[test]
fn malformed_fourth_constraint_of_four_is_reported_at_its_own_span() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (f)
        f : (Eq a, Eq b, Eq c, Bool) => a -> b -> c -> a
        f x y z =
          x
    "#};

    let errors = canonicalize_with_scalars(source)
        .expect_err("a bare type name as the fourth constraint must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::InvalidConstraint(canonical::InvalidConstraintKind::Unapplied(n), _) => {
            assert_eq!(n.as_str(), "Bool")
        }
        other => panic!("expected InvalidConstraint(Unapplied), got {:?}", other),
    }

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(labels[0].span.to_range(), range_of(source, "Bool"));
}

/// Only the outermost parentheses in front of `=>` are the list: a tuple nested
/// in it is an element, and a tuple is not a constraint.
///
/// Verified to fail by making `parser::Context::from_type` flatten nested tuples
/// as well as the outermost one: the context is then three good constraints and
/// the module canonicalizes, so `expect_err` panics.
#[test]
fn tuple_nested_in_a_constraint_list_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (f)
        f : ((Eq a, Eq b), Eq c) => a -> b -> c -> a
        f x y z =
          x
    "#};

    let errors =
        canonicalize_with_scalars(source).expect_err("a tuple nested in the list must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::InvalidConstraint(kind, _) => {
            assert_eq!(*kind, canonical::InvalidConstraintKind::Tuple)
        }
        other => panic!("expected InvalidConstraint(Tuple), got {:?}", other),
    }

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(labels[0].span.to_range(), range_of(source, "(Eq a, Eq b)"));
}

/// A facade signature may not be constrained, however well-formed the
/// constraint is. `compare` is `unsafe`, so LANG-68's effect-shape check has
/// nothing to say about it and this is the only error; its caret sits under
/// the context.
///
/// Verified to fail by disabling the `source.binding_foreign` branch that pushes
/// `FacadeConstrained` in `canonicalize`: the facade then canonicalizes
/// cleanly and `expect_err` panics.
#[test]
fn constrained_facade_signature_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module foreign Test exposing (compare)
        unsafe compare : Comparable a => Int -> Int -> Int
    "#};

    let errors = canonicalize_with_scalars(source)
        .expect_err("a constrained facade signature must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::FacadeConstrained(name, _) => assert_eq!(name.as_str(), "compare"),
        other => panic!("expected FacadeConstrained, got {:?}", other),
    }

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(labels[0].span.to_range(), range_of(source, "Comparable a"));
}

// ── Scenario 16: the unit type, `()` (LANG-72) ───────────────────────────────
//
// `()` is its own form in all three positions — a type, an expression and a
// pattern — and never a tuple: `Tuple` holds two or three elements by its shape.
// Each position's grammar production builds its own `Unit` node, and
// canonicalization carries it over unchanged. `NodeSpan`'s equality is blind, so
// each test also pins where the node was written.

/// The byte range of `needle`'s last occurrence in `source` — the body, in a
/// declaration whose annotation spells the same text first.
fn last_range_of(source: &str, needle: &str) -> std::ops::Range<usize> {
    let start = source.rfind(needle).expect("source contains the needle");
    start..start + needle.len()
}

/// `()` as a type and as an expression, in the chapter's own example
/// (`docs/spec/types.md#the-unit-type`).
///
/// Verified to fail by deleting the `"(" ")"` production from `AtomicType` in
/// `grammar.lalrpop` (the annotation stops parsing) and, separately, from
/// `AtomicExpr` (the body stops parsing); and by mapping
/// `parser::ExpressionKind::Unit` to `ExpressionKind::Int(0)` in
/// `Expression::from_parser`, which the whole-value comparison catches.
#[test]
fn unit_type_and_value_canonicalize() {
    let source = indoc::indoc! {r#"
        module Test exposing (nothingUseful)
        nothingUseful : ()
        nothingUseful = ()
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    let value = module.values.get(&"nothingUseful".into()).unwrap();
    assert_eq!(
        value,
        &canonical::Value::TypedValue {
            context: vec![],
            marked_unsafe: false,
            span: NodeSpan::none(),
            annotation_span: NodeSpan::none(),
            name: "nothingUseful".into(),
            patterns: vec![],
            body: canonical::Expression::bare(canonical::ExpressionKind::Unit),
            tpe: canonical::Type::Unit,
        }
    );

    match value {
        canonical::Value::TypedValue { body, .. } => assert_eq!(
            body.span.to_range(),
            Some(last_range_of(source, "()")),
            "the value's span must cover both parentheses"
        ),
        other => panic!("expected a TypedValue, got {:?}", other),
    }
}

/// `()` as a parameter's pattern, in the patterns chapter's own example
/// (`docs/spec/patterns.md#the-unit-pattern`): it binds nothing, and the
/// parameter's type is the unit type.
///
/// Verified to fail by deleting the `"(" ")"` production from `Pattern` in
/// `grammar.lalrpop` (the binding stops parsing), and by mapping
/// `parser::PatternKind::Unit` to `PatternKind::Anything` in
/// `Pattern::from_parser`, which the comparison catches.
#[test]
fn unit_pattern_canonicalizes() {
    let source = indoc::indoc! {r#"
        module Test exposing (Flag, always)
        type Flag
          = On
          | Off
        always : () -> Flag
        always () =
          On
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    let patterns = match module.values.get(&"always".into()) {
        Some(canonical::Value::TypedValue { patterns, .. }) => patterns,
        other => panic!("expected a TypedValue for `always`, got {:?}", other),
    };

    assert_eq!(
        patterns,
        &vec![(
            canonical::Pattern::bare(canonical::PatternKind::Unit),
            canonical::Type::Unit,
        )]
    );
    assert_eq!(
        patterns[0].0.span.to_range(),
        Some(last_range_of(source, "()")),
        "the pattern's span must cover both parentheses"
    );
}

/// The unit type in variant position is rejected like a tuple type is, with the
/// caret over it.
///
/// Verified to fail by changing the `parser::TypeKind::Unit` arm of `do_types` to
/// report `InvalidVariantKind::Tuple`: the variant match then panics.
#[test]
fn unit_in_variant_position_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Nothing
          = ()
    "#};

    let errors = canonicalize_standalone(source).expect_err("`()` is not a variant");

    match &errors[0] {
        canonical::Error::InvalidVariant(canonical::InvalidVariantKind::Unit, _) => (),
        other => panic!("expected InvalidVariant(Unit), got {:?}", other),
    }

    assert!(
        errors[0].message().contains("unit type"),
        "the message must name what was written, got {:?}",
        errors[0].message()
    );

    assert_eq!(
        only_invalid_variant_label(&errors).span.to_range(),
        variant_range(source, "()"),
        "the caret must cover the unit type"
    );
}

/// The unit type in front of `=>` is not a constraint.
///
/// Verified to fail by changing the `parser::TypeKind::Unit` arm of
/// `validate_context` to report `InvalidConstraintKind::Tuple`: the kind
/// assertion then fails.
#[test]
fn unit_as_constraint_context_is_rejected() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing (f)
        f : () => a -> a
        f x =
          x
    "#};

    let errors =
        canonicalize_with_scalars(source).expect_err("`()` in front of `=>` must not compile");
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    match &errors[0] {
        canonical::Error::InvalidConstraint(kind, _) => {
            assert_eq!(*kind, canonical::InvalidConstraintKind::Unit)
        }
        other => panic!("expected InvalidConstraint, got {:?}", other),
    }

    let labels = errors[0].labels();
    assert_eq!(labels.len(), 1, "expected one label, got {:?}", labels);
    assert_eq!(labels[0].span.to_range(), range_of(source, "()"));
}

/// `()` is an admitted type
/// (`docs/spec/interop.md#which-types-may-cross-the-boundary`), as a facade's
/// parameter, as its result, and as a tuple's element.
///
/// Verified to fail by changing `check_facade_admitted_type`'s `Type::Unit` arm
/// to `Err(FacadeRejectedKind::Variable)`: the module is then rejected and
/// `expect` panics.
#[test]
fn facade_signature_over_unit_is_accepted() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (tick, wrap)
        unsafe tick : () -> ()
        unsafe wrap : Int -> (Int, ())
    "#};

    canonicalize_with_scalars(source).expect("`()` is admitted in a facade signature");
}

// ── String literals ──────────────────────────────────────────────────────────

/// A string literal reaches the canonical AST holding its value, escape sequences
/// already read — `\n` is one line feed, not a backslash and an `n`.
///
/// Verified by mapping `Literal::String` to `ExpressionKind::String(String::new())` in
/// `Expression::from_parser`: the assertion goes red.
#[test]
fn string_literal_canonicalizes_to_its_value() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        greeting = "hi\nthere"
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    assert_eq!(
        module.values.get(&"greeting".into()).unwrap(),
        &canonical::Value::Value {
            span: NodeSpan::none(),
            name: "greeting".into(),
            patterns: vec![],
            body: canonical::Expression::bare(canonical::ExpressionKind::String(
                "hi\nthere".to_owned()
            )),
        }
    );
}

/// A string literal in a `case` branch is a literal pattern holding its value.
///
/// Verified by mapping `Literal::String` to `PatternKind::String(String::new())` in
/// `Pattern::from_parser`: the assertion goes red.
#[test]
fn string_literal_pattern_canonicalizes_to_its_value() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        isHello s =
          case s of
            "hello" ->
              1

            _ ->
              0
    "#};
    let module = canonicalize_standalone(source).expect("should canonicalize");

    let body = match module.values.get(&"isHello".into()).unwrap() {
        canonical::Value::Value { body, .. } => body,
        other => panic!("expected an unannotated value, got {:?}", other),
    };
    let branches = match &body.kind {
        canonical::ExpressionKind::Case(_, branches) => branches,
        other => panic!("expected a case, got {:?}", other),
    };

    assert_eq!(
        branches[0].pattern.kind,
        canonical::PatternKind::String("hello".to_owned())
    );
}

/// The shared stand-ins for `std/core`'s opaquely exposed types reject a `(..)`
/// entry the way the real modules do, so a test or a spec block cannot import
/// `Int(..)`, `Char(..)`, `String(..)` or `Task(..)` and pass here only to fail in
/// a real build.
///
/// Mutation-checked by emptying `opaque_unions` in `basics_interface`: the `Int(..)`
/// case then resolves and `expect_err` panics.
#[test]
fn the_stand_in_interfaces_for_opaque_core_types_reject_a_constructor_entry() {
    use zelkova_compiler::PhaseError;

    let interfaces: HashMap<_, _> = vec![
        basics_interface(),
        char_interface(),
        string_interface(),
        task_interface(),
    ]
    .into_iter()
    .collect();

    for (module, entry) in [
        ("Basics", "Int(..)"),
        ("Basics", "Float(..)"),
        ("Char", "Char(..)"),
        ("String", "String(..)"),
        ("Task", "Task(..)"),
    ] {
        let source = format!(
            "module Main exposing ()\nimport {} exposing ({})\n",
            module, entry
        );
        let errors = canonicalize_with_interfaces(&source, &interfaces)
            .expect_err(&format!("`{}` should not resolve", entry));
        assert_eq!(errors.len(), 1, "{}: got {:?}", entry, errors);
        assert_eq!(
            errors[0].message(),
            format!(
                "`{}` exposes the type `{}` but not its constructors",
                module,
                entry.trim_end_matches("(..)")
            ),
            "{}",
            entry
        );
    }

    // `Task` exposes `Failure` with its constructors, so that entry still resolves.
    let source = "module Main exposing ()\nimport Task exposing (Failure(..))\n";
    canonicalize_with_interfaces(source, &interfaces)
        .expect("`Failure(..)` is exposed with its constructors");
}

// ── A declaration that fails canonicalization costs only itself ─────────────

/// The names `values` holds, sorted.
fn sorted_value_names(module: &canonical::Module) -> Vec<&str> {
    let mut names: Vec<&str> = module.values.keys().map(|name| name.as_str()).collect();
    names.sort();
    names
}

/// `TOOL-9`'s reproduction of `BUG-34`: `Pair`'s tuple variant is the one error, and
/// `Size`, declared beside it, keeps its constructor, so `small` still canonicalizes.
///
/// Mutation-checked by putting `HashMap::new()` back for what `do_types` returns in
/// `canonicalize`: `types` comes back empty, `Small` is not found, and the error count
/// goes red.
#[test]
fn a_type_that_fails_does_not_cost_its_sibling_its_constructors() {
    let source = indoc::indoc! {r#"
        module Example exposing (Size, Pair, small)

        type Size
          = Small

        type Pair
          = (Size, Size)

        small : Size
        small = Small
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::new());

    assert!(
        matches!(
            errors.as_slice(),
            [canonical::Error::InvalidVariant(
                canonical::InvalidVariantKind::Tuple,
                _
            )]
        ),
        "got {:?}",
        errors
    );
    assert!(module.types.contains_key(&"Size".into()));
    assert!(!module.types.contains_key(&"Pair".into()));
    assert_eq!(sorted_value_names(&module), vec!["small"]);
    assert!(module.broken.is_empty(), "got {:?}", module.broken);
}

/// `BUG-34`'s first case: a mistyped variant beside a sound sibling `type`, both
/// exposed, reports the variant and nothing about either export.
///
/// Mutation-checked by deleting the loop that registers every type name with
/// `insert_declared_type` before `do_types` runs: `do_exports` then finds no `Pair`,
/// and an `ExportNotFound` turns the error count red.
#[test]
fn a_mistyped_variant_reports_only_itself() {
    let source = indoc::indoc! {r#"
        module Example exposing (Size, Pair)

        type Size
          = Small

        type Pair
          = (Size, Size)
    "#};

    let errors = canonicalize_standalone(source).expect_err("a tuple is not a variant");

    assert!(
        matches!(
            errors.as_slice(),
            [canonical::Error::InvalidVariant(
                canonical::InvalidVariantKind::Tuple,
                _
            )]
        ),
        "got {:?}",
        errors
    );
}

/// `BUG-34`'s second case: an imported type applied at the wrong arity inside a
/// variant reports the arity and nothing about the export of the type it is in.
///
/// Mutation-checked the same way as [`a_mistyped_variant_reports_only_itself`]: with
/// the `insert_declared_type` loop deleted, `B` is not found as an export and the error
/// count goes red.
#[test]
fn a_variant_at_the_wrong_arity_reports_only_the_arity() {
    let source = indoc::indoc! {r#"
        module Example exposing (B)

        import Maybe exposing (Maybe)

        type B
          = MkB Maybe
    "#};

    let errors = canonicalize_with_interfaces(source, &HashMap::from([maybe_interface()]))
        .expect_err("`Maybe` takes one argument");

    assert!(
        matches!(errors.as_slice(), [canonical::Error::TypeArityMismatch(..)]),
        "got {:?}",
        errors
    );
}

/// A function whose body uses an operator nothing declares is broken, with the
/// annotation it was written with, and its caller still canonicalizes.
///
/// Mutation-checked by recording a broken declaration whose annotation canonicalized
/// with `tpe: None` (the `(Annotation::Canonical(tpe), Err(body_error))` arm of
/// `do_values`): the `tpe` assertion goes red.
#[test]
fn a_body_that_fails_is_broken_and_keeps_its_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (f, g)

        f : Int -> Int
        f x = x <+> 1

        g : Int
        g = f 1
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::VariableNotFound(..)]),
        "got {:?}",
        errors
    );
    assert_eq!(sorted_value_names(&module), vec!["g"]);

    match module.broken.as_slice() {
        [broken] => {
            assert_eq!(broken.name.as_str(), "f");
            assert_eq!(
                broken.tpe,
                Some(canonical::Type::Arrow(Box::new(int_t()), Box::new(int_t())))
            );

            let annotation = "f : Int -> Int";
            let start = source.find(annotation).expect("source annotates `f`");
            assert_eq!(
                broken.annotation_span.to_range(),
                Some(start..start + annotation.len())
            );
        }
        other => panic!("expected `f` alone to be broken, got {:?}", other),
    }
}

/// An annotation that fails costs the declaration its type, and its error is the only
/// one when the body is sound.
///
/// Pins the `(Annotation::Failed(..), Ok(..))` arm of `do_values`. Mutation-checked by
/// making that arm record the declaration as a `Value::Value`: `broken` comes back
/// empty and the assertion on it goes red.
#[test]
fn an_annotation_that_fails_is_broken_with_no_type() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        f : Nope -> Int
        f x = x
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::TypeNotFound(..)]),
        "got {:?}",
        errors
    );
    assert!(module.values.is_empty(), "got {:?}", module.values);
    match module.broken.as_slice() {
        [broken] => {
            assert_eq!(broken.name.as_str(), "f");
            assert_eq!(broken.tpe, None);
            assert!(broken.annotation_span.to_range().is_none());
        }
        other => panic!("expected `f` alone to be broken, got {:?}", other),
    }
}

/// An annotation that fails does not hide a body that fails: both are reported, the
/// annotation's first.
///
/// Mutation-checked by dropping the body's error from the
/// `(Annotation::Failed(..), body)` arm of `do_values`: the `VariableNotFound` goes
/// missing and the assertion goes red.
#[test]
fn an_annotation_and_a_body_that_fail_are_both_reported() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        f : Nope -> Int
        f x = x <+> 1
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(
            errors.as_slice(),
            [
                canonical::Error::TypeNotFound(..),
                canonical::Error::VariableNotFound(..)
            ]
        ),
        "got {:?}",
        errors
    );
    assert_eq!(
        module
            .broken
            .iter()
            .map(|broken| (broken.name.as_str(), broken.tpe.is_some()))
            .collect::<Vec<_>>(),
        vec![("f", false)]
    );
}

/// An `exposing` entry that names nothing costs the module that entry and nothing
/// else: the interface exposes `ok`, and still does not expose the unexposed `hidden`.
///
/// Mutation-checked by answering `Exports::Everything` from `do_exports` whenever it
/// has errors, as it did while a module with errors was thrown away: `hidden` reaches
/// the interface and the assertion goes red.
#[test]
fn an_export_that_fails_exposes_only_the_entries_that_resolved() {
    let source = indoc::indoc! {r#"
        module A exposing (ok, missing)

        ok : Int
        ok = 1

        hidden : Int
        hidden = 2
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(
            errors.as_slice(),
            [canonical::Error::ExportNotFound(name, canonical::ExportType::Value, _)]
                if name.as_str() == "missing"
        ),
        "got {:?}",
        errors
    );

    let interface = module.to_interface(None);
    let mut exposed: Vec<&str> = interface.values.keys().map(|name| name.as_str()).collect();
    exposed.sort();
    assert_eq!(exposed, vec!["ok"]);
}

/// A broken declaration whose annotation canonicalized reaches the interface by that
/// annotation: as a value when the header exposes it by name, and as an exposed
/// operator's backing function otherwise. Neither has an arity, since a broken
/// declaration's parameters are unknown.
///
/// Mutation-checked twice, each going red: leaving `self.broken` out of `values` in
/// `to_interface` drops `f`, and reading only `self.values` in `declared_type` drops
/// `add` from `infix_functions`.
#[test]
fn a_broken_declaration_reaches_the_interface_by_its_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (f, (|+|))

        infix left 6 (|+|) = add

        f : Int -> Int
        f x = x <+> 1

        add : Int -> Int -> Int
        add a b = a <+> b
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));
    assert_eq!(errors.len(), 2, "got {:?}", errors);

    let interface = module.to_interface(None);
    let int_to_int = canonical::Type::Arrow(Box::new(int_t()), Box::new(int_t()));

    assert_eq!(
        interface
            .values
            .get(&"f".into())
            .map(|signature| &signature.tpe),
        Some(&int_to_int)
    );
    assert_eq!(
        interface
            .infix_functions
            .get(&"add".into())
            .map(|signature| &signature.tpe),
        Some(&canonical::Type::Arrow(
            Box::new(int_t()),
            Box::new(int_to_int.clone())
        ))
    );
    assert!(interface.arities.is_empty(), "got {:?}", interface.arities);
}

/// A facade signature whose type canonicalized and which a later check rejects is
/// broken with that type, and the facade's sound signatures still canonicalize.
///
/// Mutation-checked by recording a signature `check_facade_signature` rejected with
/// `tpe: None`: the `tpe` assertion goes red.
#[test]
fn a_rejected_facade_signature_is_broken_and_keeps_its_type() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (now, inc)

        now : Int

        unsafe inc : Int -> Int
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &effect_interfaces());

    assert!(
        matches!(
            errors.as_slice(),
            [canonical::Error::FacadeResultNotEffect(name, _)] if name.as_str() == "now"
        ),
        "got {:?}",
        errors
    );
    assert_eq!(sorted_value_names(&module), vec!["inc"]);
    match module.broken.as_slice() {
        [broken] => {
            assert_eq!(broken.name.as_str(), "now");
            assert_eq!(broken.tpe, Some(int_t()));
        }
        other => panic!("expected `now` alone to be broken, got {:?}", other),
    }
}

/// A facade signature that has a binding is rejected for the binding, and the
/// signature's own error is reported beside it: a type that does not resolve, or no
/// signature at all.
///
/// Mutation-checked by restoring `tpe.and_then(Result::ok)` in the facade's binding
/// check, which reports `BindingPatternsInvalidLen` alone, or by dropping the
/// `NoTypeInBinding` of a missing signature: the error list goes red either way.
#[test]
fn a_facade_signature_with_a_binding_still_reports_its_own_error() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (one, two, three)

        one : Nope
        one x = x

        two x = x

        unsafe three : Int -> Int
        three x = x
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &effect_interfaces());

    // The declarations are not canonicalized in a fixed order, so the errors are counted
    // by kind.
    let count = |is: fn(&canonical::Error) -> bool| errors.iter().filter(|e| is(e)).count();
    assert_eq!(errors.len(), 5, "got {:?}", errors);
    assert_eq!(
        count(|e| matches!(e, canonical::Error::TypeNotFound(..))),
        1,
        "got {:?}",
        errors
    );
    assert_eq!(
        count(|e| matches!(e, canonical::Error::NoTypeInBinding(two, _) if two.as_str() == "two")),
        1,
        "got {:?}",
        errors
    );
    assert_eq!(
        count(|e| matches!(e, canonical::Error::BindingPatternsInvalidLen(..))),
        3,
        "got {:?}",
        errors
    );
    assert!(module.values.is_empty(), "got {:?}", module.values);

    let broken: Vec<(&str, bool)> = module
        .broken
        .iter()
        .map(|b| (b.name.as_str(), b.tpe.is_some()))
        .collect();
    assert_eq!(
        broken,
        vec![("one", false), ("three", true), ("two", false)]
    );
}

/// An exposed declaration that is broken and was written with no annotation is
/// reported as `ExportedValueNotAnnotated`, as an unannotated declaration that
/// canonicalized is, for an explicit entry and under `exposing (..)` alike. One that was
/// written with an annotation, sound or not, is not: its annotation's own error stands.
///
/// Mutation-checked by emptying `unannotated_broken` where `canonicalize` builds it, by
/// dropping its use in `do_exports`'s `Lower` arm, and by dropping it from the `Open` arm:
/// each turns an assertion on the unannotated names red.
#[test]
fn an_exposed_broken_declaration_with_no_annotation_is_not_annotated() {
    let explicit = indoc::indoc! {r#"
        module Test exposing (f, g, h, k)

        f x = x <+> 1

        g : Int -> Int
        g x = x <+> 1

        h : Nope
        h = 1

        k : Int
        k = 1
    "#};

    let unannotated = |errors: &[canonical::Error]| -> Vec<String> {
        errors
            .iter()
            .filter_map(|e| match e {
                canonical::Error::ExportedValueNotAnnotated(name, _, _) => {
                    Some(name.as_str().to_string())
                }
                _ => None,
            })
            .collect()
    };

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(explicit, &HashMap::from([basics_interface()]));

    assert_eq!(
        module
            .broken
            .iter()
            .map(|b| b.name.as_str())
            .collect::<Vec<_>>(),
        vec!["f", "g", "h"]
    );
    assert_eq!(unannotated(&errors), vec!["f"], "got {:?}", errors);
    // `f` and `g`'s operator, `h`'s type, and `f`'s annotation.
    assert_eq!(errors.len(), 4, "got {:?}", errors);

    let open = explicit.replace("exposing (f, g, h, k)", "exposing (..)");
    let canonical::Canonicalized { errors, .. } =
        canonicalize_recovering_with_interfaces(&open, &HashMap::from([basics_interface()]));
    assert_eq!(unannotated(&errors), vec!["f"], "got {:?}", errors);
}

// ── TOOL-10: an incomplete scope drops the not-found errors that restate a failure ──

/// An empty interface for a module `Lib`, with the given `incomplete` flag: nothing
/// in it, so every name an importer asks of it is missing.
fn lib_interface(incomplete: bool) -> HashMap<zelkova_compiler::name::Name, Interface> {
    let interface = Interface {
        module_name: zelkova_compiler::ModuleName::new(test_package(), "Lib".into()),
        values: HashMap::new(),
        unions: HashMap::new(),
        opaque_unions: Default::default(),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete,
    };

    HashMap::from([("Lib".into(), interface)])
}

/// A failed `infix` declaration is one error. The operator it would have named is
/// missing from the module's scope, where it is used, and from its `exposing` list, and
/// neither is reported again.
///
/// Mutation-checked by not setting the flag after `do_infixes` in
/// `canonicalize_recovering`: `VariableNotFound` for the use and `ExportNotFound` for the
/// header entry come back.
#[test]
fn a_failed_infix_is_reported_once_and_not_again_by_its_use_and_its_export() {
    let source = indoc::indoc! {r#"
        module A exposing ((<+>), use)

        infix left 6 (<+>) = nope

        use : Int
        use = 1 <+> 2
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(
            errors.as_slice(),
            [canonical::Error::InfixReferenceInvalidValue(..)]
        ),
        "got {:?}",
        errors
    );
    assert!(module.incomplete);
}

/// A failed `type` declaration is one error. Its constructor, used below, is missing
/// from the scope and is not reported as a missing variant.
///
/// Mutation-checked by not setting the flag after `do_types` in
/// `canonicalize_recovering`: `VariantNotFound` for `MkT` comes back.
#[test]
fn a_failed_type_is_reported_once_and_not_again_by_its_constructor() {
    let source = indoc::indoc! {r#"
        module A exposing (k)

        type T = MkT | (T, T)

        k : T
        k = MkT
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::InvalidVariant(..)]),
        "got {:?}",
        errors
    );
    assert!(module.incomplete);
}

/// An import that does not resolve is one error and a module is returned. The use of
/// the module's name below is not reported again.
///
/// Mutation-checked by not setting the flag in `new_environment` when an import
/// fails: `VariableNotFound` for `Nope.y` comes back.
#[test]
fn an_unresolved_import_is_reported_once_and_the_module_is_returned() {
    let source = indoc::indoc! {r#"
        module A exposing (x)

        import Nope

        x : Int
        x = Nope.y
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    match errors.as_slice() {
        [error @ canonical::Error::EnvironmentErrors(inner)] => {
            use zelkova_compiler::PhaseError;

            assert_eq!(inner.len(), 1, "got {:?}", errors);
            assert_eq!(
                error.message(),
                "cannot find a module named `Nope` to import"
            );
        }
        other => panic!("expected one EnvironmentErrors, got {:?}", other),
    }
    assert!(module.incomplete);
}

/// The control for the three above: with nothing failed, a misspelt name in a body is
/// still `VariableNotFound`, and the module is not incomplete.
///
/// Mutation-checked by making `without_restated` ignore its flag (filtering
/// unconditionally): the error is dropped and the assertion goes red.
#[test]
fn a_misspelt_name_in_a_complete_scope_is_still_reported() {
    let source = indoc::indoc! {r#"
        module A exposing (x)

        x : Int
        x = nope
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::VariableNotFound(..)]),
        "got {:?}",
        errors
    );
    assert!(!module.incomplete);
}

/// The second control: only the five not-found errors are dropped in an incomplete
/// scope. `g` has an annotation and no binding, and is reported beside the import.
///
/// Mutation-checked by widening `without_restated`'s filter to drop every error:
/// `NoBindings` goes with the rest and the assertion goes red.
#[test]
fn an_incomplete_scope_still_reports_what_is_not_a_missing_name() {
    let source = indoc::indoc! {r#"
        module A exposing (g)

        import Nope

        g : Int
    "#};

    let canonical::Canonicalized { errors, .. } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(
            errors.as_slice(),
            [
                canonical::Error::EnvironmentErrors(..),
                canonical::Error::NoBindings(..)
            ]
        ),
        "got {:?}",
        errors
    );
}

/// An `exposing` entry missing from an interface that is itself missing names is
/// dropped, and the importer is incomplete in turn. Against a complete interface the
/// same entry is `ValueNotFound`.
///
/// Mutation-checked by ignoring `interface.incomplete` in `process_import`'s `Lower`
/// arm: the first assertion goes red with `ValueNotFound`.
#[test]
fn an_import_entry_missing_from_an_incomplete_interface_is_dropped() {
    let source = indoc::indoc! {r#"
        module Main exposing ()

        import Lib exposing (missing)
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &lib_interface(true));
    assert!(errors.is_empty(), "got {:?}", errors);
    assert!(module.incomplete);

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &lib_interface(false));
    match errors.as_slice() {
        [error @ canonical::Error::EnvironmentErrors(..)] => {
            use zelkova_compiler::PhaseError;

            assert_eq!(
                error.message(),
                "the imported module does not expose a value named `missing`"
            );
        }
        other => panic!("expected one EnvironmentErrors, got {:?}", other),
    }
    // The failed import is what makes this scope incomplete.
    assert!(module.incomplete);
}

/// The type and operator entries are dropped the same way, and a constructor entry for a
/// type the incomplete interface exposes only opaquely is not: that interface did publish
/// the type, so nothing was left out.
///
/// Mutation-checked by ignoring `interface.incomplete` in the `Public` (`Size(..)`) arm
/// and in the `Operator` arm of `process_import`, each going red on the first
/// assertion; and by dropping `ConstructorsNotExposed` along with them, which turns the
/// second red.
#[test]
fn an_incomplete_interface_drops_missing_types_and_operators_but_not_opaque_constructors() {
    let source = indoc::indoc! {r#"
        module Main exposing ()

        import Lib exposing (Gone(..), Away, (<+>))
    "#};

    let canonical::Canonicalized { errors, .. } =
        canonicalize_recovering_with_interfaces(source, &lib_interface(true));
    assert!(errors.is_empty(), "got {:?}", errors);

    let mut interfaces = opaque_and_clear_lib();
    let lib = interfaces.get_mut(&"Lib".into()).expect("Lib is there");
    lib.incomplete = true;
    let source = indoc::indoc! {r#"
        module Main exposing ()

        import Lib exposing (Opaque(..))
    "#};

    let canonical::Canonicalized { errors, .. } =
        canonicalize_recovering_with_interfaces(source, &interfaces);
    match errors.as_slice() {
        [error @ canonical::Error::EnvironmentErrors(..)] => {
            use zelkova_compiler::PhaseError;

            assert_eq!(
                error.message(),
                "`Lib` exposes the type `Opaque` but not its constructors"
            );
        }
        other => panic!("expected one EnvironmentErrors, got {:?}", other),
    }
}

/// An annotation that does not canonicalize is the one failure that leaves a name out
/// of the interface while the module's own scope is whole: the module is incomplete
/// though nothing else is wrong with it.
///
/// Mutation-checked by dropping the `annotation_failed` half of `Module::incomplete`:
/// the second assertion goes red.
#[test]
fn a_failed_annotation_makes_the_module_incomplete() {
    let source = indoc::indoc! {r#"
        module A exposing (f)

        f : Nope -> Int
        f x = 1
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::TypeNotFound(..)]),
        "got {:?}",
        errors
    );
    assert!(module.incomplete);
    assert!(module.to_interface(None).incomplete);
}

// ── TOOL-11: a declaration that failed to parse ─────────────────────────────

/// Parse `source` with `parser::parse_recovering`, which keeps the module beside its
/// syntax errors, and canonicalize it against `interfaces`. The syntax errors themselves
/// are the parser's to report and are not looked at here.
fn canonicalize_partly_parsed(
    source: &str,
    interfaces: &HashMap<zelkova_compiler::name::Name, Interface>,
) -> canonical::Canonicalized {
    use codespan_reporting::files::SimpleFile;
    use zelkova_syntax::parser;

    let file = SimpleFile::new("Test.zel".to_string(), source.to_string());
    let parsed = parser::parse_recovering(&file);
    assert!(
        !parsed.failures.is_empty(),
        "the source is meant to hold a syntax error"
    );
    let module = parsed.module.expect("the header parses");
    canonical::canonicalize_recovering(&test_package(), interfaces, &module)
}

/// A function whose binding failed to parse is broken with the annotation that did, no
/// error is reported for it, and a caller of it is a value: the syntax error is what says
/// what is wrong.
///
/// Mutation-checked by removing the test on the failed chunk's name from `do_values`:
/// `f` is then read as an annotation with no binding and `NoBindings` is reported.
#[test]
fn a_function_whose_binding_failed_to_parse_is_broken_with_its_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (f, g)

        f : Int -> Int
        f x = = x

        g : Int
        g = f 1
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_partly_parsed(source, &HashMap::from([basics_interface()]));

    assert!(errors.is_empty(), "got {:?}", errors);
    assert_eq!(sorted_value_names(&module), vec!["g"]);
    match module.broken.as_slice() {
        [broken] => {
            assert_eq!(broken.name.as_str(), "f");
            assert_eq!(
                broken.tpe,
                Some(canonical::Type::Arrow(Box::new(int_t()), Box::new(int_t())))
            );
        }
        other => panic!("expected `f` alone to be broken, got {:?}", other),
    }
    assert!(!module.incomplete);
}

/// A value with no annotation whose only declaration failed to parse is still a name in
/// scope, so a reference to it resolves and nothing is reported; it is broken with no
/// type, and since its annotation could have been the chunk that failed, the module is
/// incomplete.
///
/// Mutation-checked twice, each going red: not registering a failed chunk's name with
/// `insert_top_level_value` reports `g`'s reference to `h` as `VariableNotFound`, and
/// dropping the failed-chunk half of `annotation_failed` leaves `incomplete` false.
#[test]
fn a_value_that_failed_to_parse_with_no_annotation_makes_the_module_incomplete() {
    let source = indoc::indoc! {r#"
        module Test exposing (g)

        h = = 2

        g : Int
        g = h
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_partly_parsed(source, &HashMap::from([basics_interface()]));

    assert!(errors.is_empty(), "got {:?}", errors);
    assert_eq!(sorted_value_names(&module), vec!["g"]);
    match module.broken.as_slice() {
        [broken] => {
            assert_eq!(broken.name.as_str(), "h");
            assert_eq!(broken.tpe, None);
            let chunk = source.find("h = = 2").expect("source declares `h`");
            assert_eq!(broken.span.to_range().map(|range| range.start), Some(chunk));
        }
        other => panic!("expected `h` alone to be broken, got {:?}", other),
    }
    assert!(module.incomplete);
}

/// The control for the two tests above: an annotation with no binding in a module with no
/// syntax error is reported as `NoBindings`, because no failed chunk stands behind it.
///
/// Mutation-checked by making `do_values` treat every function as one a failed chunk
/// names (dropping the name test in its `unparsed.get`): `NoBindings` is silenced.
#[test]
fn an_annotation_with_no_binding_and_no_syntax_error_is_still_reported() {
    use codespan_reporting::files::SimpleFile;
    use zelkova_syntax::parser;

    let source = indoc::indoc! {r#"
        module Test exposing (f)

        f : Int
    "#};
    let file = SimpleFile::new("Test.zel".to_string(), source.to_string());
    let parsed = parser::parse_recovering(&file);
    assert!(parsed.failures.is_empty(), "got {:?}", parsed.failures);
    let module = parsed.module.expect("the module parses");

    let canonical::Canonicalized { errors, .. } = canonical::canonicalize_recovering(
        &test_package(),
        &HashMap::from([basics_interface()]),
        &module,
    );

    assert!(
        matches!(errors.as_slice(), [canonical::Error::NoBindings(..)]),
        "got {:?}",
        errors
    );
}

/// A `type` declaration that failed to parse names nothing that can be read off it, so
/// the scope is incomplete: a value naming the type or its constructor is broken, and
/// neither not-found is reported.
///
/// Mutation-checked by not setting the environment's flag for a failed chunk that names
/// nothing: `TypeNotFound` is reported for `k`'s annotation.
#[test]
fn a_type_that_failed_to_parse_makes_the_scope_incomplete() {
    let source = indoc::indoc! {r#"
        module Test exposing (k)

        type U = (

        k : U
        k = MkU
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_partly_parsed(source, &HashMap::from([basics_interface()]));

    assert!(errors.is_empty(), "got {:?}", errors);
    assert!(module.values.is_empty(), "got {:?}", module.values);
    match module.broken.as_slice() {
        [broken] => assert_eq!(broken.name.as_str(), "k"),
        other => panic!("expected `k` alone to be broken, got {:?}", other),
    }
    assert!(module.incomplete);
}

/// An `infix` declaration may name a function whose only declaration failed to parse:
/// the operator is declared and nothing is reported.
///
/// Mutation-checked by making `do_infixes` look only at the parsed functions:
/// `InfixReferenceInvalidValue` is reported for `(<+>)`.
#[test]
fn an_infix_may_name_a_function_that_failed_to_parse() {
    let source = indoc::indoc! {r#"
        module Test exposing ((<+>))

        infix left 6 (<+>) = plus

        plus x y = = x
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_partly_parsed(source, &HashMap::from([basics_interface()]));

    assert!(errors.is_empty(), "got {:?}", errors);
    assert!(
        module
            .infixes
            .contains_key(&zelkova_compiler::name::Name::from("<+>")),
        "got {:?}",
        module.infixes
    );
}

/// A facade signature that parsed beside a binding chunk of the same name that did not
/// is broken with its type, and nothing is reported for it.
///
/// Mutation-checked by removing the failed-chunk test from the facade branch of
/// `canonicalize_recovering`: `inc` is then a sound signature, a value.
#[test]
fn a_facade_signature_named_by_a_failed_chunk_is_broken_with_its_type() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (inc)

        unsafe inc : Int -> Int

        inc x = = x
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_partly_parsed(source, &effect_interfaces());

    assert!(errors.is_empty(), "got {:?}", errors);
    assert!(module.values.is_empty(), "got {:?}", module.values);
    match module.broken.as_slice() {
        [broken] => {
            assert_eq!(broken.name.as_str(), "inc");
            assert_eq!(
                broken.tpe,
                Some(canonical::Type::Arrow(Box::new(int_t()), Box::new(int_t())))
            );
        }
        other => panic!("expected `inc` alone to be broken, got {:?}", other),
    }
}

/// A function whose annotation is itself the chunk that failed to parse, beside a binding
/// that did, is exposed without `ExportedValueNotAnnotated`: the user did write an
/// annotation, and the syntax error says it is wrong.
///
/// Mutation-checked by removing `&& !unparsed.contains_key(&f.name)` from the
/// `unannotated_broken` filter in `canonicalize_recovering`: `f` is then told to
/// `do_exports` as an unannotated declaration and `ExportedValueNotAnnotated` is reported.
#[test]
fn an_exposed_function_whose_annotation_failed_to_parse_is_not_reported_unannotated() {
    let source = indoc::indoc! {r#"
        module Test exposing (f)

        f : Int ->

        f x = x
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_partly_parsed(source, &HashMap::from([basics_interface()]));

    assert!(errors.is_empty(), "got {:?}", errors);
    assert!(
        module
            .broken
            .iter()
            .any(|broken| broken.name.as_str() == "f"),
        "got {:?}",
        module.broken
    );
}

/// A function the parser kept an annotation of and a failed chunk also names is broken
/// over the whole of what it was written as: its span starts at the annotation and ends
/// at the end of the failed binding.
///
/// Mutation-checked by replacing the span merge in `Rejected::unparsed` with
/// `let _ = chunk;`: the span ends with the annotation and the end assertion goes red.
#[test]
fn a_function_named_by_a_failed_chunk_is_spanned_over_the_chunk_too() {
    let source = indoc::indoc! {r#"
        module Test exposing (f)

        f : Int -> Int
        f x = = x
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_partly_parsed(source, &HashMap::from([basics_interface()]));

    assert!(errors.is_empty(), "got {:?}", errors);
    let [broken] = module.broken.as_slice() else {
        panic!("expected `f` alone to be broken, got {:?}", module.broken);
    };
    let range = broken.span.to_range().expect("`f` is spanned");
    assert_eq!(
        range.start,
        source.find("f : Int").expect("source declares `f`")
    );
    assert!(
        range.end >= source.find("= x").expect("source holds the binding") + "= x".len(),
        "the span ends at {}, before the failed binding does",
        range.end
    );
}

/// A facade function named by a failed chunk is still checked against the rules of a
/// facade signature: an annotation that parsed and breaks one is reported beside the
/// syntax error, as it is for any other facade.
///
/// Mutation-checked by replacing `check_facade_signature(function, &tpe).err()` in the
/// failed-chunk branch of the facade loop with `None`: the error is silenced and the
/// assertion goes red.
#[test]
fn a_facade_signature_named_by_a_failed_chunk_still_reports_its_own_errors() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (id)

        unsafe id : a -> a

        id x = = x
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_partly_parsed(source, &effect_interfaces());

    assert!(
        matches!(
            errors.as_slice(),
            [canonical::Error::FacadeTypeNotAdmitted(..)]
        ),
        "got {:?}",
        errors
    );
    assert!(module.values.is_empty(), "got {:?}", module.values);
    assert!(
        module
            .broken
            .iter()
            .any(|broken| broken.name.as_str() == "id"),
        "got {:?}",
        module.broken
    );
}

// ── TOOL-12: an unresolved name inside a sound body is a hole ───────────────

/// The body of `name`, a value `module` holds, or a panic listing what it holds instead.
fn body_of<'m>(module: &'m canonical::Module, name: &str) -> &'m canonical::Expression {
    match module.values.get(&name.into()) {
        Some(canonical::Value::Value { body, .. })
        | Some(canonical::Value::TypedValue { body, .. }) => body,
        None => panic!(
            "expected `{}` to be a value, got values {:?} and broken {:?}",
            name,
            sorted_value_names(module),
            module.broken
        ),
    }
}

/// A value that does not resolve is a hole, and its declaration is kept: `g` is reported
/// once, is a value and not broken, and its body is the application of `f` to a hole
/// written where `nope` is.
///
/// Mutation-checked by returning the `VariableNotFound` from `Expression::from_parser`'s
/// `Variable` arm instead of pushing it: `g` is then broken and the assertion on
/// `broken` goes red.
#[test]
fn an_unresolved_value_in_a_sound_body_is_a_hole() {
    let source = indoc::indoc! {r#"
        module Test exposing (g)

        f : Int -> Int
        f x = x

        g : Int
        g = f nope
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::VariableNotFound(..)]),
        "got {:?}",
        errors
    );
    assert!(module.broken.is_empty(), "got {:?}", module.broken);

    match &body_of(&module, "g").kind {
        canonical::ExpressionKind::Apply(_, argument) => {
            assert!(
                matches!(argument.kind, canonical::ExpressionKind::Hole),
                "got {:?}",
                argument
            );
            assert_eq!(argument.span.to_range(), Some(range_of(source, "nope")));
        }
        other => panic!("expected `g` to apply `f`, got {:?}", other),
    }
}

/// A constructor that does not resolve, written as an expression, is a hole too.
///
/// Mutation-checked by returning the `VariantNotFound` from `Expression::from_parser`'s
/// `TypeConstructor` arm instead of pushing it: `g` has no body to read and the test
/// panics.
#[test]
fn an_unresolved_constructor_in_a_sound_body_is_a_hole() {
    let source = indoc::indoc! {r#"
        module Test exposing (g)

        g : Int
        g = Nope
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::VariantNotFound(..)]),
        "got {:?}",
        errors
    );
    let body = body_of(&module, "g");
    assert!(
        matches!(body.kind, canonical::ExpressionKind::Hole),
        "got {:?}",
        body
    );
    assert_eq!(body.span.to_range(), Some(range_of(source, "Nope")));
}

/// A constructor pattern that does not resolve is a hole holding its arguments, and the
/// names they bind are in scope in the branch: `y` is not reported.
///
/// Mutation-checked by not exposing a pattern hole's arguments in
/// `ScopedEnvironment::expose_pattern`: `y` is then a second missing name, and the
/// assertion on the errors goes red.
#[test]
fn an_unresolved_constructor_pattern_binds_its_arguments() {
    let source = indoc::indoc! {r#"
        module Test exposing (h)

        h : Int -> Int
        h x =
          case x of
            Nope y -> y
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::VariantNotFound(..)]),
        "got {:?}",
        errors
    );

    match &body_of(&module, "h").kind {
        canonical::ExpressionKind::Case(_, branches) => match branches.as_slice() {
            [branch] => {
                match &branch.pattern.kind {
                    canonical::PatternKind::Hole(args) => assert!(
                        matches!(args.as_slice(), [arg] if matches!(&arg.kind, canonical::PatternKind::Variable(y) if y.as_str() == "y")),
                        "got {:?}",
                        args
                    ),
                    other => panic!("expected a pattern hole, got {:?}", other),
                }
                assert_eq!(
                    branch.pattern.span.to_range(),
                    Some(range_of(source, "Nope y"))
                );
                assert!(
                    matches!(&branch.expression.kind, canonical::ExpressionKind::VarLocal(y) if y.as_str() == "y"),
                    "got {:?}",
                    branch.expression
                );
            }
            other => panic!("expected one branch, got {:?}", other),
        },
        other => panic!("expected `h` to be a `case`, got {:?}", other),
    }
}

/// The control for the three above: an operator that does not resolve is no hole. It
/// leaves its infix chain with nothing to associate by, so the declaration is broken as
/// before.
///
/// Mutation-checked by answering an operator `resolve_infix_operator` cannot resolve with
/// a hole standing for the whole chain, in the `InfixChain` arm of
/// `Expression::from_parser`: `g` is then a value and the assertion on `values` goes
/// red.
#[test]
fn an_unresolved_operator_still_breaks_its_declaration() {
    let source = indoc::indoc! {r#"
        module Test exposing (g)

        g : Int
        g = 1 <+> 2
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::VariableNotFound(..)]),
        "got {:?}",
        errors
    );
    assert!(module.values.is_empty(), "got {:?}", module.values);
    assert_eq!(
        module
            .broken
            .iter()
            .map(|broken| broken.name.as_str())
            .collect::<Vec<_>>(),
        vec!["g"]
    );
}

/// In a scope an unresolved import made incomplete, a hole's error is dropped like any
/// other missing name's, and the declaration is still kept: the import's error is the
/// only one, and `g` holds a hole where `Nope.y` is.
///
/// Mutation-checked by extending `canonicalize_recovering`'s errors with `do_values`'
/// unfiltered, skipping `without_restated`: the `VariableNotFound` for `Nope.y` comes
/// back and the assertion on the errors goes red.
#[test]
fn a_hole_in_an_incomplete_scope_drops_its_error_and_keeps_its_declaration() {
    let source = indoc::indoc! {r#"
        module Test exposing (g)

        import Nope

        f : Int -> Int
        f x = x

        g : Int
        g = f Nope.y
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &HashMap::from([basics_interface()]));

    assert!(
        matches!(errors.as_slice(), [canonical::Error::EnvironmentErrors(..)]),
        "got {:?}",
        errors
    );
    assert!(module.broken.is_empty(), "got {:?}", module.broken);
    assert!(module.incomplete);

    match &body_of(&module, "g").kind {
        canonical::ExpressionKind::Apply(_, argument) => {
            assert!(
                matches!(argument.kind, canonical::ExpressionKind::Hole),
                "got {:?}",
                argument
            );
            assert_eq!(argument.span.to_range(), Some(range_of(source, "Nope.y")));
        }
        other => panic!("expected `g` to apply `f`, got {:?}", other),
    }
}

// ── Records ──────────────────────────────────────────────────────────────────
//
// `docs/spec/records.md`: a record type is a set of fields, so its canonical form is
// order-independent; a record and an update keep the order their fields were written
// in, since each is a subexpression evaluated in that order; and a label given twice in
// any of the three is an error, under the repeat.

/// The annotation `name` was declared with.
fn annotation_of<'m>(module: &'m canonical::Module, name: &str) -> &'m canonical::Type {
    match module.values.get(&name.into()) {
        Some(canonical::Value::TypedValue { tpe, .. }) => tpe,
        other => panic!("expected `{}` to be annotated, got {:?}", name, other),
    }
}

/// `{ label : tpe, … }`, built the way canonicalization builds one.
fn record_t(fields: Vec<(&str, canonical::Type)>) -> canonical::Type {
    canonical::Type::Record(
        fields
            .into_iter()
            .map(|(label, tpe)| (label.into(), tpe))
            .collect(),
    )
}

/// The labels of a canonical record's or update's fields, in their order.
fn labels_of(fields: &[canonical::Field]) -> Vec<&str> {
    fields.iter().map(|field| field.label.as_str()).collect()
}

/// A record type is a set of fields: two spellings that order the same fields
/// differently canonicalize to one value, and each field's type is resolved as any
/// other type is.
///
/// Mutation-checked by keying each field in `from_parser_type` by its written position
/// as well as its label (`0low`, `1high`), which makes the map remember the source
/// order: `first` and `second` then differ and the first assertion goes red.
#[test]
fn a_record_type_is_the_same_type_in_any_field_order() {
    let module = canonicalize_with_scalars(indoc::indoc! {r#"
        module Test exposing (first, second)

        first : { low : Int, high : Char } -> Int
        first r = 1

        second : { high : Char, low : Int } -> Int
        second r = 1
    "#})
    .expect("a record type in an annotation canonicalizes");

    assert_eq!(
        annotation_of(&module, "first"),
        annotation_of(&module, "second")
    );
    assert_eq!(
        annotation_of(&module, "first"),
        &canonical::Type::Arrow(
            Box::new(record_t(vec![("high", char_t()), ("low", int_t())])),
            Box::new(int_t()),
        )
    );
}

/// The canonical field set is walked in label order — the order two spellings of one
/// record type agree on — whatever order the source wrote.
///
/// Mutation-checked by the position-keyed map described on the test above: the keys
/// then come back as written.
#[test]
fn a_record_types_fields_are_in_label_order() {
    let module = canonicalize_with_scalars(indoc::indoc! {r#"
        module Test exposing (f)

        f : { b : Int, ab : Int, a : Int } -> Int
        f r = 1
    "#})
    .expect("a record type canonicalizes");

    let canonical::Type::Arrow(record, _) = annotation_of(&module, "f") else {
        panic!(
            "expected a function type, got {:?}",
            annotation_of(&module, "f")
        );
    };
    let canonical::Type::Record(fields) = record.as_ref() else {
        panic!("expected a record type, got {:?}", record);
    };
    assert_eq!(
        fields
            .keys()
            .map(|label| label.as_str())
            .collect::<Vec<_>>(),
        vec!["a", "ab", "b"]
    );
}

/// A field's type that names nothing in scope is reported as any other type is, at its
/// own span.
///
/// Mutation-checked by replacing a field type that fails to canonicalize with `()` in
/// `from_parser_type`: the module then canonicalizes.
#[test]
fn a_record_types_field_resolves_its_type() {
    let source = indoc::indoc! {r#"
        module Test exposing (f)

        f : { a : Missing } -> Int
        f r = 1
    "#};
    let errors = canonicalize_with_scalars(source).expect_err("`Missing` is not a type");

    match errors.as_slice() {
        [canonical::Error::TypeNotFound(name, span)] => {
            assert_eq!(name.as_str(), "Missing");
            assert_eq!(span.to_range(), Some(range_of(source, "Missing")));
        }
        other => panic!("expected one TypeNotFound, got {:?}", other),
    }
}

/// A record expression keeps its fields in the order they were written — `b` before `a`
/// — because each is a subexpression, evaluated left to right, and the label set is the
/// only order-independent part of it. Each field's value is canonicalized in the scope
/// the record is written in.
///
/// Mutation-checked by sorting the fields by label in `Field::from_parser`: the
/// assertion on the labels then sees `a` first.
#[test]
fn a_record_keeps_the_order_its_fields_were_written_in() {
    let module = canonicalize_with_scalars(indoc::indoc! {r#"
        module Test exposing ()

        r x =
          { b = x, a = 2 }
    "#})
    .expect("a record canonicalizes");

    let canonical::ExpressionKind::Record(fields) = &body_of(&module, "r").kind else {
        panic!("expected a record, got {:?}", body_of(&module, "r"));
    };
    assert_eq!(labels_of(fields), vec!["b", "a"]);
    assert_eq!(fields[0].value, c_var_local("x"));
    assert_eq!(fields[1].value, c_int(2));
}

/// An update is the record it updates, canonicalized as any expression is, and its fields
/// in the order they were written.
///
/// Mutation-checked by building the update arm of `Expression::from_parser` from a reversed
/// field list: the label assertion goes red.
#[test]
fn an_update_keeps_its_record_and_the_order_of_its_fields() {
    let module = canonicalize_with_scalars(indoc::indoc! {r#"
        module Test exposing ()

        u r =
          { r | taken = 1, expected = 2 }
    "#})
    .expect("an update canonicalizes");

    let canonical::ExpressionKind::Update(record, fields) = &body_of(&module, "u").kind else {
        panic!("expected an update, got {:?}", body_of(&module, "u"));
    };
    assert_eq!(**record, c_var_local("r"));
    assert_eq!(labels_of(fields), vec!["taken", "expected"]);
}

/// The label spans of a repeated-label error: the primary under the repeat, the
/// secondary under the first field to give the label.
fn assert_repeated_at(
    error: &canonical::Error,
    label: &str,
    form: canonical::RecordForm,
    repeat: std::ops::Range<usize>,
    first: std::ops::Range<usize>,
) {
    use zelkova_compiler::PhaseError;

    match error {
        canonical::Error::RepeatedLabel(name, found, repeat_span, first_span) => {
            assert_eq!(name.as_str(), label);
            assert_eq!(*found, form);
            assert_eq!(repeat_span.to_range(), Some(repeat.clone()));
            assert_eq!(first_span.to_range(), Some(first.clone()));
        }
        other => panic!("expected RepeatedLabel, got {:?}", other),
    }

    let labels = error.labels();
    assert_eq!(labels.len(), 2, "expected two labels, got {:?}", labels);
    assert!(labels[0].primary, "the repeat carries the primary label");
    assert_eq!(
        labels[0].span.to_range(),
        repeat,
        "the caret under the repeat"
    );
    assert!(
        !labels[1].primary,
        "the first field carries a secondary label"
    );
    assert_eq!(
        labels[1].span.to_range(),
        first,
        "the label under the first"
    );
}

/// The byte range of the `nth` occurrence (from 0) of `needle` in `source`.
fn nth_range_of(source: &str, needle: &str, nth: usize) -> std::ops::Range<usize> {
    let start = source
        .match_indices(needle)
        .nth(nth)
        .unwrap_or_else(|| panic!("`{}` does not occur {} times", needle, nth + 1))
        .0;
    start..start + needle.len()
}

/// A label given twice in a record type is an error, with the caret under the repeated
/// label and not the record, and the declaration does not canonicalize.
///
/// Mutation-checked twice: making `repeated_labels` return `Ok(())` (the module then
/// canonicalizes and `expect_err` panics), and giving the error the record's span in
/// place of the repeat's label (the range assertion goes red).
#[test]
fn a_label_repeated_in_a_record_type_is_an_error_at_the_repeat() {
    let source = indoc::indoc! {r#"
        module Test exposing (f)

        f : { abc : Int, b : Int, abc : Char } -> Int
        f r = 1
    "#};
    let errors = canonicalize_with_scalars(source).expect_err("`abc` is given twice");

    let [error] = errors.as_slice() else {
        panic!("expected one error, got {:?}", errors);
    };
    assert_repeated_at(
        error,
        "abc",
        canonical::RecordForm::Type,
        nth_range_of(source, "abc", 1),
        nth_range_of(source, "abc", 0),
    );
}

/// The same in a record, where it is checked although the fields' order is kept.
///
/// Mutation-checked by removing the `repeated_labels` call from `Field::from_parser`: the
/// module then canonicalizes.
#[test]
fn a_label_repeated_in_a_record_is_an_error_at_the_repeat() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        r =
          { abc = 1, b = 2, abc = 3 }
    "#};
    let errors = canonicalize_with_scalars(source).expect_err("`abc` is given twice");

    let [error] = errors.as_slice() else {
        panic!("expected one error, got {:?}", errors);
    };
    assert_repeated_at(
        error,
        "abc",
        canonical::RecordForm::Record,
        nth_range_of(source, "abc", 1),
        nth_range_of(source, "abc", 0),
    );
}

/// The same in an update.
///
/// Mutation-checked as the record's test is, by removing the `repeated_labels` call
/// from `Field::from_parser`, which the two share.
#[test]
fn a_label_repeated_in_an_update_is_an_error_at_the_repeat() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        u r =
          { r | abc = 1, abc = 2 }
    "#};
    let errors = canonicalize_with_scalars(source).expect_err("`abc` is given twice");

    let [error] = errors.as_slice() else {
        panic!("expected one error, got {:?}", errors);
    };
    assert_repeated_at(
        error,
        "abc",
        canonical::RecordForm::Update,
        nth_range_of(source, "abc", 1),
        nth_range_of(source, "abc", 0),
    );
}

/// A label given three times is two errors, each under its own repeat and both naming
/// the first, grouped so that the group's labels are every member's.
///
/// Mutation-checked by recording each repeat against the field before it rather than the
/// first: the second error's secondary label then sits under the second `abc`.
#[test]
fn a_label_given_three_times_is_two_errors_naming_the_first() {
    use zelkova_compiler::PhaseError;

    let source = indoc::indoc! {r#"
        module Test exposing ()

        r =
          { abc = 1, abc = 2, abc = 3 }
    "#};
    let errors = canonicalize_with_scalars(source).expect_err("`abc` is given three times");

    let [canonical::Error::Many(members)] = errors.as_slice() else {
        panic!("expected one group, got {:?}", errors);
    };
    let [second, third] = members.as_slice() else {
        panic!("expected two errors, got {:?}", members);
    };
    let first = nth_range_of(source, "abc", 0);
    assert_repeated_at(
        second,
        "abc",
        canonical::RecordForm::Record,
        nth_range_of(source, "abc", 1),
        first.clone(),
    );
    assert_repeated_at(
        third,
        "abc",
        canonical::RecordForm::Record,
        nth_range_of(source, "abc", 2),
        first,
    );
    assert_eq!(
        errors[0].labels().len(),
        4,
        "the group flattens its members' labels"
    );
}

/// A record type is not a variant: a `type` declaration's body naming one is rejected
/// at the record, as every other shape that is not a constructor is.
///
/// Mutation-checked by making `do_types`' record arm build a constructor of no
/// arguments: the module then canonicalizes.
#[test]
fn a_record_type_is_not_a_variant() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        type Point
          = { x : Int }
    "#};
    let errors = canonicalize_with_scalars(source).expect_err("a record is not a variant");

    match errors.as_slice() {
        [canonical::Error::InvalidVariant(canonical::InvalidVariantKind::Record, span)] => {
            assert_eq!(span.to_range(), Some(range_of(source, "{ x : Int }")));
        }
        other => panic!("expected InvalidVariant(Record), got {:?}", other),
    }
}

/// A record type is not a constraint: one written before `=>` is rejected at itself.
///
/// Mutation-checked by reporting it as `InvalidConstraintKind::Unit` in `validate_context`:
/// the match then falls through to its panic.
#[test]
fn a_record_type_is_not_a_constraint() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        f : { x : a } => a -> a
        f y = y
    "#};
    let errors = canonicalize_with_scalars(source).expect_err("a record is not a constraint");

    match errors.as_slice() {
        [canonical::Error::InvalidConstraint(canonical::InvalidConstraintKind::Record, span)] => {
            assert_eq!(span.to_range(), Some(range_of(source, "{ x : a }")));
        }
        other => panic!("expected InvalidConstraint(Record), got {:?}", other),
    }
}

/// A record type is admitted in a facade signature when every field's type is
/// ([Which types may cross the
/// boundary](../../docs/spec/interop.md#which-types-may-cross-the-boundary)), so a
/// function type inside a field is rejected as one anywhere else is, and a record of
/// admitted fields is not.
///
/// Mutation-checked by making `check_facade_admitted_type`'s record arm answer `Ok(())`:
/// `apply` then canonicalizes and the first assertion goes red.
#[test]
fn a_facade_record_is_admitted_when_its_fields_are() {
    let errors = canonicalize_with_scalars(indoc::indoc! {r#"
        module foreign Test exposing (apply)

        unsafe apply : { f : Int -> Int } -> Int
    "#})
    .expect_err("a function type inside a record cannot cross");
    match errors.as_slice() {
        [canonical::Error::FacadeTypeNotAdmitted(name, kind, _)] => {
            assert_eq!(name.as_str(), "apply");
            assert_eq!(*kind, canonical::FacadeRejectedKind::Function);
        }
        other => panic!("expected FacadeTypeNotAdmitted, got {:?}", other),
    }

    canonicalize_with_scalars(indoc::indoc! {r#"
        module foreign Test exposing (point)

        unsafe point : Int -> { x : Int, y : (Int, Char) }
    "#})
    .expect("a record of admitted fields is admitted");
}

/// `Task` inside a record's field is as misplaced as it is anywhere else in a facade
/// signature, whether the record is a parameter or the result: the record does not hide
/// it from the "`Task` nowhere else" rule.
///
/// Mutation-checked by making `contains_task`'s record arm answer `false`: both
/// signatures then canonicalize and the first `expect_err` panics.
#[test]
fn task_inside_a_record_field_is_rejected() {
    for source in [
        indoc::indoc! {r#"
            module foreign Test exposing (f)

            unsafe f : { t : Task Int } -> Int
        "#},
        indoc::indoc! {r#"
            module foreign Test exposing (f)

            unsafe f : Int -> { t : Task Int }
        "#},
    ] {
        let errors = canonicalize_with_effects(source)
            .expect_err("`Task` inside a record's field is not admitted");

        match errors.as_slice() {
            [canonical::Error::FacadeTaskMisplaced(name, _)] => {
                assert_eq!(name.as_str(), "f");
            }
            other => panic!("expected one FacadeTaskMisplaced, got {:?}", other),
        }
    }
}

/// A record's fields and an update's record are part of the body a parameterless
/// binding depends on, so a cycle running through them is reported.
///
/// Mutation-checked by making `collect_top_level_refs`'s record and update arms collect
/// nothing: the module then canonicalizes.
#[test]
fn a_cycle_through_a_record_is_a_self_dependency() {
    for source in [
        "module Test exposing ()\n\na =\n  { x = a }\n",
        "module Test exposing ()\n\na =\n  { a | x = 1 }\n",
    ] {
        let errors = canonicalize_with_scalars(source).expect_err("`a` depends on its own value");
        assert!(
            matches!(errors.as_slice(), [canonical::Error::SelfDependency(..)]),
            "{}: got {:?}",
            source,
            errors
        );
    }
}

/// A name that does not resolve inside a record field or an update's record is a hole,
/// and the declaration holding it says so.
///
/// Mutation-checked by making `expression_holds_hole`'s record and update arms answer
/// `false`: `holds_hole` then answers `false` for the declarations.
#[test]
fn a_hole_inside_a_record_is_held_by_its_declaration() {
    for source in [
        "module Test exposing ()\n\nr =\n  { x = nope }\n",
        "module Test exposing ()\n\nr =\n  { nope | x = 1 }\n",
    ] {
        let canonical::Canonicalized { module, errors } =
            canonicalize_recovering_with_interfaces(source, &scalar_interfaces());
        assert!(
            matches!(errors.as_slice(), [canonical::Error::VariableNotFound(..)]),
            "{}: got {:?}",
            source,
            errors
        );
        let value = module.values.get(&"r".into()).expect("`r` is kept");
        assert!(value.holds_hole(), "{}: `r` holds a hole", source);
    }
}

// ── Record patterns ──────────────────────────────────────────────────────────
//
// `docs/spec/records.md`, *Record patterns*: a record pattern reaches the canonical
// module as its entries, in the order written, each a label and a whole pattern; it
// binds what those patterns bind and nothing for its labels; and a label given twice is
// the error the other three record forms report.

/// The patterns `name` was declared with, its parameters in order.
fn parameters_of<'m>(module: &'m canonical::Module, name: &str) -> Vec<&'m canonical::Pattern> {
    match module.values.get(&name.into()) {
        Some(canonical::Value::Value { patterns, .. }) => patterns.iter().collect(),
        Some(canonical::Value::TypedValue { patterns, .. }) => {
            patterns.iter().map(|(pattern, _)| pattern).collect()
        }
        None => panic!("expected `{}` to be a value", name),
    }
}

/// A record pattern is `PatternKind::Record`, its entries in the order they were written
/// — `b` before `a` — each keeping its label's span and its own pattern, canonicalized as
/// any pattern is.
///
/// Mutation-checked twice: sorting the entries by label in `Pattern::from_parser`'s record
/// arm (the label assertion sees `a` first), and giving each entry the record pattern's
/// span in place of its label's (the label range assertion goes red).
#[test]
fn a_record_pattern_keeps_its_entries_in_written_order() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        f { b = x, a = 1 } =
          x
    "#};
    let module = canonicalize_with_scalars(source).expect("a record pattern canonicalizes");

    let [pattern] = parameters_of(&module, "f")[..] else {
        panic!("expected one parameter");
    };
    let canonical::PatternKind::Record(entries) = &pattern.kind else {
        panic!("expected a record pattern, got {:?}", pattern);
    };
    assert_eq!(
        entries
            .iter()
            .map(|entry| entry.label.as_str())
            .collect::<Vec<_>>(),
        vec!["b", "a"]
    );
    assert_eq!(entries[0].pattern, p_var("x"));
    assert_eq!(
        entries[1].pattern,
        canonical::Pattern::bare(canonical::PatternKind::Int(1))
    );
    assert_eq!(
        entries[0].label_span.to_range(),
        Some(range_of(source, "b =").start..range_of(source, "b =").start + 1)
    );
    assert_eq!(
        pattern.span.to_range(),
        Some(range_of(source, "{ b = x, a = 1 }"))
    );
}

/// A record pattern binds what its entries' patterns bind, at any depth: `x` from the
/// shorthand inside a field, `t` from a field's variable, the record inside a
/// constructor's argument. It binds nothing for a label: `centre` names a field, and a
/// body using it is a name that does not resolve.
///
/// Mutation-checked twice: making `ScopedEnvironment::expose_pattern`'s record arm expose
/// nothing (`x` and `t` are then unresolved and `depth` is not a value), and making it
/// expose each entry's label as a variable as well (`centre` then resolves and the
/// assertion on the errors goes red).
#[test]
fn a_nested_record_pattern_binds_its_entries_variables_and_not_its_labels() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        type Celsius
          = Celsius

        type Reading
          = Reading { centre : { x : Celsius }, taken : Celsius }

        depth r =
          case r of
            Reading { centre = { x }, taken = t } ->
              (x, t)

        label r =
          case r of
            Reading { centre = { x } } ->
              centre
    "#};

    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &scalar_interfaces());

    match errors.as_slice() {
        [canonical::Error::VariableNotFound(name, span, _)] => {
            assert_eq!(name.unqualified_name().as_str(), "centre");
            assert_eq!(span.to_range(), Some(nth_range_of(source, "centre", 3)));
        }
        other => panic!("expected `centre` alone to be unresolved, got {:?}", other),
    }

    let canonical::ExpressionKind::Case(_, branches) = &body_of(&module, "depth").kind else {
        panic!("expected `depth` to be a `case`");
    };
    assert_eq!(
        branches[0].expression,
        c_tuple(Tuple::two(c_var_local("x"), c_var_local("t")))
    );
}

/// A name bound by two entries is not reported here, as one bound by two tuple elements
/// is not: both are `LANG-18`'s gap, and in both the body's `x` resolves to a local.
///
/// Mutation-checked by making `ScopedEnvironment::expose_pattern`'s record arm expose
/// nothing: `x` is then unresolved in `record` alone and the comparison goes red.
#[test]
fn a_name_bound_by_two_entries_is_treated_as_one_bound_by_two_tuple_elements() {
    let module = canonicalize_with_scalars(indoc::indoc! {r#"
        module Test exposing ()

        tuple (x, x) =
          x

        record { a = x, b = x } =
          x
    "#})
    .expect("a repeated name is not reported (LANG-18)");

    assert_eq!(body_of(&module, "tuple"), &c_var_local("x"));
    assert_eq!(body_of(&module, "record"), body_of(&module, "tuple"));
}

/// A label given twice in a record pattern is the error the other three forms report,
/// its caret under the repeat and the first entry's label named, the shorthand included,
/// and its message says it was found in a record pattern.
///
/// Mutation-checked three times: removing the `repeated_labels` call from
/// `Pattern::from_parser`'s record arm (the module then canonicalizes and `expect_err`
/// panics), passing it `RecordForm::Record` (the form assertion goes red), and spelling
/// `RecordForm::Pattern`'s message "record" (the message assertion goes red).
#[test]
fn a_label_repeated_in_a_record_pattern_is_an_error_at_the_repeat() {
    use zelkova_compiler::PhaseError;

    for source in [
        "module Test exposing ()\n\nf { abc = x, b = y, abc = z } =\n  1\n",
        "module Test exposing ()\n\nf { abc, b, abc } =\n  1\n",
    ] {
        let errors = canonicalize_with_scalars(source).expect_err("`abc` is given twice");

        let [error] = errors.as_slice() else {
            panic!("{}: expected one error, got {:?}", source, errors);
        };
        assert_repeated_at(
            error,
            "abc",
            canonical::RecordForm::Pattern,
            nth_range_of(source, "abc", 1),
            nth_range_of(source, "abc", 0),
        );
        assert_eq!(
            error.message(),
            "`abc` labels two fields of one record pattern, and a label may be given only once"
        );
    }
}

/// A constructor that does not resolve inside a record pattern's entry is a hole, the
/// declaration holding it says so, and what the hole's arguments bind is in scope: `y`
/// is not reported.
///
/// Mutation-checked twice: making `pattern_holds_hole`'s record arm answer `false`
/// (`holds_hole` then answers `false` for `f`), and making
/// `ScopedEnvironment::expose_pattern`'s record arm expose nothing (`y` is then a second
/// missing name and the assertion on the errors goes red).
#[test]
fn a_hole_inside_a_record_pattern_is_held_by_its_declaration() {
    let source =
        "module Test exposing ()\n\nf r =\n  case r of\n    { a = (Nope y) } ->\n      y\n";
    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &scalar_interfaces());

    assert!(
        matches!(errors.as_slice(), [canonical::Error::VariantNotFound(..)]),
        "got {:?}",
        errors
    );
    let value = module.values.get(&"f".into()).expect("`f` is kept");
    assert!(value.holds_hole(), "`f` holds a hole");
}

// ── Reading a field ──────────────────────────────────────────────────────────
//
// `docs/spec/records.md`, *Reading a field* and *The accessor*: `r.name` and `.name`
// reach the canonical module with their record canonicalized as any expression is and
// their label, which names no declaration, left as written.

/// An access keeps its record, canonicalized in the scope it is written in, and its
/// label; an accessor keeps its label. Each label carries the span of the label alone.
///
/// Mutation-checked by giving the canonical `Access` and `Accessor` the node's span in
/// place of the parser's label span in `Expression::from_parser`: the two label range
/// assertions go red.
#[test]
fn an_access_and_an_accessor_keep_their_labels() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        get r =
          r.name

        pick =
          .label
    "#};
    let module = canonicalize_with_scalars(source).expect("an access and an accessor canonicalize");

    let get = body_of(&module, "get");
    let canonical::ExpressionKind::Access(record, label, label_span) = &get.kind else {
        panic!("expected an access, got {:?}", get);
    };
    assert_eq!(**record, c_var_local("r"));
    assert_eq!(label.as_str(), "name");
    assert_eq!(label_span.to_range(), Some(range_of(source, "name")));
    assert_eq!(get.span.to_range(), Some(range_of(source, "r.name")));

    let pick = body_of(&module, "pick");
    let canonical::ExpressionKind::Accessor(label, label_span) = &pick.kind else {
        panic!("expected an accessor, got {:?}", pick);
    };
    assert_eq!(label.as_str(), "label");
    let accessor = range_of(source, ".label");
    assert_eq!(
        label_span.to_range(),
        Some(accessor.start + 1..accessor.end)
    );
    assert_eq!(pick.span.to_range(), Some(accessor));
}

/// `Maybe .withDefault` is `Maybe` applied to an accessor, and `Maybe` names a module
/// and no constructor, so it is reported as a constructor that does not resolve, under
/// `Maybe`. The same spacing after a constructor is an ordinary application of it, and
/// with no space the name is the qualified `Maybe.withDefault`.
///
/// This is where `LANG-52`'s rejection of `Widget .size` went once an accessor existed:
/// the parser reads the source, and canonicalization is the phase that rejects it.
///
/// Mutation-checked by making `consume_operator` never yield `AccessorDot`: the first
/// source then fails to parse and `parse_source` panics.
#[test]
fn a_module_name_applied_to_an_accessor_is_no_constructor() {
    let mut interfaces = scalar_interfaces();
    let (name, interface) = maybe_interface();
    interfaces.insert(name, interface);

    let source = indoc::indoc! {r#"
        module Test exposing ()

        import Maybe

        f =
          Maybe .withDefault
    "#};
    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("`Maybe` is a module and not a constructor");
    match errors.as_slice() {
        [canonical::Error::VariantNotFound(name, span, _)] => {
            assert_eq!(name.unqualified_name().as_str(), "Maybe");
            // The second `Maybe`: the first is the import's.
            assert_eq!(span.to_range(), Some(nth_range_of(source, "Maybe", 1)));
        }
        other => panic!("expected one VariantNotFound, got {:?}", other),
    }

    let module = canonicalize_with_interfaces(
        indoc::indoc! {r#"
            module Test exposing ()

            import Maybe exposing (Maybe(..))

            f =
              Just .withDefault

            g =
              Maybe.withDefault
        "#},
        &interfaces,
    )
    .expect("a constructor applied to an accessor, and a qualified name, canonicalize");

    let f = body_of(&module, "f");
    let canonical::ExpressionKind::Apply(ctor, arg) = &f.kind else {
        panic!("expected an application, got {:?}", f);
    };
    assert!(
        matches!(&ctor.kind, canonical::ExpressionKind::VarConstructor(name, _) if name.unqualified_name().as_str() == "Just"),
        "expected `Just`, got {:?}",
        ctor
    );
    assert!(
        matches!(&arg.kind, canonical::ExpressionKind::Accessor(label, _) if label.as_str() == "withDefault"),
        "expected the accessor `.withDefault`, got {:?}",
        arg
    );
    assert!(
        matches!(
            &body_of(&module, "g").kind,
            canonical::ExpressionKind::VarForeign(..)
        ),
        "expected `Maybe.withDefault` to be the imported value, got {:?}",
        body_of(&module, "g")
    );
}

/// An access's record is part of the body a parameterless binding depends on, so a
/// cycle running through it is reported.
///
/// Mutation-checked by making `collect_top_level_refs`'s `Access` arm collect nothing:
/// the module then canonicalizes.
#[test]
fn a_cycle_through_an_access_is_a_self_dependency() {
    let errors = canonicalize_with_scalars("module Test exposing ()\n\na =\n  a.x\n")
        .expect_err("`a` depends on its own value");
    assert!(
        matches!(errors.as_slice(), [canonical::Error::SelfDependency(..)]),
        "got {:?}",
        errors
    );
}

/// A name that does not resolve inside an access's record is a hole, reported under the
/// name, and the declaration holding it says so.
///
/// Mutation-checked by making `expression_holds_hole`'s `Access` arm answer `false`:
/// `holds_hole` then answers `false` for `r`.
#[test]
fn a_hole_inside_an_access_is_held_by_its_declaration() {
    let source = "module Test exposing ()\n\nr =\n  nope.x\n";
    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &scalar_interfaces());
    match errors.as_slice() {
        [canonical::Error::VariableNotFound(name, span, _)] => {
            assert_eq!(name.unqualified_name().as_str(), "nope");
            assert_eq!(span.to_range(), Some(range_of(source, "nope")));
        }
        other => panic!("expected one VariableNotFound, got {:?}", other),
    }
    let value = module.values.get(&"r".into()).expect("`r` is kept");
    assert!(value.holds_hole(), "`r` holds a hole");
}

// ── LANG-39: classes and instances, resolved ──

/// Canonicalize `source` against `interfaces`, insist that it canonicalizes, and add its
/// interface to `interfaces` under its own name, so a module after it can import it.
fn publish(source: &str, interfaces: &mut HashMap<zelkova_compiler::name::Name, Interface>) {
    let module = canonicalize_with_interfaces(source, interfaces)
        .unwrap_or_else(|errors| panic!("expected the module to canonicalize, got {:?}", errors));
    let interface = module.to_interface(None);
    interfaces.insert(interface.module_name.name().clone(), interface);
}

/// The errors `source` canonicalizes with against the scalars, insisting that there are
/// some.
fn class_errors(source: &str) -> Vec<canonical::Error> {
    canonicalize_with_scalars(source).expect_err("expected the module to be rejected")
}

/// Every label `error` renders with, as the byte ranges they underline, primary first.
fn label_ranges(error: &canonical::Error) -> Vec<(bool, std::ops::Range<usize>)> {
    use zelkova_compiler::PhaseError;

    let mut labels: Vec<_> = error
        .labels()
        .into_iter()
        .map(|label| (label.primary, label.span.to_range()))
        .collect();
    labels.sort_by_key(|(primary, _)| !*primary);
    labels
}

/// The byte range of the `nth` occurrence of `needle` in `source`, counting from zero.
fn nth_range(source: &str, needle: &str, nth: usize) -> std::ops::Range<usize> {
    let start = source
        .match_indices(needle)
        .nth(nth)
        .unwrap_or_else(|| panic!("`{}` does not occur {} times", needle, nth + 1))
        .0;
    start..start + needle.len()
}

/// A class and its instances are in the canonical module: the members' types, the
/// superclass, an instance's context and its bindings.
///
/// Mutation-checked by building every class's `superclasses` as empty in
/// `class_signature`, and every instance's `context` as empty in `do_instances`: the
/// matching assertion goes red.
#[test]
fn a_class_and_its_instances_are_in_the_module() {
    let source = indoc::indoc! {r#"
        module Test exposing (Eq, Comparable, Colour, Order(..))

        type Order
          = LT
          | EQ
          | GT

        type Colour
          = Red

        type Box a
          = Box a

        class Eq a where
          eq : a -> a -> Bool

        class Eq a => Comparable a where
          compare : a -> a -> Order

        instance Eq Colour where
          eq x y =
            True

        instance Comparable Colour where
          compare x y =
            EQ

        instance Eq a => Eq (Box a) where
          eq left right =
            True
    "#};
    let module = canonicalize_with_scalars(source).expect("the module canonicalizes");

    let eq = test_qual("Test.Eq");
    let a = || canonical::Type::Variable("a".into());
    let order = canonical::Type::Type(test_qual("Test.Order"), vec![]);
    let arrow = |l, r| canonical::Type::Arrow(Box::new(l), Box::new(r));

    let comparable = &module.classes[&"Comparable".into()].signature;
    assert_eq!(comparable.variable.as_str(), "a");
    let [superclass] = comparable.superclasses.as_slice() else {
        panic!("one superclass, got {:?}", comparable.superclasses);
    };
    assert_eq!(superclass.class, eq);
    assert_eq!(superclass.variable.as_str(), "a");
    let [compare] = comparable.members.as_slice() else {
        panic!("one member, got {:?}", comparable.members);
    };
    assert_eq!(compare.name.as_str(), "compare");
    assert_eq!(compare.tpe, arrow(a(), arrow(a(), order)));

    let eq_members = &module.classes[&"Eq".into()].signature.members;
    assert_eq!(eq_members[0].tpe, arrow(a(), arrow(a(), bool_t())));

    assert_eq!(module.instances.len(), 3);
    let boxed = &module.instances[2];
    assert_eq!(boxed.signature.class, eq);
    assert_eq!(
        boxed.signature.head,
        canonical::InstanceHead::Type(test_qual("Test.Box"), vec!["a".into()])
    );
    let [context] = boxed.signature.context.as_slice() else {
        panic!("one constraint, got {:?}", boxed.signature.context);
    };
    assert_eq!(context.class, eq);
    assert_eq!(context.variable.as_str(), "a");
    let [canonical::Value::Value { name, patterns, .. }] = boxed.bindings.as_slice() else {
        panic!("one binding, got {:?}", boxed.bindings);
    };
    assert_eq!(name.as_str(), "eq");
    assert_eq!(patterns.len(), 2);
}

/// The module every instance-head test below writes its one instance into: a class, and
/// the types a head can name.
fn with_instance(instance: &str) -> String {
    format!(
        "{}\n{}",
        indoc::indoc! {r#"
            module Test exposing ()

            type Box a
              = Box a

            type Pair a b
              = Pair a b

            class Eq a where
              eq : a -> a -> Bool
        "#},
        instance
    )
}

/// The range of the one label `error` renders with, insisting that it is primary and
/// alone.
fn sole_label(error: &canonical::Error) -> std::ops::Range<usize> {
    match label_ranges(error).as_slice() {
        [(true, range)] => range.clone(),
        other => panic!("expected one primary label, got {:?}", other),
    }
}

/// The one error `source` is rejected with, which has to be an instance-head error, and
/// the range of its label.
fn head_problem(source: &str) -> (canonical::InstanceHeadProblem, std::ops::Range<usize>) {
    match class_errors(source).as_slice() {
        [error @ canonical::Error::InvalidInstanceHead(problem, _)] => {
            (problem.clone(), sole_label(error))
        }
        other => panic!("expected one InvalidInstanceHead, got {:?}", other),
    }
}

/// An argument of the head's type that is not a variable is an error under it.
///
/// Mutation-checked by accepting any argument in `distinct_variables`: the module
/// canonicalizes.
#[test]
fn an_instance_head_applied_to_a_concrete_type_is_an_error() {
    let source = with_instance("instance Eq (Box Int) where\n  eq x y =\n    True\n");
    let (problem, range) = head_problem(&source);
    assert_eq!(problem, canonical::InstanceHeadProblem::ArgumentNotVariable);
    assert_eq!(range, range_of(&source, "Int"));
}

/// A variable written twice in a head is an error under the second.
///
/// Mutation-checked by never recording a variable as seen in `distinct_variables`.
#[test]
fn an_instance_head_repeating_a_variable_is_an_error() {
    let source = with_instance("instance Eq (Pair a a) where\n  eq x y =\n    True\n");
    let (problem, range) = head_problem(&source);
    assert_eq!(
        problem,
        canonical::InstanceHeadProblem::RepeatedVariable("a".into())
    );
    let second = source.find("Pair a a").expect("the head") + "Pair a ".len();
    assert_eq!(range, second..second + 1);
}

/// A function type is not a head.
///
/// Mutation-checked by answering `Unit` for an arrow in `instance_head_type`.
#[test]
fn an_instance_head_naming_a_function_type_is_an_error() {
    let source = with_instance("instance Eq (b -> c) where\n  eq x y =\n    True\n");
    let (problem, range) = head_problem(&source);
    assert_eq!(problem, canonical::InstanceHeadProblem::Function);
    assert_eq!(range, range_of(&source, "b -> c"));
}

/// A bare variable is not a head.
///
/// Mutation-checked by answering `Unit` for a variable in `instance_head_type`.
#[test]
fn an_instance_head_naming_a_bare_variable_is_an_error() {
    let source = with_instance("instance Eq b where\n  eq x y =\n    True\n");
    let (problem, range) = head_problem(&source);
    assert_eq!(
        problem,
        canonical::InstanceHeadProblem::Variable("b".into())
    );
    let at = source.find("instance Eq b").expect("the head") + "instance Eq ".len();
    assert_eq!(range, at..at + 1);
}

/// A class applied to two types is an error under the whole head.
///
/// Mutation-checked by reading only the first argument of a head in `instance_head`.
#[test]
fn an_instance_head_applying_its_class_to_two_types_is_an_error() {
    let source = with_instance("instance Eq Int Bool where\n  eq x y =\n    True\n");
    let (problem, range) = head_problem(&source);
    assert_eq!(problem, canonical::InstanceHeadProblem::ClassApplied(2));
    assert_eq!(range, range_of(&source, "Eq Int Bool"));
}

/// A context may constrain only a variable the head binds.
///
/// Mutation-checked by accepting every variable in `constraints`' bound check.
#[test]
fn an_instance_context_on_a_variable_the_head_does_not_bind_is_an_error() {
    let source = with_instance("instance Eq c => Eq (Box a) where\n  eq x y =\n    True\n");
    match class_errors(&source).as_slice() {
        [error @ canonical::Error::ConstraintVariableUnbound(name, _)] => {
            assert_eq!(name.as_str(), "c");
            let at = source.find("Eq c").expect("the context") + "Eq ".len();
            assert_eq!(sole_label(error), at..at + 1);
        }
        other => panic!("expected one ConstraintVariableUnbound, got {:?}", other),
    }
}

/// A member signature with a context of its own is an error under the context.
///
/// Mutation-checked by dropping the `MemberConstrained` push in `class_signature`.
#[test]
fn a_member_signature_with_a_constraint_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Eq a where
          eq : a -> a -> Bool

        class Same a where
          same : Eq b => a -> b -> Bool
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::MemberConstrained(name, _)] => {
            assert_eq!(name.as_str(), "same");
            assert_eq!(sole_label(error), range_of(source, "Eq b"));
        }
        other => panic!("expected one MemberConstrained, got {:?}", other),
    }
}

/// A member signature marked `unsafe` is an error, the caret starting at the word.
///
/// Mutation-checked by dropping the `MemberUnsafe` push in `class_signature`.
#[test]
fn a_member_signature_marked_unsafe_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Same a where
          unsafe same : a -> a -> Bool
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::MemberUnsafe(name, _)] => {
            assert_eq!(name.as_str(), "same");
            assert_eq!(
                sole_label(error),
                range_of(source, "unsafe same : a -> a -> Bool")
            );
        }
        other => panic!("expected one MemberUnsafe, got {:?}", other),
    }
}

/// A member signature that never mentions the class variable is an error under it.
///
/// Mutation-checked by making `mentions` answer `true` for every type.
#[test]
fn a_member_signature_not_mentioning_the_class_variable_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Sized a where
          size : Int
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::MemberMissesClassVariable(name, class, variable, _)] => {
            assert_eq!(name.as_str(), "size");
            assert_eq!(class.as_str(), "Sized");
            assert_eq!(variable.as_str(), "a");
            assert_eq!(sole_label(error), range_of(source, "size : Int"));
        }
        other => panic!("expected one MemberMissesClassVariable, got {:?}", other),
    }
}

/// The module every instance-body test below writes its one instance into.
fn with_eq_instance(bindings: &str) -> String {
    format!(
        "{}\ninstance Eq Colour where\n{}",
        indoc::indoc! {r#"
            module Test exposing ()

            type Colour
              = Red

            class Eq a where
              eq : a -> a -> Bool
              neq : a -> a -> Bool
        "#},
        bindings
    )
}

/// A member no binding defines is an error under the instance's head line, naming the
/// member and the class.
///
/// Mutation-checked by dropping the missing-member loop in `instance_bindings`.
#[test]
fn an_instance_missing_a_member_is_an_error_naming_it_and_the_class() {
    use zelkova_compiler::PhaseError;

    let source = with_eq_instance("  eq x y =\n    True\n");
    let errors = class_errors(&source);
    let [error @ canonical::Error::InstanceMemberMissing(member, class, _)] = errors.as_slice()
    else {
        panic!("expected one InstanceMemberMissing, got {:?}", errors);
    };
    assert_eq!(member.as_str(), "neq");
    assert_eq!(class.as_str(), "Eq");
    assert_eq!(
        sole_label(error),
        range_of(&source, "instance Eq Colour where")
    );
    let message = error.message();
    assert!(
        message.contains("`neq`") && message.contains("`Eq`"),
        "{}",
        message
    );
}

/// A binding that names no member is an error under the binding.
///
/// Mutation-checked by skipping the membership test in `instance_bindings`.
#[test]
fn an_instance_binding_naming_no_member_is_an_error() {
    let source =
        with_eq_instance("  eq x y =\n    True\n  neq x y =\n    False\n  other x =\n    True\n");
    match class_errors(&source).as_slice() {
        [error @ canonical::Error::InstanceBindingNotMember(name, class, _)] => {
            assert_eq!(name.as_str(), "other");
            assert_eq!(class.as_str(), "Eq");
            let start = source.find("other x").expect("the binding");
            assert_eq!(sole_label(error).start, start);
        }
        other => panic!("expected one InstanceBindingNotMember, got {:?}", other),
    }
}

/// A member bound twice is an error under the second binding, with the first labelled.
///
/// Mutation-checked by never recording a binding as seen in `instance_bindings`.
#[test]
fn an_instance_binding_a_member_twice_is_an_error() {
    let source =
        with_eq_instance("  eq x y =\n    True\n  neq x y =\n    False\n  eq a b =\n    False\n");
    let errors = class_errors(&source);
    let [error @ canonical::Error::InstanceMemberBoundTwice(name, _, _)] = errors.as_slice() else {
        panic!("expected one InstanceMemberBoundTwice, got {:?}", errors);
    };
    assert_eq!(name.as_str(), "eq");
    let first = source.find("eq x y").expect("the first binding");
    let second = source.find("eq a b").expect("the second binding");
    let starts: Vec<_> = label_ranges(error)
        .into_iter()
        .map(|(primary, range)| (primary, range.start))
        .collect();
    assert_eq!(starts, vec![(true, second), (false, first)]);
}

/// A class and a type of one name are an error, under the class with the type labelled.
///
/// Mutation-checked by dropping the type lookup in `declare_classes`.
#[test]
fn a_class_and_a_type_of_one_name_are_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        type Eq
          = Eq

        class Eq a where
          eq : a -> a -> Bool
    "#};
    let errors = class_errors(source);
    let [error @ canonical::Error::ClassNameTaken(name, canonical::NameTakenBy::Type, _, _)] =
        errors.as_slice()
    else {
        panic!("expected one ClassNameTaken, got {:?}", errors);
    };
    assert_eq!(name.as_str(), "Eq");
    assert_eq!(
        label_ranges(error),
        vec![
            (true, range_of(source, "class Eq a where")),
            (false, range_of(source, "type Eq\n  = Eq")),
        ]
    );
}

/// Two classes of one name are an error, under the second with the first labelled.
///
/// Mutation-checked by never recording a class in `declare_classes`' `first`.
#[test]
fn two_classes_of_one_name_are_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Eq a where
          eq : a -> a -> Bool

        class Eq a where
          same : a -> a -> Bool
    "#};
    let errors = class_errors(source);
    let [error @ canonical::Error::ClassNameTaken(name, canonical::NameTakenBy::Class, _, _)] =
        errors.as_slice()
    else {
        panic!("expected one ClassNameTaken, got {:?}", errors);
    };
    assert_eq!(name.as_str(), "Eq");
    assert_eq!(
        label_ranges(error),
        vec![
            (true, nth_range(source, "class Eq a where", 1)),
            (false, nth_range(source, "class Eq a where", 0)),
        ]
    );
}

/// A class head that is not a name applied to one variable is an error under the head.
///
/// Mutation-checked by dropping the `InvalidClassHead` push in `declare_classes`: the
/// class is then dropped with no error at all.
#[test]
fn a_class_head_over_two_variables_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Same a b where
          same : a -> b -> Bool
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::InvalidClassHead(_)] => {
            assert_eq!(sole_label(error), range_of(source, "Same a b"));
        }
        other => panic!("expected one InvalidClassHead, got {:?}", other),
    }
}

/// A constraint on something other than one type variable is an error under the
/// constraint.
///
/// Mutation-checked by dropping the `ConstraintNotOnVariable` push in `constraints`.
#[test]
fn a_constraint_on_a_concrete_type_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Eq a where
          eq : a -> a -> Bool

        class Eq Int => Same a where
          same : a -> a -> Bool
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::ConstraintNotOnVariable(_)] => {
            assert_eq!(sole_label(error), range_of(source, "Eq Int"));
        }
        other => panic!("expected one ConstraintNotOnVariable, got {:?}", other),
    }
}

/// Two members of one name are an error, under the second with the first labelled.
///
/// Mutation-checked by never recording a member in `class_signature`'s `first`.
#[test]
fn a_member_declared_twice_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Eq a where
          eq : a -> a -> Bool
          eq : a -> Bool
    "#};
    let errors = class_errors(source);
    let [error @ canonical::Error::MemberDeclaredTwice(name, _, _)] = errors.as_slice() else {
        panic!("expected one MemberDeclaredTwice, got {:?}", errors);
    };
    assert_eq!(name.as_str(), "eq");
    assert_eq!(
        label_ranges(error),
        vec![
            (true, range_of(source, "eq : a -> Bool")),
            (false, range_of(source, "eq : a -> a -> Bool")),
        ]
    );
}

/// `Class(..)` in an import list is an error under the entry: a class has no
/// constructors to bring in.
///
/// Mutation-checked by dropping the class lookup in `process_import`'s `Public` arm: the
/// entry is then `UnionNotFound`.
#[test]
fn an_import_of_a_class_with_constructors_is_an_error() {
    use zelkova_compiler::PhaseError;

    let mut interfaces = scalar_interfaces();
    publish(EQ, &mut interfaces);

    let source = indoc::indoc! {r#"
        module K exposing ()

        import E exposing (Eq(..))
    "#};
    let errors =
        canonicalize_with_interfaces(source, &interfaces).expect_err("the import is rejected");
    let [error @ canonical::Error::EnvironmentErrors(..)] = errors.as_slice() else {
        panic!("expected one EnvironmentErrors, got {:?}", errors);
    };
    assert_eq!(
        error.message(),
        "`Eq` is a class, and a class has no constructors to import"
    );
    assert_eq!(sole_label(error), range_of(source, "Eq(..)"));
}

/// A superclass that names no class is an error under the constraint.
///
/// Mutation-checked by dropping the `ClassNotFound` push in `constraints`.
#[test]
fn a_superclass_naming_no_class_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Missing a => Same a where
          same : a -> a -> Bool
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::ClassNotFound(name, _)] => {
            assert_eq!(name.as_str(), "Missing");
            assert_eq!(sole_label(error), range_of(source, "Missing a"));
        }
        other => panic!("expected one ClassNotFound, got {:?}", other),
    }
}

/// An instance whose class names no class is an error under the head.
///
/// Mutation-checked by dropping the `ClassNotFound` push in `instance_head`: the
/// instance is then dropped with no error at all.
#[test]
fn an_instance_of_no_class_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        type Colour
          = Red

        instance Missing Colour where
          same x y =
            True
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::ClassNotFound(name, _)] => {
            assert_eq!(name.as_str(), "Missing");
            assert_eq!(sole_label(error), range_of(source, "Missing Colour"));
        }
        other => panic!("expected one ClassNotFound, got {:?}", other),
    }
}

/// `Comparable`, declared in a module of its own name, beside the `Order` its member
/// returns.
const COMPARABLE: &str = indoc::indoc! {r#"
    module Comparable exposing (Comparable, Order(..))

    type Order
      = LT
      | EQ
      | GT

    class Comparable a where
      compare : a -> a -> Order
"#};

/// `Colour`, declared in a module of its own name.
const COLOUR: &str = indoc::indoc! {r#"
    module Colour exposing (Colour(..))

    type Colour
      = Red
      | Blue
"#};

/// An instance is legal in the module declaring its class, and in the module declaring
/// its type.
///
/// Mutation-checked by comparing the instance's module against the class's only in
/// `do_instances`: the instance in `Colour` is then the orphan error.
#[test]
fn an_instance_beside_its_class_or_its_type_resolves() {
    let mut interfaces = scalar_interfaces();
    publish(COLOUR, &mut interfaces);

    let with_class = indoc::indoc! {r#"
        module Comparable exposing (Comparable, Order(..))

        import Colour exposing (Colour)

        type Order
          = EQ

        class Comparable a where
          compare : a -> a -> Order

        instance Comparable Colour where
          compare a b =
            EQ
    "#};
    let module = canonicalize_with_interfaces(with_class, &interfaces)
        .expect("an instance beside its class resolves");
    assert_eq!(module.instances.len(), 1);

    let mut interfaces = scalar_interfaces();
    publish(COMPARABLE, &mut interfaces);
    let with_type = indoc::indoc! {r#"
        module Colour exposing (Colour(..))

        import Comparable exposing (Comparable, Order(..))

        type Colour
          = Red

        instance Comparable Colour where
          compare a b =
            EQ
    "#};
    let module = canonicalize_with_interfaces(with_type, &interfaces)
        .expect("an instance beside its type resolves");
    assert_eq!(module.instances.len(), 1);
}

/// An instance in a third module is the orphan error, which names both modules the
/// instance may go in.
///
/// Mutation-checked by accepting every instance in `do_instances`'s orphan check.
#[test]
fn an_instance_in_a_third_module_is_an_orphan() {
    use zelkova_compiler::PhaseError;

    let mut interfaces = scalar_interfaces();
    publish(COMPARABLE, &mut interfaces);
    publish(COLOUR, &mut interfaces);

    let source = indoc::indoc! {r#"
        module App exposing ()

        import Colour exposing (Colour)
        import Comparable exposing (Comparable, Order(..))

        instance Comparable Colour where
          compare a b =
            EQ
    "#};
    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("an orphan instance is rejected");
    let [error @ canonical::Error::OrphanInstance(..)] = errors.as_slice() else {
        panic!("expected one OrphanInstance, got {:?}", errors);
    };
    assert_eq!(
        error.message(),
        "`Comparable` is declared in `Comparable` and `Colour` in `Colour`; an instance may go in either"
    );
    let labels = error.labels();
    assert_eq!(labels.len(), 1, "{:?}", labels);
    assert_eq!(
        labels[0].span.to_range(),
        range_of(source, "instance Comparable Colour where")
    );
    assert_eq!(
        labels[0].message,
        "declare this instance in `Comparable` or in `Colour`"
    );
}

/// An instance for a tuple is legal only beside its class.
///
/// Mutation-checked by letting a head no module declares pass `do_instances`' orphan
/// check.
#[test]
fn a_tuple_instance_away_from_its_class_is_an_orphan() {
    use zelkova_compiler::PhaseError;

    let mut interfaces = scalar_interfaces();
    publish(COMPARABLE, &mut interfaces);

    let source = indoc::indoc! {r#"
        module App exposing ()

        import Comparable exposing (Comparable, Order(..))

        instance Comparable (a, b) where
          compare x y =
            EQ
    "#};
    let errors = canonicalize_with_interfaces(source, &interfaces)
        .expect_err("a tuple instance away from its class is rejected");
    let [error @ canonical::Error::OrphanInstance(instance, _)] = errors.as_slice() else {
        panic!("expected one OrphanInstance, got {:?}", errors);
    };
    assert_eq!(instance.head, canonical::HeadName::TwoTuple);
    assert_eq!(
        sole_label(error),
        range_of(source, "instance Comparable (a, b) where")
    );
    assert_eq!(
        error.labels()[0].message,
        "declare this instance in `Comparable`"
    );
}

/// Two instances of one class for one type in one module: the second is the error, and
/// the first is labelled.
///
/// Mutation-checked by never recording an instance in `do_instances`'s `declared`.
#[test]
fn two_instances_of_one_class_for_one_type_are_a_duplicate() {
    let source = with_eq_instance("  eq x y =\n    True\n  neq x y =\n    False\n")
        + "\ninstance Eq Colour where\n  eq x y =\n    False\n  neq x y =\n    True\n";
    let errors = class_errors(&source);
    let [error @ canonical::Error::DuplicateInstance(..)] = errors.as_slice() else {
        panic!("expected one DuplicateInstance, got {:?}", errors);
    };
    assert_eq!(
        label_ranges(error),
        vec![
            (true, nth_range(&source, "instance Eq Colour where", 1)),
            (false, nth_range(&source, "instance Eq Colour where", 0)),
        ]
    );
}

/// An instance reaches every module that can reach its declaring module through
/// imports, whatever any `exposing` list says: `A` exposes nothing, `B` imports `A` and
/// exposes nothing, and `C`, importing only `B`, has `A`'s instance in scope and
/// publishes it in turn. Reached by two routes, it is in scope once.
///
/// Mutation-checked by publishing only the module's own instances in `to_interface`
/// (`C` then has nothing in scope), and by dropping the check in
/// `insert_imported_instance` (`D` then has it twice).
#[test]
fn an_instance_reaches_a_module_through_its_imports_transitively() {
    let mut interfaces = scalar_interfaces();
    publish(
        indoc::indoc! {r#"
            module A exposing ()

            type Colour
              = Red

            class Eq a where
              eq : a -> a -> Bool

            instance Eq Colour where
              eq x y =
                True
        "#},
        &mut interfaces,
    );
    publish(
        "module B exposing ()\n\nimport A\n\nb : Int\nb =\n  1\n",
        &mut interfaces,
    );

    let c = canonicalize_with_interfaces(
        "module C exposing ()\n\nimport B\n\nc : Int\nc =\n  1\n",
        &interfaces,
    )
    .expect("C canonicalizes");

    let a_instance = |instances: &[canonical::PublishedInstance]| {
        instances
            .iter()
            .filter(|published| {
                published.signature.class == test_qual("A.Eq")
                    && published.signature.head
                        == canonical::InstanceHead::Type(test_qual("A.Colour"), vec![])
                    && published.signature.module.name().as_str() == "A"
            })
            .count()
    };
    assert_eq!(a_instance(&c.imported_instances), 1, "in C's scope");
    assert_eq!(
        a_instance(&c.to_interface(None).instances),
        1,
        "in C's interface"
    );

    let d = canonicalize_with_interfaces(
        "module D exposing ()\n\nimport A\nimport B\n\nd : Int\nd =\n  1\n",
        &interfaces,
    )
    .expect("D canonicalizes");
    assert_eq!(d.imported_instances.len(), 1, "{:?}", d.imported_instances);
}

/// An instance of a class with a superclass needs an instance of the superclass for the
/// same type.
///
/// Mutation-checked by dropping the superclass loop in `do_instances`.
#[test]
fn an_instance_without_its_superclass_instance_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        type Order
          = EQ

        type Colour
          = Red

        class Eq a where
          eq : a -> a -> Bool

        class Eq a => Comparable a where
          compare : a -> a -> Order

        instance Comparable Colour where
          compare x y =
            EQ
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::MissingSuperclassInstance(instance, superclass, _)] => {
            assert_eq!(instance.class, test_qual("Test.Comparable"));
            assert_eq!(*superclass, test_qual("Test.Eq"));
            assert_eq!(
                instance.head,
                canonical::HeadName::Type(test_qual("Test.Colour"))
            );
            assert_eq!(
                sole_label(error),
                range_of(source, "instance Comparable Colour where")
            );
        }
        other => panic!("expected one MissingSuperclassInstance, got {:?}", other),
    }
}

/// `Eq`, declared in a module of its own name.
const EQ: &str = indoc::indoc! {r#"
    module E exposing (Eq)

    class Eq a where
      eq : a -> a -> Bool
"#};

/// A module declaring `Comparable` with `Eq` as its superclass, and an instance of it for
/// the `Colour` that `T` declares.
const COMPARABLE_COLOUR: &str = indoc::indoc! {r#"
    module K exposing ()

    import E exposing (Eq)
    import T exposing (Colour)

    class Eq a => Comparable a where
      compare : a -> a -> Bool

    instance Comparable Colour where
      compare x y =
        True
"#};

/// `T`, declaring `Colour` and an `Eq` instance for it whose bindings are `bindings`.
fn colour_with_eq(bindings: &str) -> String {
    format!(
        "{}{}",
        indoc::indoc! {r#"
            module T exposing (Colour)

            import E exposing (Eq)

            type Colour
              = Red

            instance Eq Colour where
        "#},
        bindings
    )
}

/// A superclass instance declared in an imported module satisfies an instance of the
/// subclass: `T` declares `instance Eq Colour`, and `K`'s `instance Comparable Colour`
/// is accepted on the strength of it.
///
/// Mutation-checked by leaving the imported instances out of `do_instances`' `in_scope`:
/// `K` is then rejected with `MissingSuperclassInstance`.
#[test]
fn an_imported_instance_satisfies_a_superclass() {
    let mut interfaces = scalar_interfaces();
    publish(EQ, &mut interfaces);
    publish(&colour_with_eq("  eq x y =\n    True\n"), &mut interfaces);

    let module = canonicalize_with_interfaces(COMPARABLE_COLOUR, &interfaces)
        .unwrap_or_else(|errors| panic!("expected K to canonicalize, got {:?}", errors));
    assert_eq!(module.instances.len(), 1);
}

/// A superclass instance its module wrote and could not keep is not reported missing in
/// an importer: `T`'s `instance Eq Colour` binds a name `Eq` has no member of, which
/// `T` reports, and `K`'s `instance Comparable Colour` says nothing more about it.
///
/// Mutation-checked by taking `MissingSuperclassInstance` off `without_restated`'s list:
/// `K` is then rejected with it.
#[test]
fn a_superclass_instance_that_failed_in_its_own_module_is_not_reported_missing() {
    let mut interfaces = scalar_interfaces();
    publish(EQ, &mut interfaces);

    let t = colour_with_eq("  eq x y =\n    True\n  extra x =\n    True\n");
    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(&t, &interfaces);
    assert!(
        matches!(
            errors.as_slice(),
            [canonical::Error::InstanceBindingNotMember(..)]
        ),
        "got {:?}",
        errors
    );
    let interface = module.to_interface(None);
    interfaces.insert(interface.module_name.name().clone(), interface);

    let canonical::Canonicalized { errors, .. } =
        canonicalize_recovering_with_interfaces(COMPARABLE_COLOUR, &interfaces);
    assert!(errors.is_empty(), "got {:?}", errors);
}

/// The `VarForeign` a value's body is, when it is one.
fn foreign_body(module: &canonical::Module, name: &str) -> QualName {
    let Some(canonical::Value::TypedValue { body, .. }) = module.values.get(&name.into()) else {
        panic!("`{}` is an annotated value", name);
    };
    match &body.kind {
        canonical::ExpressionKind::Apply(callee, _) => match &callee.kind {
            canonical::ExpressionKind::Apply(callee, _) => match &callee.kind {
                canonical::ExpressionKind::VarForeign(qname, _, _) => qname.clone(),
                other => panic!("expected a foreign value, got {:?}", other),
            },
            other => panic!("expected an application, got {:?}", other),
        },
        other => panic!("expected an application, got {:?}", other),
    }
}

/// A member is callable by its bare name where the import list names its class, by its
/// bare name where the import list names it alone, and qualified with neither.
///
/// Mutation-checked by inserting the class without its members in
/// `insert_foreign_class` (the import naming the class fails), and by dropping the
/// member lookup of `process_import`'s `Lower` arm (the import naming the member fails).
#[test]
fn a_member_is_callable_wherever_its_class_or_it_is_imported() {
    let mut interfaces = scalar_interfaces();
    publish(COMPARABLE, &mut interfaces);
    let compare = test_qual("Comparable.compare");

    for source in [
        "module App exposing ()\n\nimport Comparable exposing (Comparable, Order)\n\nuse : Int -> Order\nuse x =\n  compare x x\n",
        "module App exposing ()\n\nimport Comparable exposing (compare, Order)\n\nuse : Int -> Order\nuse x =\n  compare x x\n",
        "module App exposing ()\n\nimport Comparable exposing (Order)\n\nuse : Int -> Order\nuse x =\n  Comparable.compare x x\n",
    ] {
        let module = canonicalize_with_interfaces(source, &interfaces)
            .unwrap_or_else(|errors| panic!("{} got {:?}", source, errors));
        assert_eq!(foreign_body(&module, "use"), compare, "{}", source);
    }
}

/// A member listed on its own in its module's header is an error under the entry.
///
/// Mutation-checked by dropping the `MemberExposedAlone` check in `do_exports`.
#[test]
fn a_header_listing_a_member_alone_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (same)

        class Same a where
          same : a -> a -> Bool
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::MemberExposedAlone(member, class, _)] => {
            assert_eq!(member.as_str(), "same");
            assert_eq!(class.as_str(), "Same");
            assert_eq!(sole_label(error), nth_range(source, "same", 0));
        }
        other => panic!("expected one MemberExposedAlone, got {:?}", other),
    }
}

/// `Class(..)` in a module's header is an error under the entry.
///
/// Mutation-checked by answering `UnionPublic` for a class in `do_exports`.
#[test]
fn a_header_exposing_a_class_with_constructors_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (Same(..))

        class Same a where
          same : a -> a -> Bool
    "#};
    match class_errors(source).as_slice() {
        [error @ canonical::Error::ClassExposedWithConstructors(name, _)] => {
            assert_eq!(name.as_str(), "Same");
            assert_eq!(sole_label(error), range_of(source, "Same(..)"));
        }
        other => panic!("expected one ClassExposedWithConstructors, got {:?}", other),
    }
}

/// A class a module only imported may not be exposed by it, and the entry is an error
/// under it while the module's own class beside it is exposed.
///
/// Mutation-checked by accepting every class `find_class` answers for in `do_exports`:
/// the module canonicalizes.
#[test]
fn a_header_exposing_an_imported_class_is_an_error() {
    use zelkova_compiler::PhaseError;

    let mut interfaces = scalar_interfaces();
    publish(EQ, &mut interfaces);

    let source = indoc::indoc! {r#"
        module K exposing (Comparable, Eq)

        import E exposing (Eq)

        class Eq a => Comparable a where
          compare : a -> a -> Bool
    "#};
    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &interfaces);
    let [error @ canonical::Error::ExportNotFound(name, canonical::ExportType::Class, _)] =
        errors.as_slice()
    else {
        panic!("expected one ExportNotFound, got {:?}", errors);
    };
    assert_eq!(name.as_str(), "Eq");
    assert_eq!(
        error.message(),
        "`Eq` is exposed by this module but no class of that name is declared in it"
    );
    assert_eq!(
        label_ranges(error),
        vec![(true, nth_range(source, "Eq", 0))]
    );
    let interface = module.to_interface(None);
    assert!(interface.classes.contains_key(&"Comparable".into()));
    assert!(!interface.classes.contains_key(&"Eq".into()));
}

/// An `infix` declaration may name a member of a class its module declares, and a use
/// of the operator in that module names the member.
///
/// Mutation-checked by dropping the member half of `do_infixes`' existence check.
#[test]
fn an_infix_may_name_a_member_of_a_class_of_its_module() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        infix non 4 (===) = same

        class Same a where
          same : a -> a -> Bool

        use : Int -> Bool
        use x =
          x === x
    "#};
    let module = canonicalize_with_scalars(source).expect("the module canonicalizes");
    assert_eq!(module.infixes[&"===".into()].function_name.as_str(), "same");
}

/// A facade holds signatures only, so a class and an instance in one are each an error
/// under its head line.
///
/// Mutation-checked by dropping each of the two `errors.extend` calls for a facade's
/// classes and instances in `canonicalize_recovering`: the matching error goes missing.
#[test]
fn a_facade_declaring_a_class_or_an_instance_is_an_error() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing ()

        class Same a where
          same : a -> a -> Bool

        instance Same Int where
          same x y =
            True
    "#};
    match class_errors(source).as_slice() {
        [class @ canonical::Error::ClassDeclared(_), instance @ canonical::Error::InstanceDeclared(_)] =>
        {
            assert_eq!(sole_label(class), range_of(source, "class Same a where"));
            assert_eq!(
                sole_label(instance),
                range_of(source, "instance Same Int where")
            );
        }
        other => panic!(
            "expected ClassDeclared and InstanceDeclared, got {:?}",
            other
        ),
    }
}

/// A member shares the value namespace of its module: a top-level declaration of its
/// name is an error under the member, with the declaration labelled.
///
/// Mutation-checked by dropping the top-level half of `member_clashes`.
#[test]
fn a_member_and_a_value_of_one_name_are_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        class Same a where
          same : a -> a -> Bool

        same : Int
        same =
          1
    "#};
    let errors = class_errors(source);
    let [error @ canonical::Error::MemberNameTaken(member, class, None, _, _)] = errors.as_slice()
    else {
        panic!("expected one MemberNameTaken, got {:?}", errors);
    };
    assert_eq!(member.as_str(), "same");
    assert_eq!(class.as_str(), "Same");
    assert_eq!(
        label_ranges(error),
        vec![
            (true, range_of(source, "same : a -> a -> Bool")),
            (false, range_of(source, "same : Int\nsame =\n  1")),
        ]
    );
}

// ── LANG-70: an annotation's constraints, resolved and kept ──

/// The module the annotation tests below write one declaration into: `Eq` and
/// `Comparable`, each declared.
fn with_classes(declaration: &str) -> String {
    format!(
        "{}\n{}",
        indoc::indoc! {r#"
            module Test exposing ()

            class Eq a where
              eq : a -> a -> Bool

            class Comparable a where
              lt : a -> a -> Bool
        "#},
        declaration
    )
}

/// A constraint naming a class nothing declares is an error naming it, with the caret
/// under the constraint and not under the annotation.
///
/// Mutation-checked by resolving no annotation's context in `annotation` (handing
/// `classes::constraints` no context): the module then canonicalizes and
/// `class_errors` panics.
#[test]
fn an_annotation_constraint_naming_no_class_is_an_error() {
    let source = with_classes(indoc::indoc! {r#"
        min : Nonsense a => a -> a -> a
        min x y =
          x
    "#});
    match class_errors(&source).as_slice() {
        [error @ canonical::Error::ClassNotFound(name, _)] => {
            assert_eq!(name.as_str(), "Nonsense");
            assert_eq!(sole_label(error), range_of(&source, "Nonsense a"));
        }
        other => panic!("expected one ClassNotFound, got {:?}", other),
    }
}

/// A constraint has exactly one argument, and it is a type variable: a concrete type, an
/// applied type and two arguments are each an error under the constraint.
///
/// Mutation-checked by resolving no annotation's context in `annotation`: each module
/// then canonicalizes and `class_errors` panics.
#[test]
fn an_annotation_constraint_not_on_one_variable_is_an_error() {
    for constraint in ["Comparable Int", "Comparable (Maybe a)", "Comparable a b"] {
        let source = with_classes(&format!("f : {} => a -> a\nf x =\n  x\n", constraint));
        match class_errors(&source).as_slice() {
            [error @ canonical::Error::ConstraintNotOnVariable(_)] => {
                assert_eq!(sole_label(error), range_of(&source, constraint));
            }
            other => panic!(
                "expected one ConstraintNotOnVariable for `{}`, got {:?}",
                constraint, other
            ),
        }
    }
}

/// A constraint on a variable the annotated type does not mention is an error naming the
/// variable, with the caret under it.
///
/// Mutation-checked by accepting every variable in `classes::constraints`' bound check
/// (`bound.contains(&variable) || true`): the module then canonicalizes and
/// `class_errors` panics.
#[test]
fn an_annotation_constraint_on_a_variable_its_type_does_not_mention_is_an_error() {
    use zelkova_compiler::PhaseError;

    let source = with_classes(indoc::indoc! {r#"
        f : Eq b => a -> a
        f x =
          x
    "#});
    match class_errors(&source).as_slice() {
        [error @ canonical::Error::ConstraintVariableNotInType(name, _)] => {
            assert_eq!(name.as_str(), "b");
            assert_eq!(
                error.message(),
                "the type variable `b` is constrained, but the annotated type does not mention it"
            );
            let written = range_of(&source, "b =>").start;
            assert_eq!(sole_label(error), written..written + 1);
        }
        other => panic!("expected one ConstraintVariableNotInType, got {:?}", other),
    }
}

/// Every bad constraint of one context is reported, each at its own span.
///
/// Mutation-checked by resolving no annotation's context in `annotation`: the module
/// then canonicalizes and `class_errors` panics.
#[test]
fn every_bad_constraint_of_an_annotation_is_reported() {
    let source = with_classes(indoc::indoc! {r#"
        f : (Missing a, Eq Int, Eq b) => a -> a
        f x =
          x
    "#});
    let errors = class_errors(&source);
    let [missing @ canonical::Error::ClassNotFound(..), concrete @ canonical::Error::ConstraintNotOnVariable(_), unmentioned @ canonical::Error::ConstraintVariableNotInType(..)] =
        errors.as_slice()
    else {
        panic!("expected three errors, got {:?}", errors);
    };
    assert_eq!(sole_label(missing), range_of(&source, "Missing a"));
    assert_eq!(sole_label(concrete), range_of(&source, "Eq Int"));
    let written = range_of(&source, "b)").start;
    assert_eq!(sole_label(unmentioned), written..written + 1);
}

/// The byte range `constraint` was written at.
fn constraint_range(constraint: &canonical::Constraint) -> std::ops::Range<usize> {
    constraint
        .span
        .span()
        .expect("a constraint read from source has a span")
        .to_range()
}

/// A context naming declared classes resolves, and is on the canonical value beside the
/// type: each constraint's class, its variable and where it was written, in order. A
/// constraint repeated, and one a superclass already implies, are legal.
///
/// Mutation-checked by building every `Value::TypedValue`'s `context` as empty in
/// `value_declaration`: the destructuring panics.
#[test]
fn an_annotation_context_is_on_the_canonical_value() {
    let source = with_classes(indoc::indoc! {r#"
        lookup : (Comparable k, Eq v, Eq v) => k -> v -> Bool
        lookup key value =
          True
    "#});
    let module = canonicalize_with_scalars(&source).expect("the module canonicalizes");

    let Some(canonical::Value::TypedValue { context, .. }) = module.values.get(&"lookup".into())
    else {
        panic!("expected a TypedValue for `lookup`");
    };
    let [comparable, eq, again] = context.as_slice() else {
        panic!("expected three constraints, got {:?}", context);
    };
    assert_eq!(comparable.class, test_qual("Test.Comparable"));
    assert_eq!(comparable.variable.as_str(), "k");
    assert_eq!(
        constraint_range(comparable),
        range_of(&source, "Comparable k")
    );
    assert_eq!(eq.class, test_qual("Test.Eq"));
    assert_eq!(eq.variable.as_str(), "v");
    assert_eq!(constraint_range(eq), nth_range(&source, "Eq v", 0));
    assert_eq!(constraint_range(again), nth_range(&source, "Eq v", 1));
}

/// A declaration whose body fails and whose annotation does not is broken with its
/// context, and the interface publishes that context with its type.
///
/// Mutation-checked by building every `Broken`'s `context` as empty in `Broken::of`:
/// the destructuring panics.
#[test]
fn a_broken_declaration_keeps_its_annotation_context() {
    let source = indoc::indoc! {r#"
        module Test exposing (f)

        class Eq a where
          eq : a -> a -> Bool

        f : Eq a => a -> a
        f x y =
          x
    "#};
    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(source, &scalar_interfaces());
    assert!(
        matches!(
            errors.as_slice(),
            [canonical::Error::BindingPatternsInvalidLen(_)]
        ),
        "got {:?}",
        errors
    );

    let [broken] = module.broken.as_slice() else {
        panic!("expected one broken declaration, got {:?}", module.broken);
    };
    let [eq] = broken.context.as_slice() else {
        panic!("expected one constraint, got {:?}", broken.context);
    };
    assert_eq!(eq.class, test_qual("Test.Eq"));

    let interface = module.to_interface(None);
    let [published] = interface.values[&"f".into()].context.as_slice() else {
        panic!("expected `f` published with one constraint");
    };
    assert_eq!(published.class, test_qual("Test.Eq"));
}

/// A constraint may name a variable the annotated type mentions only inside an applied
/// type, or only inside a tuple: either is a variable of the type, and the context lands
/// on the canonical value.
///
/// Mutation-checked by emptying the `Unqualified` arm, then the `Tuple` arm, of
/// `classes::written_variables`: `same`, then `first`, is reported with
/// `ConstraintVariableNotInType` and `canonicalize_with_scalars` returns the error.
#[test]
fn a_constraint_on_a_variable_inside_an_applied_type_or_a_tuple_resolves() {
    let source = with_classes(indoc::indoc! {r#"
        type Box a = Box a

        same : Eq a => Box a -> Box a -> Bool
        same x y =
          True

        first : Comparable b => (b, Int) -> Bool
        first pair =
          True
    "#});
    let module = canonicalize_with_scalars(&source).expect("the module canonicalizes");

    for (name, class, variable) in [("same", "Test.Eq", "a"), ("first", "Test.Comparable", "b")] {
        let Some(canonical::Value::TypedValue { context, .. }) = module.values.get(&name.into())
        else {
            panic!("expected a TypedValue for `{}`", name);
        };
        let [constraint] = context.as_slice() else {
            panic!("expected one constraint on `{}`, got {:?}", name, context);
        };
        assert_eq!(constraint.class, test_qual(class));
        assert_eq!(constraint.variable.as_str(), variable);
    }
}

/// A declaration whose context fails is broken with no annotation at all: it is not a
/// value of the module, its broken entry carries neither type nor context, and the
/// interface does not publish it. Keeping the type with the bad constraints dropped would
/// publish a constrained function as an unconstrained one.
///
/// Mutation-checked by having `value_declaration` push a failed annotation's errors onto
/// `unresolved` and carry on with `Annotation::Canonical(tpe, Vec::new())` whenever the
/// type itself canonicalizes: the `ClassNotFound` is still reported, `f` is a value of
/// the module, and the `broken` destructuring panics.
#[test]
fn a_declaration_whose_context_fails_is_broken_without_its_annotation() {
    let source = with_classes(indoc::indoc! {r#"
        f : Nonsense a => a -> a
        f x =
          x
    "#});
    let canonical::Canonicalized { module, errors } =
        canonicalize_recovering_with_interfaces(&source, &scalar_interfaces());
    assert!(
        matches!(errors.as_slice(), [canonical::Error::ClassNotFound(..)]),
        "got {:?}",
        errors
    );

    let [broken] = module.broken.as_slice() else {
        panic!("expected one broken declaration, got {:?}", module.broken);
    };
    assert_eq!(broken.name.as_str(), "f");
    assert_eq!(broken.tpe, None);
    assert!(broken.context.is_empty(), "got {:?}", broken.context);
    assert!(!module.values.contains_key(&"f".into()));
    assert!(module.incomplete);

    let interface = module.to_interface(None);
    assert!(!interface.values.contains_key(&"f".into()));
}

/// `Eq` and a function constrained by it, declared in a module of its own name.
const CLASSES: &str = indoc::indoc! {r#"
    module Classes exposing (Eq, same)

    class Eq a where
      eq : a -> a -> Bool

    same : Eq a => a -> a -> Bool
    same x y =
      eq x y
"#};

/// A class imported from another module resolves through the import, to the class that
/// module declared, and the interface the importer is canonicalized against carries the
/// exporting module's constrained function with its context.
///
/// Mutation-checked by publishing every value with an empty `context` in
/// `Module::declared_signature`, which turns the interface assertion red; and by
/// resolving no annotation's context in `annotation`, which turns the importer's
/// assertion red.
#[test]
fn an_imported_class_resolves_and_its_constrained_function_carries_its_context() {
    let mut interfaces = scalar_interfaces();
    publish(CLASSES, &mut interfaces);

    let [published] = interfaces[&"Classes".into()].values[&"same".into()]
        .context
        .as_slice()
    else {
        panic!("expected `same` published with one constraint");
    };
    assert_eq!(published.class, test_qual("Classes.Eq"));
    assert_eq!(published.variable.as_str(), "a");

    let importer = indoc::indoc! {r#"
        module Main exposing (alike)

        import Classes exposing (Eq, same)

        alike : Eq b => b -> Bool
        alike x =
          same x x
    "#};
    let module =
        canonicalize_with_interfaces(importer, &interfaces).expect("the importer canonicalizes");
    let Some(canonical::Value::TypedValue { context, .. }) = module.values.get(&"alike".into())
    else {
        panic!("expected a TypedValue for `alike`");
    };
    let [eq] = context.as_slice() else {
        panic!("expected one constraint, got {:?}", context);
    };
    assert_eq!(eq.class, test_qual("Classes.Eq"));
    assert_eq!(eq.variable.as_str(), "b");
}

/// A constrained function behind an exposed operator, itself not exposed by name, carries
/// its context in `Interface::infix_functions`.
///
/// Mutation-checked by publishing every value with an empty `context` in
/// `Module::declared_signature`: the destructuring panics.
#[test]
fn a_constrained_function_behind_an_exposed_operator_carries_its_context() {
    let source = indoc::indoc! {r#"
        module Test exposing (Eq, (===))

        infix non 4 (===) = same

        class Eq a where
          eq : a -> a -> Bool

        same : Eq a => a -> a -> Bool
        same x y =
          eq x y
    "#};
    let module = canonicalize_with_scalars(source).expect("the module canonicalizes");
    let interface = module.to_interface(None);

    assert!(!interface.values.contains_key(&"same".into()));
    let [eq] = interface.infix_functions[&"same".into()].context.as_slice() else {
        panic!("expected `same` behind `===` with one constraint");
    };
    assert_eq!(eq.class, test_qual("Test.Eq"));
    assert_eq!(eq.variable.as_str(), "a");
}

// ── LANG-83: a derivation is checked, and a `derived` instance has its members ──

/// A module header and the types the derivation tests below derive for.
const DERIVATION_TYPES: &str = indoc::indoc! {r#"
    module Test exposing ()

    type Colour
      = Red
      | Green

    type Box a
      = Box a

"#};

/// `DERIVATION_TYPES`, then `declarations`.
fn with_derivation(declarations: &str) -> String {
    format!("{}{}", DERIVATION_TYPES, declarations)
}

/// `Eq`, derived the way the chapter derives it.
const EQ_DERIVED: &str = indoc::indoc! {r#"
    class Eq a where
      eq : a -> a -> Bool

      derived eq
        matched = True
        differed _ _ = False
        combine x y =
          case x of
            True ->
              y

            False ->
              False

"#};

/// The one class error `source` is rejected with and the ranges of its labels, primary
/// first. Anything but one error is a panic, so a test cannot pass on a second error that
/// nothing looks at.
fn one_class_error(source: &str) -> (canonical::Error, Vec<(bool, std::ops::Range<usize>)>) {
    let mut errors = class_errors(source);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);
    let error = errors.remove(0);
    let labels = label_ranges(&error);
    (error, labels)
}

/// A derivation for a name the class does not declare is an error at the derivation.
///
/// Mutation-checked by dropping the `DerivationForNonMember` push in `class_derivations`, so
/// that the derivation is skipped without a word: the module canonicalizes and `class_errors`
/// panics.
#[test]
fn a_derivation_for_a_name_that_is_not_a_member_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool

          derived nope
            matched = True
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationForNonMember(member, class, _) = &error else {
        panic!("expected DerivationForNonMember, got {:?}", error);
    };
    assert_eq!(member.as_str(), "nope");
    assert_eq!(class.as_str(), "Eq");
    assert_eq!(labels, vec![(true, range_of(&source, "derived nope"))]);
}

/// A member whose signature returns the class variable cannot carry a derivation: the walk
/// would be asked for a third value of the type it is walking.
///
/// Mutation-checked by dropping `!mentions(result, variable)` from the walk over two values
/// in `walk_of`: `add` is then derived, the module canonicalizes and `class_errors` panics.
#[test]
fn a_derivation_on_a_member_returning_the_class_variable_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Number a where
          add : a -> a -> a

          derived add
            matched = 0
            differed _ _ = 0
            combine x y =
              x
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationSignature(member, problem, _, _) = &error else {
        panic!("expected DerivationSignature, got {:?}", error);
    };
    assert_eq!(member.as_str(), "add");
    assert_eq!(
        *problem,
        canonical::DerivationSignatureProblem::ResultMentionsClassVariable
    );
    // The derivation is where the error is, and the signature is shown beside it.
    assert_eq!(
        labels,
        vec![
            (true, range_of(&source, "derived add")),
            (false, range_of(&source, "add : a -> a -> a")),
        ]
    );
}

/// A member taking no value of the class's type cannot carry one either: it would have to
/// be built from a description of the type's constructors.
///
/// Mutation-checked by answering `Shape` where `walk_of` answers `NoClassValue`: the problem
/// assertion goes red.
#[test]
fn a_derivation_on_a_member_taking_no_class_value_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Bounded a where
          bottom : a

          derived bottom
            matched = True
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationSignature(member, problem, _, _) = &error else {
        panic!("expected DerivationSignature, got {:?}", error);
    };
    assert_eq!(member.as_str(), "bottom");
    assert_eq!(
        *problem,
        canonical::DerivationSignatureProblem::NoClassValue
    );
    assert_eq!(labels[0], (true, range_of(&source, "derived bottom")));
}

/// Any other signature is an error too: here the class value is not the first parameter.
///
/// Mutation-checked by letting any first parameter through `walk_of`'s test for the class
/// variable (`|| true`): `sized` is then derived as a walk of two values and the module
/// canonicalizes; and by swapping the derivation and the signature spans in
/// `DerivationSignature`'s `labels()` arm: the range assertion goes red.
#[test]
fn a_derivation_on_a_member_whose_first_parameter_is_not_the_class_value_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Sized a where
          sized : Int -> a -> Bool

          derived sized
            atConstructor _ = True
            combine x y =
              x
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationSignature(_, problem, _, _) = &error else {
        panic!("expected DerivationSignature, got {:?}", error);
    };
    assert_eq!(*problem, canonical::DerivationSignatureProblem::Shape);
    assert_eq!(
        labels,
        vec![
            (true, range_of(&source, "derived sized")),
            (false, range_of(&source, "sized : Int -> a -> Bool")),
        ]
    );
}

/// A derivation without `combine` is an error naming it, at the derivation.
///
/// Mutation-checked by dropping the `DerivationBindingMissing` push in `class_derivations`:
/// the module canonicalizes and `class_errors` panics.
#[test]
fn a_derivation_without_combine_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationBindingMissing(binding, member, _) = &error else {
        panic!("expected DerivationBindingMissing, got {:?}", error);
    };
    assert_eq!(binding.as_str(), "combine");
    assert_eq!(member.as_str(), "eq");
    assert_eq!(labels, vec![(true, range_of(&source, "derived eq"))]);
}

/// A binding the member's signature does not call for is an error under that binding, and
/// names what the derivation does take. `atConstructor` is not one of the two-value
/// derivation's.
///
/// Mutation-checked by dropping the `DerivationBindingUnexpected` push in
/// `class_derivations`, so that the binding is ignored: the module canonicalizes and
/// `class_errors` panics.
#[test]
fn an_extra_binding_in_a_derivation_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
            combine x y =
              y

            atConstructor p =
              True
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationBindingUnexpected(binding, member, takes, _) = &error else {
        panic!("expected DerivationBindingUnexpected, got {:?}", error);
    };
    assert_eq!(binding.as_str(), "atConstructor");
    assert_eq!(member.as_str(), "eq");
    let takes: Vec<&str> = takes.iter().map(|name| name.as_str()).collect();
    assert_eq!(takes, vec!["matched", "differed", "combine"]);
    assert_eq!(labels.len(), 1);
    assert!(labels[0].0);
    let binding_text = "atConstructor p =\n      True";
    assert_eq!(labels[0].1, range_of(&source, binding_text));
}

/// A binding written twice is an error at the second, and the first is shown beside it.
///
/// Mutation-checked by never finding an earlier binding in `class_derivations`'s `written`
/// lookup: the second `matched` replaces the first and the module canonicalizes; and by
/// swapping the two spans in `DerivationBindingRepeated`'s `labels()` arm: the range
/// assertion goes red.
#[test]
fn a_repeated_binding_in_a_derivation_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
            combine x y =
              y

            matched = False
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationBindingRepeated(binding, member, _, _) = &error else {
        panic!("expected DerivationBindingRepeated, got {:?}", error);
    };
    assert_eq!(binding.as_str(), "matched");
    assert_eq!(member.as_str(), "eq");
    // The repeat is where the error is, and the first is shown beside it.
    assert_eq!(
        labels,
        vec![
            (true, range_of(&source, "matched = False")),
            (false, range_of(&source, "matched = True")),
        ]
    );
}

/// A binding with more parameters than the walk supplies it cannot be placed, so it is an
/// error at the binding.
///
/// Mutation-checked by never finding a binding with too many parameters in
/// `class_derivations` (`false &&` in front of the count): the module canonicalizes.
#[test]
fn a_derivation_binding_with_too_many_parameters_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
            combine x y z =
              y
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationBindingTakesTooMany(binding, member, most, _) = &error else {
        panic!("expected DerivationBindingTakesTooMany, got {:?}", error);
    };
    assert_eq!(binding.as_str(), "combine");
    assert_eq!(member.as_str(), "eq");
    assert_eq!(*most, 2);
    assert_eq!(labels.len(), 1);
    assert_eq!(labels[0].1, range_of(&source, "combine x y z =\n      y"));
}

/// A member has at most one derivation: the second is an error, with the first beside it.
///
/// Mutation-checked by never finding an earlier derivation in `class_derivations`'s `first`
/// lookup: the second is read as the first would be and the module canonicalizes; and by
/// swapping `span` and `earlier` in `DerivationRepeated`'s `labels()` arm: the primary label
/// is then on the first derivation and the range assertion goes red.
#[test]
fn a_member_with_two_derivations_is_an_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
            combine x y =
              y

          derived eq
            matched = False
            differed _ _ = False
            combine x y =
              y
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationRepeated(member, _, _) = &error else {
        panic!("expected DerivationRepeated, got {:?}", error);
    };
    assert_eq!(member.as_str(), "eq");
    // The second derivation is where the error is, and the first is shown beside it.
    assert_eq!(
        labels,
        vec![
            (true, nth_range(&source, "derived eq", 1)),
            (false, nth_range(&source, "derived eq", 0)),
        ]
    );
}

/// A class with two members and a derivation for one is an error naming the other, at the
/// class's head.
///
/// Mutation-checked by switching the coverage check in `class_derivations` off (`false &&`):
/// the module canonicalizes and `class_errors` panics.
#[test]
fn a_class_with_one_of_two_members_derived_is_an_error_naming_the_other() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool
          neq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
            combine x y =
              y
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivationIncomplete(class, left_out, _) = &error else {
        panic!("expected DerivationIncomplete, got {:?}", error);
    };
    assert_eq!(class.as_str(), "Eq");
    let left_out: Vec<&str> = left_out.iter().map(|name| name.as_str()).collect();
    assert_eq!(left_out, vec!["neq"]);
    assert_eq!(labels, vec![(true, range_of(&source, "class Eq a where"))]);
}

/// A class whose every member carries a derivation is derivable, and says so on its
/// signature and in its interface entry, with the bindings: a module that imports the class
/// reads them from there. A class with none is not derivable, and says that.
///
/// Mutation-checked by recording no derivation on the signature in `canonicalize_recovering`
/// (`signature.derivations = Vec::new()`): the `derivable()` assertion on `Eq` goes red; and by
/// publishing a class's signature without its derivations in `to_interface`: the interface
/// assertions go red.
#[test]
fn a_class_deriving_every_member_is_derivable_and_publishes_its_bindings() {
    let source = format!(
        "{}{}{}",
        "module Test exposing (Eq, Plain)\n\n",
        EQ_DERIVED,
        indoc::indoc! {r#"
            class Plain a where
              plain : a -> Bool
        "#}
    );
    let module = canonicalize_with_scalars(&source).expect("the module canonicalizes");

    let eq = &module.classes[&"Eq".into()].signature;
    assert!(eq.derivable());
    let derivation = eq.derivation(&"eq".into()).expect("a derivation of `eq`");
    assert_eq!(derivation.result, bool_t());
    assert_eq!(
        derivation.span.span().map(|span| span.to_range()),
        Some(range_of(&source, "derived eq"))
    );
    let canonical::DerivationBindings::Pair { .. } = &derivation.bindings else {
        panic!(
            "a derivation over two values, got {:?}",
            derivation.bindings
        );
    };

    let plain = &module.classes[&"Plain".into()].signature;
    assert!(!plain.derivable());
    assert!(plain.derivations.is_empty());

    let interface = module.to_interface(None);
    assert!(interface.classes[&"Eq".into()].derivable());
    assert_eq!(interface.classes[&"Eq".into()].derivations.len(), 1);
    assert!(!interface.classes[&"Plain".into()].derivable());
}

/// A member at `a -> R` carries a derivation over one value, with `atConstructor`.
///
/// Mutation-checked by reading a member at `a -> R` as a walk over two values in `walk_of`
/// (`Walk::Pair` for it): the derivation then wants `matched` and `differed`, the module is
/// rejected and `canonicalizes` panics.
#[test]
fn a_member_at_a_to_r_carries_a_derivation_over_one_value() {
    let source = with_derivation(indoc::indoc! {r#"
        class Hashable a where
          hash : a -> Int

          derived hash
            atConstructor _ = 1
            combine x y =
              x
    "#});
    let module = canonicalize_with_scalars(&source).expect("the module canonicalizes");

    let hashable = &module.classes[&"Hashable".into()].signature;
    assert!(hashable.derivable());
    assert!(hashable.walks_one_value());
    let derivation = hashable
        .derivation(&"hash".into())
        .expect("a derivation of `hash`");
    assert_eq!(derivation.result, int_t());
    assert!(matches!(
        derivation.bindings,
        canonical::DerivationBindings::Single { .. }
    ));
}

/// `derived` under a class whose declaration carries no derivation is an error naming the
/// class, at the word.
///
/// Mutation-checked by never rejecting a class in `plan` (`false &&` in front of the
/// `derivable()` test): the instance is kept, the module canonicalizes and `class_errors`
/// panics.
#[test]
fn a_derived_instance_of_a_class_with_no_derivation_is_an_error_naming_the_class() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool

        instance Eq Colour where
          derived
    "#});
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivedInstanceNotDerivable(class, _) = &error else {
        panic!("expected DerivedInstanceNotDerivable, got {:?}", error);
    };
    assert_eq!(class.as_str(), "Eq");
    assert_eq!(labels, vec![(true, range_of(&source, "derived"))]);
}

/// `derived` for a scalar type is an error: a scalar has no shape for a walk to read.
///
/// Mutation-checked by switching the `scalar_of` check in `plan` off: `Int` has no union in
/// scope, the instance is dropped without an error, and `class_errors` panics.
#[test]
fn a_derived_instance_for_a_scalar_is_an_error() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            instance Eq Int where
              derived
        "#}
    ));
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivedInstanceNoShape(problem, _) = &error else {
        panic!("expected DerivedInstanceNoShape, got {:?}", error);
    };
    assert_eq!(
        *problem,
        canonical::DerivedShapeProblem::Scalar("Int".into())
    );
    // The word of the instance, the second `derived` in the source.
    assert_eq!(labels, vec![(true, nth_range(&source, "derived", 1))]);
}

/// What `source` canonicalizes to, with `Int`'s instance written beside it: the tests of what
/// a derived instance needs read the instances' contexts off the module.
fn derived_module(source: &str) -> canonical::Module {
    canonicalize_with_scalars(source)
        .unwrap_or_else(|errors| panic!("expected the module to canonicalize, got {:?}", errors))
}

/// What an instance needs of its head's variables, as the class and the variable of each
/// constraint, in order.
fn context_of(instance: &canonical::Instance) -> Vec<(String, String)> {
    instance
        .signature
        .context
        .iter()
        .map(|constraint| {
            (
                constraint.class.unqualified_name().to_string(),
                constraint.variable.to_string(),
            )
        })
        .collect()
}

/// The instance of the module whose head is the type named `head`.
fn instance_for<'a>(module: &'a canonical::Module, head: &str) -> &'a canonical::Instance {
    module
        .instances
        .iter()
        .find(|instance| match &instance.signature.head {
            canonical::InstanceHead::Type(name, _) => name.unqualified_name().as_str() == head,
            _ => false,
        })
        .unwrap_or_else(|| panic!("an instance for `{}`", head))
}

fn pairs(constraints: &[(&str, &str)]) -> Vec<(String, String)> {
    constraints
        .iter()
        .map(|(class, variable)| (class.to_string(), variable.to_string()))
        .collect()
}

/// `Int` has an `Eq` instance, written out, which a variant holding one needs.
const EQ_INT: &str = indoc::indoc! {r#"
    instance Eq Int where
      eq a b =
        True

"#};

/// A derived instance for a type its module exposes without its constructors is an error: the
/// walk is read off the constructors, and there are none to read. Exposed with them, the same
/// instance is fine.
///
/// Mutation-checked by switching the check for a union with no variants in `plan` off: the
/// opaque import is derived, with no alternative to walk, and `expect_err` panics.
#[test]
fn a_derived_instance_for_a_type_imported_opaquely_is_an_error() {
    let mut interfaces = scalar_interfaces();
    publish(
        "module Colour exposing (Colour)\n\ntype Colour\n  = Red\n",
        &mut interfaces,
    );
    let source = format!(
        "module Test exposing (Eq)\n\nimport Colour exposing (Colour)\n\n{}instance Eq Colour where\n  derived\n",
        EQ_DERIVED
    );
    let errors = canonicalize_with_interfaces(&source, &interfaces)
        .expect_err("expected the module to be rejected");

    let [error @ canonical::Error::DerivedInstanceNoShape(problem, _)] = errors.as_slice() else {
        panic!("expected one DerivedInstanceNoShape, got {:?}", errors);
    };
    assert_eq!(
        *problem,
        canonical::DerivedShapeProblem::NoConstructors("Colour".into())
    );
    assert_eq!(
        label_ranges(error),
        vec![(true, nth_range(&source, "derived", 1))]
    );

    // With its constructors exposed the same instance has a shape.
    let mut interfaces = scalar_interfaces();
    publish(
        "module Colour exposing (Colour(..))\n\ntype Colour\n  = Red\n",
        &mut interfaces,
    );
    let module = canonicalize_with_interfaces(&source, &interfaces)
        .expect("a type exposed with its constructors is derived");
    assert_eq!(module.instances.len(), 1);
}

/// A derived instance for a type imported by name alone, `exposing (Colour)`, or not at all and
/// named qualified, is accepted when the declaring module exposes `Colour(..)`: "constructors in
/// scope" is read as what the declaring module exposes and not as what the import list names
/// ([*What a derived instance requires*](../../../docs/spec/type-classes.md#what-a-derived-instance-requires)).
///
/// Mutation-checked by treating every type of another module as having no constructors in
/// `plan` (`|| declaring_module(name) != *env.module_name()` added to the test for a union with
/// no variants): both instances are rejected with `DerivedInstanceNoShape` and the first
/// assertion goes red.
#[test]
fn a_derived_instance_for_a_type_imported_by_name_is_accepted_when_its_module_exposes_the_constructors(
) {
    let mut interfaces = scalar_interfaces();
    publish(
        "module Colour exposing (Colour(..))\n\ntype Colour\n  = Red\n  | Green\n",
        &mut interfaces,
    );

    for (import, head) in [
        ("import Colour exposing (Colour)", "Colour"),
        ("import Colour", "Colour.Colour"),
    ] {
        let source = format!(
            "module Test exposing (Eq)\n\n{}\n\n{}instance Eq {} where\n  derived\n",
            import, EQ_DERIVED, head
        );
        let module = canonicalize_with_interfaces(&source, &interfaces).unwrap_or_else(|errors| {
            panic!(
                "`{}`: expected it to canonicalize, got {:?}",
                import, errors
            )
        });
        assert_eq!(module.instances.len(), 1, "`{}`", import);
        assert_eq!(module.instances[0].bindings.len(), 1, "`{}`", import);
    }
}

/// `()` has no element for a derivation over one value to begin at, so a class with such a
/// member cannot derive it. A class whose derivations walk two values can: its answer is
/// `matched`.
///
/// Mutation-checked by switching the `walks_one_value` check in `plan` off: the first source
/// canonicalizes and `one_class_error` panics; and by blaming the class's head line instead of
/// the instance's word for `UnitHasNoElement` in `plan` (`signature.span` for
/// `candidate.span`): the range assertion goes red.
#[test]
fn a_derived_unit_instance_needs_a_class_that_walks_two_values() {
    let one_value = with_derivation(indoc::indoc! {r#"
        class Hashable a where
          hash : a -> Int

          derived hash
            atConstructor _ = 1
            combine x y =
              x

        instance Hashable () where
          derived
    "#});
    let (error, labels) = one_class_error(&one_value);
    let canonical::Error::DerivedInstanceNoShape(problem, _) = &error else {
        panic!("expected DerivedInstanceNoShape, got {:?}", error);
    };
    assert_eq!(
        *problem,
        canonical::DerivedShapeProblem::UnitHasNoElement("Hashable".into())
    );
    // The word of the instance, the second `derived` in the source.
    assert_eq!(labels, vec![(true, nth_range(&one_value, "derived", 1))]);

    let two_values = with_derivation(&format!("{}instance Eq () where\n  derived\n", EQ_DERIVED));
    let module = derived_module(&two_values);
    let [instance] = module.instances.as_slice() else {
        panic!("one instance, got {:?}", module.instances);
    };
    assert_eq!(instance.signature.head, canonical::InstanceHead::Unit);
    assert!(instance.signature.context.is_empty());
    assert_eq!(instance.bindings.len(), 1);
}

/// `Box a`, holding an `a`, needs the class of `a`: one constraint on the head's variable,
/// where the word `derived` was written. A tuple needs it of each element.
///
/// Mutation-checked by adding no constraint for a variable in `reduce`: the context is empty
/// and the first assertion goes red.
#[test]
fn a_derived_instance_infers_a_constraint_on_the_variable_it_holds() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            instance Eq (Box a) where
              derived

            instance Eq (a, b) where
              derived
        "#}
    ));
    let module = derived_module(&source);

    let boxed = instance_for(&module, "Box");
    assert_eq!(context_of(boxed), pairs(&[("Eq", "a")]));
    // The constraint was not written, and its span is the word that asked for it.
    assert_eq!(
        boxed.signature.context[0]
            .span
            .span()
            .map(|span| span.to_range()),
        Some(nth_range(&source, "derived", 1))
    );

    let tuple = module
        .instances
        .iter()
        .find(|instance| matches!(instance.signature.head, canonical::InstanceHead::Tuple(_)))
        .expect("the tuple instance");
    assert_eq!(context_of(tuple), pairs(&[("Eq", "a"), ("Eq", "b")]));
}

/// A parameter no variant uses carries no constraint: the instance needs nothing of it.
///
/// Mutation-checked by constraining every variable of the head, whatever the type holds, in
/// `derive_all`: the context is `Eq a` and the assertion goes red.
#[test]
fn a_parameter_no_variant_uses_infers_no_constraint() {
    let source = with_derivation(&format!(
        "{}{}{}",
        EQ_DERIVED,
        EQ_INT,
        indoc::indoc! {r#"
            type Phantom a
              = Phantom Int

            instance Eq (Phantom a) where
              derived
        "#}
    ));
    let module = derived_module(&source);

    assert!(instance_for(&module, "Phantom")
        .signature
        .context
        .is_empty());
}

/// A variant holding a concrete type with no instance is an error naming the variant and
/// the type, at the word `derived`, with the type's declaration shown beside it.
///
/// Mutation-checked by answering `Ok` for a type with no instance in `reduce`: the module
/// canonicalizes and `class_errors` panics.
#[test]
fn a_variant_holding_a_type_with_no_instance_is_an_error_naming_the_variant_and_the_type() {
    use zelkova_compiler::PhaseError;

    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            type Key
              = Key

            type Entry
              = Entry Key

            instance Eq Entry where
              derived
        "#}
    ));
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivedInstanceRequires(requirement, _) = &error else {
        panic!("expected DerivedInstanceRequires, got {:?}", error);
    };
    assert_eq!(requirement.class.as_str(), "Eq");
    assert_eq!(
        requirement.part,
        canonical::DerivedPart::Variant("Entry".into())
    );
    assert_eq!(requirement.argument, "Key");
    assert_eq!(requirement.missing, "Key");
    assert!(!requirement.function);

    let message = error.message();
    assert!(
        message.contains("`Entry`") && message.contains("`Key`"),
        "{}",
        message
    );

    assert_eq!(
        labels,
        vec![
            (true, nth_range(&source, "derived", 1)),
            (false, range_of(&source, "type Entry\n  = Entry Key")),
        ]
    );
}

/// A variant holding a function is that error with no fix: no instance can be declared for a
/// function type.
///
/// Mutation-checked by accepting an arrow in `reduce`: the module canonicalizes and
/// `class_errors` panics; and by dropping the `declared` label in `DerivedInstanceRequires`'s
/// `labels()` arm, or blaming no span for the error in `derive_all` (`NodeSpan::none()` for
/// `candidate.span`): the range assertion goes red.
#[test]
fn a_variant_holding_a_function_is_an_error_no_instance_can_fix() {
    let source = with_derivation(&format!(
        "{}{}{}",
        EQ_DERIVED,
        EQ_INT,
        indoc::indoc! {r#"
            type Handler
              = Handler (Int -> Int)

            instance Eq Handler where
              derived
        "#}
    ));
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivedInstanceRequires(requirement, _) = &error else {
        panic!("expected DerivedInstanceRequires, got {:?}", error);
    };
    assert_eq!(requirement.argument, "Int -> Int");
    assert!(requirement.function);
    assert_eq!(
        labels,
        vec![
            (true, nth_range(&source, "derived", 1)),
            (
                false,
                range_of(&source, "type Handler\n  = Handler (Int -> Int)")
            ),
        ]
    );
}

/// An argument that is an application needs the instance for its head and whatever that
/// instance's context asks of the arguments, down to the variables: `Maybe2 a` needs
/// `Eq (Maybe2 a)`, which asks `Eq a`, which is a constraint on the derived instance; and
/// `Maybe2 Key` needs `Eq Key`, which is missing, and the error says which type was.
///
/// Mutation-checked by not reading an instance's context in `reduce`: `Wrap`'s context is
/// empty and its assertion goes red; and by dropping the `declared` label in
/// `DerivedInstanceRequires`'s `labels()` arm: the range assertion goes red.
#[test]
fn an_argument_that_is_an_application_needs_what_its_instance_asks_of_the_arguments() {
    let declarations = format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            type Maybe2 a
              = Nothing2
              | Just2 a

            instance Eq a => Eq (Maybe2 a) where
              eq _ _ =
                True

            type Key
              = Key

            type Wrap a
              = Wrap (Maybe2 a)

            instance Eq (Wrap a) where
              derived
        "#}
    );
    let module = derived_module(&with_derivation(&declarations));
    assert_eq!(
        context_of(instance_for(&module, "Wrap")),
        pairs(&[("Eq", "a")])
    );

    let source = with_derivation(&format!(
        "{}\ntype Entry\n  = Entry (Maybe2 Key)\n\ninstance Eq Entry where\n  derived\n",
        declarations
    ));
    // `Wrap` is fine, `Entry` is the one failure.
    let errors = class_errors(&source);
    let [canonical::Error::DerivedInstanceRequires(requirement, _)] = errors.as_slice() else {
        panic!("expected one DerivedInstanceRequires, got {:?}", errors);
    };
    assert_eq!(
        requirement.part,
        canonical::DerivedPart::Variant("Entry".into())
    );
    assert_eq!(requirement.argument, "Maybe2 Key");
    assert_eq!(requirement.missing, "Key");
    // The word of `Entry`'s instance, the third `derived` of the source after the class's and
    // `Wrap`'s, and the type it is for beside it.
    assert_eq!(
        label_ranges(&errors[0]),
        vec![
            (true, nth_range(&source, "derived", 2)),
            (
                false,
                range_of(&source, "type Entry\n  = Entry (Maybe2 Key)")
            ),
        ]
    );
}

/// A recursive type asks for its own instance, which counts as in scope: `List a` holding an
/// `a` and a `List a` infers `Eq a` and nothing more.
///
/// Mutation-checked by leaving the instances being derived out of the table in `derive_all`:
/// the recursive argument has no instance and `derived_module` panics.
#[test]
fn a_recursive_type_infers_the_constraint_of_its_parameter_and_nothing_more() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            type List a
              = Nil
              | Cons a (List a)

            instance Eq (List a) where
              derived
        "#}
    ));
    let module = derived_module(&source);

    assert_eq!(
        context_of(instance_for(&module, "List")),
        pairs(&[("Eq", "a")])
    );
}

/// Two types in one module that hold each other each infer what the other needs: `A` needs
/// what `B` needs of `a` and `b`, and the other way round.
///
/// Mutation-checked by leaving the instances being derived out of the table in `derive_all`:
/// each holds the other, which has no instance, and `derived_module` panics.
#[test]
fn two_mutually_recursive_types_each_infer_what_the_other_needs() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            type A a b
              = A (B a b)

            type B a b
              = Next b (A a b)
              | End a

            instance Eq (A a b) where
              derived

            instance Eq (B a b) where
              derived
        "#}
    ));
    let module = derived_module(&source);

    for head in ["A", "B"] {
        let mut context = context_of(instance_for(&module, head));
        context.sort();
        assert_eq!(
            context,
            pairs(&[("Eq", "a"), ("Eq", "b")]),
            "for `{}`",
            head
        );
    }
}

/// A longer cycle needs the fixed point to go all the way round: `A` holds a `B`, which holds
/// a `C`, which holds an `A`, each with a parameter of its own the others lack. Whatever order
/// the instances are read in, each ends with both parameters, which one pass over them in the
/// order written does not give: `A` is read before `C` has said what it needs.
///
/// Mutation-checked by running the fixed point once in `derive_all` (`break` after the first
/// pass): `A` is then read against a `B` that has not yet heard from `C`, and the assertion
/// on `A` goes red.
#[test]
fn a_cycle_of_three_types_reaches_a_fixed_point() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            type A a b
              = A (B a b)

            type B a b
              = B b (C a b)

            type C a b
              = C a (A a b)

            instance Eq (A a b) where
              derived

            instance Eq (B a b) where
              derived

            instance Eq (C a b) where
              derived
        "#}
    ));
    let module = derived_module(&source);

    for head in ["A", "B", "C"] {
        let mut context = context_of(instance_for(&module, head));
        context.sort();
        assert_eq!(
            context,
            pairs(&[("Eq", "a"), ("Eq", "b")]),
            "for `{}`",
            head
        );
    }
}

/// An instance derived in another module than its class has its inferred context in its own
/// module's interface, and what it is made of with it.
///
/// Mutation-checked by adding none of the inferred constraints to a derived instance's context
/// in `do_instances`: the instance is published with the context it wrote, and the context
/// assertion goes red.
#[test]
fn a_derived_instance_in_another_module_than_its_class_publishes_its_context() {
    let mut interfaces = scalar_interfaces();
    publish(
        &format!("module Classes exposing (Eq)\n\n{}", EQ_DERIVED),
        &mut interfaces,
    );

    let source = indoc::indoc! {r#"
        module Types exposing (Box(..))

        import Classes exposing (Eq)

        type Box a
          = Box a

        instance Eq (Box a) where
          derived
    "#};
    let module =
        canonicalize_with_interfaces(source, &interfaces).expect("the module canonicalizes");
    let interface = module.to_interface(None);

    let published = interface
        .instances
        .iter()
        .find(|published| published.signature.module.name().as_str() == "Types")
        .expect("the instance is in the module's interface");
    let context: Vec<(String, String)> = published
        .signature
        .context
        .iter()
        .map(|constraint| {
            (
                constraint.class.unqualified_name().to_string(),
                constraint.variable.to_string(),
            )
        })
        .collect();
    assert_eq!(context, pairs(&[("Eq", "a")]));
    assert_eq!(published.signature.class, test_qual("Classes.Eq"));

    // Its members are the class's one member, written out here.
    let [binding] = instance_for(&module, "Box").bindings.as_slice() else {
        panic!("one member");
    };
    let canonical::Value::Value { name, patterns, .. } = binding else {
        panic!("a binding without an annotation");
    };
    assert_eq!(name.as_str(), "eq");
    assert_eq!(patterns.len(), 2);
}

/// Every node of a generated member carries the span of the word `derived`, the placed
/// bodies of the class included: they were written in another file, and a span carried over
/// would point at unrelated text.
///
/// Mutation-checked by keeping the class's own span on every node `Generated::rewrite` builds
/// (`expression.span` for `self.span`): the placed `case` of `combine` is then a span of the
/// class and the assertion goes red.
#[test]
fn every_node_of_a_generated_member_is_blamed_on_the_word_derived() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            instance Eq (Box a) where
              derived
        "#}
    ));
    let module = derived_module(&source);
    let word = nth_range(&source, "derived", 1);

    fn spans(expression: &canonical::Expression, into: &mut Vec<Option<std::ops::Range<usize>>>) {
        use canonical::ExpressionKind::*;

        into.push(expression.span.span().map(|span| span.to_range()));
        match &expression.kind {
            Apply(function, argument) => {
                spans(function, into);
                spans(argument, into);
            }
            Case(scrutinee, branches) => {
                spans(scrutinee, into);
                for branch in branches {
                    into.push(branch.pattern.span.span().map(|span| span.to_range()));
                    into.push(branch.span.span().map(|span| span.to_range()));
                    spans(&branch.expression, into);
                }
            }
            If(a, b, c) => {
                spans(a, into);
                spans(b, into);
                spans(c, into);
            }
            _ => {}
        }
    }

    let [canonical::Value::Value {
        body,
        span,
        patterns,
        ..
    }] = instance_for(&module, "Box").bindings.as_slice()
    else {
        panic!("one member");
    };
    let mut found = vec![span.span().map(|span| span.to_range())];
    found.extend(
        patterns
            .iter()
            .map(|pattern| pattern.span.span().map(|span| span.to_range())),
    );
    spans(body, &mut found);

    assert!(
        found.len() > 10,
        "a body with some nodes in it: {:?}",
        found
    );
    for span in found {
        assert_eq!(span, Some(word.clone()));
    }
}

/// A context written on a derived instance is the instance's context, kept as written and in
/// the order written, where inference alone would have found `Eq a` and no more.
///
/// Mutation-checked by returning the inferred context from `derive_all` whatever the candidate
/// writes: the context is `Eq a` alone and the assertion goes red.
#[test]
fn a_context_written_on_a_derived_instance_is_its_context() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            class Other a where
              other : a -> Bool

            instance (Other a, Eq a) => Eq (Box a) where
              derived
        "#}
    ));
    let module = canonicalize_with_scalars(&source).expect("the written context covers `Eq a`");

    assert_eq!(
        context_of(instance_for(&module, "Box")),
        pairs(&[("Other", "a"), ("Eq", "a")])
    );
}

/// A written context that leaves out a constraint the type's arguments need is an error at the
/// context, with the word `derived` beside it, naming the constraint and the variant that needs
/// it. A parenthesised context is underlined with its parentheses, and a tuple's element is
/// named by its place.
///
/// Mutation-checked by making `provides` answer `true`: the module canonicalizes and
/// `one_class_error` panics; and by swapping the two spans in the error's `labels()` arm: the
/// range assertions go red.
#[test]
fn a_written_context_that_lacks_a_needed_constraint_is_an_error() {
    use zelkova_compiler::PhaseError;

    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            class Other a where
              other : a -> Bool

            instance Other a => Eq (Box a) where
              derived
        "#}
    ));
    let (error, labels) = one_class_error(&source);

    let canonical::Error::DerivedInstanceContextTooNarrow(gap, _, _) = &error else {
        panic!("expected DerivedInstanceContextTooNarrow, got {:?}", error);
    };
    assert_eq!(gap.class.as_str(), "Eq");
    assert_eq!(gap.missing, "Eq a");
    assert_eq!(
        error.message(),
        "`Box` holds a `a`, which needs `Eq a` for the derived instance of `Eq`, and the instance's context does not provide it"
    );
    assert_eq!(
        labels,
        vec![
            (true, nth_range(&source, "Other a", 1)),
            (false, nth_range(&source, "derived", 1)),
        ]
    );

    let parenthesised = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            class Other a where
              other : a -> Bool

            instance (Other a, Eq b) => Eq (a, b) where
              derived
        "#}
    ));
    let (error, labels) = one_class_error(&parenthesised);
    let canonical::Error::DerivedInstanceContextTooNarrow(gap, _, _) = &error else {
        panic!("expected DerivedInstanceContextTooNarrow, got {:?}", error);
    };
    assert_eq!(gap.missing, "Eq a");
    assert_eq!(gap.part, canonical::DerivedPart::Element(1));
    assert_eq!(
        labels,
        vec![
            (true, range_of(&parenthesised, "(Other a, Eq b)")),
            (false, nth_range(&parenthesised, "derived", 1)),
        ]
    );
}

/// A written context provides the superclasses of the classes it names, however many classes
/// away: `Sorted a` provides the `Eq a` a derived `Eq (Box a)` needs through `Comparable`, and
/// the context kept is the one written. It does not grow by what it provides, so a `Wrap` of
/// that `Box` needs `Sorted a` and no `Eq a` beside it.
///
/// Mutation-checked by dropping the `pending.extend` of `provides`, so that only a constraint
/// written outright counts: the module is rejected and the `expect` panics; and by dropping the
/// `continue` of `derive_all`'s fixed point, so that a written context grows: `Wrap`'s context
/// gains `Eq a` and its assertion goes red.
#[test]
fn a_written_context_provides_through_a_superclass() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            class Eq a => Comparable a where
              lessThan : a -> a -> Bool

            class Comparable a => Sorted a where
              sorted : a -> Bool

            instance Sorted a => Eq (Box a) where
              derived

            type Wrap a
              = Wrap (Box a)

            instance Eq (Wrap a) where
              derived
        "#}
    ));
    let module = canonicalize_with_scalars(&source).expect("`Sorted a` provides `Eq a`");

    assert_eq!(
        context_of(instance_for(&module, "Box")),
        pairs(&[("Sorted", "a")])
    );
    assert_eq!(
        context_of(instance_for(&module, "Wrap")),
        pairs(&[("Sorted", "a")])
    );
}

/// What a derived instance writes is what another derived instance needs of it: a `Wrap` of a
/// `Box` whose derived instance wrote `(Other a, Eq a)` needs both, where a `Box` that wrote
/// nothing would have asked `Eq a` alone.
///
/// Mutation-checked by giving a derived instance no context in the `written` list of
/// `do_instances`: `Wrap`'s context is empty and the assertion goes red.
#[test]
fn a_written_context_is_what_another_derived_instance_needs() {
    let source = with_derivation(&format!(
        "{}{}",
        EQ_DERIVED,
        indoc::indoc! {r#"
            class Other a where
              other : a -> Bool

            instance (Other a, Eq a) => Eq (Box a) where
              derived

            type Wrap a
              = Wrap (Box a)

            instance Eq (Wrap a) where
              derived
        "#}
    ));
    let module = canonicalize_with_scalars(&source).expect("both instances are derived");

    assert_eq!(
        context_of(instance_for(&module, "Wrap")),
        pairs(&[("Other", "a"), ("Eq", "a")])
    );
}

/// An instance whose class's declaration did not canonicalize has no members to keep. It is
/// dropped from the module and from its interface, with nothing said of it here: the class's own
/// error says it.
///
/// Mutation-checked by keeping an instance whose derivation failed silently (dropping
/// `&& !failed_silently` in `do_instances`): it is kept with no bindings and the first
/// assertion goes red.
#[test]
fn a_derived_instance_of_a_class_that_failed_is_dropped_with_nothing_said_of_it() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Nope

          derived eq
            matched = True
            differed _ _ = False
            combine x y =
              y

        instance Eq Colour where
          derived
    "#});
    let canonicalized = canonicalize_recovering_with_interfaces(&source, &scalar_interfaces());

    assert!(
        matches!(
            canonicalized.errors.as_slice(),
            [canonical::Error::TypeNotFound(..)]
        ),
        "{:?}",
        canonicalized.errors
    );
    assert!(
        canonicalized.module.instances.is_empty(),
        "a memberless instance was kept"
    );
    assert!(canonicalized.module.to_interface(None).instances.is_empty());
}

/// A class whose derivation failed has said so, and an instance asking for it to be derived
/// does not say it again: the one error is the derivation's. The same holds for the instance
/// of an importing module, which reads the class from its interface. The instance is left
/// out of the module that wrote it, whose scope is incomplete for it, so an importer of that
/// is not told of the instance missing.
///
/// Mutation-checked by raising `DerivedInstanceNotDerivable` whatever `derivations_rejected`
/// says in `plan`: the instance is reported too and `one_class_error` panics, and so does the
/// importer's; and by dropping the `instances.len() < source.instances.len()` test that marks
/// the scope incomplete in `canonicalize_recovering`: the importer's `incomplete` assertion goes
/// red.
#[test]
fn an_instance_of_a_class_whose_derivation_failed_is_not_reported_for_it() {
    let class = indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool
          neq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
            combine x y =
              y

    "#};
    let source = with_derivation(&format!("{}instance Eq Colour where\n  derived\n", class));
    let (error, _) = one_class_error(&source);

    assert!(
        matches!(error, canonical::Error::DerivationIncomplete(..)),
        "{:?}",
        error
    );

    let mut interfaces = scalar_interfaces();
    let declared = canonicalize_recovering_with_interfaces(
        &format!("module Classes exposing (Eq)\n\n{}", class),
        &interfaces,
    );
    assert!(
        declared.module.classes[&"Eq".into()]
            .signature
            .derivations_rejected
    );
    let interface = declared.module.to_interface(None);
    interfaces.insert(interface.module_name.name().clone(), interface);

    let importer = indoc::indoc! {r#"
        module Types exposing (Colour(..))

        import Classes exposing (Eq)

        type Colour
          = Red

        instance Eq Colour where
          derived
    "#};
    let canonicalized = canonicalize_recovering_with_interfaces(importer, &interfaces);
    assert!(
        canonicalized.errors.is_empty(),
        "{:?}",
        canonicalized.errors
    );
    assert!(canonicalized.module.instances.is_empty());
    assert!(canonicalized.module.incomplete);
}

/// A failed derivation removes no name from the scope, so an error that has nothing to do with
/// it is still reported: here a name no declaration gives, in an instance of another class,
/// beside the derivation that has no `combine`. The scope is as complete as it was.
///
/// Mutation-checked by marking the scope incomplete after a derivation error in
/// `canonicalize_recovering` (`env.set_incomplete()`): the `VariableNotFound` is dropped and
/// the assertion goes red.
#[test]
fn a_failed_derivation_does_not_hide_an_unrelated_error() {
    let source = with_derivation(indoc::indoc! {r#"
        class Eq a where
          eq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False

        class Show a where
          show : a -> Int

        instance Show Bool where
          show b =
            nope
    "#});
    let errors = class_errors(&source);

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, canonical::Error::DerivationBindingMissing(..))),
        "{:?}",
        errors
    );
    let not_found: Vec<_> = errors
        .iter()
        .filter(|error| matches!(error, canonical::Error::VariableNotFound(..)))
        .collect();
    assert_eq!(not_found.len(), 1, "{:?}", errors);
    assert_eq!(
        label_ranges(not_found[0]),
        vec![(true, range_of(&source, "nope"))]
    );
}

/// A class with no derivation, derived for, is reported whether or not the scope is incomplete
/// for another reason: here a type declaration that did not canonicalize.
///
/// Mutation-checked by adding `DerivedInstanceNotDerivable` to `without_restated`'s list: the
/// error is dropped in the incomplete scope and the assertion goes red.
#[test]
fn a_derived_instance_of_a_class_with_no_derivation_is_reported_in_an_incomplete_scope() {
    let source = with_derivation(indoc::indoc! {r#"
        type Broken
          = Broken Nope

        class Eq a where
          eq : a -> a -> Bool

        instance Eq Colour where
          derived
    "#});
    let errors = class_errors(&source);

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, canonical::Error::TypeNotFound(..))),
        "{:?}",
        errors
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error, canonical::Error::DerivedInstanceNotDerivable(..))),
        "{:?}",
        errors
    );
}
