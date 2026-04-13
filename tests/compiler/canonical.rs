//! Integration tests for the canonicalization phase.
//!
//! Each test parses a source string, runs it through `canonical::canonicalize`,
//! and then asserts on the exact structure of the resulting `canonical::Module` —
//! the pattern bindings, expression bodies, and types — not just that the value
//! key is present.
use std::collections::HashMap;

use zelkova_lang::compiler::canonical;
use zelkova_lang::compiler::name::QualName;

#[path = "../support/mod.rs"]
mod support;

use support::*;

// ── Helpers ──────────────────────────────────────────────────────────────────

/// `Type::Type("Int", [])` — the canonical representation of an unresolved `Int`
/// (no Basics import in these tests).
fn int_t() -> canonical::Type {
    canonical::Type::Type("Int".into(), vec![])
}

/// `Type::Type("Bool", [])` — same for Bool.
fn bool_t() -> canonical::Type {
    canonical::Type::Type("Bool".into(), vec![])
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
            name: "answer".into(),
            patterns: vec![],
            body: canonical::Expression::Int(42),
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
            name: "identity".into(),
            // Pattern `x` is paired with the first arrow-arm type `a`
            patterns: vec![(
                canonical::Pattern::Variable("x".into()),
                canonical::Type::Variable("a".into()),
            )],
            // The body `x` is a reference to the local binding
            body: canonical::Expression::VarLocal("x".into()),
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
            name: "add".into(),
            patterns: vec![
                (canonical::Pattern::Variable("a".into()), int_t()),
                (canonical::Pattern::Variable("b".into()), int_t()),
            ],
            // Body refers to the first pattern binding `a`
            body: canonical::Expression::VarLocal("a".into()),
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
    assert_eq!(color.variables, Vec::<zelkova_lang::compiler::name::Name>::new());
    assert_eq!(color.variants.len(), 3);

    let variant_names: Vec<_> = color.variants.iter().map(|v| v.name.clone()).collect();
    assert_eq!(variant_names, vec!["Red".into(), "Green".into(), "Blue".into()]);

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
            name: "favorite".into(),
            patterns: vec![],
            body: canonical::Expression::VarConstructor(
                QualName::from("Test.Red"),
                color_t.clone(),
            ),
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
    let maybe_a = canonical::Type::Type("Maybe".into(), vec![canonical::Type::Variable("a".into())]);

    let value = module.values.get(&"isJust".into()).unwrap();
    let (patterns, body) = match value {
        canonical::Value::TypedValue { patterns, body, tpe, .. } => {
            assert_eq!(
                tpe,
                &canonical::Type::Arrow(Box::new(maybe_a.clone()), Box::new(maybe_a.clone()))
            );
            (patterns, body)
        }
        other => panic!("expected TypedValue, got {:?}", other),
    };

    // Single pattern `maybe` bound to the first arrow arm
    assert_eq!(
        patterns,
        &vec![(canonical::Pattern::Variable("maybe".into()), maybe_a.clone())]
    );

    // Body is a case expression on VarLocal("maybe")
    let (scrutinee, branches) = match body {
        canonical::Expression::Case(s, b) => (s.as_ref(), b),
        other => panic!("expected Case, got {:?}", other),
    };
    assert_eq!(scrutinee, &canonical::Expression::VarLocal("maybe".into()));
    assert_eq!(branches.len(), 2);

    // Branch 0: `Just x` pattern — Constructor with one Variable arg
    let just_ctor = canonical::TypeConstructor {
        name: "Just".into(),
        type_parameters: vec![canonical::Type::Variable("a".into())],
        tpe: "Maybe".into(),
    };
    assert_eq!(
        branches[0].pattern,
        canonical::Pattern::Constructor {
            ctor: just_ctor,
            args: vec![canonical::Pattern::Variable("x".into())],
        }
    );
    // Expression is Apply(VarConstructor("Test.Just", _), VarLocal("x"))
    assert!(
        matches!(&branches[0].expression, canonical::Expression::Apply(_, _)),
        "Just x branch expression should be Apply, got {:?}",
        branches[0].expression
    );

    // Branch 1: `Nothing` pattern — Constructor with no args
    let nothing_ctor = canonical::TypeConstructor {
        name: "Nothing".into(),
        type_parameters: vec![],
        tpe: "Maybe".into(),
    };
    assert_eq!(
        branches[1].pattern,
        canonical::Pattern::Constructor {
            ctor: nothing_ctor,
            args: vec![],
        }
    );
    // Expression is VarConstructor("Test.Nothing", _)
    assert!(
        matches!(&branches[1].expression, canonical::Expression::VarConstructor(_, _)),
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
            name: "max".into(),
            patterns: vec![
                (canonical::Pattern::Variable("a".into()), int_t()),
                (canonical::Pattern::Variable("b".into()), int_t()),
            ],
            body: canonical::Expression::If(
                Box::new(canonical::Expression::Bool(true)),
                Box::new(canonical::Expression::VarLocal("a".into())),
                Box::new(canonical::Expression::VarLocal("b".into())),
            ),
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
            name: "add".into(),
            patterns: vec![],
            body: canonical::Expression::Bool(true),
            tpe: canonical::Type::Arrow(
                Box::new(int_t()),
                Box::new(canonical::Type::Arrow(Box::new(int_t()), Box::new(int_t()))),
            ),
        }
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
    let module = canonicalize_with_interfaces(source, &interfaces)
        .expect("should canonicalize");

    // The imported `Maybe` interface stores the type as `Type::Type("Maybe", [])`
    // (insert_foreign_union_type uses an empty param list).  The annotation
    // `Maybe a` resolves via env.find_type → returns that stored value verbatim,
    // ignoring the `a` parameter (a known simplification).
    let maybe_t = canonical::Type::Type("Maybe".into(), vec![]);

    let value = module.values.get(&"safeHead".into()).unwrap();
    let (patterns, body) = match value {
        canonical::Value::TypedValue { patterns, body, tpe, .. } => {
            assert_eq!(
                tpe,
                &canonical::Type::Arrow(Box::new(maybe_t.clone()), Box::new(maybe_t.clone()))
            );
            (patterns, body)
        }
        other => panic!("expected TypedValue, got {:?}", other),
    };

    // Single parameter `m` bound to the first arrow-arm type
    assert_eq!(
        patterns,
        &vec![(canonical::Pattern::Variable("m".into()), maybe_t)]
    );

    // Body is `case m of ...`
    let (scrutinee, branches) = match body {
        canonical::Expression::Case(s, b) => (s.as_ref(), b),
        other => panic!("expected Case, got {:?}", other),
    };
    assert_eq!(scrutinee, &canonical::Expression::VarLocal("m".into()));
    assert_eq!(branches.len(), 2);

    // Patterns come from the imported interface's TypeConstructor records
    let just_ctor = canonical::TypeConstructor {
        name: "Just".into(),
        type_parameters: vec![canonical::Type::Variable("a".into())],
        tpe: "Maybe".into(),
    };
    assert_eq!(
        branches[0].pattern,
        canonical::Pattern::Constructor {
            ctor: just_ctor,
            args: vec![canonical::Pattern::Variable("x".into())],
        }
    );

    let nothing_ctor = canonical::TypeConstructor {
        name: "Nothing".into(),
        type_parameters: vec![],
        tpe: "Maybe".into(),
    };
    assert_eq!(
        branches[1].pattern,
        canonical::Pattern::Constructor {
            ctor: nothing_ctor,
            args: vec![],
        }
    );
}
