//! Layer 2: Type checker expectation tests.
//!
//! These tests describe the expected behaviour once the type checker is
//! integrated into the main pipeline.  They are all marked `#[ignore]` and
//! will be un-ignored (or replaced) when `type_check` is no longer a stub.
//!
//! The tests go through the full `check_module` pipeline (parse →
//! canonicalize → type_check → exhaustiveness), which is the same path that
//! Layer 3 pipeline tests use.  The difference is that these tests assert on
//! *type-level* properties — a type mismatch should become an error, an
//! identity function should produce a polymorphic type, etc.
//!
//! ## How to activate
//!
//! Once the type checker is connected, remove the `#[ignore]` attributes one
//! by one as you confirm each scenario works.  You may also want to expose a
//! richer return type from `check_module` (or a new function) so you can
//! inspect inferred types directly rather than just `is_ok` / `is_err`.

use std::collections::HashMap;

use zelkova_lang::compiler::{check_module, parser};

mod support;

use support::*;

fn run(source: &str) -> Result<zelkova_lang::compiler::canonical::Module, zelkova_lang::compiler::CompilationError> {
    let parsed = parse_source(source);
    let interfaces = HashMap::new();
    check_module(&test_package(), &interfaces, &parsed)
}

// ── Polymorphic identity ──────────────────────────────────────────────────────

/// An identity function with annotation `a -> a` should type-check.
#[test]
#[ignore = "type checker not yet integrated"]
fn identity_function_types() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        identity : a -> a
        identity x = x
    "#};
    assert!(run(source).is_ok(), "identity : a -> a should type-check");
}

// ── Int literal type ──────────────────────────────────────────────────────────

/// A constant `42` should have type `Int`.
#[test]
#[ignore = "type checker not yet integrated"]
fn int_literal_has_type_int() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer : Int
        answer = 42
    "#};
    assert!(run(source).is_ok(), "answer : Int = 42 should type-check");
}

// ── Function application ──────────────────────────────────────────────────────

/// Applying a `Bool -> Bool` function to a `Bool` should yield `Bool`.
#[test]
#[ignore = "type checker not yet integrated"]
fn function_application_types() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        not : Bool -> Bool
        not b = b
        result : Bool
        result = not true
    "#};
    assert!(run(source).is_ok(), "not applied to Bool should type-check");
}

// ── Type mismatch: annotation vs body ────────────────────────────────────────

/// A function annotated `Int -> Int` whose body returns a `Bool` should fail.
#[test]
#[ignore = "type checker not yet integrated"]
fn type_mismatch_annotation_vs_body() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : Int -> Int
        bad x = true
    "#};
    assert!(run(source).is_err(), "Int -> Int with Bool body should fail");
}

// ── Unbound variable ──────────────────────────────────────────────────────────

/// Referencing a name that is not in scope should produce a type error.
#[test]
#[ignore = "type checker not yet integrated"]
fn unbound_variable_is_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        oops : Int
        oops = nonExistentBinding
    "#};
    // Note: canonicalization already catches unbound variables today, so this
    // test may pass even before the type checker lands.  It documents the
    // expected end-state regardless.
    assert!(run(source).is_err(), "unbound variable should be an error");
}

// ── Constructor usage ─────────────────────────────────────────────────────────

/// `Just 42` should have type `Maybe Int` once the type checker is integrated.
#[test]
#[ignore = "type checker not yet integrated"]
fn constructor_usage_just_42() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a = Just a | Nothing
        wrapped : Maybe Int
        wrapped = Just 42
    "#};
    assert!(run(source).is_ok(), "Just 42 : Maybe Int should type-check");
}

// ── Case expression: branches must return same type ───────────────────────────

/// Both branches of a `case` must have the same type.
#[test]
#[ignore = "type checker not yet integrated"]
fn case_branches_must_match() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a = Just a | Nothing
        extract : Maybe Int -> Int
        extract m =
          case m of
            Just x -> x
            Nothing -> 42
    "#};
    assert!(
        run(source).is_ok(),
        "case with matching branch types should type-check"
    );
}

/// Case branches returning different types should fail.
#[test]
#[ignore = "type checker not yet integrated"]
fn case_branches_type_mismatch() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a = Just a | Nothing
        bad : Maybe Int -> Int
        bad m =
          case m of
            Just x -> x
            Nothing -> true
    "#};
    assert!(
        run(source).is_err(),
        "case with mismatching branch types should fail"
    );
}

// ── If expression ─────────────────────────────────────────────────────────────

/// `if` condition must be `Bool` and both branches must have matching types.
#[test]
#[ignore = "type checker not yet integrated"]
fn if_expression_types() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        max : Int -> Int -> Int
        max a b = if true then a else b
    "#};
    assert!(run(source).is_ok(), "if/then/else with matching types should type-check");
}

/// `if` with non-Bool condition should fail.
#[test]
#[ignore = "type checker not yet integrated"]
fn if_non_bool_condition() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : Int
        bad = if 42 then 1 else 2
    "#};
    assert!(
        run(source).is_err(),
        "if condition must be Bool, not Int"
    );
}
