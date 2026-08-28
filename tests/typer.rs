//! Layer 2: Type checker expectation tests.
//!
//! These tests go through the full `check_module` pipeline (parse →
//! canonicalize → type_check → exhaustiveness), which is the same path that
//! Layer 3 pipeline tests use. The difference is that these tests assert on
//! *type-level* properties — a type mismatch should become an error, an
//! identity function should produce a polymorphic type, etc. A failure here
//! can implicate canonicalization or `typer::type_check`, since both run on
//! the way to the assertion.

use std::collections::HashMap;
use std::ops::Range;

use zelkova_lang::compiler::{check_module, typer, CompilationError, PhaseError, SpanLabel};

mod support;

use support::*;

fn run(
    source: &str,
) -> Result<zelkova_lang::compiler::canonical::Module, zelkova_lang::compiler::CompilationError> {
    let parsed = parse_source(source);
    let interfaces = HashMap::new();
    check_module(&test_package(), &interfaces, &parsed)
}

/// The type errors `source` produced, insisting that they *are* type errors.
///
/// `is_err()` on its own cannot tell "the type checker rejected this" from "it never
/// got that far": a source that fails to canonicalize — a misspelt constructor, an
/// import that does not resolve — also returns `Err`, and would keep a test green
/// while the phase it is about did nothing at all.
fn type_errors(source: &str) -> Vec<typer::Error> {
    match run(source) {
        Ok(_) => panic!("expected a type error, but the module checked"),
        Err(CompilationError::Type(errors, _)) => errors,
        Err(other) => panic!("expected a type error, got {:?}", other),
    }
}

/// Exactly one type error, which is what every source in this file is written to
/// produce: several would make "the first label" an accident of iteration order.
fn one_type_error(source: &str) -> typer::Error {
    let mut errors = type_errors(source);
    assert_eq!(errors.len(), 1, "expected one type error, got {:?}", errors);
    errors.remove(0)
}

/// The byte range of `needle` in `source`, which is what a label carries.
///
/// Computed from the source rather than written down, so editing a test's source
/// cannot leave a stale offset silently passing.
fn range_of(source: &str, needle: &str) -> Range<usize> {
    let start = source
        .find(needle)
        .unwrap_or_else(|| panic!("`{}` is not in the source", needle));
    assert_eq!(
        source[start + 1..].find(needle),
        None,
        "`{}` must occur once for the range to be unambiguous",
        needle
    );
    start..(start + needle.len())
}

fn ranges(labels: &[SpanLabel]) -> Vec<Range<usize>> {
    labels.iter().map(|l| l.span.to_range()).collect()
}

// ── Polymorphic identity ──────────────────────────────────────────────────────

/// An identity function with annotation `a -> a` should type-check.
#[test]
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

/// A function annotated `Int -> Int` whose body returns a `Bool` should fail, with
/// the caret under the body rather than across the declaration.
///
/// The parameter is what makes this different from the declaration-level example in
/// `annotation_mismatch_points_at_the_expression_and_the_annotation`: the annotation
/// is `Int -> Int`, so the type the body is held to is a *component* of it, reached
/// by decomposing the constraint that gives the function its shape. If that
/// decomposition dropped the origin, the caret would land on the whole function.
///
/// Mutation-checked by having the `Fun`/`Fun` arm of `unify_one_constraint` build
/// fresh constraints instead of `constraint.component(..)`: the primary label moves
/// off `true`.
#[test]
fn type_mismatch_annotation_vs_body() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : Int -> Int
        bad x = true
    "#};
    let error = one_type_error(source);

    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "true"),
            range_of(source, "bad : Int -> Int")
        ],
        "expected a caret under the body and the annotation behind it"
    );
}

// ── Where a type error points ─────────────────────────────────────────────────

/// `ERR-4`'s worked example: the caret goes under `false`, and `Int` is explained by
/// the annotation on the line above.
///
/// Every part of this is a claim about a different link in the chain. The primary
/// range says the term language carried the canonical spans down into the
/// constraints. The secondary range says the substitution that solved the branch's
/// type remembered which constraint solved it, and that following that chain back
/// arrives at the annotation rather than at the `if` in between. The `primary` flags
/// say which of the two is the error and which is the context — codespan renders them
/// differently, and swapping them would tell the reader to go and change the
/// annotation.
///
/// The `1` in the true branch is deliberately not named anywhere: it is a `number`,
/// which unifies with `Int` happily, so only one of the two branches is wrong and only
/// one caret is right.
///
/// Mutation-checked three ways, each red on its own: giving every `Term` built by
/// `canonical_expr_to_term` a `NodeSpan::none()` (the labels fall back to the whole
/// declaration); pushing the annotation constraint after the body's in
/// `infer_annotated` (the primary lands on the `if` rather than on `false`); and
/// making `Substitution::apply` keep the constraint's origin unchanged (the secondary
/// label disappears).
#[test]
fn annotation_mismatch_points_at_the_expression_and_the_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer : Int
        answer = if true then 1 else false
    "#};
    let error = one_type_error(source);

    let labels = error.labels();
    assert_eq!(
        ranges(&labels),
        vec![range_of(source, "false"), range_of(source, "answer : Int")],
        "expected a caret under `false` and the annotation behind it"
    );
    assert!(labels[0].primary, "`false` is what has to change");
    assert!(
        !labels[1].primary,
        "the annotation is context, not the error"
    );

    assert_eq!(error.message(), "cannot match `Int` with `Bool`");
    assert!(
        error
            .notes()
            .iter()
            .any(|n| n.contains("declaration of `answer`")),
        "the declaration should still be named for a reader with no carets, got {:?}",
        error.notes()
    );
}

// ── Unbound variable ──────────────────────────────────────────────────────────

/// Referencing a name that is not in scope should produce a type error.
#[test]
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
    let error = one_type_error(source);

    // The `Nothing` branch is the one that disagrees with `Int`; `Just x -> x` does
    // not, and underlining the whole `case` would be underlining both.
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "true"),
            range_of(source, "bad : Maybe Int -> Int")
        ]
    );
}

// ── If expression ─────────────────────────────────────────────────────────────

/// `if` condition must be `Bool` and both branches must have matching types.
#[test]
fn if_expression_types() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        max : Int -> Int -> Int
        max a b = if true then a else b
    "#};
    assert!(
        run(source).is_ok(),
        "if/then/else with matching types should type-check"
    );
}

/// `if` with non-Bool condition should fail, pointing at the condition and saying
/// which rule it broke.
///
/// This is the one shape where the explanation and the failure are the same piece of
/// text — `42` is both the literal that has the wrong type and the condition that
/// required a `Bool` — so there is only one label, and the rule that was broken has
/// to arrive as a note instead. Drawing the secondary label anyway would put two
/// carets under the same two characters.
///
/// Mutation-checked by dropping the `Reason::IfCondition` arm of `Reason::note` (the
/// note disappears) and by removing the `span != primary.span` guard in
/// `Error::labels` (a second, identical label appears).
#[test]
fn if_non_bool_condition() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : Int
        bad = if 42 then 1 else 2
    "#};
    let error = one_type_error(source);

    assert_eq!(ranges(&error.labels()), vec![range_of(source, "42")]);
    assert!(
        error
            .notes()
            .iter()
            .any(|n| n.contains("condition of an `if` must be a `Bool`")),
        "the broken rule should be named, got {:?}",
        error.notes()
    );
}

// ── Char and Float literals ───────────────────────────────────────────────────

/// A `Char` literal `'a'` should have type `Char`.
#[test]
fn char_literal_has_type_char() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        myChar : Char
        myChar = 'a'
    "#};
    assert!(run(source).is_ok(), "myChar : Char = 'a' should type-check");
}

/// A `Float` literal `3.14` should have type `Float`.
#[test]
fn float_literal_has_type_float() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        myFloat : Float
        myFloat = 3.14
    "#};
    assert!(
        run(source).is_ok(),
        "myFloat : Float = 3.14 should type-check"
    );
}

/// A `Char` literal used where `Int` is expected should fail, and the message should
/// name both types the way the source spells them.
#[test]
fn char_type_mismatch() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : Int
        bad = 'x'
    "#};
    let error = one_type_error(source);

    assert_eq!(
        error.message(),
        "cannot match `Int` with `Char`",
        "the headline names both types, in declared-then-inferred order"
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_of(source, "'x'"), range_of(source, "bad : Int")]
    );
}

// ── Tuple types and expressions ───────────────────────────────────────────────

/// A pair `(Int, Bool)` should type-check.
#[test]
fn tuple_pair_typechecks() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        pair : (Int, Bool)
        pair = (42, true)
    "#};
    assert!(run(source).is_ok(), "(Int, Bool) tuple should type-check");
}

/// Using `(Int, Int)` where `(Int, Bool)` is expected should fail.
#[test]
fn tuple_type_mismatch() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : (Int, Int)
        bad = (42, true)
    "#};
    let error = one_type_error(source);

    // Not the whole tuple: the first element agrees with the annotation and only the
    // second does not.
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "true"),
            range_of(source, "bad : (Int, Int)")
        ]
    );
}

/// A triple `(Int, Bool, Char)` should type-check. AST-3 replaced the typer's
/// `Type::Tuple`/`Term::Tuple`/`TypedTerm::Tuple` `(a, b, Option<c>)` shape
/// with `Tuple<T>`, and `tuple_pair_typechecks` above only exercises the
/// `Tuple::Two` arm on every changed site (annotate, constraint generation,
/// unification, substitution, the `canonical_*_to_typer_*` conversions) — this
/// pins the `Tuple::Three` arm on the same sites.
#[test]
fn tuple_triple_typechecks() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        triple : (Int, Bool, Char)
        triple = (42, true, 'a')
    "#};
    assert!(
        run(source).is_ok(),
        "(Int, Bool, Char) tuple should type-check"
    );
}

/// Using `(Int, Bool, Int)` where `(Int, Bool, Char)` is expected should fail:
/// the third element's type must be unified too, not ignored.
#[test]
fn tuple_triple_type_mismatch() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : (Int, Bool, Char)
        bad = (42, true, 7)
    "#};
    // `type_errors` rather than `is_err`: these are about which `unify` arm the two
    // arities reach, so a failure raised by an earlier phase would not exercise them.
    assert_eq!(type_errors(source).len(), 1);
}

/// A pair used where a triple is expected should fail. Unification has one arm
/// per arity (`Two` against `Two`, `Three` against `Three`), so a mixed pair
/// matches neither and has to reach the generic `Type`-mismatch arm at the
/// bottom of `unify_one_constraint`. Nothing else in the suite exercises that
/// fallthrough: every other tuple test agrees on arity and differs only in
/// element types.
#[test]
fn tuple_pair_against_triple_annotation_is_a_mismatch() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : (Int, Bool, Char)
        bad = (42, true)
    "#};
    // `type_errors` rather than `is_err`: these are about which `unify` arm the two
    // arities reach, so a failure raised by an earlier phase would not exercise them.
    assert_eq!(type_errors(source).len(), 1);
}

/// The other direction of `tuple_pair_against_triple_annotation_is_a_mismatch`:
/// the fallthrough must be reached whichever side of the constraint carries the
/// larger arity, since `unify_one_constraint` matches on the pair `(a, b)` and
/// the two arity arms are not symmetric on their own.
#[test]
fn tuple_triple_against_pair_annotation_is_a_mismatch() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : (Int, Bool)
        bad = (42, true, 'a')
    "#};
    // `type_errors` rather than `is_err`: these are about which `unify` arm the two
    // arities reach, so a failure raised by an earlier phase would not exercise them.
    assert_eq!(type_errors(source).len(), 1);
}
