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

use zelkova_compiler::ir::{Solved, TypedTerm, TypedTermKind};
use zelkova_compiler::name::Name;
use zelkova_compiler::{check_module, typer, CompilationError, PhaseError, SpanLabel};

mod support;

use support::*;

fn run(
    source: &str,
) -> Result<zelkova_compiler::CheckedModule, zelkova_compiler::CompilationError> {
    let parsed = parse_source(source);
    let interfaces = HashMap::from([basics_interface(), char_interface()]);
    check_module(&test_package(), &interfaces, &parsed)
}

/// What the typer solved for `source`, one entry per declaration.
///
/// Goes through `canonicalize` and `typer::type_check` directly rather than through
/// [`run`], which reshapes what the typer solved into an `ir::Module` and keeps only
/// the declarations that have one — these tests are about the entries themselves,
/// including the ones that never become a declaration.
fn solved(source: &str) -> HashMap<Name, Solved> {
    let interfaces = HashMap::from([basics_interface(), char_interface()]);
    let canonical = canonicalize_with_interfaces(source, &interfaces)
        .unwrap_or_else(|errors| panic!("expected the module to canonicalize, got {:?}", errors));

    typer::type_check(&canonical, &interfaces)
        .unwrap_or_else(|errors| panic!("expected the module to type check, got {:?}", errors))
}

/// The one declaration named `name`, which the typer typed.
fn typed_declaration<'a>(solved: &'a HashMap<Name, Solved>, name: &str) -> &'a TypedTerm {
    solved
        .get(&Name::new(name))
        .unwrap_or_else(|| panic!("`{}` should be in the solved types, got {:?}", name, solved))
        .typed()
        .unwrap_or_else(|| panic!("`{}` should have been typed", name))
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

/// The byte range of `needle` within the unique occurrence of `context`.
///
/// For text that is not unique in the file on its own — the `1` in a `case`'s
/// scrutinee, the `not` in `result = not 42` — but is unique inside a phrase that is.
fn range_within(source: &str, context: &str, needle: &str) -> Range<usize> {
    let outer = range_of(source, context);
    let inner = range_of(&source[outer.clone()], needle);

    (outer.start + inner.start)..(outer.start + inner.end)
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
        result = not True
    "#};
    assert!(run(source).is_ok(), "not applied to Bool should type-check");
}

/// `true` and `false` are ordinary lowercase names: a top-level value, a parameter
/// and a reference to either, with no `Bool` anywhere.
///
/// Mutation-checked by restoring `"true"` and `"false"` to the tokenizer's `keyword`
/// table: the source no longer parses and `run` is never reached.
#[test]
fn true_and_false_are_ordinary_names() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        true : Int
        true = 1
        f : Int -> Int
        f false = false
        g : Int
        g = f true
    "#};
    assert!(run(source).is_ok(), "{:?}", run(source).err());

    let solved = solved(source);
    assert_eq!(format!("{}", typed_declaration(&solved, "true").tpe), "Int");
    assert_eq!(format!("{}", typed_declaration(&solved, "g").tpe), "Int");
}

/// `Basics.True` written qualified is the `Bool` an `if` asks for.
///
/// Mutation-checked by having `typer::bool_type` name a `Basics.Boolean` instead:
/// the condition is then a `Basics.Bool` that does not match it.
#[test]
fn if_condition_may_be_basics_true() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer : Int
        answer = if Basics.True then 1 else 2
    "#};

    assert!(run(source).is_ok(), "{:?}", run(source).err());
}

// ── Type mismatch: annotation vs body ────────────────────────────────────────

/// A function annotated `Int -> Int` whose body returns a `Char` should fail, with
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
/// off `'a'`.
#[test]
fn type_mismatch_annotation_vs_body() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : Int -> Int
        bad x = 'a'
    "#};
    let error = one_type_error(source);

    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "'a'"),
            range_of(source, "bad : Int -> Int")
        ],
        "expected a caret under the body and the annotation behind it"
    );
}

/// A body that is a bare constructor gets no caret of its own: only the annotation is
/// labelled.
///
/// This pins what the checker does today, not what it should do. `constraint::collect`'s
/// `Identifier` arm adds no constraint, so nothing is blamed on `True` and the mismatch
/// surfaces only through the annotation's constraint. `ERR-17` is the ticket that would
/// give the body a label; when it lands this test is meant to go red and be rewritten.
///
/// Mutation-checked by giving the body of `answer` a literal (`'a'`) instead: the primary
/// label moves onto it and the single-label assertion fails.
#[test]
fn a_mistyped_bare_constructor_body_is_blamed_only_through_the_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer : Int
        answer = True
    "#};
    let error = one_type_error(source);

    assert_eq!(
        ranges(&error.labels()),
        vec![range_of(source, "answer : Int")],
        "expected only the annotation to be labelled"
    );
}

// ── Where a type error points ─────────────────────────────────────────────────

/// `ERR-4`'s worked example: the caret goes under `'a'`, and `Int` is explained by
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
/// `infer_annotated` (the primary lands on the `if` rather than on `'a'`); and
/// making `Substitution::apply` keep the constraint's origin unchanged (the secondary
/// label disappears).
#[test]
fn annotation_mismatch_points_at_the_expression_and_the_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer : Int
        answer = if True then 1 else 'a'
    "#};
    let error = one_type_error(source);

    let labels = error.labels();
    assert_eq!(
        ranges(&labels),
        vec![range_of(source, "'a'"), range_of(source, "answer : Int")],
        "expected a caret under `'a'` and the annotation behind it"
    );
    assert!(labels[0].primary, "`'a'` is what has to change");
    assert!(
        !labels[1].primary,
        "the annotation is context, not the error"
    );

    assert_eq!(error.message(), "cannot match `Int` with `Char`");
    assert!(
        error
            .notes()
            .iter()
            .any(|n| n.contains("declaration of `answer`")),
        "the declaration should still be named for a reader with no carets, got {:?}",
        error.notes()
    );
}

/// An argument of the wrong type is explained by the *function*, not by whatever
/// annotation happens to be in scope.
///
/// `42` has to be a `Bool` because `not : Bool -> Bool`. The annotation on `result`
/// is not load-bearing for it at all: change it to `Char` and the argument still has
/// to be a `Bool`. Pointing a reader at `result : Bool` sends them to edit a line
/// that will not help.
///
/// The proof that the annotation is not the answer is the same source without one —
/// `unannotated_application_mismatch_blames_the_function_too` below — which has
/// nothing else it could possibly name. The two must agree, and before the side
/// tracking in `Origin` they did not: the annotated form walked the chain of
/// substitutions one link too far and blamed the annotation.
///
/// Mutation-checked by making `Origin::cause_of` ignore the side it is given and
/// return `self.explanation()` whichever it was: the secondary label moves back onto
/// `result : Bool`, and it is the only test in the file that notices.
#[test]
fn application_mismatch_blames_the_function_not_the_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        not : Bool -> Bool
        not b = b
        result : Bool
        result = not 42
    "#};
    let error = one_type_error(source);

    let labels = error.labels();
    assert_eq!(
        ranges(&labels),
        vec![
            range_of(source, "42"),
            range_within(source, "result = not 42", "not")
        ],
        "expected a caret under `42` and the applied function behind it"
    );
    assert_eq!(
        labels[1].message, "expected because of this application",
        "the reason `Bool` was expected is `not`'s own type"
    );
}

/// The same shape with no annotation to be tempted by, which is what fixes the
/// expected answer for the test above: there is exactly one place `Bool` can have
/// come from, and it is `not`.
#[test]
fn unannotated_application_mismatch_blames_the_function_too() {
    let source = indoc::indoc! {r#"
        module Test exposing (not)
        not : Bool -> Bool
        not b = b
        result = not 42
    "#};
    let error = one_type_error(source);

    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "42"),
            range_within(source, "result = not 42", "not")
        ]
    );
}

/// A `case` whose scrutinee is a compound expression reports the mismatch inside that
/// expression, explained by the pattern that required the type.
///
/// This is what the ordering rule in `constraint::collect`'s doc buys, and the `Case`
/// arm used to be the one site that broke it by collecting the scrutinee's
/// constraints before its own. Solved in that order, the `if`'s inner branch and
/// literal constraints settle the scrutinee's type first, and the failure surfaces on
/// the `Red` pattern with a secondary caret pointing into the middle of the `if` —
/// the compiler's working rather than the user's mistake.
///
/// Mutation-checked by moving `collect(scrutinee)` back above the branch loop: the
/// two labels swap round.
#[test]
fn case_pattern_mismatch_points_into_the_scrutinee() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Color = Red | Blue
        bad : Int
        bad =
          case if True then 1 else 2 of
            Red -> 1
            Blue -> 2
    "#};
    let error = one_type_error(source);

    let labels = error.labels();
    assert_eq!(
        ranges(&labels),
        vec![
            range_within(source, "if True then 1 else 2", "1"),
            range_within(source, "Red -> 1", "Red")
        ],
        "expected a caret in the scrutinee and the pattern that required its type"
    );
    assert_eq!(labels[1].message, "expected because of this pattern");
}

// ── Unbound variable ──────────────────────────────────────────────────────────

/// A name that is not in scope is caught by *canonicalization*, not by the typer,
/// and this test asserts the error the user actually gets.
///
/// The typer has an `UnboundVariable` of its own and never reports it: its
/// environment is built from one module, so a perfectly valid cross-module reference
/// is missing from it, and `type_check` skips the variant rather than blaming the
/// source (its doc comment sets out both cases). What that leaves is
/// `canonical::Error::VariableNotFound`, one phase earlier, with the caret under the
/// name — so that is what there is to pin.
///
/// Asserting the phase and the caret rather than `is_err()` is the point. The old
/// form of this test conceded in a comment that canonicalization was what caught
/// this, then asserted something that could not tell the two phases apart; it stayed
/// green with `typer::type_check` deleted outright, which is exactly what `run` being
/// a whole-pipeline helper makes easy to do by accident.
#[test]
fn unbound_variable_is_a_canonicalization_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        oops : Int
        oops = nonExistentBinding
    "#};
    let error = match run(source) {
        Ok(_) => panic!("expected an error, but the module checked"),
        Err(CompilationError::Canonical(mut errors, _)) => {
            assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);
            errors.remove(0)
        }
        Err(other) => panic!("expected a canonicalization error, got {:?}", other),
    };

    // Qualified with the module the name was written in — canonicalization has
    // already resolved it against `Test`'s own environment by the time it fails.
    assert_eq!(
        error.message(),
        "cannot find a value named `Test.nonExistentBinding`"
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_of(source, "nonExistentBinding")],
        "the caret belongs under the name, not across the declaration"
    );
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

/// `BUG-17`'s acceptance case, the positive half: `Maybe Int`'s written argument
/// now survives canonicalization, so a body that agrees with it — `Just 1`, an
/// `Int` — still type-checks. Layer 1 (`crates/zelkova-compiler/tests/canonical.rs`) can only see
/// that the argument survives; this layer is what can tell that the annotation
/// actually constrains, because unification is what would reject a disagreement.
#[test]
fn type_application_argument_accepts_an_agreeing_body() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a = Just a | Nothing
        f : Maybe Int
        f = Just 1
    "#};
    assert!(run(source).is_ok(), "Just 1 : Maybe Int should type-check");
}

/// `BUG-17`'s acceptance case, the negative half: before the fix, `Maybe Int`
/// canonicalized to `Maybe a` — the declaration's own type variable, not `Int` —
/// so this module type-checked with no error at all (verified by probing the
/// pre-fix tree). `Just 'c'` disagreeing with the `Int` actually written is what a
/// green run of `type_application_argument_accepts_an_agreeing_body` above cannot
/// distinguish from "the annotation was ignored entirely" — this is the test that
/// can.
///
/// Mutation-checked by reverting `Type::from_parser_type`'s `Some` arm to return
/// the environment's stored type verbatim instead of applying `args`: this test
/// goes green as `run(source).is_ok()` (no error), same as the pre-fix probe.
#[test]
fn type_application_argument_rejects_a_disagreeing_body() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Maybe a = Just a | Nothing
        f : Maybe Int
        f = Just 'c'
    "#};
    let error = one_type_error(source);

    assert_eq!(
        error.message(),
        "cannot match `Int` with `Char`",
        "the annotation's `Int` argument should be what the body's `Char` disagrees with"
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_of(source, "'c'"), range_of(source, "f : Maybe Int")]
    );
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
            Nothing -> True
    "#};
    let error = one_type_error(source);

    // The `Nothing` branch is the one that disagrees with `Int`; `Just x -> x` does
    // not, and underlining the whole `case` would be underlining both.
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "True"),
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
        max a b = if True then a else b
    "#};
    assert!(
        run(source).is_ok(),
        "if/then/else with matching types should type-check"
    );
}

/// An `if` condition is a `Basics.Bool`, which is what an annotation naming `Bool`
/// resolves to once `Basics` is in scope.
///
/// `if_expression_types` above reaches the same constraint from the constructor `True`; this
/// one comes at it from the annotation, so the two sides of the `Bool` question — the
/// type the typer produces on its own and the type a source spells — are both pinned.
///
/// Mutation-checked by giving `scalar_literal` back a `scalars::BOOL` row: the
/// annotation is then a literal `Bool` and the condition an `Adt`, and the module
/// fails with *cannot match `Bool` with `Bool`*.
#[test]
fn if_condition_may_be_an_annotated_bool() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        pick : Bool -> Int -> Int -> Int
        pick c a b = if c then a else b
    "#};

    assert!(run(source).is_ok(), "{:?}", run(source).err());
}

/// A module's own `type Bool` is not the `Bool` an `if` asks for.
///
/// `Bool` is a scalar, and a scalar is `Basics`' declaration and no other
/// (`DEC-15` decisions 1 and 5), so the condition of an `if` is `Basics.Bool`
/// whatever else a module chooses to call `Bool`. The message names the module on
/// both sides, because one word for two declarations would say nothing.
///
/// Mutation-checked by comparing the two names' unqualified halves in the unifier's
/// `Adt`/`Adt` arm (`n1.unqualified_name() == n2.unqualified_name()`): the two
/// `Bool`s then unify, the module checks clean, and `one_type_error` panics.
#[test]
fn if_condition_is_not_a_modules_own_bool() {
    let source = indoc::indoc! {r#"
        module Test exposing (Bool, pick)

        type Bool
          = True
          | False

        pick : Bool -> Int -> Int -> Int
        pick c a b =
          if c then a else b
    "#};

    let error = one_type_error(source);

    assert!(
        matches!(error.kind, typer::ErrorKind::UnificationFailed { .. }),
        "expected a unification failure, got {:?}",
        error
    );
    assert_eq!(
        error.message(),
        "cannot match `Test.Bool` with `Basics.Bool`"
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

// ── String literals ───────────────────────────────────────────────────────────

/// An unannotated binding to a string literal infers `String`, and its body is the
/// literal's value.
///
/// Verified by constraining `TypedTermKind::String` to `TypeLiteral::Char` in
/// `constraint::collect`: the type assertion goes red, reading `Char`.
#[test]
fn string_literal_infers_string() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        greeting = "hello"
    "#};

    let solved = solved(source);
    let term = typed_declaration(&solved, "greeting");

    assert_eq!(format!("{}", term.tpe), "String");
    assert!(
        matches!(&term.kind, TypedTermKind::String(value) if value == "hello"),
        "got {:?}",
        term.kind
    );
}

/// `String` in an annotation is the type a string literal has, so the two agree.
///
/// Verified by removing `STRING`'s entry from `scalar_literal`: the annotation then
/// reads as the union `String.String` and the literal's `String` does not match it.
#[test]
fn string_literal_agrees_with_a_string_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        greeting : String
        greeting = "hello"
    "#};
    let interfaces = HashMap::from([basics_interface(), char_interface(), string_interface()]);

    let result = check_module(&test_package(), &interfaces, &parse_source(source));

    assert!(result.is_ok(), "got {:?}", result.err());
}

/// A string literal where an `Int` is expected is a mismatch naming `String`.
#[test]
fn string_type_mismatch() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : Int
        bad = "x"
    "#};
    let error = one_type_error(source);

    assert_eq!(error.message(), "cannot match `Int` with `String`");
    assert_eq!(
        ranges(&error.labels()),
        vec![range_of(source, "\"x\""), range_of(source, "bad : Int")]
    );
}

// ── Tuple types and expressions ───────────────────────────────────────────────

/// A pair `(Int, Bool)` should type-check.
#[test]
fn tuple_pair_typechecks() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        pair : (Int, Bool)
        pair = (42, True)
    "#};
    assert!(run(source).is_ok(), "(Int, Bool) tuple should type-check");
}

/// Using `(Int, Char)` where `(Int, Int)` is expected should fail.
#[test]
fn tuple_type_mismatch() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : (Int, Int)
        bad = (42, 'a')
    "#};
    let error = one_type_error(source);

    // Not the whole tuple: the first element agrees with the annotation and only the
    // second does not.
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "'a'"),
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
        triple = (42, True, 'a')
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
        bad = (42, True, 7)
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
        bad = (42, True)
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
        bad = (42, True, 'a')
    "#};
    // `type_errors` rather than `is_err`: these are about which `unify` arm the two
    // arities reach, so a failure raised by an earlier phase would not exercise them.
    assert_eq!(type_errors(source).len(), 1);
}

// ── A module's own type spelled like a scalar ─────────────────────────────────

/// `BUG-26`'s module: declaring `Bool` and annotating with it type checks.
///
/// A scalar is known by the qualified name of its declaration, so this `Bool` is
/// `Test.Bool` — an ordinary union, the same type its constructors `True` and
/// `False` are registered at.
///
/// Mutation-checked by restoring both halves of a literal `Bool` — a
/// `TypeLiteral::Bool` with a `scalar_literal` row, *and* the match on the
/// unqualified half in `scalars::Scalar::declares` — so this `Bool` is taken for the
/// scalar: the module then fails with "cannot match `Bool` with `Bool`".
#[test]
fn a_module_declaring_its_own_bool_annotates_with_it() {
    let source = indoc::indoc! {r#"
        module Test exposing (Bool, not)

        type Bool
          = True
          | False

        not : Bool -> Bool
        not b =
          case b of
            True ->
              False

            False ->
              True
    "#};

    assert!(run(source).is_ok(), "{:?}", run(source).err());
}

/// The negative half: the same module's `case` still rejects a branch of the wrong
/// type, so the positive half above is not passing because the annotation stopped
/// constraining anything.
///
/// An integer literal is still `number` until `LANG-41`, which is the type the
/// message names.
///
/// Mutation-checked by the same restoration: the annotation is then the literal
/// `Bool`, which the `True` pattern already fails to match, so the one error is no
/// longer about the branch.
#[test]
fn a_module_declaring_its_own_bool_still_rejects_a_wrong_branch() {
    let source = indoc::indoc! {r#"
        module Test exposing (Bool, not)

        type Bool
          = True
          | False

        not : Bool -> Bool
        not b =
          case b of
            True ->
              False

            False ->
              1
    "#};

    assert_eq!(
        one_type_error(source).message(),
        "cannot match `Bool` with `number`"
    );
}

/// A module's own `Int` and `Basics`' `Int` are two types: an integer literal can
/// be the second (it is `number` until `LANG-41`) and is never the first.
///
/// Mutation-checked by restoring the match on the unqualified half in
/// `scalars::Scalar::declares`: the local `Int` becomes the literal `Int` and the
/// module checks clean.
#[test]
fn a_module_declaring_its_own_int_does_not_get_the_scalar() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Int = MkInt
        answer : Int
        answer = 42
    "#};

    assert_eq!(
        one_type_error(source).message(),
        "cannot match `Int` with `number`"
    );
}

// ── `Bool` inside `Basics` ────────────────────────────────────────────────────

/// `Basics`' `Bool` is the union `True` and `False` build, and an annotation naming
/// it is that same union.
///
/// `Bool` is a scalar *and* an ordinary union (`DEC-15` decision 5): the compiler
/// knows its representation and nothing about its structure, so every use of it goes
/// through the ordinary union machinery — the annotation, the constructors it
/// registers, and a `case` over them. `Basics` is the module where that has to hold,
/// since it is the only one that both declares the type and can name its
/// constructors.
///
/// Mutation-checked by giving `scalar_literal` back a `scalars::BOOL` row: `yes` then
/// fails with *cannot match `Bool` with `Bool`*.
#[test]
fn basics_own_bool_is_the_union_its_constructors_build() {
    let source = indoc::indoc! {r#"
        module Basics exposing (Bool, yes, not)

        type Bool
          = True
          | False

        yes : Bool
        yes = True

        not : Bool -> Bool
        not b =
          case b of
            True ->
              False

            False ->
              True
    "#};

    assert!(run(source).is_ok(), "{:?}", run(source).err());
}

/// The negative half: `Basics`' `Bool` still rejects a value of another type, so the
/// test above is not passing because the annotation stopped constraining anything.
///
/// An integer literal is still `number` until `LANG-41`, which is the type the
/// message names.
#[test]
fn basics_own_bool_still_rejects_a_wrong_value() {
    let source = indoc::indoc! {r#"
        module Basics exposing (Bool, yes)

        type Bool
          = True
          | False

        yes : Bool
        yes = 1
    "#};

    assert_eq!(
        one_type_error(source).message(),
        "cannot match `Bool` with `number`"
    );
}

// ── What the typer hands back ─────────────────────────────────────────────────

/// A declaration the typer checked comes back with the type it solved, and so does
/// every node inside it.
///
/// The interior is the half that matters. `annotate` gives each node a fresh inference
/// variable and unification solves those somewhere else entirely, so a term whose root
/// alone had the substitution applied would answer `Bool -> Char` here and `t14` for
/// the `if` — enough to say what a declaration's type is, and not enough to generate
/// code from.
///
/// Mutation-checked by having `infer_annotated` rebuild the root alone —
/// `TypedTerm { tpe: substitution.apply_type(&typed_term.tpe), ..typed_term }` — instead
/// of calling `Substitution::apply_term`: the declaration's own type still passes and
/// both interior assertions go red, naming a `t`-number.
#[test]
fn a_typed_declaration_carries_the_types_of_its_interior() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        pick : Bool -> Char
        pick c = if c then 'a' else 'b'
    "#};

    let solved = solved(source);
    let term = typed_declaration(&solved, "pick");

    assert_eq!(format!("{}", term.tpe), "Bool -> Char");

    // The body of the function is the `if`, whose own type is the `Char` its two
    // branches agree on — a type the annotation never spells at that node.
    let body = match &term.kind {
        TypedTermKind::Fun { body, .. } => body,
        other => panic!("expected a function, got {:?}", other),
    };
    assert_eq!(format!("{}", body.tpe), "Char");

    // And one level deeper again: the condition is the parameter, which the annotation
    // says is a `Bool`.
    let cond = match &body.kind {
        TypedTermKind::If { cond, .. } => cond,
        other => panic!("expected an `if`, got {:?}", other),
    };
    assert_eq!(format!("{}", cond.tpe), "Bool");
}

/// A declaration whose body reaches a name the typer's environment does not hold is
/// present in what comes back, marked as un-typed.
///
/// `helper` carries no annotation, so the typer's first pass — which registers the
/// module's *annotated* values — never puts it in the environment, and `answer`'s body
/// cannot be inferred. Neither is a mistake in the source, so neither is an error; what
/// it may not be is missing, because a backend handed a map without `answer` in it
/// cannot tell that from a declaration that checked.
///
/// `helper` itself is asserted typed, so this cannot pass by the whole map being empty.
///
/// Mutation-checked by restoring the bare `continue` on the
/// `Err(ErrorKind::UnboundVariable { .. })` arm of `type_check`: `answer` goes missing
/// and the lookup panics.
#[test]
fn a_declaration_the_typer_cannot_resolve_comes_back_marked() {
    let source = indoc::indoc! {r#"
        module Test exposing (answer)
        helper = 1
        answer : Int
        answer = helper
    "#};

    let solved = solved(source);

    match solved.get(&Name::new("answer")) {
        Some(Solved::UnboundName { name, .. }) => {
            assert_eq!(name, "test-project:Test.helper")
        }
        other => panic!("expected `answer` to be marked un-typed, got {:?}", other),
    }

    assert_eq!(
        format!("{}", typed_declaration(&solved, "helper").tpe),
        "number"
    );
}

/// The other skip: a declaration the term language cannot express is present too, and
/// says so.
///
/// A pattern nested inside a tuple pattern is one `translate_pattern` refuses, and a
/// parameter's pattern goes through it as a `case` branch's does, so nothing about
/// `unwrap` is checked — including its annotation.
///
/// The span is asserted because the warning `ERR-8` will make of this needs a caret,
/// and the whole declaration is the only position available: which construct stopped
/// the translation does not come back.
///
/// Mutation-checked twice: restoring the bare `continue` on the `else` branch of
/// `value_to_term_and_annotation` in `type_check` makes `unwrap` go missing and the
/// match falls through to the panic; handing `NodeSpan::none()` to the variant instead
/// of `value.span()` leaves the first assertion passing and turns the range red.
#[test]
fn a_declaration_the_typer_cannot_translate_comes_back_marked() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        unwrap : ((Int, Int), Int) -> Int
        unwrap ((n, m), k) = n
    "#};

    let solved = solved(source);

    let span = match solved.get(&Name::new("unwrap")) {
        Some(Solved::Untranslatable { span }) => *span,
        other => panic!(
            "expected `unwrap` to be marked untranslatable, got {:?}",
            other
        ),
    };

    // `NodeSpan`'s `PartialEq` always answers `true`, so the range is what proves the
    // position: the annotation merged with the binding under it.
    assert_eq!(
        span.to_range(),
        Some(range_of(
            source,
            "unwrap : ((Int, Int), Int) -> Int\nunwrap ((n, m), k) = n"
        ))
    );
}

/// A module with one ill-typed declaration still answers for every declaration:
/// `type_check_recovering` hands back the well-typed one's term beside the error, and
/// marks the ill-typed one rejected rather than leaving it out.
///
/// Mutation-checked by making `type_check_recovering` return an empty `solved` when it
/// has errors: `ok` is then missing and its assertion goes red. Dropping the
/// `Solved::Rejected` insert instead turns the `bad` assertion red.
#[test]
fn a_module_with_a_type_error_still_answers_for_every_declaration() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        ok : Int
        ok = 1
        bad : Int
        bad = 'a'
    "#};

    let interfaces = HashMap::from([basics_interface(), char_interface()]);
    let canonical = canonicalize_with_interfaces(source, &interfaces)
        .unwrap_or_else(|errors| panic!("expected the module to canonicalize, got {:?}", errors));

    let typer::TypeCheck { solved, errors } = typer::type_check_recovering(&canonical, &interfaces);

    match solved.get(&Name::new("ok")) {
        Some(Solved::Typed(term)) => assert_eq!(format!("{}", term.tpe), "Int"),
        other => panic!("expected `ok` to be typed, got {:?}", other),
    }
    assert!(
        matches!(solved.get(&Name::new("bad")), Some(Solved::Rejected)),
        "expected `bad` to be marked rejected, got {:?}",
        solved.get(&Name::new("bad"))
    );

    assert_eq!(errors.len(), 1, "got {:?}", errors);
    assert_eq!(errors[0].declaration, Name::new("bad"));
}

// ── Patterns in parameters ────────────────────────────────────────────────────

/// A parameter written as a tuple pattern is typed, and its type is read off the
/// pattern: `first` is never annotated, so the tuple it takes and the element it
/// returns are everything inference had to go on.
///
/// Mutation-checked by restoring `wrap_with_patterns`'s `_ => None` for every pattern
/// other than a variable or `_`: `first` comes back `Untranslatable` and the lookup
/// panics.
#[test]
fn a_tuple_pattern_parameter_is_typed() {
    let source = indoc::indoc! {r#"
        module Test exposing (answer)
        answer : Int
        answer = 1
        first (x, _) = x
    "#};

    let solved = solved(source);

    // The variables' numbering is inference's business, so the shape is what is
    // compared: a pair, whose first element is what comes back.
    let rendered = format!("{}", typed_declaration(&solved, "first").tpe);
    let shape = rendered
        .strip_prefix("( ")
        .and_then(|rest| rest.split_once(", "))
        .and_then(|(first, rest)| rest.split_once(" ) -> ").map(|(_, back)| (first, back)));
    match shape {
        Some((first, back)) => assert_eq!(
            first, back,
            "`first` returns the first element: {}",
            rendered
        ),
        None => panic!("expected `( a, b ) -> a`, got {}", rendered),
    }
}

/// A tuple pattern heading a `case` branch is typed the way one in a parameter is: the
/// translation is `translate_pattern`'s, which both positions share.
///
/// Mutation-checked by removing `translate_pattern`'s `PatternKind::Tuple` arm, so a
/// tuple pattern falls through to `None` again: `swap` comes back `Untranslatable` and
/// the lookup panics.
#[test]
fn a_tuple_pattern_in_a_case_is_typed() {
    let source = indoc::indoc! {r#"
        module Test exposing (swap)
        swap : (Int, Char) -> (Char, Int)
        swap pair =
          case pair of
            (a, b) -> (b, a)
    "#};

    let solved = solved(source);

    assert_eq!(
        format!("{}", typed_declaration(&solved, "swap").tpe),
        "( Int, Char ) -> ( Char, Int )"
    );
}

/// A parameter written as a constructor pattern over a union of the same module is
/// typed — `Basics.never`'s shape, where the union is recursive and the function calls
/// itself on what the pattern bound.
///
/// Mutation-checked by restoring `wrap_with_patterns`'s `_ => None`: `never` comes back
/// `Untranslatable` and the lookup panics.
#[test]
fn a_constructor_pattern_parameter_is_typed() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Never = JustOneMore Never
        never : Never -> a
        never (JustOneMore nvr) = never nvr
    "#};

    let solved = solved(source);

    let rendered = format!("{}", typed_declaration(&solved, "never").tpe);
    assert!(
        rendered.starts_with("Never -> t"),
        "expected `Never` to anything, got {}",
        rendered
    );
}

/// A parameter's pattern is checked against the type the annotation gives that
/// parameter, and the mismatch is reported as the parameter's: the caret is under the
/// pattern, and the rule the note states is about a parameter, not about a `case` the
/// source never wrote.
///
/// Mutation-checked by handing `constraint::collect` `Reason::CasePattern` for a
/// `CaseForm::Parameter` match as well: the note then speaks of a `case` and the last
/// assertion goes red.
#[test]
fn a_parameter_pattern_that_does_not_match_its_annotation_is_a_type_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        bad : Int -> Int
        bad (x, y) = x
    "#};
    let error = one_type_error(source);

    let labels = error.labels();
    assert_eq!(
        ranges(&labels).first(),
        Some(&range_of(source, "(x, y)")),
        "expected the caret under the pattern, got {:?}",
        labels
    );
    assert_eq!(labels[0].message, "this pattern");
    assert!(
        error
            .notes()
            .iter()
            .any(|n| n == "a parameter's pattern must match the type of the argument it takes"),
        "the note should state the parameter's rule, got {:?}",
        error.notes()
    );
}

/// A body of the wrong type under a patterned parameter is blamed on the body, and
/// reported as the declaration's: the pattern matches its argument, so neither the
/// caret nor the note may say it does not, and there is no `case` in the source for
/// either to name.
///
/// The tuple is the shape that used to misblame: its elements are fresh variables that
/// only the pattern's constraint ties to the argument, so a branch constraint solved
/// before it settled them from the declared result instead. The constructor has
/// concrete arguments and was always blamed on the body, but as a `case` branch.
///
/// Mutation-checked two ways, each red on its own: pushing the branch constraint in
/// `constraint::collect` before the pattern's (the tuple's caret moves onto `(x, _)`),
/// and handing it `Reason::CaseBranch` for a `CaseForm::Parameter` match as well (the
/// label says "this branch of the `case`").
#[test]
fn a_parameter_patterns_body_of_the_wrong_type_is_blamed_on_the_body() {
    let sources = [
        (
            indoc::indoc! {r#"
                module Test exposing (..)
                f : (Int, Int) -> Char
                f (x, _) = x
            "#},
            "f : (Int, Int) -> Char",
        ),
        (
            indoc::indoc! {r#"
                module Test exposing (..)
                type W = W Int
                f : W -> Char
                f (W x) = x
            "#},
            "f : W -> Char",
        ),
    ];

    for (source, annotation) in sources {
        let error = one_type_error(source);
        let labels = error.labels();

        assert_eq!(
            ranges(&labels),
            vec![
                range_within(source, ") = x", "x"),
                range_of(source, annotation)
            ],
            "expected the caret under the body and the annotation behind it in {}",
            source
        );
        assert_eq!(labels[0].message, "the body of this declaration");
        assert!(
            error
                .notes()
                .iter()
                .any(|n| n == "a declaration's body must have the type its annotation declares"),
            "the note should state the body's rule, got {:?}",
            error.notes()
        );
        assert!(
            !labels.iter().any(|l| l.message.contains("case"))
                && !error.notes().iter().any(|n| n.contains("case")),
            "the source wrote no `case`, got {:?} and {:?}",
            labels,
            error.notes()
        );
    }
}

/// A `case` branch of the wrong type under a tuple pattern is blamed on the branch.
/// The pattern matches the scrutinee; what disagrees with the annotation is `x`.
///
/// Mutation-checked by pushing the branch constraint in `constraint::collect` before
/// the pattern's: the tuple's element variables are then settled from the annotation's
/// `Char`, and the caret moves onto `(x, _)` with the pattern's note.
#[test]
fn a_case_branch_of_the_wrong_type_is_blamed_on_the_branch_not_its_pattern() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        f : (Int, Int) -> Char
        f p =
          case p of
            (x, _) -> x
    "#};
    let error = one_type_error(source);

    let labels = error.labels();
    assert_eq!(
        ranges(&labels),
        vec![
            range_within(source, "-> x", "x"),
            range_of(source, "f : (Int, Int) -> Char")
        ],
        "expected the caret under the branch and the annotation behind it, got {:?}",
        labels
    );
    assert_eq!(labels[0].message, "this branch of the `case`");
}

/// A patterned parameter after a plain one, and a plain one after it, are each bound
/// where they were written: `pick`'s body reads the element its second parameter bound
/// and the parameter after that, and the annotation holds only if each is the type its
/// position says.
///
/// Mutation-checked by nesting each parameter's match directly inside its own `Fun`
/// rather than inside all of them: the declaration's type is unchanged, but the typed
/// term's first three nodes are no longer the three parameters, and the depth
/// assertion goes red.
#[test]
fn a_pattern_parameter_between_plain_ones_is_typed() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        pick : Int -> (Char, Int) -> Char -> (Int, Char)
        pick n (c, _) d = (n, c)
    "#};

    let solved = solved(source);
    let pick = typed_declaration(&solved, "pick");

    assert_eq!(
        format!("{}", pick.tpe),
        "Int -> ( Char, Int ) -> Char -> ( Int, Char )"
    );

    let mut depth = 0;
    let mut term = pick;
    while let TypedTermKind::Fun { body, .. } = &term.kind {
        depth += 1;
        term = body;
    }
    assert_eq!(
        depth, 3,
        "one `Fun` per parameter, outside every match: {:?}",
        pick
    );
    assert!(
        matches!(term.kind, TypedTermKind::Case { .. }),
        "inside the parameters is the match on the patterned one, got {:?}",
        term
    );
}

/// Every declaration of a `module foreign` facade comes back, each saying it has no
/// body.
///
/// A facade declares signatures and nothing else, and canonicalization gives each one a
/// synthetic placeholder body, so there is nothing for inference to do — but the
/// entries still have to be there. An empty map would read, to whatever consumes it, as
/// a facade that declares nothing, which is exactly the confusion [`Solved`] exists to
/// prevent: `ir::build` emits a declaration per facade signature.
///
/// Mutation-checked by returning `Ok(HashMap::new())` from `type_check`'s
/// `binding_foreign` branch: the count and both lookups go red. `cargo run` and
/// `stdlib_package_compiles` both stay green under that same change, which is why this
/// test is here — they walk `Js.Bitwise` but read only whether the pass errored.
#[test]
fn a_facade_declaration_comes_back_with_no_body() {
    let source = indoc::indoc! {r#"
        module foreign Test exposing (and, complement)

        unsafe and : Int -> Int -> Int
        unsafe complement : Int -> Int
    "#};

    let solved = solved(source);

    assert_eq!(solved.len(), 2, "got {:?}", solved);
    for name in ["and", "complement"] {
        match solved.get(&Name::new(name)) {
            Some(Solved::NoBody) => (),
            other => panic!("expected `{}` to have no body, got {:?}", name, other),
        }
    }
}

/// An integer literal larger than a `u32` survives translation with its value.
///
/// [`Int` is 64 bits](../docs/spec/evaluation-semantics.md#numbers) (`DEC-16`), and the
/// typed term is what code is generated from, so a literal that arrives at the backend
/// narrowed is a program that computes a different number than the one written.
/// Inference cannot notice: every integer literal is a `number` whatever its value, so
/// the module type checks either way.
///
/// Mutation-checked by putting the old truncation back in `canonical_expr_to_term`
/// (`TermKind::Int(*i as u32 as i64)`, the widened variant's spelling of what
/// `TermKind::Int(*i as u32)` did): the literal comes back as 705032704.
#[test]
fn an_int_literal_wider_than_u32_keeps_its_value() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        big : Int
        big = 5000000000
    "#};

    let solved = solved(source);

    match &typed_declaration(&solved, "big").kind {
        TypedTermKind::Int(value) => assert_eq!(*value, 5_000_000_000),
        other => panic!("expected an integer literal, got {:?}", other),
    }
}

// ── The unit type (LANG-72) ───────────────────────────────────────────────────

/// `()` has the unit type, annotated as in [the chapter's own
/// example](../docs/spec/types.md#the-unit-type) or not. The unannotated
/// declaration is the one that pins the value's own constraint: under an annotation
/// the body's fresh variable would be solved to `()` by the annotation alone.
///
/// Mutation-checked by deleting the constraint `constraint::collect` pushes for
/// `TypedTermKind::Unit`: `unit` then renders as an unsolved variable.
#[test]
fn unit_value_has_the_unit_type() {
    let source = indoc::indoc! {r#"
        module Test exposing (nothingUseful)
        nothingUseful : ()
        nothingUseful = ()
        unit = ()
    "#};

    let solved = solved(source);

    let annotated = typed_declaration(&solved, "nothingUseful");
    assert_eq!(format!("{}", annotated.tpe), "()");
    assert!(
        matches!(annotated.kind, TypedTermKind::Unit),
        "expected the unit value, got {:?}",
        annotated.kind
    );
    assert_eq!(format!("{}", typed_declaration(&solved, "unit").tpe), "()");
}

/// A parameter written `()` is typed, as in [the patterns chapter's
/// example](../docs/spec/patterns.md#the-unit-pattern). Without an annotation, the
/// pattern is the only thing saying what the parameter's type is.
///
/// Mutation-checked by making `pattern_constraints` place no constraint for a
/// `TermPatternKind::Unit`: `ignore` then renders with an unsolved parameter.
#[test]
fn a_unit_pattern_parameter_is_typed() {
    let source = indoc::indoc! {r#"
        module Test exposing (Flag, always)
        type Flag
          = On
          | Off
        always : () -> Flag
        always () =
          On
        ignore () =
          Off
    "#};

    let solved = solved(source);

    assert_eq!(
        format!("{}", typed_declaration(&solved, "always").tpe),
        "() -> Flag"
    );
    assert_eq!(
        format!("{}", typed_declaration(&solved, "ignore").tpe),
        "() -> Flag"
    );
}

/// `()` where an `Int` is expected is a type error, and the caret sits under the
/// `()` with the annotation behind it.
///
/// Mutation-checked two ways, each red on its own: deleting the constraint
/// `constraint::collect` pushes for `TypedTermKind::Unit` (the module then checks),
/// and giving that constraint `NodeSpan::none()` instead of the term's span (the
/// primary label falls back to the whole declaration).
#[test]
fn unit_where_an_int_is_expected_is_a_type_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (x)
        x : Int
        x = ()
    "#};
    let error = one_type_error(source);

    assert_eq!(error.message(), "cannot match `Int` with `()`");

    let labels = error.labels();
    assert_eq!(
        ranges(&labels),
        vec![range_of(source, "()"), range_of(source, "x : Int")],
        "expected a caret under `()` and the annotation behind it"
    );
    assert!(labels[0].primary, "`()` is what has to change");
    assert_eq!(labels[0].message, "this unit value");
}

/// A `()` pattern where the argument is an `Int` is the parameter's mismatch.
///
/// Mutation-checked by making `pattern_constraints` place no constraint for a
/// `TermPatternKind::Unit`: the module then checks.
#[test]
fn a_unit_pattern_against_an_int_is_a_type_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (bad)
        bad : Int -> Int
        bad () = 1
    "#};
    let error = one_type_error(source);

    let labels = error.labels();
    assert_eq!(
        ranges(&labels).first(),
        Some(&range_of(source, "()")),
        "expected the caret under the pattern, got {:?}",
        labels
    );
    assert_eq!(labels[0].message, "this pattern");
}

/// A `()` written as a tuple's element is typed, as `_` there is: `()` is irrefutable,
/// so `translate_sub_pattern` admits it below the top of a pattern. Without an
/// annotation, the nested `()` is the only thing saying what the second element's type
/// is.
///
/// Mutation-checked two ways, each red on its own: dropping `PatternKind::Unit` from
/// `translate_sub_pattern`'s admitted shapes (neither declaration is typed), and making
/// `pattern_constraints` place no constraint for a `TermPatternKind::Unit` (`second`
/// then renders with an unsolved element).
#[test]
fn a_nested_unit_pattern_is_typed() {
    let source = indoc::indoc! {r#"
        module Test exposing (first)
        first : (Int, ()) -> Int
        first (x, ()) = x
        second (c, ()) = 'a'
    "#};

    let solved = solved(source);

    assert_eq!(
        format!("{}", typed_declaration(&solved, "first").tpe),
        "( Int, () ) -> Int"
    );
    // `c`'s type is left free, and a free variable renders under its numeric id.
    let second = format!("{}", typed_declaration(&solved, "second").tpe);
    assert!(second.ends_with(", () ) -> Char"), "got {}", second);
}

/// A `()` written as a tuple's element, where that element is an `Int`, is a type error
/// with the caret under the nested `()` — reported by the typer, rather than the
/// declaration going untyped and surfacing only at code generation.
///
/// Mutation-checked two ways, each red on its own: dropping `PatternKind::Unit` from
/// `translate_sub_pattern`'s admitted shapes (the declaration is then skipped and no type
/// error is reported), and making `pattern_constraints` place no constraint for a
/// `TermPatternKind::Unit` (the module then checks).
#[test]
fn a_nested_unit_pattern_against_an_int_is_a_type_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (bad)
        bad : (Int, Int) -> Int
        bad (x, ()) = x
    "#};
    let error = one_type_error(source);
    // `()` also occurs inside `(x, ())`'s own parentheses, so the needle is the nested
    // `()` and the tuple's closing parenthesis after it.
    let nested = range_of(source, "())");
    let nested = nested.start..nested.start + 2;

    let labels = error.labels();
    assert_eq!(
        ranges(&labels).first(),
        Some(&nested),
        "expected the caret under the nested `()`, got {:?}",
        labels
    );
}

// ── `Task`, as `std/core/src/Task.zel` declares it ───────────────────────────

/// The interfaces a module outside `zelkova-core` sees when the real `Task.zel` is in
/// the build: `Basics`, `Char`, and `Task` itself, checked from `std/core`'s source
/// rather than the hand-built double `task_interface` supplies.
fn interfaces_with_task() -> HashMap<Name, zelkova_compiler::Interface> {
    let source = include_str!("../../../std/core/src/Task.zel");
    let core = zelkova_compiler::PackageName::new("zelkova-core").unwrap();
    let task = check_module(&core, &HashMap::new(), &parse_source(source))
        .unwrap_or_else(|error| panic!("expected Task.zel to check, got {:?}", error));

    let mut interfaces = HashMap::from([basics_interface(), char_interface()]);
    interfaces.insert(task.canonical.name.name().clone(), task.to_interface(None));
    interfaces
}

/// `source`, a module of an ordinary package, checked against [`interfaces_with_task`].
fn run_with_task(
    source: &str,
) -> Result<zelkova_compiler::CheckedModule, zelkova_compiler::CompilationError> {
    check_module(
        &test_package(),
        &interfaces_with_task(),
        &parse_source(source),
    )
}

/// `Task.succeed` and `Task.map` have the types the
/// [chapter](../docs/spec/evaluation-semantics.md#sequencing) writes, and a module with
/// no `import` reaches `Task` through the default imports.
///
/// Each annotation is the whole signature, so a `succeed` or `map` typed any more
/// loosely or differently (`map` with its arguments swapped, say) would not unify with it.
///
/// Mutation-checked by narrowing `map`'s annotation in `Task.zel` to
/// `(a -> a) -> Task a -> Task a`, which keeps `Task.zel` itself valid: the `mapped` line
/// below then fails to type. (Swapping `map`'s parameters instead breaks `Task.zel`, and
/// every test here goes red at load.)
#[test]
fn task_succeed_and_map_have_the_documented_signatures() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        one : Task Int
        one = Task.succeed 1

        wrap : Int -> Task Int
        wrap = Task.succeed

        mapped : (Int -> Bool) -> Task Int -> Task Bool
        mapped = Task.map
    "#};

    if let Err(error) = run_with_task(source) {
        panic!("expected the module to type check, got {:?}", error);
    }
}

/// A `Task Int` where a `Task Bool` is expected is a type error, so `Task`'s parameter
/// is a real one and not something the checker ignores.
///
/// Mutation-checked by making `unify_one_constraint`'s ADT arm compare no arguments
/// (`.filter(|_| false)` before its `collect`): `bools = ints` then checks.
#[test]
fn a_task_int_where_a_task_bool_is_expected_is_a_type_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        ints : Task Int
        ints = Task.succeed 1

        bools : Task Bool
        bools = ints
    "#};

    match run_with_task(source) {
        Err(CompilationError::Type(errors, _)) => {
            assert_eq!(errors.len(), 1, "expected one type error, got {:?}", errors)
        }
        other => panic!("expected a type error, got {:?}", other.map(|_| ())),
    }
}

/// Outside `zelkova-core`, neither `Task`'s constructor nor `Done` can be named: the
/// type is exposed opaquely, and `Done` not at all. Naming either is a canonicalization
/// error, not a type error, so the checker never sees the source.
///
/// Mutation-checked by exposing `Task(..)` in `Task.zel`'s header (the constructor line
/// then checks) and, separately, by adding `Done` to it (the `Done` line then checks).
#[test]
fn outside_core_the_task_constructor_and_done_cannot_be_named() {
    let constructor = indoc::indoc! {r#"
        module Test exposing (..)
        forged : Task Int
        forged = Task.Task
    "#};
    let done = indoc::indoc! {r#"
        module Test exposing (..)
        stop : Task.Done -> Int
        stop _ = 1
    "#};

    for source in [constructor, done] {
        match run_with_task(source) {
            Err(CompilationError::Canonical(_, _)) => {}
            other => panic!(
                "expected a canonicalization error, got {:?}",
                other.map(|_| ())
            ),
        }
    }
}
