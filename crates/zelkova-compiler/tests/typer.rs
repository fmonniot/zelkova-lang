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
/// The `1` in the true branch is deliberately not named anywhere: it is an `Int`,
/// which the declaration's annotation accepts, so only one of the two branches is
/// wrong and only one caret is right.
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
/// An integer literal is an `Int`, which is the type the message names.
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
        "cannot match `Bool` with `Int`"
    );
}

/// A module's own `Int` and `Basics`' `Int` are two types: an integer literal can
/// be the second and is never the first.
///
/// Mutation-checked by restoring the match on the unqualified half in
/// `scalars::Scalar::declares`: the local `Int` becomes the literal `Int` and the
/// module checks clean.
///
/// The message it pins is a known ambiguity, not the intended output: it names two
/// different types with one spelling, because `AdtNames::collide` never compares a
/// union against a scalar of the same name, so neither side is qualified.
/// `ERR-22` (`docs/tickets/err-22.md`) is the ticket that qualifies them; this
/// assertion changes with it.
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
        "cannot match `Int` with `Int`"
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
/// An integer literal is an `Int`, which is the type the message names.
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
        "cannot match `Bool` with `Int`"
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
        "Int"
    );
}

/// The other skip: a declaration the term language cannot express is present too, and
/// says so.
///
/// A float pattern is one `translate_pattern` refuses, and a parameter's pattern goes
/// through it as a `case` branch's does, so nothing about `unwrap` is checked —
/// including its annotation.
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
        unwrap : (Float, Int) -> Int
        unwrap (1.5, k) = k
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
            "unwrap : (Float, Int) -> Int\nunwrap (1.5, k) = k"
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

    let typer::TypeCheck { solved, errors, .. } =
        typer::type_check_recovering(&canonical, &interfaces);

    match solved.get(&Name::new("ok")) {
        Some(Solved::Typed { term, .. }) => assert_eq!(format!("{}", term.tpe), "Int"),
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
/// Inference cannot notice: every integer literal is an `Int` whatever its value, so
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

/// A `()` written as a tuple's element is typed, as `_` there is. Without an
/// annotation, the nested `()` is the only thing saying what the second element's type
/// is.
///
/// Mutation-checked two ways, each red on its own: making `translate_sub_pattern`
/// answer `None` for a `()` (neither declaration is typed), and making
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
/// Mutation-checked two ways, each red on its own: making `translate_sub_pattern`
/// answer `None` for a `()` (the declaration is then skipped and no type error is
/// reported), and making `pattern_constraints` place no constraint for a
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

// ── Nested constructor patterns ──────────────────────────────────────────────

/// The unions the nested-pattern tests below match on.
///
/// The module exposes only them, so that a declaration below can be left unannotated
/// and have its type inferred from its patterns alone.
const NESTED_UNIONS: &str = indoc::indoc! {r#"
    module Test exposing (Flag, Count, Shape, Boxed, Wrapper)
    type Flag = On | Off
    type Count = One | Many
    type Shape = Dot | Circle Count
    type Boxed = Boxed Count
    type Wrapper = Wrapper Shape
"#};

/// The first branch's pattern of the `case` that `term`'s body is, under however many
/// parameters it takes.
fn first_branch_pattern(term: &TypedTerm) -> &zelkova_compiler::ir::TermPattern {
    match &term.kind {
        TypedTermKind::Fun { body, .. } => first_branch_pattern(body),
        TypedTermKind::Case { branches, .. } => &branches[0].0,
        other => panic!("expected a `case` under the parameters, got {:?}", other),
    }
}

/// Every name `pattern` binds below its top, with the solved type of the position it is
/// bound at, in source order.
fn nested_bindings(pattern: &zelkova_compiler::ir::TermPattern) -> Vec<(String, String)> {
    use zelkova_compiler::ir::TermPatternKind;

    let subs: Vec<_> = match &pattern.kind {
        TermPatternKind::Constructor { args, .. } | TermPatternKind::Hole { args } => {
            args.iter().collect()
        }
        TermPatternKind::Tuple { elements } => elements.iter().collect(),
        _ => vec![],
    };
    subs.into_iter()
        .flat_map(|sub| match &sub.pattern.kind {
            TermPatternKind::Bind(name) => vec![(name.clone(), sub.tpe.to_string())],
            _ => nested_bindings(&sub.pattern),
        })
        .collect()
}

/// A `case` over a tuple with a constructor in each element: `On` and `(Circle n)` are
/// the only things saying what the two elements' types are, and `n` is bound at the
/// type `Circle` declares for its argument.
///
/// Mutation-checked two ways, each red on its own: restoring the refusal in
/// `translate_sub_pattern` (anything but a variable, `_` or `()` answers `None`), so
/// `pick` comes back untranslatable and `typed_declaration` panics; and making
/// `pattern_constraints` stop at the top of a pattern rather than recurse into its
/// sub-patterns, so nothing says what the elements are and the type comes out with two
/// unsolved variables.
#[test]
fn a_constructor_in_a_tuple_element_is_typed() {
    let source = format!(
        "{}{}",
        NESTED_UNIONS,
        indoc::indoc! {r#"
            pick pair =
              case pair of
                (On, (Circle n)) ->
                  n
        "#}
    );

    let solved = solved(&source);
    let pick = typed_declaration(&solved, "pick");

    assert_eq!(format!("{}", pick.tpe), "( Flag, Shape ) -> Count");
    assert_eq!(
        nested_bindings(first_branch_pattern(pick)),
        vec![("n".to_string(), "Count".to_string())]
    );
}

/// A `case` over a constructor holding an applied constructor: `Wrapper (Circle n)`
/// binds `n` two levels down, at `Circle`'s argument type, and `Wrapper` alone says what
/// the scrutinee is.
///
/// Mutation-checked by restoring the refusal in `translate_sub_pattern`: `inner` comes
/// back untranslatable, and `typed_declaration` panics.
#[test]
fn an_applied_constructor_in_a_constructor_argument_is_typed() {
    let source = format!(
        "{}{}",
        NESTED_UNIONS,
        indoc::indoc! {r#"
            inner w =
              case w of
                Wrapper (Circle n) ->
                  n
        "#}
    );

    let solved = solved(&source);
    let inner = typed_declaration(&solved, "inner");

    assert_eq!(format!("{}", inner.tpe), "Wrapper -> Count");
    assert_eq!(
        nested_bindings(first_branch_pattern(inner)),
        vec![("n".to_string(), "Count".to_string())]
    );
}

/// A nested constructor of a union other than the one its position holds is a type
/// error, with the caret under the nested pattern — its parentheses, name and argument,
/// the span the grammar gives it — rather than under `Wrapper` or the whole branch.
///
/// Mutation-checked two ways, each red on its own: restoring the refusal in
/// `translate_sub_pattern` (the declaration is skipped and the module checks), and
/// making `pattern_constraints` stop at the top of a pattern rather than recurse into
/// its sub-patterns (the module checks).
#[test]
fn a_nested_constructor_of_the_wrong_type_is_a_type_error() {
    let source = format!(
        "{}{}",
        NESTED_UNIONS,
        indoc::indoc! {r#"
            bad : Wrapper -> Count
            bad w =
              case w of
                Wrapper (Boxed n) ->
                  n
        "#}
    );

    let error = one_type_error(&source);

    let labels = error.labels();
    assert_eq!(
        ranges(&labels).first(),
        Some(&range_of(&source, "(Boxed n)")),
        "expected the caret under the nested pattern, got {:?}",
        labels
    );
    assert_eq!(labels[0].message, "this pattern");
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

// ── Records (`LANG-51`) ───────────────────────────────────────────────────────

/// The label sets of `labels`, in order — what a record term's fields are written in.
fn field_labels(fields: &[zelkova_compiler::ir::Field<TypedTerm>]) -> Vec<&str> {
    fields.iter().map(|field| field.label.as_str()).collect()
}

/// The body of a declaration of `arity` parameters: the term under its `Fun`s.
fn body_of(term: &TypedTerm, arity: usize) -> &TypedTerm {
    let mut term = term;
    for _ in 0..arity {
        match &term.kind {
            TypedTermKind::Fun { body, .. } => term = body,
            other => panic!("expected a parameter, got {:?}", other),
        }
    }
    term
}

/// The primary label's range, which every error below has exactly one of.
fn primary_range(error: &typer::Error) -> Range<usize> {
    let primary: Vec<_> = error.labels().into_iter().filter(|l| l.primary).collect();
    assert_eq!(
        primary.len(),
        1,
        "expected one primary label, got {:?}",
        primary
    );
    primary[0].span.to_range()
}

/// A record's type is the record type its fields spell out, and the term keeps the
/// fields in the order they were written while the type writes them in label order —
/// only the type is a set.
///
/// Mutation-checked by dropping the `Reason::RecordFields` equation from
/// `constraint::collect`'s record arm: `point`'s type is then an unsolved `t…` and the
/// first assertion goes red.
#[test]
fn a_record_has_the_record_type_of_its_fields() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        point =
          { y = 'c', x = True }
    "#});

    let point = typed_declaration(&solved, "point");
    assert_eq!(format!("{}", point.tpe), "{ x : Bool, y : Char }");
    match &point.kind {
        TypedTermKind::Record(fields) => assert_eq!(field_labels(fields), vec!["y", "x"]),
        other => panic!("expected a record, got {:?}", other),
    }
}

/// `{ low : Int, high : Char }` and `{ high : Char, low : Int }` are one type, so either
/// declaration satisfies either annotation.
///
/// Mutation-checked by pairing the two records' field types in opposite orders in the
/// `Record`/`Record` arm of `unify_one_constraint` (`fields2.values().rev()` zipped with
/// `fields1`'s): `Int` meets `Char` and `swapped` is rejected.
#[test]
fn the_two_spellings_of_one_record_type_unify() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        taken : { low : Int, high : Char }
        taken =
          { low = 1, high = 'c' }

        swapped : { high : Char, low : Int }
        swapped =
          taken
    "#});

    assert_eq!(
        format!("{}", typed_declaration(&solved, "swapped").tpe),
        "{ high : Char, low : Int }"
    );
}

/// Two record types with different labels do not unify, and the diagnostic names the
/// labels each has that the other lacks, in the headline's order.
///
/// Mutation-checked by dropping the label-set guard of the `Record`/`Record` arm of
/// `unify_one_constraint`, which then unifies the labels the two share and nothing else:
/// `point` checks and `one_type_error` panics.
#[test]
fn two_record_types_of_different_labels_do_not_unify() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        point : { x : Int, z : Int }
        point =
          { y = 1, z = 2 }
    "#};

    let error = one_type_error(source);
    match &error.kind {
        // The fields' own types are not solved yet when the record's equation fails —
        // a term's equations come before its children's — so the labels are what is
        // compared.
        typer::ErrorKind::UnificationFailed { left, right, .. } => {
            let labels = |tpe: &typer::Type| match tpe {
                typer::Type::Record(fields) => fields
                    .keys()
                    .map(|label| label.as_str().to_string())
                    .collect::<Vec<_>>(),
                other => panic!("expected a record type, got {:?}", other),
            };
            assert_eq!(labels(left), vec!["x", "z"]);
            assert_eq!(labels(right), vec!["y", "z"]);
        }
        other => panic!("expected a unification failure, got {:?}", other),
    }
    assert!(
        error.notes().contains(
            &"the first record type has a field `x` that the second does not, and the second has a field `y` that the first does not"
                .to_string()
        ),
        "got {:?}",
        error.notes()
    );
}

/// Two record types whose label sets differ on one side only: the note names that side
/// alone, by the headline's order, and says "fields" for more than one.
///
/// Mutation-checked twice, once per one-sided arm of `label_difference`: swapping
/// "first" and "second" in the `(false, true)` arm turns `missing`'s note red, and in the
/// `(true, false)` arm `extra`'s.
#[test]
fn a_label_set_differing_on_one_side_names_that_side() {
    let extra = one_type_error(indoc::indoc! {r#"
        module Test exposing ()

        extra : { x : Int }
        extra =
          { x = 1, y = 2 }
    "#});
    assert!(
        extra.notes().contains(
            &"the second record type has a field `y` that the first does not".to_string()
        ),
        "got {:?}",
        extra.notes()
    );

    let missing = one_type_error(indoc::indoc! {r#"
        module Test exposing ()

        missing : { x : Int, y : Int, z : Int }
        missing =
          { x = 1 }
    "#});
    assert!(
        missing.notes().contains(
            &"the first record type has fields `y`, `z` that the second does not".to_string()
        ),
        "got {:?}",
        missing.notes()
    );
}

/// The occurs check goes into a record's fields: a record holding the value it is the
/// type of would be the infinite type `a = { x : a }`, annotated or not.
///
/// Mutation-checked by giving `occurs` the arm `Type::Record(_fields) => false`: `loop`
/// and `wrapped` then both type check, and `one_type_error` panics.
#[test]
fn a_record_cannot_hold_its_own_type() {
    for source in [
        indoc::indoc! {r#"
            module Test exposing ()

            loop : a -> a
            loop r =
              { x = r }
        "#},
        indoc::indoc! {r#"
            module Test exposing ()

            same : a -> a -> a
            same x y =
              x

            wrapped r =
              same r { x = r }
        "#},
    ] {
        let error = one_type_error(source);
        assert!(
            matches!(&error.kind, typer::ErrorKind::CircularType { .. }),
            "got {:?}",
            error.kind
        );
    }
}

/// An access has the type of the field it reads, and a field read at another type is
/// an error under the access, explained by the annotation the record type came from.
///
/// Mutation-checked by having `FieldConstraint::read` answer an equation between the
/// field's type and itself, which skips the lookup's equation: `wrong` then checks and
/// `one_type_error` panics, and `nameOf`'s body is an unsolved `t…`.
#[test]
fn a_field_access_has_the_type_of_its_field() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        nameOf : { name : Char, age : Int } -> Char
        nameOf person =
          person.name
    "#});
    let body = body_of(typed_declaration(&solved, "nameOf"), 1);
    assert!(
        matches!(body.kind, TypedTermKind::Access { .. }),
        "got {:?}",
        body
    );
    assert_eq!(format!("{}", body.tpe), "Char");

    let source = indoc::indoc! {r#"
        module Test exposing ()

        wrong : { name : Char } -> Int
        wrong person =
          person.name
    "#};
    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::UnificationFailed { origin, .. } => {
            assert_eq!(origin.reason, typer::Reason::Access)
        }
        other => panic!("expected a unification failure, got {:?}", other),
    }
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "person.name"),
            range_of(source, "wrong : { name : Char } -> Int"),
        ]
    );
}

/// An update has the type of the record it updates, whatever its fields' new values are
/// written as.
///
/// Mutation-checked by dropping the `Reason::Update` equation from
/// `constraint::collect`'s update arm: `bump`'s body is then free to be a `Bool`, `bump`
/// checks, and `one_type_error` panics.
#[test]
fn an_update_keeps_the_type_of_its_record() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        correct : { a : Int, b : Char } -> { a : Int, b : Char }
        correct r =
          { r | a = 1 }
    "#});
    let body = body_of(typed_declaration(&solved, "correct"), 1);
    match &body.kind {
        TypedTermKind::Update { record, fields } => {
            assert_eq!(format!("{}", record.tpe), "{ a : Int, b : Char }");
            assert_eq!(field_labels(fields), vec!["a"]);
        }
        other => panic!("expected an update, got {:?}", other),
    }
    assert_eq!(format!("{}", body.tpe), "{ a : Int, b : Char }");

    let error = one_type_error(indoc::indoc! {r#"
        module Test exposing ()

        bump : { a : Int } -> Bool
        bump r =
          { r | a = 1 }
    "#});
    assert!(
        matches!(&error.kind, typer::ErrorKind::UnificationFailed { origin, .. } if origin.reason == typer::Reason::Update),
        "got {:?}",
        error.kind
    );
}

/// An update naming a label its record type lacks is the update that adds a field, and
/// is an error under that label, with a secondary label under the annotation the record
/// type came from.
///
/// Mutation-checked by giving `FieldConstraint`'s update arm `label_span: span` (the
/// whole update): the range assertion goes red. The secondary label is mutation-checked
/// by having `FieldConstraint::read` give `MissingField` `because: None`: the labels
/// assertion goes red.
#[test]
fn an_update_naming_an_absent_field_fails_under_that_field() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        added : { taken : Int } -> { taken : Int }
        added r =
          { r | expected = 1 }
    "#};

    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::MissingField {
            record,
            label,
            form,
            ..
        } => {
            assert_eq!(format!("{}", record), "{ taken : Int }");
            assert_eq!(label.as_str(), "expected");
            assert_eq!(*form, typer::RecordUse::Update);
        }
        other => panic!("expected a missing field, got {:?}", other),
    }
    assert_eq!(
        error.message(),
        "the record type `{ taken : Int }` has no field `expected`"
    );
    assert_eq!(primary_range(&error), range_of(source, "expected"));
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "expected"),
            range_of(source, "added : { taken : Int } -> { taken : Int }"),
        ]
    );
}

/// An update giving a field a value of another type is an error under that value, and
/// the record type it was held to is explained by the annotation.
///
/// Mutation-checked twice: by giving the update's `FieldConstraint` `field_span: span`
/// (the whole update), and the range assertion goes red; and by having
/// `unifier::read_fields` drop the equation a decided field constraint becomes instead of
/// solving it, so that the new value is never unified with the field: `retyped` checks
/// and `one_type_error` panics.
#[test]
fn an_update_retyping_a_field_fails_under_its_new_value() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        retyped : { x : Int } -> { x : Int }
        retyped r =
          { r | x = 'c' }
    "#};

    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::UnificationFailed {
            left,
            right,
            origin,
        } => {
            assert_eq!(format!("{}", left), "Int");
            assert_eq!(format!("{}", right), "Char");
            assert_eq!(origin.reason, typer::Reason::UpdateField);
        }
        other => panic!("expected a unification failure, got {:?}", other),
    }
    assert_eq!(primary_range(&error), range_of(source, "'c'"));
}

/// An accessor written as an argument takes its record type from the parameter it is
/// passed to, annotated or not, one written as a declaration's whole body takes it from
/// the annotation, and a label that type lacks is an error under the accessor.
///
/// Mutation-checked by giving `annotate`'s accessor arm a plain fresh variable for its
/// type, so that `constraint::collect` finds no arrow to read a field constraint off:
/// `.age` is then never looked up, `misread` checks, and `one_type_error` panics.
#[test]
fn an_accessor_is_typed_from_its_argument_position() {
    let helpers = indoc::indoc! {r#"
        module Test exposing ()

        apply : ({ name : Char } -> Char) -> { name : Char } -> Char
        apply f r =
          f r
    "#};

    let solved = solved(&format!(
        "{}{}",
        helpers,
        indoc::indoc! {r#"

            nameOf : { name : Char } -> Char
            nameOf person =
              apply .name person

            unannotated person =
              apply .name person

            direct : { name : Char } -> Char
            direct =
              .name
        "#}
    ));
    for name in ["unannotated", "direct"] {
        assert_eq!(
            format!("{}", typed_declaration(&solved, name).tpe),
            "{ name : Char } -> Char"
        );
    }
    let body = body_of(typed_declaration(&solved, "nameOf"), 1);
    let TypedTermKind::Apply { fun, .. } = &body.kind else {
        panic!("expected an application, got {:?}", body);
    };
    let TypedTermKind::Apply { arg: accessor, .. } = &fun.kind else {
        panic!("expected an application, got {:?}", fun);
    };
    assert!(
        matches!(accessor.kind, TypedTermKind::Accessor { .. }),
        "got {:?}",
        accessor
    );
    assert_eq!(format!("{}", accessor.tpe), "{ name : Char } -> Char");

    let source = format!(
        "{}{}",
        helpers,
        indoc::indoc! {r#"

            misread : { name : Char } -> Char
            misread person =
              apply .age person
        "#}
    );
    let error = one_type_error(&source);
    assert!(
        matches!(
            &error.kind,
            typer::ErrorKind::MissingField {
                form: typer::RecordUse::Accessor,
                ..
            }
        ),
        "got {:?}",
        error.kind
    );
    assert_eq!(primary_range(&error), range_of(&source, ".age"));
}

/// An accessor nothing fixes the record type of is an error naming it, under the whole
/// accessor.
///
/// Mutation-checked by having `unifier::read_fields` answer `Ok(substitution)` when a
/// pass decides nothing: `pick` then checks and `one_type_error` panics.
#[test]
fn an_accessor_nothing_fixes_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        pick =
          .name
    "#};

    let error = one_type_error(source);
    assert!(
        matches!(&error.kind, typer::ErrorKind::RecordTypeUnknown { label, form: typer::RecordUse::Accessor, .. } if label.as_str() == "name"),
        "got {:?}",
        error.kind
    );
    assert_eq!(
        error.message(),
        "cannot type the accessor `.name`: nothing in this declaration says which record type it reads"
    );
    assert_eq!(primary_range(&error), range_of(source, ".name"));
}

/// `f person = person.name` and `f r = { r | x = 1 }` are each an error with the caret
/// across the form, saying an annotation would supply the type — the use does not decide
/// the record's type — and each checks once annotated.
///
/// Mutation-checked by letting a field constraint solve its record type: in
/// `FieldConstraint::read`, a record type still a variable answered with the equation
/// between it and `{ label : field }`. Both unannotated declarations then check, as the
/// one-field record their use touches, and `one_type_error` panics.
#[test]
fn a_use_does_not_decide_a_records_type() {
    let access = indoc::indoc! {r#"
        module Test exposing ()

        f person =
          person.name
    "#};
    let update = indoc::indoc! {r#"
        module Test exposing ()

        f r =
          { r | x = 1 }
    "#};

    for (source, form, text) in [
        (access, typer::RecordUse::Access, "person.name"),
        (update, typer::RecordUse::Update, "{ r | x = 1 }"),
    ] {
        let error = one_type_error(source);
        assert!(
            matches!(&error.kind, typer::ErrorKind::RecordTypeUnknown { form: found, .. } if *found == form),
            "got {:?}",
            error.kind
        );
        assert_eq!(primary_range(&error), range_of(source, text));
        assert!(
            error
                .notes()
                .contains(&"a type annotation on `f` would supply it".to_string()),
            "got {:?}",
            error.notes()
        );
    }

    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        f : { name : Char } -> Char
        f person =
          person.name

        g : { x : Int } -> { x : Int }
        g r =
          { r | x = 1 }
    "#});
    assert_eq!(
        format!("{}", typed_declaration(&solved, "f").tpe),
        "{ name : Char } -> Char"
    );
    assert_eq!(
        format!("{}", typed_declaration(&solved, "g").tpe),
        "{ x : Int } -> { x : Int }"
    );
}

/// A record type supplied by a record expression written *after* the access in the
/// same declaration counts: the access is read once the whole declaration is solved.
///
/// Mutation-checked by reading the field constraints in `infer_annotated` against the
/// substitution of the annotation alone, before the body's equations are solved:
/// `r.a`'s record type is then still a variable, and `read` is rejected.
#[test]
fn a_record_written_after_an_access_supplies_its_type() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        first : a -> b -> a
        first x y =
          x

        same : a -> a -> a
        same x y =
          x

        read r =
          first r.a (same r { a = 'c' })
    "#});

    assert_eq!(
        format!("{}", typed_declaration(&solved, "read").tpe),
        "{ a : Char } -> Char"
    );
}

/// A field read off a field: `r.a.b` and `(.a r).b` each read the record type the first
/// read solved, and an access whose record type is only solved by a read written *after*
/// it waits for that one.
///
/// Mutation-checked by reading the field constraints in one pass only, a constraint
/// whose record type is still a variable being an error at once (`unifier::read_fields`
/// returning `field.unknown()` instead of keeping it): `later` is then rejected, since
/// `x.b` is collected before the `.a` that solves `x`.
#[test]
fn a_field_of_a_field_is_read_in_turn() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        first : a -> b -> a
        first x y =
          x

        same : a -> a -> a
        same x y =
          x

        chained : { a : { b : Int } } -> Int
        chained r =
          r.a.b

        accessed : { a : { b : Int } } -> Int
        accessed r =
          (.a r).b

        later r x =
          first x.b (same x (same r { a = { b = 'c' } }).a)
    "#});

    for name in ["chained", "accessed"] {
        assert_eq!(
            format!("{}", typed_declaration(&solved, name).tpe),
            "{ a : { b : Int } } -> Int"
        );
    }
    assert_eq!(
        format!("{}", typed_declaration(&solved, "later").tpe),
        "{ a : { b : Char } } -> { b : Char } -> Char"
    );
}

/// An unannotated chain is reported at its root, the access whose record type the rest
/// wait on, in whichever order the two are collected: `r.a.b` collects `r.a` first, and
/// `.b r.a` collects `.b` first, since an application walks its function before its
/// argument.
///
/// Mutation-checked by having `unifier::unknown` blame the first constraint left instead
/// of the first root (`unexplained.first()` alone): `.b r.a` is then reported at `.b`,
/// and its label assertion goes red.
#[test]
fn an_unannotated_chain_is_reported_at_its_root() {
    for (source, root) in [
        (
            indoc::indoc! {r#"
                module Test exposing ()

                g r =
                  r.a.b
            "#},
            "r.a.b",
        ),
        (
            indoc::indoc! {r#"
                module Test exposing ()

                g r =
                  .b r.a
            "#},
            ".b r.a",
        ),
    ] {
        let error = one_type_error(source);
        assert!(
            matches!(&error.kind, typer::ErrorKind::RecordTypeUnknown { label, .. } if label.as_str() == "a"),
            "in {:?}, got {:?}",
            root,
            error.kind
        );
        assert_eq!(primary_range(&error), range_within(source, root, "r.a"));
    }
}

/// A type variable inside a record type is instantiated afresh at each use, like one
/// anywhere else in a declared type, so one declaration may read `{ x : a }` at two
/// field types, and a use at the wrong one is an error.
///
/// Mutation-checked by having `Types::instantiate` return a record type unchanged: the
/// two uses in `both` then share `a`, which cannot be both `Int` and `Char`, and the
/// solve panics; and `wrong`'s result is a fresh variable unrelated to its argument, so
/// it checks.
#[test]
fn a_record_type_holding_a_variable_is_instantiated_at_each_use() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        get : { x : a } -> a
        get r =
          r.x

        both : (Int, Char)
        both =
          (get { x = 1 }, get { x = 'c' })
    "#});
    let get = typed_declaration(&solved, "get");
    let a = variable_of(get);
    assert_eq!(format!("{}", get.tpe), format!("{{ x : {} }} -> {}", a, a));
    assert_eq!(
        format!("{}", typed_declaration(&solved, "both").tpe),
        "( Int, Char )"
    );

    let error = one_type_error(indoc::indoc! {r#"
        module Test exposing ()

        get : { x : a } -> a
        get r =
          r.x

        wrong : Char
        wrong =
          get { x = 1 }
    "#});
    assert_eq!(error.declaration.as_str(), "wrong");
}

/// The text of the single type variable `term`'s type ends in — `get`'s `a`, whose
/// number is the typer's business.
fn variable_of(term: &TypedTerm) -> String {
    match &term.tpe {
        typer::Type::Fun { return_tpe, .. } => format!("{}", return_tpe),
        other => panic!("expected a function, got {:?}", other),
    }
}

/// A field read off a type that is not a record is an error under the label, with a
/// secondary label under the annotation the type came from.
///
/// Mutation-checked by answering `Ok(None)` from `FieldConstraint::read` for a type of
/// any other form: the access then waits forever and is reported as
/// `RecordTypeUnknown`, and the variant assertion goes red. The secondary label is
/// mutation-checked by having `FieldConstraint::read` give `NotARecord` `because: None`:
/// the labels assertion goes red.
#[test]
fn a_field_of_a_type_that_is_not_a_record_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        f : Int -> Int
        f n =
          n.x
    "#};

    let error = one_type_error(source);
    assert!(
        matches!(&error.kind, typer::ErrorKind::NotARecord { tpe, .. } if format!("{}", tpe) == "Int"),
        "got {:?}",
        error.kind
    );
    assert_eq!(primary_range(&error), range_within(source, "n.x", "x"));
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_within(source, "n.x", "x"),
            range_of(source, "f : Int -> Int"),
        ]
    );
}

/// An annotation's own type variable written for the record supplies no record type:
/// `r.x` against `a` is as unknown as against no annotation at all, and the note says
/// the annotation writes a type variable there rather than asking for one.
///
/// Mutation-checked as [`a_use_does_not_decide_a_records_type`] is: letting a field
/// constraint solve its record type makes `f` check. The note is mutation-checked by
/// having `infer_annotated` pass `annotated: false` to `unifier::read_fields`: the
/// supplier is then `Annotation`, and the variant assertion goes red.
#[test]
fn a_type_variable_is_not_a_record_type() {
    let error = one_type_error(indoc::indoc! {r#"
        module Test exposing ()

        f : a -> Int
        f r =
          r.x
    "#});
    assert!(
        matches!(
            &error.kind,
            typer::ErrorKind::RecordTypeUnknown {
                supplier: typer::Supplier::AnnotationVariable,
                ..
            }
        ),
        "got {:?}",
        error.kind
    );
    let notes = error.notes();
    assert!(
        notes.contains(&"the annotation on `f` does not say which record type this is: it writes a type variable where the record type would be spelled out".to_string()),
        "got {:?}",
        notes
    );
    assert!(
        !notes.iter().any(|note| note.contains("would supply it")),
        "got {:?}",
        notes
    );
}

/// A record type that is not part of the declaration's type — an accessor handed to a
/// parameter that takes any value — is one no annotation on the declaration could
/// supply, annotated or not, and the note says so instead of asking for one.
///
/// Mutation-checked by having `unifier::unknown` take every record type as part of the
/// declaration's (`declared.contains(&record) || true`): `annotated`'s supplier is then
/// `AnnotationVariable`, and the variant assertion goes red.
#[test]
fn a_record_type_outside_the_declarations_type_is_the_bodys_to_supply() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        first : a -> b -> a
        first x y =
          x

        annotated : Int -> Int
        annotated n =
          first n .x

        unannotated n =
          first n .x
    "#};

    let errors = type_errors(source);
    assert_eq!(errors.len(), 2, "got {:?}", errors);
    for error in &errors {
        let name = error.declaration.as_str();
        assert!(
            matches!(
                &error.kind,
                typer::ErrorKind::RecordTypeUnknown {
                    supplier: typer::Supplier::Body,
                    ..
                }
            ),
            "for {}, got {:?}",
            name,
            error.kind
        );
        assert!(
            error.notes().contains(&format!(
                "it is not part of `{}`'s type, so no annotation on `{}` could supply it",
                name, name
            )),
            "for {}, got {:?}",
            name,
            error.notes()
        );
    }
}

/// A constructor whose argument is a record is typed like any other, so a `case` on it
/// reads the record's fields.
///
/// Mutation-checked by having `canonical_type_to_typer_type` answer `None` for a record
/// type again: `Link` then drops out of the environment, `value` comes back
/// `Untranslatable`, and `typed_declaration` panics.
#[test]
fn a_constructor_holding_a_record_is_typed() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        type Chain
          = End
          | Link { value : Int, next : Chain }

        value : Chain -> Int
        value c =
          case c of
            Link r ->
              r.value

            End ->
              0

        link : Chain
        link =
          Link { value = 1, next = End }
    "#});

    assert_eq!(
        format!("{}", typed_declaration(&solved, "value").tpe),
        "Chain -> Int"
    );
    assert_eq!(
        format!("{}", typed_declaration(&solved, "link").tpe),
        "Chain"
    );
}

/// A field read off a name that did not resolve raises no type error of its own when
/// the name's real type could have supplied its record type — the record is the name
/// (`f`), the result of applying it (`h`), or a field read off either (`chained`,
/// `accessed`) — because the name's own error, the one to fix, is reported already
/// ([`DEC-23` decisions 3 and 6](../../../docs/decisions/dec-23.md)). A record type
/// nothing the name could be would supply is still reported beside it (`g`).
///
/// Mutation-checked three ways, in `unifier`, each turning the first assertion red: by
/// passing `&[]` for the holes in `unknown`, at `f`; by having `reach` return its seeds'
/// variables without the rounds, at `chained`; and by seeding it with only those holes'
/// types that are a bare variable (the hole's type, not its variables), at `h`.
#[test]
fn a_field_of_an_unresolved_name_is_not_reported_again() {
    let interfaces = HashMap::from([basics_interface(), char_interface()]);
    let canonical = canonicalize_recovering_with_interfaces(
        indoc::indoc! {r#"
            module Test exposing ()

            f : Int
            f =
              missing.x

            h : Int -> Int
            h z =
              (missing z).x

            chained : Int
            chained =
              missing.x.y

            accessed : Int
            accessed =
              (.x missing).y

            g person =
              first missing person.name

            first : a -> b -> a
            first x y =
              x
        "#},
        &interfaces,
    );
    assert!(!canonical.errors.is_empty(), "`missing` does not resolve");

    let check = typer::type_check_recovering(&canonical.module, &interfaces);
    for name in ["f", "h", "chained", "accessed"] {
        assert!(
            matches!(
                check.solved.get(&Name::new(name)),
                Some(Solved::Typed { .. })
            ),
            "for {}, got {:?}",
            name,
            check.solved.get(&Name::new(name))
        );
    }
    let declarations: Vec<&str> = check
        .errors
        .iter()
        .map(|e| e.declaration.as_str())
        .collect();
    assert_eq!(declarations, vec!["g"], "got {:?}", check.errors);
    assert!(matches!(
        check.errors[0].kind,
        typer::ErrorKind::RecordTypeUnknown { .. }
    ));
}

// ── Record patterns (`LANG-84`) ───────────────────────────────────────────────

/// The first branch's pattern of the `case` a declaration of `arity` parameters has for
/// its body — which, for a parameter written as a pattern, is the match it became.
fn first_pattern(term: &TypedTerm, arity: usize) -> &zelkova_compiler::ir::TermPattern {
    match &body_of(term, arity).kind {
        TypedTermKind::Case { branches, .. } => &branches[0].0,
        other => panic!("expected a match, got {:?}", other),
    }
}

/// The entries of a record pattern, each label beside the type its field was solved to.
fn entry_types(pattern: &zelkova_compiler::ir::TermPattern) -> Vec<(String, String)> {
    match &pattern.kind {
        zelkova_compiler::ir::TermPatternKind::Record { fields } => fields
            .iter()
            .map(|field| {
                (
                    field.label.as_str().to_string(),
                    format!("{}", field.value.tpe),
                )
            })
            .collect(),
        other => panic!("expected a record pattern, got {:?}", other),
    }
}

/// A parameter annotated with a record type and written `{ name }` binds `name` at the
/// field's type, and the declaration's type is the annotation's. `name`'s type is also
/// the body's, which the annotation fixes, so `ageless` binds `age` too, whose type
/// nothing but its field gives.
///
/// Mutation-checked by having `FieldConstraint::read` answer, for a decided entry, an
/// equation between the entry's type and itself, so that the field's type is never
/// unified with it: `age`'s type is then an unsolved `t…`, and the last assertion goes
/// red.
#[test]
fn a_record_pattern_parameter_binds_its_field_at_the_fields_type() {
    let named = solved(indoc::indoc! {r#"
        module Test exposing ()

        nameOf : { name : Char } -> Char
        nameOf { name } =
          name
    "#});

    let name_of = typed_declaration(&named, "nameOf");
    assert_eq!(format!("{}", name_of.tpe), "{ name : Char } -> Char");
    assert_eq!(
        entry_types(first_pattern(name_of, 1)),
        vec![("name".to_string(), "Char".to_string())]
    );
    match &body_of(name_of, 1).kind {
        TypedTermKind::Case { branches, .. } => {
            assert_eq!(format!("{}", branches[0].1.tpe), "Char")
        }
        other => panic!("expected a match, got {:?}", other),
    }

    let ageless = solved(indoc::indoc! {r#"
        module Test exposing ()

        ageless : { name : Char, age : Int } -> Char
        ageless { name, age } =
          name
    "#});
    assert_eq!(
        entry_types(first_pattern(typed_declaration(&ageless, "ageless"), 1)),
        vec![
            ("name".to_string(), "Char".to_string()),
            ("age".to_string(), "Int".to_string())
        ]
    );
}

/// A record pattern names a subset of a record's fields: two of three check, each bound
/// at its own field's type, and the third is neither matched nor bound.
///
/// Mutation-checked by having `constraint::pattern_constraints` give a record pattern the
/// own equation a tuple has — the record type of its entries' types, held to `against` —
/// on top of its field constraints: `{ x : Int, y : Char }` then fails to unify with the
/// annotation's three fields, and `solved` panics.
#[test]
fn a_record_pattern_names_a_subset_of_the_fields() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        pair : { x : Int, y : Char, z : Bool } -> (Int, Char)
        pair { x, y } =
          (x, y)
    "#});

    let pair = typed_declaration(&solved, "pair");
    assert_eq!(
        format!("{}", pair.tpe),
        "{ x : Int, y : Char, z : Bool } -> ( Int, Char )"
    );
    assert_eq!(
        entry_types(first_pattern(pair, 1)),
        vec![
            ("x".to_string(), "Int".to_string()),
            ("y".to_string(), "Char".to_string())
        ]
    );
}

/// A record pattern naming a label the matched record type lacks is an error under that
/// label, with a secondary label under the annotation the record type came from.
///
/// Mutation-checked by giving the record pattern's `FieldConstraint` `label_span:
/// pattern.span` (the whole pattern) in `constraint::pattern_constraints`: the range
/// assertions go red. And by having `FieldConstraint::read` skip the label lookup,
/// answering a field of a record type with the equation between the entry's type and
/// itself whether or not the label is there: `absent` checks and `one_type_error` panics.
#[test]
fn a_record_pattern_naming_an_absent_label_fails_under_it() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        absent : { x : Int, y : Int } -> Int
        absent { x, wide } =
          x
    "#};

    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::MissingField {
            record,
            label,
            form,
            ..
        } => {
            assert_eq!(format!("{}", record), "{ x : Int, y : Int }");
            assert_eq!(label.as_str(), "wide");
            assert_eq!(*form, typer::RecordUse::Pattern);
        }
        other => panic!("expected a missing field, got {:?}", other),
    }
    assert_eq!(
        error.message(),
        "the record type `{ x : Int, y : Int }` has no field `wide`"
    );
    assert_eq!(primary_range(&error), range_of(source, "wide"));
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_of(source, "wide"),
            range_of(source, "absent : { x : Int, y : Int } -> Int"),
        ]
    );
    assert!(
        error.notes().contains(
            &"a record pattern names some of the fields of the record it matches, and each label it names must be one of them"
                .to_string()
        ),
        "got {:?}",
        error.notes()
    );
}

/// When two entries of a record pattern both name an absent label, the one written first
/// is reported, even when it is nested in an earlier entry: in `{ a = { b }, d }` that is
/// `b`, as in the tuple `({ b }, { d })`. The field constraints are collected pre-order,
/// each entry just before the entries nested in it, and the first failed read is the
/// error.
///
/// Mutation-checked by moving the recursion in `constraint::pattern_constraints`'
/// `Record` arm back after the loop that pushes the entries, so that `d` is collected
/// before `b`: the error is then `d`'s, and the label assertion goes red.
#[test]
fn a_record_pattern_reports_the_absent_label_written_first_at_any_depth() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        f : { a : { x : Int }, c : Int } -> Int
        f { a = { b }, d } =
          1
    "#};

    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::MissingField { record, label, .. } => {
            assert_eq!(label.as_str(), "b");
            assert_eq!(format!("{}", record), "{ x : Int }");
        }
        other => panic!("expected a missing field, got {:?}", other),
    }
    assert_eq!(primary_range(&error), range_within(source, "{ b }", "b"));
}

/// `{ centre = { x, y } }` against a record holding a record binds `x` and `y` at the
/// inner fields' types: the inner pattern's record type is the outer entry's field type,
/// read in turn. `x` is also the body, so `y` is the one only its field types.
///
/// Mutation-checked by having `constraint::pattern_constraints` not recurse into a
/// record pattern's entries (no sub-patterns for the `Record` arm): the inner `{ x, y }`
/// is then never read, `y` is an unsolved `t…`, and the assertion on the inner entries
/// goes red.
#[test]
fn a_record_pattern_in_a_field_binds_at_the_inner_fields_type() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        centreX : { centre : { x : Char, y : Int }, radius : Int } -> Char
        centreX { centre = { x, y } } =
          x
    "#});

    let centre_x = typed_declaration(&solved, "centreX");
    let pattern = first_pattern(centre_x, 1);
    assert_eq!(
        entry_types(pattern),
        vec![("centre".to_string(), "{ x : Char, y : Int }".to_string())]
    );
    let zelkova_compiler::ir::TermPatternKind::Record { fields } = &pattern.kind else {
        panic!("expected a record pattern, got {:?}", pattern);
    };
    assert_eq!(
        entry_types(&fields[0].value.pattern),
        vec![
            ("x".to_string(), "Char".to_string()),
            ("y".to_string(), "Int".to_string())
        ]
    );
}

/// A record pattern inside a constructor pattern takes its record type from the
/// constructor's argument, at a branch head and in a parameter alike, and a label that
/// argument's record type lacks is an error.
///
/// Mutation-checked by having `translate_pattern` give each constructor argument a fresh
/// type variable instead of the constructor's declared parameter type: the record
/// pattern's type is then unknown, both declarations are `RecordTypeUnknown`, and
/// `solved` panics.
#[test]
fn a_record_pattern_in_a_constructor_takes_the_arguments_type() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        type Reading
          = Reading { taken : Char, depth : Int }
          | Missing

        taken : Reading -> Char
        taken r =
          case r of
            Reading { taken } ->
              taken

            Missing ->
              'c'

        depth : Reading -> Int
        depth (Reading { depth }) =
          depth
    "#});

    let taken = typed_declaration(&solved, "taken");
    assert_eq!(format!("{}", taken.tpe), "Reading -> Char");
    let zelkova_compiler::ir::TermPatternKind::Constructor { args, .. } =
        &first_pattern(taken, 1).kind
    else {
        panic!("expected a constructor pattern");
    };
    assert_eq!(format!("{}", args[0].tpe), "{ depth : Int, taken : Char }");
    assert_eq!(
        entry_types(&args[0].pattern),
        vec![("taken".to_string(), "Char".to_string())]
    );
    assert_eq!(
        format!("{}", typed_declaration(&solved, "depth").tpe),
        "Reading -> Int"
    );

    let source = indoc::indoc! {r#"
        module Test exposing ()

        type Reading
          = Reading { taken : Char }

        other : Reading -> Char
        other (Reading { expected }) =
          expected
    "#};
    let error = one_type_error(source);
    assert!(
        matches!(&error.kind, typer::ErrorKind::MissingField { record, label, .. } if format!("{}", record) == "{ taken : Char }" && label.as_str() == "expected"),
        "got {:?}",
        error.kind
    );
    assert_eq!(
        primary_range(&error),
        range_within(source, "{ expected }", "expected")
    );
}

/// `{ taken = Celsius, expected = e }` checks, `e` bound at the field's type, and a
/// constructor of the wrong type in a field is an error under that constructor — the
/// entry's own pattern — explained by the annotation the record type came from.
///
/// Mutation-checked by giving the record pattern's `FieldConstraint` `field_span:
/// pattern.span` (the whole pattern) in `constraint::pattern_constraints`: the range
/// assertion goes red.
#[test]
fn a_constructor_in_a_field_is_checked_against_the_fields_type() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        type Celsius
          = Celsius

        describe : { taken : Celsius, expected : Celsius } -> Celsius
        describe reading =
          case reading of
            { taken = Celsius, expected = e } ->
              e
    "#});
    let describe = typed_declaration(&solved, "describe");
    assert_eq!(
        format!("{}", describe.tpe),
        "{ expected : Celsius, taken : Celsius } -> Celsius"
    );
    assert_eq!(
        entry_types(first_pattern(describe, 1)),
        vec![
            ("taken".to_string(), "Celsius".to_string()),
            ("expected".to_string(), "Celsius".to_string())
        ]
    );

    let source = indoc::indoc! {r#"
        module Test exposing ()

        type Celsius
          = Celsius

        type Kelvin
          = Kelvin

        describe : { taken : Celsius, expected : Celsius } -> Celsius
        describe reading =
          case reading of
            { taken = Kelvin, expected = e } ->
              e
    "#};
    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::UnificationFailed {
            left,
            right,
            origin,
        } => {
            assert_eq!(format!("{}", left), "Celsius");
            assert_eq!(format!("{}", right), "Kelvin");
            assert_eq!(origin.reason, typer::Reason::RecordPatternEntry);
        }
        other => panic!("expected a unification failure, got {:?}", other),
    }
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_within(source, "taken = Kelvin", "Kelvin"),
            range_of(
                source,
                "describe : { taken : Celsius, expected : Celsius } -> Celsius"
            ),
        ]
    );
}

/// An unannotated `nameOf { name } = name` is an error with the caret on the whole
/// pattern, saying an annotation would supply the record type — the pattern does not
/// decide it — and the same declaration annotated checks.
///
/// Mutation-checked by letting a record pattern's field constraint solve its record type:
/// in `FieldConstraint::read`, a record type still a variable answered, for
/// `RecordUse::Pattern`, with the equation between it and `{ label : field }`. `nameOf`
/// then checks as the one-field record its pattern names, and `one_type_error` panics.
#[test]
fn a_record_pattern_does_not_decide_its_record_type() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        nameOf { name } =
          name
    "#};

    let error = one_type_error(source);
    assert!(
        matches!(
            &error.kind,
            typer::ErrorKind::RecordTypeUnknown {
                label,
                form: typer::RecordUse::Pattern,
                supplier: typer::Supplier::Annotation,
                ..
            } if label.as_str() == "name"
        ),
        "got {:?}",
        error.kind
    );
    assert_eq!(
        error.message(),
        "cannot type this record pattern: nothing in this declaration says which record type it matches"
    );
    assert_eq!(primary_range(&error), range_of(source, "{ name }"));
    assert!(
        error
            .notes()
            .contains(&"a type annotation on `nameOf` would supply it".to_string()),
        "got {:?}",
        error.notes()
    );

    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        nameOf : { name : Char } -> Char
        nameOf { name } =
          name
    "#});
    assert_eq!(
        format!("{}", typed_declaration(&solved, "nameOf").tpe),
        "{ name : Char } -> Char"
    );
}

/// A record pattern is checked wherever a pattern is written: at a `case` branch head
/// whose scrutinee is a tuple element, and as an element of a parameter's tuple pattern.
///
/// Mutation-checked by translating a record pattern as `TermPatternKind::Anything` in
/// `translate_pattern`: neither tuple pattern then holds a record pattern, and the search
/// for one panics.
#[test]
fn a_record_pattern_nests_in_a_tuple_and_heads_a_branch() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        swap : ({ x : Char }, Int) -> Char
        swap ({ x }, _) =
          x

        branch : (Int, { x : Char }) -> Char
        branch pair =
          case pair of
            (_, { x }) ->
              x
    "#});

    for name in ["swap", "branch"] {
        let term = typed_declaration(&solved, name);
        let zelkova_compiler::ir::TermPatternKind::Tuple { elements } =
            &first_pattern(term, 1).kind
        else {
            panic!("expected a tuple pattern in `{}`", name);
        };
        let record = elements
            .iter()
            .find(|element| {
                matches!(
                    element.pattern.kind,
                    zelkova_compiler::ir::TermPatternKind::Record { .. }
                )
            })
            .unwrap_or_else(|| panic!("expected a record pattern in `{}`", name));
        assert_eq!(format!("{}", record.tpe), "{ x : Char }");
        assert_eq!(
            entry_types(&record.pattern),
            vec![("x".to_string(), "Char".to_string())]
        );
    }
}

/// A record pattern matched against a type that is not a record is an error under the
/// label of its first entry, with a secondary label under the annotation the type came
/// from.
///
/// Mutation-checked by having `FieldConstraint::read` answer `Ok(None)` for a type of any
/// other form: the entry then waits forever, is reported as `RecordTypeUnknown`, and the
/// variant assertion goes red.
#[test]
fn a_record_pattern_against_a_type_that_is_not_a_record_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        f : Int -> Int
        f { x } =
          x
    "#};

    let error = one_type_error(source);
    assert!(
        matches!(&error.kind, typer::ErrorKind::NotARecord { tpe, form: typer::RecordUse::Pattern, .. } if format!("{}", tpe) == "Int"),
        "got {:?}",
        error.kind
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_within(source, "{ x }", "x"),
            range_of(source, "f : Int -> Int"),
        ]
    );
    assert_eq!(
        error.labels()[0].message,
        "`x` is matched against a type that is not a record"
    );
}

/// What supplies a record pattern's type may be written after it: the record the
/// branch's body builds decides the scrutinee's type, and the pattern is read once the
/// whole declaration is solved.
///
/// Mutation-checked by reading the field constraints in `infer_annotated` against the
/// substitution of the annotation alone, before the body's equations are solved: the
/// pattern's record type is then still a variable, and `read` is rejected.
#[test]
fn a_record_written_after_a_record_pattern_supplies_its_type() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        first : a -> b -> a
        first x y =
          x

        same : a -> a -> a
        same x y =
          x

        read r =
          case r of
            { a } ->
              first a (same r { a = 'c', b = 1 })
    "#});

    assert_eq!(
        format!("{}", typed_declaration(&solved, "read").tpe),
        "{ a : Char, b : Int } -> Char"
    );
}

/// A field read off a name a record pattern bound reads the type the pattern's entry was
/// solved to: the entry is read first, then the access whose record type it decided.
///
/// Mutation-checked by having `unifier::read_fields` drop the equation a decided field
/// constraint becomes instead of solving it, so that `centre` is never bound at its
/// field's type: `centre.x` then waits forever, and `solved` panics on its
/// `RecordTypeUnknown`.
#[test]
fn a_field_of_a_name_a_record_pattern_binds_is_read_in_turn() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()

        centreX : { centre : { x : Char } } -> Char
        centreX { centre } =
          centre.x
    "#});

    let centre_x = typed_declaration(&solved, "centreX");
    assert_eq!(
        format!("{}", centre_x.tpe),
        "{ centre : { x : Char } } -> Char"
    );
    assert_eq!(
        entry_types(first_pattern(centre_x, 1)),
        vec![("centre".to_string(), "{ x : Char }".to_string())]
    );
}

/// A record pattern whose record type a name that did not resolve would have supplied
/// raises no type error of its own: written as the argument of a constructor that did
/// not resolve (`f`), or matched against an unresolved value (`g`). The unresolved
/// name's own error is the one reported ([`DEC-23` decisions 3 and
/// 6](../../../docs/decisions/dec-23.md)). One nothing the name could be would supply is
/// still reported beside it (`h`).
///
/// Mutation-checked by having `constraint::pattern_constraints` leave a hole pattern's
/// argument types out of `Constraints::holes`: `f` is then `RecordTypeUnknown`, and the
/// declarations assertion goes red.
#[test]
fn a_record_pattern_an_unresolved_name_explains_is_not_reported_again() {
    let interfaces = HashMap::from([basics_interface(), char_interface()]);
    let canonical = canonicalize_recovering_with_interfaces(
        indoc::indoc! {r#"
            module Test exposing ()

            f : Int -> Int
            f (Missing { x }) =
              x

            g : Int
            g =
              case missing of
                { x } ->
                  x

            h n =
              case n of
                ((Missing _), { y }) ->
                  y
        "#},
        &interfaces,
    );
    assert!(
        !canonical.errors.is_empty(),
        "`Missing` and `missing` do not resolve"
    );

    let check = typer::type_check_recovering(&canonical.module, &interfaces);
    for name in ["f", "g"] {
        assert!(
            matches!(
                check.solved.get(&Name::new(name)),
                Some(Solved::Typed { .. })
            ),
            "for {}, got {:?}",
            name,
            check.solved.get(&Name::new(name))
        );
    }
    let declarations: Vec<&str> = check
        .errors
        .iter()
        .map(|e| e.declaration.as_str())
        .collect();
    assert_eq!(declarations, vec!["h"], "got {:?}", check.errors);
    assert!(matches!(
        check.errors[0].kind,
        typer::ErrorKind::RecordTypeUnknown {
            form: typer::RecordUse::Pattern,
            ..
        }
    ));
}

/// A body using a name a record pattern binds at another type than its field's is
/// reported at the entry, since the entry is read after every equation, the body's
/// included: the caret is under the binding in the pattern and the annotation is what
/// explains the field's type. A tuple pattern is reported at the body instead, its own
/// equation coming before the body's. This pins the place the record pattern's error is
/// reported at today; `ERR-20` is where the body would be the place.
///
/// The note does not say the entry fails to match its field, which a name cannot do: it
/// says the body uses the name at another type, which is what failed.
///
/// Mutation-checked by giving the record pattern's `FieldConstraint` `field_span:
/// pattern.span`: the primary range goes red. By having `entry_reason` in
/// `constraint.rs` answer `Reason::RecordPatternEntry` for a `Bind`: the reason assertion
/// goes red. And by deleting `Reason::RecordPatternBinding`'s arm of `Reason::note`: the
/// notes assertion goes red.
#[test]
fn a_body_using_a_bound_field_at_another_type_is_reported_at_the_entry() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        nameOf : { name : Char, age : Int } -> Int
        nameOf { name } =
          name
    "#};

    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::UnificationFailed {
            left,
            right,
            origin,
        } => {
            assert_eq!(format!("{}", left), "Char");
            assert_eq!(format!("{}", right), "Int");
            assert_eq!(origin.reason, typer::Reason::RecordPatternBinding);
        }
        other => panic!("expected a unification failure, got {:?}", other),
    }
    assert_eq!(
        primary_range(&error),
        range_within(source, "{ name }", "name")
    );
    assert_eq!(
        error.notes(),
        vec![
            "in the declaration of `nameOf`".to_string(),
            "a name a record pattern binds has the type of the field it names, and the body uses this one at another type"
                .to_string(),
        ]
    );
}

/// An entry whose pattern binds a name without being one, `(Box x)`, can fail either
/// because the pattern does not match the field or because the body uses `x` at another
/// type — here the second: `x` is a `Char` and is returned as an `Int` — and the note
/// names both rather than blaming the pattern.
///
/// Mutation-checked by having `entry_reason` in `constraint.rs` answer
/// `Reason::RecordPatternEntry` for every entry that is not a `Bind`: the reason assertion
/// goes red. And by deleting `Reason::RecordPatternEntryWithBindings`' arm of
/// `Reason::note`: the notes assertion goes red.
#[test]
fn an_entry_binding_a_name_inside_another_pattern_names_both_causes() {
    let source = indoc::indoc! {r#"
        module Test exposing ()

        type Box a
          = Box a

        unbox : { content : Box Char } -> Int
        unbox { content = (Box x) } =
          x
    "#};

    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::UnificationFailed {
            left,
            right,
            origin,
        } => {
            assert_eq!(format!("{}", left), "Char");
            assert_eq!(format!("{}", right), "Int");
            assert_eq!(origin.reason, typer::Reason::RecordPatternEntryWithBindings);
        }
        other => panic!("expected a unification failure, got {:?}", other),
    }
    assert_eq!(primary_range(&error), range_of(source, "(Box x)"));
    assert_eq!(
        error.notes(),
        vec![
            "in the declaration of `unbox`".to_string(),
            "either this entry does not match the type of the field it names, or the body uses a name it binds at another type than that field gives the name"
                .to_string(),
        ]
    );
}

// ── A literal's type is its spelling ──────────────────────────────────────────

/// A literal written without a point is an `Int`, and one written with a point is a
/// `Float`; nothing else decides either.
///
/// Mutation-checked twice: deleting the integer-literal arm's equation in
/// `constraint::walk` leaves `x` an inference variable, and giving it `Float` makes `x`
/// a `Float`; each turns the first assertion red.
#[test]
fn a_literal_is_typed_by_its_spelling() {
    let solved = solved(indoc::indoc! {r#"
        module Test exposing ()
        x = 1
        y = 1.5
    "#});

    assert_eq!(format!("{}", typed_declaration(&solved, "x").tpe), "Int");
    assert_eq!(format!("{}", typed_declaration(&solved, "y").tpe), "Float");
}

/// An integer literal is not a `Float`: a declaration annotated `Float` with a body of
/// `1` is a type error, and `1.0` is what it means.
///
/// Mutation-checked by giving the integer-literal arm of `constraint::walk` `Float`
/// in place of `Int`, which an annotation of `Float` then accepts: the first assertion
/// goes red.
#[test]
fn an_integer_literal_is_not_a_float() {
    let error = one_type_error(indoc::indoc! {r#"
        module Test exposing (..)
        x : Float
        x = 1
    "#});
    assert_eq!(error.message(), "cannot match `Float` with `Int`");

    assert!(run(indoc::indoc! {r#"
        module Test exposing (..)
        x : Float
        x = 1.0
    "#})
    .is_ok());
}

/// The error for an integer literal at the wrong type names `Int`, a spelling no
/// annotation can read as a type variable.
///
/// Mutation-checked by deleting the integer-literal arm's equation: nothing then
/// constrains the literal, the declaration checks, and `one_type_error` goes red.
#[test]
fn a_literal_mismatch_names_no_type_variable() {
    let message = one_type_error(indoc::indoc! {r#"
        module Test exposing (..)
        x : Char
        x = 1
    "#})
    .message();

    assert_eq!(message, "cannot match `Char` with `Int`");
}

// ── Class obligations ─────────────────────────────────────────────────────────
//
// A use of a name whose type has a context asks an instance of each of the context's
// classes at the type the use gave it, and the answer is read once the declaration's
// equations are solved (`typer::classes`). Each source below declares the classes and
// instances it needs, because nothing in `std/core` does yet.

/// `Eq`, a union to use it at, and a second union with no instance, which every source
/// below that wants them starts from. It exposes nothing, so a declaration appended to it
/// may go without an annotation.
const EQ: &str = indoc::indoc! {r#"
    module Test exposing ()

    type Colour
      = Red
      | Blue

    type Plain
      = Plain

    type Box a
      = Box a

    class Eq a where
      eq : a -> a -> Bool

    instance Eq Colour where
      eq a b =
        True

    instance Eq a => Eq (Box a) where
      eq (Box left) (Box right) =
        eq left right

    instance Eq Int where
      eq a b =
        True
"#};

/// `EQ`, followed by `declarations`.
fn with_eq(declarations: &str) -> String {
    format!("{}\n{}", EQ, declarations)
}

/// The class and the type a [`typer::ErrorKind::NoInstance`] names, written the way the
/// message does, or a panic for any other kind.
fn no_instance_of(error: &typer::Error) -> (String, String) {
    match &error.kind {
        typer::ErrorKind::NoInstance { class, tpe, .. } => {
            (class.unqualified_name().to_string(), format!("{}", tpe))
        }
        other => panic!("expected `NoInstance`, got {:?}", other),
    }
}

/// A use whose obligation an instance discharges checks, and so does a use through an
/// instance with a context when the context's own obligation is discharged in turn.
///
/// Mutation-checked by making the instance lookup in `entail` find nothing: every use is
/// then a `NoInstance` and `run(..).is_ok()` goes red.
#[test]
fn a_use_an_instance_discharges_checks() {
    let source = with_eq(indoc::indoc! {r#"
        same : Colour -> Colour -> Bool
        same x y =
          eq x y

        boxed : Bool
        boxed =
          eq (Box Red) (Box Blue)

        nested : Bool
        nested =
          eq (Box (Box 1)) (Box (Box 2))
    "#});

    assert!(
        run(&source).is_ok(),
        "every use is at a type with an instance"
    );
}

/// An instance's context is asked of the type's arguments: `Eq (Box Plain)` needs `Eq
/// Plain`, which has no instance, and the error names the inner class and type, with the
/// caret under the use.
///
/// Mutation-checked by skipping the context loop in `entail`: the use is then answered by
/// `Eq (Box a)` alone, the module checks and `one_type_error` goes red.
#[test]
fn an_instance_context_is_asked_of_the_arguments() {
    let source = with_eq(indoc::indoc! {r#"
        boxed : Bool
        boxed =
          eq (Box Plain) (Box Plain)
    "#});

    let error = one_type_error(&source);
    assert_eq!(
        no_instance_of(&error),
        ("Eq".to_string(), "Plain".to_string())
    );
    assert_eq!(error.message(), "there is no instance of `Eq` for `Plain`");

    // What asked for it is named: the use needed `Eq (Box Plain)`.
    let typer::ErrorKind::NoInstance { needed_by, .. } = &error.kind else {
        panic!("expected `NoInstance`");
    };
    let needed_by = needed_by
        .as_ref()
        .expect("it was asked through an instance");
    assert_eq!(format!("{}", needed_by.tpe), "Box Plain");

    let at = range_within(&source, "eq (Box Plain) (Box Plain)", "eq");
    assert_eq!(ranges(&error.labels()), vec![at]);
}

/// A use at a type with no instance is an error naming the class and the type, with the
/// caret under the use and not under the declaration.
///
/// Mutation-checked by having `entail` answer `Ok(())` where the lookup finds no instance:
/// the declaration checks and `one_type_error` goes red.
#[test]
fn a_use_at_a_type_with_no_instance_is_an_error_at_the_use() {
    let source = with_eq(indoc::indoc! {r#"
        same : Plain -> Plain -> Bool
        same x y =
          eq x y
    "#});

    let error = one_type_error(&source);
    assert_eq!(
        no_instance_of(&error),
        ("Eq".to_string(), "Plain".to_string())
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_within(&source, "eq x y", "eq")],
        "the caret is under the use, not across the declaration"
    );
}

/// A use at a function type is the same error: no instance can be declared for one.
///
/// Mutation-checked by having the `Type::Fun` arm of `entail` answer `Ok(())`: the
/// declaration checks and `one_type_error` goes red.
#[test]
fn a_use_at_a_function_type_has_no_instance() {
    let source = with_eq(indoc::indoc! {r#"
        flip : Colour -> Colour
        flip c =
          c

        funs : Bool
        funs =
          eq flip flip
    "#});

    let error = one_type_error(&source);
    assert_eq!(
        no_instance_of(&error),
        ("Eq".to_string(), "Colour -> Colour".to_string())
    );
    assert!(
        error
            .notes()
            .iter()
            .any(|note| note.contains("a function type has no instances")),
        "got {:?}",
        error.notes()
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_within(&source, "eq flip flip", "eq")]
    );
}

/// A constrained declaration's body may use a member of its context's class, and a member
/// of that class's superclass, transitively.
///
/// Mutation-checked twice: with `provide` not following `superclasses`, the second and
/// third declarations fail with `MissingConstraint`; with it following one level only, the
/// third does.
#[test]
fn a_context_provides_its_class_and_every_superclass() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)

        class Eq a where
          eq : a -> a -> Bool

        class Eq a => Comparable a where
          lt : a -> a -> Bool

        class Comparable a => Ordered a where
          gt : a -> a -> Bool

        own : Eq a => a -> a -> Bool
        own x y =
          eq x y

        superclass : Comparable a => a -> a -> Bool
        superclass x y =
          eq x y

        grandparent : Ordered a => a -> a -> Bool
        grandparent x y =
          eq x y
    "#};

    assert!(run(source).is_ok(), "{:?}", run(source).err());
}

/// A class the context does not provide is the missing-constraint error, naming the
/// constraint to add, with the caret under the use.
///
/// Mutation-checked by making `entail` treat any given of a *different* class as
/// providing the obligation: the declaration checks and `one_type_error` goes red.
#[test]
fn a_class_the_context_does_not_provide_is_a_missing_constraint() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)

        class Eq a where
          eq : a -> a -> Bool

        class Eq a => Comparable a where
          lt : a -> a -> Bool

        notProvided : Eq a => a -> a -> Bool
        notProvided x y =
          lt x y
    "#};

    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::MissingConstraint {
            class,
            variable,
            written,
            ..
        } => {
            assert_eq!(class.unqualified_name().as_str(), "Comparable");
            assert_eq!(variable, "a");
            assert_eq!(*written, typer::Written::Annotation);
        }
        other => panic!("expected `MissingConstraint`, got {:?}", other),
    }
    assert_eq!(
        error.message(),
        "`Comparable a` is required here, and the annotation does not provide it"
    );
    assert!(error.notes().contains(
        &"add `Comparable a` to the constraints of the annotation on `notProvided`".to_string()
    ));
    assert_eq!(
        ranges(&error.labels()),
        vec![range_within(source, "lt x y", "lt")]
    );
}

/// A given is on its own variable: `Eq a` does not provide `Eq b`, and the error names the
/// variable that is missing it.
///
/// Mutation-checked by having `entail` answer a variable of any given's class whatever the
/// variable: the declaration checks and `one_type_error` goes red.
#[test]
fn a_given_provides_its_own_variable_and_no_other() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)

        class Eq a where
          eq : a -> a -> Bool

        other : Eq a => a -> b -> Bool
        other x y =
          eq y y
    "#};

    let error = one_type_error(source);
    match &error.kind {
        typer::ErrorKind::MissingConstraint {
            class, variable, ..
        } => {
            assert_eq!(class.unqualified_name().as_str(), "Eq");
            assert_eq!(variable, "b");
        }
        other => panic!("expected `MissingConstraint`, got {:?}", other),
    }
}

/// A declaration with no annotation whose body needs a class of an undetermined type is
/// the needs-annotation error, which says what the declaration has to state; one whose
/// body pins the type down checks.
///
/// Mutation-checked by making `discharge` answer `MissingConstraint` whether or not the
/// declaration is annotated: the first assertion goes red.
#[test]
fn an_unannotated_declaration_needing_a_class_needs_an_annotation() {
    let source = with_eq(indoc::indoc! {r#"
        same x y =
          eq x y
    "#});

    let error = one_type_error(&source);
    match &error.kind {
        typer::ErrorKind::ConstraintNeedsAnnotation { class, stated, .. } => {
            assert_eq!(class.unqualified_name().as_str(), "Eq");
            assert_eq!(stated, "Eq a => a -> a -> Bool");
        }
        other => panic!("expected `ConstraintNeedsAnnotation`, got {:?}", other),
    }
    assert!(error.notes().contains(
        &"`same` is what the declaration has to state: `same : Eq a => a -> a -> Bool`".to_string()
    ));
    assert_eq!(
        ranges(&error.labels()),
        vec![range_within(&source, "eq x y", "eq")]
    );

    // The type is determined, and `Eq Int` is discharged where it stands.
    assert!(run(&with_eq(indoc::indoc! {r#"
        isZero n =
          eq n 0
    "#}))
    .is_ok());
}

/// A declaration with no annotation states every constraint it needs, each once and over
/// the variables its type holds.
///
/// Mutation-checked by listing only the first residual: the `stated` text loses `Eq b`.
#[test]
fn what_an_unannotated_declaration_has_to_state_is_every_constraint() {
    let source = with_eq(indoc::indoc! {r#"
        both x y u v =
          (eq x y, eq u v)
    "#});

    match one_type_error(&source).kind {
        typer::ErrorKind::ConstraintNeedsAnnotation { stated, .. } => {
            assert_eq!(stated, "(Eq a, Eq b) => a -> a -> b -> b -> ( Bool, Bool )")
        }
        other => panic!("expected `ConstraintNeedsAnnotation`, got {:?}", other),
    }
}

/// A class needed at a type nothing determines — one that is not part of the
/// declaration's type, so that no annotation on it could name it — is the third error.
///
/// Mutation-checked by classifying a variable outside the declaration's type as
/// `MissingConstraint`: the kind assertion goes red.
#[test]
fn a_class_needed_at_a_type_nothing_determines_is_undetermined() {
    let source = with_eq(indoc::indoc! {r#"
        read : Int -> a
        read n =
          read n

        undetermined : Int -> Bool
        undetermined n =
          eq (read n) (read n)
    "#});

    let error = one_type_error(&source);
    match &error.kind {
        typer::ErrorKind::UndeterminedConstraint { class, .. } => {
            assert_eq!(class.unqualified_name().as_str(), "Eq")
        }
        other => panic!("expected `UndeterminedConstraint`, got {:?}", other),
    }
    assert_eq!(
        error.message(),
        "`Eq` is required of a type nothing in this declaration determines"
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_within(&source, "eq (read n) (read n)", "eq")]
    );
}

/// When several obligations fail, the one whose use comes first is named, on every run.
///
/// Mutation-checked by discharging the obligations in reverse order, which names
/// `Comparable`.
#[test]
fn the_first_failing_obligation_in_order_is_the_one_reported() {
    let source = with_eq(indoc::indoc! {r#"
        class Comparable a where
          lt : a -> a -> Bool

        twice : ( Bool, Bool )
        twice =
          ( eq Plain Plain, lt Plain Plain )
    "#});

    for _ in 0..8 {
        let error = one_type_error(&source);
        assert_eq!(no_instance_of(&error).0, "Eq");
    }
}

/// An instance's binding is checked against the member's signature at the instance's
/// type: a binding of the wrong type is an error, blamed on the binding, with the head
/// line as the second label where an annotation would be.
///
/// Mutation-checked by skipping `InstanceCheck::binding` for every binding: the module
/// checks, and `type_errors` panics.
#[test]
fn an_instance_binding_of_the_wrong_type_is_an_error() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)

        type Colour
          = Red

        class Eq a where
          eq : a -> a -> Bool

        instance Eq Colour where
          eq a b =
            1
    "#};

    let error = one_type_error(source);
    assert!(
        matches!(error.kind, typer::ErrorKind::UnificationFailed { .. }),
        "got {:?}",
        error.kind
    );
    assert_eq!(error.message(), "cannot match `Bool` with `Int`");
    assert!(error
        .notes()
        .iter()
        .any(|note| note.contains("in the binding of `eq` in the instance `Eq Colour`")));
    assert_eq!(
        ranges(&error.labels()),
        vec![
            range_within(source, "eq a b =\n    1", "1"),
            range_of(source, "instance Eq Colour where"),
        ]
    );
}

/// An instance binding can use the instance's own context, as a function can use its
/// annotation's: `EQ`'s `Eq (Box a)` compares its contents through the `Eq a` it was given.
///
/// Mutation-checked by passing the instance's givens to its bindings as an empty list:
/// the `Box` instance of `EQ` then fails with `MissingConstraint` and `run(..).is_ok()`
/// goes red.
#[test]
fn an_instance_binding_is_given_the_instance_context() {
    assert!(run(&with_eq("")).is_ok(), "{:?}", run(&with_eq("")).err());
}

/// An instance of a class with a superclass needs the superclass's instance at the same
/// head, and for a type with parameters the superclass's context has to be provided:
/// `instance Comparable (Box a)` beside `instance Eq a => Eq (Box a)` is an error naming
/// `Eq a`, and it checks with `Eq a` in its own context.
///
/// Mutation-checked by discharging the superclass obligations with no givens: the second
/// source fails too.
#[test]
fn an_instance_has_to_provide_its_superclass_context() {
    let declarations = indoc::indoc! {r#"
        module Test exposing (..)

        type Box a
          = Box a

        class Eq a where
          eq : a -> a -> Bool

        class Eq a => Comparable a where
          lt : a -> a -> Bool

        instance Eq a => Eq (Box a) where
          eq (Box left) (Box right) =
            eq left right

    "#};

    let without = format!(
        "{}{}",
        declarations,
        indoc::indoc! {r#"
            instance Comparable (Box a) where
              lt (Box left) (Box right) =
                False
        "#}
    );
    let error = one_type_error(&without);
    match &error.kind {
        typer::ErrorKind::MissingConstraint {
            class,
            variable,
            written,
            ..
        } => {
            assert_eq!(class.unqualified_name().as_str(), "Eq");
            assert_eq!(variable, "a");
            assert_eq!(*written, typer::Written::InstanceContext);
        }
        other => panic!("expected `MissingConstraint`, got {:?}", other),
    }
    assert_eq!(
        error.message(),
        "`Eq a` is required here, and the instance's context does not provide it"
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_of(&without, "instance Comparable (Box a) where")]
    );

    let with = format!(
        "{}{}",
        declarations,
        indoc::indoc! {r#"
            instance Eq a => Comparable (Box a) where
              lt (Box left) (Box right) =
                False
        "#}
    );
    assert!(run(&with).is_ok(), "{:?}", run(&with).err());
}

/// A derived instance is an instance that exists: a use at its type is answered by it,
/// whatever it derives.
///
/// Mutation-checked by leaving a `derived` instance out of `ClassTable::of`: the use then
/// has no instance and `run(..).is_ok()` goes red.
#[test]
fn a_derived_instance_is_an_instance_that_exists() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)

        type Colour
          = Red
          | Blue

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

        instance Eq Colour where
          derived

        same : Bool
        same =
          eq Red Blue
    "#};

    assert!(run(source).is_ok(), "{:?}", run(source).err());
}

/// A derived instance's superclasses are not checked: its context is none recorded until
/// `LANG-83` infers one, and a check against none would reject `derived Comparable (Box a)`
/// beside `Eq a => Eq (Box a)`, which the derivation is going to make sound. The same
/// instance written out is `MissingConstraint`, as the second source shows, so the skip is
/// the only thing the first passes by.
///
/// This is the test `LANG-83` turns round: once a derived instance has an inferred context,
/// its superclass obligations are discharged against it, as a written instance's are.
///
/// Mutation-checked by checking the superclasses of a `derived` instance too, with its
/// context as given (none): the first source is then `MissingConstraint` and `run(..)`
/// goes red.
#[test]
fn a_derived_instance_is_not_held_to_its_superclass_context_until_lang_83() {
    let declarations = indoc::indoc! {r#"
        module Test exposing (..)

        type Box a
          = Box a

        class Eq a where
          eq : a -> a -> Bool

        class Eq a => Comparable a where
          lt : a -> a -> Bool

          derived lt
            matched = False
            differed _ _ = False
            combine x y =
              case x of
                True ->
                  y

                False ->
                  False

        instance Eq a => Eq (Box a) where
          eq (Box left) (Box right) =
            eq left right

    "#};

    let derived = format!(
        "{}instance Comparable (Box a) where\n  derived\n",
        declarations
    );
    assert!(run(&derived).is_ok(), "{:?}", run(&derived).err());

    let written = format!(
        "{}instance Comparable (Box a) where\n  lt (Box left) (Box right) =\n    False\n",
        declarations
    );
    assert!(matches!(
        one_type_error(&written).kind,
        typer::ErrorKind::MissingConstraint { .. }
    ));
}

/// In an instance's binding, a constraint on a variable the member's signature binds is a
/// `MissingConstraint` written as `MemberSignature`, and the note does not tell the user to
/// put it in the instance's context: the head does not bind the variable, so canonicalization
/// would reject that. The head's own variable is still the instance's context to fix.
///
/// Mutation-checked by giving the solver no member variables: the constraint on `b` is then
/// written as `InstanceContext` and the `written` assertion goes red.
#[test]
fn a_constraint_on_a_member_signature_variable_cannot_be_added_to_the_instance() {
    let declarations = indoc::indoc! {r#"
        module Test exposing (..)

        type Box a
          = Box a

        class Eq a where
          eq : a -> a -> Bool

        class Container a where
          has : a -> b -> b -> Bool

    "#};

    let source = format!(
        "{}{}",
        declarations,
        indoc::indoc! {r#"
            instance Container (Box a) where
              has c x y =
                eq x y
        "#}
    );
    let error = one_type_error(&source);
    match &error.kind {
        typer::ErrorKind::MissingConstraint {
            class,
            variable,
            written,
            ..
        } => {
            assert_eq!(class.unqualified_name().as_str(), "Eq");
            assert_eq!(variable, "b");
            assert_eq!(*written, typer::Written::MemberSignature);
        }
        other => panic!("expected `MissingConstraint`, got {:?}", other),
    }
    assert_eq!(
        error.message(),
        "`Eq b` is required here, and the member's signature does not provide it"
    );
    assert!(
        error
            .notes()
            .iter()
            .all(|note| !note.contains("to the context of the instance")),
        "{:?}",
        error.notes()
    );
    assert!(error
        .notes()
        .iter()
        .any(|note| note.contains("no context of the instance can constrain it")));
    assert_eq!(
        ranges(&error.labels()),
        vec![range_within(&source, "eq x y", "eq")]
    );

    // A variable of the head is the instance's context to fix, as before.
    let head = format!(
        "{}{}",
        declarations,
        indoc::indoc! {r#"
            instance Container (Box a) where
              has (Box inner) x y =
                eq inner inner
        "#}
    );
    let error = one_type_error(&head);
    assert!(
        matches!(
            error.kind,
            typer::ErrorKind::MissingConstraint {
                written: typer::Written::InstanceContext,
                ..
            }
        ),
        "{:?}",
        error.kind
    );
}

/// An instance's context is asked of the type's arguments in position: with `instance (Eq
/// a, Eq b) => Eq (Pair a b)`, a use at `Pair Colour Plain` fails on `Plain` and a use at
/// `Pair Plain Colour` on `Plain` too, each with the caret under the use and the use's own
/// obligation named. The same holds for a tuple head.
///
/// Mutation-checked by having the context loop in `entail` always take the first argument:
/// `Pair Colour Plain` and `( Colour, Plain )` are then answered by `Eq Colour` twice and
/// check, and `one_type_error` goes red. Taking the last one instead goes red on the other
/// order.
#[test]
fn an_instance_context_is_instantiated_at_each_argument_in_position() {
    // The head the instance is declared for, and a use at each order of the two arguments.
    let heads = [
        (
            "(Pair a b)",
            [
                "eq (Pair Red Plain) (Pair Red Plain)",
                "eq (Pair Plain Red) (Pair Plain Red)",
            ],
        ),
        (
            "( a, b )",
            [
                "eq ( Red, Plain ) ( Red, Plain )",
                "eq ( Plain, Red ) ( Plain, Red )",
            ],
        ),
    ];

    for (head, expressions) in heads {
        for expression in expressions {
            let source = with_eq(&format!(
                indoc::indoc! {r#"
                    type Pair a b
                      = Pair a b

                    instance (Eq a, Eq b) => Eq {} where
                      eq x y =
                        True

                    pair : Bool
                    pair =
                      {}
                "#},
                head, expression
            ));

            let error = one_type_error(&source);
            assert_eq!(
                no_instance_of(&error),
                ("Eq".to_string(), "Plain".to_string()),
                "{}",
                expression
            );
            let typer::ErrorKind::NoInstance { needed_by, .. } = &error.kind else {
                panic!("expected `NoInstance`");
            };
            assert!(needed_by.is_some(), "{}", expression);
            assert_eq!(
                ranges(&error.labels()),
                vec![range_within(&source, expression, "eq")],
                "{}",
                expression
            );
        }
    }
}

/// A variable met before a missing instance wins over it, and a missing instance met before
/// any variable wins over one after: the first failure in order is reported, but a
/// `MissingConstraint` is not given up for the `NoInstance` that follows it.
///
/// Mutation-checked by returning the missing instance's error at once whatever came before:
/// the first source is then `NoInstance` and the `MissingConstraint` match goes red.
#[test]
fn a_variable_met_before_a_missing_instance_is_reported_first() {
    let variable_first = with_eq(indoc::indoc! {r#"
        both : a -> ( Bool, Bool )
        both x =
          ( eq x x, eq Plain Plain )
    "#});
    let error = one_type_error(&variable_first);
    assert!(
        matches!(error.kind, typer::ErrorKind::MissingConstraint { .. }),
        "{:?}",
        error.kind
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_within(&variable_first, "eq x x", "eq")]
    );

    let instance_first = with_eq(indoc::indoc! {r#"
        both : a -> ( Bool, Bool )
        both x =
          ( eq Plain Plain, eq x x )
    "#});
    let error = one_type_error(&instance_first);
    assert_eq!(
        no_instance_of(&error),
        ("Eq".to_string(), "Plain".to_string())
    );
}

/// An obligation at a record type is accepted and raises no error: a record is no instance's
/// head, a class with a derivation walks its fields once `LANG-85` lands, and until then
/// nothing answers or refuses the obligation.
///
/// Mutation-checked by restoring the error arm, `Type::Record(_) => return
/// Err(no_instance(&predicate))`, in `entail`: `eq r s` is then a `NoInstance` and
/// `run(..).is_ok()` goes red.
#[test]
fn an_obligation_at_a_record_is_accepted_until_lang_85() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)

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

        instance Eq Int where
          derived

        same : { x : Int } -> { x : Int } -> Bool
        same r s =
          eq r s
    "#};

    assert!(run(source).is_ok(), "{:?}", run(source).err());
}

/// A use through an operator whose `infix` declaration names a member raises the
/// obligation the member does, at the operator.
///
/// Mutation-checked by not registering a class's members in the environment: the operator
/// then names a value the typer does not know and the declaration is `UnboundName`, so
/// `one_type_error` panics.
#[test]
fn an_operator_naming_a_member_raises_its_obligation() {
    let source = with_eq(indoc::indoc! {r#"
        infix left 4 (==) = eq

        same : Plain -> Plain -> Bool
        same x y =
          x == y
    "#});

    let error = one_type_error(&source);
    assert_eq!(
        no_instance_of(&error),
        ("Eq".to_string(), "Plain".to_string())
    );
    assert_eq!(
        ranges(&error.labels()),
        vec![range_within(&source, "x == y", "==")]
    );

    assert!(run(&with_eq(indoc::indoc! {r#"
        infix left 4 (==) = eq

        same : Colour -> Colour -> Bool
        same x y =
          x == y
    "#}))
    .is_ok());
}

/// An annotation's variable that the body forces to a concrete type takes its given with
/// it: `min : Comparable a => a -> a -> a` whose body makes `a` an `Int` proves
/// `Comparable Int` and publishes `Comparable a`, and **checks**.
///
/// This pins a hole on purpose. It is the width of the hole every annotation has while its
/// variables are flexible: `LANG-12` makes them rigid, and rejects this declaration with
/// its own error. That ticket has to turn this test round; nothing in this one narrows the
/// hole (no partial rigidity check), so the day `LANG-12` lands it goes red and says where.
///
/// The second half is what shows the given went with the variable: with no `Comparable
/// Int`, the declaration is a `NoInstance` at `Int` and not a missing constraint.
///
/// Mutation-checked by letting a given answer an obligation on any type of its class: the
/// second half goes red.
#[test]
fn an_annotation_variable_forced_to_int_checks_until_lang_12() {
    let declarations = indoc::indoc! {r#"
        module Test exposing (..)

        class Eq a where
          eq : a -> a -> Bool

        class Eq a => Comparable a where
          lt : a -> a -> Bool

        min : Comparable a => a -> a -> a
        min x y =
          if lt x 0 then
            x

          else
            y
    "#};

    let with_instances = format!(
        "{}\n{}",
        declarations,
        indoc::indoc! {r#"
            instance Eq Int where
              eq a b =
                True

            instance Comparable Int where
              lt a b =
                True
        "#}
    );
    assert!(
        run(&with_instances).is_ok(),
        "{:?}",
        run(&with_instances).err()
    );

    // Without an instance at `Int` the obligation is on `Int`, where a given does not
    // reach: the error is the instance's absence, not the annotation's.
    let error = one_type_error(declarations);
    assert_eq!(
        no_instance_of(&error),
        ("Comparable".to_string(), "Int".to_string())
    );
}

// ── Class obligations across modules ──────────────────────────────────────────

const EQS: &str = indoc::indoc! {r#"
    module Eqs exposing (Eq, same)

    class Eq a where
      eq : a -> a -> Bool

    same : Eq a => a -> a -> Bool
    same x y =
      eq x y
"#};

const COLOURS: &str = indoc::indoc! {r#"
    module Colours exposing (Colour(..))

    import Eqs exposing (Eq)

    type Colour
      = Red
      | Blue

    instance Eq Colour where
      eq a b =
        True
"#};

/// A member and a constrained function imported from another module are checked against
/// the context in that module's `Interface`, and an instance declared in a third module
/// discharges the obligation.
///
/// Mutation-checked two ways: reading an interface's value with an empty context makes
/// `same 1 2` check, so the second assertion goes red; and leaving `imported_instances`
/// out of `ClassTable::of` makes the first use fail with `NoInstance`. The member half is
/// `an_imported_member_is_checked_against_its_class`'s.
#[test]
fn an_imported_constrained_name_is_checked_against_its_interface() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)

        import Colours exposing (Colour(..))
        import Eqs exposing (Eq, same)

        good : Bool
        good =
          same Red Blue

        member : Bool
        member =
          eq Red Blue
    "#};

    let checked = check_package_module(&[EQS, COLOURS, main], "Main");
    assert!(checked.is_ok(), "{:?}", checked.err());

    let bad = indoc::indoc! {r#"
        module Main exposing (..)

        import Eqs exposing (Eq, same)

        bad : Bool
        bad =
          same 1 2
    "#};

    let errors = check_package_module(&[EQS, COLOURS, bad], "Main").expect_err("no `Eq Int`");
    match errors.as_slice() {
        [CompilationError::Type(type_errors, _)] => {
            let [error] = type_errors.as_slice() else {
                panic!("expected one type error, got {:?}", type_errors);
            };
            assert_eq!(no_instance_of(error), ("Eq".to_string(), "Int".to_string()));
            assert_eq!(
                ranges(&error.labels()),
                vec![range_within(bad, "same 1 2", "same")]
            );
        }
        other => panic!("expected one type error, got {:?}", other),
    }
}

/// A member imported from another module is checked against its class: used at a type no
/// instance is declared for, it is an error at the use. A member nothing declares a type
/// for would be left unchecked, and this is what tells the two apart.
///
/// Mutation-checked by leaving imported interfaces' classes out of the environment: `eq`
/// is then a name the typer does not know, the declaration is left unchecked, and
/// `expect_err` panics.
#[test]
fn an_imported_member_is_checked_against_its_class() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)

        import Eqs exposing (Eq)

        bad : Bool
        bad =
          eq 1 2
    "#};

    let errors = check_package_module(&[EQS, COLOURS, main], "Main").expect_err("no `Eq Int`");
    match errors.as_slice() {
        [CompilationError::Type(type_errors, _)] => {
            let [error] = type_errors.as_slice() else {
                panic!("expected one type error, got {:?}", type_errors);
            };
            assert_eq!(no_instance_of(error), ("Eq".to_string(), "Int".to_string()));
            assert_eq!(
                ranges(&error.labels()),
                vec![range_within(main, "eq 1 2", "eq")]
            );
        }
        other => panic!("expected one type error, got {:?}", other),
    }
}

/// The instance declared in `Colours` reaches `Main` although `Main` names nothing in it
/// but the type: an instance is in scope wherever its class and its type are.
///
/// Mutation-checked with the one above.
#[test]
fn an_instance_of_a_third_module_is_in_scope_through_imports() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)

        import Colours exposing (Colour(..))
        import Eqs exposing (same)

        good : Bool
        good =
          same Red Blue
    "#};

    let checked = check_package_module(&[EQS, COLOURS, main], "Main");
    assert!(checked.is_ok(), "{:?}", checked.err());
}
