//! Patterns nested inside other patterns (`docs/spec/patterns.md`, *Patterns nest*):
//! a constructor as a tuple element, an applied constructor as a constructor argument,
//! and a parenthesised constructor at the head of a `case` branch. Each test asserts the
//! `PatternKind` the source parses to, not only that it parsed; the spans are read
//! directly, since `NodeSpan`'s `PartialEq` is blind.

use super::support::*;
use codespan_reporting::files::SimpleFile;
use zelkova_syntax::parser::*;
use zelkova_syntax::position::{BytePos, Span};
use zelkova_syntax::tuple::Tuple;

fn pattern_var(variable: &str) -> Pattern {
    Pattern::bare(PatternKind::Variable(name(variable)))
}

fn pattern_ctor_with(constructor: &str, args: Vec<Pattern>) -> Pattern {
    Pattern::bare(PatternKind::Constructor(name(constructor), args))
}

/// The patterns of the `case` branches in the body of the module's only function, in
/// source order.
fn branch_patterns(source: &str) -> Vec<Pattern> {
    let file = SimpleFile::new("Main.zel".to_owned(), source.to_owned());
    let module = parse(&file)
        .unwrap_or_else(|error| panic!("expected the source to parse, got {:?}", error));
    let [function] = module.functions.as_slice() else {
        panic!("expected one function, got {:?}", module.functions);
    };
    let [binding] = function.bindings.as_slice() else {
        panic!("expected one binding, got {:?}", function.bindings);
    };
    let ExpressionKind::Case(_, branches) = &binding.body.kind else {
        panic!(
            "expected the body to be a `case`, got {:?}",
            binding.body.kind
        );
    };
    branches
        .iter()
        .map(|branch| branch.pattern.clone())
        .collect()
}

/// Where `needle` first occurs in `source`, as the span a node written there carries.
fn at(source: &str, needle: &str) -> Option<Span<BytePos>> {
    word_at(source, needle, needle)
}

/// The span of `word` written at the start of the first occurrence of `context`, for a
/// word that also occurs earlier in `source` on its own.
fn word_at(source: &str, context: &str, word: &str) -> Option<Span<BytePos>> {
    assert!(context.starts_with(word), "`{}` starts `{}`", word, context);
    let start = source.find(context).expect("source contains the fragment");
    Some(Span {
        start: BytePos(start as u32),
        end: BytePos((start + word.len()) as u32),
    })
}

/// `(On, On)`: a nullary constructor as each element of a tuple pattern, each spanning
/// its own name.
///
/// Mutation-checked by deleting `Pattern`'s nullary `QualTypeIdent` alternative from
/// `grammar.lalrpop`: the source no longer parses, and the test goes red.
#[test]
fn a_nullary_constructor_is_a_tuple_element() {
    let source = indoc::indoc! {"
        module Example exposing (Flag, both)

        both pair =
          case pair of
            (On, Off) ->
              On

            _ ->
              Off
    "};

    let patterns = branch_patterns(source);

    assert_eq!(
        patterns[0].kind,
        PatternKind::Tuple(Tuple::two(
            pattern_ctor(name("On")),
            pattern_ctor(name("Off"))
        ))
    );
    let PatternKind::Tuple(Tuple::Two(first, second)) = &patterns[0].kind else {
        panic!("expected a two-element tuple, got {:?}", patterns[0].kind);
    };
    assert_eq!(first.span.span(), word_at(source, "On,", "On"));
    assert_eq!(second.span.span(), word_at(source, "Off)", "Off"));
    assert_eq!(patterns[1].kind, PatternKind::Anything);
}

/// `Wrapper (Circle n)`: an applied constructor as the argument of a bare one at a
/// branch's head. The inner constructor spans its parentheses, its name and its
/// argument, which is the text `canonical::Error::VariantNotFound`'s caret sits under
/// when `Circle` does not resolve.
///
/// Mutation-checked two ways, each red on its own: deleting `Pattern`'s
/// parenthesised-applied-constructor alternative (the source no longer parses), and
/// making that alternative's span `NodeSpan::new(l, l)` (the span assertion fails).
#[test]
fn an_applied_constructor_is_a_constructor_argument() {
    let source = indoc::indoc! {"
        module Example exposing (inner)

        inner w =
          case w of
            Wrapper (Circle n) ->
              n

            _ ->
              One
    "};

    let patterns = branch_patterns(source);

    assert_eq!(
        patterns[0].kind,
        PatternKind::Constructor(
            name("Wrapper"),
            vec![pattern_ctor_with("Circle", vec![pattern_var("n")])],
        )
    );
    let PatternKind::Constructor(_, args) = &patterns[0].kind else {
        panic!("expected a constructor pattern, got {:?}", patterns[0].kind);
    };
    assert_eq!(args[0].span.span(), at(source, "(Circle n)"));
    assert_eq!(patterns[0].span.span(), at(source, "Wrapper (Circle n)"));
}

/// `(Circle n)` at a branch's head: a parenthesised applied constructor reads the same
/// as the bare one, and `Dot` after it is still a nullary constructor.
///
/// Mutation-checked two ways, each red on its own: deleting `Pattern`'s
/// parenthesised-applied-constructor alternative (the source no longer parses), and
/// making that alternative's span `NodeSpan::new(l, l)` (the span assertion fails).
#[test]
fn a_parenthesised_constructor_heads_a_case_branch() {
    let source = indoc::indoc! {"
        module Example exposing (describe)

        describe shape =
          case shape of
            (Circle n) ->
              n

            Dot ->
              One
    "};

    let patterns = branch_patterns(source);

    assert_eq!(
        patterns[0].kind,
        PatternKind::Constructor(name("Circle"), vec![pattern_var("n")])
    );
    assert_eq!(patterns[0].span.span(), at(source, "(Circle n)"));
    assert_eq!(
        patterns[1].kind,
        PatternKind::Constructor(name("Dot"), vec![])
    );
}

/// A constructor nested two deep in a parameter, `Wrapper (Circle n)` itself
/// parenthesised, and a parenthesised nullary constructor beside it, which is grouping
/// and spans the name alone.
///
/// Mutation-checked two ways, each red on its own: deleting `Pattern`'s
/// parenthesised-applied-constructor alternative (the source no longer parses), and
/// making that alternative's span `NodeSpan::new(l, l)` (the span assertion fails).
#[test]
fn constructors_nest_in_a_parameter() {
    let source = indoc::indoc! {"
        module Example exposing (inner)

        inner (Wrapper (Circle n)) (Dot) =
          n
    "};

    let file = SimpleFile::new("Main.zel".to_owned(), source.to_owned());
    let module = parse(&file)
        .unwrap_or_else(|error| panic!("expected the source to parse, got {:?}", error));
    let patterns = &module.functions[0].bindings[0].patterns;

    assert_eq!(
        patterns,
        &vec![
            pattern_ctor_with(
                "Wrapper",
                vec![pattern_ctor_with("Circle", vec![pattern_var("n")])],
            ),
            pattern_ctor(name("Dot")),
        ]
    );
    assert_eq!(patterns[0].span.span(), at(source, "(Wrapper (Circle n))"));
    assert_eq!(patterns[1].span.span(), word_at(source, "Dot)", "Dot"));
}
