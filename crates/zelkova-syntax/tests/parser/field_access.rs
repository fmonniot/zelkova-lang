//! The two forms that read a field: the access `r.name` and the accessor `.name`
//! (`docs/spec/records.md`, *Reading a field* and *The accessor*).
//!
//! A `.` written against the expression to its left is an access of it; one with
//! whitespace before it, or one opening an expression, begins an accessor, which is
//! written against its label. Each test asserts the `ExpressionKind` the source parses
//! to; the ones about positions read the spans directly, since `NodeSpan`'s `PartialEq`
//! is blind.

use super::support::*;
use codespan_reporting::files::SimpleFile;
use indoc::indoc;
use zelkova_syntax::parser::tokenizer::Token;
use zelkova_syntax::parser::*;
use zelkova_syntax::position::{BytePos, Span};

fn access(record: Expression, label: &str) -> Expression {
    Expression::bare(ExpressionKind::Access(
        Box::new(record),
        name(label),
        no_span(),
    ))
}

fn accessor(label: &str) -> Expression {
    Expression::bare(ExpressionKind::Accessor(name(label), no_span()))
}

fn app(f: Expression, arg: Expression) -> Expression {
    expr_app(Box::new(f), Box::new(arg))
}

fn var(n: &str) -> Expression {
    expr_var(name(n))
}

/// The source of a module whose only declaration is `main = <expression>`.
fn main_is(expression: &str) -> String {
    format!(
        indoc! {"
            module Main exposing (..)

            main =
              {}
        "},
        expression
    )
}

fn parsed(source: &str) -> Module {
    let file = SimpleFile::new("Main.zel".to_owned(), source.to_owned());
    parse(&file).unwrap_or_else(|error| panic!("expected {:?} to parse, got {:?}", source, error))
}

/// The body of `main = <expression>`.
fn body(expression: &str) -> Expression {
    let module = parsed(&main_is(expression));
    let [function] = module.functions.as_slice() else {
        panic!("expected one function, got {:?}", module.functions);
    };
    let [binding] = function.bindings.as_slice() else {
        panic!("expected one binding, got {:?}", function.bindings);
    };
    binding.body.clone()
}

/// The span of the first occurrence of `needle` in `source`, as a parser node records it.
fn at(source: &str, needle: &str) -> Option<Span<BytePos>> {
    let start = source
        .find(needle)
        .unwrap_or_else(|| panic!("`{}` is not in the source", needle));
    Some(Span {
        start: BytePos(start as u32),
        end: BytePos((start + needle.len()) as u32),
    })
}

/// Access binds tighter than application: `f r.name` is one application of `f`, to the
/// access `r.name`, and not `(f r).name`.
///
/// Mutation-checked by moving the access production from `Accessible` to `AppExpr`, as
/// `<record: AppExpr> "." <label: VarIdent>`: `f r.name` then parses as an access of the
/// application `f r`, and the assertion goes red.
#[test]
fn an_access_binds_tighter_than_application() {
    assert_eq!(body("f r.name"), app(var("f"), access(var("r"), "name")));
    assert_eq!(
        body("f r.name s"),
        app(app(var("f"), access(var("r"), "name")), var("s"))
    );
}

/// `f .name` is an application of `f` to an accessor: the whitespace before the `.` is
/// the only thing that tells it from the access `f.name`.
///
/// Mutation-checked by making `consume_operator` yield `Dot` for a `.` after an operand
/// whatever lies between them (dropping the `previous_operand_end` comparison): `f
/// .name` then parses as the access `f.name`, and the first assertion goes red.
#[test]
fn a_spaced_dot_before_a_label_is_an_application_to_an_accessor() {
    assert_eq!(body("f .name"), app(var("f"), accessor("name")));
    assert_eq!(body("f.name"), access(var("f"), "name"));
    assert_eq!(
        body("f .name .age"),
        app(app(var("f"), accessor("name")), accessor("age"))
    );
}

/// An access chains left to right: `r.a.b` is `(r.a).b`, the inner access being the
/// record the outer one reads.
///
/// Mutation-checked by replacing `<record: Accessible>` in the access production with a
/// copy of `Accessible` that has no access in it, which makes access non-recursive:
/// `r.a.b` then no longer parses and `body` panics. (The AST has no right-nested reading
/// of `r.a.b` to compare against, since a label is not an expression.)
#[test]
fn an_access_nests_left() {
    assert_eq!(body("r.a.b"), access(access(var("r"), "a"), "b"));
    assert_eq!(body("r.centre.x"), access(access(var("r"), "centre"), "x"));
}

/// What an access may be written on: every atomic expression but a bare constructor
/// name. A parenthesised expression, a record, a literal, a qualified variable, an
/// accessor and another access each take one; `Widget.size` and `Widget.Size.x` stay
/// qualified names, and `Widget.size.x` reads `x` off the qualified `Widget.size`.
///
/// Mutation-checked by adding a constructor production to `Accessible`: LALRPOP then
/// reports the local ambiguity against `QualTypeIdent` and the crate does not build,
/// which is the case the grammar comment on `Accessible` names. And by removing the
/// parenthesised production from `Accessible` (and adding it to `AtomicExpr`): `(Just
/// r).name` then fails to parse.
#[test]
fn what_an_access_may_be_written_on() {
    assert_eq!(
        body("(Just r).name"),
        access(app(expr_ctor(name("Just")), var("r")), "name")
    );
    assert_eq!(
        body("{ a = r }.a"),
        access(
            Expression::bare(ExpressionKind::Record(vec![Field::new(
                name("a"),
                no_span(),
                var("r"),
            )])),
            "a"
        )
    );
    assert_eq!(
        body("\"s\".y"),
        access(expr_lit(Literal::String("s".to_owned())), "y")
    );
    assert_eq!(body("Basics.r.x"), access(var("Basics.r"), "x"));
    assert_eq!(body("Widget.size.x"), access(var("Widget.size"), "x"));
    assert_eq!(body("(.name).x"), access(accessor("name"), "x"));
    assert_eq!(body(".name.x"), access(accessor("name"), "x"));

    assert_eq!(body("Widget.size"), var("Widget.size"));
    assert_eq!(body("Widget.Size.x"), var("Widget.Size.x"));
    assert_eq!(body("Widget.Size"), expr_ctor(name("Widget.Size")));
}

/// An accessor is an expression wherever one may be written: on its own, in
/// parentheses, as an argument, an operand and a tuple element, whatever token is
/// before its `.`.
///
/// Mutation-checked by removing the accessor production from `Accessible`: every source
/// below then fails to parse.
#[test]
fn an_accessor_stands_wherever_an_expression_does() {
    assert_eq!(body(".name"), accessor("name"));
    assert_eq!(body("(.name)"), accessor("name"));
    assert_eq!(
        body("f (.name) r"),
        app(app(var("f"), accessor("name")), var("r"))
    );
    assert_eq!(
        body("(.a, .b)"),
        expr_tuple(zelkova_syntax::tuple::Tuple::two(
            accessor("a"),
            accessor("b")
        ))
    );
    assert_eq!(
        body("a |> .name"),
        expr_infix_chain(Box::new(var("a")), vec![(name("|>"), accessor("name"))])
    );
    assert_eq!(
        body("if c then .a else .b"),
        expr_if(
            Box::new(var("c")),
            Box::new(accessor("a")),
            Box::new(accessor("b"))
        )
    );
    // A soft keyword is a label like any other lowercase name.
    assert_eq!(body(".left"), accessor("left"));
}

/// An uppercase name followed by a detached `.` and an attached label is that name
/// applied to an accessor, the same reading `f .name` gets: `Widget .size` is no
/// qualified name. Canonicalization is what rejects it where `Widget` is a module and
/// no constructor (`crates/zelkova-compiler/tests/canonical.rs`). A comment before the
/// `.` separates it as a space does.
///
/// Mutation-checked by making `consume_operator` never yield `AccessorDot` (every
/// non-`Dot` `.` a `SpacedDot`): both sources are then rejected at the `.`.
#[test]
fn a_constructor_before_a_spaced_dot_is_applied_to_an_accessor() {
    assert_eq!(
        body("Widget .size"),
        app(expr_ctor(name("Widget")), accessor("size"))
    );
    assert_eq!(
        body("Widget{- a -}.size"),
        app(expr_ctor(name("Widget")), accessor("size"))
    );
}

/// An accessor's `.` is written against its label: `. name` and `.  name` are no
/// accessor, and each is rejected as `Error::SpacedDot` at the `.`. So is a `.` against
/// an uppercase name, which is no label.
///
/// Mutation-checked by making `consume_operator` yield `AccessorDot` for a `.` with
/// whitespace after it (`before_a_lowercase_name || after_is_apart`): `. name` and `.
/// name` then parse as accessors and `rejected_at_dot` panics on `Ok`.
#[test]
fn an_accessor_is_written_against_its_label() {
    for expression in [". name", ".  name", "f . name", "(. name)", ".Name"] {
        let source = main_is(expression);
        let file = SimpleFile::new("Main.zel".to_owned(), source.clone());
        match parse(&file) {
            Err(Error::SpacedDot { dot, .. }) => {
                let at_dot = source.rfind('.').map(|dot| dot as u32);
                assert_eq!(Some(dot.start.0), at_dot, "for {:?}", expression);
                assert_eq!(dot.end.0, dot.start.0 + 1, "for {:?}", expression);
            }
            other => panic!("expected `SpacedDot` for {:?}, got {:?}", expression, other),
        }
    }
}

/// A label is a lowercase name, so an access naming an uppercase one is rejected at it.
///
/// Mutation-checked by giving the access production a `TypeIdent` alternative for its
/// label: `r.Name` then parses.
#[test]
fn an_access_takes_a_lowercase_label() {
    let source = main_is("r.Name");
    let file = SimpleFile::new("Main.zel".to_owned(), source.clone());
    match parse(&file) {
        Err(Error::UnexpectedToken { token, .. }) => {
            assert_eq!(token.value, Token::UpperIdentifier("Name".to_owned()));
            assert_eq!(Some(token.span), at(&source, "Name"));
        }
        other => panic!("expected an unexpected `Name`, got {:?}", other),
    }
}

/// An access spans the whole `r.name`, from its record's first character, and an
/// accessor the whole `.name`; each carries its label's span on its own, which is where
/// a diagnostic about the field puts its caret.
///
/// Mutation-checked by giving the access's label the node's span (`NodeSpan::new(l, r)`
/// in place of `NodeSpan::new(ll, r)`), and the same for the accessor: the label
/// assertions go red. And by starting the access's span at the `.` (`@L` after the
/// record): the first span assertion goes red.
#[test]
fn an_access_and_an_accessor_span_their_label_apart() {
    let source = main_is("f (g r).centre .x");
    let module = parsed(&source);
    let body = &module.functions[0].bindings[0].body;

    let ExpressionKind::Application(_, accessor_node) = &body.kind else {
        panic!("expected an application, got {:?}", body);
    };
    let ExpressionKind::Accessor(label, label_span) = &accessor_node.kind else {
        panic!("expected an accessor, got {:?}", accessor_node);
    };
    assert_eq!(label.as_str(), "x");
    let dot_x = at(&source, ".x").expect("`.x` is in the source");
    assert_eq!(accessor_node.span.span(), Some(dot_x));
    assert_eq!(
        label_span.span(),
        Some(Span {
            start: BytePos(dot_x.start.0 + 1),
            end: dot_x.end,
        })
    );

    let ExpressionKind::Application(f_applied, _) = &body.kind else {
        unreachable!()
    };
    let ExpressionKind::Application(_, access_node) = &f_applied.kind else {
        panic!("expected `f` applied to the access, got {:?}", f_applied);
    };
    let ExpressionKind::Access(_, label, label_span) = &access_node.kind else {
        panic!("expected an access, got {:?}", access_node);
    };
    assert_eq!(label.as_str(), "centre");
    assert_eq!(access_node.span.span(), at(&source, "(g r).centre"));
    assert_eq!(label_span.span(), at(&source, "centre"));
}
