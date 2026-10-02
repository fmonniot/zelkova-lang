//! The four record forms of the grammar: a record type, a record, an update and a
//! record pattern (`docs/spec/records.md`). Each test asserts the `TypeKind`,
//! `ExpressionKind` or `PatternKind` the source parses to, not only that it parsed; the
//! ones about positions read the spans directly, since `NodeSpan`'s `PartialEq` is
//! blind.

use super::support::*;
use codespan_reporting::files::SimpleFile;
use zelkova_syntax::parser::tokenizer::Token;
use zelkova_syntax::parser::*;
use zelkova_syntax::position::{BytePos, Span};

fn field<T>(label: &str, value: T) -> Field<T> {
    Field::new(name(label), no_span(), value)
}

fn type_record(fields: Vec<Field<Type>>) -> Type {
    Type::bare(TypeKind::Record(fields))
}

fn expr_record(fields: Vec<Field<Expression>>) -> Expression {
    Expression::bare(ExpressionKind::Record(fields))
}

fn expr_update(record: Expression, fields: Vec<Field<Expression>>) -> Expression {
    Expression::bare(ExpressionKind::Update(Box::new(record), fields))
}

fn parsed(source: &str) -> Module {
    let file = SimpleFile::new("Main.zel".to_owned(), source.to_owned());
    parse(&file).unwrap_or_else(|error| panic!("expected the source to parse, got {:?}", error))
}

/// The annotation of the module's only function.
fn annotation(source: &str) -> Type {
    let module = parsed(source);
    let [function] = module.functions.as_slice() else {
        panic!("expected one function, got {:?}", module.functions);
    };
    function
        .tpe
        .clone()
        .unwrap_or_else(|| panic!("expected `{}` to be annotated", function.name))
}

/// The body of the module's only function.
fn body(source: &str) -> Expression {
    let module = parsed(source);
    let [function] = module.functions.as_slice() else {
        panic!("expected one function, got {:?}", module.functions);
    };
    let [binding] = function.bindings.as_slice() else {
        panic!("expected one binding, got {:?}", function.bindings);
    };
    binding.body.clone()
}

/// The token `source` is rejected at.
fn rejected_at(source: &str) -> (Token, u32) {
    let file = SimpleFile::new("Main.zel".to_owned(), source.to_owned());
    match parse(&file) {
        Ok(module) => panic!("expected a syntax error, got {:?}", module),
        Err(Error::UnexpectedToken { token, .. }) => (token.value, token.span.start.0),
        Err(other) => panic!("expected an unexpected token, got {:?}", other),
    }
}

/// The span of the first occurrence of `needle` in `source`, as a parser node records it.
fn at(source: &str, needle: &str) -> Option<Span<BytePos>> {
    at_from(source, needle, 0)
}

/// The span of the first occurrence of `needle` in `source` at or after byte `from`.
fn at_from(source: &str, needle: &str, from: usize) -> Option<Span<BytePos>> {
    let start = from
        + source[from..]
            .find(needle)
            .unwrap_or_else(|| panic!("`{}` is not in the source", needle));
    Some(Span {
        start: BytePos(start as u32),
        end: BytePos((start + needle.len()) as u32),
    })
}

/// A record type is `TypeKind::Record`, its fields in the order written, each field's
/// type a whole type: a function type and an application are field types like a name.
///
/// Mutation-checked by deleting `AtomicType`'s record production: the annotation is then
/// rejected at its `{`.
#[test]
fn a_record_type_is_its_fields_in_written_order() {
    let tpe = annotation(indoc::indoc! {r#"
        module Main exposing (..)

        f : { b : Maybe Int, a : Int -> Int }
    "#});

    assert_eq!(
        tpe,
        type_record(vec![
            field(
                "b",
                type_unqualified_with(name("Maybe"), vec![type_unqualified(name("Int"))]),
            ),
            field(
                "a",
                type_arrow(type_unqualified(name("Int")), type_unqualified(name("Int"))),
            ),
        ])
    );
}

/// A record type is atomic: it is an argument with no parentheses around it, it may be
/// either side of an arrow, and a field's type may be another record.
///
/// Mutation-checked by moving the record production from `AtomicType` to `Type`: an
/// application's argument is an `AtomicType`, so the annotation is then rejected at the
/// `{` after `Box`.
#[test]
fn a_record_type_is_an_argument_without_parentheses() {
    let tpe = annotation(indoc::indoc! {r#"
        module Main exposing (..)

        f : Box { inner : { x : Int } } -> { y : Int }
    "#});

    assert_eq!(
        tpe,
        type_arrow(
            type_unqualified_with(
                name("Box"),
                vec![type_record(vec![field(
                    "inner",
                    type_record(vec![field("x", type_unqualified(name("Int")))]),
                )])],
            ),
            type_record(vec![field("y", type_unqualified(name("Int")))]),
        )
    );
}

/// A record inside a variant is the constructor's argument, which is how a recursive
/// shape holds a record.
///
/// Mutation-checked with the test above, by deleting `AtomicType`'s record production
/// and by moving it to `Type`: either way the variant is rejected at its `{`.
#[test]
fn a_record_type_is_a_constructors_argument() {
    let module = parsed(indoc::indoc! {r#"
        module Main exposing (..)

        type Chain
          = End
          | Link { value : Int, next : Chain }
    "#});

    assert_eq!(
        module.types[0].variants[1],
        type_unqualified_with(
            name("Link"),
            vec![type_record(vec![
                field("value", type_unqualified(name("Int"))),
                field("next", type_unqualified(name("Chain"))),
            ])],
        )
    );
}

/// A record is `ExpressionKind::Record`, and its fields stay in the order they were
/// written — `b` before `a` — because they are subexpressions and are evaluated in that
/// order.
///
/// Mutation-checked twice: deleting the record arm of `AtomicExpr`'s brace production
/// (the source is then rejected), and sorting `fields` by label in that production (the
/// assertion then sees `a` first).
#[test]
fn a_record_keeps_its_fields_in_written_order() {
    let expr = body(indoc::indoc! {r#"
        module Main exposing (..)

        main = { b = f x, a = 2 }
    "#});

    assert_eq!(
        expr,
        expr_record(vec![
            field(
                "b",
                expr_app(Box::new(expr_var(name("f"))), Box::new(expr_var(name("x"))),),
            ),
            field("a", expr_lit(Literal::Int(2))),
        ])
    );
}

/// A record is atomic, so it is an argument with no parentheses: `f { a = 1 }` applies
/// `f` to the record.
///
/// Mutation-checked by making `AtomicExpr`'s brace production read `{ {` before its
/// fields: the source is then rejected.
#[test]
fn a_record_is_an_argument_without_parentheses() {
    let expr = body(indoc::indoc! {r#"
        module Main exposing (..)

        main = f { a = 1 } 2
    "#});

    assert_eq!(
        expr,
        expr_app(
            Box::new(expr_app(
                Box::new(expr_var(name("f"))),
                Box::new(expr_record(vec![field("a", expr_lit(Literal::Int(1)))])),
            )),
            Box::new(expr_lit(Literal::Int(2))),
        )
    );
}

/// An update is `ExpressionKind::Update`: the record it updates, then its fields in the
/// order written.
///
/// Mutation-checked by building `ExpressionKind::Record(fields)` in the update arm of
/// `AtomicExpr`'s brace production, dropping the head: the assertion then sees a record.
#[test]
fn an_update_is_its_record_and_its_fields() {
    let expr = body(indoc::indoc! {r#"
        module Main exposing (..)

        main = { r | taken = 1, expected = 2 }
    "#});

    assert_eq!(
        expr,
        expr_update(
            expr_var(name("r")),
            vec![
                field("taken", expr_lit(Literal::Int(1))),
                field("expected", expr_lit(Literal::Int(2))),
            ],
        )
    );
}

/// The left operand of an update's `|` is a whole expression rather than a name:
/// `{ f x | a = 1 }` updates whatever `f x` returns, and `{ a + b | c = 1 }` whatever the
/// operator returns.
///
/// Mutation-checked as the test above is, by dropping the head in the update arm.
#[test]
fn an_updates_record_is_any_expression() {
    assert_eq!(
        body(indoc::indoc! {r#"
            module Main exposing (..)

            main = { f x | a = 1 }
        "#}),
        expr_update(
            expr_app(Box::new(expr_var(name("f"))), Box::new(expr_var(name("x"))),),
            vec![field("a", expr_lit(Literal::Int(1)))],
        )
    );

    assert_eq!(
        body(indoc::indoc! {r#"
            module Main exposing (..)

            main = { a + b | c = 1 }
        "#}),
        expr_update(
            expr_infix_chain(
                Box::new(expr_var(name("a"))),
                vec![(name("+"), expr_var(name("b")))],
            ),
            vec![field("c", expr_lit(Literal::Int(1)))],
        )
    );
}

/// A record written across several lines with the separator leading each line, inside a
/// `case` branch so the layout pass's blocks are around it, is the same record.
///
/// Mutation-checked by dropping the head in the update arm, as above: the nested update
/// then reads as a record.
#[test]
fn a_record_may_lead_each_line_with_its_separator() {
    let expr = body(indoc::indoc! {r#"
        module Main exposing (..)

        main =
          case x of
            _ ->
              { taken = 1
              , expected = { r
                | a = 2
                }
              }
    "#});

    let ExpressionKind::Case(_, branches) = &expr.kind else {
        panic!("expected a case, got {:?}", expr.kind);
    };
    assert_eq!(
        branches[0].expression,
        expr_record(vec![
            field("taken", expr_lit(Literal::Int(1))),
            field(
                "expected",
                expr_update(
                    expr_var(name("r")),
                    vec![field("a", expr_lit(Literal::Int(2)))],
                ),
            ),
        ])
    );
}

/// A label is spelled the way a value name is, so a soft keyword is a label like any
/// other lowercase name.
///
/// Mutation-checked by spelling `ExprField`'s label `"lo_ident"` in place of `VarIdent`:
/// the source is then rejected at `left`.
#[test]
fn a_soft_keyword_is_a_label() {
    assert_eq!(
        body(indoc::indoc! {r#"
            module Main exposing (..)

            main = { left = 1, unsafe = 2 }
        "#}),
        expr_record(vec![
            field("left", expr_lit(Literal::Int(1))),
            field("unsafe", expr_lit(Literal::Int(2))),
        ])
    );
}

/// Each field's span is its label's alone, which is where a diagnostic about the label
/// points, and the record's span runs from its `{` to its `}`.
///
/// Mutation-checked by giving `ExprField` and `TypeField` the span `NodeSpan::none()`, and
/// separately by moving `AtomicExpr`'s brace production's `<r:@R>` before the `"}"`: each
/// turns an assertion red.
#[test]
fn a_field_is_spanned_at_its_label_and_a_record_at_its_braces() {
    let source = indoc::indoc! {r#"
        module Main exposing (..)

        main : { taken : Int, expected : Int }
        main = { taken = 1, expected = 2 }
    "#};
    let module = parsed(source);
    let function = &module.functions[0];
    let value_line = source.find("main =").expect("the binding is in the source");

    let tpe = function.tpe.as_ref().expect("`main` is annotated");
    let TypeKind::Record(type_fields) = &tpe.kind else {
        panic!("expected a record type, got {:?}", tpe.kind);
    };
    assert_eq!(
        tpe.span.span(),
        at(source, "{ taken : Int, expected : Int }")
    );
    assert_eq!(type_fields[0].label_span.span(), at(source, "taken"));
    assert_eq!(type_fields[1].label_span.span(), at(source, "expected"));

    let expr = &function.bindings[0].body;
    let ExpressionKind::Record(fields) = &expr.kind else {
        panic!("expected a record, got {:?}", expr.kind);
    };
    assert_eq!(
        expr.span.span(),
        at_from(source, "{ taken = 1, expected = 2 }", value_line)
    );
    assert_eq!(
        fields[0].label_span.span(),
        at_from(source, "taken", value_line)
    );
    assert_eq!(
        fields[1].label_span.span(),
        at_from(source, "expected", value_line)
    );
    assert_eq!(
        fields[1].value.span.span(),
        at_from(source, "2", value_line)
    );
}

/// A trailing comma is an error, reported at the `}` after it, in every form: the
/// separator leading each line already buys what one would.
///
/// Mutation-checked by replacing `CommaOne` with the `Comma` macro, which allows one:
/// each source then parses.
#[test]
fn a_trailing_comma_is_rejected_at_the_brace() {
    let cases = [
        "module Main exposing (..)\n\nf = { a = 1, }\n",
        "module Main exposing (..)\n\nf = { r | a = 1, }\n",
        "module Main exposing (..)\n\nf : { a : Int, }\n",
    ];

    for source in cases {
        let brace = source.rfind('}').expect("the source has a closing brace") as u32;
        assert_eq!(rejected_at(source), (Token::RBrace, brace), "{}", source);
    }
}

/// A record has at least one field, so `{}` is neither a record nor a record type and is
/// rejected at its `}`; so is an update naming no field.
///
/// Mutation-checked by replacing `CommaOne` with the `Comma` macro, which allows none:
/// each source then parses.
#[test]
fn a_record_with_no_field_is_rejected() {
    let cases = [
        "module Main exposing (..)\n\nf = {}\n",
        "module Main exposing (..)\n\nf = { r | }\n",
        "module Main exposing (..)\n\nf : {}\n",
    ];

    for source in cases {
        let brace = source.rfind('}').expect("the source has a closing brace") as u32;
        assert_eq!(rejected_at(source), (Token::RBrace, brace), "{}", source);
    }
}

/// The two spellings belong to their own grammars: `:` is a field of a record type and
/// `=` a field of a record, and neither is read in the other's position.
///
/// Mutation-checked by giving `ExprField` a second alternative reading `label : expr`: the
/// first source then parses.
#[test]
fn a_field_is_spelled_for_its_grammar() {
    let expression = "module Main exposing (..)\n\nf = { a : 1 }\n";
    let colon = expression.find(':').expect("the source has a colon") as u32;
    assert_eq!(rejected_at(expression), (Token::Colon, colon));

    let annotation = "module Main exposing (..)\n\nf : { a = Int }\n";
    let equal = annotation.rfind('=').expect("the source has an equal sign") as u32;
    assert_eq!(rejected_at(annotation), (Token::Equal, equal));
}

// ── Record patterns ──────────────────────────────────────────────────────────

fn pattern_record(fields: Vec<Field<Pattern>>) -> Pattern {
    Pattern::bare(PatternKind::Record(fields))
}

fn pattern_var(variable: &str) -> Pattern {
    Pattern::bare(PatternKind::Variable(name(variable)))
}

fn pattern_ctor_with(constructor: &str, args: Vec<Pattern>) -> Pattern {
    Pattern::bare(PatternKind::Constructor(name(constructor), args))
}

/// The parameters of the module's only function.
fn parameters(source: &str) -> Vec<Pattern> {
    let module = parsed(source);
    let [function] = module.functions.as_slice() else {
        panic!("expected one function, got {:?}", module.functions);
    };
    let [binding] = function.bindings.as_slice() else {
        panic!("expected one binding, got {:?}", function.bindings);
    };
    binding.patterns.clone()
}

/// The patterns of the `case` branches in the body of the module's only function.
fn branch_patterns(source: &str) -> Vec<Pattern> {
    let ExpressionKind::Case(_, branches) = body(source).kind else {
        panic!("expected the body to be a `case`");
    };
    branches.into_iter().map(|branch| branch.pattern).collect()
}

/// `{ x }` is the shorthand for `{ x = x }`: the grammar builds the entry `x` holding a
/// variable pattern `x`, the one shape a record pattern has, and both spans the
/// shorthand builds — the entry's label and the variable's — are the label's.
///
/// Mutation-checked twice: building the shorthand's value as `PatternKind::Anything` in
/// `PatternField`'s bare alternative (the shape assertion goes red), and giving that
/// value `NodeSpan::none()` (the variable's range assertion goes red).
#[test]
fn the_shorthand_is_its_label_bound_as_a_variable() {
    let source = indoc::indoc! {r#"
        module Main exposing (..)

        nameOf { name } = name
    "#};

    let patterns = parameters(source);
    assert_eq!(
        patterns,
        vec![pattern_record(vec![field("name", pattern_var("name"))])]
    );

    let PatternKind::Record(fields) = &patterns[0].kind else {
        panic!("expected a record pattern, got {:?}", patterns[0].kind);
    };
    let label = at(source, "name }");
    let label = label.map(|span| Span {
        start: span.start,
        end: BytePos(span.start.0 + "name".len() as u32),
    });
    assert_eq!(fields[0].label_span.span(), label);
    assert_eq!(fields[0].value.span.span(), label);
    assert_eq!(patterns[0].span.span(), at(source, "{ name }"));
}

/// `label = pattern` keeps its pattern, and the entries stay in the order written —
/// `b` before `a` — beside a shorthand entry of the same pattern.
///
/// Mutation-checked twice: deleting `Pattern`'s record production (the source is then
/// rejected at its `{`), and sorting `fields` by label in it (the assertion then sees
/// `a` first).
#[test]
fn a_record_pattern_keeps_its_entries_in_written_order() {
    assert_eq!(
        parameters(indoc::indoc! {r#"
            module Main exposing (..)

            f { b = _, a = 1, c } = c
        "#}),
        vec![pattern_record(vec![
            field("b", Pattern::bare(PatternKind::Anything)),
            field("a", Pattern::bare(PatternKind::Literal(Literal::Int(1)))),
            field("c", pattern_var("c")),
        ])]
    );
}

/// A record pattern nests in both directions: a record inside a field, a constructor —
/// nullary bare, applied in parentheses — inside a field, and a record pattern as an
/// applied constructor's argument inside a tuple element, with no parentheses around the
/// record itself.
///
/// Mutation-checked by moving the record production from `Pattern` to `CasePattern`:
/// every position here is a `Pattern`, so the source is then rejected at the first `{`.
#[test]
fn a_record_pattern_nests_in_both_directions() {
    let patterns = parameters(indoc::indoc! {r#"
        module Main exposing (..)

        f ((Reading { centre = { x }, taken = Celsius, expected = (Kelvin k) }), n) = x
    "#});

    assert_eq!(
        patterns,
        vec![Pattern::bare(PatternKind::Tuple(
            zelkova_syntax::tuple::Tuple::two(
                pattern_ctor_with(
                    "Reading",
                    vec![pattern_record(vec![
                        field("centre", pattern_record(vec![field("x", pattern_var("x"))])),
                        field("taken", pattern_ctor_with("Celsius", vec![])),
                        field(
                            "expected",
                            pattern_ctor_with("Kelvin", vec![pattern_var("k")]),
                        ),
                    ])],
                ),
                pattern_var("n"),
            )
        ))]
    );
}

/// A record pattern is the argument of a constructor written bare at a `case` branch's
/// head, and a branch head of its own.
///
/// Mutation-checked with the test above, by moving the record production from `Pattern`
/// to `CasePattern`: the first branch's argument is a `Pattern`, so the source is then
/// rejected at its `{`.
#[test]
fn a_record_pattern_is_a_branch_head_and_a_bare_constructors_argument() {
    assert_eq!(
        branch_patterns(indoc::indoc! {r#"
            module Main exposing (..)

            f r =
              case r of
                Reading { centre = { x } } ->
                  x

                { expected } ->
                  expected
        "#}),
        vec![
            pattern_ctor_with(
                "Reading",
                vec![pattern_record(vec![field(
                    "centre",
                    pattern_record(vec![field("x", pattern_var("x"))]),
                )])],
            ),
            pattern_record(vec![field("expected", pattern_var("expected"))]),
        ]
    );
}

/// `{}` names no field and `{ x, }` ends on a comma, and each is rejected at its `}`, as
/// a record and a record type are.
///
/// Mutation-checked twice: replacing `CommaOne` with the `Comma` macro in `Pattern`'s
/// record production (each source then parses), and deleting that production (each is
/// then rejected at its `{`).
#[test]
fn a_record_pattern_of_no_entry_or_a_trailing_comma_is_rejected() {
    for source in [
        "module Main exposing (..)\n\nf {} = 1\n",
        "module Main exposing (..)\n\nf { x, } = 1\n",
    ] {
        let brace = source.rfind('}').expect("the source has a closing brace") as u32;
        assert_eq!(rejected_at(source), (Token::RBrace, brace), "{}", source);
    }
}

/// An entry is a lowercase label, alone or followed by `=` and a pattern: `{ x = }` is
/// rejected at the `}` where its pattern should be, `{ X }` at the uppercase name, and
/// `{ x y }` at the second name, which no separator divides from the first.
///
/// Mutation-checked by deleting `Pattern`'s record production: each source is then
/// rejected at its `{` and the positions go red.
#[test]
fn a_malformed_record_pattern_entry_is_rejected_where_it_goes_wrong() {
    let cases = [
        (
            "module Main exposing (..)\n\nf { x = } = 1\n",
            Token::RBrace,
            "} =",
        ),
        (
            "module Main exposing (..)\n\nf { X } = 1\n",
            Token::UpperIdentifier("X".to_owned()),
            "X",
        ),
        (
            "module Main exposing (..)\n\nf { x y } = 1\n",
            Token::LowerIdentifier("y".to_owned()),
            "y",
        ),
    ];

    for (source, token, needle) in cases {
        let position = source.find(needle).expect("the source has the token") as u32;
        assert_eq!(rejected_at(source), (token, position), "{}", source);
    }
}

/// An entry is spanned at its label and keeps its pattern's own span, and the record
/// pattern spans its braces.
///
/// Mutation-checked by giving `PatternField`'s `label = pattern` alternative the span
/// `NodeSpan::none()`: the label assertions go red.
#[test]
fn a_record_pattern_entry_is_spanned_at_its_label() {
    let source = indoc::indoc! {r#"
        module Main exposing (..)

        f { taken = t, expected = _ } = t
    "#};
    let patterns = parameters(source);
    let PatternKind::Record(fields) = &patterns[0].kind else {
        panic!("expected a record pattern, got {:?}", patterns[0].kind);
    };

    assert_eq!(
        patterns[0].span.span(),
        at(source, "{ taken = t, expected = _ }")
    );
    assert_eq!(fields[0].label_span.span(), at(source, "taken"));
    assert_eq!(
        fields[0].value.span.span(),
        at(source, "t,").map(|span| Span {
            start: span.start,
            end: BytePos(span.start.0 + 1),
        })
    );
    assert_eq!(fields[1].label_span.span(), at(source, "expected"));
}
