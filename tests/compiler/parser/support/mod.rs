//! Helpers functions and macros to support testing the frontend
//! part of the compiler.
//!
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::parser::*;
use zelkova_lang::compiler::position::NodeSpan;
use zelkova_lang::compiler::tuple::Tuple;

// macros to simplify tests

#[macro_export]
macro_rules! test_parse_ok {
    ($test_name: ident, $source: expr, $expected: expr $(,)?) => {
        #[test]
        fn $test_name() {
            use codespan_reporting::files::SimpleFile;
            use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
            use codespan_reporting::term::{self};
            use zelkova_lang::compiler::parser;

            let test_name = stringify!($test_name);

            let _ = ::env_logger::try_init();
            let source = indoc::indoc! {$source}.to_string();
            let file = SimpleFile::new(test_name.to_owned(), source);

            let e = parser::parse(&file);

            match e {
                Ok(expr) => assert_eq!(expr, $expected),
                Err(err) => {
                    let writer = StandardStream::stderr(ColorChoice::Auto);
                    let config = codespan_reporting::term::Config {
                        tab_width: 2,
                        ..codespan_reporting::term::Config::default()
                    };

                    term::emit_to_write_style(
                        &mut writer.lock(),
                        &config,
                        &file,
                        &err.diagnostic(()),
                    )
                    .unwrap();
                    assert_eq!(None, Some(err), "{} should not produce an error", test_name);
                }
            }
        }
    };
}

/// The counterpart of `test_parse_ok!` for sources which must *not* parse:
/// run `source` through `parser::parse` and return the layout error it
/// produced, together with the byte range of the rendered diagnostic's primary
/// label. Panics (test failure) if parsing succeeds, or fails with anything
/// other than `Error::Layout`.
///
/// The range is returned alongside the error because `NodeSpan`'s `PartialEq`
/// is blind (see its doc comment and `CLAUDE.md`'s *Standing invariants*), so
/// the diagnostic's label is the only thing that actually pins *where* an
/// error points.
pub fn layout_error(source: &str) -> (layout::LayoutError, std::ops::Range<usize>) {
    use codespan_reporting::files::SimpleFile;

    let file = SimpleFile::new("test".to_owned(), source.to_owned());

    let err = match parse(&file) {
        Ok(module) => panic!(
            "expected a layout error, but parsing succeeded: {:?}",
            module
        ),
        Err(err) => err,
    };

    let layout_err = match &err {
        Error::Layout(layout_err) => layout_err.clone(),
        other => panic!("expected a layout error, got {:?}", other),
    };

    let range = err
        .diagnostic(())
        .labels
        .first()
        .expect("layout error diagnostic has no primary label")
        .range
        .clone();

    (layout_err, range)
}

// AST constructor as simple functions

pub fn name(name: &str) -> Name {
    name.into()
}

// `Expression`, `Pattern` and `Type` are each a `NodeSpan` beside a `…Kind`, so a
// hand-built literal would otherwise read `Expression::bare(ExpressionKind::Lit(..))`
// at every node. One function per variant, taking exactly the variant's arguments,
// keeps the literals below as readable as they were when the enums were the nodes.
// Every one of them uses `no_span()`; see that function for why that is not a loss.

pub fn type_unqualified(name: Name) -> Type {
    Type::bare(TypeKind::Unqualified(name, vec![]))
}

pub fn type_unqualified_with(name: Name, types: Vec<Type>) -> Type {
    Type::bare(TypeKind::Unqualified(name, types))
}

pub fn type_variable(name: Name) -> Type {
    Type::bare(TypeKind::Variable(name))
}

pub fn type_arrow(tpe1: Type, tpe2: Type) -> Type {
    Type::bare(TypeKind::Arrow(Box::new(tpe1), Box::new(tpe2)))
}

pub fn type_tuple2(tpe1: Type, tpe2: Type) -> Type {
    Type::bare(TypeKind::Tuple(Tuple::two(tpe1, tpe2)))
}

pub fn expr_lit(lit: Literal) -> Expression {
    Expression::bare(ExpressionKind::Lit(lit))
}

pub fn expr_var(name: Name) -> Expression {
    Expression::bare(ExpressionKind::Variable(name))
}

pub fn expr_app(f: Box<Expression>, arg: Box<Expression>) -> Expression {
    Expression::bare(ExpressionKind::Application(f, arg))
}

/// A flat run of operator applications — see `ExpressionKind::InfixChain`.
/// Each pair is one operator and the operand to its right; the operator's own
/// span is `no_span()` like everything else a hand-built literal carries,
/// since `NodeSpan`'s blind `PartialEq` makes it compare equal to whatever the
/// parser actually computed.
pub fn expr_infix_chain(first: Box<Expression>, rest: Vec<(Name, Expression)>) -> Expression {
    Expression::bare(ExpressionKind::InfixChain(
        first,
        rest.into_iter().map(|(op, e)| (op, no_span(), e)).collect(),
    ))
}

pub fn expr_tuple(tuple: Tuple<Expression>) -> Expression {
    Expression::bare(ExpressionKind::Tuple(tuple))
}

pub fn expr_if(
    pred: Box<Expression>,
    if_true: Box<Expression>,
    if_false: Box<Expression>,
) -> Expression {
    Expression::bare(ExpressionKind::If(pred, if_true, if_false))
}

/// The span a hand-built AST literal gets: none.
///
/// A literal written in a test cannot know the byte offsets the tokenizer computed,
/// and does not have to — `NodeSpan`'s `PartialEq` always returns `true`, so any span
/// compares equal to the parsed one (see the type's documentation for that trade and
/// its cost). Naming it here makes `span: no_span()` in the literals below read as
/// "deliberately absent" rather than "not filled in yet", and keeps the whole-value
/// `assert_eq!`s about structure, which is what they are for.
pub fn no_span() -> NodeSpan {
    NodeSpan::none()
}
