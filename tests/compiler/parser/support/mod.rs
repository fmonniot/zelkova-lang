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

// TODO test_parse_error(source, error) -> macro (test_name, source, error)

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
