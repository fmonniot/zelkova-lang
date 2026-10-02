//! The `.` of a qualified name takes no whitespace on either side
//! (`docs/spec/records.md`, *Whitespace before a `.` decides which form it is*).
//!
//! `Widget.size` is one name. `Widget . size`, `Widget .size` and `Widget. size` are not,
//! in an expression, in a type, in an `import` and in a module header, and each is
//! reported as `parser::Error::SpacedDot` pointing at the `.`.
//!
//! Every rejection test below was verified to fail by making `consume_operator` in
//! `tokenizer.rs` yield `Token::Dot` for every lone `.`, which returns the parser to
//! accepting the spacing: the source then parses, and `spaced_dot` panics on `Ok`.

use super::support::*;
use codespan_reporting::files::SimpleFile;
use zelkova_syntax::parser;

/// Parse `source`, which must fail with `Error::SpacedDot`, and return the text of the
/// byte range the error's diagnostic labels, which must be the `.` and nothing else.
///
/// A whole-value `assert_eq!` on the error would pin the variant but not where it points,
/// so the range is read off the rendered diagnostic, as `layout_error` does.
fn spaced_dot(source: &str) -> (usize, String) {
    let file = SimpleFile::new("test".to_owned(), source.to_owned());

    let error = match parser::parse(&file) {
        Ok(module) => panic!("expected `SpacedDot`, but this parsed: {:?}", module),
        Err(error) => error,
    };

    assert!(
        matches!(error, parser::Error::SpacedDot { .. }),
        "expected `SpacedDot` for {:?}, got {:?}",
        source,
        error
    );

    let diagnostic = error.diagnostic(());
    assert_eq!(
        diagnostic.message,
        "a qualified name is written with no spaces around its `.`"
    );

    let range = diagnostic.labels[0].range.clone();

    (range.start, source[range].to_owned())
}

/// Assert that `source` is rejected as `SpacedDot`, pointing at the `.` of the first
/// `needle` it contains.
fn assert_rejected_at_dot(source: &str, needle: &str) {
    let (start, text) = spaced_dot(source);

    assert_eq!(text, ".", "the label should cover exactly the `.`");
    assert_eq!(
        Some(start),
        source
            .find(needle)
            .zip(needle.find('.'))
            .map(|(at, dot)| at + dot),
        "the label should be the `.` of {:?}",
        needle
    );
}

#[test]
fn a_name_in_an_expression_takes_no_space_around_its_dot() {
    for spelling in ["Widget . size", "Widget .size", "Widget. size"] {
        assert_rejected_at_dot(
            &format!("module Main exposing (..)\n\nmain = {}\n", spelling),
            spelling,
        );
    }
}

#[test]
fn a_constructor_in_an_expression_takes_no_space_around_its_dot() {
    for spelling in ["Widget . Size", "Widget .Size", "Widget. Size"] {
        assert_rejected_at_dot(
            &format!("module Main exposing (..)\n\nmain = {}\n", spelling),
            spelling,
        );
    }
}

#[test]
fn a_type_takes_no_space_around_its_dot() {
    for spelling in ["Widget . Size", "Widget .Size", "Widget. Size"] {
        assert_rejected_at_dot(
            &format!(
                "module Main exposing (..)\n\nmain : {}\nmain = Widget.Small\n",
                spelling
            ),
            spelling,
        );
    }
}

#[test]
fn a_pattern_takes_no_space_around_its_dot() {
    assert_rejected_at_dot(
        "module Main exposing (..)\n\nmain x =\n  case x of\n    Widget . Small -> 1\n",
        "Widget . Small",
    );
}

#[test]
fn an_import_takes_no_space_around_its_dot() {
    for spelling in ["Ui . Widget", "Ui .Widget", "Ui. Widget"] {
        assert_rejected_at_dot(
            &format!("module Main exposing (..)\n\nimport {}\n", spelling),
            spelling,
        );
    }
}

#[test]
fn a_module_header_takes_no_space_around_its_dot() {
    for spelling in ["Ui . Widget", "Ui .Widget", "Ui. Widget"] {
        assert_rejected_at_dot(&format!("module {} exposing (..)\n", spelling), spelling);
    }
}

#[test]
fn a_comment_next_to_a_dot_is_whitespace() {
    assert_rejected_at_dot(
        "module Main exposing (..)\n\nmain = Widget{- a -}.size\n",
        "-}.size",
    );
    assert_rejected_at_dot(
        "module Main exposing (..)\n\nmain = Widget.{- a -}size\n",
        "Widget.{",
    );
}

#[test]
fn a_dot_ending_the_line_is_spaced() {
    assert_rejected_at_dot(
        "module Main exposing (..)\n\nmain = Widget.\n  size\n",
        "Widget.",
    );
}

#[test]
fn a_qualified_name_in_an_expression_is_unaffected() {
    let file = SimpleFile::new(
        "test".to_owned(),
        "module Main exposing (..)\n\nmain = Widget.size\n".to_owned(),
    );

    let module = parser::parse(&file).expect("`Widget.size` should parse");

    assert_eq!(
        module.functions[0].bindings[0].body,
        expr_var(name("Widget.size"))
    );
}

/// What `a_name_in_an_expression_takes_no_space_around_its_dot` rejects is the spacing and
/// not the name: the same declarations written with the `.` against both sides parse,
/// qualified types, constructors, patterns, imports and module headers included.
#[test]
fn the_unspaced_spelling_of_each_parses() {
    for source in [
        "module Main exposing (..)\n\nmain = Widget.size\n",
        "module Main exposing (..)\n\nmain = Widget.Small\n",
        "module Main exposing (..)\n\nmain : Widget.Size\nmain = Widget.Small\n",
        "module Main exposing (..)\n\nmain x =\n  case x of\n    Widget.Small -> 1\n",
        "module Main exposing (..)\n\nimport Ui.Widget\n",
        "module Ui.Widget exposing (..)\n",
    ] {
        let file = SimpleFile::new("test".to_owned(), source.to_owned());

        if let Err(error) = parser::parse(&file) {
            panic!("{:?} should parse, got {:?}", source, error);
        }
    }
}
