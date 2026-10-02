//! The `.` of a qualified name takes no whitespace on either side
//! (`docs/spec/records.md`, *Whitespace before a `.` decides which form it is*).
//!
//! `Widget.size` is one name. `Widget . size`, `Widget .size` and `Widget. size` are not,
//! in an expression, in a type, in a pattern, in an `import` and in a module header.
//! Each is reported as `parser::Error::SpacedDot` pointing at the `.`, but for
//! `Widget .size` in an expression, which is `Widget` applied to the accessor `.size`
//! (`a_constructor_before_a_spaced_dot_is_applied_to_an_accessor` in `field_access.rs`),
//! and which canonicalization rejects where `Widget` is a module and not a constructor.
//! The lowercase spelling `Widget .size` is covered in a type, a pattern, an `import` and
//! a module header, where the tokenizer yields an `AccessorDot` and not a `SpacedDot`.
//!
//! Every rejection test below was verified to fail by making `consume_operator` in
//! `tokenizer.rs` yield `Token::Dot` for every lone `.`, which returns the parser to
//! accepting the spacing: the source then parses, and `spaced_dot` panics on `Ok`.
//!
//! The lowercase spellings (`Widget .size`, `Ui .widget`) were verified to fail by
//! deleting `| Token::AccessorDot` from the arm of `From<ParseError>` in `error.rs` that
//! builds `Error::SpacedDot`: the type, pattern, `import` and module-header tests then
//! get an `UnexpectedToken` on `AccessorDot` and fail on the variant.

use super::support::*;
use codespan_reporting::files::SimpleFile;
use zelkova_syntax::parser;

/// Parse `source`, which must fail with `Error::SpacedDot`, and return the diagnostic's
/// message and notes, and the start and text of the byte range it labels.
///
/// A whole-value `assert_eq!` on the error would pin the variant but not where it points,
/// so the range is read off the rendered diagnostic, as `layout_error` does.
fn rejected_dot(source: &str) -> (String, Vec<String>, usize, String) {
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
    let range = diagnostic.labels[0].range.clone();

    (
        diagnostic.message,
        diagnostic.notes,
        range.start,
        source[range].to_owned(),
    )
}

/// `rejected_dot` for a `.` that interrupts a qualified name, whose message is the one
/// about qualified names.
fn spaced_dot(source: &str) -> (usize, String) {
    let (message, _, start, text) = rejected_dot(source);

    assert_eq!(
        message,
        "a qualified name is written with no spaces around its `.`"
    );

    (start, text)
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
    for spelling in ["Widget . size", "Widget. size"] {
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
    for spelling in [
        "Widget . Size",
        "Widget .Size",
        "Widget. Size",
        "Widget .size",
    ] {
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
    for spelling in ["Widget . Small", "Widget .small"] {
        assert_rejected_at_dot(
            &format!(
                "module Main exposing (..)\n\nmain x =\n  case x of\n    {} -> 1\n",
                spelling
            ),
            spelling,
        );
    }
}

#[test]
fn an_import_takes_no_space_around_its_dot() {
    for spelling in ["Ui . Widget", "Ui .Widget", "Ui. Widget", "Ui .widget"] {
        assert_rejected_at_dot(
            &format!("module Main exposing (..)\n\nimport {}\n", spelling),
            spelling,
        );
    }
}

#[test]
fn a_module_header_takes_no_space_around_its_dot() {
    for spelling in ["Ui . Widget", "Ui .Widget", "Ui. Widget", "Ui .widget"] {
        assert_rejected_at_dot(&format!("module {} exposing (..)\n", spelling), spelling);
    }
}

/// A comment after a `.` separates it from the name as a space does. One before it does
/// too, and makes `Widget{- a -}.size` an application to an accessor, as `Widget .size`
/// is (`a_constructor_before_a_spaced_dot_is_applied_to_an_accessor` in `field_access.rs`).
#[test]
fn a_comment_next_to_a_dot_is_whitespace() {
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

/// A `.` that interrupts no qualified name gets no advice about one: there is no name in
/// `. name`, `(. name)`, `1 .`, `a . b`, `r. name`, `(f x) .1` or `type Widget . a` to
/// write in one piece. The message is the neutral one, with no note, and still labels
/// exactly the `.`.
///
/// The grammar would have accepted a `Dot` after `1`, `a`, `r` and `)`, each of which a
/// field access may be written on, so the expected tokens alone would call each of
/// those a qualified name; what keeps them neutral is that no uppercase name stands
/// before the `.`. `type Widget . a` is the other half: an uppercase name stands before
/// its `.`, and a declaration's own name is never qualified, so no `Dot` was expected.
///
/// `case x of .name -> 1` is the one of these where the tokenizer yields an `AccessorDot`
/// (a `.` against a lowercase label, here in a pattern, which has no accessor form), and
/// it is reported as `SpacedDot` too.
///
/// Verified to fail by making `From<ParseError>` set `continues_a_name` to `true`
/// unconditionally: `type Widget . a` then gets the qualified-name message. And by
/// making `after_a_name` in `parser/mod.rs` return its error unchanged: `1 .`, `a . b` and
/// `r. name` then get it. And by deleting `| Token::AccessorDot` from the arm of
/// `From<ParseError>` that builds `Error::SpacedDot`: `case x of .name -> 1` is then an
/// `UnexpectedToken` on `AccessorDot`.
#[test]
fn a_dot_that_interrupts_no_name_is_not_called_a_qualified_name() {
    for source in [
        "module Main exposing (..)\n\nmain = . name\n",
        "module Main exposing (..)\n\nmain = (. name)\n",
        "module Main exposing (..)\n\nmain = 1 .\n",
        "module Main exposing (..)\n\nmain = a . b\n",
        "module Main exposing (..)\n\nmain = r. name\n",
        "module Main exposing (..)\n\nmain = (f x) .1\n",
        "module Main exposing (..)\n\ntype Widget . a = A\n",
        "module Main exposing (..)\n\nmain x =\n  case x of\n    .name -> 1\n",
    ] {
        let (message, notes, start, text) = rejected_dot(source);

        assert_eq!(message, "unexpected `.`", "for {:?}", source);
        assert!(notes.is_empty(), "for {:?}: {:?}", source, notes);
        assert_eq!(text, ".", "for {:?}", source);
        // Each source has one `.` after the `(..)` of its header, and it is the one rejected.
        assert_eq!(Some(start), source.rfind('.'), "for {:?}", source);
    }
}
