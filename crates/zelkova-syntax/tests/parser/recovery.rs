//! `TOOL-4`: a syntax error ends the declaration it is in, not the module.
//!
//! `parser::parse_recovering` cuts the token stream into one chunk per top-level
//! declaration before the layout pass and parses each on its own, so these tests run a
//! module with errors in more than one declaration and look at every failure and at what
//! survived. `NodeSpan`'s `PartialEq` is blind, so positions are asserted on the rendered
//! diagnostic's primary label and on `Failure::span`, both of which are real byte offsets.
use codespan_reporting::files::SimpleFile;
use zelkova_syntax::name::Name;
use zelkova_syntax::parser::tokenizer::Token;
use zelkova_syntax::parser::{self, Error, Failure, Module, Parsed};

fn parse_recovering(source: &str) -> Parsed {
    let file = SimpleFile::new("test".to_owned(), source.to_owned());
    parser::parse_recovering(&file)
}

/// Where `error`'s rendered diagnostic points: the start of its primary label.
fn error_start(error: &Error) -> usize {
    error
        .diagnostic(())
        .labels
        .first()
        .unwrap_or_else(|| panic!("{:?} renders with no label", error))
        .range
        .start
}

/// The names of the functions `module` holds, sorted: `Module::from_declarations` groups
/// them through a `HashMap`, so their order carries nothing.
fn function_names(module: &Module) -> Vec<Name> {
    let mut names: Vec<Name> = module.functions.iter().map(|f| f.name.clone()).collect();
    names.sort_by_key(|name| name.to_string());
    names
}

/// Assert that `failure` covers the declaration starting at `declaration_start` and that
/// its error points inside that declaration's text, before `declaration_end`.
fn assert_inside(
    failure: &Failure,
    declaration_start: usize,
    declaration_end: usize,
    description: &str,
) {
    assert_eq!(
        failure.span.start.0 as usize, declaration_start,
        "{}: the failure starts at its declaration",
        description
    );
    let at = error_start(&failure.error);
    assert!(
        (declaration_start..declaration_end).contains(&at),
        "{}: the error at {} is outside {}..{}: {:?}",
        description,
        at,
        declaration_start,
        declaration_end,
        failure.error
    );
}

/// Two declarations with a syntax error each, one well-formed declaration between them
/// and one after: both errors come back, in source order and each inside its own
/// declaration, and the module holds the two that parsed.
///
/// Verified to fail by making `Chunks::cuts` in `crates/zelkova-syntax/src/parser/chunk.rs` return
/// `false`, so the cut never fires: the whole module is then one chunk, whose first error
/// is the only failure and which leaves no module.
#[test]
fn every_declaration_with_a_syntax_error_is_reported_and_the_others_are_kept() {
    let source = indoc::indoc! {"
        module Main exposing (..)

        f x =
          x + )

        ok = 1

        g = = 2

        after = 3
    "};
    let f = source.find("f x").unwrap();
    let ok = source.find("ok =").unwrap();
    let g = source.find("g =").unwrap();
    let after = source.find("after =").unwrap();

    let Parsed { module, failures } = parse_recovering(source);

    assert_eq!(failures.len(), 2, "got {:?}", failures);
    assert_inside(&failures[0], f, ok, "the `)` in `f`");
    assert_eq!(
        error_start(&failures[0].error),
        source.find("+ )").unwrap() + 2
    );
    assert_inside(&failures[1], g, after, "the second `=` in `g`");
    assert_eq!(error_start(&failures[1].error), g + "g = ".len());

    let module = module.expect("the header parsed, so the module is there");
    assert_eq!(module.name, Name::from("Main"));
    assert_eq!(
        function_names(&module),
        vec![Name::from("after"), Name::from("ok")]
    );
}

/// A `case` whose scrutinee is still open when the next declaration starts in column 1.
/// The cut ends the first declaration before `g`, its blocks close there, and the grammar
/// reports the missing `of`; `g` parses.
///
/// This is one of the two reports `TOOL-4` changes: before the cut, `g` was read while the
/// scrutinee was open and the layout pass reported it as *not indented far enough*.
///
/// Verified to fail by making `Chunks::cuts` return `false`: the module is then one chunk
/// and the failure is that layout error, on `g`. Restoring `Position::new(0, 1, 1)` as the
/// end of input in `Layout::next_token` reds it too, since the blocks then close at byte 0.
#[test]
fn a_case_left_without_of_ends_at_the_next_declaration() {
    let source = indoc::indoc! {"
        module Main exposing (..)

        f x =
          case x

        g = 1
    "};
    let g = source.find("g = 1").unwrap();

    let Parsed { module, failures } = parse_recovering(source);

    assert_eq!(failures.len(), 1, "got {:?}", failures);
    match &failures[0].error {
        Error::UnexpectedToken { token, expected } => {
            assert_eq!(token.value, Token::CloseBlock);
            assert_eq!(token.span.start.0 as usize, g);
            assert_eq!(expected, &vec!["of".to_string()]);
        }
        other => panic!("expected the grammar to ask for `of`, got {:?}", other),
    }

    let module = module.expect("the header parsed, so the module is there");
    assert_eq!(function_names(&module), vec![Name::from("g")]);
}

/// A tokenizer error belongs to the declaration it is in: an odd indentation in one
/// declaration and a grammar error in a later one are both reported, the first as the
/// tokenizer's.
///
/// Verified to fail by making `Chunks::cuts` return `false`: the tokenizer's error is
/// then the module's only one.
#[test]
fn a_tokenizer_error_ends_only_its_own_declaration() {
    let source = indoc::indoc! {"
        module Main exposing (..)

        f x =
           x

        ok = 1

        g = = 2
    "};
    let f = source.find("f x").unwrap();
    let ok = source.find("ok =").unwrap();
    let g = source.find("g =").unwrap();

    let Parsed { module, failures } = parse_recovering(source);

    assert_eq!(failures.len(), 2, "got {:?}", failures);
    assert!(
        matches!(failures[0].error, Error::Tokenizer(_)),
        "expected the odd indentation first, got {:?}",
        failures[0].error
    );
    assert_inside(&failures[0], f, ok, "the odd indentation in `f`");
    assert!(
        matches!(failures[1].error, Error::UnexpectedToken { .. }),
        "expected the grammar's error second, got {:?}",
        failures[1].error
    );
    assert_inside(&failures[1], g, source.len(), "the second `=` in `g`");

    let module = module.expect("the header parsed, so the module is there");
    assert_eq!(function_names(&module), vec![Name::from("ok")]);
}

/// A declaration left unfinished on the last line of a file is reported at the end of the
/// file, where its blocks are closed, rather than at its first byte.
///
/// Verified to fail by restoring `let position = Position::new(0, 1, 1);` as the end of
/// input in `Layout::next_token`: the error then points at byte 0.
#[test]
fn a_declaration_unfinished_at_the_end_of_the_file_is_reported_there() {
    let source = indoc::indoc! {"
        module Main exposing (..)

        f x =
    "};

    let Parsed { failures, .. } = parse_recovering(source);

    assert_eq!(failures.len(), 1, "got {:?}", failures);
    assert!(
        matches!(
            &failures[0].error,
            Error::UnexpectedToken { token, .. } if token.value == Token::CloseBlock
        ),
        "got {:?}",
        failures[0].error
    );
    assert_eq!(error_start(&failures[0].error), source.len());
    assert_eq!(failures[0].span.end.0 as usize, source.len());
}

/// The same declaration left unfinished in the middle of a file is reported at the first
/// byte of the declaration after it, which is where its blocks are closed.
///
/// Verified to fail by restoring `let position = Position::new(0, 1, 1);` as the end of
/// input in `Layout::next_token`: the error then points at byte 0.
#[test]
fn a_declaration_unfinished_before_another_is_reported_at_the_next_one() {
    let source = indoc::indoc! {"
        module Main exposing (..)

        f x =

        g = 1
    "};
    let g = source.find("g = 1").unwrap();

    let Parsed { module, failures } = parse_recovering(source);

    assert_eq!(failures.len(), 1, "got {:?}", failures);
    assert_eq!(error_start(&failures[0].error), g);
    assert_eq!(failures[0].span.end.0 as usize, g);

    let module = module.expect("the header parsed, so the module is there");
    assert_eq!(function_names(&module), vec![Name::from("g")]);
}

/// A module header that does not parse leaves no module, and the declarations after it
/// are still parsed for their own errors. `parse` keeps its meaning: the first failure's
/// error, which here is the header's.
///
/// Verified to fail by making `parse` return the last failure's error, ahead of the
/// header's: it is then `g`'s.
#[test]
fn a_broken_header_leaves_no_module_and_parse_reports_it_first() {
    let source = indoc::indoc! {"
        module Main exposing ( , )

        ok = 1

        g = = 2
    "};
    let ok = source.find("ok =").unwrap();
    let g = source.find("g =").unwrap();

    let Parsed { module, failures } = parse_recovering(source);

    assert!(module.is_none(), "got {:?}", module);
    assert_eq!(failures.len(), 2, "got {:?}", failures);
    assert_inside(&failures[0], 0, ok, "the header");
    assert_inside(&failures[1], g, source.len(), "the second `=` in `g`");

    let file = SimpleFile::new("test".to_owned(), source.to_owned());
    let first = parser::parse(&file).expect_err("the header does not parse");
    assert_eq!(first, failures[0].error);
}

/// A column-1 token no declaration starts with does not cut: it stays in the chunk of the
/// declaration above, which the layout pass closes at it, and the grammar rejects the
/// token itself with the tokens a declaration can start with as expected. That is the
/// report a whole module gave before the cut, and the declarations after it still parse.
///
/// Verified to fail by making the `Decls` entry point in `grammar.lalrpop` read a single
/// `Decl` instead of `Decl+`: the grammar then rejects the `OpenBlock` the layout pass
/// puts before `)`, expecting nothing.
#[test]
fn a_stray_column_1_token_is_rejected_as_it_was_before_the_cut() {
    let source = indoc::indoc! {"
        module Main exposing (..)

        f = 1
        )

        g = 2
    "};
    let stray = source.find("\n)").unwrap() + 1;

    let Parsed { module, failures } = parse_recovering(source);

    assert_eq!(failures.len(), 1, "got {:?}", failures);
    match &failures[0].error {
        Error::UnexpectedToken { token, expected } => {
            assert_eq!(token.value, Token::RPar);
            assert_eq!(token.span.start.0 as usize, stray);
            assert!(
                expected.iter().any(|e| e == "lo_ident") && expected.iter().any(|e| e == "type"),
                "expected the tokens a declaration starts with, got {:?}",
                expected
            );
        }
        other => panic!("expected the grammar to reject `)`, got {:?}", other),
    }

    let module = module.expect("the header parsed, so the module is there");
    assert_eq!(function_names(&module), vec![Name::from("g")]);
}
