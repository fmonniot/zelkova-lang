use super::support::*;
use zelkova_lang::compiler::parser;
use zelkova_lang::compiler::parser::*;

// module

test_parse_ok!(
    module_js,
    r#"
    module javascript Maybe exposing ( map )
    "#,
    Module {
        name: name("Maybe"),
        binding_javascript: true,
        exposing: Exposing::Explicit(vec![Exposed::Lower(name("map"))]),
        imports: vec![],
        infixes: vec![],
        types: vec![],
        functions: vec![],
    }
);

// exposing

test_parse_ok!(
    exposing,
    r#"
    module Maybe exposing
        ( Maybe(..), Option
        , andThen
        , map, map2, map3
        , withDefault
        )
    "#,
    Module {
        name: name("Maybe"),
        binding_javascript: false,
        exposing: Exposing::Explicit(vec![
            Exposed::Upper(name("Maybe"), Privacy::Public),
            Exposed::Upper(name("Option"), Privacy::Private),
            Exposed::Lower(name("andThen")),
            Exposed::Lower(name("map")),
            Exposed::Lower(name("map2")),
            Exposed::Lower(name("map3")),
            Exposed::Lower(name("withDefault")),
        ]),
        imports: vec![],
        infixes: vec![],
        types: vec![],
        functions: vec![],
    }
);

// imports

test_parse_ok!(
    import_private,
    r#"
    module Maybe exposing (..)

    import List
    import List as L
    "#,
    Module {
        name: name("Maybe"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![
            Import {
                span: no_span(),
                name: name("List"),
                alias: None,
                exposing: Exposing::Explicit(vec![]),
            },
            Import {
                span: no_span(),
                name: name("List"),
                alias: Some(name("L")),
                exposing: Exposing::Explicit(vec![]),
            }
        ],
        infixes: vec![],
        types: vec![],
        functions: vec![],
    }
);

test_parse_ok!(
    import_open,
    r#"
    module Maybe exposing (..)

    import List exposing (..)
    import List as L exposing (..)
    "#,
    Module {
        name: name("Maybe"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![
            Import {
                span: no_span(),
                name: name("List"),
                alias: None,
                exposing: Exposing::Open,
            },
            Import {
                span: no_span(),
                name: name("List"),
                alias: Some(name("L")),
                exposing: Exposing::Open,
            }
        ],
        infixes: vec![],
        types: vec![],
        functions: vec![],
    }
);

test_parse_ok!(
    import_selective,
    r#"
    module Maybe exposing (..)

    import List exposing ( map, foldl )
    import Maybe exposing ( Maybe )
    import Maybe exposing ( Maybe(..) )
    "#,
    Module {
        name: name("Maybe"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![
            Import {
                span: no_span(),
                name: name("List"),
                alias: None,
                exposing: Exposing::Explicit(vec![
                    Exposed::Lower(name("map")),
                    Exposed::Lower(name("foldl")),
                ]),
            },
            Import {
                span: no_span(),
                name: name("Maybe"),
                alias: None,
                exposing: Exposing::Explicit(vec![
                    Exposed::Upper(name("Maybe"), Privacy::Private,)
                ]),
            },
            Import {
                span: no_span(),
                name: name("Maybe"),
                alias: None,
                exposing: Exposing::Explicit(vec![Exposed::Upper(name("Maybe"), Privacy::Public,)]),
            }
        ],
        infixes: vec![],
        types: vec![],
        functions: vec![],
    }
);

// infixes

test_parse_ok!(
    infix_right,
    r#"
    module Maybe exposing (..)

    infix right 0 (<|) = apL    
    "#,
    Module {
        name: name("Maybe"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![Infix {
            span: no_span(),
            operator: name("<|"),
            associativity: Associativity::Right,
            precedence: 0,
            function_name: name("apL"),
        }],
        types: vec![],
        functions: vec![],
    }
);

test_parse_ok!(
    infix_left,
    r#"
    module Maybe exposing (..)

    infix left  7 (//) = idiv
    "#,
    Module {
        name: name("Maybe"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![Infix {
            span: no_span(),
            operator: name("//"),
            associativity: Associativity::Left,
            precedence: 7,
            function_name: name("idiv"),
        }],
        types: vec![],
        functions: vec![],
    }
);

test_parse_ok!(
    infix_non,
    r#"
    module Maybe exposing (..)

    infix non   4 (==) = eq
    "#,
    Module {
        name: name("Maybe"),
        binding_javascript: false,
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![Infix {
            span: no_span(),
            operator: name("=="),
            associativity: Associativity::None,
            precedence: 4,
            function_name: name("eq"),
        }],
        types: vec![],
        functions: vec![],
    }
);

// spans

/// `ERR-3`: the one parser test that pins a position rather than a shape.
///
/// Every `test_parse_ok!` above compares a whole `Module` against a hand-built
/// literal, and `NodeSpan`'s `PartialEq` always returns `true` — so those assertions
/// verify nothing at all about where anything parsed: they pass whether the spans are
/// right, wrong or absent. That blindness is a deliberate trade (see `NodeSpan`'s
/// documentation), and this test is the other half of it. Without it the whole span
/// plumbing would be green and unverified.
///
/// It checks the merge in particular: a `Function` is assembled from a `FunType` and
/// a `FunBinding` the grammar saw as separate declarations, and its span has to cover
/// both. It also checks the span was captured *inside* the declaration productions
/// rather than around the `Decl` wrapper — the layout pass emits `OpenBlock` and
/// `CloseBlock` zero-width at the start of the block, so a span taken there would
/// begin before the annotation's first character.
///
/// Mutation-checked three ways, each red on its own: making `FunBinding` emit
/// `NodeSpan::none()` (the end collapses onto the annotation); making `FunType` emit
/// `NodeSpan::none()` (the start jumps to the body); and replacing the `merge` in
/// `Module::from_declarations` with a plain assignment.
#[test]
fn function_span_covers_annotation_and_body() {
    use codespan_reporting::files::SimpleFile;
    use zelkova_lang::compiler::position::{BytePos, Span};

    let source = indoc::indoc! {r#"
        module Main exposing (..)

        answer : Int
        answer = 42
    "#};

    let file = SimpleFile::new("Main.zel".to_string(), source.to_string());
    let module = parser::parse(&file).expect("should parse");

    assert_eq!(module.functions.len(), 1);

    // Computed from the text rather than written as literals, so editing the source
    // above cannot silently turn this into an assertion about the wrong bytes.
    let start = source.find("answer : Int").unwrap() as u32;
    let body = "answer = 42";
    let end = (source.find(body).unwrap() + body.len()) as u32;

    assert_eq!(
        module.functions[0].span.span(),
        Some(Span {
            start: BytePos(start),
            end: BytePos(end),
        })
    );
}
