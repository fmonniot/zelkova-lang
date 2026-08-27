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

/// `ERR-3`, commit 2: the parser test that pins where a *sub-expression* parsed.
///
/// `function_span_covers_annotation_and_body` above pins a declaration's span; this
/// is the same job one level down, and it exists for the same reason: every
/// `test_parse_ok!` in this directory compares a whole `Module`, `NodeSpan`'s
/// `PartialEq` always returns `true`, and so none of them can tell a correct span
/// from a missing one. Only a direct `.span` assertion can.
///
/// Three nested nodes are checked at once, because a single one would not catch a
/// span that is merely *present*: the application covers `negate 42`, its function
/// covers `negate`, and its argument covers `42`. The three ranges are distinct and
/// computed from the source text.
///
/// Mutation-checked three ways, each red on its own: making the `AtomicExpr`
/// `QualVarIdent` production emit `NodeSpan::none()`; making its `Lit` production do
/// the same; and replacing the `AppExpr` application's `NodeSpan::new(l, r)` with
/// `NodeSpan::none()`.
#[test]
fn expression_spans_cover_each_sub_expression() {
    use codespan_reporting::files::SimpleFile;
    use zelkova_lang::compiler::position::{BytePos, Span};

    let source = indoc::indoc! {r#"
        module Main exposing (..)

        main = negate 42
    "#};

    let file = SimpleFile::new("Main.zel".to_string(), source.to_string());
    let module = parser::parse(&file).expect("should parse");

    // Computed from the text, so editing the source above cannot silently turn
    // these into assertions about the wrong bytes.
    let at = |needle: &str| {
        let start = source.find(needle).expect("source contains the fragment");
        Some(Span {
            start: BytePos(start as u32),
            end: BytePos((start + needle.len()) as u32),
        })
    };

    let body = &module.functions[0].bindings[0].body;
    assert_eq!(body.span.span(), at("negate 42"));

    let ExpressionKind::Application(function, argument) = &body.kind else {
        panic!("expected an application, got {:?}", body.kind);
    };

    assert_eq!(function.span.span(), at("negate"));
    assert_eq!(argument.span.span(), at("42"));
}

/// `ERR-3`: the `case … of` expression's own span, asserted directly.
///
/// A `case` is the one expression whose production ends by consuming a layout token,
/// and the layout pass positions an implicitly-closed block at the token that closed
/// it — the first token of the *next* declaration, or `EndOfFile` (`BytePos(0)`) at
/// the end of the file. Taking `@R` after `CaseBranch+` therefore produced spans that
/// ran into the following declaration, or inverted outright. Nothing in the suite
/// could see it: every `test_parse_ok!` compares whole `Module`s and `NodeSpan`'s
/// `PartialEq` is blind, and the two `.span` tests above use one-line bodies.
///
/// The source deliberately puts a declaration *after* the case, so an end taken one
/// token too far shows up as a range overrunning into `other`.
///
/// Mutation-checked two ways, each red on its own: restoring `<r:@R>` after
/// `<branches: CaseBranch+>` in the `case` production with `NodeSpan::new(l, r)` (the
/// case's end jumps to the end of `other`), and making `CaseBranch` take its span
/// from an `@R` after `<expression: Expr>` instead of from `expression.span`.
#[test]
fn case_expression_span_stops_at_its_last_branch() {
    use codespan_reporting::files::SimpleFile;
    use zelkova_lang::compiler::position::{BytePos, Span};

    let source = indoc::indoc! {r#"
        module Main exposing (..)

        classify c =
          case c of
            Red -> 1
            Blue -> 2

        other = 3
    "#};

    let file = SimpleFile::new("Main.zel".to_string(), source.to_string());
    let module = parser::parse(&file).expect("should parse");

    // Computed from the text, so editing the source above cannot silently turn
    // these into assertions about the wrong bytes.
    let at = |needle: &str| {
        let start = source.find(needle).expect("source contains the fragment");
        Some(Span {
            start: BytePos(start as u32),
            end: BytePos((start + needle.len()) as u32),
        })
    };

    let classify = module
        .functions
        .iter()
        .find(|f| f.name == name("classify"))
        .expect("the module declares `classify`");

    let body = &classify.bindings[0].body;
    let ExpressionKind::Case(scrutinee, branches) = &body.kind else {
        panic!("expected a case expression, got {:?}", body.kind);
    };

    assert_eq!(
        scrutinee.span.span(),
        at("c of").map(|s| Span {
            start: s.start,
            end: BytePos(s.start.0 + 1),
        })
    );
    assert_eq!(branches.len(), 2);
    assert_eq!(branches[0].span.span(), at("Red -> 1"));
    assert_eq!(branches[1].span.span(), at("Blue -> 2"));

    // The case runs from its own `case` keyword to the last byte of the last
    // branch — not to the layout token that closed the block, and not into `other`.
    let start = source.find("case c of").expect("source has the case");
    let last = "Blue -> 2";
    let end = source.find(last).expect("source has a second branch") + last.len();
    assert_eq!(
        body.span.span(),
        Some(Span {
            start: BytePos(start as u32),
            end: BytePos(end as u32),
        })
    );

    // The declaration around it inherits that end rather than the layout token's.
    assert_eq!(
        classify.span.span(),
        Some(Span {
            start: BytePos(source.find("classify c =").expect("source has the binding") as u32),
            end: BytePos(end as u32),
        })
    );
}

/// `ERR-3`: a `parser::Type`'s span, asserted directly.
///
/// No other test observes one, and none can do so indirectly:
/// `canonical::Type::from_parser_type` drops the span by design (`ERR-5`), so no
/// pipeline diagnostic ever carries a type position. Combined with the blind
/// `PartialEq`, the `test_parse_ok!`s in `types.rs` say nothing about where a type
/// was written.
///
/// The `Type` productions are also the most intricate spans in the grammar: the
/// parenthesised form runs `l..r` past an optional `-> T`, while the tuple arities
/// take a *second* `@R` at the `)` and hand `l..m` to the tuple, so the `Tuple` and
/// the `Arrow` around it end at different bytes. That split is what this pins.
///
/// Mutation-checked three ways, each red on its own: making the two-element tuple
/// production hand the tuple `NodeSpan::new(l, r)` instead of `NodeSpan::new(l, m)`
/// (the tuple then ends where the arrow does); handing the `Arrow` that same
/// production builds `NodeSpan::none()`; and making `AtomicType`'s `QualTypeIdent`
/// production emit `NodeSpan::none()`.
#[test]
fn type_annotation_spans_cover_each_component() {
    use codespan_reporting::files::SimpleFile;
    use zelkova_lang::compiler::position::{BytePos, Span};
    use zelkova_lang::compiler::tuple::Tuple;

    let source = indoc::indoc! {r#"
        module Main exposing (..)

        f : (Int, Char) -> Int
        f x = 1
    "#};

    let file = SimpleFile::new("Main.zel".to_string(), source.to_string());
    let module = parser::parse(&file).expect("should parse");

    let at = |needle: &str| {
        let start = source.find(needle).expect("source contains the fragment");
        Some(Span {
            start: BytePos(start as u32),
            end: BytePos((start + needle.len()) as u32),
        })
    };

    let annotation = module.functions[0]
        .tpe
        .as_ref()
        .expect("`f` carries an annotation");

    // The arrow covers the whole annotation, including its result.
    assert_eq!(annotation.span.span(), at("(Int, Char) -> Int"));

    let TypeKind::Arrow(left, right) = &annotation.kind else {
        panic!("expected an arrow, got {:?}", annotation.kind);
    };

    // The tuple stops at the `)`, where the second `@R` is taken.
    assert_eq!(left.span.span(), at("(Int, Char)"));
    let result = source.rfind("Int").expect("source has a result type");
    assert_eq!(
        right.span.span(),
        Some(Span {
            start: BytePos(result as u32),
            end: BytePos((result + "Int".len()) as u32),
        }),
        "the result type"
    );

    let TypeKind::Tuple(Tuple::Two(a, b)) = &left.kind else {
        panic!("expected a two-element tuple, got {:?}", left.kind);
    };
    assert_eq!(
        a.span.span(),
        at("Int,").map(|s| Span {
            start: s.start,
            end: BytePos(s.end.0 - 1),
        })
    );
    assert_eq!(b.span.span(), at("Char"));
}

/// `ERR-3`: a tuple `parser::Pattern`'s span, asserted directly.
///
/// `unknown_constructor_labels_the_pattern` in `tests/pipeline.rs` covers the bare
/// constructor production; the tuple production takes its own `@L`/`@R` around the
/// parentheses and is otherwise unobserved, for the same `PartialEq` reason as the
/// types above.
///
/// Mutation-checked two ways, each red on its own: making the two-element tuple
/// `Pattern` production emit `NodeSpan::none()`, and making the `Pattern`
/// `VarIdent` production do the same.
#[test]
fn tuple_pattern_span_covers_the_parentheses() {
    use codespan_reporting::files::SimpleFile;
    use zelkova_lang::compiler::position::{BytePos, Span};
    use zelkova_lang::compiler::tuple::Tuple;

    let source = indoc::indoc! {r#"
        module Main exposing (..)

        fst (a, b) = a
    "#};

    let file = SimpleFile::new("Main.zel".to_string(), source.to_string());
    let module = parser::parse(&file).expect("should parse");

    let at = |needle: &str| {
        let start = source.find(needle).expect("source contains the fragment");
        Some(Span {
            start: BytePos(start as u32),
            end: BytePos((start + needle.len()) as u32),
        })
    };

    let pattern = &module.functions[0].bindings[0].patterns[0];
    assert_eq!(pattern.span.span(), at("(a, b)"));

    let PatternKind::Tuple(Tuple::Two(a, b)) = &pattern.kind else {
        panic!(
            "expected a two-element tuple pattern, got {:?}",
            pattern.kind
        );
    };
    assert_eq!(
        a.span.span(),
        at("a,").map(|s| Span {
            start: s.start,
            end: BytePos(s.end.0 - 1),
        })
    );
    assert_eq!(
        b.span.span(),
        at("b)").map(|s| Span {
            start: s.start,
            end: BytePos(s.end.0 - 1),
        })
    );
}
