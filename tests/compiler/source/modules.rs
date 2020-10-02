use super::support::*;
use zelkova_lang::compiler::source::*;

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
        exposing: Exposing::Open,
        imports: vec![
            Import {
                name: name("List"),
                alias: None,
                exposing: Exposing::Explicit(vec![]),
            },
            Import {
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
        exposing: Exposing::Open,
        imports: vec![
            Import {
                name: name("List"),
                alias: None,
                exposing: Exposing::Open,
            },
            Import {
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
        exposing: Exposing::Open,
        imports: vec![
            Import {
                name: name("List"),
                alias: None,
                exposing: Exposing::Explicit(vec![
                    Exposed::Lower(name("map")),
                    Exposed::Lower(name("foldl")),
                ]),
            },
            Import {
                name: name("Maybe"),
                alias: None,
                exposing: Exposing::Explicit(vec![
                    Exposed::Upper(name("Maybe"), Privacy::Private,)
                ]),
            },
            Import {
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
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![Infix {
            operator: name("<|"),
            associativy: Associativity::Right,
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
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![Infix {
            operator: name("//"),
            associativy: Associativity::Left,
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
        exposing: Exposing::Open,
        imports: vec![],
        infixes: vec![Infix {
            operator: name("=="),
            associativy: Associativity::None,
            precedence: 4,
            function_name: name("eq"),
        }],
        types: vec![],
        functions: vec![],
    }
);
