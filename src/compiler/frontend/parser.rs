//! Transform a serie of tokens into the frontend AST.

use super::Module;
use crate::compiler::frontend::error::Error;
use crate::compiler::frontend::tokenizer::Spanned;

lalrpop_mod!(grammar, "/compiler/frontend/grammar.rs");

pub fn parse(i: impl Iterator<Item = Result<Spanned, Error>>) -> Result<Module, Error> {
    Ok(grammar::ModuleParser::new().parse(i)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::*;
    use crate::compiler::position::Position;
    use tokenizer::Token;

    // Create an approximation for the token position in the stream.
    // We don't count the spaces between tokens, but it gives us enough
    // to understand where a failure happened.
    fn tokens_to_spanned(tokens: Vec<Token>) -> Vec<Spanned> {
        let mut pos = Position::new(0, 1, 1);

        tokens
            .into_iter()
            .map(|token| {
                let start = pos.clone();
                let inc = match &token {
                    Token::Module => 6,
                    Token::Identifier { name } => name.len(),
                    Token::Exposing => 8,
                    Token::LPar | Token::RPar => 1,
                    Token::Comma => 1,
                    Token::Newline => 1,
                    _ => 0,
                };

                pos.increment_by(inc);

                let end = pos.clone();

                (start, token, end)
            })
            .collect()
    }

    fn ident_token(s: &str) -> Token {
        Token::Identifier {
            name: s.to_string(),
        }
    }

    fn name(s: &str) -> Name {
        Name(s.to_string())
    }

    #[test]
    fn parser_module_decl_simple_expression() {
        let run_test = |tokens, body| {
            let mut base_tokens = vec![
                Token::OpenBlock,
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::RPar,
                Token::CloseBlock,
            ];

            base_tokens.extend(tokens);

            let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(base_tokens));

            let expected = Ok(Module {
                name: Name("Main".to_string()),
                exposing: Exposing::Explicit(vec![Exposed::Lower(name("main"))]),
                imports: vec![],
                infixes: vec![],
                types: vec![],
                functions: vec![Function {
                    name: Name("main".to_string()),
                    tpe: None,
                    bindings: vec![Match {
                        patterns: vec![],
                        body,
                    }],
                }],
            });

            assert_eq!(actual, expected);
        };

        // constant
        run_test(
            vec![
                Token::OpenBlock,
                ident_token("main"),
                Token::Equal,
                Token::Integer { value: 42 },
                Token::CloseBlock,
            ],
            Expression::Lit(Literal::Int(42)),
        );

        // single variable
        run_test(
            vec![
                Token::OpenBlock,
                ident_token("main"),
                Token::Equal,
                ident_token("myvar"),
                Token::CloseBlock,
            ],
            Expression::Variable(Name("myvar".to_string())),
        );

        // function application (`map myfunction 42`)
        run_test(
            vec![
                Token::OpenBlock,
                ident_token("main"),
                Token::Equal,
                ident_token("map"),
                ident_token("myfunction"),
                Token::Integer { value: 2 },
                Token::CloseBlock,
            ],
            Expression::Application(
                // map: (a -> b) -> a -> b
                // first application result in: a -> b
                // second application result in: b
                Box::new(Expression::Application(
                    Box::new(Expression::Variable(Name("map".to_string()))),
                    Box::new(Expression::Variable(Name("myfunction".to_string()))),
                )),
                Box::new(Expression::Lit(Literal::Int(2))),
            ),
        );

        // function application with parenthesis (`(map myfunction) 42`)
        run_test(
            vec![
                Token::OpenBlock,
                ident_token("main"),
                Token::Equal,
                Token::LPar,
                ident_token("map"),
                ident_token("myfunction"),
                Token::RPar,
                Token::Integer { value: 2 },
                Token::CloseBlock,
            ],
            Expression::Application(
                // map: (a -> b) -> a -> b
                // first application result in: a -> b
                // second application result in: b
                Box::new(Expression::Application(
                    Box::new(Expression::Variable(Name("map".to_string()))),
                    Box::new(Expression::Variable(Name("myfunction".to_string()))),
                )),
                Box::new(Expression::Lit(Literal::Int(2))),
            ),
        );

        // infix operation
        run_test(
            vec![
                Token::OpenBlock,
                ident_token("main"),
                Token::Equal,
                Token::Integer { value: 2 },
                Token::Operator("+".to_string()),
                Token::Integer { value: 3 },
                Token::CloseBlock,
            ],
            Expression::Application(
                // map: (a -> b) -> a -> b
                // first application result in: a -> b
                // second application result in: b
                Box::new(Expression::Application(
                    Box::new(Expression::Variable(Name("+".to_string()))),
                    Box::new(Expression::Lit(Literal::Int(2))),
                )),
                Box::new(Expression::Lit(Literal::Int(3))),
            ),
        );

        // tuple
        run_test(
            vec![
                Token::OpenBlock,
                ident_token("main"),
                Token::Equal,
                Token::LPar,
                Token::Integer { value: 2 },
                Token::Comma,
                Token::Integer { value: 3 },
                Token::RPar,
                Token::CloseBlock,
            ],
            Expression::Tuple(Box::new(vec![
                Expression::Lit(Literal::Int(2)),
                Expression::Lit(Literal::Int(3)),
            ])),
        );
    }

    #[test]
    fn parser_module_decl_if_expression() {
        let run_test = |tokens, body| {
            let mut base_tokens = vec![
                Token::OpenBlock,
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::RPar,
                Token::CloseBlock,
                Token::OpenBlock,
                ident_token("main"),
                Token::Equal,
            ];

            base_tokens.extend(tokens);
            base_tokens.push(Token::CloseBlock);

            let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(base_tokens));

            let expected = Ok(Module {
                name: Name("Main".to_string()),
                exposing: Exposing::Explicit(vec![Exposed::Lower(name("main"))]),
                imports: vec![],
                infixes: vec![],
                types: vec![],
                functions: vec![Function {
                    name: Name("main".to_string()),
                    tpe: None,
                    bindings: vec![Match {
                        patterns: vec![],
                        body,
                    }],
                }],
            });

            assert_eq!(actual, expected);
        };

        // simple if
        run_test(
            vec![
                Token::If,
                Token::True,
                Token::Then,
                Token::Integer { value: 2 },
                Token::Else,
                Token::Integer { value: 3 },
            ],
            Expression::If(
                Box::new(Expression::Lit(Literal::Bool(true))),
                Box::new(Expression::Lit(Literal::Int(2))),
                Box::new(Expression::Lit(Literal::Int(3))),
            ),
        );

        // if - else if - else -
        run_test(
            vec![
                Token::If,
                Token::False,
                Token::Then,
                Token::Integer { value: 2 },
                Token::Else,
                Token::If,
                Token::True,
                Token::Then,
                Token::Integer { value: 3 },
                Token::Else,
                Token::Integer { value: 4 },
            ],
            Expression::If(
                Box::new(Expression::Lit(Literal::Bool(false))),
                Box::new(Expression::Lit(Literal::Int(2))),
                Box::new(Expression::If(
                    Box::new(Expression::Lit(Literal::Bool(true))),
                    Box::new(Expression::Lit(Literal::Int(3))),
                    Box::new(Expression::Lit(Literal::Int(4))),
                )),
            ),
        );
    }

    #[test]
    fn parser_module_decl_type_definition() {
        let run_test = |msg, tokens, tpe| {
            let mut base_tokens = vec![
                Token::OpenBlock,
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("length"),
                Token::RPar,
                Token::CloseBlock,
            ];

            base_tokens.extend(tokens);

            let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(base_tokens));

            let expected = Ok(Module {
                name: Name("Main".to_string()),
                exposing: Exposing::Explicit(vec![Exposed::Lower(name("length"))]),
                imports: vec![],
                infixes: vec![],
                types: vec![],
                functions: vec![Function {
                    name: Name("length".to_string()),
                    tpe: Some(tpe),
                    bindings: vec![],
                }],
            });

            assert_eq!(actual, expected, "\n{}", msg);
        };

        run_test(
            "Constant => length : Int",
            vec![
                Token::OpenBlock,
                ident_token("length"),
                Token::Colon,
                ident_token("Int"),
                Token::CloseBlock,
            ],
            Type::unqualified(Name("Int".to_string())),
        );

        run_test(
            "Function type => length: String -> Int",
            vec![
                Token::OpenBlock,
                ident_token("length"),
                Token::Colon,
                ident_token("String"),
                Token::Arrow,
                ident_token("Int"),
                Token::CloseBlock,
            ],
            Type::Arrow(
                Box::new(Type::unqualified(Name("String".to_string()))),
                Box::new(Type::unqualified(Name("Int".to_string()))),
            ),
        );

        run_test(
            "Tuples => length: (a -> b, b -> c) -> a",
            vec![
                Token::OpenBlock,
                ident_token("length"),
                Token::Colon,
                Token::LPar,
                ident_token("a"),
                Token::Arrow,
                ident_token("b"),
                Token::Comma,
                ident_token("b"),
                Token::Arrow,
                ident_token("c"),
                Token::RPar,
                Token::Arrow,
                ident_token("a"),
                Token::CloseBlock,
            ],
            Type::Arrow(
                Box::new(Type::Tuple(
                    Box::new(
                        Type::Arrow(
                            Box::new(Type::Variable(Name("a".to_string()))),
                            Box::new(Type::Variable(Name("b".to_string())))
                        )
                    ),
                    Box::new(vec![
                        Type::Arrow(
                            Box::new(Type::Variable(Name("b".to_string()))),
                            Box::new(Type::Variable(Name("c".to_string())))
                        )
                    ])
                )),
                Box::new(Type::Variable(Name("a".to_string()))),
            ),
        );

        run_test(
            "Higher order function type => (String -> Int) -> String -> Int",
            vec![
                Token::OpenBlock,
                ident_token("length"),
                Token::Colon,
                Token::LPar,
                ident_token("String"),
                Token::Arrow,
                ident_token("Int"),
                Token::RPar,
                Token::Arrow,
                ident_token("String"),
                Token::Arrow,
                ident_token("Int"),
                Token::CloseBlock,
            ],
            Type::Arrow(
                Box::new(Type::Arrow(
                    Box::new(Type::Arrow(
                        Box::new(Type::unqualified(Name("String".to_string()))),
                        Box::new(Type::unqualified(Name("Int".to_string()))),
                    )),
                    Box::new(Type::unqualified(Name("String".to_string()))),
                )),
                Box::new(Type::unqualified(Name("Int".to_string()))),
            ),
        );

        run_test(
            "(String -> Int) -> (String -> Int) -> Int",
            vec![
                Token::OpenBlock,
                ident_token("length"),
                Token::Colon,
                Token::LPar,
                ident_token("String"),
                Token::Arrow,
                ident_token("Int"),
                Token::RPar,
                Token::Arrow,
                Token::LPar,
                ident_token("String"),
                Token::Arrow,
                ident_token("Int"),
                Token::RPar,
                Token::Arrow,
                ident_token("Int"),
                Token::CloseBlock,
            ],
            Type::Arrow(
                Box::new(Type::Arrow(
                    Box::new(Type::Arrow(
                        Box::new(Type::unqualified(Name("String".to_string()))),
                        Box::new(Type::unqualified(Name("Int".to_string()))),
                    )),
                    Box::new(Type::Arrow(
                        Box::new(Type::unqualified(Name("String".to_string()))),
                        Box::new(Type::unqualified(Name("Int".to_string()))),
                    )),
                )),
                Box::new(Type::unqualified(Name("Int".to_string()))),
            ),
        );

        run_test(
            "polymorphic function type => length : a -> Maybe a -> a",
            vec![
                Token::OpenBlock,
                ident_token("length"),
                Token::Colon,
                ident_token("a"),
                Token::Arrow,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Arrow,
                ident_token("a"),
                Token::CloseBlock,
            ],
            Type::Arrow(
                Box::new(Type::Arrow(
                    Box::new(Type::Variable(name("a"))),
                    Box::new(Type::unqualified_with(
                        name("Maybe"),
                        vec![Type::Variable(name("a"))],
                    )),
                )),
                Box::new(Type::Variable(name("a"))),
            ),
        )
    }

    #[test]
    fn parser_module_decl_imports() {
        let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(vec![
            // module MyModule exposing (..)
            Token::OpenBlock,
            Token::Module,
            ident_token("MyModule"),
            Token::Exposing,
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::CloseBlock,
            // import List
            Token::OpenBlock,
            Token::Import,
            ident_token("List"),
            Token::CloseBlock,
            // import List as L
            Token::OpenBlock,
            Token::Import,
            ident_token("List"),
            Token::As,
            ident_token("L"),
            Token::CloseBlock,
            // import List as L exposing (..)
            Token::OpenBlock,
            Token::Import,
            ident_token("List"),
            Token::As,
            ident_token("L"),
            Token::Exposing,
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::CloseBlock,
            // import List exposing (..)
            Token::OpenBlock,
            Token::Import,
            ident_token("List"),
            Token::Exposing,
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::CloseBlock,
            // import List exposing ( map, foldl )
            Token::OpenBlock,
            Token::Import,
            ident_token("List"),
            Token::Exposing,
            Token::LPar,
            ident_token("map"),
            Token::Comma,
            ident_token("foldl"),
            Token::RPar,
            Token::CloseBlock,
            // import Maybe exposing ( Maybe )
            Token::OpenBlock,
            Token::Import,
            ident_token("Maybe"),
            Token::Exposing,
            Token::LPar,
            ident_token("Maybe"),
            Token::RPar,
            Token::CloseBlock,
            // import Maybe exposing ( Maybe(..) )
            Token::OpenBlock,
            Token::Import,
            ident_token("Maybe"),
            Token::Exposing,
            Token::LPar,
            ident_token("Maybe"),
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::RPar,
            Token::CloseBlock,
        ]));

        let expected = Ok(Module {
            name: name("MyModule"),
            exposing: Exposing::Open,
            types: vec![],
            functions: vec![],
            infixes: vec![],
            imports: vec![
                // import List
                Import {
                    name: name("List"),
                    alias: None,
                    exposing: Exposing::Explicit(vec![]),
                },
                // import List as L
                Import {
                    name: name("List"),
                    alias: Some(name("L")),
                    exposing: Exposing::Explicit(vec![]),
                },
                // import List as L exposing (..)
                Import {
                    name: name("List"),
                    alias: Some(name("L")),
                    exposing: Exposing::Open,
                },
                // import List exposing (..)
                Import {
                    name: name("List"),
                    alias: None,
                    exposing: Exposing::Open,
                },
                // import List exposing ( map, foldl )
                Import {
                    name: name("List"),
                    alias: None,
                    exposing: Exposing::Explicit(vec![
                        Exposed::Lower(name("map")),
                        Exposed::Lower(name("foldl")),
                    ]),
                },
                // import Maybe exposing ( Maybe )
                Import {
                    name: name("Maybe"),
                    alias: None,
                    exposing: Exposing::Explicit(vec![Exposed::Upper(
                        name("Maybe"),
                        Privacy::Private,
                    )]),
                },
                // import Maybe exposing ( Maybe(..) )
                Import {
                    name: name("Maybe"),
                    alias: None,
                    exposing: Exposing::Explicit(vec![Exposed::Upper(
                        name("Maybe"),
                        Privacy::Public,
                    )]),
                },
            ],
        });

        assert_eq!(actual, expected);
    }

    #[test]
    fn parser_module_exposing() {
        /*
        module Maybe exposing
          ( Maybe(..)
          , andThen
          , map, map2, map3, map4, map5
          , withDefault
          )
        */
        let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(vec![
            Token::OpenBlock,
            Token::Module,
            ident_token("Maybe"),
            Token::Exposing,
            Token::LPar,
            ident_token("Maybe"),
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::Comma,
            ident_token("andThen"),
            Token::Comma,
            ident_token("map"),
            Token::Comma,
            ident_token("map2"),
            Token::Comma,
            ident_token("map3"),
            Token::Comma,
            ident_token("map4"),
            Token::Comma,
            ident_token("map5"),
            Token::Comma,
            ident_token("withDefault"),
            Token::RPar,
            Token::CloseBlock,
        ]));

        let expected = Ok(Module {
            name: name("Maybe"),
            exposing: Exposing::Explicit(vec![
                Exposed::Upper(name("Maybe"), Privacy::Public),
                Exposed::Lower(name("andThen")),
                Exposed::Lower(name("map")),
                Exposed::Lower(name("map2")),
                Exposed::Lower(name("map3")),
                Exposed::Lower(name("map4")),
                Exposed::Lower(name("map5")),
                Exposed::Lower(name("withDefault")),
            ]),
            imports: vec![],
            infixes: vec![],
            types: vec![],
            functions: vec![],
        });

        assert_eq!(actual, expected);
    }

    #[test]
    fn parser_module_decl_union_type() {
        let run_test = |tokens, tpe| {
            let mut base_tokens = vec![
                Token::OpenBlock,
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::RPar,
                Token::CloseBlock,
            ];

            base_tokens.extend(tokens);

            let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(base_tokens));

            let expected = Ok(Module {
                name: Name("Main".to_string()),
                exposing: Exposing::Explicit(vec![Exposed::Lower(name("main"))]),
                imports: vec![],
                infixes: vec![],
                types: vec![tpe],
                functions: vec![],
            });

            assert_eq!(actual, expected);
        };

        // type UserStatus = Regular | Visitor
        run_test(
            vec![
                Token::OpenBlock,
                Token::Type,
                ident_token("UserStatus"),
                Token::Equal,
                ident_token("Regular"),
                Token::Pipe,
                ident_token("Visitor"),
                Token::CloseBlock,
            ],
            UnionType {
                name: name("UserStatus"),
                type_arguments: vec![],
                variants: vec![
                    Type::unqualified(name("Regular")),
                    Type::unqualified(name("Visitor")),
                ],
            },
        );

        /*
        type User
            = Regular String Int
            | Visitor String
            | Anonymous
        */
        run_test(
            vec![
                Token::OpenBlock,
                Token::Type,
                ident_token("User"),
                Token::Equal,
                ident_token("Regular"),
                ident_token("String"),
                ident_token("Int"),
                Token::Pipe,
                ident_token("Visitor"),
                ident_token("String"),
                Token::Pipe,
                ident_token("Anonymous"),
                Token::CloseBlock,
            ],
            UnionType {
                name: name("User"),
                type_arguments: vec![],
                variants: vec![
                    Type::unqualified_with(
                        name("Regular"),
                        vec![
                            Type::unqualified(name("String")),
                            Type::unqualified(name("Int")),
                        ],
                    ),
                    Type::unqualified_with(
                        name("Visitor"),
                        vec![Type::unqualified(name("String"))],
                    ),
                    Type::unqualified(name("Anonymous")),
                ],
            },
        );

        /*
        type Msg
            = ReceivedMessage { user : User, message : String }
        */
        // TODO Once we have support for records

        /*
        type Maybe a
            = Just a
            | Nothing
        */
        run_test(
            vec![
                Token::OpenBlock,
                Token::Type,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                Token::Pipe,
                ident_token("Nothing"),
                Token::CloseBlock,
            ],
            UnionType {
                name: name("Maybe"),
                type_arguments: vec![name("a")],
                variants: vec![
                    Type::unqualified_with(name("Just"), vec![Type::Variable(name("a"))]),
                    Type::unqualified(name("Nothing")),
                ],
            },
        );
    }

    #[test]
    fn parser_module_infix() {
        let run_test = |tokens, infix| {
            let mut base_tokens = vec![
                Token::OpenBlock,
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::RPar,
                Token::CloseBlock,
            ];

            base_tokens.extend(tokens);

            let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(base_tokens));

            let expected = Ok(Module {
                name: Name("Main".to_string()),
                exposing: Exposing::Explicit(vec![Exposed::Lower(name("main"))]),
                imports: vec![],
                infixes: vec![infix],
                types: vec![],
                functions: vec![],
            });

            assert_eq!(actual, expected);
        };

        // infix right 0 (<|) = apL
        run_test(
            vec![
                Token::OpenBlock,
                Token::Infix,
                Token::Right,
                Token::Integer { value: 0 },
                Token::LPar,
                Token::Operator("<|".to_string()),
                Token::RPar,
                Token::Equal,
                ident_token("apL"),
                Token::CloseBlock,
            ],
            Infix {
                operator: Name("<|".to_string()),
                associativy: Associativity::Right,
                precedence: 0,
                function_name: Name("apL".to_string()),
            },
        );

        // infix left  7 (//) = idiv
        run_test(
            vec![
                Token::OpenBlock,
                Token::Infix,
                Token::Left,
                Token::Integer { value: 7 },
                Token::LPar,
                Token::Operator("//".to_string()),
                Token::RPar,
                Token::Equal,
                ident_token("idiv"),
                Token::CloseBlock,
            ],
            Infix {
                operator: Name("//".to_string()),
                associativy: Associativity::Left,
                precedence: 7,
                function_name: Name("idiv".to_string()),
            },
        );

        // infix non   4 (==) = eq
        run_test(
            vec![
                Token::OpenBlock,
                Token::Infix,
                Token::Non,
                Token::Integer { value: 4 },
                Token::LPar,
                Token::Operator("==".to_string()),
                Token::RPar,
                Token::Equal,
                ident_token("eq"),
                Token::CloseBlock,
            ],
            Infix {
                operator: Name("==".to_string()),
                associativy: Associativity::None,
                precedence: 4,
                function_name: Name("eq".to_string()),
            },
        );
    }
}
