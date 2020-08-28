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
    use tokenizer::Token;
    use crate::compiler::position::Position;

    // Create an approximation for the token position in the stream.
    // We don't count the spaces between tokens, but it gives us enough
    // to understand where a failure happened.
    fn tokens_to_spanned(tokens: Vec<Token>) -> Vec<Spanned> {
        let mut pos = Position::new(0, 0);

        tokens
            .into_iter()
            .map(|token| {
                let start = pos.clone();
                let inc = match &token {
                    Token::Module => 6,
                    Token::Identifier { name } => name.len() as u32,
                    Token::Exposing => 8,
                    Token::LPar | Token::RPar => 1,
                    Token::Comma => 1,
                    _ => 0,
                };

                pos.go_right_by(inc);

                if token == Token::Newline {
                    pos.newline();
                }

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
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::RPar,
                Token::Newline,
            ];

            base_tokens.extend(tokens);

            let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(base_tokens));

            let expected = Ok(Module {
                name: Name("Main".to_string()),
                exposing: Exposing::Explicit(vec![Exposed::Lower(name("main"))]),
                declarations: vec![Declaration::Function(BindGroup {
                    name: Name("main".to_string()),
                    patterns: vec![Match {
                        pattern: vec![],
                        body,
                    }],
                })],
            });

            assert_eq!(actual, expected);
        };

        // constant
        run_test(
            vec![
                ident_token("main"),
                Token::Equal,
                Token::Integer { value: 42 },
                Token::Newline,
            ],
            Expression::Lit(Literal::Int(42)),
        );

        // single variable
        run_test(
            vec![
                ident_token("main"),
                Token::Equal,
                ident_token("myvar"),
                Token::Newline,
            ],
            Expression::Variable(Name("myvar".to_string())),
        );

        // function application (`map myfunction 42`)
        run_test(
            vec![
                ident_token("main"),
                Token::Equal,
                ident_token("map"),
                ident_token("myfunction"),
                Token::Integer { value: 2 },
                Token::Newline,
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
                ident_token("main"),
                Token::Equal,
                Token::LPar,
                ident_token("map"),
                ident_token("myfunction"),
                Token::RPar,
                Token::Integer { value: 2 },
                Token::Newline,
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
                ident_token("main"),
                Token::Equal,
                Token::Integer { value: 2 },
                Token::Plus,
                Token::Integer { value: 3 },
                Token::Newline,
            ],
            Expression::Application(
                // map: (a -> b) -> a -> b
                // first application result in: a -> b
                // second application result in: b
                Box::new(Expression::Application(
                    Box::new(Expression::Variable(Name("plus".to_string()))),
                    Box::new(Expression::Lit(Literal::Int(2))),
                )),
                Box::new(Expression::Lit(Literal::Int(3))),
            ),
        );

        // tuple
        run_test(
            vec![
                ident_token("main"),
                Token::Equal,
                Token::LPar,
                Token::Integer { value: 2 },
                Token::Comma,
                Token::Integer { value: 3 },
                Token::RPar,
                Token::Newline,
            ],
            Expression::Tuple(Box::new(vec![
                Expression::Lit(Literal::Int(2)),
                Expression::Lit(Literal::Int(3)),
            ])),
        );
    }

    #[test]
    fn parser_module_decl_type_definition() {
        let run_test = |tokens, declarations| {
            let mut base_tokens = vec![
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::RPar,
                Token::Newline,
            ];

            base_tokens.extend(tokens);

            let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(base_tokens));

            let expected = Ok(Module {
                name: Name("Main".to_string()),
                exposing: Exposing::Explicit(vec![Exposed::Lower(name("main"))]),
                declarations,
            });

            assert_eq!(actual, expected);
        };

        // Simple type
        run_test(
            vec![
                ident_token("main"),
                Token::Colon,
                ident_token("Int"),
                Token::Newline,
            ],
            vec![Declaration::FunctionType(FunType {
                name: Name("main".to_string()),
                tpe: Type::Named(Name("Int".to_string())),
            })],
        );

        // Function type
        run_test(
            vec![
                ident_token("length"),
                Token::Colon,
                ident_token("String"),
                Token::Arrow,
                ident_token("Int"),
                Token::Newline,
            ],
            vec![Declaration::FunctionType(FunType {
                name: Name("length".to_string()),
                tpe: Type::Arrow(
                    Box::new(Type::Named(Name("String".to_string()))),
                    Box::new(Type::Named(Name("Int".to_string()))),
                ),
            })],
        );

        // Higher order function type (`(String -> Int) -> String -> Int`)
        run_test(
            vec![
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
                Token::Newline,
            ],
            vec![Declaration::FunctionType(FunType {
                name: Name("length".to_string()),
                tpe: Type::Arrow(
                    Box::new(Type::Arrow(
                        Box::new(Type::Arrow(
                            Box::new(Type::Named(Name("String".to_string()))),
                            Box::new(Type::Named(Name("Int".to_string()))),
                        )),
                        Box::new(Type::Named(Name("String".to_string()))),
                    )),
                    Box::new(Type::Named(Name("Int".to_string()))),
                ),
            })],
        );

        // `(String -> Int) -> (String -> Int) -> Int`)
        run_test(
            vec![
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
                Token::Newline,
            ],
            vec![Declaration::FunctionType(FunType {
                name: Name("length".to_string()),
                tpe: Type::Arrow(
                    Box::new(Type::Arrow(
                        Box::new(Type::Arrow(
                            Box::new(Type::Named(Name("String".to_string()))),
                            Box::new(Type::Named(Name("Int".to_string()))),
                        )),
                        Box::new(Type::Arrow(
                            Box::new(Type::Named(Name("String".to_string()))),
                            Box::new(Type::Named(Name("Int".to_string()))),
                        )),
                    )),
                    Box::new(Type::Named(Name("Int".to_string()))),
                ),
            })],
        );
    }

    #[test]
    fn parser_module_decl_imports() {
        let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(vec![
            // module MyModule exposing (..)
            Token::Module,
            ident_token("MyModule"),
            Token::Exposing,
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::Newline,
            // import List
            Token::Import,
            ident_token("List"),
            Token::Newline,
            // import List as L
            Token::Import,
            ident_token("List"),
            Token::As,
            ident_token("L"),
            Token::Newline,
            // import List as L exposing (..)
            Token::Import,
            ident_token("List"),
            Token::As,
            ident_token("L"),
            Token::Exposing,
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::Newline,
            // import List exposing (..)
            Token::Import,
            ident_token("List"),
            Token::Exposing,
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::Newline,
            // import List exposing ( map, foldl )
            Token::Import,
            ident_token("List"),
            Token::Exposing,
            Token::LPar,
            ident_token("map"),
            Token::Comma,
            ident_token("foldl"),
            Token::RPar,
            Token::Newline,
            // import Maybe exposing ( Maybe )
            Token::Import,
            ident_token("Maybe"),
            Token::Exposing,
            Token::LPar,
            ident_token("Maybe"),
            Token::RPar,
            Token::Newline,
            // import Maybe exposing ( Maybe(..) )
            Token::Import,
            ident_token("Maybe"),
            Token::Exposing,
            Token::LPar,
            ident_token("Maybe"),
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::RPar,
            Token::Newline,
        ]));

        let expected = Ok(Module {
            name: name("MyModule"),
            exposing: Exposing::Open,
            declarations: vec![
                // import List
                Declaration::Import(Import {
                    name: name("List"),
                    alias: None,
                    exposing: Exposing::Explicit(vec![]),
                }),
                // import List as L
                Declaration::Import(Import {
                    name: name("List"),
                    alias: Some(name("L")),
                    exposing: Exposing::Explicit(vec![]),
                }),
                // import List as L exposing (..)
                Declaration::Import(Import {
                    name: name("List"),
                    alias: Some(name("L")),
                    exposing: Exposing::Open,
                }),
                // import List exposing (..)
                Declaration::Import(Import {
                    name: name("List"),
                    alias: None,
                    exposing: Exposing::Open,
                }),
                // import List exposing ( map, foldl )
                Declaration::Import(Import {
                    name: name("List"),
                    alias: None,
                    exposing: Exposing::Explicit(vec![
                        Exposed::Lower(name("map")),
                        Exposed::Lower(name("foldl")),
                    ]),
                }),
                // import Maybe exposing ( Maybe )
                Declaration::Import(Import {
                    name: name("Maybe"),
                    alias: None,
                    exposing: Exposing::Explicit(vec![Exposed::Upper(
                        name("Maybe"),
                        Privacy::Private,
                    )]),
                }),
                // import Maybe exposing ( Maybe(..) )
                Declaration::Import(Import {
                    name: name("Maybe"),
                    alias: None,
                    exposing: Exposing::Explicit(vec![Exposed::Upper(
                        name("Maybe"),
                        Privacy::Public,
                    )]),
                }),
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
            Token::Module,
            ident_token("Maybe"),
            Token::Exposing,
            Token::Newline,
            Token::Indent,
            Token::LPar,
            ident_token("Maybe"),
            Token::LPar,
            Token::DotDot,
            Token::RPar,
            Token::Newline,
            Token::Indent,
            Token::Comma,
            ident_token("andThen"),
            Token::Newline,
            Token::Indent,
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
            Token::Newline,
            Token::Indent,
            Token::Comma,
            ident_token("withDefault"),
            Token::Newline,
            Token::Indent,
            Token::RPar,
            Token::Newline,
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
            declarations: vec![],
        });

        assert_eq!(actual, expected);

        /*
        module Main exposing (main, fun)
        */
        let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(vec![
            Token::Module,
            Token::Identifier {
                name: "Main".to_string(),
            },
            Token::Exposing,
            Token::LPar,
            Token::Identifier {
                name: "main".to_string(),
            },
            Token::Comma,
            Token::Identifier {
                name: "fun".to_string(),
            },
            Token::RPar,
            Token::Newline,
        ]));

        let expected = Ok(Module {
            name: Name("Main".to_string()),
            exposing: Exposing::Explicit(vec![
                Exposed::Lower(name("main")),
                Exposed::Lower(name("fun")),
            ]),
            declarations: vec![],
        });

        assert_eq!(actual, expected)
    }

    #[test]
    fn parser_module_decl_union_type() {
        let run_test = |tokens, tpe| {
            let mut base_tokens = vec![
                Token::Module,
                ident_token("Main"),
                Token::Exposing,
                Token::LPar,
                ident_token("main"),
                Token::RPar,
                Token::Newline,
            ];

            base_tokens.extend(tokens);

            let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(base_tokens));

            let expected = Ok(Module {
                name: Name("Main".to_string()),
                exposing: Exposing::Explicit(vec![Exposed::Lower(name("main"))]),
                declarations: vec![Declaration::Union(tpe)],
            });

            assert_eq!(actual, expected);
        };

        // type UserStatus = Regular | Visitor
        run_test(
            vec![
                Token::Type,
                ident_token("UserStatus"),
                Token::Equal,
                ident_token("Regular"),
                Token::Pipe,
                ident_token("Visitor"),
                Token::Newline,
            ],
            UnionType {
                name: name("UserStatus"),
                type_arguments: vec![],
                variants: vec![(name("Regular"), vec![]), (name("Visitor"), vec![])],
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
                Token::Type,
                ident_token("User"),
                Token::Newline,
                Token::Indent,
                Token::Indent,
                Token::Equal,
                ident_token("Regular"),
                ident_token("String"),
                ident_token("Int"),
                Token::Newline,
                Token::Indent,
                Token::Indent,
                Token::Pipe,
                ident_token("Visitor"),
                ident_token("String"),
                Token::Newline,
                Token::Indent,
                Token::Indent,
                Token::Pipe,
                ident_token("Anonymous"),
                Token::Newline,
            ],
            UnionType {
                name: name("User"),
                type_arguments: vec![],
                variants: vec![
                    (
                        name("Regular"),
                        vec![Type::Named(name("String")), Type::Named(name("Int"))],
                    ),
                    (name("Visitor"), vec![Type::Named(name("String"))]),
                    (name("Anonymous"), vec![]),
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
                Token::Type,
                ident_token("Maybe"),
                ident_token("a"),
                Token::Newline,
                Token::Indent,
                Token::Indent,
                Token::Equal,
                ident_token("Just"),
                ident_token("a"),
                Token::Newline,
                Token::Indent,
                Token::Indent,
                Token::Pipe,
                ident_token("Nothing"),
                Token::Newline,
            ],
            UnionType {
                name: name("Maybe"),
                type_arguments: vec![name("a")],
                variants: vec![
                    (name("Just"), vec![Type::Variable(name("a"))]),
                    (name("Nothing"), vec![]),
                ],
            },
        );
    }
}
