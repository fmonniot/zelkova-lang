use crate::frontend::Module;
use crate::position::Position;
use crate::tokenizer::{self, LexicalError, Spanned, Token};
use lalrpop_util::ParseError;

lalrpop_mod!(grammar);

pub fn parse(
    i: impl Iterator<Item = tokenizer::Result<Spanned>>,
) -> Result<Module, ParseError<Position, Token, LexicalError>> {
    grammar::ModuleParser::new().parse(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::*;
    use crate::position::Position;
    use crate::tokenizer::Token;

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

    #[test]
    fn parser_module_without_declarations() {
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
            exposing: vec![Name("main".to_string()), Name("fun".to_string())],
            declarations: vec![],
        });

        assert_eq!(actual, expected)
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
                exposing: vec![Name("main".to_string())],
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
                exposing: vec![Name("main".to_string())],
                declarations,
            });

            println!("actual: {:#?}", actual);
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
}
