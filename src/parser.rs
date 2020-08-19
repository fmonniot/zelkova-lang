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
    fn parser_module_single_decl_assignment() {
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

        // body literal
        run_test(
            vec![
                ident_token("main"),
                Token::Equal,
                Token::Integer { value: 42 },
                Token::Newline,
            ],
            Expression::Lit(Literal::Int(42)),
        );

        // body single variable
        run_test(
            vec![
                ident_token("main"),
                Token::Equal,
                ident_token("myvar"),
                Token::Newline,
            ],
            Expression::Variable(Name("myvar".to_string())),
        );

        // body function application
        run_test(
            vec![
                ident_token("main"),
                Token::Equal,
                ident_token("map"),
                ident_token("myfunction"),
                Token::Integer { value: 42 },
                Token::Newline,
            ],
            Expression::Application(
                Box::new(Expression::Variable(Name("map".to_string()))),
                Box::new(Expression::Application(
                    Box::new(Expression::Variable(Name("myfunction".to_string()))),
                    Box::new(Expression::Lit(Literal::Int(42))),
                )),
            ),
        );
    }

    #[test]
    fn parser_module_decl_with_type_definition() {
        let actual = grammar::ModuleParser::new().parse(tokens_to_spanned(vec![
            // module
            Token::Module,
            ident_token("Main"),
            Token::Exposing,
            Token::LPar,
            ident_token("main"),
            Token::RPar,
            Token::Newline,
            // tpe definition
            ident_token("main"),
            Token::Colon,
            ident_token("Int"),
            Token::Newline,
            // function definition
            ident_token("main"),
            Token::Equal,
            Token::Integer { value: 42 },
            Token::Newline,
        ]));

        let expected = Ok(Module {
            name: Name("Main".to_string()),
            exposing: vec![Name("main".to_string())],
            declarations: vec![
                Declaration::FunctionType(FunType {
                    name: Name("main".to_string()),
                    tpe: Name("Int".to_string()),
                }),
                Declaration::Function(BindGroup {
                    name: Name("main".to_string()),
                    patterns: vec![Match {
                        pattern: vec![],
                        body: Expression::Lit(Literal::Int(42)),
                    }],
                }),
            ],
        });

        assert_eq!(actual, expected);
    }
}
