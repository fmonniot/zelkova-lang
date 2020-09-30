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
                    Token::LowerIdentifier(name) => name.len(),
                    Token::UpperIdentifier(name) => name.len(),
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
        let first = s.chars().next().unwrap();
        if first.is_uppercase() {
            Token::UpperIdentifier(s.to_string())
        } else {
            Token::LowerIdentifier(s.to_string())
        }
    }

    fn name(s: &str) -> Name {
        Name(s.to_string())
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
                    Box::new(Type::Arrow(
                        Box::new(Type::Variable(Name("a".to_string()))),
                        Box::new(Type::Variable(Name("b".to_string()))),
                    )),
                    Box::new(vec![Type::Arrow(
                        Box::new(Type::Variable(Name("b".to_string()))),
                        Box::new(Type::Variable(Name("c".to_string()))),
                    )]),
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
}
