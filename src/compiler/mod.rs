use std::path::Path;

mod frontend;
mod position;
mod type_checker;

// Testing method, we will need compilation unit in the long run (collection of files/modules)
pub fn compile_file<P: AsRef<Path>>(path: P) {
    let source = std::fs::read_to_string(path).expect("Can't read file");

    // Tokenize the source code into a serie of tokens
    let tokenizer = frontend::tokenizer::make_tokenizer(&source).map(|r| r.map_err(|e| e.into()));

    // Then manage the indentation aspect of our code
    let indented = frontend::indentation::layout(tokenizer);

    // Some debug intsruction to easily find errors early on.
    // This is for development only, and we should have a better error reporting
    // system in the future.
    let tokens: Vec<_> = indented.collect();
    let token_errors: Vec<_> = tokens.iter().filter(|r| r.is_err()).collect();
    if !token_errors.is_empty() {
        println!("frontend token errors: {:?}", token_errors);
    }

    // parser
    // TODO Should works on reference and not consume the original iterator
    let ast = frontend::parser::parse(tokens.iter().cloned());

    // Debug purpose: we show either the AST or an error with its list of tokens for debug.
    // Ultimately using codespan-reporting could even be better as it would point to
    // the source code itself. Although having tokens is valuable to debug the parser itself.
    match ast {
        Ok(ast) => println!("frontend AST: {:#?}", ast),
        Err(err) => {
            let pos = err.position_start();
            let token_position = tokens
                .iter()
                .position(|t| match t {
                    Ok(t) => &t.0 == pos,
                    Err(_) => false,
                })
                .expect("Can't find error's position in the token stream");

            let around = tokens
                .iter()
                .skip(if token_position >= 5 {
                    token_position - 5
                } else {
                    0
                })
                .take(20)
                .map(|s| s.as_ref().map(|s| s.1.clone()))
                .collect::<Vec<_>>();

            println!(
                "Error found\nToken around error: {:?}\nError: {:#?}",
                around, err
            );
        }
    }

    // desugar
    // type check
    // core (first IR)

    // then depending on the program type
    // evaluate (repl)
    // web assembly (emitted)
}
