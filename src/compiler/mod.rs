use std::path::Path;

mod frontend;
mod position;
mod type_checker;

// Testing method, we will need compilation unit in the long run (collection of files/modules)
pub fn compile_file<P: AsRef<Path>>(path: P) {
    let source = std::fs::read_to_string(path).expect("Can't read file");

    // This is the first pass of our "compiler"
    let tokenizer = frontend::tokenizer::make_tokenizer(&source).map(|r| r.map_err(|e| e.into()));

    // TODO Insert indentation between tokenizer and tokens
    let indented = frontend::indentation::layout(tokenizer);

    let tokens: Vec<_> = indented.collect();
    println!(
        "frontend token errors: {:?}",
        tokens.iter().filter(|r| r.is_err()).collect::<Vec<_>>()
    );
    println!(
        "first tokens: {:?}",
        tokens
            .iter()
            .skip(0)
            .take(35)
            .map(|s| s.as_ref().map(|s| s.1.clone()))
            .collect::<Vec<_>>()
    );

    // parser
    // TODO Needs to use indentation::Error now, which lead me to think we should probably
    // have a frontend::Error enum instead of each module taking over the previous one.
    let ast = frontend::parser::parse(tokens.into_iter());

    // TODO Be a bit smarter in how we show the tokens above, instead of taking the first
    // we should take the one around the error below.
    // Ultimately using codespan-reporting could even be better as it would point to
    // the source code itself. Although having tokens is valuable to debug the parser itself.
    println!("frontend AST: {:#?}", ast);

    // desugar
    // type check
    // core (first IR)

    // then depending on the program type
    // evaluate (repl)
    // web assembly (emitted)
}
