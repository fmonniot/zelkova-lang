use std::path::Path;

mod frontend;
mod position;
mod type_checker;

// Testing method, we will need compilation unit in the long run (collection of files/modules)
pub fn compile_file<P: AsRef<Path>>(path: P) {
    let source = std::fs::read_to_string(path).expect("Can't read file");

    // This is the first pass of our "compiler"
    let tokenizer = frontend::tokenizer::make_tokenizer(&source);
    let tokens: Vec<_> = tokenizer.collect();
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

    // next passes
    // parser
    let ast = frontend::parser::parse(tokens.into_iter());
    println!("frontend AST: {:#?}", ast);

    // desugar
    // type check
    // core (first IR)

    // then depending on the program type
    // evaluate (repl)
    // web assembly (emitted)
}
