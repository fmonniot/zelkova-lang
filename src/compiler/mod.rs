use std::path::Path;

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::StandardStream;
use codespan_reporting::term::{self, ColorArg};
use std::str::FromStr;

mod frontend;
mod position;
mod type_checker;

// Testing method, we will need compilation unit in the long run (collection of files/modules)
pub fn compile_file<P: AsRef<Path>>(path: P) {
    let path = path.as_ref();
    let file_name = path.file_name().unwrap().to_string_lossy();
    let source = std::fs::read_to_string(path).expect("Can't read file");
    let source_file = SimpleFile::new(file_name, &source);

    // Error reporter
    let writer = StandardStream::stderr(ColorArg::from_str("auto").unwrap().into());
    let config = codespan_reporting::term::Config {
        tab_width: 2,
        ..codespan_reporting::term::Config::default()
    };

    // Tokenize the source code into a serie of tokens
    let tokenizer = frontend::tokenizer::make_tokenizer(&source).map(|r| r.map_err(|e| e.into()));

    // Then manage the indentation aspect of our code
    let indented = frontend::indentation::layout(tokenizer);

    // Some debug instruction to easily find errors early on.
    // This is for development only, and we should have a better error reporting
    // system in the future.
    let tokens: Vec<_> = indented.collect();
    let token_errors: Vec<_> = tokens.iter().filter_map(|r| r.as_ref().err()).collect();
    for err in token_errors {
        term::emit(&mut writer.lock(), &config, &source_file, &err.diagnostic()).unwrap();
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
            term::emit(&mut writer.lock(), &config, &source_file, &err.diagnostic()).unwrap();
        }
    }

    // desugar
    // type check
    // core (first IR)

    // then depending on the program type
    // evaluate (repl)
    // web assembly (emitted)
}
