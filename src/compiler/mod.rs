use std::path::Path;

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::termcolor::{Color, ColorSpec, StandardStream};
use codespan_reporting::term::{self, ColorArg};
use std::io::Write;
use std::str::FromStr;

mod frontend;
mod position;
mod source_files;
mod type_checker;

use frontend::Module;
use source_files::SourceFiles;

pub fn compile_files<'a, T>(paths: T)
where
    T: IntoIterator<Item = &'a Path>,
{
    // Build files structure
    let mut files = SourceFiles::new();
    for path in paths {
        let path: &Path = path.as_ref();
        let file_name = path.file_name().unwrap().to_string_lossy().to_string();
        let source = std::fs::read_to_string(path).expect("Can't read file");

        files.add(file_name, source);
    }

    // Error reporter
    let mut writer = StandardStream::stderr(ColorArg::from_str("auto").unwrap().into());
    let config = codespan_reporting::term::Config {
        tab_width: 2,
        ..codespan_reporting::term::Config::default()
    };

    // frontend pass
    let results: Vec<_> = files
        .iter()
        .enumerate()
        .map(|(idx, file)| frontend(file).map_err(|err| err.diagnostic(idx)))
        .collect();

    for result in results {
        match result {
            Ok(m) => {
                writer
                    .set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Green)))
                    .unwrap();
                write!(&mut writer, "success").unwrap();
                writer.reset().unwrap();
                writeln!(&mut writer, ": frontend pass on module {}", m.name.0).unwrap();
            }
            Err(err) => {
                term::emit(&mut writer.lock(), &config, &files, &err).unwrap();
            }
        }
    }

    // desugar
    // type check
    // core (first IR)

    // then depending on the program type
    // evaluate (repl)
    // web assembly (emitted)
}

fn frontend(source_file: &SimpleFile<String, String>) -> Result<Module, frontend::error::Error> {
    let source = source_file.source();

    // Tokenize the source code into a serie of tokens
    let tokenizer = frontend::tokenizer::make_tokenizer(source).map(|r| r.map_err(|e| e.into()));

    // Then manage the indentation aspect of our code
    let indented = frontend::layout::layout(tokenizer);

    // Some debug instruction to easily find errors early on.
    // This is for development only, and we should have a better error reporting
    // system in the future.
    let tokens: Vec<_> = indented.collect();

    // parser
    // TODO Should works on reference and not consume the original iterator
    frontend::parser::parse(tokens.iter().cloned())
}
