use std::path::Path;

use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::termcolor::{Color, ColorSpec, StandardStream};
use codespan_reporting::term::{self, ColorArg};
use std::io::Write;
use std::str::FromStr;

mod frontend;
mod position;
mod source_files;
mod type_checker;

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
        .map(|(idx, file)| frontend::parse(file).map_err(|err| err.diagnostic(idx)))
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
