//! Helpers functions and macros to support testing the frontend
//! part of the compiler.
//!
use zelkova_lang::compiler::frontend::*;

// macros to simplify tests

#[macro_export]
macro_rules! test_parse_ok {
    ($test_name: ident, $source: expr, $expected: expr $(,)?) => {
        #[test]
        fn $test_name() {
            use codespan_reporting::files::SimpleFile;
            use codespan_reporting::term::termcolor::StandardStream;
            use codespan_reporting::term::{self, ColorArg};
            use std::str::FromStr;
            use zelkova_lang::compiler::frontend;

            let test_name = stringify!($test_name);

            let _ = ::env_logger::try_init();
            let source = indoc::indoc! {$source}.to_string();
            let file = SimpleFile::new(test_name.to_owned(), source);

            let e = frontend::parse(&file);

            match e {
                Ok(expr) => assert_eq!(expr, $expected),
                Err(err) => {
                    let writer = StandardStream::stderr(ColorArg::from_str("auto").unwrap().into());
                    let config = codespan_reporting::term::Config {
                        tab_width: 2,
                        ..codespan_reporting::term::Config::default()
                    };

                    term::emit(&mut writer.lock(), &config, &file, &err.diagnostic(())).unwrap();
                    assert_eq!(
                        None,
                        Some(err),
                        "{} should not produce an error",
                        test_name
                    );
                }
            }
        }
    };
}

// TODO test_parse_error(source, error) -> macro (test_name, source, error)

// AST constructor as simple functions

pub fn name(name: &str) -> Name {
    Name(name.to_string())
}
