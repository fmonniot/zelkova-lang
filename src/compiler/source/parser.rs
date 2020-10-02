//! Transform a serie of tokens into the AST.
//! We are using [lalrpop] to generate the rust code for the grammar.
//!
//! [lalrpop]: https://github.com/lalrpop/lalrpop/

use super::error::Error;
use super::tokenizer::Spanned;
use super::Module;

lalrpop_mod!(grammar, "/compiler/source/grammar.rs");

pub fn parse(i: impl Iterator<Item = Result<Spanned, Error>>) -> Result<Module, Error> {
    Ok(grammar::ModuleParser::new().parse(i)?)
}
