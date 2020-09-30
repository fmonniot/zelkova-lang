//! Transform a serie of tokens into the frontend AST.

use super::Module;
use crate::compiler::frontend::error::Error;
use crate::compiler::frontend::tokenizer::Spanned;

lalrpop_mod!(grammar, "/compiler/frontend/grammar.rs");

pub fn parse(i: impl Iterator<Item = Result<Spanned, Error>>) -> Result<Module, Error> {
    Ok(grammar::ModuleParser::new().parse(i)?)
}
