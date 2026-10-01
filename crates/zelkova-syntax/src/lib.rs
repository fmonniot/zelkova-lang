//! Zelkova's syntax: the tokenizer, the layout pass, the grammar and the `parser` AST it
//! builds, with the positions, tuples and names that AST is made of.

#[macro_use]
extern crate lalrpop_util;

pub mod name;
pub mod parser;
pub mod position;
pub mod tuple;
