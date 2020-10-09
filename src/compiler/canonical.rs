//! The canonical representation of a zelkova programs is a translation of a local
//! source into the broader world.
//!
//! For example, all terms and types will be scoped to their proper package and
//! module name. This let us have one global scope when referencing to other
//! terms (most notably while type checking a module).
use super::source;
use super::Interface;
use super::{ModuleName, PackageName};
use std::collections::HashMap;

pub struct Module {}

pub enum Error {}

pub fn canonicalize(
    package: PackageName,
    interfaces: HashMap<ModuleName, Interface>,
    source: source::Module,
) -> Result<Module, Error> {
    todo!()
}
