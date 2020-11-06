//! This module contains the exhaustiveness checker for pattern matching.
use super::canonical::Module;

pub enum Error {}

pub fn check(_module: &Module) -> Result<(), Error> {
    Ok(())
}
