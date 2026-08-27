//! This module contains the exhaustiveness checker for pattern matching.
//!
//! The checker itself is not written yet: [`check`] accepts every module. What is
//! here is the error it will report, and that is deliberate — an uninhabited
//! `enum Error {}` made `check` unable to fail, which in turn made the conversion
//! into a `CompilationError` a `todo!()` that nothing could reach and nobody had to
//! justify. The first error the real checker reported would have panicked the
//! compiler.
use super::canonical::Module;
use super::name::Name;
use super::PhaseError;

/// A `case` expression whose branches do not cover every variant of the type it
/// scrutinises.
///
/// Nothing constructs this yet — [`check`] is a stub. It is defined now so that the
/// phase has a renderable error before it has a checker, the same way
/// `canonical::Error::InvalidTupleSize` is kept for a rejection path that does not
/// exist yet. Writing the checker means constructing this, not inventing an error
/// type and a diagnostic at the same time.
#[derive(Debug, PartialEq)]
pub enum Error {
    NonExhaustiveMatch {
        /// The value whose body holds the `case` expression.
        value: Name,
        /// The union type being matched on.
        tpe: Name,
        /// The variants of `tpe` that no branch matches.
        missing: Vec<Name>,
    },
}

impl PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::NonExhaustiveMatch { value, tpe, .. } => format!(
                "the `case` expression in `{}` does not cover every variant of `{}`",
                value, tpe
            ),
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Error::NonExhaustiveMatch { missing, .. } => vec![format!(
                "no branch matches: {}",
                missing
                    .iter()
                    .map(|n| n.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )],
        }
    }
}

/// Verify that every `case` expression in `module` covers its scrutinee's variants.
///
/// Currently a stub: it inspects nothing and accepts every module. The
/// `Vec<Error>` return is the accumulating shape the other phases use — when the
/// checker is written, one uncovered `case` must not hide the next.
pub fn check(_module: &Module) -> Result<(), Vec<Error>> {
    Ok(())
}
