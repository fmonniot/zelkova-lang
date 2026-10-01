//! Home to the `Name` primitive: an identifier, qualified or not, as a source file spells it.
//!
//! `QualName`, the identifier every phase after parsing reaches for, is the compiler's and
//! lives in `zelkova_compiler::name`, which re-exports this `Name` beside it.

/// new type over identifier names
///
/// This is a simple `String` representing an identifier name. In the future, we
/// might want to introduce interning (either on `Name` or `QualName`). The inner
/// `String` is private so that such a change stays local to this module; reach
/// for `Name::new`, `as_str`, `From<&str>`, or `Display` instead of the field.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Name(String);

impl Name {
    /// Build a `Name` from any owned-or-borrowed string source.
    pub fn new<S: Into<String>>(s: S) -> Name {
        Name(s.into())
    }

    /// Borrow the underlying identifier text.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Qualify the existing name with a module: `Size` qualified with `AcmeWidgets` is
    /// `AcmeWidgets.Size`.
    ///
    /// The result is a *spelling* — what a source file writes to reach a name through a
    /// module, an alias or a namespace — and not an identity, which is why it is a
    /// `Name` and not a `zelkova_compiler::name::QualName`: a spelling says nothing about
    /// which package declared what it reaches.
    // TODO Rename to qualify_with_str (+ use Into<String> generic)
    pub fn qualify_with(self, s: String) -> Name {
        // TODO We want a check on s to make sure it's upper case
        Name(format!("{}.{}", s, self.0))
    }

    /// The last dot-separated segment of this name: `add` of `Js.Basics.add`, and the
    /// whole name when it has no dot.
    ///
    /// A qualified spelling names a declaration by its own name in its last segment,
    /// whatever module or alias the segments before it wrote.
    pub fn last_segment(&self) -> Name {
        match self.0.rsplit_once('.') {
            Some((_, last)) => Name(last.to_string()),
            None => self.clone(),
        }
    }

    // TODO tests
    pub fn starts_with(&self, other: &Name) -> bool {
        self.0.starts_with(&other.0)
    }
}

impl From<&str> for Name {
    fn from(n: &str) -> Self {
        Name(n.to_string())
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_and_as_str_roundtrip() {
        let name = Name::new("hello");
        assert_eq!(name.as_str(), "hello");

        let owned = Name::new(String::from("world"));
        assert_eq!(owned.as_str(), "world");
    }

    #[test]
    fn prefixed_and_last_segment() {
        let name: Name = "My.function".into();

        assert_eq!(
            name.clone().qualify_with("Qual".to_string()),
            "Qual.My.function".into()
        );
        assert_eq!(name.last_segment(), "function".into());
        assert_eq!(Name::new("function").last_segment(), "function".into());
    }
}
