//! Home to the `Name` and `QualName` primitives
//!
//! `Name` is a generic identifier, which can be qualified (refers to a value/type and its
//! module) or not. `QualName` is an identifier which have a reference to its module.
//!
//! While `Name` is pretty useful during parsing (it let us have a generic, simple
//! to manipulate, type), `QualName` is actually the identifier we want for every
//! subsequent phases as we have to refer to the actual property of that identifier,
//! and each identifier's name is only unique within its own module.
//!
//! Having a dedicated type let us enforce such distinction at compile time (as well
//! as having a potential performance boost by not having to parse the underlying
//! `String` on each access).
//!
//! In the future, and if performance requires it, this module will probably also host
//! the interner for qualified and unqualified names.

/// new type over identifier names
///
/// This is a simple `String` representing an identifier name. In the future, we
/// might want to introduce interning (either on `Name` or `QualName`).
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Name(pub String);

impl Name {
    /// Qualify the existing name with a module
    // TODO Return QualName ?
    // TODO Rename to qualify_with_str (+ use Into<String> generic)
    pub fn qualify_with(self, s: String) -> Name {
        // TODO We want a check on s to make sure it's upper case
        Name(format!("{}.{}", s, self.0))
    }

    // TODO Return QualName ?
    pub fn qualify_with_name(&self, qual: &Name) -> Option<QualName> {
        QualName::from_strs(&self.0, qual.0.split("."))
    }

    // TODO tests
    pub fn starts_with(&self, other: &Name) -> bool {
        self.0.starts_with(&other.0)
    }

    pub fn to_qual(&self) -> Option<QualName> {
        QualName::from_str(&self.0)
    }
}

impl From<&str> for Name {
    fn from(n: &str) -> Self {
        Name(n.to_string())
    }
}

/// Qualified name
///
/// This can though as a non empty vector, where the non empty part is at the end.
/// For example, `My.Module.function` is a vec of `My`, `Module`, and `function` with
/// the always non-empty part is `function` and the rest is a standard vector.
///
/// TODO Can `module` be empty ? This actually lead to the question, should this
/// struct require the presence of a module name (would need to be the current one
/// for local names) ?
///
/// It actually make sense to require it, that way we know for sure that, once we are
/// given a `QualName`, no further resolution is necessary.
///
/// - TODO: Change `QualName::from_str` to return an `Option`
/// - TODO: Change `Name.to_qual` to return an `Option`
#[derive(Debug, PartialEq, Clone)]
pub struct QualName {
    module: Vec<String>,
    name: String,
}

impl QualName {
    pub fn from_str<S: Into<String>>(s: S) -> Option<QualName> {
        let name = s.into();
        let mut segments: Vec<_> = name.split(".").map(String::from).collect();

        match segments.len() {
            1 => None,
            _ => Some(QualName {
                name: segments.pop().unwrap(),
                module: segments,
            }),
        }
    }

    // TODO Write tests
    pub fn from_strs<S1, S2, I>(name: S1, prefix: I) -> Option<QualName>
    where
        S1: Into<String>,
        S2: Into<String>,
        I: Iterator<Item = S2>,
    {
        let name = name.into();
        let module: Vec<String> = prefix.map(|s| s.into()).collect();

        match module.len() {
            0 => None,
            _ => Some(QualName { name, module }),
        }
    }

    // TODO Write tests
    pub fn to_name(&self) -> Name {
        if self.module.len() > 0 {
            Name(format!("{}.{}", self.module.join("."), self.name))
        } else {
            Name(self.name.clone())
        }
    }
}

impl From<&'static str> for QualName {
    fn from(n: &str) -> Self {
        QualName::from_str(n).expect(&format!(
            "From conversion should only be used with qualified name ('{}' used)",
            &n
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unqual_to_qual() {
        let name: Name = "My.function".into();

        assert_eq!(
            name.to_qual(),
            Some(QualName {
                module: vec!["My".into()],
                name: "function".into()
            })
        );
    }

    #[test]
    fn qual_name_from_str() {
        assert_eq!(QualName::from_str("Int"), None);

        assert_eq!(
            QualName::from_str("Basics.Int"),
            Some(QualName {
                module: vec!["Basics".into()],
                name: "Int".into()
            })
        );

        assert_eq!(
            QualName::from_str("My.App.Module.function"),
            Some(QualName {
                module: vec!["My".into(), "App".into(), "Module".into(),],
                name: "function".into()
            })
        );
    }
}
