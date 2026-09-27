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
//! `String` on each access, although at the cost of more memory usage).
//!
//! In the future, if performance requires it, this module will probably also host
//! the interner for qualified and unqualified names.

use super::PackageName;

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
    /// `Name` and not a [`QualName`]: a spelling says nothing about which package
    /// declared what it reaches.
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

/// Qualified name: a declaration, named by the package and the module that declare it.
///
/// The module half can be thought of as a non-empty vector: `My.Module.function` is a vec of
/// `My` and `Module`, plus the always non-empty `function`.
///
/// # Why the package is part of it
///
/// A module's name is unique within its package and not across a build: a package may
/// hold its own `Size` beside a dependency's, which it reaches as `AcmeWidgets.Size`
/// ([*What a package boundary cannot
/// rename*](../../../docs/spec/packages.md#what-a-package-boundary-cannot-rename)). Both
/// declare `Size.Size`, and only the package tells the two apart. With it, every
/// `QualName` names one declaration on its own: once we are given a `QualName`, no further
/// resolution is necessary, and two `QualName`s are equal exactly when they name the same
/// declaration. Every map the phases after canonicalization key by one — the typer's
/// unions and constructors, a scalar's identity ([`Scalar::declares`]) — relies on that.
///
/// The package is part of the identity and of nothing a source file writes:
/// [`to_name`](QualName::to_name) leaves it out, since `acme-widgets` is a spelling no
/// Zelkova source contains.
///
/// [`Scalar::declares`]: super::scalars::Scalar::declares
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct QualName {
    package: PackageName,
    module: Vec<String>,
    name: String,
}

impl QualName {
    /// The name `s` spells, declared in `package`: `Basics.Int` is the module `Basics`
    /// and the name `Int`. `None` when `s` has no module half.
    pub fn parse<S: Into<String>>(package: PackageName, s: S) -> Option<QualName> {
        let name = s.into();
        let mut segments: Vec<_> = name.split('.').map(String::from).collect();

        match segments.pop() {
            Some(name) if !segments.is_empty() => Some(QualName {
                package,
                name,
                module: segments,
            }),
            _ => None,
        }
    }

    /// The name `name`, declared in `package` by the module whose segments `prefix`
    /// yields. `None` when `prefix` yields nothing.
    // TODO Write tests
    pub fn from_strs<S1, S2, I>(package: PackageName, name: S1, prefix: I) -> Option<QualName>
    where
        S1: Into<String>,
        S2: Into<String>,
        I: Iterator<Item = S2>,
    {
        let name = name.into();
        let module: Vec<String> = prefix.map(|s| s.into()).collect();

        match module.len() {
            0 => None,
            _ => Some(QualName {
                package,
                name,
                module,
            }),
        }
    }

    /// The name `name`, as declared by `module` of `package`.
    ///
    /// `module` is written out with its dots, the way a `module` header writes it, and
    /// is split back into segments here; `name` is kept whole. It is how
    /// [`ModuleName::qualify_name`](super::ModuleName::qualify_name) names a declaration
    /// of a module it holds, and how a name the compiler knows without reading it from a
    /// source file — a [scalar](super::scalars) — becomes a `QualName`.
    ///
    /// **`module` must be a non-empty module path.** That is a precondition on the
    /// caller, not a check: this returns a `QualName` rather than an `Option` because
    /// the two halves arrive separately and there is no text to go looking for a module
    /// in, which is what keeps the scalar names off any `unwrap` path. It is *not* a
    /// claim that every input is well formed — `in_module(package, "", "Bool")` builds a
    /// name that renders as `.Bool` and that
    /// [`Scalar::declares`](super::scalars::Scalar::declares) recognises for nothing,
    /// where [`QualName::parse`] and [`QualName::from_strs`] would both have returned
    /// `None`. Every caller passes a module path it holds.
    pub fn in_module(package: PackageName, module: &str, name: &str) -> QualName {
        QualName {
            package,
            module: module.split('.').map(String::from).collect(),
            name: name.to_string(),
        }
    }

    /// The name `name`, declared by the same module of the same package as this one.
    ///
    /// It is how a union's constructors are named from the union: a constructor is
    /// declared by the module that declares the type it builds.
    pub fn sibling(&self, name: &Name) -> QualName {
        QualName {
            package: self.package.clone(),
            module: self.module.clone(),
            name: name.as_str().to_string(),
        }
    }

    /// The package that declares this name.
    pub fn package(&self) -> &PackageName {
        &self.package
    }

    /// The module and the name, written out with dots — `Size.Size` — and without the
    /// package, which no source spells.
    // TODO Write tests
    pub fn to_name(&self) -> Name {
        if !self.module.is_empty() {
            Name(format!("{}.{}", self.module.join("."), self.name))
        } else {
            Name(self.name.clone())
        }
    }

    pub fn unqualified_name(&self) -> Name {
        Name(self.name.clone())
    }

    /// The module half, written out with its dots — `My.App` of `My.App.function`.
    ///
    /// The counterpart of [`QualName::unqualified_name`]; the two halves rejoined are
    /// [`QualName::to_name`].
    pub fn module_name(&self) -> Name {
        Name(self.module.join("."))
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

    fn package() -> PackageName {
        PackageName::new("test-project").unwrap()
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

    #[test]
    fn qual_name_from_str() {
        assert_eq!(QualName::parse(package(), "Int"), None);

        assert_eq!(
            QualName::parse(package(), "Basics.Int"),
            Some(QualName {
                package: package(),
                module: vec!["Basics".into()],
                name: "Int".into()
            })
        );

        assert_eq!(
            QualName::parse(package(), "My.App.Module.function"),
            Some(QualName {
                package: package(),
                module: vec!["My".into(), "App".into(), "Module".into(),],
                name: "function".into()
            })
        );
    }

    /// The package is part of the identity: the same spelling declared by two packages
    /// is two names, and neither spells its package.
    #[test]
    fn a_package_tells_two_same_spelled_names_apart() {
        let widgets = PackageName::new("acme-widgets").unwrap();
        let mine = QualName::in_module(package(), "Size", "Size");
        let theirs = QualName::in_module(widgets.clone(), "Size", "Size");

        assert_ne!(mine, theirs);
        assert_eq!(mine.to_name(), theirs.to_name());
        assert_eq!(theirs.package(), &widgets);
    }

    #[test]
    fn a_sibling_shares_the_package_and_the_module() {
        let widgets = PackageName::new("acme-widgets").unwrap();
        let union = QualName::in_module(widgets.clone(), "Acme.Size", "Size");

        assert_eq!(
            union.sibling(&"Small".into()),
            QualName::in_module(widgets, "Acme.Size", "Small")
        );
    }
}
