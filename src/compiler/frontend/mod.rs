//! This module contains all the data types representing the frontend
//! of our language.
//!
//! The frontend part is what the user interact with, and contains some
//! syntax sugar to make their life easier. For example, pattern matching
//! in function declaration is one such thing.
//!
pub mod error;
pub mod layout;
pub mod parser;
pub mod tokenizer;

use std::collections::HashMap;

/// new type over identifier names
// TODO At some point I think it'd make sense to have Upper/Lower name
// instead of a catch-all type.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Name(pub String);

impl Name {
    fn is_type(&self) -> bool {
        // If the name exists, because it has to go through the tokenizer,
        // it will have a length of at least one character.
        let c = self.0.chars().next().unwrap();

        c.is_uppercase()
    }
}

/// A part of a declared type. This is also used in type annotations.
///
/// Types with no arguments will be composed of exactly one `Type`.
/// As their name indicates, a type with arguments will requires more
/// `Type` as arguments. For example the optional type will require
/// one no-arg type: `Maybe Int` (this is another name for higher-kinded types)
#[derive(Debug, PartialEq)]
pub enum Type {
    /// Type constructor
    Unqualified(Name, Box<Vec<Type>>),
    Arrow(Box<Type>, Box<Type>),
    // Qualified type eg Maybe.Maybe
    /// type variable
    Variable(Name),
}

impl Type {
    /// Special constructor for an arrow, when we want to apply currying
    /// on the right hand side.
    pub fn curry_arrow(t1: Type, t2: Type) -> Type {
        match t2 {
            Type::Arrow(t21, t22) => {
                // t1 -> (t21 -> t22)
                // we want t1 -> t21 -> t22
                Type::Arrow(Box::new(Type::Arrow(Box::new(t1), t21)), t22)
            }
            _ => Type::Arrow(Box::new(t1), Box::new(t2)),
        }
    }

    pub fn unqualified(name: Name) -> Type {
        Type::Unqualified(name, Box::new(vec![]))
    }

    pub fn unqualified_with(name: Name, types: Vec<Type>) -> Type {
        Type::Unqualified(name, Box::new(types))
    }
}

/// A Module is the top-level structure for a source file.
///
/// It contains everything in a source file.
///
///
/// The elm compiler declare a module as follow:
/// ```
/// data Module =
///   Module
///     { _name    :: ModuleName.Canonical
///     , _exports :: Exports
///     , _docs    :: Src.Docs
///     , _decls   :: Decls
///     , _unions  :: Map.Map Name Union
///     , _aliases :: Map.Map Name Alias
///     , _binops  :: Map.Map Name Binop
///     , _effects :: Effects
///     }
/// ```
#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: Name,
    pub exposing: Exposing,
    pub imports: Vec<Import>,
    pub types: Vec<UnionType>,
    pub functions: Vec<Function>,
}

impl Module {
    fn from_declarations(name: Name, exposing: Exposing, declarations: Vec<Declaration>) -> Module {
        let mut imports = vec![];
        let mut types = vec![];
        let mut functions = HashMap::<Name, Vec<Declaration>>::new();

        for declaration in declarations {
            match declaration {
                Declaration::Function(FunBinding { ref name, .. })
                | Declaration::FunctionType(FunType { ref name, .. }) => {
                    match functions.get_mut(&name) {
                        Some(decls) => decls.push(declaration),
                        None => {
                            functions.insert(name.clone(), vec![declaration]);
                        }
                    }
                }
                Declaration::Import(i) => imports.push(i),
                Declaration::Union(t) => types.push(t),
            }
        }

        let functions = functions.into_iter().map(|(name, decls)| {
            let mut tpe = None;
            let mut bindings = vec![];

            // TODO Error if more than function type is defined

            for d in decls {
                match d {
                    Declaration::Function(b) => bindings.push(b.pattern),
                    Declaration::FunctionType(t) => {tpe.replace(t.tpe);}
                    _ => panic!("Invalid kind of declaration used in functions, report this error ({:?})", d),
                }
            }

            Function { name, tpe, bindings }
        }).collect::<Vec<_>>();

        Module {
            name,
            exposing,
            imports,
            types,
            functions,
        }
    }
}

/// `Function` represent a function declaration in the source code.
///
/// In _zelkova_, like in _Haskell_ but as opposed to _Elm_, we can
/// use pattern matching and multiple line to declare a function:
///
/// ```zel
/// map : (a -> b) -> Maybe a -> Maybe b
/// map f (Just value) = f value
/// map _ Nothing => Nothing
/// ```
///
/// Because of this syntax, a function is defined as a list of `Match`
/// statement, which each entry specifying a line. The parser will happily let
/// us define a function which doesn't match all cases, a later phase will
/// need to check this.
#[derive(Debug, PartialEq)]
pub struct Function {
    name: Name,
    tpe: Option<Type>,
    bindings: Vec<Match>,
}

/// Exposing represent whether an import (or export) expose terms.
///
/// `Exposing::Explicit` represents a selection of term exported
/// (or imported) for a given module.
///
/// `Exposing::Open` means every top-level terms are exported, or when
/// used in imports, all exported terms are imported.
#[derive(Debug, PartialEq)]
pub enum Exposing {
    Open,
    Explicit(Vec<Exposed>),
}

/// Exposed represent the terms exposed/imported by a module.
#[derive(Debug, PartialEq)]
pub enum Exposed {
    Lower(Name),
    Upper(Name, Privacy),
    Operator(Name),
}

/// Privacy control how a custom type is exposed.
///
/// For example, given the following type:
/// ```type MyType = VariantA | VariantB```
///
/// When importing or exporting this type, we have
/// two privacy settings:
/// - public: `MyType(..)`. In this mode the variant
///   constructors are made public to other modules.
/// - private: `MyType`. In this mode the custom type
///   is behaving as an opaque type. Other module can't
///   know what's inside this type.
#[derive(Debug, PartialEq)]
pub enum Privacy {
    Public,
    Private,
}

/// A Declaration is a top-level block and is the basis for a `Module`.
#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function(FunBinding),
    FunctionType(FunType),
    Import(Import),
    /// Union types are also called custom types in Elm
    Union(UnionType),
    // type aliases, infixes and ports will end up here
}

/// A representation of the `import` declaration
#[derive(Debug, PartialEq)]
pub struct Import {
    name: Name,
    alias: Option<Name>,
    exposing: Exposing,
}

/// Represents the type signature of a particular function
#[derive(Debug, PartialEq)]
pub struct FunType {
    pub name: Name,
    pub tpe: Type,
}

#[derive(Debug, PartialEq)]
pub struct UnionType {
    pub name: Name,
    pub type_arguments: Vec<Name>,
    pub variants: Vec<Type>, // TODO Restrict to Type::Unqualified
}

/// A `FunBinding` is one of the (possibly multiple) function declaration.
///
/// ## Examples
///
///
/// `const = 42` in Zelkova will result in the following AST:
/// ```
/// Declaration::Function(
///     FunBinding {
///         name: Name("const"),
///         patterns: Match {
///             pattern: [],
///             body: Expression::Lit(Literal::Int(42)),
///         }
///     }
/// )
/// ```
///
///
/// `identity x y = x` in Zelkova will result in the following AST:
/// ```
/// Declaration::Function(
///     FunBinding {
///         name: Name("identity"),
///         pattern: Match {
///             patterns: [ Pattern::Var(Name("x")), Pattern::Var(Name("y")) ],
///             body: Expression::Var(Name("x")),
///         ]
///     }
/// )
/// ```
#[derive(Debug, PartialEq)]
pub struct FunBinding {
    pub name: Name,
    pub pattern: Match,
}

/// The match structure is composed of a serie of patterns and an associated expression
#[derive(Debug, PartialEq)]
pub struct Match {
    pub patterns: Vec<Pattern>,
    pub body: Expression,
}

/// A pattern is the left handside of a pattern-match expression
///
/// A pattern-matching expression can be present in function declaration
/// or as part of the `case of` syntax.
///
/// ## Missing Patterns
/// - `Record [Name]`
/// - `Alias Pattern (Name)`
/// - `Unit`
/// - `Tuple Pattern Pattern [Pattern]`
/// - `Ctor Name [Pattern]`
/// - `CtorQual Name Name [Pattern]`
/// - `List [Pattern]`
/// - `Cons Pattern Pattern`
#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Variable(Name),
    Literal(Literal),
    Anything,
}

/// An Expression
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Lit(Literal), // Literal, as other are fully named
    Application(Box<Expression>, Box<Expression>),
    Variable(Name), // TODO Qualified variable
    Tuple(Box<Vec<Expression>>),
    Case(Box<Expression>, Vec<CaseBranch>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseBranch {
    patterns: Vec<Pattern>,
    expression: Expression,
}

/// A literal
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frontend_type_curry_arrow() {
        // String -> Int
        let arrow1 = Type::Arrow(
            Box::new(Type::unqualified(Name("String".to_string()))),
            Box::new(Type::unqualified(Name("Int".to_string()))),
        );

        // String -> Int
        let arrow2 = Type::Arrow(
            Box::new(Type::unqualified(Name("String".to_string()))),
            Box::new(Type::unqualified(Name("Int".to_string()))),
        );

        // (String -> Int) -> String -> Int
        let expected = Type::Arrow(
            Box::new(Type::Arrow(
                Box::new(Type::Arrow(
                    Box::new(Type::unqualified(Name("String".to_string()))),
                    Box::new(Type::unqualified(Name("Int".to_string()))),
                )),
                Box::new(Type::unqualified(Name("String".to_string()))),
            )),
            Box::new(Type::unqualified(Name("Int".to_string()))),
        );

        assert_eq!(Type::curry_arrow(arrow1, arrow2), expected);
    }
}
