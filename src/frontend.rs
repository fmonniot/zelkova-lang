//! This module contains all the data types representing the frontend
//! of our language.
//!
//! The frontend part is what the user interact with, and contains some
//! syntax sugar to make their life easier. For example, pattern matching
//! in function declaration is one such thing.

/// new type over everything that is named in our language
#[derive(Debug, PartialEq)]
pub struct Name(pub String);

/// A Module is the top-level structure for a source file.
///
/// It contains everything in the file.
#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: Name,
    pub exposing: Vec<Name>,
    pub declarations: Vec<Declaration>,
}

/// A Declaration is everything that compose a `Module`.
#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function(BindGroup),
    FunctionType(FunType),
    // type aliases, custom types, import and ports will end up here
}

/// Represents the type signature of a particular function
///
/// The current representation is very simple and only
/// represent constant functions. This will be iterated
/// upon ;)
#[derive(Debug, PartialEq)]
pub struct FunType {
    pub name: Name,
    pub tpe: Name, // This might be a type annotation instead of a Name
}

/// A BindGroup is one of the (possibly) multiple function declaration.
///
/// Example:
///
/// ### Function decomposition
///
/// `const = 42` in Zelkova will result in the following AST:
/// ```
/// Declaration::Function(
///     BindGroup {
///         name: Name("const"),
///         patterns: [
///             Match {
///                 pattern: [],
///                 body: Expression::Lit(Literal::Int(42)),
///             }
///         ]
///     }
/// )
/// ```
///
///
/// `identity x y = x` in Zelkova will result in the following AST:
/// ```
/// Declaration::Function(
///     BindGroup {
///         name: Name("const"),
///         patterns: [
///             Match {
///                 pattern: [ Pattern::Var(Name("x")), Pattern::Var(Name("y")) ],
///                 body: Expression::Var(Name("x")),
///             }
///         ]
///     }
/// )
/// ```
#[derive(Debug, PartialEq)]
pub struct BindGroup {
    pub name: Name,
    pub patterns: Vec<Match>,
}

/// The match structure is composed of a serie of patterns and an associated expression
#[derive(Debug, PartialEq)]
pub struct Match {
    pub pattern: Vec<Pattern>,
    pub body: Expression,
}

/// A pattern is the left handside of a pattern-match expression
///
/// A pattern-matching expression can be present in function declaration
/// or as part of the `case of` syntax.
#[derive(Debug, PartialEq)]
pub enum Pattern {
    Var(Name),
    Lit(Literal),
}

/// An Expression
#[derive(Debug, PartialEq)]
pub enum Expression {
    Lit(Literal), // Literal, as other are fully named
    Application(Box<Expression>, Box<Expression>),
    Variable(Name),
}

/// A literal
#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
}
