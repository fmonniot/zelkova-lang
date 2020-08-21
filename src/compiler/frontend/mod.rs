//! This module contains all the data types representing the frontend
//! of our language.
//!
//! The frontend part is what the user interact with, and contains some
//! syntax sugar to make their life easier. For example, pattern matching
//! in function declaration is one such thing.
//!
pub mod parser;
pub mod tokenizer;

/// new type over identifier names
#[derive(Debug, PartialEq, Clone)]
pub struct Name(pub String);

/// A declared type in a module. This is used in type annotations.
#[derive(Debug, PartialEq)]
pub enum Type {
    Named(Name),
    Arrow(Box<Type>, Box<Type>),
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
}

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
#[derive(Debug, PartialEq)]
pub struct FunType {
    pub name: Name,
    pub tpe: Type,
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
///         name: Name("identity"),
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
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Lit(Literal), // Literal, as other are fully named
    Application(Box<Expression>, Box<Expression>),
    Variable(Name),
    Tuple(Box<Vec<Expression>>),
}

/// A literal
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frontend_type_curry_arrow() {
        // String -> Int
        let arrow1 = Type::Arrow(
            Box::new(Type::Named(Name("String".to_string()))),
            Box::new(Type::Named(Name("Int".to_string()))),
        );

        // String -> Int
        let arrow2 = Type::Arrow(
            Box::new(Type::Named(Name("String".to_string()))),
            Box::new(Type::Named(Name("Int".to_string()))),
        );

        // (String -> Int) -> String -> Int
        let expected = Type::Arrow(
            Box::new(Type::Arrow(
                Box::new(Type::Arrow(
                    Box::new(Type::Named(Name("String".to_string()))),
                    Box::new(Type::Named(Name("Int".to_string()))),
                )),
                Box::new(Type::Named(Name("String".to_string()))),
            )),
            Box::new(Type::Named(Name("Int".to_string()))),
        );

        assert_eq!(Type::curry_arrow(arrow1, arrow2), expected);
    }
}
