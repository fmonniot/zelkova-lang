//! This module contains all the data types representing the source
//! of our language.
//!
//! This is the part the user interact with, and contains some syntax sugar
//! to make their life easier (for example, pattern matching in function
//! declaration)
//!
//! TODO Fill in the sections below
//!
//! ## Compiler phase
//! `parse`
//! ## AST
//! TODO
//! ## Modules
//! TODO hierarchy and modules.
use codespan_reporting::files::SimpleFile;

pub mod error;
pub mod layout;
pub mod tokenizer;

pub use error::Error;

use std::collections::HashMap;

lalrpop_mod!(grammar, "/compiler/parser/grammar.rs");

pub fn parse(source_file: &SimpleFile<String, String>) -> Result<Module, Error> {
    let source = source_file.source();

    // Tokenize the source code into a serie of tokens
    let tokenizer = tokenizer::make_tokenizer(source).map(|r| r.map_err(|e| e.into()));

    // Manage the indentation aspect of our code
    let indented = layout::layout(tokenizer);

    // Parse the tokens into an AST
    // TODO Should works on reference and not consume the original iterator
    let module = grammar::ModuleParser::new().parse(indented)?;

    // And do some early nitpicking
    // TODO Check module name is valid. Need to take SourceFile instead of SimpleFile as parameter.

    Ok(module)
}

// TODO Simplify all Box<Vec<_>> into Vec<_> (vec is already on the heap, no need to box it)

/// new type over identifier names
// TODO names are currently stored as a String, and destructured on-demand
// when checking for qualified/unqualified (including types). We should
// probably have a different internal representation, implement the Display
// trait and hide its internals. This will require modifying basically every
// tests we have though :(.
// TODO This might also be too generic as a solution, or we use Name too often,
// as it might conflict with record projection. Let's see when we implement
// records.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Name(pub String);

impl Name {
    fn is_type(&self) -> bool {
        // If the name exists, because it has to go through the tokenizer,
        // it will have a length of at least one character.
        let last = self.0.rsplit('.').next().unwrap();
        let c2 = last.chars().next().unwrap();

        c2.is_uppercase()
    }

    /// Qualify the existing name with a module
    fn qualify_with(self, s: String) -> Name {
        // TODO We want a check on s to make sure it's upper case
        Name(format!("{}.{}", s, self.0))
    }

    /// Whether this `Name` represents a qualified identifier or not
    fn is_qualified(&self) -> bool {
        self.0.chars().any(|c| c == '.')
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
    // Qualified type eg Maybe.Maybe
    /// Type constructor →
    ///
    /// Applications of the type constructor → are written infix and
    /// associate to the right, so T → T' → T" stands for T → (T' → T").
    Arrow(Box<Type>, Box<Type>),
    /// Type variable
    Variable(Name),
    Tuple(Box<Type>, Box<Vec<Type>>),
}

impl Type {
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
/// ```haskell
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
    pub infixes: Vec<Infix>,
    pub types: Vec<UnionType>,
    pub functions: Vec<Function>,
}

impl Module {
    fn from_declarations(name: Name, exposing: Exposing, declarations: Vec<Declaration>) -> Module {
        let mut imports = vec![];
        let mut types = vec![];
        let mut infixes = vec![];
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
                Declaration::Infix(i) => infixes.push(i),
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
            infixes,
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
    pub name: Name,
    pub tpe: Option<Type>,
    pub bindings: Vec<Match>,
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
/// ```zel
/// type MyType = VariantA | VariantB
/// ```
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
    Infix(Infix),
    // type aliases, infixes and ports will end up here
}

/// A representation of the `import` declaration
#[derive(Debug, PartialEq)]
pub struct Import {
    pub name: Name,
    pub alias: Option<Name>,
    pub exposing: Exposing,
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

#[derive(Debug, PartialEq)]
pub struct Infix {
    pub operator: Name,
    pub associativy: Associativity, // TODO Fix typo
    /// Precedence rules the order in which part of the expression are parsed
    /// (in absence of parenthesis). The higher precedence will be parsed first.
    pub precedence: u8,
    pub function_name: Name,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Associativity {
    Left,
    None,
    Right,
}

/// A `FunBinding` is one of the (possibly multiple) function declaration.
///
/// ## Examples
///
///
/// `const = 42` in Zelkova will result in the following AST:
/// ```text
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
/// ```text
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
/// - `Ctor Name [Pattern]`
/// - `CtorQual Name Name [Pattern]`
/// - `List [Pattern]`
/// - `Cons Pattern Pattern`
#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Variable(Name),
    Literal(Literal),
    Tuple(Box<Pattern>, Box<Pattern>, Vec<Pattern>),
    Constructor(Name, Vec<Pattern>),
    Anything,
}

// TODO Inline all Box<Vec into Vec
// which make sense, as vectors are already on the heap
/// An Expression
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Lit(Literal), // Literal, as other are fully named
    Application(Box<Expression>, Box<Expression>),
    Variable(Name), // TODO Qualified variable
    TypeConstructor(Name),
    Tuple(Box<Vec<Expression>>),
    Case(Box<Expression>, Vec<CaseBranch>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseBranch {
    pattern: Pattern,
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
    fn name_unqualified() {
        let n = Name("variable".to_owned());
        assert_eq!(n.is_type(), false);
        assert_eq!(n.is_qualified(), false);

        let n = Name("Type".to_owned());
        assert_eq!(n.is_type(), true);
        assert_eq!(n.is_qualified(), false);
    }

    #[test]
    fn name_qualified() {
        let n = Name("Module.variable".to_owned());
        assert_eq!(n.is_type(), false, "should not be a type");
        assert_eq!(n.is_qualified(), true);

        let n = Name("Module.Type".to_owned());
        assert_eq!(n.is_type(), true);
        assert_eq!(n.is_qualified(), true);
    }
}
