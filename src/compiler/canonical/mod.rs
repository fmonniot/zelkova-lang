//! The canonical representation of a zelkova programs is a translation of a local
//! source into the broader world.
//!
//! This phase is where we integrate the local parsed module into the rest of the
//! program. We do the following steps:
//!
//! - Resolve all imports
//! - Qualify all `Name` (eg. a local value `test` in a `Mod.A` module will be renamed `Mod.A.test`)
//! - Checks that exported names are actually present in the module
//! - Checks there is none cyclic dependency between this module and others (Might be done earlier, let's see)
//!
//! Note that we use `HashMap`'s a lot in this module's structures. This is because later phases will
//! want to have cheap access to the different components of a `Module`.
//!
//! TODO Rename this to core ? I feel it's going to be te main internal representation of the language.
use super::parser;
use super::Interface;
use super::{ModuleName, PackageName};
use crate::utils::collect_accumulate;
use std::collections::HashMap;

mod environment;
use environment::EnvError;

// Some elements which are common to both AST
use crate::compiler::name::{Name, QualName};
pub use environment::Environment;
pub use parser::Associativity;

// begin AST

/// A resolved module
#[derive(Debug)]
pub struct Module {
    pub name: ModuleName,
    pub exports: Exports,
    /// Operator name to infix details
    pub infixes: HashMap<Name, Infix>,
    pub types: HashMap<Name, UnionType>,
    pub values: HashMap<Name, Value>,
}

impl Module {
    pub fn to_interface(&self) -> super::Interface {
        super::Interface {
            values: HashMap::new(), // TODO
            unions: self.types.clone(),
            infixes: self.infixes.clone(),
        }
    }
}

#[derive(Debug)]
pub enum Exports {
    Everything,
    /// Non qualified name to its export type
    Specifics(HashMap<Name, ExportType>),
}

#[derive(Debug)]
pub enum ExportType {
    Value,
    Infix,
    UnionPublic,
    UnionPrivate,
}

#[derive(Debug, Clone)]
pub struct Infix {
    associativity: Associativity,
    precedence: u8,
    function_name: Name,
}

#[derive(Debug, Clone)]
pub struct UnionType {
    variables: Vec<Name>,
    variants: Vec<TypeConstructor>,
}

#[derive(Debug, Clone)]
pub struct TypeConstructor {
    name: Name,
    types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Variable(Name),
    Type(Name, Vec<Type>),
    // Record
    // Unit
    Arrow(Box<Type>, Box<Type>),
    /// Tuple in elm have size 2 or 3, so the third argument is optional.
    /// Should we keep the same restriction in zelkova ?
    Tuple(Box<Type>, Box<Type>, Option<Box<Type>>),
    // Alias
}

impl Type {
    fn from_parser_type(env: &Environment, tpe: &parser::Type) -> Type {
        match tpe {
            parser::Type::Unqualified(name, vars) => {
                match env.find_type(&name) {
                    Some(t) => t.clone(),
                    None => {
                        let types = vars
                            .iter()
                            .map(|t| Type::from_parser_type(env, &t))
                            .collect();

                        // TODO Insert back into Environment ?
                        Type::Type(name.clone(), types)
                    }
                }
            }
            parser::Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(Type::from_parser_type(env, t1)),
                Box::new(Type::from_parser_type(env, t2)),
            ),
            parser::Type::Variable(n) => Type::Variable(n.clone()),
            parser::Type::Tuple(t1, t_many) => {
                let (t2, t3) = match t_many.len() {
                    0 => panic!("Tuple of length 1 is not suported by the parser"),
                    1 | 2 => (t_many.get(0).unwrap().clone(), t_many.get(1).clone()),
                    _ => panic!("For now we restrict tuple to sizes 2 and 3"),
                };

                Type::Tuple(
                    Box::new(Type::from_parser_type(env, t1)),
                    Box::new(Type::from_parser_type(env, &t2)),
                    t3.map(|t| Box::new(Type::from_parser_type(env, t))),
                )
            }
        }
    }

    // TODO Write some tests
    fn to_linear_types(tpe: &Type) -> Vec<Type> {
        match tpe {
            Type::Arrow(a, b) => {
                let mut next = Type::to_linear_types(b);

                next.insert(0, *a.clone());

                next
            }
            _ => {
                vec![tpe.clone()]
            }
        }
    }

}

#[derive(Debug)]
pub enum Value {
    Value {
        name: Name,
        patterns: Vec<Pattern>,
        body: Expression,
    },
    TypedValue {
        name: Name,
        patterns: Vec<(Pattern, Type)>,
        body: Expression,
        tpe: Type,
    },
}

#[derive(Debug)]
pub enum Pattern {
    Anything,
    Variable(Name), // TODO Name or QualName ?
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    /// Tuple in elm have size 2 or 3, so the third argument is optional.
    /// Should we keep the same restriction in zelkova ?
    Tuple(Box<Pattern>, Box<Pattern>, Option<Box<Pattern>>),

    Constructor {
        tpe: QualName, // Type name
        name: Name,    // Constructor name
        union: UnionType,
        args: Vec<PatternConstructorArg>,
    },
}

impl Pattern {
    fn from_parser(p: &parser::Pattern) -> Pattern {
        match p {
            parser::Pattern::Anything => Pattern::Anything,
            parser::Pattern::Variable(name) => Pattern::Variable(name.clone()),
            parser::Pattern::Literal(parser::Literal::Int(i)) => Pattern::Int(*i),
            parser::Pattern::Literal(parser::Literal::Float(f)) => Pattern::Float(*f),
            parser::Pattern::Literal(parser::Literal::Char(c)) => Pattern::Char(*c),
            parser::Pattern::Literal(parser::Literal::Bool(b)) => Pattern::Bool(*b),
            parser::Pattern::Tuple(a, b, c) => Pattern::Tuple(
                Box::new(Pattern::from_parser(a)),
                Box::new(Pattern::from_parser(b)),
                c.get(0).map(Pattern::from_parser).map(Box::new),
            ),
            parser::Pattern::Constructor(name, args) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct PatternConstructorArg {
    tpe: Option<Type>, // Option until we have a type for representing non-infered types

    /// More often than not, this will be a variable. But for nested structure
    /// this can be any supported pattern.
    arg: Box<Pattern>,
}

// TODO Find a way to detect recursive functions (even indirect recursivity,
// eg. `a` calls `b` calls `a`)
/// Expression is an optimized version for checks and caches.
/// 
/// Elm declare those expressions:
/// ```haskell
/// data Expr_
///   = VarLocal Name
///   | VarTopLevel ModuleName.Canonical Name
///   | VarKernel Name Name
///   | VarForeign ModuleName.Canonical Name Annotation
///   | VarCtor CtorOpts ModuleName.Canonical Name Index.ZeroBased Annotation
///   | VarDebug ModuleName.Canonical Name Annotation
///   | VarOperator Name ModuleName.Canonical Name Annotation -- CACHE real name for optimization
///   | Chr ES.String
///   | Str ES.String
///   | Int Int
///   | Float EF.Float
///   | List [Expr]
///   | Negate Expr
///   | Binop Name ModuleName.Canonical Name Annotation Expr Expr -- CACHE real name for optimization
///   | Lambda [Pattern] Expr
///   | Call Expr [Expr]
///   | If [(Expr, Expr)] Expr
///   | Let Def Expr
///   | LetRec [Def] Expr
///   | LetDestruct Pattern Expr Expr
///   | Case Expr [CaseBranch]
///   | Accessor Name
///   | Access Expr (A.Located Name)
///   | Update Name Expr (Map.Map Name FieldUpdate)
///   | Record (Map.Map Name Expr)
///   | Unit
///   | Tuple Expr Expr (Maybe Expr)
/// ```
#[derive(Debug)]
pub enum Expression {
    VarLocal(Name),
    VarTopLevel(QualName),
    VarKernel(QualName),
    VarForeign(QualName, Type),
    VarConstructor(QualName, Type),
    Char(char),
    Int(i64),
    Float(f64),
    Bool(bool),
    // List
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    // Let
    // LetRec
    // LetDestruct (eg. `(a,b) = someTuple`)
    Case(Box<Expression>, Vec<CaseBranch>),
    // Accessor
    // Access
    // Update (record)
    // Unit
    Tuple(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
}

impl Expression {

    fn from_parser(e: &parser::Expression) -> Expression {

        todo!()
    }
}

#[derive(Debug)]
pub struct CaseBranch {
    pattern: Pattern,
    expression: Expression,
}

// end AST

#[derive(Debug)]
pub enum Error {
    ExportNotFound(Name, ExportType),
    EnvironmentErrors(Vec<EnvError>),
    InfixReferenceInvalidValue(Name, Name), // (infix, function)
    BindingPatternsInvalidLen,
    NoBindings,
}

impl From<Vec<EnvError>> for Error {
    fn from(errors: Vec<EnvError>) -> Self {
        Error::EnvironmentErrors(errors)
    }
}

/// Transform a given `parser::Module` into a `canonical::Module`
pub fn canonicalize(
    package: &PackageName,
    interfaces: &HashMap<Name, Interface>,
    source: &parser::Module,
) -> Result<Module, Vec<Error>> {
    let mut errors: Vec<Error> = vec![];
    let mut env = Environment::new(&interfaces, &source.imports).map_err(|e| vec![e.into()])?;

    let name = ModuleName {
        package: package.clone(),
        name: source.name.clone(),
    };

    // Because we are rewriting infixes in this phase, we must do this check before
    // resolving values.
    let infixes = do_infixes(&source.infixes, &mut env, &source.functions).unwrap_or_else(|err| {
        errors.extend(err);
        HashMap::new()
    });

    let types = do_types(&env, &source.types).unwrap_or_else(|err| {
        errors.extend(err);
        HashMap::new()
    });

    // TODO Should I manage infixes rewrite here too ?
    // Yes I should do it here
    let values = do_values(&env, &source.functions).unwrap_or_else(|err| {
        errors.extend(err);
        HashMap::new()
    });

    // We do exports at the end, and verify that all exported value do
    // have a reference within the current module
    let exports = do_exports(&source.exposing, &env).unwrap_or_else(|err| {
        errors.extend(err);
        Exports::Everything // Never exposed, as we will return the errors instead
    });

    if errors.is_empty() {
        Ok(Module {
            name,
            exports,
            infixes,
            types,
            values,
        })
    } else {
        Err(errors)
    }
}

fn do_values(env: &Environment, functions: &Vec<parser::Function>) -> Result<HashMap<Name, Value>, Vec<Error>> {
    let iter = functions.iter().map(|function| {

        // Bindings to expression

        // TODO Better error message with position of mismatch
        // TODO Error when bindings is empty
        let bindings_size = function.bindings.iter()
            .all(|v| v.patterns.len() == function.bindings[0].patterns.len());

        if !bindings_size {
            Err(Error::BindingPatternsInvalidLen)?
        }

        let (patterns, body): (Vec<Pattern>, Expression) = match function.bindings.len() {
            0 => Err(Error::NoBindings),
            1 => {
                // if one binding, we can convert directly to cano format
                let binding = &function.bindings[0];

                let patterns = binding.patterns.iter().map(|p| Pattern::from_parser(p)).collect();
                let body = Expression::from_parser(&binding.body);


                Ok((patterns, body))
            }
            _ => {
                // if multiple bindings, we need to create synthetics variables and put all bindings into a case expression

                todo!("multiple bindings not implemented")
            }
        }?;

        let name = function.name.clone();
        
        match &function.tpe {
            Some(t) => {
                let tpe = Type::from_parser_type(env, &t);
                let linear = Type::to_linear_types(&tpe);

                if linear.len() != patterns.len() {
                    // TODO Better error message
                    Err(Error::BindingPatternsInvalidLen)?
                }

                let patterns = patterns.into_iter().zip(linear).collect();
                
                Ok((function.name.clone(), Value::TypedValue { name, patterns, body, tpe }))
            },
            None => Ok((function.name.clone(), Value::Value { name, patterns, body }))
        }
    });

    collect_accumulate(iter).map(|vec| vec.into_iter().collect())
}

fn do_types(
    env: &Environment,
    types: &Vec<parser::UnionType>,
) -> Result<HashMap<Name, UnionType>, Vec<Error>> {
    let iter = types.iter().map(|tpe| {
        let name = tpe.name.clone();
        let variables = tpe.type_arguments.clone();

        println!("do_types(in:{:?})", tpe);

        // variants are represented as parser::Type::Unqualified. Other types
        // can be safely ignored in this context.

        let variants = tpe
            .variants
            .iter()
            .filter_map(|t| {
                match t {
                    parser::Type::Unqualified(name, vars) => {
                        // TODO It might actually make more sense to put Type::from_parser_type
                        // on `Environment`.
                        Some(TypeConstructor {
                            name: name.clone(), // TODO Qualify with current module name
                            types: vars
                                .iter()
                                .map(|t| Type::from_parser_type(&env, t))
                                .collect(),
                        })
                    }
                    _ => None,
                }
            })
            .collect();

        Ok((
            name,
            UnionType {
                variables,
                variants,
            },
        ))
    });

    collect_accumulate(iter).map(|vec| vec.into_iter().collect())
}

fn do_infixes(
    infixes: &Vec<parser::Infix>,
    env: &mut Environment,
    functions: &Vec<parser::Function>,
) -> Result<HashMap<Name, Infix>, Vec<Error>> {
    let iter = infixes.iter().map(|infix| {
        let op_name = infix.operator.clone();
        let function_name = infix.function_name.clone();

        let function_exist = functions
            .iter()
            .find(|f| f.name == infix.function_name)
            .is_some();

        if function_exist {
            let infix = Infix {
                associativity: infix.associativy,
                precedence: infix.precedence,
                function_name,
            };

            env.insert_local_infix(op_name.clone(), infix.clone());

            Ok((op_name, infix))
        } else {
            Err(Error::InfixReferenceInvalidValue(op_name, function_name))
        }
    });

    collect_accumulate(iter).map(|vec| vec.into_iter().collect())
}

// TODO Add existence checks for values and types
fn do_exports(
    source_exposing: &parser::Exposing,
    env: &Environment,
) -> Result<Exports, Vec<Error>> {
    match source_exposing {
        parser::Exposing::Open => Ok(Exports::Everything),
        parser::Exposing::Explicit(exposed) => {
            let specifics = exposed.into_iter().map(|exposed| match exposed {
                parser::Exposed::Lower(name) => Ok((name.clone(), ExportType::Value)),
                parser::Exposed::Upper(name, parser::Privacy::Public) => {
                    Ok((name.clone(), ExportType::UnionPublic))
                }
                parser::Exposed::Upper(name, parser::Privacy::Private) => {
                    Ok((name.clone(), ExportType::UnionPrivate))
                }
                parser::Exposed::Operator(name) => {
                    if env.local_infix_exists(&name) {
                        Ok((name.clone(), ExportType::Infix))
                    } else {
                        Err(Error::ExportNotFound(name.clone(), ExportType::Infix))
                    }
                }
            });

            let specifics = collect_accumulate(specifics)?;

            Ok(Exports::Specifics(specifics.into_iter().collect()))
        }
    }
}
