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
pub use environment::Environment;
pub use parser::{Associativity, Name};

// begin AST

/// A resolved module
#[derive(Debug)]
pub struct Module {
    pub name: ModuleName,
    pub exports: Exports,
    pub infixes: HashMap<Name, Infix>,
    pub types: HashMap<Name, UnionType>,
    pub values: HashMap<Name, Value>,
}

#[derive(Debug)]
pub enum Exports {
    Everything,
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
    Type(ModuleName, Name, Vec<Type>),
    // Record
    // Unit
    Arrow(Box<Type>, Box<Type>),
    /// Tuple in elm have size 2 or 3, so the third argument is optional.
    /// Should we keep the same restriction in zelkova ?
    Tuple(Box<Type>, Box<Type>, Option<Box<Type>>),
    // Alias
}

#[derive(Debug)]
pub struct Value; // TODO

// end AST

#[derive(Debug)]
pub enum Error {
    ExportNotFound(Name, ExportType),
    EnvironmentErrors(Vec<EnvError>),
    InfixReferenceInvalidValue(Name, Name), // (infix, function)
}

impl From<Vec<EnvError>> for Error {
    fn from(errors: Vec<EnvError>) -> Self {
        Error::EnvironmentErrors(errors)
    }
}

/// Transform a given `parser::Module` into a `canonical::Module`
pub fn canonicalize(
    package: PackageName,
    interfaces: HashMap<Name, Interface>,
    source: parser::Module,
) -> Result<Module, Vec<Error>> {
    let mut errors: Vec<Error> = vec![];
    let mut env = Environment::new(&interfaces, source.imports).map_err(|e| vec![e.into()])?;

    let name = ModuleName {
        package,
        name: source.name.clone(),
    };

    // Because we are rewriting infixes in this phase, we must do this check before
    // resolving values.
    let infixes = do_infixes(&source.infixes, &mut env, &source.functions).unwrap_or_else(|err| {
        errors.extend(err);
        HashMap::new()
    });

    // TODO Should I manage infixes rewrite here too ?
    // Yes I should do it here
    let values = do_values().unwrap_or_else(|err| {
        errors.extend(err);
        HashMap::new()
    });

    let types = do_types().unwrap_or_else(|err| {
        errors.extend(err);
        HashMap::new()
    });

    // We do exports at the end, and verify that all exported value do
    // have a reference within the current module
    let exports = do_exports(source.exposing, &env).unwrap_or_else(|err| {
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

fn do_values() -> Result<HashMap<Name, Value>, Vec<Error>> {
    Ok(HashMap::new())
}

fn do_types() -> Result<HashMap<Name, UnionType>, Vec<Error>> {
    Ok(HashMap::new())
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
fn do_exports(source_exposing: parser::Exposing, env: &Environment) -> Result<Exports, Vec<Error>> {
    match source_exposing {
        parser::Exposing::Open => Ok(Exports::Everything),
        parser::Exposing::Explicit(exposed) => {
            let specifics = exposed.into_iter().map(|exposed| match exposed {
                parser::Exposed::Lower(name) => Ok((name, ExportType::Value)),
                parser::Exposed::Upper(name, parser::Privacy::Public) => {
                    Ok((name, ExportType::UnionPublic))
                }
                parser::Exposed::Upper(name, parser::Privacy::Private) => {
                    Ok((name, ExportType::UnionPrivate))
                }
                parser::Exposed::Operator(name) => {
                    if env.local_infix_exists(&name) {
                        Ok((name, ExportType::Infix))
                    } else {
                        Err(Error::ExportNotFound(name, ExportType::Infix))
                    }
                }
            });

            let specifics = collect_accumulate(specifics)?;

            Ok(Exports::Specifics(specifics.into_iter().collect()))
        }
    }
}
