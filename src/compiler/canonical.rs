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
use std::collections::HashMap;

// Some elements which are common to both AST
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

#[derive(Debug)]
pub struct UnionType;// TODO

#[derive(Debug)]
pub struct Value;// TODO


// end AST

#[derive(Debug)]
pub enum Error {
    ExportNotFound(Name, ExportType)
}

/// Transform a given `parser::Module` into a `canonical::Module`
pub fn canonicalize(
    package: PackageName,
    interfaces: HashMap<ModuleName, Interface>,
    source: parser::Module,
) -> Result<Module, Vec<Error>> {
    let mut errors: Vec<Error> = vec![];
    let mut env = NamedEnvironment::from_interfaces(&interfaces);

    let name = ModuleName {
        package,
        name: source.name.clone(),
    };

    let infixes = do_infixes(&source.infixes, &mut env);

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

fn do_infixes(infixes: &Vec<parser::Infix>, env: &mut NamedEnvironment) -> HashMap<Name, Infix> {
    infixes
        .iter()
        .map(|infix| {
            // resolve function name
            let op_name = infix.operator.clone();
            let infix = Infix {
                associativity: infix.associativy,
                precedence: infix.precedence,
                function_name: infix.function_name.clone(),
            };

            env.insert_local_infix(op_name.clone(), infix.clone());

            (op_name, infix)
        })
        .collect()
}

// TODO Add existence checks for values and types
fn do_exports(
    source_exposing: parser::Exposing,
    env: &NamedEnvironment,
) -> Result<Exports, Vec<Error>> {
    match source_exposing {
        parser::Exposing::Open => Ok(Exports::Everything),
        parser::Exposing::Explicit(exposed) => {
            let specifics = exposed
                .into_iter()
                .map(|exposed| match exposed {
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
                    },
                });

            let specifics = collect_accumulate(specifics)?;

            Ok(Exports::Specifics(specifics.into_iter().collect()))
        }
    }
}


#[derive(Default)]
struct NamedEnvironment {
    infixes: HashMap<Name, Infix>,
    values: HashMap<Name, Value>
}

impl NamedEnvironment {
    fn from_interfaces(interfaces: &HashMap<ModuleName, Interface>) -> NamedEnvironment {
        let mut env = NamedEnvironment::default();

        for (_module_name, interface) in interfaces {
            for (n, infix) in &interface.infixes {
                env.insert_local_infix(n.clone(), Infix {
                    associativity: infix.associativy,
                    precedence: infix.precedence,
                    function_name: infix.function_name.clone(),
                });
            }
        }

        todo!()
    }

    // TODO Do we need a local/foreign distinction for infixes ? (or in general ?)
    fn insert_local_infix(&mut self, name: Name, infix: Infix) {
        self.infixes.insert(name, infix);
    }

    fn local_infix_exists(&self, name: &Name) -> bool {
        self.infixes.contains_key(&name)
    }
}

/// This let use collect an iterator of result into a result of vectors.
/// 
/// The generic signature, assuming `I: FromIterator` would look like `Iterator<Result<T, E>> -> Result<I<T>, I<E>>`.
/// I wish this was part of the standard library, but as it is not here is my custom version. And because
/// I don't particulary need to work with something else than `Vec` at the moment, the output type is fixed :)
fn collect_accumulate<T, E, I>(iterator: I) -> Result<Vec<T>, Vec<E>> where I: Iterator<Item=Result<T, E>> {
    let mut items = vec![];
    let mut errors = vec![];

    for i in iterator {
        match i {
            Ok(t) => items.push(t),
            Err(e) => errors.push(e),
        };
    }

    if errors.is_empty() {
        Ok(items)
    } else {
        Err(errors)
    }
}