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
    let mut env = NamedEnvironment::new(&interfaces, source.imports).map_err(|e| vec![e.into()])?;

    let name = ModuleName {
        package,
        name: source.name.clone(),
    };

    let infixes = do_infixes(&source.infixes, &mut env);

    // TODO Should I manage infixes rewrite here too ?
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

#[derive(Default)]
struct NamedEnvironment {
    infixes: HashMap<Name, Infix>,
    types: HashMap<Name, ()>,
    constructors: HashMap<Name, TypeConstructor>,
    variables: HashMap<Name, ()>,
    aliases: HashMap<Name, Name>,
}

#[derive(Debug)]
pub enum EnvError {
    InterfaceNotFound(Name),
    UnionNotFound(Name),
    InfixNotFound(Name),
    Multiple(Vec<EnvError>),
}

impl NamedEnvironment {
    fn new(
        interfaces: &HashMap<Name, Interface>,
        imports: Vec<parser::Import>,
    ) -> Result<NamedEnvironment, Vec<EnvError>> {
        let mut env = NamedEnvironment::default();
        let mut errors = vec![];

        for parser::Import {
            name,
            alias,
            exposing,
        } in imports
        {
            match env.process_import(interfaces, &name, &alias, &exposing) {
                Ok(_) => (),
                Err(err) => {
                    errors.push(err);
                }
            }
        }

        if errors.is_empty() {
            Ok(env)
        } else {
            Err(errors)
        }
    }

    fn process_import(
        &mut self,
        interfaces: &HashMap<Name, Interface>,
        name: &Name,
        alias: &Option<Name>,
        exposing: &parser::Exposing,
    ) -> Result<(), EnvError> {
        let interface = interfaces
            .get(&name)
            .ok_or_else(|| EnvError::InterfaceNotFound(name.clone()))?;

        match exposing {
            parser::Exposing::Open => {
                // We add everything to the current environment
                for (union_name, union) in &interface.unions {
                    self.types.insert(union_name.qualify_with_name(name), ());

                    for variant in &union.variants {
                        self.constructors.insert(variant.name.qualify_with_name(name), variant.clone());
                    }
                }
            }
            parser::Exposing::Explicit(exposeds) => {
                let iter = exposeds.iter().map(|exposed| {
                    match exposed {
                        parser::Exposed::Lower(variable_name) => {
                            self.variables
                                .insert(variable_name.qualify_with_name(name), ());
                        }
                        parser::Exposed::Upper(type_name, parser::Privacy::Private) => {
                            self.types.insert(type_name.qualify_with_name(name), ());
                        }
                        parser::Exposed::Upper(type_name, parser::Privacy::Public) => {
                            let union = interface
                                .unions
                                .get(&type_name)
                                .ok_or_else(|| EnvError::UnionNotFound(type_name.clone()))?;

                            for variant in &union.variants {
                                self.constructors.insert(variant.name.qualify_with_name(name), variant.clone());
                            }
                        }
                        parser::Exposed::Operator(variable_name) => {
                            let infix = interface
                                .infixes
                                .get(&variable_name)
                                .ok_or_else(|| EnvError::InfixNotFound(variable_name.clone()))?;

                            self.infixes.insert(
                                variable_name.clone(),
                                Infix {
                                    associativity: infix.associativy,
                                    precedence: infix.precedence,
                                    function_name: infix.function_name.clone(),
                                },
                            );
                            // How do we represent infixes ?
                            // When do we do rewrite them ?
                        }
                    };

                    Ok(())
                });

                collect_accumulate(iter).map_err(EnvError::Multiple)?;
            }
        };

        if let Some(alias) = alias {
            self.aliases.insert(alias.clone(), name.clone());
        }

        Ok(())
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
fn collect_accumulate<T, E, I>(iterator: I) -> Result<Vec<T>, Vec<E>>
where
    I: Iterator<Item = Result<T, E>>,
{
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

#[cfg(test)]
mod tests {
    use super::*;

    impl From<&str> for Name {
        fn from(n: &str) -> Self {
            Name(n.to_string())
        }
    }

    fn import(name: Name, alias: Option<Name>, exposing: parser::Exposing) -> parser::Import {
        parser::Import {
            name,
            alias,
            exposing,
        }
    }

    fn exposing_all() -> parser::Exposing {
        parser::Exposing::Open
    }

    fn exposing_explicit(exposeds: Vec<parser::Exposed>) -> parser::Exposing {
        parser::Exposing::Explicit(exposeds)
    }

    fn maybe_interface() -> (Name, Interface) {
        let package = PackageName::new("zelkova", "core");
        let module = ModuleName::new(package.clone(), "Maybe".into());

        let type_var = |name: &str| Type::Variable(name.into());

        let type_hk = |name: &str, params| Type::Type(module.clone(), name.into(), params);

        let type_fun = |t1, t2| Type::Arrow(Box::new(t1), Box::new(t2));

        let mut values = HashMap::new();
        // andThen : (a -> Maybe b) -> Maybe a -> Maybe b
        values.insert(
            "andThen".into(),
            type_fun(
                type_fun(type_var("a"), type_hk("Maybe", vec![type_var("b")])),
                type_fun(
                    type_hk("Maybe", vec![type_var("a")]),
                    type_hk("Maybe", vec![type_var("b")]),
                ),
            ),
        );
        // map : (a -> b) -> Maybe a -> Maybe b
        values.insert(
            "map".into(),
            type_fun(
                type_fun(type_var("a"), type_var("b")),
                type_fun(
                    type_hk("Maybe", vec![type_var("a")]),
                    type_hk("Maybe", vec![type_var("b")]),
                ),
            ),
        );
        // withDefault : a -> Maybe a -> a
        values.insert(
            "withDefault".into(),
            type_fun(
                type_var("a"),
                type_fun(type_hk("Maybe", vec![type_var("a")]), type_var("a")),
            ),
        );

        let mut unions = HashMap::new();
        unions.insert(
            "Maybe".into(),
            UnionType {
                variables: vec![],
                variants: vec![
                    TypeConstructor {
                        name: "Just".into(),
                        types: vec![Type::Variable("a".into())],
                    },
                    TypeConstructor {
                        name: "Nothing".into(),
                        types: vec![],
                    },
                ],
            },
        );

        let interface = Interface {
            package,
            values,
            unions,
            infixes: HashMap::new(),
        };

        ("Maybe".into(), interface)
    }

    #[test]
    fn new_no_imports() -> Result<(), Vec<EnvError>> {
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        NamedEnvironment::new(&interfaces, vec![]).map(drop)
    }

    #[test]
    fn new_import_all_type_ctor() -> Result<(), Vec<EnvError>> {
        let imports = vec![import("Maybe".into(), None, exposing_all())];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = NamedEnvironment::new(&interfaces, imports)?;

        assert_eq!(env.infixes.len(), 0);
        assert_eq!(env.types.len(), 1);
        assert_eq!(env.constructors.len(), 2);
        assert_eq!(env.variables.len(), 0);
        assert_eq!(env.aliases.len(), 0);

        Ok(())
    }

    #[test]
    fn new_import_values() -> Result<(), Vec<EnvError>> {
        let imports = vec![import("Maybe".into(), None, exposing_explicit(vec![
            parser::Exposed::Lower("andThen".into())
        ]))];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = NamedEnvironment::new(&interfaces, imports)?;

        assert_eq!(env.infixes.len(), 0);
        assert_eq!(env.types.len(), 0);
        assert_eq!(env.constructors.len(), 0);
        assert_eq!(env.variables.len(), 1);
        assert_eq!(env.aliases.len(), 0);

        Ok(())
    }
}
