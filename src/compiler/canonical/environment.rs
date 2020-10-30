use super::parser;
use super::{Infix, Interface, Name, TypeConstructor, UnionType};
use crate::utils::collect_accumulate;
use std::collections::HashMap;

#[derive(Default)]
pub struct Environment {
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

impl Environment {
    pub fn new(
        interfaces: &HashMap<Name, Interface>,
        imports: &Vec<parser::Import>,
    ) -> Result<Environment, Vec<EnvError>> {
        let mut env = Environment::default();
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

    pub fn process_import(
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
                    import_union_type(self, name, union_name, union);
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

                            import_union_type(self, name, type_name, union);
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
    pub fn insert_local_infix(&mut self, name: Name, infix: Infix) {
        self.infixes.insert(name, infix);
    }

    pub fn local_infix_exists(&self, name: &Name) -> bool {
        self.infixes.contains_key(&name)
    }
}

fn import_union_type(env: &mut Environment,
                     module_name: &Name,
                     union_name: &Name,
                     union: &UnionType) {
    env.types.insert(union_name.qualify_with_name(module_name), ());

    for variant in &union.variants {
        env.constructors
            .insert(variant.name.qualify_with_name(module_name), variant.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::canonical::*;

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
        Environment::new(&interfaces, vec![]).map(drop)
    }

    #[test]
    fn new_import_all_type_ctor() -> Result<(), Vec<EnvError>> {
        let imports = vec![import("Maybe".into(), None, exposing_all())];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = Environment::new(&interfaces, imports)?;

        assert_eq!(env.infixes.len(), 0);
        assert_eq!(env.types.len(), 1);
        assert_eq!(env.constructors.len(), 2);
        assert_eq!(env.variables.len(), 0);
        assert_eq!(env.aliases.len(), 0);

        Ok(())
    }

    #[test]
    fn new_import_values() -> Result<(), Vec<EnvError>> {
        let imports = vec![import(
            "Maybe".into(),
            None,
            exposing_explicit(vec![
                parser::Exposed::Upper("Maybe".into(), parser::Privacy::Private),
                parser::Exposed::Lower("andThen".into())
            ]),
        )];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = Environment::new(&interfaces, imports)?;

        assert_eq!(env.infixes.len(), 0);
        assert_eq!(env.types.len(), 1);
        assert_eq!(env.constructors.len(), 0);
        assert_eq!(env.variables.len(), 1);
        assert_eq!(env.aliases.len(), 0);

        Ok(())
    }
}
