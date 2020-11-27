//! env module

use super::parser;
use super::{Infix, Interface, ModuleName, Name, QualName, Type, TypeConstructor, UnionType};
use crate::utils::collect_accumulate;
use std::collections::HashMap;

#[derive(Debug)]
pub enum ValueType {
    Local,
    TopLevel,
    Foreign(ModuleName, Type),
    Foreigns(Vec<ModuleName>),
}

/// Environment represent the set of values/types available to a compilation unit.
/// It is derived from previous successful units and is provisioned through a
/// module imports.
///
/// As a whole, the aim of the canonical AST is to not have to worry about the
/// Environment in later phases. We still need one to translate the parser AST.
/// The good news being, it can be local to the canonicalization function.
///
/// TODO It should expose two APIs: one for qualified names and one for unqualified.
pub trait Environment<'a> {
    fn find_type(&self, name: &Name) -> Option<&Type>;

    fn module_name(&self) -> &ModuleName;

    // TODO Here we have the issue that name might be qualified or not, and we need
    // to be able to find it in both cases. So double indices ?
    fn find_value(&self, name: &Name) -> Option<&ValueType>;

    // TODO Same issue qual/non-qual as above
    fn find_type_constructor(&self, name: &Name) -> Option<&TypeConstructor>;

    fn local_infix_exists(&self, name: &Name) -> bool;

    fn new_scope(&'a self) -> Box<dyn Environment<'a> + 'a>;
}

pub fn new_environment(
    module_name: &ModuleName,
    interfaces: &HashMap<Name, Interface>,
    imports: &Vec<parser::Import>,
) -> Result<RootEnvironment, Vec<EnvError>> {
    RootEnvironment::new(module_name, interfaces, imports)
}

#[derive(Debug)]
pub enum EnvError {
    InterfaceNotFound(Name),
    UnionNotFound(Name),
    InfixNotFound(Name),
    ValueNotFound(Name),
    Multiple(Vec<EnvError>),
}

/// RootEnvironment represents the top level module and contains information accessible
/// from the entire module.
///
/// This is opposed to a ScopedEnvironment which contains
/// additional information available only to a scoped expression (eg. local variable)
///
/// TODO Only use QualName in the global Environment (investigate that claim from past me,
/// don't think it holds as top-level values can be referenced without their qualifier)
pub struct RootEnvironment {
    module_name: ModuleName,
    infixes: HashMap<Name, Infix>,
    types: HashMap<Name, Type>,
    constructors: HashMap<Name, TypeConstructor>, // TODO Might have multiple in scope
    variables: HashMap<Name, ValueType>,          // Store real Type for type check
    aliases: HashMap<Name, Name>,
}

impl RootEnvironment {
    pub fn new(
        module_name: &ModuleName,
        interfaces: &HashMap<Name, Interface>,
        imports: &Vec<parser::Import>,
    ) -> Result<RootEnvironment, Vec<EnvError>> {
        let mut env = RootEnvironment {
            module_name: module_name.clone(),
            infixes: HashMap::new(),
            types: HashMap::new(),
            constructors: HashMap::new(),
            variables: HashMap::new(),
            aliases: HashMap::new(),
        };
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

    // TODO Can it be a straight function instead of a method ?
    pub fn process_import(
        &mut self,
        interfaces: &HashMap<Name, Interface>,
        imported_module_name: &Name,
        alias: &Option<Name>,
        exposing: &parser::Exposing,
    ) -> Result<(), EnvError> {
        let interface = interfaces
            .get(&imported_module_name)
            .ok_or_else(|| EnvError::InterfaceNotFound(imported_module_name.clone()))?;

        match exposing {
            parser::Exposing::Open => {
                // We add everything to the current environment
                for (union_name, union) in &interface.unions {
                    import_union_type(self, imported_module_name, union_name, union);
                }

                for (value_name, tpe) in &interface.values {
                    self.insert_foreign_value(
                        value_name.qualify_with_name(imported_module_name).unwrap(),
                        tpe.clone(),
                        &interface.module_name,
                    );
                }

                for (op_name, infix) in &interface.infixes {
                    self.infixes.insert(op_name.clone(), infix.clone());
                }
            }
            parser::Exposing::Explicit(exposeds) => {
                let iter = exposeds.iter().map(|exposed| {
                    match exposed {
                        parser::Exposed::Lower(variable_name) => {
                            let tpe = interface
                                .values
                                .get(&variable_name)
                                .ok_or_else(|| EnvError::ValueNotFound(variable_name.clone()))?;

                            self.insert_foreign_value(
                                variable_name
                                    .qualify_with_name(imported_module_name)
                                    .expect(""),
                                tpe.clone(),
                                &interface.module_name,
                            );
                        }
                        parser::Exposed::Upper(type_name, parser::Privacy::Private) => {
                            let tpe = Type::Type(type_name.clone(), vec![]);

                            self.types.insert(
                                type_name
                                    .qualify_with_name(imported_module_name)
                                    .unwrap()
                                    .to_name(),
                                tpe,
                            );
                        }
                        parser::Exposed::Upper(type_name, parser::Privacy::Public) => {
                            let union = interface
                                .unions
                                .get(&type_name)
                                .ok_or_else(|| EnvError::UnionNotFound(type_name.clone()))?;

                            import_union_type(self, imported_module_name, type_name, union);
                        }
                        parser::Exposed::Operator(variable_name) => {
                            let infix = interface
                                .infixes
                                .get(&variable_name)
                                .ok_or_else(|| EnvError::InfixNotFound(variable_name.clone()))?;

                            self.infixes.insert(variable_name.clone(), infix.clone());
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
            self.aliases
                .insert(alias.clone(), imported_module_name.clone());
        }

        Ok(())
    }

    // TODO Same as above. It's part of the init, so straight function should work + reduced visibility
    pub fn insert_foreign_value(&mut self, name: QualName, tpe: Type, module_name: &ModuleName) {
        let name = name.to_name(); // Until we use QualName as key in self.variables
        let vt = ValueType::Foreign(module_name.clone(), tpe.clone());

        // Can it be done more efficiently by using get_mut ?
        match self.variables.remove(&name) {
            Some(ValueType::Foreign(module, _)) => {
                self.variables
                    .insert(name, ValueType::Foreigns(vec![module_name.clone(), module]));
            }
            Some(ValueType::Foreigns(mut vec)) => {
                vec.push(module_name.clone());
                self.variables.insert(name, ValueType::Foreigns(vec));
            }
            None => {
                self.variables.insert(name, vt);
            }
            _ => todo!("find out what to do in those cases"),
        }
    }

    // TODO Do we need a local/foreign distinction for infixes ? (or in general ?)
    pub fn insert_local_infix(&mut self, name: Name, infix: Infix) {
        self.infixes.insert(name, infix);
    }
}

impl<'a> Environment<'a> for RootEnvironment {
    fn find_type(&self, name: &Name) -> Option<&Type> {
        self.types.get(name)
    }

    fn module_name(&self) -> &ModuleName {
        &self.module_name
    }

    // TODO Here we have the issue that name might be qualified or not, and we need
    // to be able to find it in both cases. So double indices ?
    fn find_value(&self, name: &Name) -> Option<&ValueType> {
        self.variables.get(name)
    }

    // TODO Same issue qual/non-qual as above
    fn find_type_constructor(&self, name: &Name) -> Option<&TypeConstructor> {
        self.constructors.get(name)
    }

    fn local_infix_exists(&self, name: &Name) -> bool {
        self.infixes.contains_key(&name)
    }

    fn new_scope(&'a self) -> Box<dyn Environment<'a> + 'a> {
        Box::new(ScopedEnvironment { parent: self })
    }
}

fn import_union_type(
    env: &mut RootEnvironment,
    module_name: &Name,
    union_name: &Name,
    union: &UnionType,
) {
    let tpe = Type::Type(union_name.clone(), vec![]);
    env.types.insert(
        union_name.qualify_with_name(module_name).unwrap().to_name(),
        tpe,
    );

    for variant in &union.variants {
        env.constructors
            .insert(variant.name.to_name(), variant.clone());
    }
}

/// An Environment scoped to a module's sub expression (`let`, function, etc…)
struct ScopedEnvironment<'a> {
    parent: &'a dyn Environment<'a>,
}

impl<'a> Environment<'a> for ScopedEnvironment<'a> {
    fn find_type(&self, name: &Name) -> Option<&Type> {
        self.parent.find_type(name)
    }

    fn module_name(&self) -> &ModuleName {
        &self.parent.module_name()
    }

    // TODO Here we have the issue that name might be qualified or not, and we need
    // to be able to find it in both cases. So double indices ?
    fn find_value(&self, name: &Name) -> Option<&ValueType> {
        self.parent.find_value(name)
    }

    // TODO Same issue qual/non-qual as above
    fn find_type_constructor(&self, name: &Name) -> Option<&TypeConstructor> {
        self.parent.find_type_constructor(name)
    }

    fn local_infix_exists(&self, name: &Name) -> bool {
        self.parent.local_infix_exists(&name)
    }

    fn new_scope(&'a self) -> Box<dyn Environment<'a> + 'a> {
        Box::new(ScopedEnvironment { parent: self })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::canonical::*;

    fn module_name() -> ModuleName {
        ModuleName::new(PackageName::new("author", "project"), "module".into())
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
        let type_var = |name: &str| Type::Variable(name.into());

        let type_hk = |name: &str, params| Type::Type(name.into(), params);

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
                        name: "Maybe.Just".into(),
                        types: vec![Type::Variable("a".into())],
                    },
                    TypeConstructor {
                        name: "Maybe.Nothing".into(),
                        types: vec![],
                    },
                ],
            },
        );

        let interface = Interface {
            module_name: ModuleName::new(PackageName::new("zelkova", "core"), "Maybe".into()),
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
        RootEnvironment::new(&module_name(), &interfaces, &vec![]).map(drop)
    }

    #[test]
    fn new_import_all_type_ctor() -> Result<(), Vec<EnvError>> {
        let imports = vec![import("Maybe".into(), None, exposing_all())];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = RootEnvironment::new(&module_name(), &interfaces, &imports)?;

        assert_eq!(env.infixes.len(), 0, "infixes");
        assert_eq!(env.types.len(), 1, "types");
        assert_eq!(env.constructors.len(), 2, "constructors");
        assert_eq!(env.variables.len(), 3, "variables={:?}", env.variables);
        assert_eq!(env.aliases.len(), 0, "aliases");

        Ok(())
    }

    #[test]
    fn new_import_values() -> Result<(), Vec<EnvError>> {
        let imports = vec![import(
            "Maybe".into(),
            None,
            exposing_explicit(vec![
                parser::Exposed::Upper("Maybe".into(), parser::Privacy::Private),
                parser::Exposed::Lower("andThen".into()),
            ]),
        )];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = RootEnvironment::new(&module_name(), &interfaces, &imports)?;

        assert_eq!(env.infixes.len(), 0);
        assert_eq!(env.types.len(), 1);
        assert_eq!(env.constructors.len(), 0);
        assert_eq!(env.variables.len(), 1);
        assert_eq!(env.aliases.len(), 0);

        Ok(())
    }
}
