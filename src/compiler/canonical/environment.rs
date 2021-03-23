//! env module

use super::{parser, Pattern};
use super::{Infix, Interface, ModuleName, Name, Type, TypeConstructor, UnionType};
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
pub trait Environment<'parent>: std::fmt::Debug {
    fn find_type(&self, name: &Name) -> Option<&Type>;

    fn module_name(&self) -> &ModuleName;

    fn find_value(&self, name: &Name) -> Option<&ValueType>;

    fn find_type_constructor(&self, name: &Name) -> Option<&TypeConstructor>;

    fn local_infix_exists(&self, name: &Name) -> bool;

    fn insert_local_value(&mut self, name: &Name);

    // 'parent must lives at least as long as 'a
    fn new_scope<'a>(&'a self) -> ScopedEnvironment<'parent, 'a>
    where
        'parent: 'a;
}

pub fn new_environment(
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
        match process_import(&mut env, interfaces, &name, &alias, &exposing) {
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
    env: &mut RootEnvironment,
    interfaces: &HashMap<Name, Interface>,
    imported_module_name: &Name,
    alias: &Option<Name>,
    exposing: &parser::Exposing,
) -> Result<(), EnvError> {
    let interface = interfaces
        .get(&imported_module_name)
        .ok_or_else(|| EnvError::InterfaceNotFound(imported_module_name.clone()))?;

    println!(
        "process_import(imported_module_name={:?}, alias={:?}, exposing: {:?})",
        imported_module_name, alias, exposing
    );

    // First we insert all values/types from the module, prefixed with the module name or its alias
    let prefix = alias.as_ref().unwrap_or(imported_module_name);

    for (value_name, tpe) in &interface.values {
        insert_foreign_value(
            env,
            value_name.qualify_with_name(prefix).unwrap().to_name(),
            tpe.clone(),
            &interface.module_name,
        );
    }

    for (union_name, union) in &interface.unions {
        /*
        let variants = if let Some(a) = alias {
            union.variants.iter().map(|tctor| tcor.)
        }
        */
        // TODO Manage aliases. It means replacing the qualified part of the type constructors
        println!("import_union = {:?} -> {:?}", union_name, union);
        insert_foreign_union_type(env, union_name, union.variants.iter());
    }

    // TODO Infix ?

    // Then we insert unqualified values/types
    match exposing {
        parser::Exposing::Open => {
            // We add everything to the current environment
            for (union_name, union) in &interface.unions {
                let tpe = Type::Type(union_name.clone(), vec![]);
                env.types.insert(
                    interface.module_name.qualify_name(union_name).to_name(),
                    tpe,
                );

                for variant in &union.variants {
                    // TODO Does that work ? What does variant.name really contains ?
                    env.constructors
                        .insert(variant.name.to_name(), variant.clone());
                }
            }

            for (value_name, tpe) in &interface.values {
                insert_foreign_value(env, value_name.clone(), tpe.clone(), &interface.module_name);
            }

            for (op_name, infix) in &interface.infixes {
                env.infixes.insert(op_name.clone(), infix.clone());
            }
        }
        parser::Exposing::Explicit(exposeds) => {
            // We only add the explicitly named types/values/infixes
            let iter = exposeds.iter().map(|exposed| {
                match exposed {
                    parser::Exposed::Lower(value_name) => {
                        let tpe = interface
                            .values
                            .get(&value_name)
                            .ok_or_else(|| EnvError::ValueNotFound(value_name.clone()))?;

                        insert_foreign_value(
                            env,
                            value_name.clone(),
                            tpe.clone(),
                            &interface.module_name,
                        );
                    }
                    parser::Exposed::Upper(type_name, parser::Privacy::Private) => {
                        let tpe = Type::Type(type_name.clone(), vec![]);

                        env.types.insert(
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

                        insert_foreign_union_type(env, type_name, union.variants.iter());
                    }
                    parser::Exposed::Operator(variable_name) => {
                        let infix = interface
                            .infixes
                            .get(&variable_name)
                            .ok_or_else(|| EnvError::InfixNotFound(variable_name.clone()))?;

                        env.infixes.insert(variable_name.clone(), infix.clone());
                        // How do we represent infixes ?
                        // When do we do rewrite them ?
                    }
                };

                Ok(())
            });

            collect_accumulate(iter).map_err(EnvError::Multiple)?;
        }
    };

    Ok(())
}

fn insert_foreign_union_type<'a, I: Iterator<Item = &'a TypeConstructor>>(
    env: &mut RootEnvironment,
    union_name: &Name,
    variants: I,
) {
    let tpe = Type::Type(union_name.clone(), vec![]);
    env.types.insert(union_name.clone(), tpe);

    for variant in variants {
        // Variant are really qualified, which means we have to alias them if needed
        // TODO Also in case of import with public visibility, the type constructor must be exposed without
        // its qualifier
        println!("variant.name = {:?}", variant.name);
        env.constructors
            .insert(variant.name.to_name(), variant.clone());
    }
}

fn insert_foreign_value(
    env: &mut RootEnvironment,
    name: Name,
    tpe: Type,
    module_name: &ModuleName,
) {
    let vt = ValueType::Foreign(module_name.clone(), tpe.clone());

    // Can it be done more efficiently by using get_mut ?
    match env.variables.remove(&name) {
        Some(ValueType::Foreign(module, _)) => {
            env.variables
                .insert(name, ValueType::Foreigns(vec![module_name.clone(), module]));
        }
        Some(ValueType::Foreigns(mut vec)) => {
            vec.push(module_name.clone());
            env.variables.insert(name, ValueType::Foreigns(vec));
        }
        None => {
            env.variables.insert(name, vt);
        }
        _ => todo!("find out what to do in those cases"),
    }
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
#[derive(Debug)]
pub struct RootEnvironment {
    module_name: ModuleName,
    infixes: HashMap<Name, Infix>,
    types: HashMap<Name, Type>,
    constructors: HashMap<Name, TypeConstructor>,
    variables: HashMap<Name, ValueType>,
    aliases: HashMap<Name, Name>, // TODO Remove if unused
}

impl RootEnvironment {
    // TODO Do we need a local/foreign distinction for infixes ? (or in general ?)
    pub fn insert_local_infix(&mut self, name: Name, infix: Infix) {
        self.infixes.insert(name, infix);
    }

    // TODO Return an error if declaration already exists
    pub fn insert_top_level_value(&mut self, name: Name) {
        self.variables.insert(name, ValueType::TopLevel);
    }

    pub fn insert_union_type(&mut self, name: Name, union: UnionType) {
        let args = union
            .variables
            .iter()
            .map(|t| Type::Variable(t.clone()))
            .collect();
        let tpe = Type::Type(name.clone(), args);
        self.types.insert(name.clone(), tpe);

        for tctor in union.variants {
            self.constructors
                .insert(tctor.name.unqualified_name(), tctor.clone());
        }
    }
}

impl<'p> Environment<'p> for RootEnvironment {
    fn module_name(&self) -> &ModuleName {
        &self.module_name
    }

    fn find_type(&self, name: &Name) -> Option<&Type> {
        self.types.get(name)
    }

    fn find_value(&self, name: &Name) -> Option<&ValueType> {
        // TODO Not a principled change. Will require a bit more thought :)
        let name = if let Some(infix) = self.infixes.get(name) {
            &infix.function_name
        } else {
            name
        };
        self.variables.get(name)
    }

    fn find_type_constructor(&self, name: &Name) -> Option<&TypeConstructor> {
        self.constructors.get(name)
    }

    fn local_infix_exists(&self, name: &Name) -> bool {
        self.infixes.contains_key(&name)
    }

    // TODO Return error if name already exists
    fn insert_local_value(&mut self, name: &Name) {
        self.variables.insert(name.clone(), ValueType::Local);
    }

    fn new_scope<'a>(&'a self) -> ScopedEnvironment<'p, 'a>
    where
        'p: 'a,
    {
        ScopedEnvironment {
            parent: self,
            variables: HashMap::new(),
        }
    }
}

/// An Environment scoped to a module's sub expression (`let`, function, etc…)
#[derive(Debug)]
pub struct ScopedEnvironment<'root, 'parent> {
    parent: &'parent dyn Environment<'root>,
    variables: HashMap<Name, ValueType>,
}

impl<'root, 'parent> Environment<'parent> for ScopedEnvironment<'root, 'parent> {
    fn find_type(&self, name: &Name) -> Option<&Type> {
        self.parent.find_type(name)
    }

    fn module_name(&self) -> &ModuleName {
        &self.parent.module_name()
    }

    fn find_value(&self, name: &Name) -> Option<&ValueType> {
        self.variables.get(name).or(self.parent.find_value(name))
    }

    fn find_type_constructor(&self, name: &Name) -> Option<&TypeConstructor> {
        self.parent.find_type_constructor(name)
    }

    fn local_infix_exists(&self, name: &Name) -> bool {
        self.parent.local_infix_exists(&name)
    }

    // TODO Return error if name already exists
    fn insert_local_value(&mut self, name: &Name) {
        self.variables.insert(name.clone(), ValueType::Local);
    }

    fn new_scope<'a>(&'a self) -> ScopedEnvironment<'parent, 'a>
    where
        'parent: 'a,
    {
        let parent: &'a dyn Environment<'parent> = self;

        ScopedEnvironment {
            parent,
            variables: HashMap::new(),
        }
    }
}

impl<'root, 'parent> ScopedEnvironment<'root, 'parent> {
    pub fn expose_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Anything => (),
            Pattern::Int(_) => (),
            Pattern::Float(_) => (),
            Pattern::Char(_) => (),
            Pattern::Bool(_) => (),

            Pattern::Variable(n) => {
                self.variables.insert(n.clone(), ValueType::Local);
            }
            Pattern::Tuple(a, b, c) => {
                self.expose_pattern(a);
                self.expose_pattern(b);
                if let Some(c) = c {
                    self.expose_pattern(c);
                }
            }
            Pattern::Constructor { args, .. } => {
                for arg in args {
                    self.expose_pattern(arg);
                }
            }
        }
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

    fn exposing_open() -> parser::Exposing {
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
                        type_parameters: vec![Type::Variable("a".into())],
                        tpe: "Maybe.Maybe".into(),
                    },
                    TypeConstructor {
                        name: "Maybe.Nothing".into(),
                        type_parameters: vec![],
                        tpe: "Maybe.Maybe".into(),
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
        new_environment(&module_name(), &interfaces, &vec![]).map(drop)
    }

    #[test]
    fn new_import_open() -> Result<(), Vec<EnvError>> {
        let imports = vec![import("Maybe".into(), None, exposing_open())];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = new_environment(&module_name(), &interfaces, &imports)?;

        assert_eq!(env.infixes.len(), 0, "infixes={:?}", env.infixes);
        assert_eq!(env.types.len(), 1 + 1, "types={:?}", env.types); // qual + explicit
        assert_eq!(
            env.constructors.len(),
            2,
            "constructors:{:?}",
            env.constructors
        );
        assert_eq!(env.variables.len(), 3 + 3, "variables={:?}", env.variables); // qual + explicit
        assert_eq!(env.aliases.len(), 0, "aliases={:?}", env.aliases);

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
        let env = new_environment(&module_name(), &interfaces, &imports)?;

        assert_eq!(env.infixes.len(), 0, "infixes={:?}", env.infixes);
        assert_eq!(env.types.len(), 1 + 1, "types={:?}", env.types); // qual + explicit
        assert_eq!(
            env.constructors.len(),
            2,
            "constructors:{:?}",
            env.constructors
        );
        assert_eq!(
            env.variables.len(),
            3 + 1,
            "variables={:?}",
            env.variables.keys()
        ); // qual + explicit
        assert_eq!(env.aliases.len(), 0, "aliases={:?}", env.aliases);

        Ok(())
    }
}
