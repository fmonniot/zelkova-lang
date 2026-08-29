//! env module

use super::{parser, Pattern, PatternKind};
use super::{Infix, Interface, ModuleName, Name, Type, TypeConstructor, UnionType};
use crate::compiler::position::NodeSpan;
use crate::compiler::{PhaseError, SourceSpan, SpanLabel};
use crate::utils::{collect_accumulate, suggest};
use log::trace;
use std::collections::HashMap;

#[derive(Debug)]
pub enum ValueType {
    Local,
    TopLevel,
    /// A value found through exactly one import, together with where it was
    /// declared in that module — `None` when the interface it came from cannot
    /// say (a hand-built one, or one built before its file was known).
    Foreign(ModuleName, Option<SourceSpan>, Type),
    /// A value exposed unqualified by more than one import — `AmbiguousVariables`
    /// once looked up — each candidate paired the same way as `Foreign`.
    Foreigns(Vec<(ModuleName, Option<SourceSpan>)>),
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

    /// Every value name resolvable from this scope right now — local
    /// bindings first, then everything the enclosing scope can already
    /// resolve — for building a "did you mean …?" suggestion once
    /// `find_value` has already failed (`ERR-7`). Not on the lookup path
    /// itself: this walks the whole table, `find_value` a single key.
    fn value_names(&self) -> Vec<Name>;

    /// Same as [`value_names`](Environment::value_names), for type
    /// constructors.
    fn type_constructor_names(&self) -> Vec<Name>;

    fn local_infix_exists(&self, name: &Name) -> bool;

    #[allow(dead_code)]
    fn insert_local_value(&mut self, name: &Name);

    // 'parent must lives at least as long as 'a
    fn new_scope<'a>(&'a self) -> ScopedEnvironment<'parent, 'a>
    where
        'parent: 'a;
}

/// Build a "did you mean …?" suggestion for a name that failed to resolve,
/// from the full set of names available at the point of failure (`ERR-7`).
///
/// Respects the qualified/unqualified distinction called out on
/// [`super::Error::VariableNotFound`]: a qualified `target` (`Widget.map`)
/// only considers candidates qualified with that exact same module prefix,
/// comparing distance on the local part once the prefix is confirmed equal;
/// an unqualified `target` only considers unqualified candidates. Crossing
/// that boundary would let a typo in one module's name get "fixed" by
/// pointing at an entirely different module's value of the same local name,
/// which is the wrong-module case the ticket calls out explicitly as worse
/// than no suggestion at all.
///
/// The `Environment`'s own candidate sets (`RootEnvironment::variables`,
/// `RootEnvironment::constructors`) hold both forms as distinct keys already
/// — `map` and `Maybe.map` are two separate entries once `Maybe` is
/// imported — so this needs no lookup beyond string-splitting on the last
/// `.`. The three `EnvError` sites that use this — `ValueNotFound`,
/// `UnionNotFound`, `InfixNotFound` — draw from a single `Interface`'s own
/// namespace (`interface.values`, `.unions`, `.infixes`), which never holds
/// a qualified name, so they always take the unqualified branch.
///
/// `InterfaceNotFound` deliberately does *not* come through here: its
/// candidates are module names, where a `.` separates two segments of one
/// identifier rather than a module from a value. `process_import` calls
/// [`suggest`](crate::utils::suggest) over the whole string instead.
pub fn suggest_name(target: &Name, candidates: impl Iterator<Item = Name>) -> Option<Name> {
    match target.as_str().rsplit_once('.') {
        Some((prefix, local)) => {
            let locals: Vec<String> = candidates
                .filter_map(|c| {
                    c.as_str().rsplit_once('.').and_then(|(p, l)| {
                        if p == prefix {
                            Some(l.to_string())
                        } else {
                            None
                        }
                    })
                })
                .collect();
            let refs: Vec<&str> = locals.iter().map(String::as_str).collect();
            suggest(local, refs).map(|l| Name::new(format!("{}.{}", prefix, l)))
        }
        None => {
            let unqualified: Vec<String> = candidates
                .filter(|c| !c.as_str().contains('.'))
                .map(|c| c.to_string())
                .collect();
            let refs: Vec<&str> = unqualified.iter().map(String::as_str).collect();
            suggest(target.as_str(), refs).map(Name::new)
        }
    }
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
    };
    let mut errors = vec![];

    for parser::Import {
        name,
        alias,
        exposing,
        span,
    } in imports
    {
        match process_import(&mut env, interfaces, name, alias, exposing, *span) {
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
    // The `import` line this is resolving. Only `InterfaceNotFound` uses it: the
    // module name itself is not part of any `parser::Exposed`, so the `import` line
    // is the finest span there is for it. Every other error raised further down
    // names one entry of the exposing list and carries that entry's own span
    // instead (`ERR-9`) — `bar` in `import Foo exposing (bar)`, not the line it
    // sits on.
    span: NodeSpan,
) -> Result<(), EnvError> {
    let interface = interfaces.get(imported_module_name).ok_or_else(|| {
        // Module names live in a flat namespace and a dotted one — `Js.Basics` —
        // is a single opaque identifier, not a prefix plus a local part. So this
        // measures the distance over the whole string with `suggest` rather than
        // going through `suggest_name`, whose qualified/unqualified split is
        // about `Name` vs `QualName` for values and constructors. Splitting here
        // would silently drop every typo outside the last segment: `Jz.Basics`
        // and `JsBasics` are both one edit from `Js.Basics`, and neither shares
        // its prefix.
        let suggestion = suggest(
            imported_module_name.as_str(),
            interfaces.keys().map(Name::as_str),
        )
        .map(Name::new);
        EnvError::InterfaceNotFound(imported_module_name.clone(), span, suggestion)
    })?;

    trace!(
        "process_import(imported_module_name={:?}, alias={:?}, exposing: {:?})",
        imported_module_name,
        alias,
        exposing
    );

    // First we insert all values/types from the module, prefixed with the module name or its alias
    let prefix = alias.as_ref().unwrap_or(imported_module_name);

    for (value_name, (node_span, tpe)) in &interface.values {
        insert_foreign_value(
            env,
            value_name.qualify_with_name(prefix).unwrap().to_name(),
            tpe.clone(),
            interface.source_span(*node_span),
            &interface.module_name,
        );
    }

    for (union_name, union) in &interface.unions {
        insert_foreign_union_type(env, Some(prefix), union_name, union.variants.iter());
    }

    // TODO Infix ?

    // Then we insert unqualified values/types
    match exposing {
        parser::Exposing::Open => {
            // We add everything to the current environment

            for (value_name, (node_span, tpe)) in &interface.values {
                insert_foreign_value(
                    env,
                    value_name.clone(),
                    tpe.clone(),
                    interface.source_span(*node_span),
                    &interface.module_name,
                );
            }

            for (op_name, infix) in &interface.infixes {
                env.infixes.insert(op_name.clone(), infix.clone());
            }

            // We need to insert the type without any qualifier, including variants
            for (union_name, union) in &interface.unions {
                insert_foreign_union_type(env, None, union_name, union.variants.iter());
            }
        }

        parser::Exposing::Explicit(exposeds) => {
            // We only add the explicitly named types/values/infixes
            let iter = exposeds.iter().map(|exposed| {
                match &exposed.kind {
                    parser::ExposedKind::Lower(value_name) => {
                        let (node_span, tpe) =
                            interface.values.get(value_name).ok_or_else(|| {
                                let suggestion =
                                    suggest_name(value_name, interface.values.keys().cloned());
                                EnvError::ValueNotFound(
                                    value_name.clone(),
                                    exposed.span,
                                    suggestion,
                                )
                            })?;

                        insert_foreign_value(
                            env,
                            value_name.clone(),
                            tpe.clone(),
                            interface.source_span(*node_span),
                            &interface.module_name,
                        );
                    }
                    parser::ExposedKind::Upper(type_name, parser::Privacy::Private) => {
                        let tpe = Type::Type(type_name.clone(), vec![]);

                        // Add the type without qualifier and without constructors (they are private)
                        env.types.insert(type_name.clone(), tpe);
                    }
                    parser::ExposedKind::Upper(type_name, parser::Privacy::Public) => {
                        let union = interface.unions.get(type_name).ok_or_else(|| {
                            let suggestion =
                                suggest_name(type_name, interface.unions.keys().cloned());
                            EnvError::UnionNotFound(type_name.clone(), exposed.span, suggestion)
                        })?;

                        insert_foreign_union_type(env, None, type_name, union.variants.iter());
                    }
                    parser::ExposedKind::Operator(variable_name) => {
                        let infix = interface.infixes.get(variable_name).ok_or_else(|| {
                            let suggestion =
                                suggest_name(variable_name, interface.infixes.keys().cloned());
                            EnvError::InfixNotFound(variable_name.clone(), exposed.span, suggestion)
                        })?;

                        env.infixes.insert(variable_name.clone(), infix.clone());
                        // How do we represent infixes ?
                        // When do we do rewrite them ?
                    }
                };

                Ok(())
            });

            collect_accumulate::<_, _, _, ()>(iter).map_err(EnvError::Multiple)?;
        }
    };

    Ok(())
}

fn insert_foreign_union_type<'a, I: Iterator<Item = &'a TypeConstructor>>(
    env: &mut RootEnvironment,
    qualifier: Option<&Name>,
    union_name: &Name,
    variants: I,
) {
    // If there is a given qualifier use it, otherwise use the name as is
    let qualify = |n: &Name| {
        qualifier
            .and_then(|q| n.qualify_with_name(q))
            .map(|q| q.to_name())
            .unwrap_or(n.clone())
    };

    env.types
        .insert(qualify(union_name), Type::Type(union_name.clone(), vec![]));

    for variant in variants {
        // Variant are not qualified, which means we have to alias/qualify them as needed
        env.constructors
            .insert(qualify(&variant.name), variant.clone());
    }
}

fn insert_foreign_value(
    env: &mut RootEnvironment,
    name: Name,
    tpe: Type,
    source: Option<SourceSpan>,
    module_name: &ModuleName,
) {
    let vt = ValueType::Foreign(module_name.clone(), source, tpe.clone());

    // Can it be done more efficiently by using get_mut ?
    match env.variables.remove(&name) {
        Some(ValueType::Foreign(module, prev_source, _)) => {
            env.variables.insert(
                name,
                ValueType::Foreigns(vec![(module_name.clone(), source), (module, prev_source)]),
            );
        }
        Some(ValueType::Foreigns(mut vec)) => {
            vec.push((module_name.clone(), source));
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
    /// No module of that name was available to import, where the `import` was
    /// written, and — when one candidate module name is a close enough
    /// typo-distance match (`ERR-7`) — a suggestion for what was meant.
    InterfaceNotFound(Name, NodeSpan, Option<Name>),
    /// A `TypeIdent(..)` in an `exposing` list naming a type the imported module
    /// does not declare, where that name was written (`ERR-9`), and an optional
    /// "did you mean …?" suggestion (`ERR-7`).
    UnionNotFound(Name, NodeSpan, Option<Name>),
    /// An `(op)` in an `exposing` list naming an infix the imported module does not
    /// declare, where that name was written (`ERR-9`), and an optional "did you
    /// mean …?" suggestion (`ERR-7`).
    InfixNotFound(Name, NodeSpan, Option<Name>),
    /// A lowercase name in an `exposing` list naming a value the imported module
    /// does not declare, where that name was written (`ERR-9`), and an optional
    /// "did you mean …?" suggestion (`ERR-7`).
    ValueNotFound(Name, NodeSpan, Option<Name>),
    Multiple(Vec<EnvError>),
}

/// A "did you mean `X`?" suffix for a label message, when a suggestion was
/// found — empty otherwise, so callers can always append the result without
/// checking `is_some()` first (`ERR-7`).
fn suggestion_suffix(suggestion: &Option<Name>) -> String {
    match suggestion {
        Some(name) => format!(" — did you mean `{}`?", name),
        None => String::new(),
    }
}

/// An import that could not be resolved always names the thing it could not find,
/// so the message can quote it back.
impl PhaseError for EnvError {
    fn message(&self) -> String {
        match self {
            EnvError::InterfaceNotFound(name, _, _) => {
                format!("cannot find a module named `{}` to import", name)
            }
            EnvError::UnionNotFound(name, _, _) => {
                format!(
                    "the imported module does not expose a type named `{}`",
                    name
                )
            }
            EnvError::InfixNotFound(name, _, _) => format!(
                "the imported module does not expose an infix operator named `{}`",
                name
            ),
            EnvError::ValueNotFound(name, _, _) => format!(
                "the imported module does not expose a value named `{}`",
                name
            ),
            EnvError::Multiple(errors) => match errors.as_slice() {
                [only] => only.message(),
                many => format!("{} imports could not be resolved", many.len()),
            },
        }
    }

    fn labels(&self) -> Vec<SpanLabel> {
        let primary = |span: &NodeSpan, message: String| match span.span() {
            Some(span) => vec![SpanLabel {
                span,
                message,
                primary: true,
                file: None,
            }],
            None => Vec::new(),
        };

        match self {
            EnvError::InterfaceNotFound(_, span, suggestion) => primary(
                span,
                format!(
                    "no module of this name was found{}",
                    suggestion_suffix(suggestion)
                ),
            ),
            EnvError::UnionNotFound(name, span, suggestion) => primary(
                span,
                format!(
                    "`{}` is not exposed by the imported module{}",
                    name,
                    suggestion_suffix(suggestion)
                ),
            ),
            EnvError::InfixNotFound(name, span, suggestion) => primary(
                span,
                format!(
                    "`{}` is not exposed by the imported module{}",
                    name,
                    suggestion_suffix(suggestion)
                ),
            ),
            EnvError::ValueNotFound(name, span, suggestion) => primary(
                span,
                format!(
                    "`{}` is not exposed by the imported module{}",
                    name,
                    suggestion_suffix(suggestion)
                ),
            ),
            EnvError::Multiple(errors) => errors.iter().flat_map(|e| e.labels()).collect(),
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            EnvError::Multiple(errors) => match errors.as_slice() {
                [only] => only.notes(),
                many => many.iter().flat_map(|e| e.message_and_notes()).collect(),
            },
            _ => Vec::new(),
        }
    }
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
}

impl RootEnvironment {
    // TODO Do we need a local/foreign distinction for infixes ? (or in general ?)
    pub fn insert_local_infix(&mut self, name: Name, infix: Infix) {
        self.infixes.insert(name, infix);
    }

    // TODO Use insert_foreign_value (and rename to remove the foreign part)
    // TODO Return an error if declaration already exists
    pub fn insert_top_level_value(&mut self, name: Name) {
        self.variables.insert(name, ValueType::TopLevel);
    }

    // TODO Use insert_foreign_union_type (and rename to remove the foreign part)
    pub fn insert_union_type(&mut self, name: Name, union: UnionType) {
        let args = union
            .variables
            .iter()
            .map(|t| Type::Variable(t.clone()))
            .collect();
        let tpe = Type::Type(name.clone(), args);
        self.types.insert(name.clone(), tpe);

        for tctor in union.variants {
            self.constructors.insert(tctor.name.clone(), tctor.clone());
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

    fn value_names(&self) -> Vec<Name> {
        self.variables.keys().cloned().collect()
    }

    fn type_constructor_names(&self) -> Vec<Name> {
        self.constructors.keys().cloned().collect()
    }

    fn local_infix_exists(&self, name: &Name) -> bool {
        self.infixes.contains_key(name)
    }

    // TODO Return error if name already exists
    // Use import_foreign_value ?
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
        self.parent.module_name()
    }

    fn find_value(&self, name: &Name) -> Option<&ValueType> {
        self.variables.get(name).or(self.parent.find_value(name))
    }

    fn find_type_constructor(&self, name: &Name) -> Option<&TypeConstructor> {
        self.parent.find_type_constructor(name)
    }

    fn value_names(&self) -> Vec<Name> {
        let mut names = self.parent.value_names();
        names.extend(self.variables.keys().cloned());
        names
    }

    fn type_constructor_names(&self) -> Vec<Name> {
        // A scope never binds its own constructors — only patterns bind
        // values (`expose_pattern`) — so this always delegates.
        self.parent.type_constructor_names()
    }

    fn local_infix_exists(&self, name: &Name) -> bool {
        self.parent.local_infix_exists(name)
    }

    // TODO Return error if name already exists
    // Needs to reuse the logic in import_foreign_value
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
        match &pattern.kind {
            PatternKind::Anything => (),
            PatternKind::Int(_) => (),
            PatternKind::Float(_) => (),
            PatternKind::Char(_) => (),
            PatternKind::Bool(_) => (),

            PatternKind::Variable(n) => {
                self.variables.insert(n.clone(), ValueType::Local);
            }
            PatternKind::Tuple(tuple) => {
                for pattern in tuple.iter() {
                    self.expose_pattern(pattern);
                }
            }
            PatternKind::Constructor { args, .. } => {
                for arg in args {
                    self.expose_pattern(arg);
                }
            }
        }
    }
}

// TODO Tests needs some love to make them easier to read. Currently the assert_eq are making it
// hard to grok what is being verified.
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
            // Hand-built, not parsed: there is no source text behind it.
            span: NodeSpan::none(),
        }
    }

    fn exposing_open() -> parser::Exposing {
        parser::Exposing::Open
    }

    fn exposing_explicit(exposeds: Vec<parser::Exposed>) -> parser::Exposing {
        parser::Exposing::Explicit(exposeds)
    }

    // module Maybe exposing (andThen, map, withDefault, Maybe(..))
    fn maybe_interface() -> (Name, Interface) {
        let type_var = |name: &str| Type::Variable(name.into());

        let type_hk = |name: &str, params| Type::Type(name.into(), params);

        let type_fun = |t1, t2| Type::Arrow(Box::new(t1), Box::new(t2));

        let mut values = HashMap::new();
        // andThen : (a -> Maybe b) -> Maybe a -> Maybe b
        values.insert(
            "andThen".into(),
            (
                // Hand-built, not canonicalized from source: no position behind it.
                NodeSpan::none(),
                type_fun(
                    type_fun(type_var("a"), type_hk("Maybe", vec![type_var("b")])),
                    type_fun(
                        type_hk("Maybe", vec![type_var("a")]),
                        type_hk("Maybe", vec![type_var("b")]),
                    ),
                ),
            ),
        );
        // map : (a -> b) -> Maybe a -> Maybe b
        values.insert(
            "map".into(),
            (
                NodeSpan::none(),
                type_fun(
                    type_fun(type_var("a"), type_var("b")),
                    type_fun(
                        type_hk("Maybe", vec![type_var("a")]),
                        type_hk("Maybe", vec![type_var("b")]),
                    ),
                ),
            ),
        );
        // withDefault : a -> Maybe a -> a
        values.insert(
            "withDefault".into(),
            (
                NodeSpan::none(),
                type_fun(
                    type_var("a"),
                    type_fun(type_hk("Maybe", vec![type_var("a")]), type_var("a")),
                ),
            ),
        );

        let mut unions = HashMap::new();
        unions.insert(
            "Maybe".into(),
            UnionType {
                span: NodeSpan::none(),
                variables: vec![],
                variants: vec![
                    TypeConstructor {
                        name: "Just".into(),
                        type_parameters: vec![Type::Variable("a".into())],
                        tpe: "Maybe".into(),
                    },
                    TypeConstructor {
                        name: "Nothing".into(),
                        type_parameters: vec![],
                        tpe: "Maybe".into(),
                    },
                ],
            },
        );

        let interface = Interface {
            module_name: ModuleName::new(PackageName::new("zelkova", "core"), "Maybe".into()),
            values,
            unions,
            infixes: HashMap::new(),
            file: None,
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
        let env = new_environment(&module_name(), &interfaces, &vec![])?;

        assert_eq!(env.infixes.len(), 0, "infixes={:?}", env.infixes);
        assert_eq!(env.types.len(), 0, "types={:?}", env.types); // qual + explicit
        assert_eq!(
            env.constructors.len(),
            0,
            "constructors:{:?}",
            env.constructors
        );
        assert_eq!(env.variables.len(), 0, "variables={:?}", env.variables); // qual + explicit

        Ok(())
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

        // Assert we have the expected
        assert!(
            env.find_value(&"andThen".into()).is_some(),
            "value andThen not found"
        );
        assert!(
            env.find_value(&"map".into()).is_some(),
            "value map not found"
        );
        assert!(
            env.find_value(&"withDefault".into()).is_some(),
            "value withDefault not found"
        );

        assert!(
            env.find_value(&"Maybe.andThen".into()).is_some(),
            "value Maybe.andThen not found"
        );
        assert!(
            env.find_value(&"Maybe.map".into()).is_some(),
            "value Maybe.map not found"
        );
        assert!(
            env.find_value(&"Maybe.withDefault".into()).is_some(),
            "value Maybe.withDefault not found"
        );

        assert!(
            env.find_type(&"Maybe".into()).is_some(),
            "type Maybe not found"
        );

        assert!(
            env.find_type_constructor(&"Maybe.Just".into()).is_some(),
            "type constructor Maybe.Just not found"
        );
        assert!(
            env.find_type_constructor(&"Maybe.Nothing".into()).is_some(),
            "type constructor Maybe.Nothing not found"
        );
        assert!(
            env.find_type_constructor(&"Just".into()).is_some(),
            "type constructor Just not found"
        );
        assert!(
            env.find_type_constructor(&"Nothing".into()).is_some(),
            "type constructor Nothing not found"
        );

        // Make sure we don't have more than what is expected
        assert_eq!(env.infixes.len(), 0, "infixes={:?}", env.infixes);
        assert_eq!(env.types.len(), 1 + 1, "types={:?}", env.types); // qual + explicit
        assert_eq!(
            env.constructors.len(),
            4,
            "constructors:{:#?}",
            env.constructors
        );
        assert_eq!(env.variables.len(), 3 + 3, "variables={:?}", env.variables); // qual + explicit

        Ok(())
    }

    #[test]
    fn new_import_values() -> Result<(), Vec<EnvError>> {
        let imports = vec![import(
            "Maybe".into(),
            None,
            exposing_explicit(vec![
                parser::Exposed::bare(parser::ExposedKind::Upper(
                    "Maybe".into(),
                    parser::Privacy::Private,
                )),
                parser::Exposed::bare(parser::ExposedKind::Lower("andThen".into())),
            ]),
        )];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = new_environment(&module_name(), &interfaces, &imports)?;

        // Lookup the expected values
        assert!(
            env.find_value(&"andThen".into()).is_some(),
            "value andThen not found"
        );

        assert!(
            env.find_value(&"Maybe.andThen".into()).is_some(),
            "value Maybe.andThen not found"
        );
        assert!(
            env.find_value(&"Maybe.map".into()).is_some(),
            "value Maybe.map not found"
        );
        assert!(
            env.find_value(&"Maybe.withDefault".into()).is_some(),
            "value Maybe.withDefault not found"
        );

        assert!(
            env.find_type(&"Maybe.Maybe".into()).is_some(),
            "type Maybe.Maybe not found"
        );
        assert!(
            env.find_type(&"Maybe".into()).is_some(),
            "type Maybe not found"
        );

        // Make sure we don't have more than what is expected
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
            3 + 1, // qual + explicit
            "variables={:?}",
            env.variables.keys()
        );

        Ok(())
    }

    #[test]
    fn new_import_type_with_constructors() -> Result<(), Vec<EnvError>> {
        let imports = vec![import(
            "Maybe".into(),
            None,
            exposing_explicit(vec![parser::Exposed::bare(parser::ExposedKind::Upper(
                "Maybe".into(),
                parser::Privacy::Public,
            ))]),
        )];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = new_environment(&module_name(), &interfaces, &imports)?;

        // Lookup the expected
        assert!(
            env.find_value(&"Maybe.andThen".into()).is_some(),
            "value Maybe.andThen not found"
        );
        assert!(
            env.find_value(&"Maybe.map".into()).is_some(),
            "value Maybe.map not found"
        );
        assert!(
            env.find_value(&"Maybe.withDefault".into()).is_some(),
            "value Maybe.withDefault not found"
        );

        assert!(
            env.find_type(&"Maybe".into()).is_some(),
            "type Maybe not found"
        );
        assert!(
            env.find_type(&"Maybe.Maybe".into()).is_some(),
            "type Maybe.Maybe not found"
        );

        assert!(
            env.find_type_constructor(&"Maybe.Just".into()).is_some(),
            "type constructor Maybe.Just not found"
        );
        assert!(
            env.find_type_constructor(&"Maybe.Nothing".into()).is_some(),
            "type constructor Maybe.Nothing not found"
        );
        assert!(
            env.find_type_constructor(&"Just".into()).is_some(),
            "type constructor Just not found"
        );
        assert!(
            env.find_type_constructor(&"Nothing".into()).is_some(),
            "type constructor Nothing not found"
        );

        // Make sure we don't have more than what is expected
        assert_eq!(env.infixes.len(), 0, "infixes={:?}", env.infixes);
        assert_eq!(env.types.len(), 2, "types={:?}", env.types); // qual + explicit
        assert_eq!(
            env.constructors.len(),
            4,
            "constructors:{:?}",
            env.constructors
        );
        assert_eq!(
            env.variables.len(),
            3,
            "variables={:?}",
            env.variables.keys()
        ); // qual + explicit

        Ok(())
    }

    #[test]
    fn new_import_type_with_constructors_and_aliases() -> Result<(), Vec<EnvError>> {
        let imports = vec![import(
            "Maybe".into(),
            Some("M".into()),
            exposing_explicit(vec![parser::Exposed::bare(parser::ExposedKind::Upper(
                "Maybe".into(),
                parser::Privacy::Public,
            ))]),
        )];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }
        let env = new_environment(&module_name(), &interfaces, &imports)?;

        // Lookup the expected
        assert!(
            env.find_value(&"M.andThen".into()).is_some(),
            "value M.andThen not found"
        );
        assert!(
            env.find_value(&"M.map".into()).is_some(),
            "value M.map not found"
        );
        assert!(
            env.find_value(&"M.withDefault".into()).is_some(),
            "value M.withDefault not found"
        );

        assert!(
            env.find_type(&"Maybe".into()).is_some(),
            "type Maybe not found"
        );
        assert!(
            env.find_type(&"M.Maybe".into()).is_some(),
            "type M.Maybe not found"
        );

        assert!(
            env.find_type_constructor(&"M.Just".into()).is_some(),
            "type constructor M.Just not found"
        );
        assert!(
            env.find_type_constructor(&"M.Nothing".into()).is_some(),
            "type constructor M.Nothing not found"
        );

        assert!(
            env.find_type_constructor(&"Just".into()).is_some(),
            "type constructor Just not found"
        );
        assert!(
            env.find_type_constructor(&"Nothing".into()).is_some(),
            "type constructor Nothing not found"
        );

        // Make sure we don't have more than what is expected
        assert_eq!(env.infixes.len(), 0, "infixes={:?}", env.infixes);
        assert_eq!(env.types.len(), 2, "types={:?}", env.types);
        assert_eq!(
            env.constructors.len(),
            4,
            "constructors:{:?}",
            env.constructors
        );
        assert_eq!(
            env.variables.len(),
            3,
            "variables={:?}",
            env.variables.keys()
        );

        Ok(())
    }

    // ── ERR-7: "did you mean …?" suggestions ────────────────────────────────
    //
    // These hand-build every `Import`/`Exposed` with `NodeSpan::none()` (see the
    // `import`/`exposing_*` helpers above), so `EnvError::labels` — which needs a
    // real span to attach a caret to — renders nothing for them; that path is
    // covered instead by `tests/compiler/canonical.rs`, which goes through the
    // parser and so has real spans. These assert directly on the `Option<Name>`
    // suggestion field each variant now carries.

    /// A minimal interface with one infix, `maybe_interface` has none — needed to
    /// exercise `EnvError::InfixNotFound`'s suggestion.
    fn ops_interface() -> (Name, Interface) {
        let mut infixes = HashMap::new();
        infixes.insert(
            "plus".into(),
            Infix {
                associativity: Associativity::Left,
                precedence: 6,
                function_name: "add".into(),
                span: NodeSpan::none(),
            },
        );

        let interface = Interface {
            module_name: ModuleName::new(PackageName::new("test", "project"), "Ops".into()),
            values: HashMap::new(),
            unions: HashMap::new(),
            infixes,
            file: None,
        };

        ("Ops".into(), interface)
    }

    /// Neutralising the `suggest` call in the `InterfaceNotFound` arm of
    /// `process_import` (passing `None` instead) turns this red: `suggestion`
    /// becomes `None` where the test expects `Some("Maybe")`.
    #[test]
    fn unknown_module_suggests_a_near_miss() {
        let imports = vec![import("Mabye".into(), None, exposing_open())];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }

        let errors = new_environment(&module_name(), &interfaces, &imports)
            .expect_err("an unknown module should not resolve");
        assert_eq!(errors.len(), 1, "got {:?}", errors);

        match &errors[0] {
            EnvError::InterfaceNotFound(name, _, suggestion) => {
                assert_eq!(name, &Name::from("Mabye"));
                assert_eq!(suggestion, &Some(Name::from("Maybe")));
            }
            other => panic!("expected InterfaceNotFound, got {:?}", other),
        }
    }

    /// A module name resembling nothing among the known interfaces gets no
    /// suggestion — a bad one would send the reader to check an unrelated import.
    #[test]
    fn unrelated_unknown_module_has_no_suggestion() {
        let imports = vec![import("Zzzzzzzzzzzz".into(), None, exposing_open())];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }

        let errors = new_environment(&module_name(), &interfaces, &imports)
            .expect_err("an unknown module should not resolve");
        assert_eq!(errors.len(), 1, "got {:?}", errors);

        match &errors[0] {
            EnvError::InterfaceNotFound(_, _, suggestion) => {
                assert_eq!(suggestion, &None);
            }
            other => panic!("expected InterfaceNotFound, got {:?}", other),
        }
    }

    /// A dotted module name — `Js.Basics`, `Js.Utils` and `Js.Bitwise` all exist
    /// in `std/core/src` today — is one identifier, not a module prefix plus a
    /// local part. Routing this through `suggest_name` would compare only the
    /// segment after the last `.` and only against candidates sharing the prefix
    /// exactly, so a typo anywhere else, or a dropped dot, would get nothing.
    /// All three of these are one edit from `Js.Basics`.
    #[test]
    fn unknown_dotted_module_suggests_on_the_whole_name() {
        let mut interfaces = HashMap::new();
        {
            let (_, iface) = maybe_interface();
            interfaces.insert("Js.Basics".into(), iface);
        }

        // `Js.Basicz` shares the prefix, `Jz.Basics` does not, and `JsBasics` has
        // no prefix at all — the last is the likeliest typo of the three.
        for typo in ["Js.Basicz", "Jz.Basics", "JsBasics"] {
            let imports = vec![import(typo.into(), None, exposing_open())];
            let errors = new_environment(&module_name(), &interfaces, &imports)
                .expect_err("an unknown module should not resolve");
            assert_eq!(errors.len(), 1, "got {:?}", errors);

            match &errors[0] {
                EnvError::InterfaceNotFound(name, _, suggestion) => {
                    assert_eq!(name, &Name::from(typo));
                    assert_eq!(
                        suggestion,
                        &Some(Name::from("Js.Basics")),
                        "no suggestion for `{}`",
                        typo
                    );
                }
                other => panic!("expected InterfaceNotFound, got {:?}", other),
            }
        }
    }

    /// The `Explicit` exposing arm wraps whatever `collect_accumulate` returns in
    /// `EnvError::Multiple`, even for a single failing entry — see
    /// `process_import`'s last statement — so the `UnionNotFound` this produces is
    /// nested one level deeper than `InterfaceNotFound`'s.
    ///
    /// Neutralising the `suggest_name` call in the `Upper(.., Public)` arm of
    /// `process_import` (passing `None` instead) turns this red.
    #[test]
    fn unknown_exposed_type_suggests_a_near_miss() {
        let imports = vec![import(
            "Maybe".into(),
            None,
            exposing_explicit(vec![parser::Exposed::bare(parser::ExposedKind::Upper(
                "Mayeb".into(),
                parser::Privacy::Public,
            ))]),
        )];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = maybe_interface();
            interfaces.insert(name, iface);
        }

        let errors = new_environment(&module_name(), &interfaces, &imports)
            .expect_err("an unknown exposed type should not resolve");
        assert_eq!(errors.len(), 1, "got {:?}", errors);

        let inner = match &errors[0] {
            EnvError::Multiple(inner) => inner,
            other => panic!("expected Multiple, got {:?}", other),
        };
        assert_eq!(inner.len(), 1, "got {:?}", inner);

        match &inner[0] {
            EnvError::UnionNotFound(name, _, suggestion) => {
                assert_eq!(name, &Name::from("Mayeb"));
                assert_eq!(suggestion, &Some(Name::from("Maybe")));
            }
            other => panic!("expected UnionNotFound, got {:?}", other),
        }
    }

    /// Same nesting as the type case above, for `EnvError::InfixNotFound`.
    ///
    /// Neutralising the `suggest_name` call in the `Operator` arm of
    /// `process_import` (passing `None` instead) turns this red.
    #[test]
    fn unknown_exposed_infix_suggests_a_near_miss() {
        let imports = vec![import(
            "Ops".into(),
            None,
            exposing_explicit(vec![parser::Exposed::bare(parser::ExposedKind::Operator(
                "pluss".into(),
            ))]),
        )];
        let mut interfaces = HashMap::new();
        {
            let (name, iface) = ops_interface();
            interfaces.insert(name, iface);
        }

        let errors = new_environment(&module_name(), &interfaces, &imports)
            .expect_err("an unknown exposed infix should not resolve");
        assert_eq!(errors.len(), 1, "got {:?}", errors);

        let inner = match &errors[0] {
            EnvError::Multiple(inner) => inner,
            other => panic!("expected Multiple, got {:?}", other),
        };
        assert_eq!(inner.len(), 1, "got {:?}", inner);

        match &inner[0] {
            EnvError::InfixNotFound(name, _, suggestion) => {
                assert_eq!(name, &Name::from("pluss"));
                assert_eq!(suggestion, &Some(Name::from("plus")));
            }
            other => panic!("expected InfixNotFound, got {:?}", other),
        }
    }
}
