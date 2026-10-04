//! What a `class` and an `instance` declaration are once canonicalized, and the checks
//! [Type classes](../../../docs/spec/type-classes.md) holds each one to.
//!
//! A class is a name in the types namespace, and its members are values of the module
//! that declares it. An instance has no name: it reaches another module through
//! [`Interface::instances`](crate::Interface::instances), which every module re-publishes
//! whole, so an instance is in scope everywhere its declaring module is reachable through
//! imports, whatever any `exposing` list says.
//!
//! Nothing here type checks anything. A member's signature is canonicalized as a type; an
//! instance's bindings and a derivation's are canonicalized as ordinary values. The typer
//! reads an instance's bindings against the member signatures this module keeps
//! (`typer::classes`) and does not read a derivation's. Whether a derivation is well
//! formed is not checked.

use super::environment::{Environment, RootEnvironment};
use super::{
    binding_value, validate_context, Error, InstanceHeadProblem, NameTakenBy, Type, Value,
};
use crate::name::{Name, QualName};
use crate::{ModuleName, SourceSpan, SpanLabel};
use std::collections::{HashMap, HashSet};
use zelkova_syntax::parser;
use zelkova_syntax::position::NodeSpan;
use zelkova_syntax::tuple::Tuple;

/// A constraint: a class, resolved to its declaration, and the type variable it is on.
///
/// What a class head's superclasses, an instance's context and an annotation's context
/// are made of.
#[derive(Debug, Clone, PartialEq)]
pub struct Constraint {
    /// The class, named by the package and module that declared it.
    pub class: QualName,
    /// The type variable the class is required of.
    pub variable: Name,
    /// Where the constraint was written.
    pub span: NodeSpan,
}

/// One member signature of a class: `compare : a -> a -> Order`.
#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub name: Name,
    /// The signature as written, the class variable free in it. Outside the class the
    /// member's type is this with the class's own constraint in front, which no
    /// canonical [`Type`] can express; the class carries the constraint instead.
    pub tpe: Type,
    /// Where the signature was written.
    pub span: NodeSpan,
}

/// What a class declares that another module can see: everything but its derivations.
///
/// It is what [`Interface::classes`](crate::Interface::classes) carries for each class a
/// module exposes, and what an importer checks an instance of that class against.
#[derive(Debug, Clone, PartialEq)]
pub struct ClassSignature {
    /// The class variable, `a` in `class Comparable a where`.
    pub variable: Name,
    /// The superclasses, each a constraint on [`variable`](Self::variable).
    pub superclasses: Vec<Constraint>,
    /// The member signatures, in the order written.
    pub members: Vec<Member>,
    /// Where the head line was written, `class` through `where`.
    pub span: NodeSpan,
}

/// A `class` declaration of the module under check.
#[derive(Debug, PartialEq)]
pub struct Class {
    pub signature: ClassSignature,
    /// The derivations in the class body, in the order written.
    pub derivations: Vec<Derivation>,
}

/// A derivation in a class body: `derived eq` and the bindings under it.
#[derive(Debug, PartialEq)]
pub struct Derivation {
    /// The member it is for, as written. Nothing checks it names one.
    pub member: Name,
    /// The bindings, in the order written, each canonicalized as an ordinary value. A
    /// binding that did not canonicalize is left out, and its error reported.
    pub bindings: Vec<Value>,
    /// Where `derived name` was written.
    pub span: NodeSpan,
}

/// The type an instance is for: a declared type applied to distinct variables, one per
/// parameter; a tuple of distinct variables; or `()`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstanceHead {
    /// A declared type, named by its declaration, and the variables it is applied to.
    Type(QualName, Vec<Name>),
    Tuple(Tuple<Name>),
    Unit,
}

impl InstanceHead {
    /// The name at the front of the head, which together with the class is what
    /// identifies an instance.
    pub fn name(&self) -> HeadName {
        match self {
            InstanceHead::Type(name, _) => HeadName::Type(name.clone()),
            InstanceHead::Tuple(Tuple::Two(..)) => HeadName::TwoTuple,
            InstanceHead::Tuple(Tuple::Three(..)) => HeadName::ThreeTuple,
            InstanceHead::Unit => HeadName::Unit,
        }
    }

    /// The variables the head binds, in the order written.
    pub fn variables(&self) -> Vec<&Name> {
        match self {
            InstanceHead::Type(_, variables) => variables.iter().collect(),
            InstanceHead::Tuple(tuple) => tuple.iter().collect(),
            InstanceHead::Unit => Vec::new(),
        }
    }
}

/// The name at the front of an [`InstanceHead`]. Two instances of one class whose heads
/// share it are one instance declared twice.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HeadName {
    Type(QualName),
    TwoTuple,
    ThreeTuple,
    Unit,
}

impl HeadName {
    /// The head in the words of the source, for a message.
    pub fn describe(&self) -> String {
        match self {
            HeadName::Type(name) => format!("`{}`", name.unqualified_name()),
            HeadName::TwoTuple => "a tuple of two".to_owned(),
            HeadName::ThreeTuple => "a tuple of three".to_owned(),
            HeadName::Unit => "`()`".to_owned(),
        }
    }

    /// The module that declares the head's type, when one does. A tuple and `()` are
    /// declared in no module.
    pub fn module(&self) -> Option<ModuleName> {
        match self {
            HeadName::Type(name) => Some(declaring_module(name)),
            HeadName::TwoTuple | HeadName::ThreeTuple | HeadName::Unit => None,
        }
    }
}

/// What identifies an instance: its class, and the name at the front of its head.
#[derive(Debug, Clone, PartialEq)]
pub struct InstanceName {
    pub class: QualName,
    pub head: HeadName,
}

/// What an instance declares that another module can see: everything but its body.
#[derive(Debug, Clone, PartialEq)]
pub struct InstanceSignature {
    /// The class, named by the package and module that declared it.
    pub class: QualName,
    pub head: InstanceHead,
    /// The constraints written in front of the head's `=>`, each on a variable the head
    /// binds.
    pub context: Vec<Constraint>,
    /// The module that declared the instance. An instance that reaches a module by two
    /// routes is recognised as one by it.
    pub module: ModuleName,
    /// Where the head line was written, `instance` through `where`, in the declaring
    /// module's source.
    pub span: NodeSpan,
}

/// The body of an instance.
#[derive(Debug, PartialEq)]
pub enum InstanceBody {
    /// The body is the word `derived`.
    Derived,
    /// One binding per member, in the order written, each canonicalized as an ordinary
    /// value. A binding that did not canonicalize is left out, and its error reported.
    Bindings(Vec<Value>),
}

/// An `instance` declaration of the module under check.
#[derive(Debug, PartialEq)]
pub struct Instance {
    pub signature: InstanceSignature,
    pub body: InstanceBody,
}

/// An instance as an [`Interface`](crate::Interface) publishes it: its signature, and
/// where it was written when the interface that first published it knew its file.
#[derive(Debug, Clone, PartialEq)]
pub struct PublishedInstance {
    pub signature: InstanceSignature,
    pub source: Option<SourceSpan>,
}

impl PublishedInstance {
    /// Whether `self` and `other` are the same declaration, reached by two routes.
    pub(crate) fn same_declaration(&self, other: &PublishedInstance) -> bool {
        self.signature.class == other.signature.class
            && self.signature.head.name() == other.signature.head.name()
            && self.signature.module == other.signature.module
    }
}

/// Where a declaration an error refers to was written: in the module under check, in an
/// imported module whose file is known, or somewhere no label can honestly point at.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeclarationSite {
    InThisModule(NodeSpan),
    InImportedModule(SourceSpan),
    Unknown,
}

impl DeclarationSite {
    /// A secondary label under the declaration, when there is somewhere to put one.
    pub fn label(&self, message: String) -> Option<SpanLabel> {
        match self {
            DeclarationSite::InThisModule(span) => span.span().map(|span| SpanLabel {
                span,
                message,
                primary: false,
                file: None,
            }),
            DeclarationSite::InImportedModule(source) => Some(SpanLabel {
                span: source.span,
                message,
                primary: false,
                file: Some(source.file),
            }),
            DeclarationSite::Unknown => None,
        }
    }
}

/// The module `name` is declared in.
pub(crate) fn declaring_module(name: &QualName) -> ModuleName {
    ModuleName::new(name.package().clone(), name.module_name())
}

/// The class name and variable of a class head shaped `Comparable a`, or `None` for a
/// head shaped any other way.
fn class_head(head: &parser::Type) -> Option<(&Name, &Name)> {
    let parser::TypeKind::Unqualified(name, args) = &head.kind else {
        return None;
    };

    match args.as_slice() {
        [parser::Type {
            kind: parser::TypeKind::Variable(variable),
            ..
        }] => Some((name, variable)),
        _ => None,
    }
}

/// The member names a class declaration writes, each with where its signature was
/// written, in source order. Read off the declaration as parsed, so a class that fails
/// to canonicalize still says which names it would have declared.
pub(super) fn member_names(class: &parser::ClassDecl) -> impl Iterator<Item = (&Name, NodeSpan)> {
    class.members.iter().filter_map(|member| match member {
        parser::ClassMember::Signature(signature) => Some((&signature.name, signature.span)),
        parser::ClassMember::Derivation(_) => None,
    })
}

/// Register the name of every class `classes` declares in `env`, so a superclass or an
/// instance can name one written further down the file.
///
/// A class whose head is not a name applied to one variable, and one whose name a type
/// or an earlier class of the module already has, is reported and not registered. What
/// comes back is the index in `classes` of each class that was, with its name and
/// variable, beside the errors.
pub(super) fn declare_classes<'a>(
    env: &mut RootEnvironment,
    classes: &'a [parser::ClassDecl],
    types: &[parser::UnionType],
) -> (Vec<(usize, &'a Name, &'a Name)>, Vec<Error>) {
    let mut declared: Vec<(usize, &Name, &Name)> = Vec::new();
    let mut first: HashMap<&Name, NodeSpan> = HashMap::new();
    let mut errors = Vec::new();

    for (index, class) in classes.iter().enumerate() {
        let Some((name, variable)) = class_head(&class.head) else {
            errors.push(Error::InvalidClassHead(class.head.span));
            continue;
        };

        if let Some(tpe) = types.iter().find(|tpe| &tpe.name == name) {
            errors.push(Error::ClassNameTaken(
                name.clone(),
                NameTakenBy::Type,
                class.span,
                tpe.span,
            ));
            continue;
        }

        if let Some(earlier) = first.get(name) {
            errors.push(Error::ClassNameTaken(
                name.clone(),
                NameTakenBy::Class,
                class.span,
                *earlier,
            ));
            continue;
        }

        first.insert(name, class.span);
        env.insert_declared_class(name);
        declared.push((index, name, variable));
    }

    (declared, errors)
}

/// The signature of `class`, a class `declare_classes` registered as `name` over
/// `variable`, or every error that kept it from having one.
pub(super) fn class_signature(
    env: &RootEnvironment,
    class: &parser::ClassDecl,
    name: &Name,
    variable: &Name,
) -> Result<ClassSignature, Vec<Error>> {
    let mut errors = Vec::new();

    let superclasses = match constraints(
        env,
        class.context.as_ref(),
        &[variable],
        Error::ConstraintVariableUnbound,
    ) {
        Ok(superclasses) => superclasses,
        Err(context_errors) => {
            errors.extend(context_errors);
            Vec::new()
        }
    };

    let mut members = Vec::new();
    let mut first: HashMap<&Name, NodeSpan> = HashMap::new();

    for member in class.members.iter() {
        let parser::ClassMember::Signature(signature) = member else {
            continue;
        };

        if let Some(earlier) = first.get(&signature.name) {
            errors.push(Error::MemberDeclaredTwice(
                signature.name.clone(),
                signature.span,
                *earlier,
            ));
            continue;
        }
        first.insert(&signature.name, signature.span);

        if let Some(context) = &signature.context {
            errors.push(Error::MemberConstrained(
                signature.name.clone(),
                context.span,
            ));
        }

        if signature.marked_unsafe {
            errors.push(Error::MemberUnsafe(signature.name.clone(), signature.span));
        }

        match Type::from_parser_type(env, &signature.tpe) {
            Ok(tpe) if !mentions(&tpe, variable) => {
                errors.push(Error::MemberMissesClassVariable(
                    signature.name.clone(),
                    name.clone(),
                    variable.clone(),
                    signature.span,
                ));
            }
            Ok(tpe) => members.push(Member {
                name: signature.name.clone(),
                tpe,
                span: signature.span,
            }),
            Err(error) => errors.push(error),
        }
    }

    if errors.is_empty() {
        Ok(ClassSignature {
            variable: variable.clone(),
            superclasses,
            members,
            span: class.span,
        })
    } else {
        Err(errors)
    }
}

/// Whether the type variable `variable` occurs anywhere in `tpe`.
fn mentions(tpe: &Type, variable: &Name) -> bool {
    match tpe {
        Type::Variable(name) => name == variable,
        Type::Type(_, args) => args.iter().any(|arg| mentions(arg, variable)),
        Type::Record(fields) => fields.values().any(|field| mentions(field, variable)),
        Type::Arrow(param, result) => mentions(param, variable) || mentions(result, variable),
        Type::Tuple(tuple) => tuple.iter().any(|element| mentions(element, variable)),
        Type::Unit => false,
    }
}

/// Every type variable `tpe` mentions, each once, in the order first written.
///
/// Read off the type as parsed, so an annotation's context can be checked against its
/// type whether or not the type itself canonicalizes.
pub(super) fn written_variables(tpe: &parser::Type) -> Vec<&Name> {
    fn walk<'a>(tpe: &'a parser::Type, into: &mut Vec<&'a Name>) {
        match &tpe.kind {
            parser::TypeKind::Variable(name) => {
                if !into.contains(&name) {
                    into.push(name);
                }
            }
            parser::TypeKind::Unqualified(_, args) => args.iter().for_each(|arg| walk(arg, into)),
            parser::TypeKind::Arrow(param, result) => {
                walk(param, into);
                walk(result, into);
            }
            parser::TypeKind::Tuple(tuple) => tuple.iter().for_each(|element| walk(element, into)),
            parser::TypeKind::Record(fields) => {
                fields.iter().for_each(|field| walk(&field.value, into))
            }
            parser::TypeKind::Unit => {}
        }
    }

    let mut variables = Vec::new();
    walk(tpe, &mut variables);
    variables
}

/// The derivations of `class`, each binding canonicalized as an ordinary value.
///
/// A binding that does not canonicalize is left out and its error returned; every name
/// one could not resolve is pushed onto `unresolved`.
pub(super) fn derivations(
    env: &RootEnvironment,
    class: &parser::ClassDecl,
    unresolved: &mut Vec<Error>,
) -> (Vec<Derivation>, Vec<Error>) {
    let mut errors = Vec::new();

    let derivations = class
        .members
        .iter()
        .filter_map(|member| match member {
            parser::ClassMember::Derivation(derivation) => Some(derivation),
            parser::ClassMember::Signature(_) => None,
        })
        .map(|derivation| {
            let bindings = derivation
                .bindings
                .iter()
                .filter_map(|binding| match binding_value(env, binding, unresolved) {
                    Ok(value) => Some(value),
                    Err(error) => {
                        errors.push(error);
                        None
                    }
                })
                .collect();

            Derivation {
                member: derivation.member.clone(),
                bindings,
                span: derivation.span,
            }
        })
        .collect();

    (derivations, errors)
}

/// The constraints `context` writes, each resolved to a class and checked to be on one
/// of `bound`: the variables the head it stands in front of binds, or the variables the
/// type of the annotation it stands in front of mentions.
///
/// A constraint on a variable outside `bound` is reported as `unbound` builds it, from
/// the variable and where it was written, since what the variable is missing from is
/// the caller's to say. Every bad constraint of the context is reported, each at its
/// own span.
pub(super) fn constraints(
    env: &RootEnvironment,
    context: Option<&parser::Context>,
    bound: &[&Name],
    unbound: fn(Name, NodeSpan) -> Error,
) -> Result<Vec<Constraint>, Vec<Error>> {
    let Some(context) = context else {
        return Ok(Vec::new());
    };

    let shaped = validate_context(context)?;
    let mut constraints = Vec::new();
    let mut errors = Vec::new();

    for ((class, args), written) in shaped.into_iter().zip(context.constraints.iter()) {
        let resolved = env.find_class(class).cloned();
        if resolved.is_none() {
            errors.push(Error::ClassNotFound(class.clone(), written.span));
        }

        let variable = match args {
            [parser::Type {
                kind: parser::TypeKind::Variable(variable),
                span,
            }] => {
                if bound.contains(&variable) {
                    Some(variable.clone())
                } else {
                    errors.push(unbound(variable.clone(), *span));
                    None
                }
            }
            _ => {
                errors.push(Error::ConstraintNotOnVariable(written.span));
                None
            }
        };

        if let (Some(class), Some(variable)) = (resolved, variable) {
            constraints.push(Constraint {
                class,
                variable,
                span: written.span,
            });
        }
    }

    if errors.is_empty() {
        Ok(constraints)
    } else {
        Err(errors)
    }
}

/// The class and the head `instance` names, or every error that kept them from
/// resolving.
fn instance_head(
    env: &RootEnvironment,
    instance: &parser::InstanceDecl,
) -> Result<(QualName, InstanceHead), Vec<Error>> {
    let head = &instance.head;
    let parser::TypeKind::Unqualified(class, args) = &head.kind else {
        return Err(vec![Error::InvalidInstanceHead(
            InstanceHeadProblem::NotAClass,
            head.span,
        )]);
    };

    let [argument] = args.as_slice() else {
        return Err(vec![Error::InvalidInstanceHead(
            InstanceHeadProblem::ClassApplied(args.len()),
            head.span,
        )]);
    };

    let mut errors = Vec::new();
    let written = class;
    let class = env.find_class(written).cloned();
    if class.is_none() {
        errors.push(Error::ClassNotFound(written.clone(), head.span));
    }

    let tpe = instance_head_type(env, argument);
    match (class, tpe) {
        (Some(class), Ok(tpe)) if errors.is_empty() => Ok((class, tpe)),
        (_, tpe) => {
            errors.extend(tpe.err().into_iter().flatten());
            Err(errors)
        }
    }
}

/// The type an instance head applies its class to, held to the three forms a head may
/// take.
fn instance_head_type(
    env: &RootEnvironment,
    argument: &parser::Type,
) -> Result<InstanceHead, Vec<Error>> {
    match &argument.kind {
        parser::TypeKind::Unqualified(name, args) => {
            let Some(declared) = env.find_type(name) else {
                return Err(vec![Error::TypeNotFound(name.clone(), argument.span)]);
            };
            if declared.arity() != args.len() {
                return Err(vec![Error::TypeArityMismatch(
                    name.clone(),
                    declared.arity(),
                    args.len(),
                    argument.span,
                )]);
            }

            let name = declared.name.clone();
            Ok(InstanceHead::Type(name, distinct_variables(args.iter())?))
        }
        parser::TypeKind::Tuple(tuple) => {
            distinct_variables(tuple.iter())?;
            let tuple = tuple.try_map(|element| match &element.kind {
                parser::TypeKind::Variable(name) => Ok(name.clone()),
                _ => Err(vec![Error::InvalidInstanceHead(
                    InstanceHeadProblem::ArgumentNotVariable,
                    element.span,
                )]),
            })?;
            Ok(InstanceHead::Tuple(tuple))
        }
        parser::TypeKind::Unit => Ok(InstanceHead::Unit),
        parser::TypeKind::Variable(name) => Err(vec![Error::InvalidInstanceHead(
            InstanceHeadProblem::Variable(name.clone()),
            argument.span,
        )]),
        parser::TypeKind::Arrow(..) => Err(vec![Error::InvalidInstanceHead(
            InstanceHeadProblem::Function,
            argument.span,
        )]),
        parser::TypeKind::Record(..) => Err(vec![Error::InvalidInstanceHead(
            InstanceHeadProblem::Record,
            argument.span,
        )]),
    }
}

/// The names of `args`, each of which has to be a type variable and none of which may
/// repeat an earlier one. Every argument that breaks either rule is an error at its own
/// span.
fn distinct_variables<'a>(
    args: impl Iterator<Item = &'a parser::Type>,
) -> Result<Vec<Name>, Vec<Error>> {
    let mut seen: HashSet<&Name> = HashSet::new();
    let mut variables = Vec::new();
    let mut errors = Vec::new();

    for arg in args {
        match &arg.kind {
            parser::TypeKind::Variable(name) if seen.contains(name) => {
                errors.push(Error::InvalidInstanceHead(
                    InstanceHeadProblem::RepeatedVariable(name.clone()),
                    arg.span,
                ));
            }
            parser::TypeKind::Variable(name) => {
                seen.insert(name);
                variables.push(name.clone());
            }
            _ => errors.push(Error::InvalidInstanceHead(
                InstanceHeadProblem::ArgumentNotVariable,
                arg.span,
            )),
        }
    }

    if errors.is_empty() {
        Ok(variables)
    } else {
        Err(errors)
    }
}

/// What [`do_instances`] made of a module's instance declarations.
pub(super) struct Instances {
    /// Every instance that passed every check, in source order.
    pub instances: Vec<Instance>,
    /// Why each of the others did not, and every name a binding could not resolve.
    pub errors: Vec<Error>,
}

/// Canonicalize every `instance` declaration of a module, and hold each to the rules a
/// declaration of one has to follow.
///
/// Every head is resolved first, so that a superclass instance written further down the
/// file satisfies one written above it. Then each instance in turn: its context, its
/// bindings against its class's members, and the three rules about where it stands, in
/// the order a reader wants them — [the orphan
/// rule](../../../docs/spec/type-classes.md#where-an-instance-may-be-declared), the same
/// instance declared twice, and a superclass with no instance for the head.
///
/// A duplicate is looked for among the instances this module declares and every
/// instance its imports brought into scope. Where the orphan rule holds and imports
/// cannot form a cycle, only the module's own can collide, and the check does not lean
/// on either.
pub(super) fn do_instances(
    env: &RootEnvironment,
    instances: &[parser::InstanceDecl],
    unresolved: &mut Vec<Error>,
) -> Instances {
    let module = env.module_name().clone();
    let mut errors = Vec::new();

    let heads: Vec<Result<(QualName, InstanceHead), Vec<Error>>> = instances
        .iter()
        .map(|instance| instance_head(env, instance))
        .collect();

    // Every instance whose class and head resolved, own or imported, by what identifies
    // it. A superclass is satisfied by any of them. An imported instance that failed in
    // its own module is not among them: its interface is incomplete, and so is this
    // module's scope, which is what lets the caller drop the error that absence raises.
    let mut in_scope: HashSet<(QualName, HeadName)> = heads
        .iter()
        .filter_map(|head| head.as_ref().ok())
        .map(|(class, head)| (class.clone(), head.name()))
        .collect();
    in_scope.extend(env.imported_instances().iter().map(|published| {
        (
            published.signature.class.clone(),
            published.signature.head.name(),
        )
    }));

    let mut declared: Vec<(QualName, HeadName, NodeSpan)> = Vec::new();
    let mut kept = Vec::new();

    for (instance, head) in instances.iter().zip(heads) {
        // An instance whose class or head did not resolve has nothing to hold its
        // context or its bindings to.
        let (class, head) = match head {
            Ok(resolved) => resolved,
            Err(head_errors) => {
                errors.extend(head_errors);
                continue;
            }
        };
        let class = &class;

        let mut instance_errors = Vec::new();
        let signature = env.class_signature(class);

        let context = match constraints(
            env,
            instance.context.as_ref(),
            &head.variables(),
            Error::ConstraintVariableUnbound,
        ) {
            Ok(context) => context,
            Err(context_errors) => {
                instance_errors.extend(context_errors);
                Vec::new()
            }
        };

        let body = match &instance.body {
            parser::InstanceBody::Derived => InstanceBody::Derived,
            parser::InstanceBody::Bindings(bindings) => {
                let (values, binding_errors) =
                    instance_bindings(env, class, signature, instance, bindings, unresolved);
                instance_errors.extend(binding_errors);
                InstanceBody::Bindings(values)
            }
        };

        let head_name = head.name();

        let class_module = declaring_module(class);
        let head_module = head_name.module();
        if class_module != module && head_module.as_ref() != Some(&module) {
            instance_errors.push(Error::OrphanInstance(
                Box::new(InstanceName {
                    class: class.clone(),
                    head: head_name.clone(),
                }),
                instance.span,
            ));
        }

        let first = declared
            .iter()
            .find(|(c, h, _)| c == class && *h == head_name)
            .map(|(_, _, span)| DeclarationSite::InThisModule(*span))
            .or_else(|| {
                env.imported_instances()
                    .iter()
                    .find(|published| {
                        &published.signature.class == class
                            && published.signature.head.name() == head_name
                    })
                    .map(|published| match published.source {
                        Some(source) => DeclarationSite::InImportedModule(source),
                        None => DeclarationSite::Unknown,
                    })
            });
        if let Some(first) = first {
            instance_errors.push(Error::DuplicateInstance(
                Box::new(InstanceName {
                    class: class.clone(),
                    head: head_name.clone(),
                }),
                instance.span,
                first,
            ));
        }
        declared.push((class.clone(), head_name.clone(), instance.span));

        if let Some(signature) = signature {
            for superclass in &signature.superclasses {
                if !in_scope.contains(&(superclass.class.clone(), head_name.clone())) {
                    instance_errors.push(Error::MissingSuperclassInstance(
                        Box::new(InstanceName {
                            class: class.clone(),
                            head: head_name.clone(),
                        }),
                        superclass.class.clone(),
                        instance.span,
                    ));
                }
            }
        }

        if instance_errors.is_empty() {
            kept.push(Instance {
                signature: InstanceSignature {
                    class: class.clone(),
                    head: head.clone(),
                    context,
                    module: module.clone(),
                    span: instance.span,
                },
                body,
            });
        } else {
            errors.extend(instance_errors);
        }
    }

    Instances {
        instances: kept,
        errors,
    }
}

/// The bindings of `instance`, an instance of `class`, each canonicalized as an ordinary
/// value, beside the errors of every binding that names no member, binds one a second
/// time or does not canonicalize, and of every member nothing binds.
///
/// `signature` is `None` when `class` resolved and its declaration did not canonicalize;
/// there are then no members to hold the bindings to, and the failure that says so has
/// been reported.
fn instance_bindings(
    env: &RootEnvironment,
    class: &QualName,
    signature: Option<&ClassSignature>,
    instance: &parser::InstanceDecl,
    bindings: &[parser::FunBinding],
    unresolved: &mut Vec<Error>,
) -> (Vec<Value>, Vec<Error>) {
    let mut values = Vec::new();
    let mut errors = Vec::new();
    let mut first: HashMap<&Name, NodeSpan> = HashMap::new();
    let class_name = class.unqualified_name();

    for binding in bindings {
        if let Some(signature) = signature {
            if !signature.members.iter().any(|m| m.name == binding.name) {
                errors.push(Error::InstanceBindingNotMember(
                    binding.name.clone(),
                    class_name.clone(),
                    binding.span,
                ));
                continue;
            }
        }

        if let Some(earlier) = first.get(&binding.name) {
            errors.push(Error::InstanceMemberBoundTwice(
                binding.name.clone(),
                binding.span,
                *earlier,
            ));
            continue;
        }
        first.insert(&binding.name, binding.span);

        match binding_value(env, binding, unresolved) {
            Ok(value) => values.push(value),
            Err(error) => errors.push(error),
        }
    }

    if let Some(signature) = signature {
        for member in &signature.members {
            if !first.contains_key(&member.name) {
                errors.push(Error::InstanceMemberMissing(
                    member.name.clone(),
                    class_name.clone(),
                    instance.span,
                ));
            }
        }
    }

    (values, errors)
}
