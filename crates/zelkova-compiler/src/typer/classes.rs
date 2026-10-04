//! Class obligations: what a use of a constrained name asks of its types, and how the
//! answer is found once unification has run.
//!
//! A use of a name whose type has a context — a class member, whose context is its class,
//! or a function whose annotation wrote one — instantiates that context at the types the
//! use gave the name's variables. Each constraint becomes an [`Obligation`]: a class, a
//! type and an [`Origin`] naming the use. An obligation is not an equation, and `unify`
//! cannot answer it on sight: `Comparable t7` waits on what `t7` turns out to be. So
//! obligations are collected beside the equations (`constraint::Constraints`), left alone
//! while [`unify`](super::unifier::unify) and `read_fields` run, and [`discharge`]d
//! against the final substitution, in the order they were collected.
//!
//! # How an obligation is answered
//!
//! With the substitution applied, its type is one of three things.
//!
//! - **A declared type, a tuple or `()`.** [The head
//!   rule](../../../docs/spec/type-classes.md#what-an-instance-is-declared-for) makes the
//!   instance a lookup by the class and the name at the front of the type, with at most
//!   one answer. None is [`ErrorKind::NoInstance`]. An instance with a context asks more:
//!   its constraints, instantiated at the type's arguments, are obligations in their
//!   turn, so `Eq (Maybe Colour)` asks for `Eq Colour`. Each is on a strictly smaller
//!   type, so the asking stops.
//! - **A function type.** No instance can be declared for one, so the same error.
//! - **A record type.** Accepted, with no further obligation. A record is no instance's
//!   head, so there is no lookup to make; a class that carries a derivation walks a record
//!   field by field ([`records.md`](../../../docs/spec/records.md#records-and-derivation)),
//!   which [`LANG-85`](../../../docs/tickets/lang-85.md) implements. Until it does an
//!   obligation at a record is neither answered nor refused.
//! - **A variable.** It is answered if a *given* provides it, and otherwise it is left for
//!   the declaration to account for: see below.
//!
//! # What is given
//!
//! The context an annotation wrote — or an instance wrote, for the bindings of its body —
//! is given inside the declaration, and not proved. A given is the unification variable
//! `canonical_type_to_typer_type` made for the variable the constraint is on, with the
//! final substitution applied, and it answers an obligation on that same variable. A
//! superclass is implied by its subclass, transitively, so `Comparable a` provides `Eq a`.
//!
//! **A given goes with the variable.** If the body forces the variable to a concrete type,
//! the given is a given on that type and answers nothing: the obligation is answered by an
//! instance, or is [`ErrorKind::NoInstance`]. `min : Comparable a => a -> a -> a` over a
//! body that makes `a` an `Int` proves `Comparable Int` and publishes `Comparable a`. That
//! is the width of the hole every annotation has while its variables are flexible, and it
//! is [`LANG-12`](../../../docs/tickets/lang-12.md)'s to close; no partial check here
//! narrows it.
//!
//! # What a variable that nothing provides is
//!
//! An obligation still on a variable, with no given on it, is one of three errors, told
//! apart because the fix differs.
//!
//! - The variable is part of the declaration's type and the declaration has an annotation:
//!   the annotation is missing the constraint ([`ErrorKind::MissingConstraint`]).
//! - The variable is part of the declaration's type and the declaration has none: it
//!   needs one, because a constraint is never inferred
//!   ([`ErrorKind::ConstraintNeedsAnnotation`]).
//! - The variable is not part of the declaration's type: nothing determines the type the
//!   class is needed at, and no annotation on this declaration can
//!   ([`ErrorKind::UndeterminedConstraint`]).
//!
//! The obligations are read in the order they were collected, and the error reported is
//! the one whose obligation came first. A variable is not reported the moment it is met:
//! the error for a declaration with no annotation says everything the declaration has to
//! state, which takes every obligation on a variable of its type, so those are collected
//! first. A missing instance ends the reading at once unless a variable was met before it.

use super::{
    free_variables_in_order, ErrorKind, Origin, Reason, Substitution, Type, TypeLiteral,
    TypeVariable, VariableNames, Written,
};
use crate::canonical::{
    ClassSignature, HeadName, InstanceHead, InstanceSignature, Module as CanonicalModule,
};
use crate::ir::Predicate;
use crate::name::{Name, QualName};
use crate::{scalars, Interface};
use std::collections::HashMap;
use zelkova_syntax::position::NodeSpan;
use zelkova_syntax::tuple::Tuple;

/// A class required of a type, and the use that required it.
///
/// Kept in order, in a `Vec`, for the reason `Constraint` is: an unordered collection
/// would make which of several unsatisfiable obligations is reported vary between runs,
/// and a set would drop the provenance of the duplicates it collapsed.
#[derive(Debug, Clone)]
pub(super) struct Obligation {
    pub(super) class: QualName,
    pub(super) tpe: Type,
    pub(super) origin: Origin,
}

impl Obligation {
    pub(super) fn new(predicate: Predicate, reason: Reason, span: NodeSpan) -> Obligation {
        Obligation {
            class: predicate.class,
            tpe: predicate.tpe,
            origin: Origin::new(reason, span),
        }
    }
}

/// Every class and instance a module can see: its own, and the ones its imports bring.
///
/// A class is found by the name of its declaration, which is what a constraint and an
/// instance both carry; an instance by its class and the name at the front of its head,
/// which [identify an instance](crate::canonical::HeadName) exactly.
pub(super) struct ClassTable<'a> {
    classes: HashMap<QualName, &'a ClassSignature>,
    instances: HashMap<(QualName, HeadName), &'a InstanceSignature>,
}

impl<'a> ClassTable<'a> {
    /// The classes of `module` and of each interface it was checked against, and every
    /// instance in scope in `module`: its own and the ones its imports brought.
    ///
    /// An interface carries only the classes its module exposes, so a class that no
    /// import exposes is not here, and neither is the superclass of one that is, when its
    /// declaring module is not imported. That leaves a superclass of a superclass out of
    /// the givens in the one case where the class in the middle is exposed by nothing the
    /// module imports.
    pub(super) fn of(
        module: &'a CanonicalModule,
        interfaces: &'a HashMap<Name, Interface>,
    ) -> ClassTable<'a> {
        let mut classes = HashMap::new();
        for interface in interfaces.values() {
            for (name, signature) in &interface.classes {
                classes.insert(interface.module_name.qualify_name(name), signature);
            }
        }
        for (name, class) in &module.classes {
            classes.insert(module.name.qualify_name(name), &class.signature);
        }

        // The module's own instances go in last, so that were one reachable by both
        // routes, the declaration in front of the typer is the one that answers.
        let mut instances = HashMap::new();
        let published = module
            .imported_instances
            .iter()
            .map(|published| &published.signature);
        let own = module.instances.iter().map(|instance| &instance.signature);
        for signature in published.chain(own) {
            instances.insert((signature.class.clone(), signature.head.name()), signature);
        }

        ClassTable { classes, instances }
    }

    /// A table that knows no class and no instance, for a caller with nothing to discharge
    /// against.
    pub(super) fn empty() -> ClassTable<'a> {
        ClassTable {
            classes: HashMap::new(),
            instances: HashMap::new(),
        }
    }

    /// The signature of `class`, when a module in reach exposes it.
    pub(super) fn class(&self, class: &QualName) -> Option<&'a ClassSignature> {
        self.classes.get(class).copied()
    }

    /// `class` required of `tpe`, and every superclass that implies, added to `into`.
    fn provide(&self, class: &QualName, tpe: &Type, into: &mut Vec<(QualName, Type)>) {
        if into.iter().any(|(c, t)| c == class && t == tpe) {
            return;
        }
        into.push((class.clone(), tpe.clone()));

        if let Some(signature) = self.class(class) {
            for superclass in &signature.superclasses {
                self.provide(&superclass.class, tpe, into);
            }
        }
    }
}

/// What a declaration's obligations are discharged against: its own type, and what it
/// was given.
pub(super) struct Declared<'a> {
    /// The declaration's type as `annotate` built it, before the substitution. Whether a
    /// variable is part of it is what tells [`ErrorKind::MissingConstraint`] and
    /// [`ErrorKind::ConstraintNeedsAnnotation`] from
    /// [`ErrorKind::UndeterminedConstraint`].
    pub(super) tpe: &'a Type,
    /// Whether the declaration wrote its type: an annotation, or the member signature an
    /// instance's binding is held to.
    pub(super) annotated: bool,
    /// What the annotation (or the instance's context) required of its variables, each a
    /// class and the variable it is on.
    pub(super) given: &'a [(QualName, TypeVariable)],
    /// The names the source wrote for the variables it constrained, which a message
    /// writes them by.
    pub(super) names: &'a [(TypeVariable, Name)],
    /// What a missing constraint would have to be written in.
    pub(super) written: Written,
}

/// An obligation left on a variable that nothing given provides.
struct Residual {
    class: QualName,
    variable: TypeVariable,
    origin: Origin,
}

/// Answer every obligation of a declaration against the final `substitution`, in the order
/// they were collected, or report the first that has no answer.
pub(super) fn discharge(
    table: &ClassTable,
    obligations: Vec<Obligation>,
    substitution: &Substitution,
    declared: &Declared,
) -> Result<(), ErrorKind> {
    if obligations.is_empty() {
        return Ok(());
    }

    let mut given = Vec::new();
    for (class, variable) in declared.given {
        let tpe = substitution.apply_type(&Type::Variable(variable.clone()));
        table.provide(class, &tpe, &mut given);
    }

    let mut residuals: Vec<Residual> = Vec::new();

    for obligation in obligations {
        let predicate = Predicate {
            class: obligation.class,
            tpe: substitution.apply_type(&obligation.tpe),
        };

        // An instance that does not exist is reported where it is found, but only once
        // nothing earlier is left waiting: the first obligation in order is the one named.
        if let Err(error) = entail(
            table,
            &given,
            predicate,
            &obligation.origin,
            None,
            &mut residuals,
        ) {
            if residuals.is_empty() {
                return Err(error);
            }
            break;
        }
    }

    let Some(first) = residuals.first() else {
        return Ok(());
    };

    let signature = substitution.apply_type(declared.tpe);
    let in_type = free_variables_in_order(&signature);
    let names = names_for(declared, substitution, &in_type);
    let origin = Box::new(first.origin.clone());

    if !in_type.contains(&first.variable) {
        return Err(ErrorKind::UndeterminedConstraint {
            class: first.class.clone(),
            origin,
        });
    }

    if declared.annotated {
        return Err(ErrorKind::MissingConstraint {
            class: first.class.clone(),
            variable: name_of(&first.variable, &names),
            written: declared.written,
            origin,
        });
    }

    // What the declaration has to state is every constraint it was found to need, on the
    // variables its type holds, each once.
    let mut needed: Vec<(&QualName, &TypeVariable)> = Vec::new();
    for residual in &residuals {
        let pair = (&residual.class, &residual.variable);
        if in_type.contains(&residual.variable) && !needed.contains(&pair) {
            needed.push(pair);
        }
    }
    let constraints: Vec<String> = needed
        .iter()
        .map(|(class, variable)| {
            format!("{} {}", class.unqualified_name(), name_of(variable, &names))
        })
        .collect();
    let context = match constraints.as_slice() {
        [one] => one.clone(),
        many => format!("({})", many.join(", ")),
    };

    Err(ErrorKind::ConstraintNeedsAnnotation {
        class: first.class.clone(),
        stated: format!("{} => {}", context, super::WithNames(&signature, &names)),
        origin,
    })
}

/// Answer `predicate`, which has the final substitution applied, or push what it leaves
/// on a variable onto `residuals`.
///
/// `needed_by` is the obligation of the use itself when `predicate` is one of its
/// instance's constraints, so that an error says what asked for it.
fn entail(
    table: &ClassTable,
    given: &[(QualName, Type)],
    predicate: Predicate,
    origin: &Origin,
    needed_by: Option<&Predicate>,
    residuals: &mut Vec<Residual>,
) -> Result<(), ErrorKind> {
    let no_instance = |predicate: &Predicate| ErrorKind::NoInstance {
        class: predicate.class.clone(),
        tpe: Box::new(predicate.tpe.clone()),
        needed_by: needed_by.map(|required| Box::new(required.clone())),
        origin: Box::new(origin.clone()),
    };

    let variable = match &predicate.tpe {
        Type::Variable(variable) => variable,
        // No instance can be declared for a function type, by the head rule.
        Type::Fun { .. } => return Err(no_instance(&predicate)),
        // A record is no instance's head, so there is nothing to look up. A class with a
        // derivation walks a record's fields (`docs/spec/records.md`, LANG-85), and nothing
        // does yet: the obligation is accepted, asks nothing further and raises no error.
        Type::Record(_) => return Ok(()),
        _ => {
            let Some((head, arguments)) = head_of(&predicate.tpe) else {
                return Err(no_instance(&predicate));
            };
            let Some(instance) = table.instances.get(&(predicate.class.clone(), head)) else {
                return Err(no_instance(&predicate));
            };

            let variables = instance.head.variables();
            if variables.len() != arguments.len() {
                return Err(no_instance(&predicate));
            }

            // What the instance asks of the type's arguments, in turn. Each is asked at
            // the span of the use that started it, and says which obligation of that use
            // it is a part of.
            let derived = Origin::new(Reason::InstanceContext, origin.span);
            let top = needed_by.unwrap_or(&predicate);
            for constraint in &instance.context {
                let Some(position) = variables
                    .iter()
                    .position(|variable| **variable == constraint.variable)
                else {
                    continue;
                };

                let Some(argument) = arguments.get(position) else {
                    continue;
                };
                let required = Predicate {
                    class: constraint.class.clone(),
                    tpe: argument.clone(),
                };
                entail(table, given, required, &derived, Some(top), residuals)?;
            }

            return Ok(());
        }
    };

    if given
        .iter()
        .any(|(class, tpe)| class == &predicate.class && tpe == &predicate.tpe)
    {
        return Ok(());
    }

    residuals.push(Residual {
        class: predicate.class,
        variable: variable.clone(),
        origin: origin.clone(),
    });

    Ok(())
}

/// The name at the front of `tpe` an instance is looked up by, and the types that name is
/// applied to — the ones an instance's head variables stand for. `None` for a type no
/// instance can be declared for.
fn head_of(tpe: &Type) -> Option<(HeadName, Vec<Type>)> {
    match tpe {
        Type::Literal(literal) => {
            let scalar = match literal {
                TypeLiteral::Int => scalars::INT,
                TypeLiteral::Float => scalars::FLOAT,
                TypeLiteral::Char => scalars::CHAR,
                TypeLiteral::String => scalars::STRING,
            };
            Some((HeadName::Type(scalar.qual_name()), Vec::new()))
        }
        Type::Adt(name, arguments) => Some((HeadName::Type(name.clone()), arguments.clone())),
        Type::Tuple(Tuple::Two(a, b)) => Some((
            HeadName::TwoTuple,
            vec![a.as_ref().clone(), b.as_ref().clone()],
        )),
        Type::Tuple(Tuple::Three(a, b, c)) => Some((
            HeadName::ThreeTuple,
            vec![a.as_ref().clone(), b.as_ref().clone(), c.as_ref().clone()],
        )),
        Type::Unit => Some((HeadName::Unit, Vec::new())),
        Type::Variable(_) | Type::Fun { .. } | Type::Record(_) => None,
    }
}

/// The name a message writes each variable of the declaration's type by: the one its
/// annotation wrote where it wrote one, and otherwise a letter no annotation variable
/// has.
///
/// The names an annotation wrote belong to the variables it was translated to, and a
/// variable of the solved type is what one of those was solved to.
fn names_for(
    declared: &Declared,
    substitution: &Substitution,
    in_type: &[TypeVariable],
) -> VariableNames {
    let mut names = VariableNames::new();

    for (variable, name) in declared.names {
        if let Type::Variable(solved) = substitution.apply_type(&Type::Variable(variable.clone())) {
            names.entry(solved).or_insert_with(|| name.to_string());
        }
    }

    let mut next = 0;
    for variable in in_type {
        if names.contains_key(variable) {
            continue;
        }
        let name = loop {
            let candidate = letter(next);
            next += 1;
            if !names.values().any(|taken| taken == &candidate) {
                break candidate;
            }
        };
        names.insert(variable.clone(), name);
    }

    names
}

/// The `n`th variable name: `a` to `z`, then `a1`, `b1`, and on.
fn letter(n: usize) -> String {
    let letter = char::from(b'a' + (n % 26) as u8);
    match n / 26 {
        0 => letter.to_string(),
        round => format!("{}{}", letter, round),
    }
}

/// `variable` as `names` writes it, or as an inference variable when it has no name.
fn name_of(variable: &TypeVariable, names: &VariableNames) -> String {
    names
        .get(variable)
        .cloned()
        .unwrap_or_else(|| format!("t{}", variable.id))
}

/// The type an instance is for, as the typer reads it: the head's declared type applied
/// to one variable per parameter, a tuple of variables, or `()`. Each of the head's
/// variables is entered into `variables` under its written name.
pub(crate) fn instance_head_type(
    head: &InstanceHead,
    variables: &mut HashMap<String, TypeVariable>,
    counter: &mut u32,
) -> Type {
    let variable = |name: &Name| crate::canonical::Type::Variable(name.clone());

    let canonical = match head {
        InstanceHead::Type(name, parameters) => {
            crate::canonical::Type::Type(name.clone(), parameters.iter().map(variable).collect())
        }
        InstanceHead::Tuple(tuple) => {
            crate::canonical::Type::Tuple(tuple.map(|name| variable(name)))
        }
        InstanceHead::Unit => crate::canonical::Type::Unit,
    };

    // Every form of a canonical type translates, and a head is built from the three that
    // can stand in one, so the fallback is a variable no one uses.
    super::canonical_type_to_typer_type(&canonical, variables, counter).unwrap_or_else(|| {
        *counter += 1;
        Type::Variable(TypeVariable { id: *counter })
    })
}
