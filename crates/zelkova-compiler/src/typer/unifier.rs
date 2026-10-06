//! Solve a list of constraints, and blame the one that could not be solved.
//!
//! Unification itself is unchanged by `ERR-4`: what is new is that a constraint
//! arrives carrying an [`Origin`], every constraint this module derives from another
//! inherits it, and the two errors raised here hand it to the caller. That is the
//! whole of "propagate the origin of the constraint it failed on".
//!
//! [`read_fields`] is the second step, run once [`unify`] has solved every equation of a
//! declaration: it reads the field constraints an access, an update and an accessor
//! wrote, and raises the three errors about a record's use — see `FieldConstraint`.
//!
//! The one place unification's symmetry is broken is [`unify_variable`], which is
//! told which *side* of the constraint the type it is solving to was read from. That
//! is not inference — the solution is the same either way — it is what lets a
//! solution record where its type actually came from.

use log::debug;

use super::{
    occurs, Binder, Constraint, ErrorKind, FieldConstraint, Side, Substitution, Supplier, Type,
    TypeLiteral, TypeVariable,
};
use std::collections::HashSet;
use zelkova_syntax::tuple::Tuple;

/// Solve the constraints in order, applying each solution to the ones still to come.
///
/// The order is the caller's and it matters: it decides which of several
/// unsatisfiable constraints is the one reported, and — through
/// `Substitution::apply` — which constraint ends up being named as the *explanation*
/// for a type. `infer_annotated` relies on that by putting the annotation first.
pub(super) fn unify(constraints: Vec<Constraint>) -> Result<Substitution, ErrorKind> {
    debug!("unify: {:?}", constraints);
    let mut iter = constraints.into_iter();

    match iter.next() {
        None => Ok(Substitution::empty()),
        Some(first) => {
            let sub_head = unify_one_constraint(&first)?;

            // Apply this substitution to the remaining constraints
            let constraints_tail: Vec<_> = iter.map(|c| sub_head.apply(&c)).collect();

            // Then recursively unify the substituted constraints
            let sub_tail = unify(constraints_tail)?;

            // And finally merged the unified substitution with the first one
            Ok(sub_head.merge(sub_tail))
        }
    }
}

fn unify_one_constraint(constraint: &Constraint) -> Result<Substitution, ErrorKind> {
    let Constraint { left, right, .. } = constraint;
    debug!("unify_one_constraint: {:?} to {:?}", left, right);
    match (left, right) {
        (Type::Literal(TypeLiteral::Int), Type::Literal(TypeLiteral::Int)) => {
            Ok(Substitution::empty())
        }
        (Type::Literal(TypeLiteral::Char), Type::Literal(TypeLiteral::Char)) => {
            Ok(Substitution::empty())
        }
        (Type::Literal(TypeLiteral::Float), Type::Literal(TypeLiteral::Float)) => {
            Ok(Substitution::empty())
        }
        (Type::Literal(TypeLiteral::String), Type::Literal(TypeLiteral::String)) => {
            Ok(Substitution::empty())
        }
        (Type::Unit, Type::Unit) => Ok(Substitution::empty()),
        // A constraint between two compound types decomposes into constraints between
        // their components, and each of those keeps this constraint's origin: they are
        // about the same text, required for the same reason. Left stays left, so the
        // invariant that `left` is the type of the text at the span survives the
        // decomposition.
        (
            Type::Fun {
                param_tpe: p1,
                return_tpe: r1,
            },
            Type::Fun {
                param_tpe: p2,
                return_tpe: r2,
            },
        ) => unify(vec![
            constraint.component(*p1.clone(), *p2.clone()),
            constraint.component(*r1.clone(), *r2.clone()),
        ]),
        // Tuples: unify element-by-element. A `Two` against a `Three` matches
        // neither arm below and falls through to the mismatch arm at the
        // bottom, same as any other `Type` mismatch.
        (Type::Tuple(Tuple::Two(a1, b1)), Type::Tuple(Tuple::Two(a2, b2))) => unify(vec![
            constraint.component(*a1.clone(), *a2.clone()),
            constraint.component(*b1.clone(), *b2.clone()),
        ]),
        (Type::Tuple(Tuple::Three(a1, b1, c1)), Type::Tuple(Tuple::Three(a2, b2, c2))) => {
            unify(vec![
                constraint.component(*a1.clone(), *a2.clone()),
                constraint.component(*b1.clone(), *b2.clone()),
                constraint.component(*c1.clone(), *c2.clone()),
            ])
        }
        // ADT types: must have same name and same number of args; unify args pairwise
        (Type::Adt(n1, args1), Type::Adt(n2, args2)) if n1 == n2 && args1.len() == args2.len() => {
            let constraints = args1
                .iter()
                .zip(args2.iter())
                .map(|(a, b)| constraint.component(a.clone(), b.clone()))
                .collect();
            unify(constraints)
        }
        // Records: the same label set, then each label's two field types pairwise, in
        // label order. A label one side has and the other lacks is a mismatch of the
        // two whole types, as a `Two` against a `Three` is — there is no row variable
        // for the missing fields to be solved into (see `Type::Record`).
        (Type::Record(fields1), Type::Record(fields2))
            if fields1.len() == fields2.len()
                && fields1.keys().all(|label| fields2.contains_key(label)) =>
        {
            let constraints = fields1
                .iter()
                .filter_map(|(label, tpe1)| {
                    let tpe2 = fields2.get(label)?;
                    Some(constraint.component(tpe1.clone(), tpe2.clone()))
                })
                .collect();
            unify(constraints)
        }
        // A rigid variable is equal to itself and, below, to nothing else.
        (Type::Variable(left), Type::Variable(right)) if left == right => Ok(Substitution::empty()),
        // `Side` names where `tpe` was read from, not where the variable was: it is
        // the solved *type* whose provenance the solution carries. A flexible variable
        // is solved to whatever it meets, a rigid one included; the guard is what keeps
        // a rigid one from being solved at all.
        (Type::Variable(tvar), tpe) if !tvar.is_rigid() => {
            unify_variable(tvar, tpe, Side::Right, constraint)
        }
        (tpe, Type::Variable(tvar)) if !tvar.is_rigid() => {
            unify_variable(tvar, tpe, Side::Left, constraint)
        }
        // What is left has a rigid variable on one side, and on the other anything that
        // is not a flexible variable and not that same variable: a type it would have to
        // be solved to, or another rigid variable.
        (Type::Variable(rigid), tpe) | (tpe, Type::Variable(rigid)) => {
            Err(ErrorKind::RigidVariable {
                variable: rigid.spelling(),
                // The guards above leave only a rigid variable here.
                binder: rigid.binder().unwrap_or(Binder::Annotation),
                tpe: Box::new(tpe.clone()),
                origin: Box::new(constraint.origin.clone()),
            })
        }
        (left, right) => Err(ErrorKind::UnificationFailed {
            left: Box::new(left.clone()),
            right: Box::new(right.clone()),
            origin: Box::new(constraint.origin.clone()),
        }),
    }
}

/// Read every [`FieldConstraint`] of a declaration against `substitution`, the solution
/// of all of its equations, and hand back that solution extended by what reading them
/// solved.
///
/// The constraints are read in passes, in the order given; a pass reads every one still
/// undecided, and each sees what the ones before it solved. One whose record type is a
/// record becomes an equation, solved here and merged in; one whose record type is
/// still a variable waits for the next pass. A pass that decides nothing ends the
/// reading, and one of the constraints still waiting is the error — so every pass either
/// shrinks what is left or is the last, and none repeats. `FieldConstraint` has the
/// design this implements.
///
/// Which constraint still waiting is the error, whether a hole explains them all so
/// that none is, and what its note says, are [`unknown`]'s to decide from `declaration`.
pub(super) fn read_fields(
    substitution: Substitution,
    fields: Vec<FieldConstraint>,
    declaration: &Declaration,
) -> Result<Substitution, ErrorKind> {
    let mut substitution = substitution;
    let mut pending = fields;

    while !pending.is_empty() {
        let before = pending.len();
        let mut waiting = Vec::new();

        for field in pending {
            match field.read(&substitution)? {
                Some(equation) => {
                    let solved = unify(vec![equation])?;
                    substitution = substitution.merge(solved);
                }
                None => waiting.push(field),
            }
        }

        if waiting.len() == before {
            // Nothing this pass decided can change what the next one would read.
            return match unknown(&waiting, &substitution, declaration) {
                Some(error) => Err(error),
                None => Ok(substitution),
            };
        }

        pending = waiting;
    }

    Ok(substitution)
}

/// What [`read_fields`] needs to know of the declaration beside its field constraints,
/// to say which waiting one is the error and what would supply its record type.
pub(super) struct Declaration<'a> {
    /// The type of every hole in the declaration (`constraint::Constraints::holes`).
    pub(super) holes: &'a [Type],
    /// The declaration's own type, before the substitution: its annotation's, once
    /// solved, when it has one.
    pub(super) declared: &'a Type,
    /// Whether the declaration has an annotation.
    pub(super) annotated: bool,
}

/// The error `waiting` is — the field constraints a pass of [`read_fields`] decided none
/// of — or `None` when a hole explains every one of them.
///
/// **What a hole explains.** A hole is a name that did not resolve, and its own error
/// is already reported; a constraint whose record type the name's real type could
/// have supplied only restates it, and is dropped
/// ([`DEC-23` decision 3](../../../docs/decisions/dec-23.md#3--an-error-that-restates-a-reported-failure-is-dropped-by-a-flag-on-the-scope)).
/// That is a constraint whose record type is [`reach`]ed from the holes' types: one of
/// their variables, as in `missing.x` or `(missing z).x`, or the field type of a use
/// that is, as in `missing.x.y`. Every other constraint is still reported, holes or no
/// holes in the declaration, because a hole "takes whatever type its surroundings
/// require and the rest of the body is checked"
/// ([decision 6](../../../docs/decisions/dec-23.md#6--an-unresolved-name-inside-a-sound-body-is-a-typed-hole)):
/// in `first missing person.name`, nothing the name could resolve to would say what
/// `person` is.
///
/// **Which one is the error.** Of the constraints left, the first one whose record type
/// no *other* of them reaches as part of its field type — the root a chain waits on:
/// `r.a` in both `r.a.b` and `.b r.a`, though `.b` is collected first in the second.
/// If every one is reached by another, the first. Only that one is reported, and the
/// others surface once it is fixed — as `unify` stops at a declaration's first failed
/// equation, so a declaration is rejected for one error.
///
/// **What its note says** is [`Supplier`]: whether the record type is reached from the
/// declaration's own type, so that an annotation on the declaration could supply it.
fn unknown(
    waiting: &[FieldConstraint],
    substitution: &Substitution,
    declaration: &Declaration,
) -> Option<ErrorKind> {
    let record_of = |field: &FieldConstraint| match substitution.apply_type(&field.record) {
        Type::Variable(tvar) => Some(tvar),
        _ => None,
    };

    let explained = reach(declaration.holes, waiting, substitution);
    let unexplained: Vec<(usize, &FieldConstraint)> = waiting
        .iter()
        .enumerate()
        .filter(|(_, field)| match record_of(field) {
            Some(record) => !explained.contains(&record),
            None => true,
        })
        .collect();

    let fed = |at: usize, record: &TypeVariable| {
        unexplained.iter().any(|(other, field)| {
            let mut variables = HashSet::new();
            free_variables(&substitution.apply_type(&field.field), &mut variables);
            *other != at && variables.contains(record)
        })
    };
    let (_, blamed) = unexplained
        .iter()
        .find(|(at, field)| match record_of(field) {
            Some(record) => !fed(*at, &record),
            None => true,
        })
        .or(unexplained.first())?;

    let declared = reach(
        std::slice::from_ref(declaration.declared),
        waiting,
        substitution,
    );
    let supplier = match record_of(blamed) {
        Some(record) if declared.contains(&record) => {
            if declaration.annotated {
                Supplier::AnnotationVariable
            } else {
                Supplier::Annotation
            }
        }
        _ => Supplier::Body,
    };

    Some(blamed.unknown(supplier))
}

/// The variables of `seeds` once solved, and every variable a waiting constraint would
/// solve if the type it reads were supplied: the field type of each constraint whose
/// record type is one of them, and so on until nothing is added.
///
/// Supplying a type for any of these variables decides exactly the constraints whose
/// record types are among them, and no other: that is the question [`unknown`] asks
/// twice, of the holes' types and of the declaration's.
///
/// Each round adds at least one variable or is the last, and there are only so many
/// variables in `waiting`'s field types, so the rounds stop.
fn reach(
    seeds: &[Type],
    waiting: &[FieldConstraint],
    substitution: &Substitution,
) -> HashSet<TypeVariable> {
    let mut reached = HashSet::new();
    for seed in seeds {
        free_variables(&substitution.apply_type(seed), &mut reached);
    }

    loop {
        let before = reached.len();
        for field in waiting {
            if let Type::Variable(record) = substitution.apply_type(&field.record) {
                if reached.contains(&record) {
                    free_variables(&substitution.apply_type(&field.field), &mut reached);
                }
            }
        }
        if reached.len() == before {
            return reached;
        }
    }
}

/// Every type variable in `tpe`, added to `into`.
fn free_variables(tpe: &Type, into: &mut HashSet<TypeVariable>) {
    match tpe {
        Type::Literal(_) | Type::Unit => (),
        Type::Variable(tvar) => {
            into.insert(tvar.clone());
        }
        Type::Fun {
            param_tpe,
            return_tpe,
        } => {
            free_variables(param_tpe, into);
            free_variables(return_tpe, into);
        }
        Type::Tuple(tuple) => tuple.iter().for_each(|t| free_variables(t, into)),
        Type::Adt(_, args) => args.iter().for_each(|t| free_variables(t, into)),
        Type::Record(fields) => fields.values().for_each(|t| free_variables(t, into)),
    }
}

/// Solve `tvar` to `tpe`, remembering on the solution where `tpe` came from.
///
/// That cause is what a *later* constraint reports as the explanation for a type it
/// never mentioned itself — see [`super::Origin::left_from`]. It is read off the side
/// `tpe` sits on, so a constraint that was merely handed this type by an earlier
/// substitution passes the credit on instead of taking it — see
/// [`super::Origin::cause_of`].
fn unify_variable(
    tvar: &TypeVariable,
    tpe: &Type,
    side: Side,
    constraint: &Constraint,
) -> Result<Substitution, ErrorKind> {
    let cause = constraint.origin.cause_of(side);

    match tpe {
        Type::Variable(tvar2) => {
            if tvar == tvar2 {
                Ok(Substitution::empty())
            } else {
                Ok(Substitution::one(tvar.clone(), tpe.clone(), cause))
            }
        }
        _ => {
            if occurs(tvar, tpe) {
                Err(ErrorKind::CircularType {
                    tpe: Box::new(tpe.clone()),
                    origin: Box::new(constraint.origin.clone()),
                })
            } else {
                Ok(Substitution::one(tvar.clone(), tpe.clone(), cause))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typer::*;
    use zelkova_syntax::position::NodeSpan;

    /// These tests are about unification, not about provenance: every constraint gets
    /// the same reason and no position, so the assertions turn on the types alone.
    fn constraint(left: Type, right: Type) -> Constraint {
        Constraint::new(left, right, Reason::Annotation, NodeSpan::none())
    }

    /// The cause every solution below carries, for the same reason: these tests are
    /// about types, so the provenance is uniform and drops out of the comparison.
    fn cause() -> Cause {
        Cause {
            reason: Reason::Annotation,
            span: NodeSpan::none(),
        }
    }

    #[test]
    fn unifies_ints() {
        let constraints = vec![constraint(
            Type::Literal(TypeLiteral::Int),
            Type::Literal(TypeLiteral::Int),
        )];

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    /// A `Bool` is an ordinary union here like any other, so this goes through the
    /// `Adt`/`Adt` arm and matches on the qualified name rather than on an arm of its
    /// own.
    #[test]
    fn unifies_bools() {
        let constraints = vec![constraint(bool_type(), bool_type())];

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    /// Two `Bool`s declared in different modules are two types, which is what an `if`
    /// on a module's own `type Bool` runs into.
    ///
    /// Mutation-checked by comparing only the unqualified halves in the `Adt`/`Adt`
    /// arm (`n1.unqualified_name() == n2.unqualified_name()`): the two unify and the
    /// assertion goes red.
    #[test]
    fn a_bool_from_another_module_is_a_different_type() {
        let local_bool = Type::Adt(
            crate::name::QualName::in_module(crate::PackageName::core(), "Example", "Bool"),
            vec![],
        );
        let constraints = vec![constraint(local_bool, bool_type())];

        match unify(constraints) {
            Err(ErrorKind::UnificationFailed { left, right, .. }) => {
                assert_eq!(format!("{}", left), "Bool");
                assert_eq!(format!("{}", right), "Bool");
            }
            other => panic!("expected a unification failure, got {:?}", other),
        }
    }

    /// `Basics.Bool` declared by a package other than `zelkova-core` is a third type:
    /// the package is as much a part of a union's identity as its module.
    ///
    /// Mutation-checked by leaving the package out of `QualName`'s equality (deriving
    /// `PartialEq` by hand over `module` and `name` only): the two unify and the
    /// assertion goes red.
    #[test]
    fn a_bool_from_another_package_is_a_different_type() {
        let rival_bool = Type::Adt(
            crate::name::QualName::in_module(
                crate::PackageName::new("acme-basics").unwrap(),
                "Basics",
                "Bool",
            ),
            vec![],
        );
        let constraints = vec![constraint(rival_bool, bool_type())];

        assert!(matches!(
            unify(constraints),
            Err(ErrorKind::UnificationFailed { .. })
        ));
    }

    #[test]
    fn unifies_functions() {
        let fun = Type::Fun {
            param_tpe: Box::new(bool_type()),
            return_tpe: Box::new(bool_type()),
        };
        let constraints = vec![constraint(fun.clone(), fun.clone())];

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    #[test]
    fn unifies_variables() {
        let tvar1 = TypeVariable::flexible(1);
        let t1 = Type::Variable(tvar1.clone());
        let t2 = Type::Variable(TypeVariable::flexible(2));

        let constraints = vec![constraint(t1, t2.clone())];

        assert_eq!(
            unify(constraints).unwrap(),
            Substitution::one(tvar1, t2, cause())
        );
    }

    #[test]
    fn unifies_variable_with_literal() {
        let tvar1 = TypeVariable::flexible(1);
        let t1 = Type::Variable(tvar1.clone());
        let t2 = Type::Literal(TypeLiteral::Int);

        let constraints = vec![constraint(t1, t2.clone())];

        assert_eq!(
            unify(constraints).unwrap(),
            Substitution::one(tvar1, t2, cause())
        );
    }

    #[test]
    fn unifies_variables_in_functions() {
        let tvar1 = TypeVariable::flexible(1);
        let tvar2 = TypeVariable::flexible(2);

        let constraints = vec![constraint(
            // tvar1 -> bool
            Type::Fun {
                param_tpe: Box::new(Type::Variable(tvar1.clone())),
                return_tpe: Box::new(bool_type()),
            },
            // int -> tvar2
            Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(Type::Variable(tvar2.clone())),
            },
        )];

        let sub = Substitution::one(tvar2, bool_type(), cause()).merge(Substitution::one(
            tvar1,
            Type::Literal(TypeLiteral::Int),
            cause(),
        ));

        assert_eq!(unify(constraints).unwrap(), sub);
    }

    /// A failed unification reports the origin of the constraint that failed, and a
    /// constraint decomposed into its components passes that origin down: the
    /// mismatch here is between the two functions' *return* types, two levels below
    /// the constraint the caller wrote.
    ///
    /// Mutation-checked by having the `Fun`/`Fun` arm build its component constraints
    /// with `Constraint::new(.., Reason::Literal, NodeSpan::none())` instead of
    /// `constraint.component(..)`: the reason assertion goes red.
    #[test]
    fn a_failure_inside_a_function_type_keeps_the_whole_constraint_reason() {
        let constraints = vec![Constraint::new(
            Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(bool_type()),
            },
            Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(Type::Literal(TypeLiteral::Char)),
            },
            Reason::IfBranch,
            NodeSpan::none(),
        )];

        match unify(constraints) {
            Err(ErrorKind::UnificationFailed {
                left,
                right,
                origin,
            }) => {
                assert_eq!(format!("{}", left), "Bool");
                assert_eq!(format!("{}", right), "Char");
                assert_eq!(origin.reason, Reason::IfBranch);
            }
            other => panic!("expected a unification failure, got {:?}", other),
        }
    }

    /// The solution of one constraint explains the next: `t1 := Bool` comes from the
    /// first constraint, and the second one — which mentioned only `t1` and `Int` —
    /// fails with the first one named as the reason `Bool` is there at all.
    ///
    /// Mutation-checked by making `Substitution::apply` return `c.origin.clone()`
    /// unchanged: the explanation is then `None` and the assertion goes red.
    #[test]
    fn a_substituted_type_is_explained_by_the_constraint_that_solved_it() {
        let t1 = Type::Variable(TypeVariable::flexible(1));

        let constraints = vec![
            Constraint::new(
                bool_type(),
                t1.clone(),
                Reason::Annotation,
                NodeSpan::none(),
            ),
            Constraint::new(
                t1,
                Type::Literal(TypeLiteral::Int),
                Reason::IfBranch,
                NodeSpan::none(),
            ),
        ];

        match unify(constraints) {
            Err(ErrorKind::UnificationFailed { origin, .. }) => {
                assert_eq!(origin.reason, Reason::IfBranch);
                assert_eq!(
                    origin.explanation().map(|c| c.reason),
                    Some(Reason::Annotation),
                    "the annotation is where `Bool` came from"
                );
            }
            other => panic!("expected a unification failure, got {:?}", other),
        }
    }
}
