//! Solve a list of constraints, and blame the one that could not be solved.
//!
//! Unification itself is unchanged by `ERR-4`: what is new is that a constraint
//! arrives carrying an [`Origin`], every constraint this module derives from another
//! inherits it, and the two errors raised here hand it to the caller. That is the
//! whole of "propagate the origin of the constraint it failed on".

use log::debug;

use super::{occurs, Constraint, ErrorKind, Substitution, Type, TypeLiteral, TypeVariable};
use crate::compiler::tuple::Tuple;

/// Returns true if `tpe` is a numeric type (Int, Float, or Number).
fn is_numeric(tpe: &Type) -> bool {
    matches!(
        tpe,
        Type::Literal(TypeLiteral::Int) | Type::Literal(TypeLiteral::Float) | Type::Number
    )
}

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
        (Type::Literal(TypeLiteral::Bool), Type::Literal(TypeLiteral::Bool)) => {
            Ok(Substitution::empty())
        }
        (Type::Literal(TypeLiteral::Int), Type::Literal(TypeLiteral::Int)) => {
            Ok(Substitution::empty())
        }
        (Type::Literal(TypeLiteral::Char), Type::Literal(TypeLiteral::Char)) => {
            Ok(Substitution::empty())
        }
        (Type::Literal(TypeLiteral::Float), Type::Literal(TypeLiteral::Float)) => {
            Ok(Substitution::empty())
        }
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
        // Number unifies with Int, Float, or another Number (but not Bool, Char, etc.)
        (Type::Number, other) | (other, Type::Number) if is_numeric(other) => {
            Ok(Substitution::empty())
        }
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
        (Type::Variable(tvar), tpe) => unify_variable(tvar, tpe, constraint),
        (tpe, Type::Variable(tvar)) => unify_variable(tvar, tpe, constraint),
        (left, right) => Err(ErrorKind::UnificationFailed {
            left: left.clone(),
            right: right.clone(),
            origin: constraint.origin.clone(),
        }),
    }
}

/// Solve `tvar` to `tpe`, remembering on the solution which constraint solved it.
///
/// That origin is what a *later* constraint reports as the explanation for a type it
/// never mentioned itself — see [`super::Origin::because`].
fn unify_variable(
    tvar: &TypeVariable,
    tpe: &Type,
    constraint: &Constraint,
) -> Result<Substitution, ErrorKind> {
    match tpe {
        Type::Variable(tvar2) => {
            if tvar == tvar2 {
                Ok(Substitution::empty())
            } else {
                Ok(Substitution::one(
                    tvar.clone(),
                    tpe.clone(),
                    constraint.origin.clone(),
                ))
            }
        }
        _ => {
            if occurs(tvar, tpe) {
                Err(ErrorKind::CircularType {
                    tpe: tpe.clone(),
                    origin: constraint.origin.clone(),
                })
            } else {
                Ok(Substitution::one(
                    tvar.clone(),
                    tpe.clone(),
                    constraint.origin.clone(),
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::position::NodeSpan;
    use crate::compiler::typer::*;

    /// These tests are about unification, not about provenance: every constraint gets
    /// the same reason and no position, so the assertions turn on the types alone.
    fn constraint(left: Type, right: Type) -> Constraint {
        Constraint::new(left, right, Reason::Annotation, NodeSpan::none())
    }

    fn origin() -> Origin {
        Origin::new(Reason::Annotation, NodeSpan::none())
    }

    #[test]
    fn unifies_ints() {
        let constraints = vec![constraint(
            Type::Literal(TypeLiteral::Int),
            Type::Literal(TypeLiteral::Int),
        )];

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    #[test]
    fn unifies_bools() {
        let constraints = vec![constraint(
            Type::Literal(TypeLiteral::Bool),
            Type::Literal(TypeLiteral::Bool),
        )];

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    #[test]
    fn unifies_functions() {
        let fun = Type::Fun {
            param_tpe: Box::new(Type::Literal(TypeLiteral::Bool)),
            return_tpe: Box::new(Type::Literal(TypeLiteral::Bool)),
        };
        let constraints = vec![constraint(fun.clone(), fun.clone())];

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    #[test]
    fn unifies_variables() {
        let tvar1 = TypeVariable { id: 1 };
        let t1 = Type::Variable(tvar1.clone());
        let t2 = Type::Variable(TypeVariable { id: 2 });

        let constraints = vec![constraint(t1, t2.clone())];

        assert_eq!(
            unify(constraints).unwrap(),
            Substitution::one(tvar1, t2, origin())
        );
    }

    #[test]
    fn unifies_variable_with_literal() {
        let tvar1 = TypeVariable { id: 1 };
        let t1 = Type::Variable(tvar1.clone());
        let t2 = Type::Literal(TypeLiteral::Int);

        let constraints = vec![constraint(t1, t2.clone())];

        assert_eq!(
            unify(constraints).unwrap(),
            Substitution::one(tvar1, t2, origin())
        );
    }

    #[test]
    fn unifies_variables_in_functions() {
        let tvar1 = TypeVariable { id: 1 };
        let tvar2 = TypeVariable { id: 2 };

        let constraints = vec![constraint(
            // tvar1 -> bool
            Type::Fun {
                param_tpe: Box::new(Type::Variable(tvar1.clone())),
                return_tpe: Box::new(Type::Literal(TypeLiteral::Bool)),
            },
            // int -> tvar2
            Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(Type::Variable(tvar2.clone())),
            },
        )];

        let sub = Substitution::one(tvar2, Type::Literal(TypeLiteral::Bool), origin()).merge(
            Substitution::one(tvar1, Type::Literal(TypeLiteral::Int), origin()),
        );

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
                return_tpe: Box::new(Type::Literal(TypeLiteral::Bool)),
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
    /// unchanged: `because` is then `None` and the assertion goes red.
    #[test]
    fn a_substituted_type_is_explained_by_the_constraint_that_solved_it() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        let constraints = vec![
            Constraint::new(
                Type::Literal(TypeLiteral::Bool),
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
                    origin.because.map(|b| b.reason),
                    Some(Reason::Annotation),
                    "the annotation is where `Bool` came from"
                );
            }
            other => panic!("expected a unification failure, got {:?}", other),
        }
    }
}
