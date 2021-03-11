use log::debug;
use std::collections::HashSet;

use super::{Constraint, Substitution, Type, TypeLiteral, TypeVariable};

pub(super) fn unify(constraints: HashSet<Constraint>) -> Substitution {
    debug!("unify: {:?}", constraints);
    let mut iter = constraints.iter();

    match iter.next() {
        None => Substitution::empty(),
        Some(first) => {
            let sub_head = unify_one_constraint(first);

            // Apply this substitution to the remaining constraints
            let constraints_tail: HashSet<_> = iter.map(|c| sub_head.apply(c)).collect();

            // Then recursively unify the substituted constraints
            let sub_tail = unify(constraints_tail);

            // And finally merged the unified substitution with the first one
            sub_head.merge(sub_tail)
        }
    }
}

fn unify_one_constraint(Constraint(a, b): &Constraint) -> Substitution {
    debug!("unify_one_constraint: {:?} to {:?}", a, b);
    match (a, b) {
        (Type::Literal(TypeLiteral::Bool), Type::Literal(TypeLiteral::Bool)) => {
            Substitution::empty()
        }
        (Type::Literal(TypeLiteral::Int), Type::Literal(TypeLiteral::Int)) => Substitution::empty(),
        (
            Type::Fun {
                param_tpe: p1,
                return_tpe: r1,
            },
            Type::Fun {
                param_tpe: p2,
                return_tpe: r2,
            },
        ) => {
            let mut constraints = HashSet::new();

            constraints.insert(Constraint(*p1.clone(), *p2.clone()));
            constraints.insert(Constraint(*r1.clone(), *r2.clone()));

            unify(constraints)
        }
        (Type::Variable(tvar), tpe) => unify_variable(tvar, tpe),
        (tpe, Type::Variable(tvar)) => unify_variable(tvar, tpe),
        (a, b) => panic!("Cannot unify {:?} with {:?}", a, b),
    }
}

fn unify_variable(tvar: &TypeVariable, tpe: &Type) -> Substitution {
    // TODO Use pattern guard instead of inlined if
    match tpe {
        Type::Variable(tvar2) => {
            if tvar == tvar2 {
                Substitution::empty()
            } else {
                Substitution::one(tvar.clone(), tpe.clone())
            }
        }
        _ => {
            if occurs(tvar, tpe) {
                panic!("circular use: {:?} occurs in {:?}", tvar, tpe)
            } else {
                Substitution::one(tvar.clone(), tpe.clone())
            }
        }
    }
}

fn occurs(tvar: &TypeVariable, tpe: &Type) -> bool {
    match tpe {
        Type::Fun {
            param_tpe,
            return_tpe,
        } => occurs(tvar, param_tpe) || occurs(tvar, return_tpe),
        Type::Variable(tvar2) => tvar == tvar2,
        _ => false,
    }
}
