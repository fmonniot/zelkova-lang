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
        (a, b) => panic!("Cannot unify {:?} with {:?}", a, b), // TODO Return Result instead
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
                panic!("circular use: {:?} occurs in {:?}", tvar, tpe) // TODO Return Result instead
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::typer::*;

    #[test]
    fn unifies_ints() {
        let mut constraints = HashSet::new();
        constraints.insert(Constraint(
            Type::Literal(TypeLiteral::Int),
            Type::Literal(TypeLiteral::Int),
        ));

        assert_eq!(unify(constraints), Substitution::empty());
    }

    #[test]
    fn unifies_bools() {
        let mut constraints = HashSet::new();
        constraints.insert(Constraint(
            Type::Literal(TypeLiteral::Bool),
            Type::Literal(TypeLiteral::Bool),
        ));

        assert_eq!(unify(constraints), Substitution::empty());
    }

    #[test]
    fn unifies_functions() {
        let fun = Type::Fun {
            param_tpe: Box::new(Type::Literal(TypeLiteral::Bool)),
            return_tpe: Box::new(Type::Literal(TypeLiteral::Bool)),
        };
        let mut constraints = HashSet::new();
        constraints.insert(Constraint(fun.clone(), fun.clone()));

        assert_eq!(unify(constraints), Substitution::empty());
    }

    #[test]
    fn unifies_variables() {
        let tvar1 = TypeVariable { id: 1 };
        let t1 = Type::Variable(tvar1.clone());
        let t2 = Type::Variable(TypeVariable { id: 2 });

        let mut constraints = HashSet::new();
        constraints.insert(Constraint(t1, t2.clone()));

        assert_eq!(unify(constraints), Substitution::one(tvar1, t2));
    }

    #[test]
    fn unifies_variable_with_literal() {
        let tvar1 = TypeVariable { id: 1 };
        let t1 = Type::Variable(tvar1.clone());
        let t2 = Type::Literal(TypeLiteral::Int);

        let mut constraints = HashSet::new();
        constraints.insert(Constraint(t1, t2.clone()));

        assert_eq!(unify(constraints), Substitution::one(tvar1, t2));
    }

    #[test]
    fn unifies_variables_in_functions() {
        let tvar1 = TypeVariable { id: 1 };
        let tvar2 = TypeVariable { id: 2 };

        let mut constraints = HashSet::new();
        constraints.insert(Constraint(
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
        ));

        let sub = Substitution::one(tvar2, Type::Literal(TypeLiteral::Bool))
            .merge(Substitution::one(tvar1, Type::Literal(TypeLiteral::Int)));

        assert_eq!(unify(constraints), sub);
    }
}
