use log::debug;
use std::collections::HashSet;

use super::{Constraint, Error, Substitution, Type, TypeLiteral, TypeVariable};

/// Returns true if `tpe` is a numeric type (Int, Float, or Number).
fn is_numeric(tpe: &Type) -> bool {
    matches!(
        tpe,
        Type::Literal(TypeLiteral::Int) | Type::Literal(TypeLiteral::Float) | Type::Number
    )
}

pub(super) fn unify(constraints: HashSet<Constraint>) -> Result<Substitution, Error> {
    debug!("unify: {:?}", constraints);
    let mut iter = constraints.iter();

    match iter.next() {
        None => Ok(Substitution::empty()),
        Some(first) => {
            let sub_head = unify_one_constraint(first)?;

            // Apply this substitution to the remaining constraints
            let constraints_tail: HashSet<_> = iter.map(|c| sub_head.apply(c)).collect();

            // Then recursively unify the substituted constraints
            let sub_tail = unify(constraints_tail)?;

            // And finally merged the unified substitution with the first one
            Ok(sub_head.merge(sub_tail))
        }
    }
}

fn unify_one_constraint(Constraint(a, b): &Constraint) -> Result<Substitution, Error> {
    debug!("unify_one_constraint: {:?} to {:?}", a, b);
    match (a, b) {
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
        // Number unifies with Int, Float, or another Number (but not Bool, Char, etc.)
        (Type::Number, other) | (other, Type::Number) if is_numeric(other) => {
            Ok(Substitution::empty())
        }
        // Tuples: unify element-by-element (must have matching arity)
        (Type::Tuple(a1, b1, None), Type::Tuple(a2, b2, None)) => {
            let mut cs = HashSet::new();
            cs.insert(Constraint(*a1.clone(), *a2.clone()));
            cs.insert(Constraint(*b1.clone(), *b2.clone()));
            unify(cs)
        }
        (Type::Tuple(a1, b1, Some(c1)), Type::Tuple(a2, b2, Some(c2))) => {
            let mut cs = HashSet::new();
            cs.insert(Constraint(*a1.clone(), *a2.clone()));
            cs.insert(Constraint(*b1.clone(), *b2.clone()));
            cs.insert(Constraint(*c1.clone(), *c2.clone()));
            unify(cs)
        }
        (Type::Variable(tvar), tpe) => unify_variable(tvar, tpe),
        (tpe, Type::Variable(tvar)) => unify_variable(tvar, tpe),
        (a, b) => Err(Error::UnificationFailed {
            left: a.clone(),
            right: b.clone(),
        }),
    }
}

fn unify_variable(tvar: &TypeVariable, tpe: &Type) -> Result<Substitution, Error> {
    match tpe {
        Type::Variable(tvar2) => {
            if tvar == tvar2 {
                Ok(Substitution::empty())
            } else {
                Ok(Substitution::one(tvar.clone(), tpe.clone()))
            }
        }
        _ => {
            if occurs(tvar, tpe) {
                Err(Error::CircularType)
            } else {
                Ok(Substitution::one(tvar.clone(), tpe.clone()))
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
        Type::Tuple(a, b, c) => {
            occurs(tvar, a)
                || occurs(tvar, b)
                || c.as_ref().map_or(false, |t| occurs(tvar, t))
        }
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

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    #[test]
    fn unifies_bools() {
        let mut constraints = HashSet::new();
        constraints.insert(Constraint(
            Type::Literal(TypeLiteral::Bool),
            Type::Literal(TypeLiteral::Bool),
        ));

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    #[test]
    fn unifies_functions() {
        let fun = Type::Fun {
            param_tpe: Box::new(Type::Literal(TypeLiteral::Bool)),
            return_tpe: Box::new(Type::Literal(TypeLiteral::Bool)),
        };
        let mut constraints = HashSet::new();
        constraints.insert(Constraint(fun.clone(), fun.clone()));

        assert_eq!(unify(constraints).unwrap(), Substitution::empty());
    }

    #[test]
    fn unifies_variables() {
        let tvar1 = TypeVariable { id: 1 };
        let t1 = Type::Variable(tvar1.clone());
        let t2 = Type::Variable(TypeVariable { id: 2 });

        let mut constraints = HashSet::new();
        constraints.insert(Constraint(t1, t2.clone()));

        assert_eq!(unify(constraints).unwrap(), Substitution::one(tvar1, t2));
    }

    #[test]
    fn unifies_variable_with_literal() {
        let tvar1 = TypeVariable { id: 1 };
        let t1 = Type::Variable(tvar1.clone());
        let t2 = Type::Literal(TypeLiteral::Int);

        let mut constraints = HashSet::new();
        constraints.insert(Constraint(t1, t2.clone()));

        assert_eq!(unify(constraints).unwrap(), Substitution::one(tvar1, t2));
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

        assert_eq!(unify(constraints).unwrap(), sub);
    }
}
