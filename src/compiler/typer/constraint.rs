use std::collections::HashSet;

use super::{Constraint, Type, TypeLiteral, TypedTerm};

pub(super) fn collect(term: &TypedTerm) -> HashSet<Constraint> {
    let mut constraints = HashSet::new();

    match term {
        TypedTerm::Bool { tpe, .. } => {
            constraints.insert(Constraint(tpe.clone(), Type::Literal(TypeLiteral::Bool)));
        }
        TypedTerm::Int { tpe, .. } => {
            constraints.insert(Constraint(tpe.clone(), Type::Literal(TypeLiteral::Int)));
        }
        TypedTerm::Fun { tpe, param, body } => {
            constraints.extend(collect(&body));

            let param_tpe = Box::new(param.tpe.clone());
            let return_tpe = Box::new(body.tpe().clone());
            constraints.insert(Constraint(
                tpe.clone(),
                Type::Fun {
                    param_tpe,
                    return_tpe,
                },
            ));
        }
        TypedTerm::Identifier { .. } => (),
        TypedTerm::Apply { tpe, fun, arg } => {
            constraints.extend(collect(fun));
            constraints.extend(collect(arg));

            let param_tpe = Box::new(arg.tpe().clone());
            let return_tpe = Box::new(tpe.clone());
            constraints.insert(Constraint(
                fun.tpe().clone(),
                Type::Fun {
                    param_tpe,
                    return_tpe,
                },
            ));
        }
        TypedTerm::If {
            tpe,
            cond,
            true_branch,
            false_branch,
        } => {
            constraints.extend(collect(cond));
            constraints.extend(collect(true_branch));
            constraints.extend(collect(false_branch));

            // If put a constraint on the condition and the branches should resolve to the same type
            constraints.insert(Constraint(
                cond.tpe().clone(),
                Type::Literal(TypeLiteral::Bool),
            ));
            constraints.insert(Constraint(true_branch.tpe().clone(), tpe.clone()));
            constraints.insert(Constraint(false_branch.tpe().clone(), tpe.clone()));
        }
        TypedTerm::Let {
            tpe,
            binding,
            value,
            body,
        } => {
            constraints.extend(collect(value));
            constraints.extend(collect(body));

            // The let expression has the body type.
            constraints.insert(Constraint(tpe.clone(), body.tpe().clone()));
            // The binding type is the one of the value.
            constraints.insert(Constraint(binding.tpe.clone(), value.tpe().clone()));
        }
    };

    constraints
}
