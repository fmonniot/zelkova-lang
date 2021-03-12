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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::typer::*;

    #[test]
    fn constrains_int() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        let mut expected = HashSet::new();

        // t1 === Int
        expected.insert(Constraint(t1.clone(), Type::Literal(TypeLiteral::Int)));

        let int = TypedTerm::Int { tpe: t1, value: 42 };

        let constraints = collect(&int);

        assert_eq!(constraints, expected);
    }

    #[test]
    fn constrains_bool() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        let mut expected = HashSet::new();

        // t1 === Int
        expected.insert(Constraint(t1.clone(), Type::Literal(TypeLiteral::Bool)));

        let b = TypedTerm::Bool {
            tpe: t1,
            value: true,
        };

        let constraints = collect(&b);

        assert_eq!(constraints, expected);
    }

    #[test]
    fn constrains_function() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });

        let mut expected = HashSet::new();

        // t1 === t2 -> t3 (eg. fn type === arg type -> body type )
        expected.insert(Constraint(
            t1.clone(),
            Type::Fun {
                param_tpe: Box::new(t2.clone()),
                return_tpe: Box::new(t3.clone()),
            },
        ));

        let arg = TypedTerm::Identifier {
            tpe: t3,
            name: "b".to_owned(),
        };
        let apply = TypedTerm::Fun {
            tpe: t1,
            param: TypeBinder::new("b".to_string(), t2),
            body: Box::new(arg),
        };

        let constraints = collect(&apply);

        assert_eq!(constraints, expected);
    }

    #[test]
    fn constrains_variable() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        let expected = HashSet::new();

        let b = TypedTerm::Identifier {
            tpe: t1,
            name: "a".to_string(),
        };

        let constraints = collect(&b);

        assert_eq!(constraints, expected);
    }

    #[test]
    fn constrains_apply() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });

        let mut expected = HashSet::new();

        // t2 === t3 -> t1 (eg. fn type === arg type -> apply type )
        expected.insert(Constraint(
            t2.clone(),
            Type::Fun {
                param_tpe: Box::new(t3.clone()),
                return_tpe: Box::new(t1.clone()),
            },
        ));

        let fun = TypedTerm::Identifier {
            tpe: t2,
            name: "fn".to_owned(),
        };
        let arg = TypedTerm::Identifier {
            tpe: t3,
            name: "arg".to_owned(),
        };
        let apply = TypedTerm::Apply {
            tpe: t1,
            fun: Box::new(fun),
            arg: Box::new(arg),
        };

        let constraints = collect(&apply);

        assert_eq!(constraints, expected);
    }

    #[test]
    fn constrains_if() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });
        let t4 = Type::Variable(TypeVariable { id: 4 });

        let mut expected = HashSet::new();

        // t2 === Bool (eg. the condition needs to be a boolean)
        // t3 === t1   (eg. the if type is the same as the first branch)
        // t4 === t1   (eg. the if type is the same as the second branch)
        expected.insert(Constraint(t2.clone(), Type::Literal(TypeLiteral::Bool)));
        expected.insert(Constraint(t3.clone(), t1.clone()));
        expected.insert(Constraint(t4.clone(), t1.clone()));

        let cond = Box::new(TypedTerm::Identifier {
            tpe: t2,
            name: "condition".to_owned(),
        });
        let true_branch = Box::new(TypedTerm::Identifier {
            tpe: t3,
            name: "if_true".to_owned(),
        });
        let false_branch = Box::new(TypedTerm::Identifier {
            tpe: t4,
            name: "if_false".to_owned(),
        });
        let if_else = TypedTerm::If {
            tpe: t1,
            cond,
            true_branch,
            false_branch,
        };

        let constraints = collect(&if_else);

        assert_eq!(constraints, expected);
    }

    #[test]
    fn constrains_let() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });
        let t4 = Type::Variable(TypeVariable { id: 4 });

        let mut expected = HashSet::new();

        // t2 === t3   (eg. var type === value type)
        // t1 === t4   (eg. let type === body type)
        expected.insert(Constraint(t2.clone(), t3.clone()));
        expected.insert(Constraint(t1.clone(), t4.clone()));

        let value = Box::new(TypedTerm::Identifier {
            tpe: t3,
            name: "val".to_owned(),
        });
        let body = Box::new(TypedTerm::Identifier {
            tpe: t4,
            name: "body".to_owned(),
        });
        let let_ = TypedTerm::Let {
            tpe: t1,
            binding: TypeBinder::new("b".to_string(), t2),
            value,
            body,
        };

        let constraints = collect(&let_);

        assert_eq!(constraints, expected);
    }
}
