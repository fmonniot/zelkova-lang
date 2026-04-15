use super::{Error, Term, TermPattern, TypeBinder, TypedTerm, Types};

pub(super) fn annotate(term: Term, types: &mut Types) -> Result<TypedTerm, Error> {
    match term {
        Term::Int(value) => Ok(TypedTerm::Int {
            tpe: types.fresh_var(),
            value,
        }),
        Term::Bool(value) => Ok(TypedTerm::Bool {
            tpe: types.fresh_var(),
            value,
        }),
        Term::Char(value) => Ok(TypedTerm::Char {
            tpe: types.fresh_var(),
            value,
        }),
        Term::Float(value) => Ok(TypedTerm::Float {
            tpe: types.fresh_var(),
            value,
        }),
        Term::Fun { param, body } => {
            let param = TypeBinder::new(param, types.fresh_var());
            types.add_binder(param.clone());

            let body = annotate(*body, types)?;

            Ok(TypedTerm::Fun {
                tpe: types.fresh_var(),
                param,
                body: Box::new(body),
            })
        }
        Term::Identifier(name) => match types.by_name(&name) {
            None => Err(Error::UnboundVariable(name)),
            Some(tpe) => Ok(TypedTerm::Identifier { tpe, name }),
        },
        Term::Apply { fun, arg } => {
            let fun = Box::new(annotate(*fun, types)?);
            let arg = Box::new(annotate(*arg, types)?);

            Ok(TypedTerm::Apply {
                tpe: types.fresh_var(),
                fun,
                arg,
            })
        }
        Term::If {
            cond,
            true_branch,
            false_branch,
        } => {
            let cond = Box::new(annotate(*cond, types)?);
            let true_branch = Box::new(annotate(*true_branch, types)?);
            let false_branch = Box::new(annotate(*false_branch, types)?);

            Ok(TypedTerm::If {
                tpe: types.fresh_var(),
                cond,
                true_branch,
                false_branch,
            })
        }
        Term::Let {
            binding,
            value,
            body,
        } => {
            let binding_tpe = types.fresh_var();
            let binding = TypeBinder::new(binding, binding_tpe);
            let value = Box::new(annotate(*value, types)?);

            // scoping: We need to add the binding before evaluating the body but after the value
            types.add_binder(binding.clone());
            let body = Box::new(annotate(*body, types)?);

            Ok(TypedTerm::Let {
                tpe: types.fresh_var(),
                binding,
                value,
                body,
            })
        }
        Term::Tuple(a, b, c) => {
            let first = Box::new(annotate(*a, types)?);
            let second = Box::new(annotate(*b, types)?);
            let third = c.map(|t| annotate(*t, types)).transpose()?.map(Box::new);
            Ok(TypedTerm::Tuple {
                tpe: types.fresh_var(),
                first,
                second,
                third,
            })
        }
        Term::Case {
            scrutinee,
            branches,
        } => {
            let scrutinee = Box::new(annotate(*scrutinee, types)?);
            let mut typed_branches = Vec::new();
            for (pattern, body) in branches {
                // Determine what bindings this pattern introduces.
                let new_bindings: Vec<(String, super::Type)> = match &pattern {
                    TermPattern::Bind(name) => {
                        // Bind to the scrutinee's type variable.
                        vec![(name.clone(), scrutinee.tpe().clone())]
                    }
                    TermPattern::Constructor { bindings, .. } => bindings.clone(),
                    TermPattern::Anything | TermPattern::Literal(_) => vec![],
                };
                for (name, tpe) in &new_bindings {
                    types.add_binder(TypeBinder::new(name.clone(), tpe.clone()));
                }
                let typed_body = Box::new(annotate(*body, types)?);
                // Restore scope: remove bindings added for this branch.
                for (name, _) in &new_bindings {
                    types.remove_binder(name);
                }
                typed_branches.push((pattern, typed_body));
            }
            Ok(TypedTerm::Case {
                tpe: types.fresh_var(),
                scrutinee,
                branches: typed_branches,
            })
        }
    }
}
