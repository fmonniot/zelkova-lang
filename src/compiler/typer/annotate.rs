use super::{Term, TypeBinder, TypedTerm, Types};

pub(super) fn annotate(term: Term, types: &mut Types) -> TypedTerm {
    match term {
        Term::Int(value) => TypedTerm::Int {
            tpe: types.fresh_var(),
            value,
        },
        Term::Bool(value) => TypedTerm::Bool {
            tpe: types.fresh_var(),
            value,
        },
        Term::Fun { param, body } => {
            let param = TypeBinder::new(param, types.fresh_var());
            types.add_binder(param.clone());

            let body = annotate(*body, types);

            TypedTerm::Fun {
                tpe: types.fresh_var(),
                param,
                body: Box::new(body),
            }
        }
        Term::Identifier(name) => match types.by_name(&name) {
            None => panic!("unbound identifier: {}", name),
            Some(tpe) => TypedTerm::Identifier { tpe, name },
        },
        Term::Apply { fun, arg } => {
            let fun = Box::new(annotate(*fun, types));
            let arg = Box::new(annotate(*arg, types));

            TypedTerm::Apply {
                tpe: types.fresh_var(),
                fun,
                arg,
            }
        }
        Term::If {
            cond,
            true_branch,
            false_branch,
        } => {
            let cond = Box::new(annotate(*cond, types));
            let true_branch = Box::new(annotate(*true_branch, types));
            let false_branch = Box::new(annotate(*false_branch, types));

            TypedTerm::If {
                tpe: types.fresh_var(),
                cond,
                true_branch,
                false_branch,
            }
        }
        Term::Let {
            binding,
            value,
            body,
        } => {
            let binding_tpe = types.fresh_var();
            let binding = TypeBinder::new(binding, binding_tpe);
            let value = Box::new(annotate(*value, types));

            // scoping: We need to add the binding before evaluating the body but after the value
            types.add_binder(binding.clone());
            let body = Box::new(annotate(*body, types));

            TypedTerm::Let {
                tpe: types.fresh_var(),
                binding,
                value,
                body,
            }
        }
    }
}
