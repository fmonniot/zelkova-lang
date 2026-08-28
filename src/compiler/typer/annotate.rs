//! Give every node of a [`Term`] a type variable, keeping the position it came with.
//!
//! This is where spans enter the typed side of the typer: a [`TypedTerm`] is built
//! from a [`Term`] node and takes its `span` verbatim. `constraint::collect` reads
//! them off the typed term when it decides what each constraint is about, so nothing
//! below this point ever has to go looking for a position again.

use super::{
    ErrorKind, Term, TermKind, TermPatternKind, TypeBinder, TypedTerm, TypedTermKind, Types,
};
use crate::compiler::tuple::Tuple;

pub(super) fn annotate(term: Term, types: &mut Types) -> Result<TypedTerm, ErrorKind> {
    let span = term.span;
    // Every arm below builds a kind and a type, and the span is copied over once,
    // here — an arm that forgot to carry it would silently cost a caret.
    let (tpe, kind) = match term.kind {
        TermKind::Int(value) => (types.fresh_var(), TypedTermKind::Int(value)),
        TermKind::Bool(value) => (types.fresh_var(), TypedTermKind::Bool(value)),
        TermKind::Char(value) => (types.fresh_var(), TypedTermKind::Char(value)),
        TermKind::Float(value) => (types.fresh_var(), TypedTermKind::Float(value)),
        TermKind::Fun { param, body } => {
            let param = TypeBinder::new(param, types.fresh_var());
            types.add_binder(param.clone());

            let body = annotate(*body, types)?;

            (
                types.fresh_var(),
                TypedTermKind::Fun {
                    param,
                    body: Box::new(body),
                },
            )
        }
        TermKind::Identifier(name) => match types.by_name(&name) {
            // The one error `annotate` can raise, and the span is the whole of what
            // `ERR-4` changed about it: the name is underlined where it was written.
            None => return Err(ErrorKind::UnboundVariable { name, span }),
            Some(tpe) => (tpe, TypedTermKind::Identifier(name)),
        },
        TermKind::Apply { fun, arg } => {
            let fun = Box::new(annotate(*fun, types)?);
            let arg = Box::new(annotate(*arg, types)?);

            (types.fresh_var(), TypedTermKind::Apply { fun, arg })
        }
        TermKind::If {
            cond,
            true_branch,
            false_branch,
        } => {
            let cond = Box::new(annotate(*cond, types)?);
            let true_branch = Box::new(annotate(*true_branch, types)?);
            let false_branch = Box::new(annotate(*false_branch, types)?);

            (
                types.fresh_var(),
                TypedTermKind::If {
                    cond,
                    true_branch,
                    false_branch,
                },
            )
        }
        TermKind::Let {
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

            (
                types.fresh_var(),
                TypedTermKind::Let {
                    binding,
                    value,
                    body,
                },
            )
        }
        TermKind::Tuple(Tuple::Two(a, b)) => {
            let elements = Tuple::two(annotate(*a, types)?, annotate(*b, types)?);
            (types.fresh_var(), TypedTermKind::Tuple(elements))
        }
        TermKind::Tuple(Tuple::Three(a, b, c)) => {
            let elements = Tuple::three(
                annotate(*a, types)?,
                annotate(*b, types)?,
                annotate(*c, types)?,
            );
            (types.fresh_var(), TypedTermKind::Tuple(elements))
        }
        TermKind::Case {
            scrutinee,
            branches,
        } => {
            let scrutinee = Box::new(annotate(*scrutinee, types)?);
            let mut typed_branches = Vec::new();
            for (pattern, body) in branches {
                // Determine what bindings this pattern introduces.
                let new_bindings: Vec<(String, super::Type)> = match &pattern.kind {
                    TermPatternKind::Bind(name) => {
                        // Bind to the scrutinee's type variable.
                        vec![(name.clone(), scrutinee.tpe.clone())]
                    }
                    TermPatternKind::Constructor { bindings, .. } => bindings.clone(),
                    TermPatternKind::Anything | TermPatternKind::Literal(_) => vec![],
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
            (
                types.fresh_var(),
                TypedTermKind::Case {
                    scrutinee,
                    branches: typed_branches,
                },
            )
        }
    };

    Ok(TypedTerm { span, tpe, kind })
}
