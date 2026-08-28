//! Turn a typed term into the constraints its shape requires, each with its reason.
//!
//! This is where a constraint's [`Reason`] is chosen, because this is the last place
//! the *structure* of the expression is still visible: by the time `unify` has a pair
//! of types in hand, nothing says whether they were brought together by an annotation,
//! by two branches of an `if`, or by an argument being passed.
//!
//! Two rules hold at every site below, and both are load-bearing for diagnostics:
//!
//! - a term's own constraints come before its children's, so that a type known from
//!   the outside — a declaration's annotation, and through it a function's parameter
//!   and result types — has been substituted into the inner constraints before any of
//!   them is solved. The failure is then reported at the innermost thing that
//!   disagrees, which is the sub-expression the user has to change, and the chain of
//!   substitutions that got there leads back to the annotation (see
//!   [`Origin::because`]). Collecting children first inverts that: the body settles
//!   on its own type first and the mismatch surfaces at the whole function.
//! - the sides of a constraint are ordered declared-first where the source has a
//!   declared side, because that is the order the headline reads them out in (see
//!   [`Constraint`]).

use super::{Constraint, Reason, TermPatternKind, Type, TypeLiteral, TypedTerm, TypedTermKind};
use crate::compiler::tuple::Tuple;

pub(super) fn collect(term: &TypedTerm) -> Vec<Constraint> {
    let mut constraints = Vec::new();
    let tpe = &term.tpe;
    let span = term.span;

    match &term.kind {
        TypedTermKind::Bool(_) => {
            constraints.push(Constraint::new(
                tpe.clone(),
                Type::Literal(TypeLiteral::Bool),
                Reason::Literal,
                span,
            ));
        }
        TypedTermKind::Int(_) => {
            // Integer literals are polymorphic numeric values: they can unify
            // with Int or Float (but not Bool, Char, etc.).
            constraints.push(Constraint::new(
                tpe.clone(),
                Type::Number,
                Reason::Literal,
                span,
            ));
        }
        TypedTermKind::Char(_) => {
            constraints.push(Constraint::new(
                tpe.clone(),
                Type::Literal(TypeLiteral::Char),
                Reason::Literal,
                span,
            ));
        }
        TypedTermKind::Float(_) => {
            constraints.push(Constraint::new(
                tpe.clone(),
                Type::Literal(TypeLiteral::Float),
                Reason::Literal,
                span,
            ));
        }
        TypedTermKind::Fun { param, body } => {
            let param_tpe = Box::new(param.tpe.clone());
            let return_tpe = Box::new(body.tpe.clone());
            constraints.push(Constraint::new(
                tpe.clone(),
                Type::Fun {
                    param_tpe,
                    return_tpe,
                },
                Reason::FunctionShape,
                span,
            ));

            constraints.extend(collect(body));
        }
        TypedTermKind::Identifier(_) => (),
        TypedTermKind::Apply { fun, arg } => {
            let param_tpe = Box::new(arg.tpe.clone());
            let return_tpe = Box::new(tpe.clone());
            // The span is the *applied* expression's, not the whole application's:
            // "the expression being applied" is only useful pointing at the thing
            // being applied.
            constraints.push(Constraint::new(
                fun.tpe.clone(),
                Type::Fun {
                    param_tpe,
                    return_tpe,
                },
                Reason::Application,
                fun.span,
            ));

            constraints.extend(collect(fun));
            constraints.extend(collect(arg));
        }
        TypedTermKind::If {
            cond,
            true_branch,
            false_branch,
        } => {
            // If put a constraint on the condition and the branches should resolve to the same type
            constraints.push(Constraint::new(
                cond.tpe.clone(),
                Type::Literal(TypeLiteral::Bool),
                Reason::IfCondition,
                cond.span,
            ));
            constraints.push(Constraint::new(
                true_branch.tpe.clone(),
                tpe.clone(),
                Reason::IfBranch,
                true_branch.span,
            ));
            constraints.push(Constraint::new(
                false_branch.tpe.clone(),
                tpe.clone(),
                Reason::IfBranch,
                false_branch.span,
            ));

            constraints.extend(collect(cond));
            constraints.extend(collect(true_branch));
            constraints.extend(collect(false_branch));
        }
        TypedTermKind::Let {
            binding,
            value,
            body,
        } => {
            // The let expression has the body type.
            constraints.push(Constraint::new(
                tpe.clone(),
                body.tpe.clone(),
                Reason::LetBody,
                span,
            ));
            // The binding type is the one of the value. Written value-first so that
            // the side named by the span — the value — is `left`.
            constraints.push(Constraint::new(
                value.tpe.clone(),
                binding.tpe.clone(),
                Reason::LetBinding,
                value.span,
            ));

            constraints.extend(collect(value));
            constraints.extend(collect(body));
        }
        TypedTermKind::Case {
            scrutinee,
            branches,
        } => {
            constraints.extend(collect(scrutinee));
            for (pattern, body) in branches {
                // Every branch must return the case expression's type.
                constraints.push(Constraint::new(
                    body.tpe.clone(),
                    tpe.clone(),
                    Reason::CaseBranch,
                    body.span,
                ));
                // Each pattern constrains the scrutinee type. The pattern is what the
                // caret should sit under, so the pattern's type is `left`.
                match &pattern.kind {
                    TermPatternKind::Literal(lit) => {
                        constraints.push(Constraint::new(
                            lit.clone(),
                            scrutinee.tpe.clone(),
                            Reason::CasePattern,
                            pattern.span,
                        ));
                    }
                    TermPatternKind::Constructor {
                        adt_name, adt_args, ..
                    } => {
                        constraints.push(Constraint::new(
                            Type::Adt(adt_name.clone(), adt_args.clone()),
                            scrutinee.tpe.clone(),
                            Reason::CasePattern,
                            pattern.span,
                        ));
                    }
                    // Bind/Anything: the binding's type was already set to scrutinee.tpe
                    // in annotate, so no extra constraint needed here.
                    TermPatternKind::Bind(_) | TermPatternKind::Anything => {}
                }

                constraints.extend(collect(body));
            }
        }
        TypedTermKind::Tuple(elements) => {
            // The tuple type must equal the tuple of its element types.
            let element_types = match elements {
                Tuple::Two(a, b) => Tuple::two(a.tpe.clone(), b.tpe.clone()),
                Tuple::Three(a, b, c) => Tuple::three(a.tpe.clone(), b.tpe.clone(), c.tpe.clone()),
            };
            constraints.push(Constraint::new(
                tpe.clone(),
                Type::Tuple(element_types),
                Reason::TupleElements,
                span,
            ));

            for elem in elements.iter() {
                constraints.extend(collect(elem));
            }
        }
    };

    constraints
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::position::NodeSpan;
    use crate::compiler::typer::*;

    /// Build a typed term with no position — these tests are about which constraints
    /// come out and why, not about where. `NodeSpan`'s `PartialEq` is blind, so the
    /// `Reason` is the part of an `Origin` these assertions actually pin.
    fn typed(tpe: Type, kind: TypedTermKind) -> TypedTerm {
        TypedTerm {
            span: NodeSpan::none(),
            tpe,
            kind,
        }
    }

    fn identifier(tpe: Type, name: &str) -> TypedTerm {
        typed(tpe, TypedTermKind::Identifier(name.to_owned()))
    }

    fn constraint(left: Type, right: Type, reason: Reason) -> Constraint {
        Constraint::new(left, right, reason, NodeSpan::none())
    }

    #[test]
    fn constrains_int() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        // Integer literals constrain to Number (polymorphic: can be Int or Float)
        let expected = vec![constraint(t1.clone(), Type::Number, Reason::Literal)];

        let int = typed(t1, TypedTermKind::Int(42));

        assert_eq!(collect(&int), expected);
    }

    #[test]
    fn constrains_bool() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        // t1 === Bool
        let expected = vec![constraint(
            t1.clone(),
            Type::Literal(TypeLiteral::Bool),
            Reason::Literal,
        )];

        let b = typed(t1, TypedTermKind::Bool(true));

        assert_eq!(collect(&b), expected);
    }

    #[test]
    fn constrains_function() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });

        // t1 === t2 -> t3 (eg. fn type === arg type -> body type )
        let expected = vec![constraint(
            t1.clone(),
            Type::Fun {
                param_tpe: Box::new(t2.clone()),
                return_tpe: Box::new(t3.clone()),
            },
            Reason::FunctionShape,
        )];

        let body = identifier(t3, "b");
        let fun = typed(
            t1,
            TypedTermKind::Fun {
                param: TypeBinder::new("b".to_string(), t2),
                body: Box::new(body),
            },
        );

        assert_eq!(collect(&fun), expected);
    }

    #[test]
    fn constrains_variable() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        let b = identifier(t1, "a");

        assert_eq!(collect(&b), vec![]);
    }

    #[test]
    fn constrains_apply() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });

        // t2 === t3 -> t1 (eg. fn type === arg type -> apply type )
        let expected = vec![constraint(
            t2.clone(),
            Type::Fun {
                param_tpe: Box::new(t3.clone()),
                return_tpe: Box::new(t1.clone()),
            },
            Reason::Application,
        )];

        let fun = identifier(t2, "fn");
        let arg = identifier(t3, "arg");
        let apply = typed(
            t1,
            TypedTermKind::Apply {
                fun: Box::new(fun),
                arg: Box::new(arg),
            },
        );

        assert_eq!(collect(&apply), expected);
    }

    #[test]
    fn constrains_if() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });
        let t4 = Type::Variable(TypeVariable { id: 4 });

        // t2 === Bool (eg. the condition needs to be a boolean)
        // t3 === t1   (eg. the if type is the same as the first branch)
        // t4 === t1   (eg. the if type is the same as the second branch)
        //
        // The reasons are asserted too: they are what a diagnostic says out loud, and
        // a condition reported as "every branch must have the same type" would be a
        // lie the types alone cannot catch.
        let expected = vec![
            constraint(
                t2.clone(),
                Type::Literal(TypeLiteral::Bool),
                Reason::IfCondition,
            ),
            constraint(t3.clone(), t1.clone(), Reason::IfBranch),
            constraint(t4.clone(), t1.clone(), Reason::IfBranch),
        ];

        let cond = Box::new(identifier(t2, "condition"));
        let true_branch = Box::new(identifier(t3, "if_true"));
        let false_branch = Box::new(identifier(t4, "if_false"));
        let if_else = typed(
            t1,
            TypedTermKind::If {
                cond,
                true_branch,
                false_branch,
            },
        );

        assert_eq!(collect(&if_else), expected);
    }

    #[test]
    fn constrains_let() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });
        let t4 = Type::Variable(TypeVariable { id: 4 });

        // t1 === t4   (eg. let type === body type)
        // t3 === t2   (eg. value type === var type)
        let expected = vec![
            constraint(t1.clone(), t4.clone(), Reason::LetBody),
            constraint(t3.clone(), t2.clone(), Reason::LetBinding),
        ];

        let value = Box::new(identifier(t3, "val"));
        let body = Box::new(identifier(t4, "body"));
        let let_ = typed(
            t1,
            TypedTermKind::Let {
                binding: TypeBinder::new("b".to_string(), t2),
                value,
                body,
            },
        );

        assert_eq!(collect(&let_), expected);
    }
}
