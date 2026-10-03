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
//!   [`Origin::left_from`]). Collecting children first inverts that: the body settles
//!   on its own type first and the mismatch surfaces at the whole function.
//! - the sides of a constraint are ordered declared-first where the source has a
//!   declared side, because that is the order the headline reads them out in (see
//!   [`Constraint`]).
//!
//! A field access, an update's fields, an accessor and a record pattern's entries say
//! something about a record type that is not an equation, and are collected into a list of
//! their own, read after the equations are solved — see [`FieldConstraint`]. That list is
//! in the order the labels were written, which is not the first rule's order: an access's
//! record comes before its own label.

use super::{
    bool_type, CaseForm, Constraint, FieldConstraint, Reason, RecordUse, SubPattern, TermPattern,
    TermPatternKind, Type, TypeLiteral, TypedTerm, TypedTermKind,
};
use zelkova_syntax::tuple::Tuple;

pub(super) fn collect(term: &TypedTerm) -> Constraints {
    let mut constraints = Constraints::default();
    walk(term, &mut constraints);
    constraints
}

/// Push `term`'s constraints, then its children's, onto `out`.
fn walk(term: &TypedTerm, out: &mut Constraints) {
    let tpe = &term.tpe;
    let span = term.span;

    match &term.kind {
        TypedTermKind::Int(_) => {
            // Integer literals are polymorphic numeric values: they can unify
            // with Int or Float (but not Bool, Char, etc.).
            out.equations.push(Constraint::new(
                tpe.clone(),
                Type::Number,
                Reason::Literal,
                span,
            ));
        }
        TypedTermKind::Char(_) => {
            out.equations.push(Constraint::new(
                tpe.clone(),
                Type::Literal(TypeLiteral::Char),
                Reason::Literal,
                span,
            ));
        }
        TypedTermKind::String(_) => {
            out.equations.push(Constraint::new(
                tpe.clone(),
                Type::Literal(TypeLiteral::String),
                Reason::Literal,
                span,
            ));
        }
        TypedTermKind::Float(_) => {
            out.equations.push(Constraint::new(
                tpe.clone(),
                Type::Literal(TypeLiteral::Float),
                Reason::Literal,
                span,
            ));
        }
        TypedTermKind::Unit => {
            out.equations
                .push(Constraint::new(tpe.clone(), Type::Unit, Reason::Unit, span));
        }
        TypedTermKind::Fun { param, body } => {
            let param_tpe = Box::new(param.tpe.clone());
            let return_tpe = Box::new(body.tpe.clone());
            out.equations.push(Constraint::new(
                tpe.clone(),
                Type::Fun {
                    param_tpe,
                    return_tpe,
                },
                Reason::FunctionShape,
                span,
            ));

            walk(body, out);
        }
        TypedTermKind::Identifier(_) => (),
        // A name that did not resolve constrains nothing: its type is solved by whatever
        // constrains the node around it. Its type is remembered all the same, for the one
        // question asked of it later (see `Constraints::holes`).
        TypedTermKind::Hole => out.holes.push(tpe.clone()),
        TypedTermKind::Apply { fun, arg, .. } => {
            let param_tpe = Box::new(arg.tpe.clone());
            let return_tpe = Box::new(tpe.clone());
            // The span is the *applied* expression's, not the whole application's:
            // "the expression being applied" is only useful pointing at the thing
            // being applied.
            out.equations.push(Constraint::new(
                fun.tpe.clone(),
                Type::Fun {
                    param_tpe,
                    return_tpe,
                },
                Reason::Application,
                fun.span,
            ));

            walk(fun, out);
            walk(arg, out);
        }
        TypedTermKind::If {
            cond,
            true_branch,
            false_branch,
        } => {
            // If put a constraint on the condition and the branches should resolve to the same type
            out.equations.push(Constraint::new(
                cond.tpe.clone(),
                bool_type(),
                Reason::IfCondition,
                cond.span,
            ));
            out.equations.push(Constraint::new(
                true_branch.tpe.clone(),
                tpe.clone(),
                Reason::IfBranch,
                true_branch.span,
            ));
            out.equations.push(Constraint::new(
                false_branch.tpe.clone(),
                tpe.clone(),
                Reason::IfBranch,
                false_branch.span,
            ));

            walk(cond, out);
            walk(true_branch, out);
            walk(false_branch, out);
        }
        TypedTermKind::Let {
            binding,
            value,
            body,
        } => {
            // The let expression has the body type.
            out.equations.push(Constraint::new(
                tpe.clone(),
                body.tpe.clone(),
                Reason::LetBody,
                span,
            ));
            // The binding type is the one of the value. Written value-first so that
            // the side named by the span — the value — is `left`.
            out.equations.push(Constraint::new(
                value.tpe.clone(),
                binding.tpe.clone(),
                Reason::LetBinding,
                value.span,
            ));

            walk(value, out);
            walk(body, out);
        }
        TypedTermKind::Case {
            scrutinee,
            branches,
            form,
        } => {
            // A parameter written as a pattern is matched the way a `case` is, and a
            // mismatch there is reported as the parameter's rather than a `case`'s:
            // its pattern is the parameter's, and its one branch is the declaration's
            // body.
            let (pattern_reason, branch_reason) = match form {
                CaseForm::Expression => (Reason::CasePattern, Reason::CaseBranch),
                CaseForm::Parameter => (Reason::ParameterPattern, Reason::DeclarationBody),
            };

            // Every branch's own constraints, all of them, before any child is walked
            // — including the scrutinee, which is a child like the branch bodies are.
            // Walking it first was this arm's one departure from the rule the module
            // doc states, and it showed: a `case` on a compound expression reported
            // its pattern mismatch against a constraint from inside the scrutinee
            // rather than against the pattern.
            for (pattern, body) in branches {
                // Each pattern constrains the scrutinee type. The pattern is what the
                // caret should sit under, so the pattern's type is `left`.
                pattern_constraints(pattern, &scrutinee.tpe, pattern_reason, out);
                // Every branch must return the case expression's type. Pushed after the
                // pattern's constraint, which is what links the names the pattern binds
                // to the scrutinee: a tuple pattern's elements are fresh variables until
                // it is solved, and a branch constraint solved first would settle them
                // from the `case`'s type and blame the pattern for a wrong body.
                out.equations.push(Constraint::new(
                    body.tpe.clone(),
                    tpe.clone(),
                    branch_reason,
                    body.span,
                ));
            }

            walk(scrutinee, out);
            for (_, body) in branches {
                walk(body, out);
            }
        }
        TypedTermKind::Tuple(elements) => {
            // The tuple type must equal the tuple of its element types.
            let element_types = match elements {
                Tuple::Two(a, b) => Tuple::two(a.tpe.clone(), b.tpe.clone()),
                Tuple::Three(a, b, c) => Tuple::three(a.tpe.clone(), b.tpe.clone(), c.tpe.clone()),
            };
            out.equations.push(Constraint::new(
                tpe.clone(),
                Type::Tuple(element_types),
                Reason::TupleElements,
                span,
            ));

            for elem in elements.iter() {
                walk(elem, out);
            }
        }
        TypedTermKind::Record(fields) => {
            // The record's type is the set of its fields' types. Labels are unique —
            // canonicalization reports a repeated one — so no field is lost to the map.
            let field_types = fields
                .iter()
                .map(|field| (field.label.clone(), field.value.tpe.clone()))
                .collect();
            out.equations.push(Constraint::new(
                tpe.clone(),
                Type::Record(field_types),
                Reason::RecordFields,
                span,
            ));

            for field in fields {
                walk(&field.value, out);
            }
        }
        TypedTermKind::Update { record, fields } => {
            // The one equation an update writes: it has the type of the record it
            // updates. What it says about that type's fields is not an equation, and is
            // read once unification has run (see `FieldConstraint`).
            out.equations.push(Constraint::new(
                tpe.clone(),
                record.tpe.clone(),
                Reason::Update,
                span,
            ));

            walk(record, out);
            for field in fields {
                out.fields.push(FieldConstraint {
                    record: record.tpe.clone(),
                    label: field.label.clone(),
                    field: field.value.tpe.clone(),
                    form: RecordUse::Update,
                    form_span: span,
                    label_span: field.label_span,
                    field_span: field.value.span,
                });
                walk(&field.value, out);
            }
        }
        TypedTermKind::Access {
            record,
            label,
            label_span,
        } => {
            // No equation at all: the access's type is the field's, and the field is
            // only known once the record's type is (see `FieldConstraint`). The record
            // is walked first, so that the field constraints of a chain `r.a.b` are in
            // the order their labels were written.
            walk(record, out);
            out.fields.push(FieldConstraint {
                record: record.tpe.clone(),
                label: label.clone(),
                field: tpe.clone(),
                form: RecordUse::Access,
                form_span: span,
                label_span: *label_span,
                field_span: span,
            });
        }
        TypedTermKind::Accessor { label, .. } => {
            // `annotate` gives an accessor the type `record -> field` and no other, so
            // the two halves are read back off it here.
            if let Type::Fun {
                param_tpe,
                return_tpe,
            } = tpe
            {
                out.fields.push(FieldConstraint {
                    record: *param_tpe.clone(),
                    label: label.clone(),
                    field: *return_tpe.clone(),
                    form: RecordUse::Accessor,
                    form_span: span,
                    label_span: span,
                    field_span: span,
                });
            }
        }
    };
}

/// Everything a declaration's term requires of its types: the equations `unify` solves
/// in order, and the field constraints read after them.
///
/// The two are kept apart because they are solved apart — see [`FieldConstraint`] for
/// why a field constraint is not an equation. Each list is in the order it was
/// collected, which for the equations is the order the module doc states, and for the
/// field constraints the order their labels were written.
#[derive(Debug, Default)]
pub(super) struct Constraints {
    pub(super) equations: Vec<Constraint>,
    pub(super) fields: Vec<FieldConstraint>,
    /// The type of every hole in the term, and of every argument of a constructor pattern
    /// that did not resolve. A field constraint whose record type nothing supplied, and
    /// which the name's real type could have, is not reported: the error that the name
    /// did not resolve already stands behind it, and is the one the user has to fix. `unifier::unknown` says which constraints those are, and why only
    /// those ([`DEC-23` decisions 3 and
    /// 6](../../../docs/decisions/dec-23.md#6--an-unresolved-name-inside-a-sound-body-is-a-typed-hole)).
    pub(super) holes: Vec<Type>,
}

/// The constraints a pattern places on `against`, the type of the value it is matched
/// against: the scrutinee's for a branch's own pattern, and the type its position
/// carries for a constructor's argument, a tuple's element or a record pattern's entry.
///
/// The pattern's own constraint comes first, then its sub-patterns', left to right and
/// depth first. A variable or `_` constrains nothing: a variable's type is `against`
/// already, as `TermPattern::bindings` gives it. A constructor that did not resolve
/// constrains nothing of its own either, its arguments are constrained as a
/// constructor's are, and their types are holes (see [`Constraints::holes`]). A
/// sub-pattern is held to the type of its position exactly as a branch's pattern is held
/// to the scrutinee's, so a nested constructor of the wrong type is blamed on the nested
/// pattern, at its own span.
///
/// A record pattern's own constraint is not an equation: it names a subset of the
/// record's fields, so it cannot build the record type `against` has to equal. Each entry
/// is a [`FieldConstraint`] on `against` instead, pushed onto the field list in the order
/// the entries were written, read once the equations are solved.
fn pattern_constraints(
    pattern: &TermPattern,
    against: &Type,
    reason: Reason,
    out: &mut Constraints,
) {
    let (own, subs): (Option<Type>, Vec<&SubPattern>) = match &pattern.kind {
        TermPatternKind::Literal { tpe, .. } => (Some(tpe.clone()), vec![]),
        TermPatternKind::Unit => (Some(Type::Unit), vec![]),
        TermPatternKind::Constructor {
            ctor,
            adt_args,
            args,
        } => (
            Some(Type::Adt(ctor.union.clone(), adt_args.clone())),
            args.iter().collect(),
        ),
        TermPatternKind::Tuple { elements } => (
            Some(Type::Tuple(elements.map(|element| element.tpe.clone()))),
            elements.iter().collect(),
        ),
        TermPatternKind::Bind(_) | TermPatternKind::Anything => (None, vec![]),
        // A constructor that did not resolve says nothing about the type it would have
        // built, and its arguments are constrained as a constructor's are. What it would
        // have said about each argument is the hole's: a record pattern written as one
        // has its record type in what the constructor's real type would have supplied.
        TermPatternKind::Hole { args } => {
            out.holes.extend(args.iter().map(|arg| arg.tpe.clone()));
            (None, args.iter().collect())
        }
        TermPatternKind::Record { fields } => {
            for field in fields {
                out.fields.push(FieldConstraint {
                    record: against.clone(),
                    label: field.label.clone(),
                    field: field.value.tpe.clone(),
                    form: RecordUse::Pattern,
                    form_span: pattern.span,
                    label_span: field.label_span,
                    field_span: field.value.pattern.span,
                });
            }
            (None, fields.iter().map(|field| &field.value).collect())
        }
    };

    if let Some(own) = own {
        out.equations
            .push(Constraint::new(own, against.clone(), reason, pattern.span));
    }
    for sub in subs {
        pattern_constraints(&sub.pattern, &sub.tpe, reason, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Reference, Saturation};
    use crate::typer::*;
    use zelkova_syntax::position::NodeSpan;

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
        typed(tpe, TypedTermKind::Identifier(Reference::local(name)))
    }

    fn collect_equations(term: &TypedTerm) -> Vec<Constraint> {
        collect(term).equations
    }

    fn constraint(left: Type, right: Type, reason: Reason) -> Constraint {
        Constraint::new(left, right, reason, NodeSpan::none())
    }

    /// `Basics.Bool`, written out rather than taken from [`bool_type`].
    ///
    /// Which package and module declared the union is the whole of its identity, so an
    /// assertion built from the function under test would hold for any name that
    /// function picked — including a bare `Bool`, which would make every module's own
    /// `Bool` an `if` condition ([`DEC-15`](../../../docs/decisions/dec-15.md) decision 1).
    fn basics_bool() -> Type {
        Type::Adt(
            crate::name::QualName::in_module(crate::PackageName::core(), "Basics", "Bool"),
            vec![],
        )
    }

    #[test]
    fn constrains_int() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        // Integer literals constrain to Number (polymorphic: can be Int or Float)
        let expected = vec![constraint(t1.clone(), Type::Number, Reason::Literal)];

        let int = typed(t1, TypedTermKind::Int(42));

        assert_eq!(collect_equations(&int), expected);
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

        assert_eq!(collect_equations(&fun), expected);
    }

    #[test]
    fn constrains_variable() {
        let t1 = Type::Variable(TypeVariable { id: 1 });

        let b = identifier(t1, "a");

        assert_eq!(collect_equations(&b), vec![]);
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
                saturation: Saturation::Partial,
            },
        );

        assert_eq!(collect_equations(&apply), expected);
    }

    #[test]
    fn constrains_if() {
        let t1 = Type::Variable(TypeVariable { id: 1 });
        let t2 = Type::Variable(TypeVariable { id: 2 });
        let t3 = Type::Variable(TypeVariable { id: 3 });
        let t4 = Type::Variable(TypeVariable { id: 4 });

        // t2 === Basics.Bool (eg. the condition needs to be a boolean)
        // t3 === t1   (eg. the if type is the same as the first branch)
        // t4 === t1   (eg. the if type is the same as the second branch)
        //
        // The reasons are asserted too: they are what a diagnostic says out loud, and
        // a condition reported as "every branch must have the same type" would be a
        // lie the types alone cannot catch.
        let expected = vec![
            constraint(t2.clone(), basics_bool(), Reason::IfCondition),
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

        assert_eq!(collect_equations(&if_else), expected);
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

        assert_eq!(collect_equations(&let_), expected);
    }
}
