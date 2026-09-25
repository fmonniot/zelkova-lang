//! Lowers a `case`'s branches into an explicit decision tree, so a backend walks one
//! rather than re-deriving, at emission time, which test distinguishes which branch and
//! what order they run in (`GEN-5`; see `docs/tickets/README.md`'s tombstone row).
//!
//! [`build`] is the pass. It reads a `case`'s branches exactly as
//! [`super::TypedTermKind::Case`] carries them — a `Vec<(TermPattern, Box<TypedTerm>)>`
//! in source order — and answers with the [`Decision`] a backend walks instead.
//! `TypedTermKind::Case`'s own shape does not change: the typer still wants a flat list,
//! to generate one set of constraints per branch, all against the same scrutinee type,
//! with an order that does not matter to it. This is a second, additional view of the
//! same branches, built once a backend is ready to emit them.
//!
//! # A chain, not a table
//!
//! [Conditional evaluation](../../../docs/spec/evaluation-semantics.md#conditional-evaluation)
//! tries a `case`'s branches in the order written and commits to the first that
//! matches, never another. [`build`] follows that literally. A [`Decision::Test`] checks
//! one value against one [`Outcome`]: `matched` is the rest of the same branch, and
//! `default` is the tree for every branch after it — a chain, not a table grouping every
//! branch that tests the same value into one node. A backend that wants a `switch` can
//! flatten a run of `Test`s on the same occurrence itself.
//!
//! Two branches naming the same constructor, `Cons a _ -> a` then `Cons _ b -> b`, each
//! get a `Test` of their own, the second in the first's `default`. The second is dead —
//! any `Cons` value takes the first — but it is still built: telling that it is dead is
//! coverage checking's question (`LANG-19`), not this pass's.
//!
//! # Recursive over a pattern's sub-patterns
//!
//! A branch's pattern is lowered by walking it, and then its sub-patterns, depth first
//! and left to right, each at the [`Occurrence`] that leads to it from the scrutinee. A
//! wildcard tests nothing and binds nothing. A variable binds the value at its
//! occurrence. A tuple tests nothing, since a value of a tuple type is always a tuple,
//! and goes on to its elements. A literal or a constructor is a `Test` at its
//! occurrence; a constructor then goes on to its arguments. Every `Test` on the way down
//! falls back, as its `default`, to the tree for the branches after this one: a pattern
//! that fails part-way through fails as a whole.
//!
//! That fallback tree is copied into each such `default` rather than shared, since a
//! [`Decision`] is a tree and not a graph. One level deep, as every pattern is today,
//! that is one copy per branch; a pattern with several refutable sub-patterns copies it
//! once per refutable sub-pattern.
//!
//! Today `typer::translate_pattern` admits only a name or `_` below the top of a pattern
//! (`LANG-16`), so no `Test` below [`Occurrence::Root`] is ever built from real source,
//! and every binding is at most one step deep. Nothing here assumes it: the day
//! `LANG-16` lifts that refusal, a nested pattern lowers through the same walk. This
//! module's own tests build such a pattern by hand to pin that.

use crate::compiler::name::Name;

use super::{Constructor, LiteralValue, TermPattern, TermPatternKind, Type, TypedTerm};

/// A path to one value reached from a `case`'s scrutinee: the scrutinee itself, or one
/// step inside another occurrence.
///
/// See this module's doc comment for why a path of more than one [`Step`] can be built
/// here but is never built from real source today.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Occurrence {
    /// The scrutinee itself.
    Root,
    /// The value one step inside another occurrence.
    At(Box<Occurrence>, Step),
}

impl Occurrence {
    /// The occurrence one step further in: `self` is a constructor or a tuple, and
    /// `step` says which of its positions.
    pub fn field(&self, step: Step) -> Occurrence {
        Occurrence::At(Box::new(self.clone()), step)
    }
}

/// One step of an [`Occurrence`]: which sub-value of a constructor or a tuple, counted
/// from zero the way [`Constructor::index`] counts a case.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Step {
    ConstructorArgument(usize),
    TupleElement(usize),
}

/// The value a [`Decision::Test`] checks for.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// The value was built by this constructor.
    Constructor(Constructor),
    /// The value equals this `Int`, `Char` or `Bool`.
    Literal(LiteralValue),
}

/// One name a [`Decision::Leaf`] binds: the value at `occurrence`, of type `tpe`.
///
/// The type is the solved one the pattern carried. It is here because a backend with
/// more than one representation — WebAssembly's `i64`, `f64` and reference — reads
/// which one a value has off its type, and the leaf gives it no other way back to that
/// type short of working it out again from the scrutinee's.
#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub name: String,
    pub occurrence: Occurrence,
    pub tpe: Type,
}

/// The tree a `case`'s branches lower to: what test distinguishes which branch, in what
/// order they are tried, and the names each leaf binds.
///
/// `'a` is the lifetime of the [`TypedTerm`] the branches were built from: a leaf
/// borrows its branch's body rather than cloning it, since [`TypedTerm`] carries no
/// `Clone` impl. A body can appear in more than one leaf once a fallback tree is copied
/// (see this module's doc comment), which a borrow makes free.
///
/// Two trees are equal when they have the same shape and each pair of leaves borrows
/// the *same* body — compared by address, since [`TypedTerm`] has no `PartialEq`. That
/// is the equality a test wants: the leaf is the one built from this branch.
#[derive(Debug, Clone)]
pub enum Decision<'a> {
    /// Check the value at `scrutinee` against `outcome`: `matched` if it is that value,
    /// `default` if it is not.
    Test {
        scrutinee: Occurrence,
        outcome: Outcome,
        matched: Box<Decision<'a>>,
        default: Box<Decision<'a>>,
    },
    /// A branch matched: its bindings, each already named and given the occurrence to
    /// read it from, and its body.
    Leaf {
        bindings: Vec<Binding>,
        body: &'a TypedTerm,
    },
    /// No branch matches.
    ///
    /// [Coverage is checked, so no such `case`
    /// compiles](../../../docs/spec/evaluation-semantics.md#two-outcomes) — but nothing
    /// checks it yet (`LANG-19`), so a `case` missing a branch compiles today and
    /// reaches this leaf at runtime. It is not deleted the day `LANG-19` lands: an
    /// unreachable leaf still has to
    /// [abort](../../../docs/spec/evaluation-semantics.md#when-a-program-aborts) rather
    /// than fall through to whatever follows it, and this is that leaf — merely one
    /// exhaustiveness will have made unreachable, rather than one still worth reaching
    /// for.
    Fail {
        /// The declaration the `case` was written in: what the description `$abort`
        /// takes (`runtime/js/zelkova.mjs`, added by `GEN-8`) is built from.
        ///
        /// This is whatever [`build`]'s caller passed, not something the tree finds:
        /// nothing in a `case`'s branches says which declaration they sit in, so naming
        /// it is the caller's contract — a backend walking a declaration's body has
        /// that declaration's name at hand. Neither the `$abort` call nor the
        /// description's exact wording is this pass's to write; emitting a `Decision`
        /// is `GEN-10`'s.
        declaration: Name,
    },
}

impl PartialEq for Decision<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Decision::Test {
                    scrutinee,
                    outcome,
                    matched,
                    default,
                },
                Decision::Test {
                    scrutinee: other_scrutinee,
                    outcome: other_outcome,
                    matched: other_matched,
                    default: other_default,
                },
            ) => {
                scrutinee == other_scrutinee
                    && outcome == other_outcome
                    && matched == other_matched
                    && default == other_default
            }
            (
                Decision::Leaf { bindings, body },
                Decision::Leaf {
                    bindings: other_bindings,
                    body: other_body,
                },
            ) => bindings == other_bindings && std::ptr::eq(*body, *other_body),
            (
                Decision::Fail { declaration },
                Decision::Fail {
                    declaration: other_declaration,
                },
            ) => declaration == other_declaration,
            _ => false,
        }
    }
}

/// Lower one `case`'s branches, in source order, into the [`Decision`] a backend walks.
///
/// `scrutinee` is the type of the value the `case` matches on — the solved type of
/// [`super::TypedTermKind::Case`]'s own `scrutinee` — which a variable written as a
/// whole branch's pattern binds. `branches` is that `case`'s own, in the same order. And
/// `declaration` names the value the `case` was written inside of, which is what a
/// [`Decision::Fail`] leaf, if the tree needs one, carries (see that field's doc).
pub fn build<'a>(
    scrutinee: &Type,
    branches: &'a [(TermPattern, Box<TypedTerm>)],
    declaration: &Name,
) -> Decision<'a> {
    let Some(((pattern, body), rest)) = branches.split_first() else {
        return Decision::Fail {
            declaration: declaration.clone(),
        };
    };

    let on_fail = build(scrutinee, rest, declaration);
    lower(
        vec![(Occurrence::Root, scrutinee.clone(), pattern)],
        Vec::new(),
        body,
        &on_fail,
    )
}

/// One branch's pattern, still to be lowered: the sub-patterns not yet walked, each at
/// its occurrence and with the type of the value there, as a stack whose next entry is
/// its last; the bindings gathered so far; the branch's body; and the tree for the
/// branches after it, which every `Test` falls back to.
fn lower<'a>(
    mut pending: Vec<(Occurrence, Type, &'a TermPattern)>,
    mut bindings: Vec<Binding>,
    body: &'a TypedTerm,
    on_fail: &Decision<'a>,
) -> Decision<'a> {
    let Some((occurrence, tpe, pattern)) = pending.pop() else {
        return Decision::Leaf { bindings, body };
    };

    match &pattern.kind {
        TermPatternKind::Anything => lower(pending, bindings, body, on_fail),
        TermPatternKind::Bind(name) => {
            bindings.push(Binding {
                name: name.clone(),
                occurrence,
                tpe,
            });
            lower(pending, bindings, body, on_fail)
        }
        TermPatternKind::Tuple { elements } => {
            let elements: Vec<_> = elements.iter().enumerate().collect();
            for (position, element) in elements.into_iter().rev() {
                pending.push((
                    occurrence.field(Step::TupleElement(position)),
                    element.tpe.clone(),
                    &element.pattern,
                ));
            }
            lower(pending, bindings, body, on_fail)
        }
        TermPatternKind::Literal { value, .. } => Decision::Test {
            matched: Box::new(lower(pending, bindings, body, on_fail)),
            default: Box::new(on_fail.clone()),
            outcome: Outcome::Literal(*value),
            scrutinee: occurrence,
        },
        TermPatternKind::Constructor { ctor, args, .. } => {
            for (position, arg) in args.iter().enumerate().rev() {
                pending.push((
                    occurrence.field(Step::ConstructorArgument(position)),
                    arg.tpe.clone(),
                    &arg.pattern,
                ));
            }
            Decision::Test {
                matched: Box::new(lower(pending, bindings, body, on_fail)),
                default: Box::new(on_fail.clone()),
                outcome: Outcome::Constructor(ctor.clone()),
                scrutinee: occurrence,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    //! Nothing in real source reaches the recursion below the top of a pattern yet
    //! (`LANG-16`), so these build a nested [`TermPattern`] by hand, the shape
    //! `typer::translate_pattern` will produce once it admits one, and pin what
    //! [`build`] makes of it.

    use super::*;
    use crate::compiler::ir::{SubPattern, TypedTermKind};
    use crate::compiler::name::QualName;
    use crate::compiler::position::NodeSpan;
    use crate::compiler::tuple::Tuple;
    use crate::compiler::typer::TypeLiteral;

    fn int() -> Type {
        Type::Literal(TypeLiteral::Int)
    }

    fn maybe(of: Type) -> Type {
        Type::Adt(QualName::in_module("Maybe", "Maybe"), vec![of])
    }

    fn pattern(kind: TermPatternKind) -> TermPattern {
        TermPattern {
            span: NodeSpan::none(),
            kind,
        }
    }

    fn sub(tpe: Type, kind: TermPatternKind) -> SubPattern {
        SubPattern {
            tpe,
            pattern: pattern(kind),
        }
    }

    fn just() -> Constructor {
        Constructor {
            union: QualName::in_module("Maybe", "Maybe"),
            name: Name::new("Just"),
            index: 0,
            arity: 1,
        }
    }

    fn just_pattern(of: Type, arg: SubPattern) -> TermPatternKind {
        TermPatternKind::Constructor {
            ctor: just(),
            adt_args: vec![of],
            args: vec![arg],
        }
    }

    fn body(value: i64) -> Box<TypedTerm> {
        Box::new(TypedTerm {
            span: NodeSpan::none(),
            tpe: int(),
            kind: TypedTermKind::Int(value),
        })
    }

    /// `Just (Just x) -> 1 ; _ -> 0` on a `Maybe (Maybe Int)`: the inner `Just` is a
    /// `Test` of its own, one step below the root, whose `default` is the wildcard
    /// branch's leaf — the same tree the outer `Test` falls back to — and `x` is bound
    /// two steps down, at the inner argument's type.
    ///
    /// Mutation-checked by having `lower`'s `Constructor` arm bind each argument's
    /// sub-pattern at `occurrence` rather than at `occurrence.field(..)`: the inner
    /// `Test`'s scrutinee comes out as the root, and the assertion goes red.
    #[test]
    fn a_nested_constructor_is_a_test_below_the_root() {
        let inner = sub(
            maybe(int()),
            just_pattern(int(), sub(int(), TermPatternKind::Bind("x".to_string()))),
        );
        let branches = vec![
            (pattern(just_pattern(maybe(int()), inner)), body(1)),
            (pattern(TermPatternKind::Anything), body(0)),
        ];

        let tree = build(&maybe(maybe(int())), &branches, &Name::new("f"));

        let fallback = Decision::Leaf {
            bindings: vec![],
            body: &branches[1].1,
        };
        let argument = Occurrence::Root.field(Step::ConstructorArgument(0));
        assert_eq!(
            tree,
            Decision::Test {
                scrutinee: Occurrence::Root,
                outcome: Outcome::Constructor(just()),
                matched: Box::new(Decision::Test {
                    scrutinee: argument.clone(),
                    outcome: Outcome::Constructor(just()),
                    matched: Box::new(Decision::Leaf {
                        bindings: vec![Binding {
                            name: "x".to_string(),
                            occurrence: argument.field(Step::ConstructorArgument(0)),
                            tpe: int(),
                        }],
                        body: &branches[0].1,
                    }),
                    default: Box::new(fallback.clone()),
                }),
                default: Box::new(fallback),
            }
        );
    }

    /// `(1, y, 2) -> 1 ; _ -> 0` on an `(Int, Int, Int)`: the tuple itself is not
    /// tested, its first and third elements are, in that order, and `y` is bound only
    /// once both tests have passed. Each of the two `Test`s falls back to the wildcard
    /// branch's leaf, its own copy of it.
    ///
    /// Mutation-checked by pushing a tuple's elements onto `lower`'s stack in source
    /// order rather than reversed, so they are walked right to left: the third
    /// element's `Test` then comes first, and the assertion goes red.
    #[test]
    fn literals_inside_a_tuple_are_tested_at_their_elements_left_to_right() {
        let literal = |value| {
            sub(
                int(),
                TermPatternKind::Literal {
                    tpe: int(),
                    value: LiteralValue::Int(value),
                },
            )
        };
        let tuple = TermPatternKind::Tuple {
            elements: Tuple::three(
                literal(1),
                sub(int(), TermPatternKind::Bind("y".to_string())),
                literal(2),
            ),
        };
        let branches = vec![
            (pattern(tuple), body(1)),
            (pattern(TermPatternKind::Anything), body(0)),
        ];

        let tree = build(
            &Type::Tuple(Tuple::three(int(), int(), int())),
            &branches,
            &Name::new("f"),
        );

        let fallback = Decision::Leaf {
            bindings: vec![],
            body: &branches[1].1,
        };
        let element = |position| Occurrence::Root.field(Step::TupleElement(position));
        assert_eq!(
            tree,
            Decision::Test {
                scrutinee: element(0),
                outcome: Outcome::Literal(LiteralValue::Int(1)),
                matched: Box::new(Decision::Test {
                    scrutinee: element(2),
                    outcome: Outcome::Literal(LiteralValue::Int(2)),
                    matched: Box::new(Decision::Leaf {
                        bindings: vec![Binding {
                            name: "y".to_string(),
                            occurrence: element(1),
                            tpe: int(),
                        }],
                        body: &branches[0].1,
                    }),
                    default: Box::new(fallback.clone()),
                }),
                default: Box::new(fallback),
            }
        );
    }
}
