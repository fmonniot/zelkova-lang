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
//! matches, never another. [`build`] follows that literally: it looks at the first
//! branch, and if it can fail to match, that becomes one `Switch` edge whose `default`
//! is the tree for everything after it — a chain, not a table grouping every branch
//! that tests the same value into one node. Two branches naming the same constructor,
//! `Cons a _ -> a` then `Cons _ b -> b`, still answer to whichever is tried first: the
//! second `Cons` never gets its own edge, because [`build`] has already produced a leaf
//! for the first one and does not look past it.
//!
//! # What a leaf binds
//!
//! A wildcard or a variable pattern ends the chain outright — every value matches it, so
//! nothing written after it is ever reached, and [`build`] does not descend into the
//! rest of the branches once it has produced that leaf. A tuple pattern does the same:
//! [tuple patterns are irrefutable](../../../docs/spec/patterns.md#a-pattern-that-can-fail-and-one-that-cannot)
//! today, since an element may only be a name or `_` (`LANG-16` has not landed), so
//! there is nothing to test and every element becomes a binding at its own
//! [`Occurrence`] instead.
//!
//! # One level, on purpose
//!
//! [`Occurrence`] is a path of any length, and [`build`] is written to recurse rather
//! than to assume a fixed depth — but nothing it reads has a second level to descend
//! into yet: a constructor's argument or a tuple's element is always a name or `_`,
//! never a further pattern (`LANG-16`). So every occurrence [`build`] produces today is
//! one step long, and the day a constructor pattern nests, teaching [`build`] to
//! recurse into that argument's own pattern — rather than always binding it — is the
//! only change; [`Occurrence`] already has the shape to carry the result.

use crate::compiler::name::Name;

use super::{Constructor, LiteralValue, TermPattern, TermPatternKind, TypedTerm};

/// A path to one value reached from a `case`'s scrutinee: the scrutinee itself, or one
/// step inside it.
///
/// See this module's doc comment for why a path of more than one [`Step`] is possible
/// here but never produced today.
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

/// Which value, of however many a [`Decision::Switch`] can see at its `scrutinee`, one
/// `edge` was built for.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// The value was built by this constructor.
    Constructor(Constructor),
    /// The value equals this `Int`, `Char` or `Bool`.
    Literal(LiteralValue),
}

/// The tree a `case`'s branches lower to: what test distinguishes which branch, in what
/// order they are tried, and the names each leaf binds.
///
/// `'a` is the lifetime of the [`TypedTerm`] the branches were built from: a leaf
/// borrows its branch's body rather than cloning it, since [`TypedTerm`] carries no
/// `Clone` impl and a branch's body is read from exactly one leaf.
#[derive(Debug)]
pub enum Decision<'a> {
    /// Test the value at `scrutinee` and try `edges` in order; the first whose
    /// [`Outcome`] the value matches is taken, `default` when none does.
    Switch {
        scrutinee: Occurrence,
        edges: Vec<(Outcome, Decision<'a>)>,
        default: Box<Decision<'a>>,
    },
    /// A branch matched: its bindings, each already named and given the occurrence to
    /// read it from, and its body.
    Leaf {
        bindings: Vec<(String, Occurrence)>,
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
        /// takes (`runtime/js/zelkova.mjs`, added by `GEN-8`) is built from. Neither
        /// the call nor the description's exact wording is this pass's to write —
        /// emitting a `Decision` is `GEN-10`'s.
        declaration: Name,
    },
}

/// Lower one `case`'s branches, in source order, into the [`Decision`] a backend walks.
///
/// `branches` is a `case`'s own — [`super::TypedTermKind::Case`]'s field of the same
/// name, in the same order — and `declaration` names the value the `case` was written
/// inside of, which is what a [`Decision::Fail`] leaf, if the tree needs one, carries.
pub fn build<'a>(
    branches: &'a [(TermPattern, Box<TypedTerm>)],
    declaration: &Name,
) -> Decision<'a> {
    lower(Occurrence::Root, branches, declaration)
}

/// [`build`], at one occurrence — recursive over the branch list rather than over a
/// pattern's own sub-structure, since nothing a pattern carries today has one to
/// recurse into (see this module's doc comment).
fn lower<'a>(
    occurrence: Occurrence,
    branches: &'a [(TermPattern, Box<TypedTerm>)],
    declaration: &Name,
) -> Decision<'a> {
    let Some(((pattern, body), rest)) = branches.split_first() else {
        return Decision::Fail {
            declaration: declaration.clone(),
        };
    };

    match &pattern.kind {
        TermPatternKind::Anything => Decision::Leaf {
            bindings: Vec::new(),
            body,
        },
        TermPatternKind::Bind(name) => Decision::Leaf {
            bindings: vec![(name.clone(), occurrence)],
            body,
        },
        TermPatternKind::Tuple { bindings, .. } => Decision::Leaf {
            bindings: bindings
                .iter()
                .map(|(position, name, _)| {
                    (
                        name.clone(),
                        occurrence.field(Step::TupleElement(*position)),
                    )
                })
                .collect(),
            body,
        },
        TermPatternKind::Literal { value, .. } => Decision::Switch {
            edges: vec![(
                Outcome::Literal(*value),
                Decision::Leaf {
                    bindings: Vec::new(),
                    body,
                },
            )],
            default: Box::new(lower(occurrence.clone(), rest, declaration)),
            scrutinee: occurrence,
        },
        TermPatternKind::Constructor { ctor, bindings, .. } => Decision::Switch {
            edges: vec![(
                Outcome::Constructor(ctor.clone()),
                Decision::Leaf {
                    bindings: bindings
                        .iter()
                        .map(|(position, name, _)| {
                            (
                                name.clone(),
                                occurrence.field(Step::ConstructorArgument(*position)),
                            )
                        })
                        .collect(),
                    body,
                },
            )],
            default: Box::new(lower(occurrence.clone(), rest, declaration)),
            scrutinee: occurrence,
        },
    }
}
