//! This module contains the type checker pass of the language
//!
//! It works with the source AST and will perform two jobs:
//! - type checks the different declarations and expression
//! - infer the types when not declared in the source
//!
//! I have no idea how that works; so bear with me while I explore
//! the space, make mistake and (hopefully) learn something :)
//!
//! Some papers on type inference:
//! - <http://steshaw.org/hm/hindley-milner.pdf>
//! - <https://pdfs.semanticscholar.org/8983/233b3dff2c5b94efb31235f62bddc22dc899.pdf>
//! - <http://gallium.inria.fr/~fpottier/publis/fpottier-elaboration.pdf>
//! - <http://gallium.inria.fr/~fpottier/publis/emlti-final.pdf>
//!
//! A type inference problem consists of a type environment Γ , an expression t, and a type T of kind ?
//!
//! Constraint generation rules:
//!
//! - Equation 1: ⟦x : T⟧ = x ≼ T
//!   "x has type T if and only if T is an instance of the type scheme associated with x"
//!   Important part: There is no relation to the typing environment Γ, instead x appears free (and will be bound to Γ later)
//!
//! - Equation 2: ⟦λz.t : T⟧ = ∃X1X2.(let z : X1 in ⟦t : X2⟧ ∧ X1 → X2 ≤ T)
//!   "λz.t has type T if and only if, for some X1 and X2,
//!   (i) under the assumption that z has type X1, t has type X2, and
//!   (ii) T is a supertype of X1 → X2."
//!   z and t types must be fresh (can't generally guess them). They are _existentially_ bound because we are going to
//!   solve their values. Note that z is _not_ fresh in the condition (i).
//!
//! - Equation 3: ⟦t1 t2 : T⟧ = ∃X2.(⟦t1 : X2 → T⟧ ∧ ⟦t2 : X2⟧)
//!   "t1 t2 has type T if and only if, for some X2, t1 has type X2 → T and t2 has type X2"
//!
//! - Equation 4: ⟦let z = t1 in t2 : T⟧ = let z : ∀X[⟦t1 : X⟧].X in ⟦t2 : T⟧
//!   "let z = t1 in t2 has type T if and only if, under the assumption that z has every type X such that ⟦t1 : X⟧ holds, t2 has type T"
//!
//!
use super::canonical;
use super::canonical::Module;
use super::scalars;
use crate::ir::{
    pattern_parameter, CaseForm, Constructor, Field, LiteralValue, Predicate, Reference,
    ReferenceKind, Saturation, Solved, SolvedInstance, SubPattern, Term, TermKind, TermPattern,
    TermPatternKind, TypeBinder, TypedTerm, TypedTermKind,
};
use crate::name::{Name, QualName};
use crate::{Interface, ModuleName, PhaseError, SpanLabel};
use log::debug;
use std::collections::{BTreeMap, HashMap, HashSet};
use zelkova_syntax::position::NodeSpan;
use zelkova_syntax::tuple::Tuple;

// ── Provenance ────────────────────────────────────────────────────────────────

/// Why two types were required to match.
///
/// A constraint on its own is a pair of types, which is everything inference needs
/// and nothing a reader can act on. The reason is what turns "cannot match `Bool`
/// with `Int`" into something located: *this branch* has type `Bool`, and `Int` is
/// what was expected *because of this annotation*. Elm calls this a `Reason`, rustc
/// an `ObligationCause`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reason {
    /// A declaration's type annotation, which its body has to satisfy.
    Annotation,
    /// A literal's own type: `'a'` is a `Char`.
    Literal,
    /// A function's type is its parameter's type arrow its body's type.
    FunctionShape,
    /// What is applied has to be a function from the argument's type.
    Application,
    /// The condition of an `if` has to be a `Bool`.
    IfCondition,
    /// Every branch of an `if` has the type of the whole `if`.
    IfBranch,
    /// Every branch of a `case` has the type of the whole `case`.
    CaseBranch,
    /// A pattern has to match the type of the expression being matched on.
    CasePattern,
    /// A parameter written as a pattern has to match the type of the argument it takes.
    ParameterPattern,
    /// The body of a declaration that wrote a parameter as a pattern has the type the
    /// declaration returns. It is the one branch of the match that parameter became,
    /// and is reported as what the source wrote: a body, not a `case` branch.
    DeclarationBody,
    /// A `let` binding has the type of the value bound to it.
    LetBinding,
    /// A `let` has the type of its body.
    LetBody,
    /// A tuple's type is the tuple of its elements' types.
    TupleElements,
    /// `()` is the unit type's one value.
    Unit,
    /// A record's type is the record type of its fields' types.
    RecordFields,
    /// An update has the type of the record it updates.
    Update,
    /// A field's new value in an update has the type the field already has. Carried by
    /// the equation a `FieldConstraint` of an update becomes once its record type is
    /// known, at the value's span.
    UpdateField,
    /// A field access has the type of the field it reads. Carried by the equation a
    /// `FieldConstraint` of an access becomes, at the access's span.
    Access,
    /// An accessor's result has the type of the field it reads. Carried by the equation
    /// a `FieldConstraint` of an accessor becomes, at the accessor's span.
    Accessor,
    /// A record pattern's entry matches a value of the type its field has. Carried by the
    /// equation a `FieldConstraint` of a record pattern becomes, at the span of the
    /// entry's own pattern — `Celsius` in `{ taken = Celsius }` — when that pattern binds
    /// no name, so that nothing but the pattern decides the entry's type and a mismatch
    /// is the pattern's.
    RecordPatternEntry,
    /// [`RecordPatternEntry`](Self::RecordPatternEntry) for an entry whose pattern is a
    /// name, the binding in `{ taken }` or `{ taken = t }`. That name's type is constrained
    /// by nothing but the body's uses of it, so when the equation fails the pattern is
    /// not what disagrees with the field: a use is, and the note says so. The caret is
    /// still under the binding ([`ERR-20`](../../docs/tickets/err-20.md)).
    RecordPatternBinding,
    /// [`RecordPatternEntry`](Self::RecordPatternEntry) for an entry whose pattern is
    /// neither a name nor binding-free, `Box x` in `{ taken = Box x }`: either the pattern
    /// or a use of a name it binds can be what disagrees with the field, and nothing here
    /// tells which, so the note names both.
    RecordPatternEntryWithBindings,
    /// A use of a name whose type has a context. The context's constraints are
    /// instantiated at the types the use gave its variables, and each requires an
    /// instance of its class at that type. Carried by an `Obligation` and never by an
    /// equation, at the span of the use.
    InstanceRequired,
    /// An instance a use requires has a context of its own, and each of its constraints
    /// requires an instance in turn, at the type the instance was used at. Carried by an
    /// `Obligation` at the span of the use that started the chain.
    InstanceContext,
    /// A class with a superclass requires an instance of the superclass at every type it
    /// has an instance for. Carried by an `Obligation` at the span of the instance
    /// declaration.
    SuperclassInstance,
    /// A binding of an instance has the type its class gave the member, at the type the
    /// instance is for. It is the reason of the equation the member's signature stands in
    /// for, at the span of the instance's head line, where an annotation would be.
    InstanceMember,
    /// A binding of a derivation has the type the chapter gives it over the member's
    /// answer type. It is the reason of the equation that type stands in for, at the span
    /// of `derived member`, where an annotation would be.
    DerivationBinding,
}

impl Reason {
    /// What goes under the caret when the constraint carrying this reason is the one
    /// that failed: what the underlined text *is*, in the vocabulary of the source.
    ///
    /// It names no type, and that is not terseness. By the time a constraint fails,
    /// `unify` has substituted solutions into both of its sides and may have
    /// decomposed it into a component of the types the source actually mentions — so
    /// neither side is reliably "the type of the text under this caret" any more.
    /// The headline already prints both types; a label that named the wrong one
    /// would be worse than a label that names none. The example that forced this:
    /// `answer : Int` with body `'a'` fails on the literal's own constraint *after*
    /// `Int` was substituted into it, and reading the type off that side produced
    /// "this literal has type `Int`".
    fn describes(&self) -> &'static str {
        match self {
            Reason::Annotation => "this type annotation",
            Reason::Literal => "this literal",
            Reason::FunctionShape => "this function",
            Reason::Application => "the expression being applied",
            Reason::IfCondition => "this condition",
            Reason::IfBranch => "this branch of the `if`",
            Reason::CaseBranch => "this branch of the `case`",
            Reason::CasePattern => "this pattern",
            Reason::ParameterPattern => "this pattern",
            Reason::DeclarationBody => "the body of this declaration",
            Reason::LetBinding => "the value bound here",
            Reason::LetBody => "the body of this `let`",
            Reason::TupleElements => "this tuple",
            Reason::Unit => "this unit value",
            Reason::RecordFields => "this record",
            Reason::Update => "this update",
            Reason::UpdateField => "this field's new value",
            Reason::Access => "this field access",
            Reason::Accessor => "this accessor",
            Reason::RecordPatternEntry
            | Reason::RecordPatternBinding
            | Reason::RecordPatternEntryWithBindings => "this field's pattern",
            Reason::InstanceRequired => "this use requires an instance",
            Reason::InstanceContext => "this use requires an instance, through another's context",
            Reason::SuperclassInstance => "this instance requires an instance of its superclass",
            Reason::InstanceMember => "this instance",
            Reason::DerivationBinding => "this derivation",
        }
    }

    /// What goes under the caret when this reason is not the failure itself but the
    /// explanation for one side of it — see [`Origin::explanation`].
    ///
    /// No type is interpolated here, deliberately. Provenance records which
    /// constraint brought a type into another one; it does not prove that the type
    /// printed in the failing constraint is still literally the one this constraint
    /// carried, and a label is not the place to guess.
    fn explains(&self) -> &'static str {
        match self {
            Reason::Annotation => "expected because of this type annotation",
            Reason::Literal => "expected because of this literal",
            Reason::FunctionShape => "expected because of this function",
            Reason::Application => "expected because of this application",
            Reason::IfCondition => "expected because this is an `if` condition",
            Reason::IfBranch => "expected because of this branch",
            Reason::CaseBranch => "expected because of this branch",
            Reason::CasePattern => "expected because of this pattern",
            Reason::ParameterPattern => "expected because of this pattern",
            Reason::DeclarationBody => "expected because of this declaration's body",
            Reason::LetBinding => "expected because of this value",
            Reason::LetBody => "expected because of this `let` body",
            Reason::TupleElements => "expected because of this tuple",
            Reason::Unit => "expected because of this unit value",
            Reason::RecordFields => "expected because of this record",
            Reason::Update => "expected because of this update",
            Reason::UpdateField => "expected because of this field's new value",
            Reason::Access => "expected because of this field access",
            Reason::Accessor => "expected because of this accessor",
            Reason::RecordPatternEntry
            | Reason::RecordPatternBinding
            | Reason::RecordPatternEntryWithBindings => "expected because of this field's pattern",
            Reason::InstanceRequired => "an instance is required because of this use",
            Reason::InstanceContext => "an instance is required through another's context here",
            Reason::SuperclassInstance => "an instance is required because of this superclass",
            Reason::InstanceMember => "expected because this instance gives the member this type",
            Reason::DerivationBinding => {
                "expected because this derivation gives the binding this type"
            }
        }
    }

    /// The rule that was broken, when naming it says something the labels do not.
    fn note(&self) -> Option<&'static str> {
        match self {
            Reason::Annotation => {
                Some("a declaration's body must have the type its annotation declares")
            }
            Reason::IfCondition => Some("the condition of an `if` must be a `Bool`"),
            Reason::IfBranch => Some("every branch of an `if` must have the same type"),
            Reason::CaseBranch => Some("every branch of a `case` must have the same type"),
            Reason::CasePattern => Some(
                "every pattern of a `case` must match the type of the expression it matches on",
            ),
            Reason::ParameterPattern => {
                Some("a parameter's pattern must match the type of the argument it takes")
            }
            Reason::Update => Some("an update has the type of the record it updates"),
            Reason::UpdateField => {
                Some("an update cannot change a field's type: each new value must have the type its field already has")
            }
            Reason::Access => Some("a field access has the type of the field it reads"),
            Reason::Accessor => Some("an accessor returns the type of the field it reads"),
            Reason::RecordPatternEntry => {
                Some("each entry of a record pattern must match the type of the field it names")
            }
            Reason::RecordPatternBinding => Some(
                "a name a record pattern binds has the type of the field it names, and the body uses this one at another type",
            ),
            Reason::RecordPatternEntryWithBindings => Some(
                "either this entry does not match the type of the field it names, or the body uses a name it binds at another type than that field gives the name",
            ),
            Reason::InstanceMember => Some(
                "an instance's binding must have the type its class gives the member, at the type the instance is for",
            ),
            Reason::DerivationBinding => Some(
                "a derivation's binding has the type its role gives it over the type the member answers with",
            ),
            // The error that carries one of these says what is required and why, so a rule
            // note beside it would say it twice.
            Reason::InstanceRequired | Reason::InstanceContext | Reason::SuperclassInstance => {
                None
            }
            _ => None,
        }
    }
}

/// One piece of source text, and why it required a type: the answer to "where did
/// this type come from".
///
/// Deliberately flat — no chain. A cause names the constraint that *introduced* a
/// type, never one that relayed it, and that is arranged when the cause is built
/// rather than by walking a chain afterwards. See `Origin::cause_of`.
// No `Eq`: `NodeSpan`'s `PartialEq` is deliberately blind (see its documentation), so
// equality here is a claim about the reason and not about the position.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cause {
    /// Why that constraint required the two types to match.
    pub reason: Reason,
    /// The source text it is about.
    pub span: NodeSpan,
}

/// Which side of a constraint a type sits on.
///
/// Unification is symmetric and does not care; provenance does. Whether a solved
/// type was read off the left or the right of the constraint that solved it is what
/// decides whether that constraint is where the type came from, or merely where a
/// substitution put it — see [`Origin::cause_of`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Side {
    Left,
    Right,
}

/// Where a constraint came from, and — once inference has moved types around — where
/// each of its two types came from.
#[derive(Debug, Clone, PartialEq)]
pub struct Origin {
    /// Why the two types were required to match.
    pub reason: Reason,
    /// The source text this constraint is about. A failure here draws its caret at
    /// this span.
    pub span: NodeSpan,
    /// The constraint whose solution first rewrote the left type, if any.
    ///
    /// `unify` solves a constraint by substituting a type for a variable and then
    /// applying that substitution to every constraint left. Once that has happened,
    /// this side holds a type this constraint never mentioned: it came from wherever
    /// the substitution did. That is the fact a reader needs — "`Int`, because of the
    /// annotation two lines up" — and it is what the secondary label of a type error
    /// renders.
    ///
    /// The *first* rewrite is kept and later ones are dropped: the first substitution
    /// to reach a side is the one that brought a foreign type into it, and later ones
    /// only rewrite what is already there.
    left_from: Option<Cause>,
    /// The same, for the right type.
    right_from: Option<Cause>,
}

impl Origin {
    fn new(reason: Reason, span: NodeSpan) -> Origin {
        Origin {
            reason,
            span,
            left_from: None,
            right_from: None,
        }
    }

    /// This constraint, as the explanation for a type it introduced itself.
    fn own_cause(&self) -> Cause {
        Cause {
            reason: self.reason,
            span: self.span,
        }
    }

    /// Where the type on `side` came from: whatever rewrote that side, or — when
    /// nothing did — this constraint itself.
    ///
    /// This is the whole of the provenance rule, and it is a rule about *sides*.
    /// When `unify` solves `t := T` from a constraint, `T` was read off one side of
    /// it. If that side is a type the constraint was written with, the constraint is
    /// the answer. If a previous solution had rewritten that side, the constraint is
    /// only relaying a type, and the answer is whatever rewrote it — which is already
    /// flat, so the credit passes straight through and no chain is ever built.
    ///
    /// Crediting a constraint that merely relayed a type is how `result : Bool` /
    /// `result = not 42` came to blame the annotation: the `Bool` that `42` fails
    /// against is `not`'s parameter type, which the application constraint carries on
    /// its *left*, while the annotation had only rewritten its right.
    fn cause_of(&self, side: Side) -> Cause {
        let rewritten = match side {
            Side::Left => self.left_from,
            Side::Right => self.right_from,
        };

        rewritten.unwrap_or_else(|| self.own_cause())
    }

    /// Record that a solution rewrote one side of this constraint, keeping whatever
    /// was already recorded for that side. See [`Origin::left_from`].
    fn rewritten(&mut self, side: Side, cause: Cause) {
        let slot = match side {
            Side::Left => &mut self.left_from,
            Side::Right => &mut self.right_from,
        };

        if slot.is_none() {
            *slot = Some(cause);
        }
    }

    /// Where the type this constraint clashed on came from, when the constraint is
    /// not itself where it came from.
    ///
    /// This is what a diagnostic's secondary label and its rule note are both read
    /// off. The left side is preferred because that is the declared or expected side
    /// by convention (see `Constraint`), so "expected because of …" reads about the
    /// right one; the right side answers when only it was rewritten.
    pub fn explanation(&self) -> Option<Cause> {
        self.left_from.or(self.right_from)
    }
}

/// What went wrong, and — for everything unification can raise — where.
///
/// Each variant carries the [`Origin`] of the constraint that failed, so a type
/// error can put its caret under the sub-expression that disagrees instead of across
/// the declaration containing it. The declaration is still named by [`Error`], one
/// level up, which is the frame that knows it.
#[derive(Debug)]
pub enum ErrorKind {
    /// Two types were required to match and do not.
    ///
    /// The two are written in the order the message reads them out — declared side
    /// first where the source had one. Neither is reliably "the type of the text the
    /// origin points at": by the time a constraint fails, unification has substituted
    /// into both sides and may have decomposed the pair the source actually wrote.
    // The origins and the types are boxed because an `ErrorKind` is the `Err` half of
    // a `Result` threaded through the whole of `unify`, and every *successful* return
    // pays for the size of the largest variant. `Origin` carries a `Cause` per side,
    // and a `Type` carries a whole `QualName` for every union it names.
    UnificationFailed {
        left: Box<Type>,
        right: Box<Type>,
        origin: Box<Origin>,
    },
    /// A type variable the declaration is universally quantified over — one its annotation
    /// wrote, or one of an instance's head or of the member signature an instance's
    /// binding is held to — would have to be a type of its own, which a declaration cannot
    /// ask of a variable that stands for every type a caller may choose
    /// ([*An annotation is a promise*](../../docs/spec/types.md#an-annotation-is-a-promise)).
    ///
    /// Raised where `unify` meets a rigid variable and anything but that variable: a
    /// concrete type, or another rigid variable of the same declaration.
    RigidVariable {
        /// The variable as the source wrote it, `a`.
        variable: String,
        /// What the variable was written in.
        binder: Binder,
        /// What the body would need it to be, written with the source's names for the
        /// other rigid variables it holds. Boxed for the reason `UnificationFailed`'s
        /// types are.
        tpe: Box<Type>,
        origin: Box<Origin>,
    },
    /// A type variable would have to occur inside its own solution.
    CircularType {
        /// The type the variable would have had to contain itself in. Boxed for the
        /// reason above.
        tpe: Box<Type>,
        origin: Box<Origin>,
    },
    /// A name the typer's environment does not know. `type_check` turns this into a
    /// [`Solved::UnboundName`] rather than an [`Error`]; that variant says why.
    ///
    /// `name` is `environment_key`'s `package:Module.name` lookup key, copied
    /// verbatim from [`Reference::name`] — not a spelling. `message()`'s arm for this
    /// variant renders it as-is, which would print the package if this variant were
    /// ever surfaced as a rendered [`Error`]; today `type_check` never does that (see
    /// above), so the leak has no path to a user yet. Whoever gives this variant a
    /// live path — `ERR-8`'s planned warning is the likely first one — has to carry a
    /// displayable name (the bare local name, or a `QualName` rendered the way
    /// `Spellings` would) alongside this key rather than rendering it directly.
    UnboundVariable {
        name: String,
        /// Where the name was written.
        span: NodeSpan,
    },
    /// A field access, an update, an accessor or a record pattern whose record type
    /// nothing else in the declaration supplied: once unification had run over the whole
    /// declaration, the type it reads a field of was still a variable
    /// ([A use does not decide a record's type](../../docs/spec/records.md#a-use-does-not-decide-a-records-type)).
    ///
    /// `span` is the whole form — `person.name`, `{ r | x = 1 }`, `.name`, `{ name }` —
    /// since what is missing is a type for it, not a label in one.
    RecordTypeUnknown {
        label: Name,
        form: RecordUse,
        span: NodeSpan,
        /// What could supply the record type, which is what the note says.
        supplier: Supplier,
    },
    /// A field access, an update, an accessor or a record pattern naming a label the
    /// record type it is read against does not have. An update naming one is the update
    /// that would add a field ([Updating a record](../../docs/spec/records.md#updating-a-record)).
    ///
    /// `span` is the label for an access, an update's field and a record pattern's entry,
    /// and the whole accessor for an accessor, whose label is all of it but the `.`.
    MissingField {
        /// The record type, as solved. Boxed for the reason `UnificationFailed`'s
        /// types are.
        record: Box<Type>,
        label: Name,
        form: RecordUse,
        span: NodeSpan,
        /// Where the record type came from — the annotation, typically — when something
        /// brought it in; the secondary label goes under it.
        because: Option<Cause>,
    },
    /// A field access, an update, an accessor or a record pattern whose record is not a
    /// record at all: the type it reads a field of was solved to a type of another form.
    ///
    /// `span` is where [`MissingField`](Self::MissingField)'s would be, and `because` is
    /// as there.
    NotARecord {
        tpe: Box<Type>,
        label: Name,
        form: RecordUse,
        span: NodeSpan,
        because: Option<Cause>,
    },
    /// A class is required of a type, and the type has no instance of it: a declared type,
    /// a tuple or `()` that no instance in reach is declared for, or a function type,
    /// which no instance can be.
    ///
    /// The type is the one the obligation had once unification was done, so it can be
    /// the argument of an instance's context rather than the type the use was at:
    /// [`needed_by`](Self::NoInstance::needed_by) is then the obligation the use raised,
    /// which asked for this one through an instance of its own.
    NoInstance {
        class: QualName,
        /// Boxed for the reason `UnificationFailed`'s types are.
        tpe: Box<Type>,
        needed_by: Option<Box<Predicate>>,
        origin: Box<Origin>,
    },
    /// A class is required of a type variable the declaration's type holds, and the
    /// constraints its annotation (or an instance's context) wrote do not provide it: the
    /// constraint to add is the fix. The exception is a variable an instance binding's
    /// member signature binds, which no context of the instance can constrain:
    /// [`Written::MemberSignature`] says so.
    MissingConstraint {
        class: QualName,
        /// The variable as the source wrote it, `a`.
        variable: String,
        /// What is missing the constraint.
        written: Written,
        origin: Box<Origin>,
    },
    /// A class is required of a type variable the type of a declaration with no
    /// annotation holds. A constraint is never inferred
    /// ([`DEC-24` decision 5](../../docs/decisions/dec-24.md#5--a-constraint-is-never-inferred)),
    /// so the declaration has to state it.
    ConstraintNeedsAnnotation {
        class: QualName,
        /// What the declaration has to state, constraints and type: `Eq a => a -> a ->
        /// Bool`.
        stated: String,
        origin: Box<Origin>,
    },
    /// A class is required of a type variable that is not part of the declaration's type.
    /// Nothing determines the type the class is needed at, and no annotation on this
    /// declaration can.
    UndeterminedConstraint {
        class: QualName,
        origin: Box<Origin>,
    },
}

/// What a constraint would have to be written in, for [`ErrorKind::MissingConstraint`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Written {
    /// A declaration's annotation.
    Annotation,
    /// An instance's context.
    InstanceContext,
    /// The signature of the class member an instance's binding defines: the variable is
    /// one the member's signature binds and the instance's head does not, so no context
    /// of the instance can constrain it.
    MemberSignature,
    /// A derivation's binding, which is given no context: none of its bindings mentions
    /// the class variable, so they stand for every type that derives the class.
    Derivation,
}

impl ErrorKind {
    /// The provenance of the constraint that failed, when the failure came from
    /// unification at all.
    fn origin(&self) -> Option<&Origin> {
        match self {
            ErrorKind::UnificationFailed { origin, .. }
            | ErrorKind::RigidVariable { origin, .. }
            | ErrorKind::CircularType { origin, .. }
            | ErrorKind::NoInstance { origin, .. }
            | ErrorKind::MissingConstraint { origin, .. }
            | ErrorKind::ConstraintNeedsAnnotation { origin, .. }
            | ErrorKind::UndeterminedConstraint { origin, .. } => Some(origin.as_ref()),
            ErrorKind::UnboundVariable { .. }
            | ErrorKind::RecordTypeUnknown { .. }
            | ErrorKind::MissingField { .. }
            | ErrorKind::NotARecord { .. } => None,
        }
    }

    /// The headline, naming each union the way `spellings` says the checked package
    /// reaches it when the message has to qualify one at all — see [`AdtNames`].
    fn message(&self, spellings: &Spellings) -> String {
        match self {
            ErrorKind::UnificationFailed { left, right, .. } => {
                // Two modules may each declare a union of the same name, and the
                // source spells both of them bare. Written that way the sentence
                // uses one word for two declarations and says nothing; the module
                // that declared each is what tells them apart, so both sides take
                // it — both, because a sentence that qualifies one side and not the
                // other reads as if only one of them had a module.
                if AdtNames::collide([left.as_ref(), right.as_ref()]) {
                    format!(
                        "cannot match `{}` with `{}`",
                        Qualified(left, spellings),
                        Qualified(right, spellings)
                    )
                } else {
                    format!("cannot match `{}` with `{}`", left, right)
                }
            }
            ErrorKind::RigidVariable {
                variable,
                binder,
                tpe,
                ..
            } => {
                // An instance's head and the member signature its binding is held to each
                // write their own variables, and may write one alike: then the message says
                // whose each is, or `b` would have to be `b`.
                let other = free_variables_in_order(tpe).into_iter().find_map(|found| {
                    let found_binder = found.binder()?;
                    (found.spelling() == *variable && found_binder != *binder)
                        .then_some(found_binder)
                });

                match other {
                    Some(other) => format!(
                        "`{}` of {} stands for any type, but here it would have to be `{}`, where `{}` is {}",
                        variable,
                        binder.of_phrase(),
                        Spelled(tpe, spellings),
                        variable,
                        other.possessive()
                    ),
                    None => format!(
                        "`{}` stands for any type, but here it would have to be `{}`",
                        variable,
                        Spelled(tpe, spellings)
                    ),
                }
            }
            // One type, but it can hold the collision on its own: the variable's
            // solution is built out of whatever it was unified against, which may
            // be two same-named unions from two modules.
            ErrorKind::CircularType { tpe, .. } => {
                let qualified = Qualified(tpe, spellings);
                let tpe: &dyn std::fmt::Display = if AdtNames::collide([tpe.as_ref()]) {
                    &qualified
                } else {
                    &**tpe
                };

                format!(
                    "circular type: a type variable would have to contain itself in `{}`",
                    tpe
                )
            }
            // `name` is the internal environment key, not a spelling — see the
            // doc comment on `UnboundVariable` above. Unreachable today only because
            // nothing renders this variant as an `Error`.
            ErrorKind::UnboundVariable { name, .. } => {
                format!("cannot find a value named `{}`", name)
            }
            ErrorKind::RecordTypeUnknown { label, form, .. } => match form {
                RecordUse::Access => format!(
                    "cannot read the field `{}`: nothing in this declaration says which record type it is read from",
                    label.as_str()
                ),
                RecordUse::Accessor => format!(
                    "cannot type the accessor `.{}`: nothing in this declaration says which record type it reads",
                    label.as_str()
                ),
                RecordUse::Update => {
                    "cannot type this update: nothing in this declaration says which record type it updates"
                        .to_string()
                }
                RecordUse::Pattern => {
                    "cannot type this record pattern: nothing in this declaration says which record type it matches"
                        .to_string()
                }
            },
            ErrorKind::MissingField { record, label, .. } => format!(
                "the record type `{}` has no field `{}`",
                Spelled(record, spellings),
                label.as_str()
            ),
            ErrorKind::NotARecord { tpe, label, .. } => format!(
                "`{}` is not a record type, so it has no field `{}`",
                Spelled(tpe, spellings),
                label.as_str()
            ),
            ErrorKind::NoInstance { class, tpe, .. } => format!(
                "there is no instance of `{}` for `{}`",
                class.unqualified_name(),
                Spelled(tpe, spellings)
            ),
            ErrorKind::MissingConstraint {
                class,
                variable,
                written,
                ..
            } => format!(
                "`{} {}` is required here, and {} does not provide it",
                class.unqualified_name(),
                variable,
                match written {
                    Written::Annotation => "the annotation",
                    Written::InstanceContext => "the instance's context",
                    Written::MemberSignature => "the member's signature",
                    Written::Derivation => "the derivation, which is given no context,",
                }
            ),
            ErrorKind::ConstraintNeedsAnnotation { class, .. } => format!(
                "a type annotation is needed: `{}` is required of a type this declaration leaves open",
                class.unqualified_name()
            ),
            ErrorKind::UndeterminedConstraint { class, .. } => format!(
                "`{}` is required of a type nothing in this declaration determines",
                class.unqualified_name()
            ),
        }
    }

    /// Where the type `MissingField` and `NotARecord` read a field of came from, when
    /// something brought it in: the secondary label of each, as a mismatch's is.
    fn record_use_because(&self) -> Option<Cause> {
        match self {
            ErrorKind::MissingField { because, .. } | ErrorKind::NotARecord { because, .. } => {
                *because
            }
            _ => None,
        }
    }

    /// The caret of one of the three errors a `FieldConstraint` raises itself, which
    /// carry a span and no [`Origin`]: each is about a form rather than about two types.
    fn record_use_label(&self) -> Option<(NodeSpan, String)> {
        match self {
            ErrorKind::RecordTypeUnknown { span, .. } => {
                Some((*span, "its record type is not known here".to_string()))
            }
            ErrorKind::MissingField { label, span, .. } => Some((
                *span,
                format!("the record has no field `{}`", label.as_str()),
            )),
            ErrorKind::NotARecord {
                label,
                form: RecordUse::Pattern,
                span,
                ..
            } => Some((
                *span,
                format!(
                    "`{}` is matched against a type that is not a record",
                    label.as_str()
                ),
            )),
            ErrorKind::NotARecord { label, span, .. } => Some((
                *span,
                format!(
                    "`{}` is read from a type that is not a record",
                    label.as_str()
                ),
            )),
            _ => None,
        }
    }
}

/// A [`Type`] written the way a message about one type writes it: bare, unless two of
/// the unions it names share a spelling, and then each by its module (see [`AdtNames`]).
struct Spelled<'a>(&'a Type, &'a Spellings);

impl std::fmt::Display for Spelled<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if AdtNames::collide([self.0]) {
            self.0.write(f, AdtNames::Qualified(self.1), None)
        } else {
            self.0.write(f, AdtNames::Unqualified, None)
        }
    }
}

/// A type error: what went wrong, where, and in which declaration.
///
/// # Where the positions come from
///
/// The typer does not check the canonical AST directly — it translates it into its
/// own [`Term`] language — but that translation now carries the canonical node's
/// [`NodeSpan`] along, and each constraint keeps the span of the term that produced
/// it. So a unification failure knows the sub-expression it is about, and
/// [`labels`](PhaseError::labels) draws the caret there.
///
/// The declaration's own span is still kept as `span`, for the one case that has no
/// finer answer: an error whose origin came from a hand-built term, or from a
/// canonical node the parser never spanned. Then the label degrades to the whole
/// declaration rather than disappearing.
#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    /// Where the declaration the error was found in was written. Used only as the
    /// fallback described above.
    pub span: NodeSpan,
    /// That declaration's name, which is what the note names: in a module of a
    /// hundred declarations, a caret is not much use without it when the diagnostic
    /// is rendered without a file (see `compile_package`).
    pub declaration: Name,
    /// What `declaration` is: a value, a binding of an instance, or an instance.
    within: Within,
    /// How the checked package spells each module in reach, which is what a union is
    /// written by in [`message`](PhaseError::message) when it has to be qualified.
    spellings: Spellings,
}

/// What an [`Error`]'s `declaration` names, which is what its first note says.
#[derive(Debug, Clone, PartialEq)]
enum Within {
    /// A value of the module: `declaration` is its name.
    Value,
    /// A binding of an instance: `declaration` is the member it defines, and this is the
    /// instance, written as its head line is.
    Binding(String),
    /// An instance, checked as a whole: `declaration` is the instance, written as its head
    /// line is.
    Instance,
    /// A binding of a derivation in a class: `declaration` is the binding, and this is
    /// the member the derivation is for.
    Derivation(String),
}

/// Type errors are about types, and [`Type`]'s `Display` writes them the way the
/// source does — so the message can name both sides of a mismatch instead of
/// dumping the typer's internal representation.
impl PhaseError for Error {
    fn message(&self) -> String {
        self.kind.message(&self.spellings)
    }

    fn notes(&self) -> Vec<String> {
        let mut notes = vec![match &self.within {
            Within::Value => format!("in the declaration of `{}`", self.declaration),
            Within::Binding(instance) => format!(
                "in the binding of `{}` in the instance `{}`",
                self.declaration, instance
            ),
            Within::Instance => format!("in the instance `{}`", self.declaration),
            Within::Derivation(member) => format!(
                "in the binding `{}` of the derivation of `{}`",
                self.declaration, member
            ),
        }];

        // The rule that was broken, taken from the failing constraint and, if that
        // one has nothing to add, from what explains it. `if`'s "every branch must
        // have the same type" is on the branch constraint; "a body must have the
        // type its annotation declares" is on the annotation, which is normally the
        // explanation rather than the failure.
        if let Some(origin) = self.kind.origin() {
            let rule = origin
                .reason
                .note()
                .or_else(|| origin.explanation().and_then(|c| c.reason.note()));

            if let Some(rule) = rule {
                notes.push(rule.to_owned());
            }
        }

        match &self.kind {
            ErrorKind::UnificationFailed { left, right, .. } => {
                if let Some(difference) = label_difference(left, right) {
                    notes.push(difference);
                }
            }
            ErrorKind::RigidVariable {
                variable, binder, ..
            } => notes.push(match binder {
                Binder::InstanceHead => format!(
                    "a type variable of an instance stands for every type the instance may be used at, so its bindings cannot rely on `{}` being one particular type",
                    variable
                ),
                Binder::Annotation => format!(
                    "a type variable in an annotation stands for every type a caller may choose, so the body cannot rely on `{}` being one particular type",
                    variable
                ),
                Binder::MemberSignature => format!(
                    "a type variable of a member's signature stands for every type a caller of the member may choose, so a binding cannot rely on `{}` being one particular type",
                    variable
                ),
            }),
            ErrorKind::RecordTypeUnknown { supplier, .. } => {
                notes.push(
                    "a record's type is never worked out from the fields a declaration uses"
                        .to_string(),
                );
                notes.push(match supplier {
                    Supplier::Annotation => format!(
                        "a type annotation on `{}` would supply it",
                        self.declaration
                    ),
                    Supplier::AnnotationVariable => format!(
                        "the annotation on `{}` does not say which record type this is: it writes a type variable where the record type would be spelled out",
                        self.declaration
                    ),
                    Supplier::Body => format!(
                        "it is not part of `{}`'s type, so no annotation on `{}` could supply it",
                        self.declaration, self.declaration
                    ),
                });
            }
            ErrorKind::MissingField {
                form: RecordUse::Update,
                ..
            } => notes.push(
                "an update cannot add a field: each label it names must already be a field of the record it updates"
                    .to_string(),
            ),
            ErrorKind::MissingField {
                form: RecordUse::Pattern,
                ..
            } => notes.push(
                "a record pattern names some of the fields of the record it matches, and each label it names must be one of them"
                    .to_string(),
            ),
            ErrorKind::NoInstance {
                class,
                tpe,
                needed_by,
                ..
            } => {
                let class = class.unqualified_name();

                notes.push(match tpe.as_ref() {
                    Type::Fun { .. } => {
                        "a function type has no instances: no instance can be declared for one"
                            .to_string()
                    }
                    _ => format!(
                        "an instance of `{}` for this type is declared in the module that declares `{}` or in the module that declares the type, and none is in scope here",
                        class, class
                    ),
                });

                if let Some(required) = needed_by {
                    notes.push(format!(
                        "it is needed because `{}` is required here, and the instance of `{}` that answers that asks the same of this type",
                        predicate_text(required, &self.spellings),
                        required.class.unqualified_name()
                    ));
                }
            }
            ErrorKind::MissingConstraint {
                class,
                variable,
                written,
                ..
            } => notes.push(match written {
                Written::Annotation => format!(
                    "add `{} {}` to the constraints of the annotation on `{}`",
                    class.unqualified_name(),
                    variable,
                    self.declaration
                ),
                Written::InstanceContext => format!(
                    "add `{} {}` to the context of the instance",
                    class.unqualified_name(),
                    variable
                ),
                Written::MemberSignature => format!(
                    "`{}` is bound by the signature of the member and not by the head of the instance, so no context of the instance can constrain it: the binding has to hold for every `{}`, and cannot need `{} {}`; change the binding, or the member's signature",
                    variable,
                    variable,
                    class.unqualified_name(),
                    variable
                ),
                Written::Derivation => format!(
                    "a derivation's bindings carry no context, so the binding has to hold for every `{}` and cannot need `{} {}`; change the binding, or the member's signature",
                    variable,
                    class.unqualified_name(),
                    variable
                ),
            }),
            ErrorKind::ConstraintNeedsAnnotation { stated, .. } => {
                notes.push(
                    "a constraint is never inferred: it is part of a type only where an annotation writes it"
                        .to_string(),
                );
                notes.push(format!(
                    "`{}` is what the declaration has to state: `{} : {}`",
                    self.declaration, self.declaration, stated
                ));
            }
            ErrorKind::UndeterminedConstraint { .. } => notes.push(format!(
                "the type is not part of `{}`'s type, so no annotation on `{}` could say which it is",
                self.declaration, self.declaration
            )),
            _ => (),
        }

        notes
    }

    /// A caret under the text that disagrees, and — when inference can say where the
    /// type it disagrees with came from — a second, secondary one under that.
    ///
    /// Falls back to underlining the whole declaration when the failing constraint
    /// has no position, which is what a term built by hand, or one translated from a
    /// canonical node with no span, produces.
    fn labels(&self) -> Vec<SpanLabel> {
        let mut labels = Vec::new();

        match &self.kind {
            ErrorKind::UnboundVariable { span, .. } => {
                if let Some(span) = span.span() {
                    labels.push(SpanLabel {
                        span,
                        message: "not found in this scope".to_owned(),
                        primary: true,
                        file: None,
                    });
                }
            }
            kind @ (ErrorKind::RecordTypeUnknown { .. }
            | ErrorKind::MissingField { .. }
            | ErrorKind::NotARecord { .. }) => {
                if let Some((span, message)) = kind.record_use_label() {
                    if let Some(span) = span.span() {
                        labels.push(SpanLabel {
                            span,
                            message,
                            primary: true,
                            file: None,
                        });
                    }
                }

                // As a mismatch's below, and for the same two reasons drawn only beside
                // a primary label on other text.
                if let (Some(primary), Some(because)) = (labels.first(), kind.record_use_because())
                {
                    match because.span.span() {
                        Some(span) if span != primary.span => labels.push(SpanLabel {
                            span,
                            message: because.reason.explains().to_owned(),
                            primary: false,
                            file: None,
                        }),
                        _ => (),
                    }
                }
            }
            kind => {
                if let Some(origin) = kind.origin() {
                    if let Some(span) = origin.span.span() {
                        labels.push(SpanLabel {
                            span,
                            message: origin.reason.describes().to_owned(),
                            primary: true,
                            file: None,
                        });
                    }

                    // Only worth drawing once the primary one exists: on its own it
                    // would be a caret under the annotation with nothing to contrast
                    // it against. And not worth drawing at all when it lands on the
                    // same text — a literal that is its own explanation renders as two
                    // carets under one word saying the same thing twice.
                    if let (Some(primary), Some(because)) = (labels.first(), origin.explanation()) {
                        match because.span.span() {
                            Some(span) if span != primary.span => labels.push(SpanLabel {
                                span,
                                message: because.reason.explains().to_owned(),
                                primary: false,
                                file: None,
                            }),
                            _ => (),
                        }
                    }
                }
            }
        }

        if labels.is_empty() {
            if let Some(span) = self.span.span() {
                labels.push(SpanLabel {
                    span,
                    message: format!("in `{}`", self.declaration),
                    primary: true,
                    file: None,
                });
            }
        }

        labels
    }
}

/// The note for two record types that failed to unify because their label sets
/// differ, naming the labels each has that the other lacks; `None` for any other pair,
/// two record types of one label set included — they failed on a field's type, which
/// the headline names.
///
/// "First" and "second" are the headline's order, which is the constraint's.
fn label_difference(left: &Type, right: &Type) -> Option<String> {
    let (Type::Record(left), Type::Record(right)) = (left, right) else {
        return None;
    };

    let only = |of: &BTreeMap<Name, Type>, other: &BTreeMap<Name, Type>| -> Vec<String> {
        of.keys()
            .filter(|label| !other.contains_key(*label))
            .map(|label| format!("`{}`", label.as_str()))
            .collect()
    };
    let only_left = only(left, right);
    let only_right = only(right, left);

    let clause = |which: &str, labels: &[String], other: &str| {
        let fields = if labels.len() == 1 {
            "a field"
        } else {
            "fields"
        };
        format!(
            "the {} record type has {} {} that the {} does not",
            which,
            fields,
            labels.join(", "),
            other
        )
    };

    match (only_left.is_empty(), only_right.is_empty()) {
        (true, true) => None,
        (false, true) => Some(clause("first", &only_left, "second")),
        (true, false) => Some(clause("second", &only_right, "first")),
        (false, false) => Some(format!(
            "{}, and the second has {} {} that the first does not",
            clause("first", &only_left, "second"),
            if only_right.len() == 1 {
                "a field"
            } else {
                "fields"
            },
            only_right.join(", ")
        )),
    }
}

/// What [`type_check_recovering`] found in one module: an answer for every declaration,
/// and every error inference reported, side by side.
#[derive(Debug)]
pub struct TypeCheck {
    /// One [`Solved`] per declaration, keyed the way `module.values` is. A declaration
    /// inference reported an error for is here as [`Solved::Rejected`], and its error is
    /// in [`errors`](Self::errors).
    pub solved: HashMap<Name, Solved>,
    /// One entry per instance the module declares, in the order
    /// [`canonical::Module::instances`] holds them: what each is for and an answer for
    /// every binding of its body.
    pub instances: Vec<SolvedInstance>,
    /// The classes of this module with a derivation binding that was rejected, by name.
    /// The error is in [`errors`](Self::errors), at the class.
    pub rejected_derivations: Vec<Name>,
    /// Every error inference reported, one per rejected declaration, binding or instance.
    /// Empty means the module type checked.
    pub errors: Vec<Error>,
}

/// [`type_check_recovering`], reduced to a module that either type checked or did not:
/// the solved map when no declaration failed, every error otherwise.
pub fn type_check(
    module: &Module,
    interfaces: &HashMap<Name, Interface>,
) -> Result<HashMap<Name, Solved>, Vec<Error>> {
    let TypeCheck { solved, errors, .. } = type_check_recovering(module, interfaces);

    if errors.is_empty() {
        Ok(solved)
    } else {
        Err(errors)
    }
}

/// Type check one canonical module and hand back what it solved, reporting every value
/// whose inference produced a reportable error rather than stopping at the first — the
/// shape `compile_package` is built to accumulate.
///
/// # What comes back
///
/// One [`Solved`] per declaration, keyed the way `module.values` is, whether or not
/// any declaration failed. A declaration the typer could not type is present and says
/// so; none is ever merely absent, because absent is indistinguishable from
/// checked-and-fine to whatever reads this next
/// ([`DEC-18` decision 1](../../docs/decisions/dec-18.md)). One that inference
/// reported an error for is [`Solved::Rejected`], with the error in
/// [`TypeCheck::errors`], so a module with a type error still has a typed term for
/// each of its other declarations
/// ([`DEC-23` decision 5](../../docs/decisions/dec-23.md)).
///
/// The term inside [`Solved::Typed`] is the one `annotate` built, with `unify`'s final
/// substitution applied to every node rather than to the declaration's own type alone.
/// That interior is the point — a backend needs the type of each sub-expression, not
/// just of the declaration containing it.
///
/// Every instance the module declares is answered for the same way, one
/// [`SolvedInstance`] each, with a [`Solved`] for each binding of its body: a binding is a
/// declaration whose annotation is the member's signature at the instance's type.
///
/// # Where an [`Error`] is built
///
/// Here, and only here: this is the last frame that holds the `canonical::Value`, so
/// it is the only one that knows the declaration's name and its span. What went
/// *wrong* and *where inside the declaration* comes up from inference on the
/// [`ErrorKind`]; see [`Error`].
///
/// # What a declaration is checked against
///
/// `interfaces` is the map `module` was canonicalized against. Every value and union
/// it exposes is put in the typer's environment beside this module's own, so a
/// declaration that forwards an imported value, builds an imported constructor or
/// matches on one is checked like any other. Each use of one of those names gets a
/// fresh instance of its declared type, so one declaration can use `Just` or
/// `Maybe.withDefault` at two types — see `Types`.
///
/// A name's entry is a context and a type (`Scheme`). A class member's is its
/// signature with its class's constraint in front, from the module's own classes and from
/// every imported interface's; a function's is its annotation's context and type. Each
/// use of one raises the obligations its context asks, which are answered once the
/// declaration's equations are solved — see the `classes` module.
pub fn type_check_recovering(module: &Module, interfaces: &HashMap<Name, Interface>) -> TypeCheck {
    // A `module foreign` facade uses synthetic placeholder bodies, so there is nothing
    // to infer — but every declaration still has to be accounted for, so each is
    // returned saying why it has no term.
    if module.binding_foreign {
        return TypeCheck {
            solved: module
                .values
                .keys()
                .map(|name| (name.clone(), Solved::NoBody))
                .collect(),
            instances: Vec::new(),
            rejected_derivations: Vec::new(),
            errors: Vec::new(),
        };
    }

    // Start at a high offset to avoid collisions with the counter inside
    // Types::new() (which starts at 10) used during inference.
    let mut counter = 10_000u32;

    // First pass: build global env from every declared type in reach — the values
    // each imported interface exposes, then this module's own annotated values, broken
    // ones included.
    let mut global: HashMap<String, Scheme> = HashMap::new();

    // An imported value is keyed the way a `VarForeign` reference spells it: its name
    // qualified by the package and module that declared it ([`environment_key`]). That
    // is the only key, so it cannot collide with a local name or with a same-named value
    // of another module, in this package or another. An operator's backing function the
    // header did not expose by name is in `infix_functions` rather than `values`, and an
    // operator resolves to a `VarForeign` naming it all the same.
    for interface in interfaces.values() {
        for (name, signature) in interface.values.iter().chain(&interface.infix_functions) {
            if let Some(scheme) = scheme_of(&signature.tpe, &signature.context, &mut counter) {
                let qname = interface.module_name.qualify_name(name);
                global.insert(environment_key(&qname), scheme);
            }
        }

        // A member is in no interface's `values`: it travels with its class, and is
        // declared here the way a value is, under the name a `VarForeign` spells.
        for (class_name, signature) in &interface.classes {
            insert_members(
                &mut global,
                &interface.module_name,
                class_name,
                signature,
                &mut counter,
            );
        }
    }

    // This module's own classes' members, under the name a `VarTopLevel` spells.
    for (class_name, class) in &module.classes {
        insert_members(
            &mut global,
            &module.name,
            class_name,
            &class.signature,
            &mut counter,
        );
    }

    // A declaration canonicalization recorded as broken is declared by its annotation,
    // when that canonicalized, exactly as an annotated one is: a caller is checked
    // against the type the declaration was written with, whatever is wrong with its body.
    let annotated = module
        .values
        .iter()
        .filter_map(|(name, value)| match value {
            canonical::Value::TypedValue { tpe, context, .. } => Some((name, tpe, context)),
            canonical::Value::Value { .. } => None,
        });
    let broken = module
        .broken
        .iter()
        .filter_map(|broken| Some((&broken.name, broken.tpe.as_ref()?, &broken.context)));

    for (name, tpe, context) in annotated.chain(broken) {
        if let Some(scheme) = scheme_of(tpe, context, &mut counter) {
            // Add both qualified (e.g. "test-project:Test.not") and unqualified (e.g.
            // "not") names
            let qname = environment_key(&module.name.qualify_name(name));
            global.insert(qname, scheme.clone());
            global.insert(name.as_str().to_string(), scheme);
        }
    }

    // What the canonical AST is read against: every union in reach, their
    // constructors and the arity of each of this module's declarations. See
    // [`Translation`].
    let translation = Translation::of(module, interfaces);

    // Second pass: add constructor types to global from every union in reach, this
    // module's own and every imported interface's alike.
    for (type_name, union_type) in &translation.unions {
        // Fresh type vars for each ADT type parameter (e.g. "a" in Maybe a)
        let mut adt_var_map: HashMap<String, TypeVariable> = HashMap::new();
        for tv_name in &union_type.variables {
            counter += 1;
            adt_var_map.insert(
                tv_name.as_str().to_string(),
                TypeVariable::flexible(counter),
            );
        }

        // Build result type: Adt(type_name, [TypeVar for each param])
        let result_args: Vec<Type> = union_type
            .variables
            .iter()
            .map(|v| Type::Variable(adt_var_map[v.as_str()].clone()))
            .collect();
        let result_type = Type::Adt(type_name.clone(), result_args);

        for ctor in &union_type.variants {
            let ctor_type = if ctor.type_parameters.is_empty() {
                result_type.clone()
            } else {
                let mut translate_var_map = adt_var_map.clone();
                let params: Vec<Type> = ctor
                    .type_parameters
                    .iter()
                    .filter_map(|t| {
                        canonical_type_to_typer_type(t, &mut translate_var_map, &mut counter)
                    })
                    .collect();
                if params.len() != ctor.type_parameters.len() {
                    continue; // untranslatable param type, skip this constructor
                }
                // Build Fun type: p1 -> p2 -> ... -> result_type
                params
                    .into_iter()
                    .rev()
                    .fold(result_type.clone(), |acc, p| Type::Fun {
                        param_tpe: Box::new(p),
                        return_tpe: Box::new(acc),
                    })
            };

            // Registered under the qualified name a `VarConstructor` spells —
            // "zelkova-core:Maybe.Just", named by the package and module that declared
            // the union — and, for this module's own unions only, under the bare name
            // too. A union of another package's module of the same name is not this
            // module's own: its `QualName` differs in the package.
            let qname = type_name.sibling(&ctor.name);
            if module.name.qualify_name(&type_name.unqualified_name()) == *type_name {
                global.insert(
                    ctor.name.as_str().to_string(),
                    Scheme::unconstrained(ctor_type.clone()),
                );
            }
            global.insert(environment_key(&qname), Scheme::unconstrained(ctor_type));
        }
    }

    // The constructor of `Basics.Position` builds the place of a constructor from an
    // `Int`. A module that declares `Position` has declared it above, and one that does
    // not still has a derived instance that builds one.
    let (position_name, _) = position_constructor();
    global
        .entry(environment_key(&position_name))
        .or_insert_with(|| {
            Scheme::unconstrained(Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(position_type()),
            })
        });

    // Third pass: check each value. A value that fails is recorded and the pass
    // moves on, so one broken declaration cannot hide the others.
    //
    // In the order of their names, because each declaration's rigid variables are numbered
    // from `counter` and stay in the types that come back: an order that depended on the
    // hash map would number the same declaration differently from one run to the next.
    let spellings = Spellings::of(module, interfaces);
    let table = classes::ClassTable::of(module, interfaces);
    let mut errors: Vec<Error> = vec![];
    let mut solved: HashMap<Name, Solved> = HashMap::new();
    let mut values: Vec<(&Name, &canonical::Value)> = module.values.iter().collect();
    values.sort_by(|left, right| left.0.cmp(right.0));
    for (name, value) in values {
        let Some((term, annotation)) =
            value_to_term_and_annotation(value, &translation, &mut counter)
        else {
            // Unsupported construct: nothing was checked, and the entry says so
            // rather than the declaration going missing — see [`Solved`].
            solved.insert(name.clone(), Solved::Untranslatable { span: value.span() });
            continue;
        };

        // The annotation is handed to inference rather than checked against its
        // result afterwards, which is what lets a mismatch *inside* the body know
        // that the type it failed against came from the annotation. Checking the two
        // whole types at the end could only ever say "this declaration is `Int` and
        // its body is something else", with the caret across the lot.
        let answer = match solve(term, annotation, &global, &table) {
            Ok(answer) => answer,
            // The declaration is answered for as well as reported, so the rest of the
            // module keeps the terms it solved.
            Err(kind) => {
                errors.push(Error {
                    kind,
                    span: value.span(),
                    declaration: name.clone(),
                    within: Within::Value,
                    spellings: spellings.clone(),
                });
                Solved::Rejected
            }
        };
        solved.insert(name.clone(), answer);
    }

    // Fourth pass: the derivations of this module's own classes. A derived instance carries
    // these bindings placed in its members, but a class that nothing derives has them
    // checked all the same, and an error in one is at the class, where it was written. It
    // runs before the instances so that an instance derived from a class whose derivation
    // is wrong does not report the same mistake again in the members it was given.
    let rejected_derivations = DerivationCheck {
        global: &global,
        table: &table,
        translation: &translation,
        spellings: &spellings,
    }
    .of(module, &mut counter, &mut errors);
    let rejected: HashSet<QualName> = rejected_derivations
        .iter()
        .map(|class| module.name.qualify_name(class))
        .collect();

    // Fifth pass: every instance, which is checked as a declaration is — its bindings
    // are values whose annotation is the member's signature at the instance's type — and
    // as a whole, for the superclasses its class asks of it.
    let instances = module
        .instances
        .iter()
        .map(|instance| {
            let check = InstanceCheck {
                global: &global,
                table: &table,
                translation: &translation,
                spellings: &spellings,
                rejected: &rejected,
            };
            check.of(instance, &mut counter, &mut errors)
        })
        .collect();

    TypeCheck {
        solved,
        instances,
        rejected_derivations,
        errors,
    }
}

/// Infer one declaration, or one binding of an instance, to the [`Solved`] it ends up as:
/// its typed term, or the entry saying a name was out of the typer's reach. An error to
/// report is the `Err`.
fn solve(
    term: Term,
    annotation: Option<Annotation>,
    global: &HashMap<String, Scheme>,
    table: &classes::ClassTable,
) -> Result<Solved, ErrorKind> {
    match infer_annotated(term, global.clone(), annotation, table) {
        // An unbound variable here is a hole in the typer's environment, not a
        // mistake in the source — see [`Solved::UnboundName`].
        Err(ErrorKind::UnboundVariable { name, span }) => Ok(Solved::UnboundName { name, span }),
        Err(kind) => Err(kind),
        Ok((term, context)) => Ok(Solved::Typed {
            term: Box::new(term),
            context,
        }),
    }
}

/// What an instance's bindings are checked under: the type the instance is for, where its
/// head line was written, and what its context gives of the head's variables.
struct InstanceScope<'a> {
    head: &'a Type,
    span: NodeSpan,
    given: &'a [(QualName, TypeVariable)],
    names: &'a [(TypeVariable, Name)],
}

/// What checking an instance reads besides the instance itself.
struct InstanceCheck<'a> {
    global: &'a HashMap<String, Scheme>,
    table: &'a classes::ClassTable<'a>,
    translation: &'a Translation<'a>,
    spellings: &'a Spellings,
    /// The classes of this module whose derivations this pass rejected.
    rejected: &'a HashSet<QualName>,
}

impl InstanceCheck<'_> {
    /// Check `instance`: each of its bindings against its member's signature at the
    /// instance's type, and the instance as a whole against the superclasses its class
    /// has. Errors are pushed onto `errors`.
    ///
    /// The instance's context is *given* in both, as an annotation's is inside a
    /// declaration: the variables the head binds are variables of the unification, a
    /// constraint of the context is on one of them, and an obligation on that same
    /// variable is answered by it.
    ///
    /// A derived instance is checked like any other. Canonicalization gave it the members
    /// its class's derivation stands for and the context its type's arguments need, so its
    /// superclasses are discharged against that context, and an error in a generated
    /// binding is on the span of the word `derived`. Where the class's derivations were
    /// rejected, in this module or the one that declares it, the class has the error and a
    /// derived instance's members, which are generated out of the derivation that failed,
    /// report none of their own.
    fn of(
        &self,
        instance: &canonical::Instance,
        counter: &mut u32,
        errors: &mut Vec<Error>,
    ) -> SolvedInstance {
        let signature = &instance.signature;

        let mut variables: HashMap<String, TypeVariable> = HashMap::new();
        let head = instance_head_type(&signature.head, &mut variables, counter);
        let head = make_rigid(head, &mut variables, Binder::InstanceHead);
        let given: Vec<(QualName, TypeVariable)> = signature
            .context
            .iter()
            .filter_map(|constraint| {
                Some((
                    constraint.class.clone(),
                    variables.get(constraint.variable.as_str())?.clone(),
                ))
            })
            .collect();
        let context = given
            .iter()
            .map(|(class, variable)| Predicate {
                class: class.clone(),
                tpe: Type::Variable(variable.clone()),
            })
            .collect();
        let mut names: Vec<(TypeVariable, Name)> = variables
            .iter()
            .map(|(name, variable)| (variable.clone(), Name::new(name.clone())))
            .collect();
        names.sort_by(|left, right| left.1.cmp(&right.1));

        let text = instance_text(signature, &head, &names);
        let class = self.table.class(&signature.class);

        let mut rejected = false;
        let mut bindings = Vec::new();

        // The members of an instance derived from a class whose derivations were rejected
        // restate the class's error, so an error in one is left to it.
        let restated = instance.derived
            && (self.rejected.contains(&signature.class)
                || class.is_some_and(|class| class.derivations_rejected));

        if let Some(class) = class {
            let obligations = class
                .superclasses
                .iter()
                .map(|superclass| classes::Obligation {
                    class: superclass.class.clone(),
                    tpe: head.clone(),
                    origin: Origin::new(Reason::SuperclassInstance, signature.span),
                })
                .collect();

            let declared = classes::Declared {
                tpe: &head,
                annotated: true,
                given: &given,
                names: &names,
                member_variables: &[],
                written: Written::InstanceContext,
            };

            if let Err(kind) =
                classes::discharge(self.table, obligations, &Substitution::empty(), &declared)
            {
                rejected = true;
                errors.push(Error {
                    kind,
                    span: signature.span,
                    declaration: Name::new(text.clone()),
                    within: Within::Instance,
                    spellings: self.spellings.clone(),
                });
            }
        }

        for value in &instance.bindings {
            let canonical::Value::Value { name, .. } = value else {
                continue;
            };

            let scope = InstanceScope {
                head: &head,
                span: signature.span,
                given: &given,
                names: &names,
            };

            let answer = match self.binding(value, class, &scope, counter) {
                Some(Ok(answer)) => answer,
                Some(Err(kind)) => {
                    if !restated {
                        errors.push(Error {
                            kind,
                            span: value.span(),
                            declaration: name.clone(),
                            within: Within::Binding(text.clone()),
                            spellings: self.spellings.clone(),
                        });
                    }
                    Solved::Rejected
                }
                None => Solved::Untranslatable { span: value.span() },
            };
            bindings.push((name.clone(), answer));
        }

        SolvedInstance {
            head,
            context,
            bindings,
            rejected,
        }
    }

    /// One binding of an instance's body, inferred against the signature of the member it
    /// defines, with the class variable replaced by the instance's head. `None` when it
    /// cannot be read: the binding does not translate, or its class has no member of that
    /// name in reach — which canonicalization has reported.
    fn binding(
        &self,
        value: &canonical::Value,
        class: Option<&canonical::ClassSignature>,
        scope: &InstanceScope,
        counter: &mut u32,
    ) -> Option<Result<Solved, ErrorKind>> {
        let canonical::Value::Value { name, .. } = value else {
            return None;
        };
        let class = class?;
        let member = class.members.iter().find(|member| &member.name == name)?;

        let (term, _) = value_to_term_and_annotation(value, self.translation, counter)?;

        // The member's signature has the class variable free in it. It is translated like
        // any annotation, and the variable it came out as is replaced by the head: every
        // other variable of the signature stays one of its own.
        let mut variables: HashMap<String, TypeVariable> = HashMap::new();
        let signature = canonical_type_to_typer_type(&member.tpe, &mut variables, counter)?;
        let signature = make_rigid(signature, &mut variables, Binder::MemberSignature);
        let class_variable = variables.get(class.variable.as_str())?;
        let tpe = Substitution::substitute(signature, class_variable, scope.head);

        // What the signature binds besides the class variable is the member's own, and is
        // written the way the class wrote it.
        let mut names = scope.names.to_vec();
        let mut member_variables = Vec::new();
        for (written, variable) in &variables {
            if variable != class_variable {
                names.push((variable.clone(), Name::new(written.clone())));
                member_variables.push(variable.clone());
            }
        }
        names.sort_by(|left, right| left.1.cmp(&right.1));

        // The head line stands where an annotation would: the span a mismatch with the
        // member's signature draws its second label under.
        let annotation = Annotation {
            tpe,
            span: scope.span,
            reason: Reason::InstanceMember,
            context: scope.given.to_vec(),
            names,
            member_variables,
            written: Written::InstanceContext,
        };

        Some(solve(term, Some(annotation), self.global, self.table))
    }
}

/// `Basics.Position`'s one constructor, and where it sits: the compiler knows it by
/// name, as it knows the scalars ([`scalars::POSITION`]).
fn position_constructor() -> (QualName, Constructor) {
    let union = scalars::POSITION.qual_name();
    let name = Name::new(scalars::POSITION.name);

    (
        union.sibling(&name),
        Constructor {
            union,
            name,
            index: 0,
            arity: 1,
        },
    )
}

/// The type `Basics.Position`: what `differed` and `atConstructor` are handed.
fn position_type() -> Type {
    Type::Adt(scalars::POSITION.qual_name(), Vec::new())
}

/// What checking the derivations of a module's classes reads.
struct DerivationCheck<'a> {
    global: &'a HashMap<String, Scheme>,
    table: &'a classes::ClassTable<'a>,
    translation: &'a Translation<'a>,
    spellings: &'a Spellings,
}

impl DerivationCheck<'_> {
    /// Check the bindings of every derivation of `module`'s classes, each as a declaration
    /// annotated with the type the chapter gives its role over the type the member
    /// answers with — `matched : R`, `differed : Position -> Position -> R`,
    /// `atConstructor : Position -> R` and `combine : R -> R -> R`. Errors are pushed onto
    /// `errors`, classes in the order of their names. The classes with a binding that did
    /// not check come back by name, in that order.
    ///
    /// No context is given to a binding: none of them mentions the class variable, so each
    /// stands for every type that derives the class, and what one needs of a type is an
    /// instance in scope or an error. A variable the answer type holds is the member
    /// signature's own, and is rigid. `Comparable`'s `differed i j = compare i j` needs
    /// `Comparable Position`.
    fn of(&self, module: &Module, counter: &mut u32, errors: &mut Vec<Error>) -> Vec<Name> {
        let mut classes: Vec<_> = module.classes.iter().collect();
        classes.sort_by(|left, right| left.0.cmp(right.0));

        let mut rejected = Vec::new();
        for (name, class) in classes {
            let before = errors.len();
            for derivation in &class.signature.derivations {
                for (role, value) in derivation.bindings.roles() {
                    self.binding(derivation, role, value, counter, errors);
                }
            }
            if errors.len() > before {
                rejected.push(name.clone());
            }
        }
        rejected
    }

    fn binding(
        &self,
        derivation: &canonical::Derivation,
        role: canonical::DerivationRole,
        value: &canonical::Value,
        counter: &mut u32,
        errors: &mut Vec<Error>,
    ) {
        let canonical::Value::Value { name, .. } = value else {
            return;
        };
        let Some((term, _)) = value_to_term_and_annotation(value, self.translation, counter) else {
            return;
        };

        let mut variables: HashMap<String, TypeVariable> = HashMap::new();
        let Some(answer) =
            canonical_type_to_typer_type(&derivation.result, &mut variables, counter)
        else {
            return;
        };
        // A variable of the answer type is one the member's signature binds beyond the
        // class's, which every caller of the member chooses for itself: a binding is held
        // to every type it may stand for.
        let answer = make_rigid(answer, &mut variables, Binder::MemberSignature);
        let function = |parameters: Vec<Type>, result: Type| {
            parameters
                .into_iter()
                .rev()
                .fold(result, |result, parameter| Type::Fun {
                    param_tpe: Box::new(parameter),
                    return_tpe: Box::new(result),
                })
        };
        let tpe = match role {
            canonical::DerivationRole::Matched => answer.clone(),
            canonical::DerivationRole::Differed => {
                function(vec![position_type(), position_type()], answer.clone())
            }
            canonical::DerivationRole::AtConstructor => {
                function(vec![position_type()], answer.clone())
            }
            canonical::DerivationRole::Combine => {
                function(vec![answer.clone(), answer.clone()], answer.clone())
            }
        };

        let mut names: Vec<(TypeVariable, Name)> = variables
            .iter()
            .map(|(name, variable)| (variable.clone(), Name::new(name.clone())))
            .collect();
        names.sort_by(|left, right| left.1.cmp(&right.1));

        let annotation = Annotation {
            tpe,
            span: derivation.span,
            reason: Reason::DerivationBinding,
            context: Vec::new(),
            names,
            member_variables: Vec::new(),
            written: Written::Derivation,
        };

        if let Err(kind) = solve(term, Some(annotation), self.global, self.table) {
            errors.push(Error {
                kind,
                span: value.span(),
                declaration: name.clone(),
                within: Within::Derivation(derivation.member.to_string()),
                spellings: self.spellings.clone(),
            });
        }
    }
}

/// `instance Eq a => Eq (Box a)`, minus the keyword: how a message names an instance.
fn instance_text(
    signature: &canonical::InstanceSignature,
    head: &Type,
    names: &[(TypeVariable, Name)],
) -> String {
    let names: VariableNames = names
        .iter()
        .map(|(variable, name)| (variable.clone(), name.to_string()))
        .collect();
    let text = WithNames(head, &names).to_string();
    let head = match head {
        Type::Adt(_, args) if !args.is_empty() => format!("({})", text),
        _ => text,
    };
    let constraints: Vec<String> = signature
        .context
        .iter()
        .map(|constraint| {
            format!(
                "{} {}",
                constraint.class.unqualified_name(),
                constraint.variable
            )
        })
        .collect();
    let context = match constraints.as_slice() {
        [] => String::new(),
        [one] => format!("{} => ", one),
        many => format!("({}) => ", many.join(", ")),
    };

    format!("{}{} {}", context, signature.class.unqualified_name(), head)
}

/// The scheme a declared type and the context written in front of it make: the type as
/// the typer reads it, and each constraint on the variable it was translated to.
fn scheme_of(
    tpe: &canonical::Type,
    context: &[canonical::Constraint],
    counter: &mut u32,
) -> Option<Scheme> {
    let mut variables = HashMap::new();
    let tpe = canonical_type_to_typer_type(tpe, &mut variables, counter)?;
    let context = context
        .iter()
        .filter_map(|constraint| {
            Some(Predicate {
                class: constraint.class.clone(),
                tpe: Type::Variable(variables.get(constraint.variable.as_str())?.clone()),
            })
        })
        .collect();

    Some(Scheme { context, tpe })
}

/// Declare the members of a class `module_name` declares, each under the name a
/// reference to it spells ([`environment_key`]): its signature, with the class's own
/// constraint in front.
fn insert_members(
    global: &mut HashMap<String, Scheme>,
    module_name: &ModuleName,
    class_name: &Name,
    class: &canonical::ClassSignature,
    counter: &mut u32,
) {
    let declared = module_name.qualify_name(class_name);

    for member in &class.members {
        let mut variables = HashMap::new();
        let Some(tpe) = canonical_type_to_typer_type(&member.tpe, &mut variables, counter) else {
            continue;
        };

        // Every member's signature mentions the class variable, so it is always found;
        // canonicalization rejects a class with a member that does not.
        let Some(variable) = variables.get(class.variable.as_str()) else {
            continue;
        };
        let context = vec![Predicate {
            class: declared.clone(),
            tpe: Type::Variable(variable.clone()),
        }];

        global.insert(
            environment_key(&module_name.qualify_name(&member.name)),
            Scheme { context, tpe },
        );
    }
}

// ── Translation helpers ───────────────────────────────────────────────────────

/// The key the typer's environment holds a declaration's type under: the package that
/// declares it, a `:`, then its qualified name — `zelkova-core:Maybe.withDefault`.
///
/// The package is in the key for the reason it is in a [`QualName`]: two packages may
/// each hold a module `Size`, and `Size.foo` alone would give both declarations one
/// entry, owned by whichever was inserted last. Neither a package name nor a module name
/// can contain a `:`, so no two declarations share a key, and no key is a bare name a
/// local is looked up by.
fn environment_key(qname: &QualName) -> String {
    format!("{}:{}", qname.package(), qname.to_name())
}

/// Union declarations, keyed by the qualified name of each declaration.
///
/// Keyed that way rather than by the spelling, so that a lookup answers "is this the
/// declaration that module named?" and not "is something spelled like that in reach?".
/// The two differ for every imported constructor whose type shares a name with a local
/// one, which is what `BUG-35` closed.
type Unions<'a> = HashMap<QualName, &'a canonical::UnionType>;

/// Everything the translation from the canonical AST reads besides the expression in
/// front of it.
///
/// Each of them answers a question the canonical node cannot: which union a constructor
/// belongs to and where in it, and how many arguments a call has to supply before it is a
/// direct call. All of them are facts of the module and of what it imports, which is why
/// they are gathered once here — the declaration being translated is the only thing that
/// changes between calls.
struct Translation<'a> {
    /// Every union in reach — this module's own, and every one an imported
    /// [`Interface`] exposes — keyed by the qualified name that identifies each
    /// declaration rather than by the spelling the `type` line wrote. A constructor
    /// carries the qualified name of the type it builds, so this key is what finds the
    /// declaration a constructor in a pattern belongs to — see [`translate_pattern`].
    ///
    /// An interface's union is only as complete as its module exposed it: an opaque
    /// one crosses with no constructors, which is also why no canonical constructor
    /// can name one.
    unions: Unions<'a>,
    /// The constructors of every union in [`unions`](Self::unions), keyed the way a
    /// `VarConstructor` spells one, each with the place in its declaration both
    /// backends need.
    constructors: HashMap<QualName, Constructor>,
    /// How many parameters each of this module's declarations was written with.
    ///
    /// This is the callee's arity at a call site naming one of them, which is what
    /// decides an application's [`Saturation`]. The rule itself — parameter count, not
    /// arrow count — is [`canonical::Value::arity`], which `ir::build` reads too.
    arities: HashMap<Name, usize>,
    /// How many parameters each value an imported [`Interface`] exposes is emitted with
    /// ([`Interface::arities`]), keyed the way a `VarForeign` spells it: its name
    /// qualified by the package and module that declared it.
    ///
    /// It is the same fact as [`arities`](Self::arities), recorded by the module that
    /// declared the value, so a call to it is saturated at the count its emitted function
    /// takes. It is also what [`ReferenceKind::Foreign`] carries, since a backend using an
    /// imported function as a value needs its arity too.
    foreign_arities: HashMap<QualName, usize>,
}

impl<'a> Translation<'a> {
    /// The translation for `module`, checked against `interfaces`.
    ///
    /// The interfaces' unions go in first and this module's own after them, so that if
    /// the map somehow held an interface of the module under check, the declarations
    /// in front of the typer are the ones that win.
    fn of(module: &'a Module, interfaces: &'a HashMap<Name, Interface>) -> Translation<'a> {
        let mut unions: Unions<'a> = HashMap::new();
        let mut foreign_arities = HashMap::new();

        for interface in interfaces.values() {
            for (name, union_type) in &interface.unions {
                unions.insert(interface.module_name.qualify_name(name), union_type);
            }

            // Every value the typer's environment registers for this interface, so that
            // every `VarForeign` it can type has an arity here. One the interface did not
            // record is read as a parameterless binding's: see `Interface::arities`.
            for name in interface
                .values
                .keys()
                .chain(interface.infix_functions.keys())
            {
                let arity = interface.arities.get(name).copied().unwrap_or(0);
                foreign_arities.insert(interface.module_name.qualify_name(name), arity);
            }
        }

        for (name, union_type) in &module.types {
            unions.insert(module.name.qualify_name(name), union_type);
        }

        let mut constructors = constructors_of(&unions);
        // `Basics.Position`'s constructor is known whether or not an interface in reach
        // carries it, since a derived instance builds one and no module exposes it.
        let (position_name, position) = position_constructor();
        constructors.entry(position_name).or_insert(position);

        let arities = module
            .values
            .iter()
            .map(|(name, value)| (name.clone(), value.arity()))
            .collect();

        Translation {
            unions,
            constructors,
            arities,
            foreign_arities,
        }
    }

    /// A translation that knows these unions and nothing else: no arity is known, so
    /// every application it produces is [`Saturation::Partial`].
    #[cfg(test)]
    fn of_types(unions: Unions<'a>) -> Translation<'a> {
        let constructors = constructors_of(&unions);

        Translation {
            unions,
            constructors,
            arities: HashMap::new(),
            foreign_arities: HashMap::new(),
        }
    }

    /// How many arguments the callee of an application spine takes, when this module
    /// knows.
    ///
    /// A declaration of this module, one of another module and a constructor each have
    /// one. `None` is what makes an application [`Saturation::Partial`], and it is the
    /// honest answer twice over: a local is a value rather than a declaration and has no
    /// arity at all, and a callee that is itself an expression — the result of a `case`,
    /// say — is a value too. A backend that cannot prove a call saturated goes through
    /// `$curry`, which is correct for both.
    fn callee_arity(&self, callee: &canonical::Expression) -> Option<usize> {
        match &callee.kind {
            canonical::ExpressionKind::VarTopLevel(qname) => {
                self.arities.get(&qname.unqualified_name()).copied()
            }
            canonical::ExpressionKind::VarForeign(qname, _, _) => Some(self.foreign_arity(qname)),
            canonical::ExpressionKind::VarConstructor(qname, _) => {
                self.constructors.get(qname).map(|ctor| ctor.arity)
            }
            _ => None,
        }
    }

    /// The arity of `qname`, a value another module declares, as its interface recorded
    /// it — 0 for one no interface in reach records, the arity of a parameterless binding.
    ///
    /// Every name canonicalization resolves to a `VarForeign` is one an interface in
    /// reach exposes, so the fallback is for a value its module recorded as broken, or for
    /// a hand-built interface map; see [`Interface::arities`].
    fn foreign_arity(&self, qname: &QualName) -> usize {
        self.foreign_arities.get(qname).copied().unwrap_or(0)
    }
}

/// Every constructor of `unions`, keyed the way a `VarConstructor` spells one: the
/// constructor's name qualified by the package and module that declared the union it
/// builds.
fn constructors_of(unions: &Unions) -> HashMap<QualName, Constructor> {
    let mut constructors = HashMap::new();

    for (union, union_type) in unions {
        for variant in crate::ir::variants_of(union_type) {
            // A union is named by its declaring package and module, so that is where
            // its constructors are named from too.
            let name = union.sibling(&variant.name);

            constructors.insert(
                name,
                Constructor {
                    union: union.clone(),
                    name: variant.name,
                    index: variant.index,
                    arity: variant.arity,
                },
            );
        }
    }

    constructors
}

/// Convert a canonical type to the typer's simplified Type representation.
///
/// Every variant of [`canonical::Type`] converts, so this answers `None` for no type
/// today. The `Option` is what a type form the typer cannot read would answer, and the
/// callers keep their handling of it: a declaration whose annotation is `None` here is
/// left unchecked ([`value_to_term_and_annotation`]), and a value or a constructor whose
/// declared type is `None` is not in the environment inference reads, so a name reaching
/// one is [`Solved::UnboundName`].
///
/// `var_map` maps named type variables (e.g. "a") to consistent TypeVariable
/// ids, so that `a -> a` produces the same variable on both sides.
///
/// # How a name decides which type it becomes
///
/// A [`canonical::Type::Type`] names its declaration in full — `Widget.Size`, not
/// `Size` (`AST-4`) — and the whole of that name is what picks the arm. A nullary
/// type whose qualified name is [a scalar the compiler
/// knows](super::scalars) becomes the matching [`Type::Literal`]; everything else
/// becomes a [`Type::Adt`], carrying that name whole.
///
/// For a name that resolved, that is the declaring module's. For one that resolved to
/// nothing it is the module under check, applied to the whole *written* spelling, dots
/// and all, because `Type::from_parser_type` hands the undivided name to
/// `ModuleName::qualify_name` rather than splitting a module half off it — so an
/// unresolved `Missing.Thing` written in `Test` arrives here as `Test.Missing.Thing`
/// and does not collapse onto the local `Test.Thing`.
pub(crate) fn canonical_type_to_typer_type(
    tpe: &canonical::Type,
    var_map: &mut HashMap<String, TypeVariable>,
    counter: &mut u32,
) -> Option<Type> {
    match tpe {
        canonical::Type::Variable(name) => {
            let tv = var_map.entry(name.as_str().to_string()).or_insert_with(|| {
                *counter += 1;
                TypeVariable::flexible(*counter)
            });
            Some(Type::Variable(tv.clone()))
        }
        canonical::Type::Arrow(a, b) => {
            let a = canonical_type_to_typer_type(a, var_map, counter)?;
            let b = canonical_type_to_typer_type(b, var_map, counter)?;
            Some(Type::Fun {
                param_tpe: Box::new(a),
                return_tpe: Box::new(b),
            })
        }
        // `Tuple::try_map` keeps the arity attached to the value instead of
        // re-deriving it here, the same way the parser → canonical conversions
        // in `canonical/mod.rs` do.
        canonical::Type::Tuple(tuple) => {
            let elements = tuple
                .try_map(|elem| canonical_type_to_typer_type(elem, var_map, counter).ok_or(()))
                .ok()?;
            Some(Type::Tuple(elements))
        }
        canonical::Type::Unit => Some(Type::Unit),
        // Label for label: the canonical map is already the set the typer's is.
        canonical::Type::Record(fields) => {
            let fields = fields
                .iter()
                .map(|(label, field)| {
                    let field = canonical_type_to_typer_type(field, var_map, counter)?;
                    Some((label.clone(), field))
                })
                .collect::<Option<BTreeMap<_, _>>>()?;
            Some(Type::Record(fields))
        }
        canonical::Type::Type(name, args) => {
            if args.is_empty() {
                if let Some(literal) = scalar_literal(name) {
                    return Some(Type::Literal(literal));
                }
            }

            let converted: Option<Vec<Type>> = args
                .iter()
                .map(|a| canonical_type_to_typer_type(a, var_map, counter))
                .collect();
            Some(Type::Adt(name.clone(), converted?))
        }
    }
}

/// `tpe`, with every variable `variables` names made rigid, and `variables` updated to
/// hold the rigid ones.
///
/// `tpe` and `variables` are what [`canonical_type_to_typer_type`] built for a type the
/// declaration being checked is quantified over: an annotation, an instance's head, the
/// member signature an instance's binding is held to. Each variable keeps its `id` and
/// takes the name it was written by and the `binder` it was written in, so a given built from
/// `variables` afterwards is on the variable the type holds, and `unify` will solve none of
/// them. A variable already rigid is left as it is.
fn make_rigid(tpe: Type, variables: &mut HashMap<String, TypeVariable>, binder: Binder) -> Type {
    let mut tpe = tpe;
    for (written, variable) in variables.iter_mut() {
        if variable.is_rigid() {
            continue;
        }
        let rigid = variable
            .clone()
            .into_rigid(Name::new(written.clone()), binder);
        tpe = Substitution::substitute(tpe, variable, &Type::Variable(rigid.clone()));
        *variable = rigid;
    }
    tpe
}

/// The literal type the typer gives a [scalar](super::scalars), if `name` is the
/// qualified name of one it has a literal type for.
///
/// Four of the five appear here. [`scalars::BOOL`] does not: it is a scalar *and* an
/// ordinary union, so `Bool` in an annotation takes the [`Type::Adt`] path every other
/// declaration takes and meets `True` and `False` there. [`bool_type`] is the same type,
/// built for the three `Bool`s no source spells.
fn scalar_literal(name: &QualName) -> Option<TypeLiteral> {
    const LITERALS: &[(scalars::Scalar, TypeLiteral)] = &[
        (scalars::INT, TypeLiteral::Int),
        (scalars::FLOAT, TypeLiteral::Float),
        (scalars::CHAR, TypeLiteral::Char),
        (scalars::STRING, TypeLiteral::String),
    ];

    LITERALS
        .iter()
        .find(|(scalar, _)| scalar.declares(name))
        .map(|(_, literal)| literal.clone())
}

/// The type of a `Bool`: the union [`scalars::BOOL`] names, with no arguments.
///
/// `Bool` is [a scalar and an ordinary union at
/// once](../../docs/spec/types.md#scalar-types) — the compiler knows its
/// representation and nothing about its structure — so this is the very type
/// `canonical_type_to_typer_type` produces for an annotation naming `Basics.Bool`, and
/// the type `Basics` registers `True` and `False` at.
///
/// An [`if` condition](../../docs/spec/expressions.md#if--then--else) needs a `Bool`
/// the source did not spell, and `translate_pattern` gives a `Basics.True` or
/// `Basics.False` pattern this type when it turns it into a test on its value. It
/// names `Basics.Bool` and nothing else, so a module declaring its own `type Bool`
/// does not satisfy an `if` ([`DEC-15`](../../docs/decisions/dec-15.md) decisions 1
/// and 5).
pub(super) fn bool_type() -> Type {
    Type::Adt(scalars::BOOL.qual_name(), vec![])
}

/// Convert a canonical expression to a Term, keeping the position it was written at.
///
/// Returns None for constructs the inference engine doesn't yet handle (a `VarKernel`
/// reference), and for a constructor of a union neither this module nor an interface in
/// [`Translation`] declares.
///
/// Every arm attaches `expr.span` to the term it builds. That is the whole of what
/// `ERR-4` needed from this function: a constraint can only point at a
/// sub-expression if the term that produced it remembers where it came from.
///
/// # What is recorded here and nowhere else
///
/// This is the only moment at which a reference's *kind* is known. A local, a
/// top-level, an imported value and a constructor are four different things to emit,
/// and the spelling each leaves behind is bare for one of them and qualified for the
/// rest — so a backend handed only the string could not tell them apart
/// ([`DEC-18` decision
/// 1](../../docs/decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)).
/// Each becomes a [`Reference`] carrying both the lookup key inference uses and what
/// the name is. An application spine records the same way whether it is
/// [saturated](Saturation), which is a question about the spine and not about any one
/// `Apply` node.
fn canonical_expr_to_term(
    expr: &canonical::Expression,
    translation: &Translation,
    counter: &mut u32,
) -> Option<Term> {
    let kind = match &expr.kind {
        // Carried at the canonical AST's own width: [`Int` is 64
        // bits](../../docs/spec/evaluation-semantics.md#numbers), and a term is what
        // code is generated from, so narrowing here would emit a different number than
        // the one that was written.
        canonical::ExpressionKind::Int(i) => TermKind::Int(*i),
        canonical::ExpressionKind::Char(c) => TermKind::Char(*c),
        canonical::ExpressionKind::String(s) => TermKind::String(s.clone()),
        canonical::ExpressionKind::Float(f) => TermKind::Float(*f),
        canonical::ExpressionKind::VarLocal(name) => {
            TermKind::Identifier(Reference::local(name.as_str()))
        }
        canonical::ExpressionKind::VarTopLevel(qname) => TermKind::Identifier(Reference {
            name: environment_key(qname),
            kind: ReferenceKind::TopLevel(qname.clone()),
        }),
        // A value another module declares. Its type is the one its module's interface
        // declared, which `type_check` registers under this same qualified name.
        canonical::ExpressionKind::VarForeign(qname, package, _) => {
            TermKind::Identifier(Reference {
                name: environment_key(qname),
                kind: ReferenceKind::Foreign(
                    qname.clone(),
                    package.clone(),
                    translation.foreign_arity(qname),
                ),
            })
        }
        // A constructor builds a tagged value rather than reading a binding, so it
        // carries its place in its declaration. That place comes from the union,
        // which is in `translation.constructors` whether this module declared it or
        // an imported interface did. The type the canonical node carries is not read:
        // the one `type_check` registers is built from the union's own variables.
        canonical::ExpressionKind::VarConstructor(qname, _) => {
            let ctor = translation.constructors.get(qname)?;

            TermKind::Identifier(Reference {
                name: environment_key(qname),
                kind: ReferenceKind::Constructor(ctor.clone()),
            })
        }
        // The spine is walked as a whole rather than one node at a time, because
        // whether a call is saturated is a fact about the callee and the number of
        // arguments reaching it. Each node keeps its own canonical span, so a type
        // error still points where it did.
        canonical::ExpressionKind::Apply(_, _) => {
            let (callee, applications) = spine(expr);
            let arity = translation.callee_arity(callee);

            let mut term = canonical_expr_to_term(callee, translation, counter)?;

            for (supplied, application) in applications.iter().enumerate() {
                // `spine` collects `Apply` nodes and nothing else.
                let canonical::ExpressionKind::Apply(_, argument) = &application.kind else {
                    return None;
                };

                let argument = canonical_expr_to_term(argument, translation, counter)?;
                // The node that consumes the callee's last argument is the direct
                // call; everything before it is a partial application and everything
                // after it applies whatever that call returned.
                let saturation = match arity {
                    Some(arity) if arity == supplied + 1 => Saturation::Saturated,
                    _ => Saturation::Partial,
                };

                term = Term {
                    span: application.span,
                    kind: TermKind::Apply {
                        fun: Box::new(term),
                        arg: Box::new(argument),
                        saturation,
                    },
                };
            }

            return Some(term);
        }
        canonical::ExpressionKind::If(cond, t, f) => {
            let cond = canonical_expr_to_term(cond, translation, counter)?;
            let t = canonical_expr_to_term(t, translation, counter)?;
            let f = canonical_expr_to_term(f, translation, counter)?;
            TermKind::If {
                cond: Box::new(cond),
                true_branch: Box::new(t),
                false_branch: Box::new(f),
            }
        }
        canonical::ExpressionKind::Tuple(tuple) => {
            let elements = tuple
                .try_map(|elem| canonical_expr_to_term(elem, translation, counter).ok_or(()))
                .ok()?;
            TermKind::Tuple(elements)
        }
        canonical::ExpressionKind::Unit => TermKind::Unit,
        canonical::ExpressionKind::Case(scrutinee_expr, branches) => {
            let scrutinee = canonical_expr_to_term(scrutinee_expr, translation, counter)?;
            let term_branches: Vec<(TermPattern, Box<Term>)> = branches
                .iter()
                .map(|cb| {
                    let pattern = translate_pattern(&cb.pattern, translation, counter)?;
                    let body = canonical_expr_to_term(&cb.expression, translation, counter)?;
                    Some((pattern, Box::new(body)))
                })
                .collect::<Option<Vec<_>>>()?;
            TermKind::Case {
                scrutinee: Box::new(scrutinee),
                branches: term_branches,
                form: CaseForm::Expression,
            }
        }
        // A name that did not resolve. Its error is canonicalization's, and the term
        // stands where the name was written so the rest of the body is still checked.
        canonical::ExpressionKind::Hole => TermKind::Hole,
        // The fields stay in the order they were written: only the record's type is a
        // set, and that is built from them by `constraint::collect`.
        canonical::ExpressionKind::Record(fields) => {
            TermKind::Record(translate_fields(fields, translation, counter)?)
        }
        canonical::ExpressionKind::Update(record, fields) => TermKind::Update {
            record: Box::new(canonical_expr_to_term(record, translation, counter)?),
            fields: translate_fields(fields, translation, counter)?,
        },
        canonical::ExpressionKind::Access(record, label, label_span) => TermKind::Access {
            record: Box::new(canonical_expr_to_term(record, translation, counter)?),
            label: label.clone(),
            label_span: *label_span,
        },
        canonical::ExpressionKind::Accessor(label, label_span) => TermKind::Accessor {
            label: label.clone(),
            label_span: *label_span,
        },
        // Not yet supported: VarKernel
        _ => return None,
    };

    Some(Term {
        span: expr.span,
        kind,
    })
}

/// The fields of a record or an update, in the order they were written, each with the
/// span of its label.
fn translate_fields(
    fields: &[canonical::Field],
    translation: &Translation,
    counter: &mut u32,
) -> Option<Vec<Field<Term>>> {
    fields
        .iter()
        .map(|field| {
            Some(Field {
                label: field.label.clone(),
                label_span: field.label_span,
                value: canonical_expr_to_term(&field.value, translation, counter)?,
            })
        })
        .collect()
}

/// An application spine: what is being applied, and the `Apply` nodes that apply it,
/// innermost first.
///
/// `f a b` is `Apply(Apply(f, a), b)` in the canonical AST, and one `Apply` node on its
/// own cannot say whether the call it is part of supplies everything `f` takes. This is
/// what turns the nesting back into a callee and a count. `applications[i]` supplies the
/// `i + 1`th argument, and the last of them is `expr` itself.
fn spine(expr: &canonical::Expression) -> (&canonical::Expression, Vec<&canonical::Expression>) {
    let mut applications = Vec::new();
    let mut callee = expr;

    while let canonical::ExpressionKind::Apply(fun, _) = &callee.kind {
        applications.push(callee);
        callee = fun;
    }

    applications.reverse();

    (callee, applications)
}

/// Translate a canonical pattern into a `TermPattern`. Returns `None` for a pattern the
/// term language does not model: a constructor that names no case of its declaration, or
/// a sub-pattern that is itself one of those.
///
/// The pattern keeps its own span, separate from the branch body's: a `case` branch
/// whose pattern does not match what is being matched on is about the pattern, and
/// the caret belongs there rather than under the expression in the `case … of` line.
fn translate_pattern(
    pattern: &canonical::Pattern,
    translation: &Translation,
    counter: &mut u32,
) -> Option<TermPattern> {
    let kind = match &pattern.kind {
        canonical::PatternKind::Anything => TermPatternKind::Anything,
        canonical::PatternKind::Variable(name) => {
            // The binding's actual type will be unified with the scrutinee type in annotate.
            TermPatternKind::Bind(name.as_str().to_string())
        }
        canonical::PatternKind::Int(value) => TermPatternKind::Literal {
            tpe: Type::Literal(TypeLiteral::Int),
            value: LiteralValue::Int(*value),
        },
        canonical::PatternKind::Char(value) => TermPatternKind::Literal {
            tpe: Type::Literal(TypeLiteral::Char),
            value: LiteralValue::Char(*value),
        },
        canonical::PatternKind::Float(value) => TermPatternKind::Literal {
            tpe: Type::Literal(TypeLiteral::Float),
            value: LiteralValue::Float(*value),
        },
        canonical::PatternKind::String(value) => TermPatternKind::Literal {
            tpe: Type::Literal(TypeLiteral::String),
            value: LiteralValue::String(value.clone()),
        },
        canonical::PatternKind::Unit => TermPatternKind::Unit,
        // `Basics`' own `True` and `False` are tested by value, as an `Int` or a
        // `Char` literal is, so a backend tests a `Bool` scrutinee by equality. The
        // type is the same one the constructor would have constrained the scrutinee
        // to. A module's own `type Bool = True | False` is not `Basics.Bool` and is
        // left alone.
        canonical::PatternKind::Constructor { ctor, args }
            if args.is_empty() && ctor.tpe == scalars::BOOL.qual_name() =>
        {
            let value = match ctor.name.as_str() {
                "True" => true,
                "False" => false,
                _ => return None,
            };
            TermPatternKind::Literal {
                tpe: bool_type(),
                value: LiteralValue::Bool(value),
            }
        }
        canonical::PatternKind::Constructor { ctor, args } => {
            // Look up the parent union to get its type variables. `ctor.tpe` names
            // the declaration the constructor builds, module included, and `unions`
            // is keyed the same way — so this finds the one declaration the
            // constructor belongs to, this module's or an imported one.
            let union_type = translation.unions.get(&ctor.tpe)?;

            // Which case of that union this pattern matches. Unification needs only
            // the union; a decision tree and a WIT `variant` both need the case, and
            // the declaration is the only thing that can say where it sits.
            let index = union_type
                .variants
                .iter()
                .position(|variant| variant.name == ctor.name)?;

            // Create fresh type vars for each ADT type parameter.
            let mut adt_var_map: HashMap<String, TypeVariable> = HashMap::new();
            for tv_name in &union_type.variables {
                *counter += 1;
                adt_var_map.insert(
                    tv_name.as_str().to_string(),
                    TypeVariable::flexible(*counter),
                );
            }

            // Build the ADT result type args from the fresh vars.
            let adt_args: Vec<Type> = union_type
                .variables
                .iter()
                .map(|v| Type::Variable(adt_var_map[v.as_str()].clone()))
                .collect();

            // Translate each constructor type parameter (reuses the same fresh vars).
            let param_types: Vec<Type> = ctor
                .type_parameters
                .iter()
                .filter_map(|t| canonical_type_to_typer_type(t, &mut adt_var_map, counter))
                .collect();
            if param_types.len() != ctor.type_parameters.len() {
                return None;
            }

            // One sub-pattern per argument, so an argument's position is its place in
            // the list.
            let args = args
                .iter()
                .zip(param_types)
                .map(|(arg_pattern, param_type)| {
                    translate_sub_pattern(arg_pattern, param_type, translation, counter)
                })
                .collect::<Option<Vec<_>>>()?;

            TermPatternKind::Constructor {
                ctor: Constructor {
                    union: ctor.tpe.clone(),
                    name: ctor.name.clone(),
                    index,
                    arity: ctor.type_parameters.len(),
                },
                adt_args,
                args,
            }
        }
        // Each element gets a fresh type, and the matched value has to be the tuple of
        // them. An element is translated as a constructor's argument is (see
        // `translate_sub_pattern`).
        canonical::PatternKind::Tuple(elements) => {
            let elements = elements
                .try_map(|element| {
                    *counter += 1;
                    let tpe = Type::Variable(TypeVariable::flexible(*counter));
                    translate_sub_pattern(element, tpe, translation, counter).ok_or(())
                })
                .ok()?;

            TermPatternKind::Tuple { elements }
        }
        // A constructor that did not resolve: nothing says what type it builds or takes,
        // so each argument is at a fresh type, and translated as a resolved constructor's
        // argument is (see `translate_sub_pattern`).
        canonical::PatternKind::Hole(args) => {
            let args = args
                .iter()
                .map(|arg| {
                    *counter += 1;
                    let tpe = Type::Variable(TypeVariable::flexible(*counter));
                    translate_sub_pattern(arg, tpe, translation, counter)
                })
                .collect::<Option<Vec<_>>>()?;

            TermPatternKind::Hole { args }
        }
        // Each entry's field gets a fresh type, and its pattern is translated as a
        // constructor's argument is (see `translate_sub_pattern`). Nothing is built here
        // for the record itself: the pattern names a subset of its fields, so its record
        // type is whatever the matched value turns out to be, and each entry is read
        // against it once the declaration's equations are solved (`FieldConstraint`).
        canonical::PatternKind::Record(entries) => {
            let fields = entries
                .iter()
                .map(|entry| {
                    *counter += 1;
                    let tpe = Type::Variable(TypeVariable::flexible(*counter));
                    Some(Field {
                        label: entry.label.clone(),
                        label_span: entry.label_span,
                        value: translate_sub_pattern(&entry.pattern, tpe, translation, counter)?,
                    })
                })
                .collect::<Option<Vec<_>>>()?;

            TermPatternKind::Record { fields }
        }
    };

    Some(TermPattern {
        span: pattern.span,
        kind,
    })
}

/// A pattern written as a constructor's argument, a tuple's element or a record
/// pattern's entry, matched against a value of type `tpe`.
///
/// It is translated by [`translate_pattern`] like a pattern anywhere else, so any
/// pattern that translates at the top of a branch translates here too, at any depth, and
/// one that does not — a constructor of a union nobody declared, say — answers `None` here as it does there. A
/// refutable sub-pattern needs nothing of its own: `pattern_constraints` holds it to
/// `tpe` the way a branch's pattern is held to the scrutinee's type, and
/// `ir::decision_tree` tests it at its occurrence.
fn translate_sub_pattern(
    pattern: &canonical::Pattern,
    tpe: Type,
    translation: &Translation,
    counter: &mut u32,
) -> Option<SubPattern> {
    Some(SubPattern {
        tpe,
        pattern: translate_pattern(pattern, translation, counter)?,
    })
}

/// A declaration's type annotation, and where it was written.
///
/// The span is the annotation's alone — `answer : Int`, not the declaration it
/// heads — because it is drawn as the secondary label of a mismatch in the body, and
/// a span covering the body too would underline the thing it is meant to contrast
/// with.
///
/// An instance's binding has no annotation of its own: the member's signature at the
/// instance's type stands in for one, and the head line of the instance for its span.
///
/// The variables of `tpe` are rigid ([`make_rigid`]) for a declaration's annotation, for an
/// instance's binding, head and member signature alike, and for a derivation's binding, whose
/// type is the chapter's role type over the derivation's answer type: the variables there
/// are the member signature's own.
struct Annotation {
    tpe: Type,
    span: NodeSpan,
    /// The reason of the equation the annotation's type is one side of.
    reason: Reason,
    /// What the annotation requires of its variables — the `=>` in front of it, or the
    /// context of the instance — each a class and the variable of `tpe` it is on. These
    /// are *given* inside the declaration.
    context: Vec<(QualName, TypeVariable)>,
    /// The name the source wrote for each variable of `tpe`, sorted by name: what a
    /// message about a constraint writes the variable by.
    names: Vec<(TypeVariable, Name)>,
    /// The variables of `tpe` that `context` cannot be on: the ones a class member's
    /// signature binds besides the class's own, when `tpe` is an instance binding's. Empty
    /// for an annotation.
    member_variables: Vec<TypeVariable>,
    /// What a constraint missing from `context` would have to be written in.
    written: Written,
}

/// Convert a canonical Value into a (Term, optional annotation) pair.
/// The body is wrapped in nested Fun nodes for each parameter — see
/// [`wrap_with_patterns`].
/// Returns None if any part of the value cannot be translated, its annotation included:
/// a body checked without the annotation it was written with would be checked against
/// less than the source says, and pass where the annotation should have failed it. No
/// annotation fails to translate today — [`canonical_type_to_typer_type`] reads every
/// canonical type — so it is the body alone that can make this `None`; the annotation's
/// `?` is kept for the type form that would not.
///
/// The annotation's variables come back rigid ([`make_rigid`]), and its context is on them.
fn value_to_term_and_annotation(
    value: &canonical::Value,
    translation: &Translation,
    counter: &mut u32,
) -> Option<(Term, Option<Annotation>)> {
    match value {
        canonical::Value::Value { patterns, body, .. } => {
            let body_term = canonical_expr_to_term(body, translation, counter)?;
            let term = wrap_with_patterns(patterns.iter(), body_term, translation, counter)?;
            Some((term, None))
        }
        canonical::Value::TypedValue {
            patterns,
            body,
            tpe,
            context,
            annotation_span,
            ..
        } => {
            let body_term = canonical_expr_to_term(body, translation, counter)?;
            let pattern_iter = patterns.iter().map(|(p, _)| p);
            let term = wrap_with_patterns(pattern_iter, body_term, translation, counter)?;
            let mut var_map = HashMap::new();
            let tpe = canonical_type_to_typer_type(tpe, &mut var_map, counter)?;
            let tpe = make_rigid(tpe, &mut var_map, Binder::Annotation);
            let given = context
                .iter()
                .filter_map(|constraint| {
                    Some((
                        constraint.class.clone(),
                        var_map.get(constraint.variable.as_str())?.clone(),
                    ))
                })
                .collect();
            let mut names: Vec<(TypeVariable, Name)> = var_map
                .iter()
                .map(|(name, variable)| (variable.clone(), Name::new(name.clone())))
                .collect();
            names.sort_by(|left, right| left.1.cmp(&right.1));

            let annotation = Annotation {
                tpe,
                span: *annotation_span,
                reason: Reason::Annotation,
                context: given,
                names,
                member_variables: Vec::new(),
                written: Written::Annotation,
            };
            Some((term, Some(annotation)))
        }
    }
}

/// Wrap a body Term in nested Fun nodes for each parameter, outermost first.
///
/// A `Fun` binds one name, so a parameter written as a variable or `_` is bound as it
/// is. A parameter written as any other pattern is bound under the name
/// [`pattern_parameter`] gives its position, and matched by a single-branch
/// [`CaseForm::Parameter`] `Case` on that name: `first (x, _) = x` is translated as
/// `first $0 = case $0 of (x, _) -> x` would be. The pattern goes through
/// [`translate_pattern`], the same translation a `case` branch's gets, so a parameter
/// admits exactly the patterns a branch does. Returns `None` when it does not.
///
/// Every `Fun` is outside every `Case`. That keeps the first `arity` nodes of the term
/// the declaration's parameters and nothing else, which is what `ir::build` reads them
/// off by. The `Case`s nest in parameter order, the first outermost, so a name two
/// patterned parameters both bind is the later one's in the body — which is how
/// canonicalization resolved it. A pattern's names also shadow a same-named parameter
/// written as a variable, whichever comes first — and when the pattern comes first,
/// that is the opposite of canonicalization, which lets the later parameter win:
/// `f (x, _) x = x` reads the pattern's `x` here and the plain parameter's there. Both
/// are a name bound twice in one clause, which the language rejects and the compiler
/// does not yet (`LANG-18`).
///
/// Each `Fun` spans its parameter through the body it wraps, so a function whose
/// declared shape does not match its definition is underlined from the parameter
/// that starts it rather than across the annotation as well. A `Case` built here spans
/// its pattern alone: it is what the source wrote in place of a plain parameter.
fn wrap_with_patterns<'a>(
    patterns: impl Iterator<Item = &'a canonical::Pattern>,
    body: Term,
    translation: &Translation,
    counter: &mut u32,
) -> Option<Term> {
    let body_span = body.span;
    let mut params: Vec<(String, NodeSpan)> = vec![];
    let mut matched: Vec<(String, TermPattern)> = vec![];

    for (position, pattern) in patterns.enumerate() {
        match &pattern.kind {
            canonical::PatternKind::Variable(name) => {
                params.push((name.as_str().to_string(), pattern.span))
            }
            canonical::PatternKind::Anything => params.push(("_".to_string(), pattern.span)),
            _ => {
                let name = pattern_parameter(position);
                let translated = translate_pattern(pattern, translation, counter)?;
                params.push((name.clone(), pattern.span));
                matched.push((name, translated));
            }
        }
    }

    let body = matched
        .into_iter()
        .rev()
        .fold(body, |acc, (name, pattern)| {
            let span = pattern.span;
            Term {
                span,
                kind: TermKind::Case {
                    scrutinee: Box::new(Term {
                        span,
                        kind: TermKind::Identifier(Reference::local(name)),
                    }),
                    branches: vec![(pattern, Box::new(acc))],
                    form: CaseForm::Parameter,
                },
            }
        });

    let term = params
        .into_iter()
        .rev()
        .fold(body, |acc, (param, span)| Term {
            span: span.merge(body_span),
            kind: TermKind::Fun {
                param,
                body: Box::new(acc),
            },
        });

    Some(term)
}

// The term language inference runs on is [`crate::ir`], and it is no longer
// only inference's. Every node carries the [`NodeSpan`] of the canonical node it was
// built from, so an error found down here can say where in the user's source it
// happened (`ERR-4`), and each carries what a backend reads off it — the kind of name a
// reference is, a call's saturation, a constructor's place in its declaration — because
// the translation below is the only place those are known. Inference reads none of
// them.

mod annotate;
mod classes;
mod constraint;
mod unifier;

pub(crate) use classes::{head_of, instance_head_type};

/// A type variable: a placeholder `unify` may solve to a type, or a **rigid** one, which it
/// may not.
///
/// A *flexible* variable is the inference variable of an expression, and the variable of a
/// declared type a use instantiates. A *rigid* variable is one the declaration being
/// checked is universally quantified over: the variables its annotation wrote, or the
/// variables of an instance's head and of the member signature its bindings are held to
/// (`make_rigid` makes them). It stands for every type a caller may choose, so it is
/// equal to itself and to nothing else, and `unifier::unify` raises
/// [`ErrorKind::RigidVariable`] where a body would need it to be anything more specific.
///
/// A rigid variable is a flexible one's `id` with the name the source wrote it by, which is
/// what a message writes it as. The `id` is what tells two variables apart: two rigid
/// variables may be written alike, as an instance's head and the member signature its
/// binding is held to each write their own.
///
/// A rigid variable belongs to the declaration whose body is checked: the types of the term
/// that comes back hold it, and nothing the environment holds for another declaration does.
/// What the environment holds for a use, a `Scheme`, is over flexible variables, which each
/// use instantiates fresh.
// TODO Copy ?
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct TypeVariable {
    id: u32,
    /// The name the source wrote this variable by and what it wrote it in, when it is rigid.
    rigid: Option<(Name, Binder)>,
}

/// What a rigid variable was written in: the three kinds of declared type a body is held to.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Binder {
    /// A declaration's annotation, or the answer type of a derivation's binding.
    Annotation,
    /// An instance's head.
    InstanceHead,
    /// The signature of a class member, for the variables it binds beyond the class's own:
    /// the signature an instance's binding is held to.
    MemberSignature,
}

impl Binder {
    /// `of` this binder, as a message writes whose a variable is.
    fn of_phrase(self) -> &'static str {
        match self {
            Binder::Annotation => "the annotation",
            Binder::InstanceHead => "the instance head",
            Binder::MemberSignature => "the member's signature",
        }
    }

    /// What a message calls this binder's variable when it says which of two alike it means.
    fn possessive(self) -> &'static str {
        match self {
            Binder::Annotation => "the annotation's",
            Binder::InstanceHead => "the instance head's",
            Binder::MemberSignature => "the member signature's",
        }
    }
}

impl TypeVariable {
    /// The flexible variable numbered `id`.
    fn flexible(id: u32) -> TypeVariable {
        TypeVariable { id, rigid: None }
    }

    /// This variable made rigid under `name`: same `id`, so it is the variable the
    /// annotation's type was translated with, now one `unify` will not solve.
    fn into_rigid(self, name: Name, binder: Binder) -> TypeVariable {
        TypeVariable {
            id: self.id,
            rigid: Some((name, binder)),
        }
    }

    /// What this variable was written in, when it is rigid.
    fn binder(&self) -> Option<Binder> {
        self.rigid.as_ref().map(|(_, binder)| *binder)
    }

    /// Whether `unify` may not solve this variable to another type.
    fn is_rigid(&self) -> bool {
        self.rigid.is_some()
    }

    /// How a message writes this variable when no name has been given for it: the name the
    /// source wrote, for a rigid one, and `t3` for a flexible one, as [`Type`]'s
    /// `Display` does.
    fn spelling(&self) -> String {
        match &self.rigid {
            Some((name, _)) => name.to_string(),
            None => format!("t{}", self.id),
        }
    }
}

impl std::fmt::Debug for TypeVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.rigid {
            None => write!(f, "TypeVariable#{}", self.id),
            Some((name, _)) => write!(f, "TypeVariable#{}(rigid {})", self.id, name),
        }
    }
}

/// The type of an [opaque scalar](../../docs/spec/types.md#scalar-types): a type
/// nothing in the language builds or inspects, whose values arrive as literals.
///
/// `Bool` is not one of them: it is a scalar *and* an ordinary union, so its
/// type is a [`Type::Adt`] like any other union's, built by this module's `bool_type`.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeLiteral {
    Int,
    Char,
    Float,
    String,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Literal(TypeLiteral),
    Variable(TypeVariable),
    Fun {
        param_tpe: Box<Type>,
        return_tpe: Box<Type>,
    },
    Tuple(Tuple<Type>),
    /// [The unit type](../../docs/spec/types.md#the-unit-type), `()`, whose one
    /// value is also written `()`.
    ///
    /// A variant of its own, the way [`Type::Tuple`] is, rather than a [`Type::Adt`]
    /// known by the qualified name of a declaration the way `Bool` is: nothing
    /// declares `()`, it is a form of type expression and never a name, so there is
    /// no declaration to name and no same-spelled type it has to be told apart from.
    /// See `canonical::Type::Unit`.
    Unit,
    /// A named algebraic data type, e.g. `Maybe Int` declared in `Maybe` →
    /// `Adt(Maybe.Maybe, [Literal(Int)])`.
    ///
    /// The name is the declaring package's and module's, in full, because that is the
    /// identity of the type: `Widget.Size` and `Gadget.Size` are two types and a value
    /// of one is never a value of the other, and so are the `Size.Size` of two
    /// packages. The unifier's equality on this name is the only thing keeping them
    /// apart, so narrowing it to the spelling — which is what the typer used to carry —
    /// made every same-named declaration one type (`BUG-35`).
    /// [`Display`](std::fmt::Display) still writes the unqualified half, since that is
    /// how a module's source spells its own types.
    Adt(QualName, Vec<Type>),
    /// A [record type](../../docs/spec/records.md#a-record-type-is-a-set-of-fields): each
    /// label, and the type of the field it names.
    ///
    /// A map, for the reason `canonical::Type::Record` is one: a record type is a set of
    /// fields, so `{ x : Int, y : Int }` and `{ y : Int, x : Int }` are one value here and
    /// nothing downstream can tell the two spellings apart. The map is ordered by label,
    /// which is the order [`Display`](std::fmt::Display) writes the fields in.
    ///
    /// # How two record types unify
    ///
    /// They unify when they carry **the same set of labels** and each label's two field
    /// types unify; see `unify_one_constraint`. There is no row variable, and nothing
    /// ever adds a field to a record type: [records are
    /// closed](../../docs/spec/records.md#records-are-closed), so a variable only ever
    /// stands for a whole type, a record type included. Two record types with different
    /// label sets are a plain [`ErrorKind::UnificationFailed`], and its diagnostic names
    /// the labels each side has that the other lacks (see [`Error`]'s `notes`).
    ///
    /// Every walk over a type — substitution, the occurs check, instantiation, the zonk,
    /// and both renderings — goes into each field's type as it goes into a tuple's
    /// elements, and nothing else about a record is special to any of them.
    ///
    /// What does *not* produce one is a use of a record: an access, an update or an
    /// accessor says of a type only that it has some label, and is read against a record
    /// type something else supplied — see `FieldConstraint`.
    Record(BTreeMap<Name, Type>),
}

/// How the name of a [`Type::Adt`] is written out.
///
/// A type is normally quoted the way the source spells it, which for a union is its
/// bare name. That is ambiguous exactly when one message names two declarations that
/// share a spelling, and [`ErrorKind::message`] switches to the qualified form there.
#[derive(Debug, Clone, Copy)]
enum AdtNames<'a> {
    /// `Size`.
    Unqualified,
    /// `Widget.Size`, or `AcmeWidgets.Size.Size`: the union's module as the checked
    /// package spells it, then its own name — see [`Spellings`].
    Qualified(&'a Spellings),
}

impl AdtNames<'_> {
    /// Whether the types a single message is about have to be written qualified for
    /// that message to distinguish the declarations it names.
    ///
    /// The question is not whether the *types* are spelled alike — `A.Size` against
    /// `Lib.Size A.Size` renders as `Size` against `Size Size`, which differs as
    /// text while still using one word for three declarations. It is whether any two
    /// of the unions named anywhere in those types are different declarations with
    /// the same bare name; if so every union in the sentence is qualified, since
    /// qualifying only the colliding pair would read as if the rest had no module.
    /// Two declarations may share their module's name too, when two packages each
    /// hold one, and they are still two: a [`QualName`] carries its package.
    fn collide<'t>(types: impl IntoIterator<Item = &'t Type>) -> bool {
        let mut names: Vec<&QualName> = Vec::new();

        for tpe in types {
            tpe.collect_adt_names(&mut names);
        }

        names.iter().enumerate().any(|(i, name)| {
            names[i + 1..]
                .iter()
                .any(|other| *other != *name && other.unqualified_name() == name.unqualified_name())
        })
    }
}

/// How the package being checked spells each module in reach: the key `compile_package`
/// stores that module's [`Interface`] under in the map it hands [`type_check`], and the
/// module under check's own name.
///
/// A qualified message writes a union by this spelling rather than by its declaring
/// module's own name, because the two differ for a wrapped dependency — and they have to
/// differ in the message whenever two packages each hold a module of the same name.
/// `AcmeWidgets.Size.Size` against `Size.Size` tells the two unions apart, and is what
/// the checked package's own source writes; `Size.Size` against `Size.Size` would not,
/// and the package name — `acme-widgets` — is a spelling no Zelkova source contains.
#[derive(Debug, Clone, Default)]
struct Spellings(HashMap<ModuleName, Name>);

impl Spellings {
    /// The spellings of every module `interfaces` holds, plus `module`'s own.
    ///
    /// A module reachable by two spellings is written by the first in alphabetical
    /// order, so the message does not change between runs.
    fn of(module: &Module, interfaces: &HashMap<Name, Interface>) -> Spellings {
        let mut spellings: HashMap<ModuleName, Name> = HashMap::new();

        for (spelling, interface) in interfaces {
            spellings
                .entry(interface.module_name.clone())
                .and_modify(|kept| {
                    if spelling.as_str() < kept.as_str() {
                        *kept = spelling.clone();
                    }
                })
                .or_insert_with(|| spelling.clone());
        }

        spellings.insert(module.name.clone(), module.name.name().clone());

        Spellings(spellings)
    }

    /// `name` as the checked package writes it: its module's spelling, then its own
    /// name. A module with no spelling here — one the checked package cannot import,
    /// whose union reached it through another module's signature — is written by its
    /// own name.
    fn spell(&self, name: &QualName) -> String {
        let module = ModuleName::new(name.package().clone(), name.module_name());

        match self.0.get(&module) {
            Some(spelling) => format!("{}.{}", spelling, name.unqualified_name()),
            None => name.to_name().to_string(),
        }
    }
}

/// A [`Type`] written with every union named by its module, as the checked package
/// spells it.
///
/// The counterpart of `Type`'s own [`Display`](std::fmt::Display), which writes the
/// unqualified half.
struct Qualified<'a>(&'a Type, &'a Spellings);

impl std::fmt::Display for Qualified<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.write(f, AdtNames::Qualified(self.1), None)
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Literal(lit) => write!(f, "Lit({:?})", lit),
            Type::Variable(TypeVariable { id, rigid: None }) => write!(f, "Var(#{})", id),
            Type::Variable(TypeVariable {
                id,
                rigid: Some((name, _)),
            }) => write!(f, "Rigid({}#{})", name, id),
            Type::Fun {
                param_tpe,
                return_tpe,
            } => write!(f, "Fun({:?} -> {:?})", param_tpe, return_tpe),
            Type::Tuple(Tuple::Two(a, b)) => write!(f, "({:?}, {:?})", a, b),
            Type::Tuple(Tuple::Three(a, b, c)) => write!(f, "({:?}, {:?}, {:?})", a, b, c),
            Type::Unit => write!(f, "()"),
            Type::Adt(name, args) if args.is_empty() => write!(f, "{}", name.to_name()),
            Type::Adt(name, args) => write!(f, "{}({:?})", name.to_name(), args),
            Type::Record(fields) => {
                let fields: Vec<String> = fields
                    .iter()
                    .map(|(label, tpe)| format!("{}: {:?}", label.as_str(), tpe))
                    .collect();
                write!(f, "{{{}}}", fields.join(", "))
            }
        }
    }
}

/// Writes a type the way the source would spell it, so diagnostics can quote it.
///
/// This is deliberately not `Debug`: `Debug` prints the typer's own vocabulary
/// (`Lit(Int)`, `Var(#3)`), which is what you want at a breakpoint and never what
/// you want in a message the user reads. Inference variables have no source syntax
/// at all, so they are written `t3` — Elm's convention for an unsolved variable.
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write(f, AdtNames::Unqualified, None)
    }
}

impl Type {
    /// The type as a message writes one that has variables of no source's naming: each
    /// variable is a letter, `a` to `z` and on, in the order it first appears, where
    /// [`Display`](std::fmt::Display) writes the inference variable (`t10122`) that no
    /// source mentions. A type with no variable reads the same either way.
    pub(crate) fn written_with_letters(&self) -> String {
        let names: VariableNames = free_variables_in_order(self)
            .into_iter()
            .enumerate()
            .map(|(position, variable)| (variable, classes::letter(position)))
            .collect();
        WithNames(self, &names).to_string()
    }

    /// Every union named anywhere in this type, outermost first, appended to `out`.
    ///
    /// A union's arguments are types in their own right and may name unions of their
    /// own, so this recurses rather than reading the head alone. [`AdtNames::collide`]
    /// is what it exists for: deciding how to write a type means looking at every
    /// name the rendering will contain, not only the one at the top.
    fn collect_adt_names<'a>(&'a self, out: &mut Vec<&'a QualName>) {
        match self {
            Type::Literal(_) | Type::Variable(_) | Type::Unit => {}
            Type::Fun {
                param_tpe,
                return_tpe,
            } => {
                param_tpe.collect_adt_names(out);
                return_tpe.collect_adt_names(out);
            }
            Type::Tuple(Tuple::Two(a, b)) => {
                a.collect_adt_names(out);
                b.collect_adt_names(out);
            }
            Type::Tuple(Tuple::Three(a, b, c)) => {
                a.collect_adt_names(out);
                b.collect_adt_names(out);
                c.collect_adt_names(out);
            }
            Type::Adt(name, args) => {
                out.push(name);
                for arg in args {
                    arg.collect_adt_names(out);
                }
            }
            Type::Record(fields) => {
                for tpe in fields.values() {
                    tpe.collect_adt_names(out);
                }
            }
        }
    }

    /// The body of every rendering: the same text each time, except for how a union is
    /// named and how a variable is. See [`AdtNames`], and [`Display`](std::fmt::Display)
    /// for why unions are normally written bare.
    ///
    /// A variable is `t3` unless `variables` has a name for it, which is how a message
    /// writes the variables of an annotation the way the source did.
    fn write(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        names: AdtNames,
        variables: Option<&VariableNames>,
    ) -> std::fmt::Result {
        /// The same, wrapped in parentheses.
        fn parenthesised(
            tpe: &Type,
            f: &mut std::fmt::Formatter<'_>,
            names: AdtNames,
            variables: Option<&VariableNames>,
        ) -> std::fmt::Result {
            write!(f, "(")?;
            tpe.write(f, names, variables)?;
            write!(f, ")")
        }

        match self {
            Type::Literal(TypeLiteral::Int) => write!(f, "Int"),
            Type::Literal(TypeLiteral::Char) => write!(f, "Char"),
            Type::Literal(TypeLiteral::Float) => write!(f, "Float"),
            Type::Literal(TypeLiteral::String) => write!(f, "String"),
            Type::Variable(variable) => match variables.and_then(|names| names.get(variable)) {
                Some(name) => write!(f, "{}", name),
                None => write!(f, "{}", variable.spelling()),
            },
            // The parameter of a function type is parenthesised when it is itself a
            // function, because `->` is right-associative: `(a -> b) -> c` and
            // `a -> b -> c` are different types.
            Type::Fun {
                param_tpe,
                return_tpe,
            } => {
                match **param_tpe {
                    Type::Fun { .. } => parenthesised(param_tpe, f, names, variables)?,
                    _ => param_tpe.write(f, names, variables)?,
                }
                write!(f, " -> ")?;
                return_tpe.write(f, names, variables)
            }
            Type::Tuple(Tuple::Two(a, b)) => {
                write!(f, "( ")?;
                a.write(f, names, variables)?;
                write!(f, ", ")?;
                b.write(f, names, variables)?;
                write!(f, " )")
            }
            Type::Tuple(Tuple::Three(a, b, c)) => {
                write!(f, "( ")?;
                a.write(f, names, variables)?;
                write!(f, ", ")?;
                b.write(f, names, variables)?;
                write!(f, ", ")?;
                c.write(f, names, variables)?;
                write!(f, " )")
            }
            Type::Unit => write!(f, "()"),
            Type::Adt(name, args) => {
                match names {
                    AdtNames::Unqualified => write!(f, "{}", name.unqualified_name())?,
                    AdtNames::Qualified(spellings) => write!(f, "{}", spellings.spell(name))?,
                }
                for arg in args {
                    // Same reason as above: an argument that is itself applied or a
                    // function needs parentheses to stay the same type when re-read.
                    write!(f, " ")?;
                    match arg {
                        Type::Adt(_, inner) if !inner.is_empty() => {
                            parenthesised(arg, f, names, variables)?
                        }
                        Type::Fun { .. } => parenthesised(arg, f, names, variables)?,
                        _ => arg.write(f, names, variables)?,
                    }
                }
                Ok(())
            }
            // The spelling [Records](../../docs/spec/records.md#the-type) uses, the
            // fields in label order. The braces delimit it, so no position needs it
            // parenthesised, and a field's type is written as it is anywhere else.
            Type::Record(fields) => {
                write!(f, "{{ ")?;
                for (position, (label, tpe)) in fields.iter().enumerate() {
                    if position > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} : ", label.as_str())?;
                    tpe.write(f, names, variables)?;
                }
                write!(f, " }}")
            }
        }
    }
}

/// Two types that have to match, and why.
///
/// # What the two sides mean
///
/// Unification treats them symmetrically; the order is a rendering convention, and
/// the only thing that reads it is the headline — *cannot match `left` with `right`*.
/// `collect` therefore writes the declared or expected side first where the source
/// has one (an annotation's type, a pattern's type) and the inferred side second, so
/// the sentence comes out in the order a reader expects.
///
/// What the sides are emphatically *not* is a way to tell which type belongs to the
/// text at `origin.span`. By the time a constraint fails, `unify` has substituted
/// other constraints' solutions into both sides, and may have decomposed it into a
/// component of the types the source mentioned. That is why the labels name the
/// [`Reason`] and leave the types to the headline — see [`Reason::describes`].
///
/// # Why these are held in a `Vec` and not a `HashSet`
///
/// They used to be a `HashSet<Constraint>`, back when a constraint was a bare pair of
/// types. An origin makes that collection wrong twice over. Deduplication now
/// discards *provenance*: two constraints with equal types but different origins are
/// one entry, and which origin survives is whichever was inserted first. And the
/// order a `HashSet` yields is unspecified, so which constraint `unify` reaches first
/// — and therefore which one is reported when several are unsatisfiable — would vary
/// between runs of the same compiler on the same file.
///
/// A `Vec` fixes both, and buys a third thing: source order. The annotation is
/// pushed first, so its type is substituted into the body's constraints before they
/// are solved, which is what lets a mismatch deep in the body say that `Int` came
/// from the annotation. The cost is that duplicate constraints are no longer
/// collapsed, which is a few more `unify` steps on terms that repeat a type.
#[derive(Debug, Clone, PartialEq)]
struct Constraint {
    left: Type,
    right: Type,
    origin: Origin,
}

impl Constraint {
    fn new(left: Type, right: Type, reason: Reason, span: NodeSpan) -> Constraint {
        Constraint {
            left,
            right,
            origin: Origin::new(reason, span),
        }
    }

    /// A constraint between two components of this one — the parameters of two
    /// function types being matched, say — which is about the same source text and
    /// was required for the same reason.
    ///
    /// The origin is inherited whole, side provenance included: if a substitution
    /// rewrote the left type, it rewrote whatever the left type decomposes into. That
    /// is an over-approximation — a substitution that reached only the return half of
    /// an arrow is credited with the parameter half too — but it errs towards naming
    /// a constraint that did carry a type in, which is the direction that keeps a
    /// caret on the source rather than on the compiler's working.
    fn component(&self, left: Type, right: Type) -> Constraint {
        Constraint {
            left,
            right,
            origin: self.origin.clone(),
        }
    }
}

/// Which use of a record wrote a `FieldConstraint`. It decides what the constraint's
/// errors say and which text their carets are under; nothing about how the constraint
/// is solved depends on it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecordUse {
    /// `record.label`.
    Access,
    /// `.label`, applied or not.
    Accessor,
    /// One field of `{ record | label = value }`.
    Update,
    /// One entry of a record pattern, `{ label = pattern }` or `{ label }`.
    Pattern,
}

/// What could supply the record type an [`ErrorKind::RecordTypeUnknown`] is missing —
/// which is what its note tells the reader to do about it. `unifier::unknown` decides it.
///
/// "Part of the declaration's type" means a variable of the declaration's solved type
/// — a parameter's, the result's, or inside either — or the field type of another use
/// whose record type is one: an annotation, unified with that type, would solve the
/// variable, and reading the use would solve the next.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Supplier {
    /// The declaration has no annotation, and the record type is part of its type: an
    /// annotation writing a record type there would supply it.
    Annotation,
    /// The record type is part of the declaration's type, and the declaration's
    /// annotation writes a type variable there — `f : a -> Int` with body `r.x` — or
    /// around it.
    AnnotationVariable,
    /// The record type is not part of the declaration's type — `.x` passed where any
    /// value is taken, as in `first n .x` — so no annotation on the declaration could
    /// supply it, and only its body can.
    Body,
}

/// A type said to be a record holding `label`, with that field at `field`: what a field
/// access, an update, an accessor and each entry of a record pattern say about the record
/// they use.
///
/// # Why it is not a [`Constraint`]
///
/// `person.name` says of `person`'s type only that it has a `name`. That is not an
/// equation between two types this unifier can write: a record type names every field it
/// has ([records are closed](../../docs/spec/records.md#records-are-closed)), and there is
/// no row variable to stand for the fields `person.name` does not mention. Solving it
/// would mean inventing a record type out of the labels the declaration happens to touch,
/// which is exactly what [the language rules
/// out](../../docs/spec/records.md#a-use-does-not-decide-a-records-type): `nameOf person =
/// person.name` would take a record with one field. So a field constraint is never an
/// equation while its record type is unknown, and **nothing here ever solves a variable
/// standing for a record type**. The type has to come from somewhere else in the
/// declaration — its annotation, a record expression, a function of known type, a
/// constructor's argument — and the constraint only reads it.
///
/// # When it is read
///
/// After [`unifier::unify`] has solved every ordinary constraint of the declaration, by
/// `unifier::read_fields`. What supplies the record type may be written after the form
/// as well as before it, so a field constraint cannot be decided at the point
/// `constraint::collect` meets it; with the whole declaration solved, it can. Then, with
/// the substitution applied to `record`:
///
/// - **A record type holding `label`**: the constraint becomes the ordinary equation
///   between that field's type and `field`, solved at once and merged into the
///   substitution. A mismatch there is an [`ErrorKind::UnificationFailed`] with the
///   caret at `field_span` and the constraint's own `reason`.
/// - **A record type without it**: [`ErrorKind::MissingField`], at `label_span`. For an
///   update that is the update that would add a field.
/// - **Another type that is not a variable**: [`ErrorKind::NotARecord`], at
///   `label_span`. Each of these two carries what brought the record type in — the
///   probe's origin's cause for its left side — for a secondary label, as a mismatch's
///   explanation is.
/// - **Still a variable**: not decided yet. Reading another field constraint may solve
///   it: in `r.a.b` the record type of `.b` is the field type `.a` reads, and in `(.a
///   r).b` it is the accessor's result. So the constraints are read in passes, in the
///   order they were collected — which is the order their labels were written — each
///   pass seeing what the ones before it solved. A pass that decides none of the
///   constraints left is the end: nothing will ever supply their record types, and one
///   of them is [`ErrorKind::RecordTypeUnknown`], with the caret on the whole form —
///   unless a name that did not resolve, a hole, explains every one of them, its own
///   error standing already. `unifier::unknown` says which one and when a hole explains
///   it. Every pass decides at least one constraint or is the last, so the passes stop.
///
/// A record type only becomes *more* known as the passes go, never less, so whether a
/// declaration's field constraints are all satisfied does not depend on the order they
/// are read in. Which of several unknown ones is reported does not depend on it either
/// as long as one is the root the others wait on — `r.a` in an unannotated `r.a.b` or
/// `.b r.a` — and otherwise the order picks it, the same on every run. Only that one is
/// reported, as only a declaration's first failed equation is.
///
/// # Why there is nothing to generalise past
///
/// A declaration is put in the environment other declarations are checked against by
/// its annotation alone, and one with no annotation is not put there at all
/// ([`Solved::UnboundName`]). So a field constraint is always read inside the one
/// declaration that wrote it, and a use of that declaration from another one never
/// supplies its record type — which is the rule as
/// [Records](../../docs/spec/records.md#a-use-does-not-decide-a-records-type) states it.
/// A type variable an annotation writes, `get : { x : a } -> a`, is a variable inside a
/// record type and not a record type's variable, so `r.x` reads it; one written for the
/// record itself, `f : a -> Int` with body `r.x`, supplies no record type and is
/// [`ErrorKind::RecordTypeUnknown`].
///
/// That rests on there being no generalisation step. One that generalised a type still
/// carrying a waiting field constraint — a `let`-bound `get r = r.x` used as `get { x =
/// 1 }`, once [`LANG-33`](../../docs/tickets/lang-33.md) brings `let` — would copy the
/// constraint's variable at each use, and the constraint, waiting on the original, would
/// never be decided: it would fail safe, as a `RecordTypeUnknown`, but not by the rule.
/// So a generalisation step must read the field constraints first, or refuse to
/// generalise over a variable one of them is still waiting on.
///
/// # A record pattern
///
/// A record pattern names a subset of a record's fields and says of the type it is matched
/// against what an accessor says of its argument's, once per entry: it is the fourth
/// [`RecordUse`], one field constraint per entry, whose `record` is the type the pattern
/// is matched against — a `case`'s scrutinee, a parameter's, or the type of the position
/// it is written in inside another pattern — and whose `field` is the entry's own type,
/// the one its sub-pattern is held to and a name it binds is bound at. `form_span` is the
/// whole pattern, `label_span` the entry's label and `field_span` the entry's own pattern,
/// so a mismatch between that pattern and the field's declared type is blamed on the
/// sub-pattern. `constraint::pattern_constraints` writes them in the order the labels were
/// written at any depth: each entry just before the entries of a record pattern nested in
/// it, and those before the next entry, so `{ a = { b }, d }` gives `a`, `b`, `d`. The
/// field types of a pattern nested in an entry, `{ centre = { x } }`, are therefore read
/// in turn as `r.a.b`'s are.
///
/// That is the mechanism unchanged, but not quite nothing more than a variant:
/// `constraint::pattern_constraints`, which `constraint::collect` reaches through `walk`,
/// pushes onto the field list as well as onto the equations, the equation a decided entry
/// becomes carries a [`Reason`] of its own — one of three, by whether the entry's pattern
/// is a name, binds none or binds some, since that decides what a failure of it can mean
/// and so what its note says — and the argument types of a constructor pattern that did
/// not resolve count as holes, since that constructor's real type is what would have
/// supplied a record pattern written as one of its arguments.
///
/// The read comes after every equation, the branch body's included, so a name a record
/// pattern binds is solved by the body's use of it before the entry is read. A body that
/// uses `name` at another type than its field's is therefore reported at the entry, as a
/// mismatch of the field's type with the use's, and not at the body, as it would be under
/// a tuple pattern, whose equation comes first ([`ERR-20`](../../docs/tickets/err-20.md)).
/// Only the note, [`Reason::RecordPatternBinding`]'s, says that a use is what failed.
///
/// # What else is read late
///
/// A class obligation is the other kind of constraint the solver can only answer once
/// unification has run: a third list beside this one in `constraint::Constraints`, read
/// by `classes::discharge` after `unifier::read_fields` in `infer_annotated` — after,
/// because reading a field can solve the variable an instance is looked up by. Reading an
/// obligation adds nothing to the substitution: an instance's context is more obligations
/// and never an equation, so no obligation can decide a field constraint, and the order of
/// the two reads has one direction only.
#[derive(Debug, Clone)]
struct FieldConstraint {
    /// The type said to be a record. Usually still a variable when collected.
    record: Type,
    label: Name,
    /// The type the field is used at: the access's own type, an update's new value's, an
    /// accessor's result, or a record pattern's entry's.
    field: Type,
    form: RecordUse,
    /// The reason the equation this constraint becomes carries, once its record type is
    /// known: [`Reason::Access`], [`Reason::Accessor`] or [`Reason::UpdateField`] for the
    /// first three forms, and for a record pattern's entry one of the three record
    /// pattern reasons, chosen by what the entry's own pattern binds.
    reason: Reason,
    /// The whole form — the caret of [`ErrorKind::RecordTypeUnknown`].
    form_span: NodeSpan,
    /// The label for an access, an update and a record pattern's entry, and the whole
    /// accessor for an accessor — the caret of [`ErrorKind::MissingField`] and
    /// [`ErrorKind::NotARecord`].
    label_span: NodeSpan,
    /// The text whose type is `field` — the access, the new value, the accessor, the
    /// entry's own pattern — and the caret of a mismatch between it and the field's
    /// declared type.
    field_span: NodeSpan,
}

impl FieldConstraint {
    /// This constraint read against everything solved so far: the equation it has
    /// become, `None` while its record type is still a variable, or the error it is.
    ///
    /// The equation is written declared-first, as `Constraint` asks: the field's type in
    /// the record, then the type it is used at. Its origin is the one
    /// [`Substitution::apply`] gives a constraint between `record` and `field`, so the
    /// declared side is explained by whatever brought the record type in — the
    /// annotation, typically — and a mismatch can say so.
    fn read(&self, substitution: &Substitution) -> Result<Option<Constraint>, ErrorKind> {
        let probe = substitution.apply(&Constraint::new(
            self.record.clone(),
            self.field.clone(),
            self.reason,
            self.field_span,
        ));

        match &probe.left {
            Type::Variable(_) => Ok(None),
            Type::Record(fields) => match fields.get(&self.label) {
                Some(declared) => Ok(Some(Constraint {
                    left: declared.clone(),
                    right: probe.right,
                    origin: probe.origin,
                })),
                None => Err(ErrorKind::MissingField {
                    record: Box::new(probe.left.clone()),
                    label: self.label.clone(),
                    form: self.form,
                    span: self.label_span,
                    because: probe.origin.left_from,
                }),
            },
            other => Err(ErrorKind::NotARecord {
                tpe: Box::new(other.clone()),
                label: self.label.clone(),
                form: self.form,
                span: self.label_span,
                because: probe.origin.left_from,
            }),
        }
    }

    /// The error this constraint is when nothing ever supplied its record type.
    fn unknown(&self, supplier: Supplier) -> ErrorKind {
        ErrorKind::RecordTypeUnknown {
            label: self.label.clone(),
            form: self.form,
            span: self.form_span,
            supplier,
        }
    }
}

/// What one type variable was solved to, and where that type came from.
///
/// The cause is not used by inference at all. It is carried so that when this
/// solution is substituted into another constraint and *that* constraint then fails,
/// the failure can say where the type it failed against came from — see
/// [`Origin::left_from`].
///
/// It is a [`Cause`] and not an `Origin` because the question it answers is already
/// settled: [`Origin::cause_of`] resolved, at the moment the variable was solved,
/// whether the solving constraint introduced this type or was handed it. Nothing
/// downstream has to walk anything.
// No `Eq`: a `Cause` holds a `NodeSpan`, whose `PartialEq` is deliberately blind
// (see its documentation), so equality here is a claim about the types and the
// reasons, not about the positions.
#[derive(Debug, PartialEq, Clone)]
struct Solution {
    tpe: Type,
    cause: Cause,
}

#[derive(Debug, PartialEq)]
struct Substitution {
    solutions: HashMap<TypeVariable, Solution>,
}

impl Substitution {
    // constructors

    fn empty() -> Substitution {
        Substitution {
            solutions: HashMap::new(),
        }
    }

    fn one(tvar: TypeVariable, tpe: Type, cause: Cause) -> Substitution {
        let mut sub = Substitution::empty();

        sub.solutions.insert(tvar, Solution { tpe, cause });

        sub
    }

    // methods

    /// Rewrite a constraint with everything solved so far, recording for each side
    /// which solution first reached it.
    ///
    /// The two sides are tracked apart, because that is what tells a relayed type
    /// from an introduced one later on — see [`Origin::cause_of`]. A solution that
    /// rewrites only the right side has said nothing about the left, and treating it
    /// as though it had is what made a type error blame an annotation that was not
    /// load-bearing.
    ///
    /// The solutions are visited in type-variable order rather than in `HashMap`
    /// order: "first" has to mean the same thing on every run, or the secondary label
    /// of a diagnostic would move between compilations of an unchanged file. Ids are
    /// handed out as inference walks the term, so that order is roughly the order the
    /// user wrote things in.
    fn apply(&self, c: &Constraint) -> Constraint {
        let mut origin = c.origin.clone();

        let mut solutions: Vec<_> = self.solutions.iter().collect();
        solutions.sort_by_key(|(tvar, _)| tvar.id);

        for (tvar, solution) in solutions {
            // A solution that does not mention a variable a side uses rewrites
            // nothing there, and explains nothing about it either.
            if occurs(tvar, &c.left) {
                origin.rewritten(Side::Left, solution.cause);
            }
            if occurs(tvar, &c.right) {
                origin.rewritten(Side::Right, solution.cause);
            }
        }

        Constraint {
            left: self.apply_type(&c.left),
            right: self.apply_type(&c.right),
            origin,
        }
    }

    fn apply_type(&self, tpe: &Type) -> Type {
        self.solutions
            .iter()
            .fold(tpe.clone(), |tpe, (tvar, solution)| {
                Substitution::substitute(tpe, tvar, &solution.tpe)
            })
    }

    /// Rewrite **every** type in a typed term with everything solved — the *zonk*.
    ///
    /// `annotate` gives each node a fresh inference variable and unification solves
    /// those variables somewhere else entirely, so until this runs a node's `tpe` is a
    /// `t17` that means nothing on its own. Applying the substitution to the root's
    /// type alone is enough to answer "what type is this declaration", which is all
    /// inference ever needed; it leaves every node below still holding a variable,
    /// which is not enough to generate code from
    /// ([`DEC-18` decision 1](../../docs/decisions/dec-18.md)).
    ///
    /// The types a pattern carries are rewritten too: a constructor pattern's
    /// arguments and the bindings it introduces are the types the branch body's
    /// variables were bound at.
    fn apply_term(&self, term: TypedTerm) -> TypedTerm {
        let kind = match term.kind {
            kind @ (TypedTermKind::Int(_)
            | TypedTermKind::Char(_)
            | TypedTermKind::String(_)
            | TypedTermKind::Float(_)
            | TypedTermKind::Unit
            | TypedTermKind::Hole) => kind,
            TypedTermKind::Identifier { reference, context } => TypedTermKind::Identifier {
                reference,
                context: self.apply_predicates(context),
            },
            TypedTermKind::Fun { param, body } => TypedTermKind::Fun {
                param: self.apply_binder(param),
                body: Box::new(self.apply_term(*body)),
            },
            TypedTermKind::Apply {
                fun,
                arg,
                saturation,
            } => TypedTermKind::Apply {
                fun: Box::new(self.apply_term(*fun)),
                arg: Box::new(self.apply_term(*arg)),
                saturation,
            },
            TypedTermKind::If {
                cond,
                true_branch,
                false_branch,
            } => TypedTermKind::If {
                cond: Box::new(self.apply_term(*cond)),
                true_branch: Box::new(self.apply_term(*true_branch)),
                false_branch: Box::new(self.apply_term(*false_branch)),
            },
            TypedTermKind::Let {
                binding,
                value,
                body,
            } => TypedTermKind::Let {
                binding: self.apply_binder(binding),
                value: Box::new(self.apply_term(*value)),
                body: Box::new(self.apply_term(*body)),
            },
            TypedTermKind::Tuple(Tuple::Two(a, b)) => {
                TypedTermKind::Tuple(Tuple::two(self.apply_term(*a), self.apply_term(*b)))
            }
            TypedTermKind::Tuple(Tuple::Three(a, b, c)) => TypedTermKind::Tuple(Tuple::three(
                self.apply_term(*a),
                self.apply_term(*b),
                self.apply_term(*c),
            )),
            TypedTermKind::Case {
                scrutinee,
                branches,
                form,
            } => TypedTermKind::Case {
                scrutinee: Box::new(self.apply_term(*scrutinee)),
                branches: branches
                    .into_iter()
                    .map(|(pattern, body)| {
                        (
                            self.apply_pattern(pattern),
                            Box::new(self.apply_term(*body)),
                        )
                    })
                    .collect(),
                form,
            },
            TypedTermKind::Record(fields) => TypedTermKind::Record(self.apply_fields(fields)),
            TypedTermKind::Update { record, fields } => TypedTermKind::Update {
                record: Box::new(self.apply_term(*record)),
                fields: self.apply_fields(fields),
            },
            TypedTermKind::Access {
                record,
                label,
                label_span,
            } => TypedTermKind::Access {
                record: Box::new(self.apply_term(*record)),
                label,
                label_span,
            },
            kind @ TypedTermKind::Accessor { .. } => kind,
        };

        TypedTerm {
            span: term.span,
            tpe: self.apply_type(&term.tpe),
            kind,
        }
    }

    /// Rewrite the type of each predicate: the context of a use, or a declaration's own,
    /// as it stands once everything is solved.
    fn apply_predicates(&self, predicates: Vec<Predicate>) -> Vec<Predicate> {
        predicates
            .into_iter()
            .map(|predicate| Predicate {
                class: predicate.class,
                tpe: self.apply_type(&predicate.tpe),
            })
            .collect()
    }

    fn apply_fields(&self, fields: Vec<Field<TypedTerm>>) -> Vec<Field<TypedTerm>> {
        fields
            .into_iter()
            .map(|field| Field {
                label: field.label,
                label_span: field.label_span,
                value: self.apply_term(field.value),
            })
            .collect()
    }

    fn apply_binder(&self, binder: TypeBinder) -> TypeBinder {
        TypeBinder {
            tpe: self.apply_type(&binder.tpe),
            name: binder.name,
        }
    }

    fn apply_pattern(&self, pattern: TermPattern) -> TermPattern {
        let kind = match pattern.kind {
            kind @ (TermPatternKind::Anything
            | TermPatternKind::Bind(_)
            | TermPatternKind::Unit) => kind,
            TermPatternKind::Literal { tpe, value } => TermPatternKind::Literal {
                tpe: self.apply_type(&tpe),
                value,
            },
            TermPatternKind::Constructor {
                ctor,
                adt_args,
                args,
            } => TermPatternKind::Constructor {
                ctor,
                adt_args: adt_args.iter().map(|a| self.apply_type(a)).collect(),
                args: args
                    .into_iter()
                    .map(|arg| self.apply_sub_pattern(arg))
                    .collect(),
            },
            TermPatternKind::Tuple { elements } => TermPatternKind::Tuple {
                elements: elements.map(|element| self.apply_sub_pattern(element.clone())),
            },
            TermPatternKind::Hole { args } => TermPatternKind::Hole {
                args: args
                    .into_iter()
                    .map(|arg| self.apply_sub_pattern(arg))
                    .collect(),
            },
            TermPatternKind::Record { fields } => TermPatternKind::Record {
                fields: fields
                    .into_iter()
                    .map(|field| Field {
                        label: field.label,
                        label_span: field.label_span,
                        value: self.apply_sub_pattern(field.value),
                    })
                    .collect(),
            },
        };

        TermPattern {
            span: pattern.span,
            kind,
        }
    }

    fn apply_sub_pattern(&self, sub: SubPattern) -> SubPattern {
        SubPattern {
            tpe: self.apply_type(&sub.tpe),
            pattern: self.apply_pattern(sub.pattern),
        }
    }

    fn substitute(tpe: Type, tvar: &TypeVariable, replacement: &Type) -> Type {
        match tpe {
            Type::Literal(_) | Type::Unit => tpe,
            Type::Fun {
                param_tpe,
                return_tpe,
            } => Type::Fun {
                param_tpe: Box::new(Substitution::substitute(*param_tpe, tvar, replacement)),
                return_tpe: Box::new(Substitution::substitute(*return_tpe, tvar, replacement)),
            },
            Type::Tuple(Tuple::Two(a, b)) => Type::Tuple(Tuple::two(
                Substitution::substitute(*a, tvar, replacement),
                Substitution::substitute(*b, tvar, replacement),
            )),
            Type::Tuple(Tuple::Three(a, b, c)) => Type::Tuple(Tuple::three(
                Substitution::substitute(*a, tvar, replacement),
                Substitution::substitute(*b, tvar, replacement),
                Substitution::substitute(*c, tvar, replacement),
            )),
            Type::Adt(name, args) => Type::Adt(
                name,
                args.into_iter()
                    .map(|a| Substitution::substitute(a, tvar, replacement))
                    .collect(),
            ),
            Type::Record(fields) => Type::Record(
                fields
                    .into_iter()
                    .map(|(label, tpe)| (label, Substitution::substitute(tpe, tvar, replacement)))
                    .collect(),
            ),
            Type::Variable(tvar2) if tvar == &tvar2 => replacement.clone(),
            tpe @ Type::Variable(_) => tpe,
        }
    }

    fn merge(&self, other: Substitution) -> Substitution {
        // This merge means we should try sub_tail first, and then sub_head
        // Merging other in self means we apply `other` substitution to `self` solutions
        // When merging, we want `other` solutions to take precedences over `self` solutions

        let self_solutions = self.solutions.iter().map(|(k, v)| {
            (
                k.clone(),
                Solution {
                    tpe: other.apply_type(&v.tpe),
                    // The cause says where this variable's type came from, which
                    // rewriting the type does not change.
                    cause: v.cause,
                },
            )
        });

        let mut sub = Substitution::empty();

        sub.solutions.extend(self_solutions);
        sub.solutions.extend(other.solutions);

        sub
    }
}

/// Does `tvar` appear anywhere inside `tpe`?
///
/// Two callers, for two different reasons: `unify_variable` uses it as the occurs
/// check that keeps it from building an infinite type, and `Substitution::apply` uses
/// it to tell whether a solution actually rewrites a given constraint — which is what
/// decides whether that solution explains one of the constraint's types.
fn occurs(tvar: &TypeVariable, tpe: &Type) -> bool {
    match tpe {
        Type::Fun {
            param_tpe,
            return_tpe,
        } => occurs(tvar, param_tpe) || occurs(tvar, return_tpe),
        Type::Tuple(tuple) => tuple.iter().any(|t| occurs(tvar, t)),
        Type::Adt(_, args) => args.iter().any(|a| occurs(tvar, a)),
        Type::Record(fields) => fields.values().any(|t| occurs(tvar, t)),
        Type::Variable(tvar2) => tvar == tvar2,
        _ => false,
    }
}

/// Every variable of `tpe`, each once, in the order it first appears.
fn free_variables_in_order(tpe: &Type) -> Vec<TypeVariable> {
    fn walk(tpe: &Type, into: &mut Vec<TypeVariable>) {
        match tpe {
            Type::Literal(_) | Type::Unit => (),
            Type::Variable(tvar) => {
                if !into.contains(tvar) {
                    into.push(tvar.clone());
                }
            }
            Type::Fun {
                param_tpe,
                return_tpe,
            } => {
                walk(param_tpe, into);
                walk(return_tpe, into);
            }
            Type::Tuple(tuple) => tuple.iter().for_each(|t| walk(t, into)),
            Type::Adt(_, args) => args.iter().for_each(|t| walk(t, into)),
            Type::Record(fields) => fields.values().for_each(|t| walk(t, into)),
        }
    }

    let mut variables = Vec::new();
    walk(tpe, &mut variables);
    variables
}

/// A [`Type`] written with its variables named, the way an annotation wrote them, and
/// every union bare.
struct WithNames<'a>(&'a Type, &'a VariableNames);

impl std::fmt::Display for WithNames<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.write(f, AdtNames::Unqualified, Some(self.1))
    }
}

/// A predicate written as the source writes a constraint's use: the class, then the type,
/// in parentheses when it is itself applied.
fn predicate_text(predicate: &Predicate, spellings: &Spellings) -> String {
    let applied = match &predicate.tpe {
        Type::Fun { .. } => true,
        Type::Adt(_, args) => !args.is_empty(),
        _ => false,
    };
    let tpe = Spelled(&predicate.tpe, spellings);

    if applied {
        format!("{} ({})", predicate.class.unqualified_name(), tpe)
    } else {
        format!("{} {}", predicate.class.unqualified_name(), tpe)
    }
}

/// The names in scope while one declaration is annotated, and the counter its fresh
/// type variables come from.
///
/// Two kinds of name, looked up differently:
///
/// - A **binder** — a parameter, a `case` binding — is introduced by the term being
///   annotated. Its type is one type: every use of it shares its variables, which is
///   what lets inference learn a parameter's type from how the body uses it.
/// - A **global** — a declaration of this module or of an imported one, or a
///   constructor — comes from `type_check`'s environment. Its type is read as quantified
///   over every variable in it, and [`by_name`](Self::by_name) hands each use a copy
///   with those variables replaced by fresh ones. That is what lets one declaration use
///   `Just` at `Maybe Int` and `Nothing` at `Maybe Char`, or `identity` at two types.
///
/// Reading every variable of a global as quantified is only right because each one was
/// translated from a declared type — an annotation or a union — on its own, so none of
/// its variables is shared with anything else in the environment. A binder shadows a
/// global of the same name.
struct Types {
    counter: u32,
    env: HashMap<String, Type>,
    globals: HashMap<String, Scheme>,
}

/// What the typer's environment holds for a global: a type, and the context its
/// constraints wrote in front of it.
///
/// Both are read as quantified over every variable in the type, and
/// [`Types::by_name`] instantiates them together — the same variable by the same fresh
/// one in the context as in the type — so that a use of `min : Comparable a => a -> a ->
/// a` at `Int` is one use of `a`. Each constraint of the context is on one of the type's
/// variables, since an annotation's constraint has to be: `canonical::Error::
/// ConstraintVariableNotInType`.
#[derive(Debug, Clone)]
struct Scheme {
    /// The constraints, in the order written, each a class and the variable of `tpe`
    /// it is required of.
    context: Vec<Predicate>,
    tpe: Type,
}

impl Scheme {
    /// A scheme with no constraint.
    fn unconstrained(tpe: Type) -> Scheme {
        Scheme {
            context: Vec::new(),
            tpe,
        }
    }
}

/// Names for type variables, for the messages that write a type the way an annotation
/// did.
type VariableNames = HashMap<TypeVariable, String>;

impl Types {
    fn new() -> Types {
        let counter = 10;
        let env = HashMap::new();
        let globals = HashMap::new();

        Types {
            counter,
            env,
            globals,
        }
    }

    /// Put the declarations of an outer scope in reach, as globals.
    fn extends_with(&mut self, global: HashMap<String, Scheme>) {
        self.globals.extend(global)
    }

    fn fresh_var(&mut self) -> Type {
        self.counter += 1;

        Type::Variable(TypeVariable::flexible(self.counter))
    }

    fn add_binder(&mut self, binding: TypeBinder) {
        self.env.insert(binding.name, binding.tpe);
    }

    fn remove_binder(&mut self, name: &str) {
        self.env.remove(name);
    }

    /// The type of one use of `name` — a binder's own type, or a fresh instance of a
    /// global's — and the constraints of that use: the global's context, instantiated
    /// with the same fresh variables as its type. A binder has none.
    ///
    /// Each constraint is an obligation of the use, which `constraint::collect` turns
    /// into one and `discharge` answers once unification is done.
    fn by_name(&mut self, name: &String) -> Option<(Type, Vec<Predicate>)> {
        if let Some(tpe) = self.env.get(name) {
            return Some((tpe.clone(), Vec::new()));
        }

        let scheme = self.globals.get(name)?.clone();
        let mut fresh = HashMap::new();
        let tpe = self.instantiate(scheme.tpe, &mut fresh);
        let context = scheme
            .context
            .into_iter()
            .map(|predicate| Predicate {
                class: predicate.class,
                tpe: self.instantiate(predicate.tpe, &mut fresh),
            })
            .collect();

        Some((tpe, context))
    }

    /// `tpe` with each of its variables replaced by a fresh one, the same variable by
    /// the same fresh one throughout.
    fn instantiate(&mut self, tpe: Type, fresh: &mut HashMap<TypeVariable, Type>) -> Type {
        match tpe {
            Type::Literal(_) | Type::Unit => tpe,
            Type::Variable(tvar) => {
                if let Some(replacement) = fresh.get(&tvar) {
                    return replacement.clone();
                }
                let replacement = self.fresh_var();
                fresh.insert(tvar, replacement.clone());
                replacement
            }
            Type::Fun {
                param_tpe,
                return_tpe,
            } => Type::Fun {
                param_tpe: Box::new(self.instantiate(*param_tpe, fresh)),
                return_tpe: Box::new(self.instantiate(*return_tpe, fresh)),
            },
            Type::Tuple(Tuple::Two(a, b)) => Type::Tuple(Tuple::two(
                self.instantiate(*a, fresh),
                self.instantiate(*b, fresh),
            )),
            Type::Tuple(Tuple::Three(a, b, c)) => Type::Tuple(Tuple::three(
                self.instantiate(*a, fresh),
                self.instantiate(*b, fresh),
                self.instantiate(*c, fresh),
            )),
            Type::Adt(name, args) => Type::Adt(
                name,
                args.into_iter()
                    .map(|arg| self.instantiate(arg, fresh))
                    .collect(),
            ),
            Type::Record(fields) => Type::Record(
                fields
                    .into_iter()
                    .map(|(label, tpe)| (label, self.instantiate(tpe, fresh)))
                    .collect(),
            ),
        }
    }
}

/// infer the type of the given term given known function defined in the outer scopes.
/// This is a translation of the algorithm demonstrated by
/// [Ionut Gan at I T.A.K.E Unconference 2015](https://www.youtube.com/watch?v=oPVTNxiMcSU)
pub fn infer(term: Term, global: HashMap<String, Type>) -> Result<Type, ErrorKind> {
    let global = global
        .into_iter()
        .map(|(name, tpe)| (name, Scheme::unconstrained(tpe)))
        .collect();

    infer_annotated(term, global, None, &classes::ClassTable::empty()).map(|(term, _)| term.tpe)
}

/// [`infer`], with the declaration's type annotation as a constraint of its own, and
/// answering with the whole solved term rather than only its type.
///
/// The annotation is put *first*, before the constraints the body generates, and that
/// ordering is the point of the function. `unify` solves constraints in order, so the
/// annotated type is substituted into the body's constraints before any of them are
/// solved; when one of them then fails, its [`Origin::explanation`] names the annotation,
/// and the diagnostic can say `Int` was expected *because of the annotation* rather
/// than merely that the declaration as a whole does not check.
///
/// The annotation's variables are rigid when it arrives (see [`TypeVariable`]), so `unify`
/// raises [`ErrorKind::RigidVariable`] where the body needs one of them to be a type of its
/// own, and a given of the annotation's context can only answer an obligation on the very
/// variable it is on.
///
/// The term handed back is zonked — see [`Substitution::apply_term`] — so every node
/// carries the type inference solved for it and not the variable `annotate` gave it.
/// Beside it comes the context the annotation required of its variables, each on the
/// rigid variable it was given on.
///
/// # The order of the steps
///
/// Equations first, then field constraints, then obligations. A field constraint can
/// solve the variable an instance is looked up by, so an obligation is read only once
/// both have run. Whether reading an instance's context can in turn decide a field
/// constraint does not arise: an obligation only reads the solution and adds nothing to
/// it.
fn infer_annotated(
    term: Term,
    global: HashMap<String, Scheme>,
    annotation: Option<Annotation>,
    table: &classes::ClassTable,
) -> Result<(TypedTerm, Vec<Predicate>), ErrorKind> {
    let mut env = Types::new();
    env.extends_with(global);

    let typed_term = annotate::annotate(term, &mut env)?;
    debug!("typed term: {:#?}", typed_term);

    let mut constraints = Vec::new();
    let annotated = annotation.is_some();
    let mut given = Vec::new();
    let mut names = Vec::new();
    let mut member_variables = Vec::new();
    let mut written = Written::Annotation;

    if let Some(annotation) = annotation {
        given = annotation.context;
        names = annotation.names;
        member_variables = annotation.member_variables;
        written = annotation.written;

        // Left is the annotation's type, because left is the type of the text the
        // span points at — see `Constraint`.
        constraints.push(Constraint::new(
            annotation.tpe,
            typed_term.tpe.clone(),
            annotation.reason,
            annotation.span,
        ));
    }

    let constraint::Constraints {
        equations,
        fields,
        holes,
        obligations,
    } = constraint::collect(&typed_term);
    constraints.extend(equations);
    debug!("Constraints: {:#?}", constraints);

    let substitution = unifier::unify(constraints)?;
    // Read only now, with every equation of the declaration solved: see
    // `FieldConstraint`.
    let declaration = unifier::Declaration {
        holes: &holes,
        declared: &typed_term.tpe,
        annotated,
    };
    let substitution = unifier::read_fields(substitution, fields, &declaration)?;

    // Read last, against the final substitution: see the `classes` module.
    classes::discharge(
        table,
        obligations,
        &substitution,
        &classes::Declared {
            tpe: &typed_term.tpe,
            annotated,
            given: &given,
            names: &names,
            member_variables: &member_variables,
            written,
        },
    )?;

    let context = given
        .iter()
        .map(|(class, variable)| Predicate {
            class: class.clone(),
            tpe: Type::Variable(variable.clone()),
        })
        .collect();

    Ok((substitution.apply_term(typed_term), context))
}

// TODO Once we have changed the Term to the zelkova primitives, rewrite the tests
// to use actual source code instead of AST. It's a pain to write them but it's even
// more of a pain to read them :)
// TODO Also write some assertions on the type instead of just printing XD
// TODO Import remaining tests. Plus the one for the modules above.
#[cfg(test)]
mod tests {
    use super::*;

    // These terms are written by hand rather than translated from source, so they
    // have no position — `Term::bare`. What they pin is inference, which does not
    // read spans; the tests that pin what a *diagnostic* points at go through real
    // source, in `crates/zelkova-compiler/tests/typer.rs`.
    fn int(i: i64) -> Term {
        Term::bare(TermKind::Int(i))
    }
    fn var(n: &str) -> Term {
        Term::bare(TermKind::Identifier(Reference::local(n)))
    }
    fn fun(arg: &str, body: Term) -> Term {
        Term::bare(TermKind::Fun {
            param: arg.to_owned(),
            body: Box::new(body),
        })
    }
    fn if_(cond: Term, true_branch: Term, false_branch: Term) -> Term {
        Term::bare(TermKind::If {
            cond: Box::new(cond),
            true_branch: Box::new(true_branch),
            false_branch: Box::new(false_branch),
        })
    }
    fn apply(fun: Term, arg: Term) -> Term {
        Term::bare(TermKind::Apply {
            fun: Box::new(fun),
            arg: Box::new(arg),
            saturation: Saturation::Partial,
        })
    }
    fn let_(binding: &str, value: Term, body: Term) -> Term {
        Term::bare(TermKind::Let {
            binding: binding.to_owned(),
            value: Box::new(value),
            body: Box::new(body),
        })
    }

    #[derive(Default)]
    struct Signature {
        counter: u8, // max 255 letters
        known: HashMap<u32, String>,
    }

    impl Signature {
        // Helper function to reduce boilerplate
        fn of_type(tpe: Type) -> String {
            let mut sig: Signature = Default::default();
            sig.type_signature(tpe)
        }

        fn type_signature(&mut self, tpe: Type) -> String {
            match tpe {
                Type::Literal(TypeLiteral::Int) => "Int".to_owned(),
                Type::Literal(TypeLiteral::Char) => "Char".to_owned(),
                Type::Literal(TypeLiteral::Float) => "Float".to_owned(),
                Type::Literal(TypeLiteral::String) => "String".to_owned(),
                Type::Unit => "()".to_owned(),
                Type::Variable(TypeVariable { id, .. }) => {
                    if let Some(name) = self.known.get(&id) {
                        name.clone()
                    } else {
                        let name = self.counter_as_letter();
                        self.counter += 1;

                        self.known.insert(id, name.clone());

                        name
                    }
                }
                Type::Fun {
                    param_tpe,
                    return_tpe,
                } => {
                    let is_param_fun = matches!(param_tpe.as_ref(), Type::Fun { .. });
                    let param = self.type_signature(*param_tpe);
                    let retur = self.type_signature(*return_tpe);

                    if is_param_fun {
                        format!("({}) -> {}", param, retur)
                    } else {
                        format!("{} -> {}", param, retur)
                    }
                }
                Type::Tuple(Tuple::Two(a, b)) => {
                    format!("({}, {})", self.type_signature(*a), self.type_signature(*b))
                }
                Type::Tuple(Tuple::Three(a, b, c)) => {
                    format!(
                        "({}, {}, {})",
                        self.type_signature(*a),
                        self.type_signature(*b),
                        self.type_signature(*c)
                    )
                }
                Type::Adt(name, args) if args.is_empty() => name.unqualified_name().to_string(),
                Type::Adt(name, args) => {
                    let arg_strs: Vec<String> =
                        args.into_iter().map(|a| self.type_signature(a)).collect();
                    format!("{} {}", name.unqualified_name(), arg_strs.join(" "))
                }
                Type::Record(fields) => {
                    let fields: Vec<String> = fields
                        .into_iter()
                        .map(|(label, tpe)| {
                            format!("{} : {}", label.as_str(), self.type_signature(tpe))
                        })
                        .collect();
                    format!("{{ {} }}", fields.join(", "))
                }
            }
        }

        fn counter_as_letter(&self) -> String {
            let m = self.counter % 26;
            let d = self.counter / 26;

            let m_char = (97 + m) as char; // 97 is 'a'
            let d_char = (96 + d) as char; // -1 because we start at 1

            if d > 0 {
                format!("{}{}", d_char, m_char)
            } else {
                format!("{}", m_char)
            }
        }
    }

    #[test]
    fn infer_identity_function() {
        let global = HashMap::new();
        let term = fun("a", var("a"));
        let infered = infer(term, global).unwrap();

        assert_eq!(Signature::of_type(infered), "a -> a".to_owned());
    }

    #[test]
    fn infer_const_function() {
        let global = HashMap::new();
        let term = fun("a", fun("b", var("a")));
        let infered = infer(term, global).unwrap();

        assert_eq!(Signature::of_type(infered), "a -> b -> a".to_owned());
    }

    #[test]
    fn infer_compose_function() {
        let global = HashMap::new();
        // \f -> \g -> \x -> f ( g x )
        let term = fun(
            "f",
            fun("g", fun("x", apply(var("f"), apply(var("g"), var("x"))))),
        );
        let infered = infer(term, global).unwrap();

        assert_eq!(
            Signature::of_type(infered),
            "(a -> b) -> (c -> a) -> c -> b".to_owned()
        );
    }

    #[test]
    fn infer_pred_function() {
        let global = HashMap::new();
        let term = fun("pred", if_(apply(var("pred"), int(1)), int(2), int(3)));
        let infered = infer(term, global).unwrap();

        // Integer literals infer as `Int`
        assert_eq!(
            Signature::of_type(infered),
            "(Int -> Bool) -> Int".to_owned()
        );
    }

    #[test]
    fn infer_increment_function() {
        let mut global = HashMap::new();
        // "+" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT)),
        global.insert(
            "+".to_owned(),
            Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(Type::Fun {
                    param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                    return_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                }),
            },
        );
        let term = let_(
            "inc",
            fun("a", apply(apply(var("+"), var("a")), int(1))),
            apply(var("inc"), int(42)),
        );
        let infered = infer(term, global).unwrap();

        assert_eq!(Signature::of_type(infered), "Int".to_owned());
    }

    #[test]
    fn infer_incdec_function() {
        let mut global = HashMap::new();
        global.insert(
            "+".to_owned(),
            Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(Type::Fun {
                    param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                    return_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                }),
            },
        );
        global.insert(
            "-".to_owned(),
            Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(Type::Fun {
                    param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                    return_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                }),
            },
        );
        let term = let_(
            "inc",
            fun("a", apply(apply(var("+"), var("a")), int(1))),
            let_(
                "dec",
                fun("a", apply(apply(var("-"), var("a")), int(1))),
                apply(var("dec"), apply(var("inc"), int(42))),
            ),
        );
        let infered = infer(term, global).unwrap();

        assert_eq!(Signature::of_type(infered), "Int".to_owned());
    }

    #[test]
    fn infer_cannot_possible() {
        let mut global = HashMap::new();
        global.insert(
            "+".to_owned(),
            Type::Fun {
                param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                return_tpe: Box::new(Type::Fun {
                    param_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                    return_tpe: Box::new(Type::Literal(TypeLiteral::Int)),
                }),
            },
        );
        global.insert("True".to_owned(), bool_type());
        let term = apply(apply(var("+"), var("True")), int(1));
        assert!(infer(term, global).is_err());
    }

    // --- A constructor pattern finds only this module's own unions ------------

    /// The qualified name of `name` as declared by `module` of the package `main`.
    fn qual(module: &str, name: &str) -> QualName {
        QualName::in_module(crate::PackageName::new("main").unwrap(), module, name)
    }

    /// `Main`'s own unions, as `translate_pattern` receives them: one nullary
    /// `Main.Size`.
    fn main_size() -> (QualName, canonical::UnionType) {
        (
            qual("Main", "Size"),
            canonical::UnionType {
                span: NodeSpan::none(),
                variables: vec![],
                variants: vec![canonical::TypeConstructor {
                    name: "Big".into(),
                    type_parameters: vec![],
                    tpe: qual("Main", "Size"),
                }],
            },
        )
    }

    /// A nullary constructor pattern for `name`, building the union `tpe`.
    fn constructor_pattern(name: &str, tpe: QualName) -> canonical::Pattern {
        canonical::Pattern {
            span: NodeSpan::none(),
            kind: canonical::PatternKind::Constructor {
                ctor: canonical::TypeConstructor {
                    name: name.into(),
                    type_parameters: vec![],
                    tpe,
                },
                args: vec![],
            },
        }
    }

    /// The four things `canonical::ExpressionKind` calls a name stay four things in the
    /// IR.
    ///
    /// They used to be one: every one of them became `TermKind::Identifier(String)`, and
    /// the string is bare for a local and qualified for the other three, so nothing
    /// downstream could tell a parameter from an import from a constructor
    /// ([`DEC-18` decision
    /// 1](../../docs/decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)).
    ///
    /// Written against hand-built canonical expressions rather than against source, so
    /// that all four are read off one translation with no other module needing to be
    /// checked first.
    ///
    /// Mutation-checked by giving the `VarForeign` and `VarTopLevel` arms of
    /// `canonical_expr_to_term` the same `ReferenceKind`, which is the collapse this
    /// pins: the two assertions naming them then read the same value.
    #[test]
    fn the_four_kinds_of_name_stay_apart() {
        let (union_name, union) = main_size();
        let translation = Translation::of_types(HashMap::from([(union_name.clone(), &union)]));
        let lib = crate::PackageName::new("lib").unwrap();
        let lib_size = QualName::in_module(lib.clone(), "Lib", "size");

        let reference = |kind: canonical::ExpressionKind| {
            let mut counter = 0;
            let expression = canonical::Expression::bare(kind);

            match canonical_expr_to_term(&expression, &translation, &mut counter) {
                Some(Term {
                    kind: TermKind::Identifier(reference),
                    ..
                }) => reference,
                other => panic!("expected a name, got {:?}", other),
            }
        };

        assert_eq!(
            reference(canonical::ExpressionKind::VarLocal("size".into())).kind,
            ReferenceKind::Local
        );
        assert_eq!(
            reference(canonical::ExpressionKind::VarTopLevel(qual("Main", "size"))).kind,
            ReferenceKind::TopLevel(qual("Main", "size"))
        );
        assert_eq!(
            reference(canonical::ExpressionKind::VarForeign(
                lib_size.clone(),
                lib.clone(),
                canonical::Type::Variable("a".into())
            ))
            .kind,
            ReferenceKind::Foreign(lib_size, lib, 0)
        );
        assert_eq!(
            reference(canonical::ExpressionKind::VarConstructor(
                qual("Main", "Big"),
                canonical::Type::Type(union_name.clone(), vec![])
            ))
            .kind,
            ReferenceKind::Constructor(Constructor {
                union: union_name,
                name: "Big".into(),
                index: 0,
                arity: 0,
            })
        );
    }

    /// `A.S` builds `A.Size`, and a `Main` that happens to declare its own `Size`
    /// is not where that union is found.
    ///
    /// `BUG-35`: the lookup narrowed the constructor's type to its unqualified half,
    /// so `A.S` found `Main.Size` and the pattern was translated at the local type.
    /// With no `A` in the translation, the lookup finds nothing at all.
    ///
    /// Mutation-checked by restoring the narrowed lookup
    /// (`unions` keyed by `Name`, `get(&ctor.tpe.unqualified_name())`): the
    /// pattern is then translated and the assertion goes red.
    #[test]
    fn an_imported_constructor_does_not_find_a_local_type_of_the_same_name() {
        let (name, union) = main_size();
        let translation = Translation::of_types(HashMap::from([(name, &union)]));
        let mut counter = 0;

        let pattern = constructor_pattern("S", qual("A", "Size"));

        assert!(
            translate_pattern(&pattern, &translation, &mut counter).is_none(),
            "`A.S` is not a constructor of `Main.Size`"
        );
    }

    /// The other half: `Main`'s own constructor still finds `Main`'s own union, so
    /// the test above is not passing because the lookup stopped finding anything.
    #[test]
    fn a_local_constructor_finds_its_own_type() {
        let (name, union) = main_size();
        let translation = Translation::of_types(HashMap::from([(name, &union)]));
        let mut counter = 0;

        let pattern = constructor_pattern("Big", qual("Main", "Size"));

        let translated = translate_pattern(&pattern, &translation, &mut counter)
            .expect("`Main.Big` is a constructor of `Main.Size`");

        match translated.kind {
            TermPatternKind::Constructor { ctor, .. } => {
                assert_eq!(ctor.union, qual("Main", "Size"));
            }
            other => panic!("expected a constructor pattern, got {:?}", other),
        }
    }

    // --- Display for Type ---------------------------------------------------

    fn int_t() -> Type {
        Type::Literal(TypeLiteral::Int)
    }
    fn bool_t() -> Type {
        bool_type()
    }
    fn char_t() -> Type {
        Type::Literal(TypeLiteral::Char)
    }
    fn fun_t(param: Type, ret: Type) -> Type {
        Type::Fun {
            param_tpe: Box::new(param),
            return_tpe: Box::new(ret),
        }
    }
    /// A union declared in a module called `Widget`, which is never what `Display`
    /// writes — the point of the tests below is that it writes the bare `name`.
    fn adt(name: &str, args: Vec<Type>) -> Type {
        Type::Adt(qual("Widget", name), args)
    }

    /// `Display for Type` is the text diagnostics quote back to the user, so its two
    /// parenthesisation rules and its inference-variable spelling are user-visible
    /// output. Nothing else pins them: the pipeline test that renders a type mismatch
    /// only ever reaches the `Literal` arms. Dropping a parenthesis here would print
    /// `Maybe Maybe Int`, which reads as a different type, with the suite still green.
    #[test]
    fn display_writes_types_the_way_the_source_spells_them() {
        let cases: Vec<(Type, &str)> = vec![
            // A function in *parameter* position is parenthesised, because `->` is
            // right-associative: `(a -> b) -> c` and `a -> b -> c` are different types.
            (
                fun_t(fun_t(int_t(), bool_t()), int_t()),
                "(Int -> Bool) -> Int",
            ),
            // In *return* position it is not, for the same reason: the chain already
            // re-reads as itself.
            (
                fun_t(int_t(), fun_t(bool_t(), char_t())),
                "Int -> Bool -> Char",
            ),
            // An applied `Adt` nested inside another needs parens to survive a re-read.
            (
                adt("Maybe", vec![adt("Maybe", vec![int_t()])]),
                "Maybe (Maybe Int)",
            ),
            // So does a function used as an `Adt` argument.
            (
                adt("Maybe", vec![fun_t(int_t(), bool_t())]),
                "Maybe (Int -> Bool)",
            ),
            // A *nullary* `Adt` argument does not: there is nothing to mis-group.
            (adt("List", vec![adt("Never", vec![])]), "List Never"),
            // And an applied `Adt` in parameter position does not either — application
            // binds tighter than `->`.
            (
                fun_t(adt("Maybe", vec![int_t()]), bool_t()),
                "Maybe Int -> Bool",
            ),
            (Type::Tuple(Tuple::two(int_t(), bool_t())), "( Int, Bool )"),
            (
                Type::Tuple(Tuple::three(int_t(), bool_t(), char_t())),
                "( Int, Bool, Char )",
            ),
            // Inference variables have no source syntax; Elm spells them `t{n}`.
            (Type::Variable(TypeVariable::flexible(7)), "t7"),
        ];

        for (tpe, expected) in cases {
            assert_eq!(format!("{}", tpe), expected, "rendering {:?}", tpe);
        }
    }

    /// `ErrorKind::CircularType` names one type, but the solution a variable would
    /// have had to contain itself in is built out of everything it was unified
    /// against — so one type is enough to hold two same-named declarations.
    ///
    /// Written by hand because no Zelkova source available today produces a circular
    /// type at all: the constructs that do (`let`, lambdas) are unimplemented, and
    /// `unifier.rs` is the only thing that raises the variant.
    ///
    /// Mutation-checked by quoting `tpe` through `Display` unconditionally, the way
    /// the arm was first written, which reports *contain itself in `Box Size Size`*.
    #[test]
    fn a_circular_type_naming_two_modules_alike_qualifies_both() {
        let tpe = Type::Adt(
            qual("Lib", "Box"),
            vec![
                Type::Adt(qual("A", "Size"), vec![]),
                Type::Adt(qual("B", "Size"), vec![]),
            ],
        );

        let kind = ErrorKind::CircularType {
            tpe: Box::new(tpe),
            origin: Box::new(Origin::new(Reason::Annotation, NodeSpan::none())),
        };

        assert_eq!(
            kind.message(&Spellings::default()),
            "circular type: a type variable would have to contain itself in \
             `Lib.Box A.Size B.Size`"
        );
    }

    /// The counterpart: one declaration per spelling, so the type is quoted the way
    /// the source writes it.
    #[test]
    fn a_circular_type_with_no_collision_stays_unqualified() {
        let kind = ErrorKind::CircularType {
            tpe: Box::new(adt("Box", vec![adt("Size", vec![])])),
            origin: Box::new(Origin::new(Reason::Annotation, NodeSpan::none())),
        };

        assert_eq!(
            kind.message(&Spellings::default()),
            "circular type: a type variable would have to contain itself in `Box Size`"
        );
    }
}
