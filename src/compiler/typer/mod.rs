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
//! - http://steshaw.org/hm/hindley-milner.pdf
//! - https://pdfs.semanticscholar.org/8983/233b3dff2c5b94efb31235f62bddc22dc899.pdf
//! - http://gallium.inria.fr/~fpottier/publis/fpottier-elaboration.pdf
//! - http://gallium.inria.fr/~fpottier/publis/emlti-final.pdf
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
use crate::compiler::ir::{
    pattern_parameter, CaseForm, Constructor, Reference, ReferenceKind, Saturation, Solved, Term,
    TermKind, TermPattern, TermPatternKind, TypeBinder, TypedTerm, TypedTermKind,
};
use crate::compiler::name::{Name, QualName};
use crate::compiler::position::NodeSpan;
use crate::compiler::tuple::Tuple;
use crate::compiler::{Interface, PhaseError, SpanLabel};
use log::debug;
use std::collections::HashMap;

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
    /// A `let` binding has the type of the value bound to it.
    LetBinding,
    /// A `let` has the type of its body.
    LetBody,
    /// A tuple's type is the tuple of its elements' types.
    TupleElements,
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
    /// `answer : Int` with body `true` fails on the literal's own constraint *after*
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
            Reason::LetBinding => "the value bound here",
            Reason::LetBody => "the body of this `let`",
            Reason::TupleElements => "this tuple",
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
            Reason::LetBinding => "expected because of this value",
            Reason::LetBody => "expected because of this `let` body",
            Reason::TupleElements => "expected because of this tuple",
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
    /// A type variable would have to occur inside its own solution.
    CircularType {
        /// The type the variable would have had to contain itself in. Boxed for the
        /// reason above.
        tpe: Box<Type>,
        origin: Box<Origin>,
    },
    /// A name the typer's environment does not know. `type_check` turns this into a
    /// [`Solved::UnboundName`] rather than an [`Error`]; that variant says why.
    UnboundVariable {
        name: String,
        /// Where the name was written.
        span: NodeSpan,
    },
}

impl ErrorKind {
    /// The provenance of the constraint that failed, when the failure came from
    /// unification at all.
    fn origin(&self) -> Option<&Origin> {
        match self {
            ErrorKind::UnificationFailed { origin, .. }
            | ErrorKind::CircularType { origin, .. } => Some(origin.as_ref()),
            ErrorKind::UnboundVariable { .. } => None,
        }
    }

    fn message(&self) -> String {
        match self {
            ErrorKind::UnificationFailed { left, right, .. } => {
                // Two modules may each declare a union of the same name, and the
                // source spells both of them bare. Written that way the sentence
                // uses one word for two declarations and says nothing; the module
                // that declared each is what tells them apart, so both sides take
                // it — both, because a sentence that qualifies one side and not the
                // other reads as if only one of them had a module.
                if AdtNames::for_all([left.as_ref(), right.as_ref()]) == AdtNames::Qualified {
                    format!(
                        "cannot match `{}` with `{}`",
                        Qualified(left),
                        Qualified(right)
                    )
                } else {
                    format!("cannot match `{}` with `{}`", left, right)
                }
            }
            // One type, but it can hold the collision on its own: the variable's
            // solution is built out of whatever it was unified against, which may
            // be two same-named unions from two modules.
            ErrorKind::CircularType { tpe, .. } => {
                let tpe: &dyn std::fmt::Display = match AdtNames::for_all([tpe.as_ref()]) {
                    AdtNames::Qualified => &Qualified(tpe),
                    AdtNames::Unqualified => &**tpe,
                };

                format!(
                    "circular type: a type variable would have to contain itself in `{}`",
                    tpe
                )
            }
            ErrorKind::UnboundVariable { name, .. } => {
                format!("cannot find a value named `{}`", name)
            }
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
}

/// Type errors are about types, and [`Type`]'s `Display` writes them the way the
/// source does — so the message can name both sides of a mismatch instead of
/// dumping the typer's internal representation.
impl PhaseError for Error {
    fn message(&self) -> String {
        self.kind.message()
    }

    fn notes(&self) -> Vec<String> {
        let mut notes = vec![format!("in the declaration of `{}`", self.declaration)];

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

/// Type check one canonical module and hand back what it solved, reporting every value
/// whose inference produced a reportable error rather than stopping at the first — the
/// shape `compile_package` is built to accumulate.
///
/// # What comes back
///
/// One [`Solved`] per declaration, keyed the way `module.values` is. A declaration the
/// typer could not type is present and says so; none is ever merely absent, because
/// absent is indistinguishable from checked-and-fine to whatever reads this next
/// ([`DEC-18` decision 1](../../../docs/decisions/dec-18.md)). The map is only returned
/// at all when no declaration failed: a module with type errors answers with them, so
/// there is no half-checked module to interpret.
///
/// The term inside [`Solved::Typed`] is the one `annotate` built, with `unify`'s final
/// substitution applied to every node rather than to the declaration's own type alone.
/// That interior is the point — a backend needs the type of each sub-expression, not
/// just of the declaration containing it.
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
/// `Maybe.withDefault` at two types — see [`Types`].
pub fn type_check(
    module: &Module,
    interfaces: &HashMap<Name, Interface>,
) -> Result<HashMap<Name, Solved>, Vec<Error>> {
    // A `module foreign` facade uses synthetic placeholder bodies, so there is nothing
    // to infer — but every declaration still has to be accounted for, so each is
    // returned saying why it has no term.
    if module.binding_foreign {
        return Ok(module
            .values
            .keys()
            .map(|name| (name.clone(), Solved::NoBody))
            .collect());
    }

    // Start at a high offset to avoid collisions with the counter inside
    // Types::new() (which starts at 10) used during inference.
    let mut counter = 10_000u32;

    // First pass: build global env from every declared type in reach — the values
    // each imported interface exposes, then this module's own annotated values.
    let mut global: HashMap<String, Type> = HashMap::new();

    // An imported value is keyed the way a `VarForeign` reference spells it: its name
    // qualified by the module that declared it. That is the only key, so it cannot
    // collide with a local name or with a same-named value of another module. An
    // operator's backing function the header did not expose by name is in
    // `infix_functions` rather than `values`, and an operator resolves to a
    // `VarForeign` naming it all the same.
    for interface in interfaces.values() {
        for (name, (_, tpe)) in interface.values.iter().chain(&interface.infix_functions) {
            let mut var_map = HashMap::new();
            if let Some(typer_tpe) = canonical_type_to_typer_type(tpe, &mut var_map, &mut counter) {
                let qname = interface.module_name.qualify_name(name).to_name();
                global.insert(qname.as_str().to_string(), typer_tpe);
            }
        }
    }

    for (name, value) in &module.values {
        if let canonical::Value::TypedValue { tpe, .. } = value {
            let mut var_map = HashMap::new();
            if let Some(typer_tpe) = canonical_type_to_typer_type(tpe, &mut var_map, &mut counter) {
                // Add both qualified (e.g. "Test.not") and unqualified (e.g. "not") names
                let qname = module
                    .name
                    .qualify_name(name)
                    .to_name()
                    .as_str()
                    .to_string();
                global.insert(qname, typer_tpe.clone());
                global.insert(name.as_str().to_string(), typer_tpe);
            }
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
            adt_var_map.insert(tv_name.as_str().to_string(), TypeVariable { id: counter });
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
            // "Maybe.Just", named by the module that declared the union — and, for
            // this module's own unions only, under the bare name too.
            let Some(qname) = ctor.name.qualify_with_name(&type_name.module_name()) else {
                continue;
            };
            if module.name.qualify_name(&type_name.unqualified_name()) == *type_name {
                global.insert(ctor.name.as_str().to_string(), ctor_type.clone());
            }
            global.insert(qname.to_name().as_str().to_string(), ctor_type);
        }
    }

    // Third pass: check each value. A value that fails is recorded and the pass
    // moves on, so one broken declaration cannot hide the others.
    let mut errors: Vec<Error> = vec![];
    let mut solved: HashMap<Name, Solved> = HashMap::new();
    for (name, value) in &module.values {
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
        match infer_annotated(term, global.clone(), annotation) {
            // An unbound variable here is a hole in the typer's environment, not a
            // mistake in the source — see [`Solved::UnboundName`].
            Err(ErrorKind::UnboundVariable {
                name: unbound,
                span,
            }) => {
                solved.insert(
                    name.clone(),
                    Solved::UnboundName {
                        name: unbound,
                        span,
                    },
                );
            }
            Err(kind) => errors.push(Error {
                kind,
                span: value.span(),
                declaration: name.clone(),
            }),
            Ok(term) => {
                solved.insert(name.clone(), Solved::Typed(term));
            }
        }
    }

    if errors.is_empty() {
        Ok(solved)
    } else {
        Err(errors)
    }
}

// ── Translation helpers ───────────────────────────────────────────────────────

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
/// Each of the three answers a question the canonical node cannot: which union a
/// constructor belongs to and where in it, and how many arguments a call has to supply
/// before it is a direct call. All three are facts of the module and of what it
/// imports, which is why they are gathered once here — the declaration being translated
/// is the only thing that changes between calls.
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
}

impl<'a> Translation<'a> {
    /// The translation for `module`, checked against `interfaces`.
    ///
    /// The interfaces' unions go in first and this module's own after them, so that if
    /// the map somehow held an interface of the module under check, the declarations
    /// in front of the typer are the ones that win.
    fn of(module: &'a Module, interfaces: &'a HashMap<Name, Interface>) -> Translation<'a> {
        let mut unions: Unions<'a> = HashMap::new();

        for interface in interfaces.values() {
            for (name, union_type) in &interface.unions {
                unions.insert(interface.module_name.qualify_name(name), union_type);
            }
        }

        for (name, union_type) in &module.types {
            unions.insert(module.name.qualify_name(name), union_type);
        }

        let constructors = constructors_of(&unions);

        let arities = module
            .values
            .iter()
            .map(|(name, value)| (name.clone(), value.arity()))
            .collect();

        Translation {
            unions,
            constructors,
            arities,
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
        }
    }

    /// How many arguments the callee of an application spine takes, when this module
    /// knows.
    ///
    /// `None` is what makes an application [`Saturation::Partial`], and it is the honest
    /// answer three times over: a local is a value rather than a declaration and has no
    /// arity at all; an imported value's arity belongs to the module that declared it
    /// and is not in the canonical AST here; and a callee that is itself an expression —
    /// the result of a `case`, say — is a value too. A backend that cannot prove a call
    /// saturated goes through `$curry`, which is correct for every one of them.
    fn callee_arity(&self, callee: &canonical::Expression) -> Option<usize> {
        match &callee.kind {
            canonical::ExpressionKind::VarTopLevel(qname) => {
                self.arities.get(&qname.unqualified_name()).copied()
            }
            canonical::ExpressionKind::VarConstructor(qname, _) => {
                self.constructors.get(qname).map(|ctor| ctor.arity)
            }
            _ => None,
        }
    }
}

/// Every constructor of `unions`, keyed the way a `VarConstructor` spells one: the
/// constructor's name qualified by the module that declared the union it builds.
fn constructors_of(unions: &Unions) -> HashMap<QualName, Constructor> {
    let mut constructors = HashMap::new();

    for (union, union_type) in unions {
        for variant in crate::compiler::ir::variants_of(union_type) {
            // A union is named by its declaring module, so that module is where its
            // constructors are named from too. `qualify_with_name` only declines an
            // empty name, which a parsed declaration never has.
            let Some(name) = variant.name.qualify_with_name(&union.module_name()) else {
                continue;
            };

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
/// The match covers all four `canonical::Type` variants — `Variable`, `Arrow`, `Tuple`
/// (either arity), and `Type` including named types with parameters — and every arm's
/// own base case returns `Some`; a `None` only ever arises by propagating up from a
/// nested recursive call. As of today no `canonical::Type` shape actually reaches such
/// a case, so the function always returns `Some`. The `Option` return stays in place for
/// when a genuinely unrepresentable variant (e.g. records, aliases) is added.
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
                TypeVariable { id: *counter }
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

/// The literal type the typer gives a [scalar](super::scalars), if `name` is the
/// qualified name of one it has a literal type for.
///
/// Three of the five appear here. The other two do not:
///
/// - [`scalars::BOOL`] is a scalar *and* an ordinary union, so `Bool` in an annotation
///   takes the [`Type::Adt`] path every other declaration takes and meets `True` and
///   `False` there. [`bool_type`] is the same type, built for the three `Bool`s no
///   source spells.
/// - [`scalars::STRING`] would gain a [`TypeLiteral`] variant the day there is a string
///   literal to give a type to; a variant nothing constructs would be a type the
///   unifier could name in an error and no source could produce.
fn scalar_literal(name: &QualName) -> Option<TypeLiteral> {
    const LITERALS: &[(scalars::Scalar, TypeLiteral)] = &[
        (scalars::INT, TypeLiteral::Int),
        (scalars::FLOAT, TypeLiteral::Float),
        (scalars::CHAR, TypeLiteral::Char),
    ];

    LITERALS
        .iter()
        .find(|(scalar, _)| scalar.declares(name))
        .map(|(_, literal)| literal.clone())
}

/// The type of a `Bool`: the union [`scalars::BOOL`] names, with no arguments.
///
/// `Bool` is [a scalar and an ordinary union at
/// once](../../../docs/spec/types.md#scalar-types) — the compiler knows its
/// representation and nothing about its structure — so this is the very type
/// `canonical_type_to_typer_type` produces for an annotation naming `Basics.Bool`, and
/// the type `Basics` registers `True` and `False` at.
///
/// Three constructs need a `Bool` the source did not spell: an [`if`
/// condition](../../../docs/spec/expressions.md#if--then--else), the `true`/`false`
/// keywords, and a `true`/`false` pattern. They name `Basics.Bool` and nothing else, so
/// a module declaring its own `type Bool` does not satisfy them
/// ([`DEC-15`](../../../docs/decisions/dec-15.md) decisions 1 and 5).
pub(super) fn bool_type() -> Type {
    Type::Adt(scalars::BOOL.qual_name(), vec![])
}

/// Convert a canonical expression to a Term, keeping the position it was written at.
///
/// Returns None for constructs the inference engine doesn't yet handle (a `VarKernel`
/// reference, complex patterns inside a `Case`), and for a constructor of a union
/// neither this module nor an interface in [`Translation`] declares.
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
/// 1](../../../docs/decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)).
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
        // bits](../../../docs/spec/evaluation-semantics.md#numbers), and a term is what
        // code is generated from, so narrowing here would emit a different number than
        // the one that was written.
        canonical::ExpressionKind::Int(i) => TermKind::Int(*i),
        canonical::ExpressionKind::Bool(b) => TermKind::Bool(*b),
        canonical::ExpressionKind::Char(c) => TermKind::Char(*c),
        canonical::ExpressionKind::Float(f) => TermKind::Float(*f),
        canonical::ExpressionKind::VarLocal(name) => {
            TermKind::Identifier(Reference::local(name.as_str()))
        }
        canonical::ExpressionKind::VarTopLevel(qname) => TermKind::Identifier(Reference {
            name: qname.to_name().as_str().to_string(),
            kind: ReferenceKind::TopLevel(qname.clone()),
        }),
        // A value another module declares. Its type is the one its module's interface
        // declared, which `type_check` registers under this same qualified name.
        canonical::ExpressionKind::VarForeign(qname, _) => TermKind::Identifier(Reference {
            name: qname.to_name().as_str().to_string(),
            kind: ReferenceKind::Foreign(qname.clone()),
        }),
        // A constructor builds a tagged value rather than reading a binding, so it
        // carries its place in its declaration. That place comes from the union,
        // which is in `translation.constructors` whether this module declared it or
        // an imported interface did. The type the canonical node carries is not read:
        // the one `type_check` registers is built from the union's own variables.
        canonical::ExpressionKind::VarConstructor(qname, _) => {
            let ctor = translation.constructors.get(qname)?;

            TermKind::Identifier(Reference {
                name: qname.to_name().as_str().to_string(),
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
        canonical::ExpressionKind::Case(scrutinee_expr, branches) => {
            let scrutinee = canonical_expr_to_term(scrutinee_expr, translation, counter)?;
            let term_branches: Vec<(TermPattern, Box<Term>)> = branches
                .iter()
                .map(|cb| {
                    let (pattern, _bindings) =
                        translate_pattern(&cb.pattern, translation, counter)?;
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
        // Not yet supported: VarKernel
        _ => return None,
    };

    Some(Term {
        span: expr.span,
        kind,
    })
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

/// Translate a canonical pattern into a `TermPattern` plus any variable bindings
/// introduced by the pattern.  Returns `None` for unsupported pattern shapes.
///
/// The pattern keeps its own span, separate from the branch body's: a `case` branch
/// whose pattern does not match what is being matched on is about the pattern, and
/// the caret belongs there rather than under the expression in the `case … of` line.
fn translate_pattern(
    pattern: &canonical::Pattern,
    translation: &Translation,
    counter: &mut u32,
) -> Option<(TermPattern, Vec<(String, Type)>)> {
    let (kind, bindings) = match &pattern.kind {
        canonical::PatternKind::Anything => (TermPatternKind::Anything, vec![]),
        canonical::PatternKind::Variable(name) => {
            // The binding's actual type will be unified with the scrutinee type in annotate.
            (TermPatternKind::Bind(name.as_str().to_string()), vec![])
        }
        canonical::PatternKind::Bool(_) => (TermPatternKind::Literal(bool_type()), vec![]),
        canonical::PatternKind::Int(_) => (
            TermPatternKind::Literal(Type::Literal(TypeLiteral::Int)),
            vec![],
        ),
        canonical::PatternKind::Char(_) => (
            TermPatternKind::Literal(Type::Literal(TypeLiteral::Char)),
            vec![],
        ),
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
                adt_var_map.insert(tv_name.as_str().to_string(), TypeVariable { id: *counter });
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

            // Build bindings from arg patterns.
            let mut bindings: Vec<(String, Type)> = vec![];
            for (arg_pattern, param_type) in args.iter().zip(param_types.iter()) {
                match &arg_pattern.kind {
                    canonical::PatternKind::Variable(name) => {
                        bindings.push((name.as_str().to_string(), param_type.clone()));
                    }
                    canonical::PatternKind::Anything => {} // no binding needed
                    _ => return None, // nested complex patterns not yet supported
                }
            }

            let kind = TermPatternKind::Constructor {
                ctor: Constructor {
                    union: ctor.tpe.clone(),
                    name: ctor.name.clone(),
                    index,
                    arity: ctor.type_parameters.len(),
                },
                adt_args,
                bindings: bindings.clone(),
            };
            (kind, bindings)
        }
        // Each element gets a fresh type, and the matched value has to be the tuple of
        // them. An element is held to the same limit a constructor's argument is: a
        // variable or `_`, nothing nested.
        canonical::PatternKind::Tuple(elements) => {
            let mut bindings: Vec<(String, Type)> = vec![];
            let elements = elements
                .try_map(|element| {
                    *counter += 1;
                    let tpe = Type::Variable(TypeVariable { id: *counter });
                    match &element.kind {
                        canonical::PatternKind::Variable(name) => {
                            bindings.push((name.as_str().to_string(), tpe.clone()));
                        }
                        canonical::PatternKind::Anything => {}
                        _ => return Err(()),
                    }
                    Ok(tpe)
                })
                .ok()?;

            let kind = TermPatternKind::Tuple {
                elements,
                bindings: bindings.clone(),
            };
            (kind, bindings)
        }
        _ => return None, // Float patterns — not yet supported
    };

    Some((
        TermPattern {
            span: pattern.span,
            kind,
        },
        bindings,
    ))
}

/// A declaration's type annotation, and where it was written.
///
/// The span is the annotation's alone — `answer : Int`, not the declaration it
/// heads — because it is drawn as the secondary label of a mismatch in the body, and
/// a span covering the body too would underline the thing it is meant to contrast
/// with.
struct Annotation {
    tpe: Type,
    span: NodeSpan,
}

/// Convert a canonical Value into a (Term, optional annotation) pair.
/// The body is wrapped in nested Fun nodes for each parameter — see
/// [`wrap_with_patterns`].
/// Returns None if any part of the value cannot be translated.
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
            annotation_span,
            ..
        } => {
            let body_term = canonical_expr_to_term(body, translation, counter)?;
            let pattern_iter = patterns.iter().map(|(p, _)| p);
            let term = wrap_with_patterns(pattern_iter, body_term, translation, counter)?;
            let mut var_map = HashMap::new();
            let annotation =
                canonical_type_to_typer_type(tpe, &mut var_map, counter).map(|tpe| Annotation {
                    tpe,
                    span: *annotation_span,
                });
            Some((term, annotation))
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
/// written as a variable, whichever comes first. Both are a name bound twice in one
/// clause, which the language rejects and the compiler does not yet (`LANG-18`).
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
                let (translated, _) = translate_pattern(pattern, translation, counter)?;
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

// The term language inference runs on is [`crate::compiler::ir`], and it is no longer
// only inference's. Every node carries the [`NodeSpan`] of the canonical node it was
// built from, so an error found down here can say where in the user's source it
// happened (`ERR-4`), and each carries what a backend reads off it — the kind of name a
// reference is, a call's saturation, a constructor's place in its declaration — because
// the translation below is the only place those are known. Inference reads none of
// them.

mod annotate;
mod constraint;
mod unifier;

// TODO Copy ?
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct TypeVariable {
    id: u32,
}

impl std::fmt::Debug for TypeVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeVariable#{}", self.id)
    }
}

/// The type of an [opaque scalar](../../../docs/spec/types.md#scalar-types): a type
/// nothing in the language builds or inspects, whose values arrive as literals.
///
/// Three of the four are here — `String` waits on a string literal to give a type to.
/// `Bool` is not one of them at all: it is a scalar *and* an ordinary union, so its
/// type is a [`Type::Adt`] like any other union's, built by this module's `bool_type`.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeLiteral {
    Int,
    Char,
    Float,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Literal(TypeLiteral),
    /// A numeric literal type: unifies with both `Int` and `Float` but not other types.
    /// This models Elm's `number` constraint for integer literals used in numeric contexts.
    Number,
    Variable(TypeVariable),
    Fun {
        param_tpe: Box<Type>,
        return_tpe: Box<Type>,
    },
    Tuple(Tuple<Type>),
    /// A named algebraic data type, e.g. `Maybe Int` declared in `Maybe` →
    /// `Adt(Maybe.Maybe, [Literal(Int)])`.
    ///
    /// The name is the declaring module's, in full, because that is the identity of
    /// the type: `Widget.Size` and `Gadget.Size` are two types and a value of one is
    /// never a value of the other. The unifier's equality on this name is the only
    /// thing keeping them apart, so narrowing it to the spelling — which is what the
    /// typer used to carry — made every same-named declaration one type (`BUG-35`).
    /// [`Display`](std::fmt::Display) still writes the unqualified half, since that is
    /// how a module's source spells its own types.
    Adt(QualName, Vec<Type>),
}

/// How the name of a [`Type::Adt`] is written out.
///
/// A type is normally quoted the way the source spells it, which for a union is its
/// bare name. That is ambiguous exactly when one message names two declarations that
/// share a spelling, and [`ErrorKind::message`] switches to the qualified form there.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AdtNames {
    /// `Size`.
    Unqualified,
    /// `Widget.Size`.
    Qualified,
}

impl AdtNames {
    /// How the types a single message is about have to be written for that message
    /// to distinguish the declarations it names.
    ///
    /// The question is not whether the *types* are spelled alike — `A.Size` against
    /// `Lib.Size A.Size` renders as `Size` against `Size Size`, which differs as
    /// text while still using one word for three declarations. It is whether any two
    /// of the unions named anywhere in those types are different declarations with
    /// the same bare name; if so every union in the sentence is qualified, since
    /// qualifying only the colliding pair would read as if the rest had no module.
    fn for_all<'a>(types: impl IntoIterator<Item = &'a Type>) -> AdtNames {
        let mut names: Vec<&QualName> = Vec::new();

        for tpe in types {
            tpe.collect_adt_names(&mut names);
        }

        let collides = names.iter().enumerate().any(|(i, name)| {
            names[i + 1..]
                .iter()
                .any(|other| *other != *name && other.unqualified_name() == name.unqualified_name())
        });

        if collides {
            AdtNames::Qualified
        } else {
            AdtNames::Unqualified
        }
    }
}

/// A [`Type`] written with every union named by the module that declared it.
///
/// The counterpart of `Type`'s own [`Display`](std::fmt::Display), which writes the
/// unqualified half.
struct Qualified<'a>(&'a Type);

impl std::fmt::Display for Qualified<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.write(f, AdtNames::Qualified)
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Literal(lit) => write!(f, "Lit({:?})", lit),
            Type::Number => write!(f, "Number"),
            Type::Variable(TypeVariable { id }) => write!(f, "Var(#{})", id),
            Type::Fun {
                param_tpe,
                return_tpe,
            } => write!(f, "Fun({:?} -> {:?})", param_tpe, return_tpe),
            Type::Tuple(Tuple::Two(a, b)) => write!(f, "({:?}, {:?})", a, b),
            Type::Tuple(Tuple::Three(a, b, c)) => write!(f, "({:?}, {:?}, {:?})", a, b, c),
            Type::Adt(name, args) if args.is_empty() => write!(f, "{}", name.to_name()),
            Type::Adt(name, args) => write!(f, "{}({:?})", name.to_name(), args),
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
        self.write(f, AdtNames::Unqualified)
    }
}

impl Type {
    /// Every union named anywhere in this type, outermost first, appended to `out`.
    ///
    /// A union's arguments are types in their own right and may name unions of their
    /// own, so this recurses rather than reading the head alone. [`AdtNames::for_all`]
    /// is what it exists for: deciding how to write a type means looking at every
    /// name the rendering will contain, not only the one at the top.
    fn collect_adt_names<'a>(&'a self, out: &mut Vec<&'a QualName>) {
        match self {
            Type::Literal(_) | Type::Number | Type::Variable(_) => {}
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
        }
    }

    /// The body of both renderings: the same text either way, except for how a union
    /// is named. See [`AdtNames`], and [`Display`](std::fmt::Display) for why unions
    /// are normally written bare.
    fn write(&self, f: &mut std::fmt::Formatter<'_>, names: AdtNames) -> std::fmt::Result {
        /// The same, wrapped in parentheses.
        fn parenthesised(
            tpe: &Type,
            f: &mut std::fmt::Formatter<'_>,
            names: AdtNames,
        ) -> std::fmt::Result {
            write!(f, "(")?;
            tpe.write(f, names)?;
            write!(f, ")")
        }

        match self {
            Type::Literal(TypeLiteral::Int) => write!(f, "Int"),
            Type::Literal(TypeLiteral::Char) => write!(f, "Char"),
            Type::Literal(TypeLiteral::Float) => write!(f, "Float"),
            Type::Number => write!(f, "number"),
            Type::Variable(TypeVariable { id }) => write!(f, "t{}", id),
            // The parameter of a function type is parenthesised when it is itself a
            // function, because `->` is right-associative: `(a -> b) -> c` and
            // `a -> b -> c` are different types.
            Type::Fun {
                param_tpe,
                return_tpe,
            } => {
                match **param_tpe {
                    Type::Fun { .. } => parenthesised(param_tpe, f, names)?,
                    _ => param_tpe.write(f, names)?,
                }
                write!(f, " -> ")?;
                return_tpe.write(f, names)
            }
            Type::Tuple(Tuple::Two(a, b)) => {
                write!(f, "( ")?;
                a.write(f, names)?;
                write!(f, ", ")?;
                b.write(f, names)?;
                write!(f, " )")
            }
            Type::Tuple(Tuple::Three(a, b, c)) => {
                write!(f, "( ")?;
                a.write(f, names)?;
                write!(f, ", ")?;
                b.write(f, names)?;
                write!(f, ", ")?;
                c.write(f, names)?;
                write!(f, " )")
            }
            Type::Adt(name, args) => {
                match names {
                    AdtNames::Unqualified => write!(f, "{}", name.unqualified_name())?,
                    AdtNames::Qualified => write!(f, "{}", name.to_name())?,
                }
                for arg in args {
                    // Same reason as above: an argument that is itself applied or a
                    // function needs parentheses to stay the same type when re-read.
                    write!(f, " ")?;
                    match arg {
                        Type::Adt(_, inner) if !inner.is_empty() => parenthesised(arg, f, names)?,
                        Type::Fun { .. } => parenthesised(arg, f, names)?,
                        _ => arg.write(f, names)?,
                    }
                }
                Ok(())
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
    /// ([`DEC-18` decision 1](../../../docs/decisions/dec-18.md)).
    ///
    /// The types a pattern carries are rewritten too: a constructor pattern's
    /// arguments and the bindings it introduces are the types the branch body's
    /// variables were bound at.
    fn apply_term(&self, term: TypedTerm) -> TypedTerm {
        let kind = match term.kind {
            kind @ (TypedTermKind::Int(_)
            | TypedTermKind::Bool(_)
            | TypedTermKind::Char(_)
            | TypedTermKind::Float(_)
            | TypedTermKind::Identifier(_)) => kind,
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
        };

        TypedTerm {
            span: term.span,
            tpe: self.apply_type(&term.tpe),
            kind,
        }
    }

    fn apply_binder(&self, binder: TypeBinder) -> TypeBinder {
        TypeBinder {
            tpe: self.apply_type(&binder.tpe),
            name: binder.name,
        }
    }

    fn apply_pattern(&self, pattern: TermPattern) -> TermPattern {
        let kind = match pattern.kind {
            kind @ (TermPatternKind::Anything | TermPatternKind::Bind(_)) => kind,
            TermPatternKind::Literal(tpe) => TermPatternKind::Literal(self.apply_type(&tpe)),
            TermPatternKind::Constructor {
                ctor,
                adt_args,
                bindings,
            } => TermPatternKind::Constructor {
                ctor,
                adt_args: adt_args.iter().map(|a| self.apply_type(a)).collect(),
                bindings: bindings
                    .into_iter()
                    .map(|(name, tpe)| (name, self.apply_type(&tpe)))
                    .collect(),
            },
            TermPatternKind::Tuple { elements, bindings } => TermPatternKind::Tuple {
                elements: elements.map(|element| self.apply_type(element)),
                bindings: bindings
                    .into_iter()
                    .map(|(name, tpe)| (name, self.apply_type(&tpe)))
                    .collect(),
            },
        };

        TermPattern {
            span: pattern.span,
            kind,
        }
    }

    fn substitute(tpe: Type, tvar: &TypeVariable, replacement: &Type) -> Type {
        match tpe {
            Type::Literal(_) | Type::Number => tpe,
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
        Type::Variable(tvar2) => tvar == tvar2,
        _ => false,
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
    globals: HashMap<String, Type>,
}

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
    fn extends_with(&mut self, global: HashMap<String, Type>) {
        self.globals.extend(global)
    }

    fn fresh_var(&mut self) -> Type {
        self.counter += 1;

        Type::Variable(TypeVariable { id: self.counter })
    }

    fn add_binder(&mut self, binding: TypeBinder) {
        self.env.insert(binding.name, binding.tpe);
    }

    fn remove_binder(&mut self, name: &str) {
        self.env.remove(name);
    }

    /// The type of one use of `name`: a binder's own type, or a fresh instance of a
    /// global's.
    fn by_name(&mut self, name: &String) -> Option<Type> {
        if let Some(tpe) = self.env.get(name) {
            return Some(tpe.clone());
        }

        let scheme = self.globals.get(name)?.clone();
        let mut fresh = HashMap::new();
        Some(self.instantiate(scheme, &mut fresh))
    }

    /// `tpe` with each of its variables replaced by a fresh one, the same variable by
    /// the same fresh one throughout.
    fn instantiate(&mut self, tpe: Type, fresh: &mut HashMap<TypeVariable, Type>) -> Type {
        match tpe {
            Type::Literal(_) | Type::Number => tpe,
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
        }
    }
}

/// infer the type of the given term given known function defined in the outer scopes.
/// This is a translation of the algorithm demonstrated by
/// [Ionut Gan at I T.A.K.E Unconference 2015](https://www.youtube.com/watch?v=oPVTNxiMcSU)
pub fn infer(term: Term, global: HashMap<String, Type>) -> Result<Type, ErrorKind> {
    infer_annotated(term, global, None).map(|term| term.tpe)
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
/// The term handed back is zonked — see [`Substitution::apply_term`] — so every node
/// carries the type inference solved for it and not the variable `annotate` gave it.
fn infer_annotated(
    term: Term,
    global: HashMap<String, Type>,
    annotation: Option<Annotation>,
) -> Result<TypedTerm, ErrorKind> {
    let mut env = Types::new();
    env.extends_with(global);

    let typed_term = annotate::annotate(term, &mut env)?;
    debug!("typed term: {:#?}", typed_term);

    let mut constraints = Vec::new();

    if let Some(annotation) = annotation {
        // Left is the annotation's type, because left is the type of the text the
        // span points at — see `Constraint`.
        constraints.push(Constraint::new(
            annotation.tpe,
            typed_term.tpe.clone(),
            Reason::Annotation,
            annotation.span,
        ));
    }

    constraints.extend(constraint::collect(&typed_term));
    debug!("Constraints: {:#?}", constraints);

    let substitution = unifier::unify(constraints)?;

    Ok(substitution.apply_term(typed_term))
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
    // source, in `tests/typer.rs`.
    fn bool(b: bool) -> Term {
        Term::bare(TermKind::Bool(b))
    }
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
                Type::Number => "number".to_owned(),
                Type::Variable(TypeVariable { id }) => {
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

        // Integer literals infer as `number` (polymorphic: Int or Float)
        assert_eq!(
            Signature::of_type(infered),
            "(number -> Bool) -> number".to_owned()
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
        let term = apply(apply(var("+"), bool(true)), int(1));
        assert!(infer(term, global).is_err());
    }

    // --- A constructor pattern finds only this module's own unions ------------

    /// The qualified name of `name` as declared by `module`.
    fn qual(module: &str, name: &str) -> QualName {
        QualName::parse(format!("{}.{}", module, name))
            .expect("a module and a name make a qualified name")
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
    /// 1](../../../docs/decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)).
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
                qual("Lib", "size"),
                canonical::Type::Variable("a".into())
            ))
            .kind,
            ReferenceKind::Foreign(qual("Lib", "size"))
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

        let (translated, _) = translate_pattern(&pattern, &translation, &mut counter)
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
            (Type::Variable(TypeVariable { id: 7 }), "t7"),
            (Type::Number, "number"),
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
            kind.message(),
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
            kind.message(),
            "circular type: a type variable would have to contain itself in `Box Size`"
        );
    }
}
