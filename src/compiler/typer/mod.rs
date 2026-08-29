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
use crate::compiler::name::Name;
use crate::compiler::position::NodeSpan;
use crate::compiler::tuple::Tuple;
use crate::compiler::{PhaseError, SpanLabel};
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
    // The origins are boxed because an `ErrorKind` is the `Err` half of a `Result`
    // threaded through the whole of `unify`, and every *successful* return pays for
    // the size of the largest variant. `Origin` carries a `Cause` per side.
    UnificationFailed {
        left: Type,
        right: Type,
        origin: Box<Origin>,
    },
    /// A type variable would have to occur inside its own solution.
    CircularType {
        /// The type the variable would have had to contain itself in.
        tpe: Type,
        origin: Box<Origin>,
    },
    /// A name the typer's environment does not know. See [`type_check`] for why this
    /// one is not reported to the user today.
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
                format!("cannot match `{}` with `{}`", left, right)
            }
            ErrorKind::CircularType { tpe, .. } => format!(
                "circular type: a type variable would have to contain itself in `{}`",
                tpe
            ),
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

/// Type check one canonical module, reporting every value whose inference produced a
/// reportable error rather than stopping at the first — the shape `compile_package` is
/// built to accumulate.
///
/// # The two failures this pass still swallows
///
/// Both are in the third pass below, and neither is a statement about the user's
/// source, which is why neither is reported as an error against it.
///
/// **An unsupported construct.** `value_to_term_and_annotation` returns `None` for
/// anything the term language cannot express — a `VarKernel` or `VarForeign`
/// reference, a nested pattern inside a `case`. The declaration is then not checked
/// at all. Reporting that as an error would fail 53 of the declarations in
/// `std/core/src` that are simply beyond today's inference; it is a gap in the
/// typer, and what it wants is a warning, which the compiler does not have yet
/// (`ERR-8`, see `docs/tickets/README.md`).
///
/// **An unbound variable.** [`ErrorKind::UnboundVariable`] means the name is missing
/// from the environment the two passes above build — and that environment is
/// assembled from *this module alone*: its own annotated values and its own type
/// constructors. Anything that crossed a module boundary is therefore absent even
/// though canonicalization resolved it perfectly well: `Maybe.isJust` referring to
/// `True`, which `Basics` declares, or `Basics.negate` referring to `-`, which the
/// infix declaration aliases to `sub`. Five declarations in `std/core/src` hit this
/// today and not one of them is a mistake in the source. A name that genuinely does
/// not exist is caught earlier, by canonicalization, as
/// `canonical::Error::VariableNotFound`, with a caret under the name — so nothing a
/// user can write reaches the user only through this path. The error nonetheless
/// carries its span now, so the day the typer's environment spans modules it can be
/// reported without further plumbing.
///
/// # Where an [`Error`] is built
///
/// Here, and only here: this is the last frame that holds the `canonical::Value`, so
/// it is the only one that knows the declaration's name and its span. What went
/// *wrong* and *where inside the declaration* comes up from inference on the
/// [`ErrorKind`]; see [`Error`].
pub fn type_check(module: &Module) -> Result<(), Vec<Error>> {
    // JavaScript binding modules use synthetic placeholder bodies — skip type checking.
    if module.binding_javascript {
        return Ok(());
    }

    // Start at a high offset to avoid collisions with the counter inside
    // Types::new() (which starts at 10) used during inference.
    let mut counter = 10_000u32;

    // First pass: build global env from all TypedValues' declared types.
    // This allows values to reference other module-level typed values.
    let mut global: HashMap<String, Type> = HashMap::new();
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

    // Second pass: add constructor types to global from module.types
    for (type_name, union_type) in &module.types {
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
        let result_type = Type::Adt(type_name.as_str().to_string(), result_args);

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

            // Register under both unqualified ("Just") and qualified ("Test.Just") names
            global.insert(ctor.name.as_str().to_string(), ctor_type.clone());
            let qname = module
                .name
                .qualify_name(&ctor.name)
                .to_name()
                .as_str()
                .to_string();
            global.insert(qname, ctor_type);
        }
    }

    // Third pass: check each value. A value that fails is recorded and the pass
    // moves on, so one broken declaration cannot hide the others.
    let mut errors: Vec<Error> = vec![];
    for (name, value) in &module.values {
        let Some((term, annotation)) =
            value_to_term_and_annotation(value, &module.types, &mut counter)
        else {
            continue; // unsupported construct — see this function's documentation
        };

        // The annotation is handed to inference rather than checked against its
        // result afterwards, which is what lets a mismatch *inside* the body know
        // that the type it failed against came from the annotation. Checking the two
        // whole types at the end could only ever say "this declaration is `Int` and
        // its body is something else", with the caret across the lot.
        match infer_annotated(term, global.clone(), annotation) {
            // See this function's documentation: an unbound variable here is a hole
            // in the typer's environment, not a mistake in the source.
            Err(ErrorKind::UnboundVariable { .. }) => continue,
            Err(kind) => errors.push(Error {
                kind,
                span: value.span(),
                declaration: name.clone(),
            }),
            Ok(_) => (),
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

// ── Translation helpers ───────────────────────────────────────────────────────

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
fn canonical_type_to_typer_type(
    tpe: &canonical::Type,
    var_map: &mut HashMap<String, TypeVariable>,
    counter: &mut u32,
) -> Option<Type> {
    match tpe {
        canonical::Type::Type(name, args) if args.is_empty() && name.as_str() == "Int" => {
            Some(Type::Literal(TypeLiteral::Int))
        }
        canonical::Type::Type(name, args) if args.is_empty() && name.as_str() == "Bool" => {
            Some(Type::Literal(TypeLiteral::Bool))
        }
        canonical::Type::Type(name, args) if args.is_empty() && name.as_str() == "Char" => {
            Some(Type::Literal(TypeLiteral::Char))
        }
        canonical::Type::Type(name, args) if args.is_empty() && name.as_str() == "Float" => {
            Some(Type::Literal(TypeLiteral::Float))
        }
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
            let converted: Option<Vec<Type>> = args
                .iter()
                .map(|a| canonical_type_to_typer_type(a, var_map, counter))
                .collect();
            Some(Type::Adt(name.as_str().to_string(), converted?))
        }
    }
}

/// Convert a canonical expression to a Term, keeping the position it was written at.
///
/// Returns None for constructs the inference engine doesn't yet handle
/// (VarKernel, VarForeign, complex patterns inside Case).
///
/// Every arm attaches `expr.span` to the term it builds. That is the whole of what
/// `ERR-4` needed from this function: a constraint can only point at a
/// sub-expression if the term that produced it remembers where it came from.
fn canonical_expr_to_term(
    expr: &canonical::Expression,
    module_types: &HashMap<Name, canonical::UnionType>,
    counter: &mut u32,
) -> Option<Term> {
    let kind = match &expr.kind {
        canonical::ExpressionKind::Int(i) => TermKind::Int(*i as u32),
        canonical::ExpressionKind::Bool(b) => TermKind::Bool(*b),
        canonical::ExpressionKind::Char(c) => TermKind::Char(*c),
        canonical::ExpressionKind::Float(f) => TermKind::Float(*f),
        canonical::ExpressionKind::VarLocal(name) => {
            TermKind::Identifier(name.as_str().to_string())
        }
        canonical::ExpressionKind::VarTopLevel(qname) => {
            TermKind::Identifier(qname.to_name().as_str().to_string())
        }
        canonical::ExpressionKind::Apply(f, a) => {
            let fun = canonical_expr_to_term(f, module_types, counter)?;
            let arg = canonical_expr_to_term(a, module_types, counter)?;
            TermKind::Apply {
                fun: Box::new(fun),
                arg: Box::new(arg),
            }
        }
        canonical::ExpressionKind::If(cond, t, f) => {
            let cond = canonical_expr_to_term(cond, module_types, counter)?;
            let t = canonical_expr_to_term(t, module_types, counter)?;
            let f = canonical_expr_to_term(f, module_types, counter)?;
            TermKind::If {
                cond: Box::new(cond),
                true_branch: Box::new(t),
                false_branch: Box::new(f),
            }
        }
        canonical::ExpressionKind::Tuple(tuple) => {
            let elements = tuple
                .try_map(|elem| canonical_expr_to_term(elem, module_types, counter).ok_or(()))
                .ok()?;
            TermKind::Tuple(elements)
        }
        canonical::ExpressionKind::Case(scrutinee_expr, branches) => {
            let scrutinee = canonical_expr_to_term(scrutinee_expr, module_types, counter)?;
            let term_branches: Vec<(TermPattern, Box<Term>)> = branches
                .iter()
                .map(|cb| {
                    let (pattern, _bindings) =
                        translate_pattern(&cb.pattern, module_types, counter)?;
                    let body = canonical_expr_to_term(&cb.expression, module_types, counter)?;
                    Some((pattern, Box::new(body)))
                })
                .collect::<Option<Vec<_>>>()?;
            TermKind::Case {
                scrutinee: Box::new(scrutinee),
                branches: term_branches,
            }
        }
        // Constructors are resolved as identifiers looked up in the global env.
        canonical::ExpressionKind::VarConstructor(qname, _) => {
            TermKind::Identifier(qname.to_name().as_str().to_string())
        }
        // VarForeign: not in the module's global env, skip
        canonical::ExpressionKind::VarForeign(_, _) => return None,
        // Not yet supported: VarKernel
        _ => return None,
    };

    Some(Term {
        span: expr.span,
        kind,
    })
}

/// Translate a canonical pattern into a `TermPattern` plus any variable bindings
/// introduced by the pattern.  Returns `None` for unsupported pattern shapes.
///
/// The pattern keeps its own span, separate from the branch body's: a `case` branch
/// whose pattern does not match what is being matched on is about the pattern, and
/// the caret belongs there rather than under the expression in the `case … of` line.
fn translate_pattern(
    pattern: &canonical::Pattern,
    module_types: &HashMap<Name, canonical::UnionType>,
    counter: &mut u32,
) -> Option<(TermPattern, Vec<(String, Type)>)> {
    let (kind, bindings) = match &pattern.kind {
        canonical::PatternKind::Anything => (TermPatternKind::Anything, vec![]),
        canonical::PatternKind::Variable(name) => {
            // The binding's actual type will be unified with the scrutinee type in annotate.
            (TermPatternKind::Bind(name.as_str().to_string()), vec![])
        }
        canonical::PatternKind::Bool(_) => (
            TermPatternKind::Literal(Type::Literal(TypeLiteral::Bool)),
            vec![],
        ),
        canonical::PatternKind::Int(_) => (
            TermPatternKind::Literal(Type::Literal(TypeLiteral::Int)),
            vec![],
        ),
        canonical::PatternKind::Char(_) => (
            TermPatternKind::Literal(Type::Literal(TypeLiteral::Char)),
            vec![],
        ),
        canonical::PatternKind::Constructor { ctor, args } => {
            // Look up the parent union type to get its type variables.
            let union_type = module_types.get(&ctor.tpe)?;

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
                adt_name: ctor.tpe.as_str().to_string(),
                adt_args,
                bindings: bindings.clone(),
            };
            (kind, bindings)
        }
        _ => return None, // Tuple, Float patterns — not yet supported
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
/// The body is wrapped in nested Fun nodes for each pattern parameter.
/// Returns None if any part of the value cannot be translated.
fn value_to_term_and_annotation(
    value: &canonical::Value,
    module_types: &HashMap<Name, canonical::UnionType>,
    counter: &mut u32,
) -> Option<(Term, Option<Annotation>)> {
    match value {
        canonical::Value::Value { patterns, body, .. } => {
            let body_term = canonical_expr_to_term(body, module_types, counter)?;
            let term = wrap_with_patterns(patterns.iter(), body_term)?;
            Some((term, None))
        }
        canonical::Value::TypedValue {
            patterns,
            body,
            tpe,
            annotation_span,
            ..
        } => {
            let body_term = canonical_expr_to_term(body, module_types, counter)?;
            let pattern_iter = patterns.iter().map(|(p, _)| p);
            let term = wrap_with_patterns(pattern_iter, body_term)?;
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

/// Wrap a body Term in nested Fun nodes for each pattern, outermost first.
/// Returns None if any pattern is not translatable (e.g. constructor patterns).
///
/// Each `Fun` spans its parameter through the body it wraps, so a function whose
/// declared shape does not match its definition is underlined from the parameter
/// that starts it rather than across the annotation as well.
fn wrap_with_patterns<'a>(
    patterns: impl Iterator<Item = &'a canonical::Pattern>,
    body: Term,
) -> Option<Term> {
    let names: Vec<(String, NodeSpan)> = patterns
        .map(|p| match &p.kind {
            canonical::PatternKind::Variable(name) => Some((name.as_str().to_string(), p.span)),
            canonical::PatternKind::Anything => Some(("_".to_string(), p.span)),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;

    let term = names.iter().rev().fold(body, |acc, (param, span)| Term {
        span: span.merge(acc.span),
        kind: TermKind::Fun {
            param: param.clone(),
            body: Box::new(acc),
        },
    });

    Some(term)
}

// First try of an implementation. Not linked to the rest of the code base for simplicity's sake.
//
// It is no longer *quite* that: the term language is still simplified — names degrade
// to `String`, anything inference does not need is dropped — but every node carries
// the [`NodeSpan`] of the canonical node it was built from, because an error found
// down here has to be able to say where in the user's source it happened (`ERR-4`).

mod annotate;
mod constraint;
mod unifier;

/// Simplified pattern used inside the typer's Term, and where it was written.
///
/// Same shape as the parser and canonical ASTs — a span beside a kind — so a reader
/// matches on `&p.kind`.
#[derive(Debug, Clone)]
pub struct TermPattern {
    pub span: NodeSpan,
    pub kind: TermPatternKind,
}

#[derive(Debug, Clone)]
pub enum TermPatternKind {
    /// Matches anything without binding.
    Anything,
    /// Binds the scrutinee type to this name.
    Bind(String),
    /// Matches a specific literal type; constrains the scrutinee to that type.
    Literal(Type),
    /// Matches an ADT constructor; carries the fresh ADT args and field bindings.
    Constructor {
        adt_name: String,
        adt_args: Vec<Type>,
        /// `(variable_name, its_type_var)` for each bound constructor argument.
        bindings: Vec<(String, Type)>,
    },
}

/// An untyped term, and where the expression it was translated from was written.
///
/// In zelkova that source is the canonical AST. The span is what every constraint
/// generated from this term inherits, and therefore what a type error draws its
/// caret under; [`NodeSpan::none`] — a term built by hand, in a test — costs nothing
/// but the caret.
#[derive(Debug, Clone)] // TODO Remove clone when not needed anymore
pub struct Term {
    pub span: NodeSpan,
    pub kind: TermKind,
}

impl Term {
    /// A term with no position: hand-built, never translated from source.
    #[cfg(test)]
    fn bare(kind: TermKind) -> Term {
        Term {
            span: NodeSpan::none(),
            kind,
        }
    }
}

#[derive(Debug, Clone)] // TODO Remove clone when not needed anymore
pub enum TermKind {
    // literals
    Bool(bool),
    Int(u32),
    Char(char),
    Float(f64),
    Identifier(String), // VAR
    Fun {
        param: String,
        body: Box<Term>,
    },
    Apply {
        fun: Box<Term>,
        arg: Box<Term>,
    },
    If {
        cond: Box<Term>,
        true_branch: Box<Term>,
        false_branch: Box<Term>,
    },
    Let {
        binding: String,
        value: Box<Term>,
        body: Box<Term>,
    },
    Tuple(Tuple<Term>),
    Case {
        scrutinee: Box<Term>,
        branches: Vec<(TermPattern, Box<Term>)>,
    },
}

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeLiteral {
    Int,
    Bool,
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
    /// A named algebraic data type, e.g. `Maybe Int` → `Adt("Maybe", [Literal(Int)])`.
    Adt(String, Vec<Type>),
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
            Type::Adt(name, args) if args.is_empty() => write!(f, "{}", name),
            Type::Adt(name, args) => write!(f, "{}({:?})", name, args),
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
        match self {
            Type::Literal(TypeLiteral::Int) => write!(f, "Int"),
            Type::Literal(TypeLiteral::Bool) => write!(f, "Bool"),
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
            } => match **param_tpe {
                Type::Fun { .. } => write!(f, "({}) -> {}", param_tpe, return_tpe),
                _ => write!(f, "{} -> {}", param_tpe, return_tpe),
            },
            Type::Tuple(Tuple::Two(a, b)) => write!(f, "( {}, {} )", a, b),
            Type::Tuple(Tuple::Three(a, b, c)) => write!(f, "( {}, {}, {} )", a, b, c),
            Type::Adt(name, args) if args.is_empty() => write!(f, "{}", name),
            Type::Adt(name, args) => {
                write!(f, "{}", name)?;
                for arg in args {
                    // Same reason as above: an argument that is itself applied or a
                    // function needs parentheses to stay the same type when re-read.
                    match arg {
                        Type::Adt(_, inner) if !inner.is_empty() => write!(f, " ({})", arg)?,
                        Type::Fun { .. } => write!(f, " ({})", arg)?,
                        _ => write!(f, " {}", arg)?,
                    }
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
/// Bind a name and a type together.
/// Used in function and let expression
struct TypeBinder {
    name: String,
    tpe: Type,
}

impl TypeBinder {
    fn new(name: String, tpe: Type) -> TypeBinder {
        TypeBinder { name, tpe }
    }
}

/// Like a [Term] but with an associated [Type], and still with its position.
/// Any term introducing a name will have a TypeBinder instead.
#[derive(Debug)]
struct TypedTerm {
    span: NodeSpan,
    tpe: Type,
    kind: TypedTermKind,
}

#[derive(Debug)]
#[allow(dead_code)]
enum TypedTermKind {
    Int(u32),
    Bool(bool),
    Char(char),
    Float(f64),
    // TODO Do I want to keep this name ? Or named Variable ? Something else ?
    Identifier(String), // This is basically a TypeBinder
    Fun {
        param: TypeBinder,
        body: Box<TypedTerm>,
    },
    Apply {
        fun: Box<TypedTerm>,
        arg: Box<TypedTerm>,
    },
    If {
        cond: Box<TypedTerm>,
        true_branch: Box<TypedTerm>,
        false_branch: Box<TypedTerm>,
    },
    Let {
        binding: TypeBinder,
        value: Box<TypedTerm>,
        body: Box<TypedTerm>,
    },
    Tuple(Tuple<TypedTerm>),
    Case {
        scrutinee: Box<TypedTerm>,
        branches: Vec<(TermPattern, Box<TypedTerm>)>,
    },
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

struct Types {
    counter: u32,
    env: HashMap<String, Type>,
}

impl Types {
    fn new() -> Types {
        let counter = 10;
        let env = HashMap::new();

        Types { counter, env }
    }

    // Add variable name binding from an outer scope
    fn extends_with(&mut self, global: HashMap<String, Type>) {
        self.env.extend(global)
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

    fn by_name(&self, name: &String) -> Option<Type> {
        self.env.get(name).cloned()
    }
}

/// infer the type of the given term given known function defined in the outer scopes.
/// This is a translation of the algorithm demonstrated by
/// [Ionut Gan at I T.A.K.E Unconference 2015](https://www.youtube.com/watch?v=oPVTNxiMcSU)
pub fn infer(term: Term, global: HashMap<String, Type>) -> Result<Type, ErrorKind> {
    infer_annotated(term, global, None)
}

/// [`infer`], with the declaration's type annotation as a constraint of its own.
///
/// The annotation is put *first*, before the constraints the body generates, and that
/// ordering is the point of the function. `unify` solves constraints in order, so the
/// annotated type is substituted into the body's constraints before any of them are
/// solved; when one of them then fails, its [`Origin::explanation`] names the annotation,
/// and the diagnostic can say `Int` was expected *because of the annotation* rather
/// than merely that the declaration as a whole does not check.
fn infer_annotated(
    term: Term,
    global: HashMap<String, Type>,
    annotation: Option<Annotation>,
) -> Result<Type, ErrorKind> {
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

    Ok(substitution.apply_type(&typed_term.tpe))
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
    fn int(i: u32) -> Term {
        Term::bare(TermKind::Int(i))
    }
    fn var(n: &str) -> Term {
        Term::bare(TermKind::Identifier(n.to_string()))
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
                Type::Literal(TypeLiteral::Bool) => "Bool".to_owned(),
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
                Type::Adt(name, args) if args.is_empty() => name,
                Type::Adt(name, args) => {
                    let arg_strs: Vec<String> =
                        args.into_iter().map(|a| self.type_signature(a)).collect();
                    format!("{} {}", name, arg_strs.join(" "))
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

    // --- Display for Type ---------------------------------------------------

    fn int_t() -> Type {
        Type::Literal(TypeLiteral::Int)
    }
    fn bool_t() -> Type {
        Type::Literal(TypeLiteral::Bool)
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
    fn adt(name: &str, args: Vec<Type>) -> Type {
        Type::Adt(name.to_string(), args)
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
}
