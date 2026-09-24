//! The intermediate representation code generation reads.
//!
//! The typer produces it. It is the only phase that knows a node's type, and every node
//! here carries one ([`DEC-18` decision
//! 1](../../../docs/decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)).
//! [`Term`] is the untyped half — what the translation from the canonical AST builds —
//! and [`TypedTerm`] is the same tree once inference has solved a type for each of its
//! nodes. A [`Module`] holds the unions a module declares and one [`Declaration`] per
//! value: everything about a module that only emission asks for.
//!
//! It is not the whole of what a backend is handed. `check_module` answers with a
//! [`CheckedModule`](crate::compiler::CheckedModule), which is this beside the
//! [`canonical::Module`] it was built from, and two of the things emission needs are
//! still only on that half — a module's `exports`, which is what a JavaScript module has
//! to export, and `canonical::Value::TypedValue`'s `marked_unsafe`, which
//! [`javascript::emit`](crate::compiler::javascript::emit) reads because an `unsafe`
//! signature and an effectful one emit differently. Nothing here duplicates them.
//!
//! The type language itself is still [`typer::Type`](crate::compiler::typer::Type). It is
//! the typer's own representation and unification is written against it, so it stays
//! there; this module names it and adds nothing to it.
//!
//! # What this shape owes WebAssembly
//!
//! One IR serves both targets and JavaScript is written first ([`DEC-18` decision
//! 2](../../../docs/decisions/dec-18.md#2--one-ir-serves-both-targets-and-javascript-is-written-first)),
//! so a reader arriving while only the JavaScript backend exists will find things
//! JavaScript has no use for. None of them is spare:
//!
//! - **A type on every node.** JavaScript needs almost none of them: the canonical AST
//!   already separates an `Int` literal from a `Float` one, and arithmetic and equality
//!   are ordinary functions behind facades. WebAssembly is statically typed, and a node's
//!   representation class — an `i64`, an `f64`, a reference — is read off its type.
//!   Solved types are also what monomorphisation consumes, which is the only way
//!   polymorphism reaches a target where [a class dictionary is erased by specialisation
//!   and never
//!   passed](../../../docs/decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed).
//! - **A constructor's index within its declaration**, and not only its name. A union is
//!   a WIT `variant` with one case per constructor and a tuple is a `tuple` ([A union
//!   crosses as a tagged
//!   value](../../../docs/spec/interop.md#a-union-crosses-as-a-tagged-value)), so a
//!   constructor is reached by its position there. JavaScript writes the name into the
//!   `$` field and never asks for the index.
//! - **Arity as a fact and saturation per call site.** A declaration emits as a plain
//!   n-ary function on both targets ([`DEC-18` decision
//!   3](../../../docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)),
//!   and a facade's [plain parameter
//!   list](../../../docs/spec/interop.md#the-javascript-companion) is the same call shape
//!   either way. WebAssembly has no closure primitive, so how a partial application is
//!   represented there is open — and an IR that made a call site's saturation something
//!   to re-derive would make that question harder rather than leaving it open.
//!
//! [`GEN-15`](../../../docs/tickets/gen-15.md) holds the questions a WebAssembly backend
//! still has to answer — linear memory or WasmGC, how a partial application is
//! represented, whether monomorphisation is whole-program — and is unscheduled. Nothing
//! in this module is a JavaScript decision, and a change that makes one of the four
//! facts above unavailable is a change that closes that ticket's options.
//!
//! # What is not here yet
//!
//! Two tickets each add a field or a pass over this shape and are deliberately not
//! written into it: a `case` becomes a decision tree
//! ([`GEN-5`](../../../docs/tickets/gen-5.md)), and a self tail call is marked
//! ([`GEN-6`](../../../docs/tickets/gen-6.md)). [`Module`] holds its declarations in a
//! `Vec` sorted by name, which is a deterministic order and not an evaluation order;
//! [`Module::initialisation_order`] is the evaluation order, over the parameterless ones
//! alone, and [`javascript::emit`](crate::compiler::javascript::emit) emits them in it.

use std::collections::HashMap;

use super::canonical;
use super::name::{Name, QualName};
use super::position::NodeSpan;
use super::tuple::Tuple;
use super::typer::Type;
use super::ModuleName;

// ── A module ──────────────────────────────────────────────────────────────────

/// One checked module's emittable shape: what a backend reads that no earlier phase
/// carried.
///
/// Not everything emission needs — `exports` and `marked_unsafe` stay on the
/// [`canonical::Module`] this was built from, and a backend is handed both halves as a
/// [`CheckedModule`](crate::compiler::CheckedModule). See this module's doc comment.
#[derive(Debug)]
pub struct Module {
    pub name: ModuleName,
    /// True when this module is a `module foreign` facade: every one of its
    /// declarations is a signature with no body, and the code behind them is in the
    /// companion beside it ([Foreign
    /// interoperability](../../../docs/spec/interop.md)).
    pub foreign: bool,
    /// The unions this module declares, sorted by name.
    pub unions: Vec<Union>,
    /// The declarations this module can emit, sorted by name.
    ///
    /// Sorted so that two runs of the compiler over one unchanged module produce the
    /// same order — `canonical::Module::values` is a `HashMap` and yields none. It is
    /// not an evaluation order: [`initialisation_order`](Self::initialisation_order) is
    /// what works out which parameterless declaration has to be initialised before which.
    pub declarations: Vec<Declaration>,
    /// The declarations that have no IR, and therefore cannot be emitted.
    ///
    /// A backend handed only [`declarations`](Self::declarations) could not tell a module
    /// it may emit whole from one that quietly lost a declaration on the way here, which
    /// is the mistake [`DEC-18` decision
    /// 1](../../../docs/decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)
    /// is about. Every value of the canonical module is in one list or the other.
    pub unchecked: Vec<Unchecked>,
    /// The names of [`declarations`](Self::declarations) that take no parameter, in the
    /// order they must be initialised: each only after every parameterless declaration its
    /// own body mentions
    /// (`docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once`).
    /// Empty for a module whose declarations all take parameters, and for a `module
    /// foreign` facade, whose constants are evaluated on whatever schedule the target
    /// gives them (`docs/spec/interop.md#facade-constants`).
    ///
    /// Sorted so that two runs of the compiler over one unchanged module produce the same
    /// order, same as [`declarations`](Self::declarations) above — including among
    /// bindings with no edge between them, where a topological sort alone leaves the order
    /// unconstrained.
    ///
    /// [`canonical::initialisation_order`] computes it, reusing the dependency graph
    /// `canonical::canonicalize` already built to reject a cycle (`LANG-35`) rather than
    /// building a second one from the same rule; see that function's doc comment for the
    /// acyclic assumption this relies on and which phase discharges it.
    /// [`javascript::emit`](crate::compiler::javascript::emit) is what emits declarations
    /// in this order — this only computes it.
    pub initialisation_order: Vec<Name>,
}

/// A union declaration: the type a constructor builds, and the constructors that build
/// it.
#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    /// The union, named by the module that declared it.
    pub name: QualName,
    /// The type variables the declaration was written with, in order.
    pub variables: Vec<Name>,
    /// Its constructors, in declaration order — which is the order
    /// [`Variant::index`] counts.
    pub variants: Vec<Variant>,
}

/// One constructor of a [`Union`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    /// The constructor's own name, unqualified: the value of the `$` field on
    /// JavaScript, and the WIT case name.
    pub name: Name,
    /// Its position in the `type` declaration, counted from zero.
    pub index: usize,
    /// How many arguments it takes.
    pub arity: usize,
}

/// One value of a module: what a backend emits as a named binding.
#[derive(Debug)]
pub struct Declaration {
    pub name: Name,
    /// How many arguments a call site has to supply for the call to be a direct call.
    ///
    /// For a declaration with a body this is the number of parameters it was written
    /// with, which is [`Body::parameters`]'s length — *not* the number of arrows in its
    /// type. `f : Int -> Int -> Int` written `f a = add a` has arity 1, and a call
    /// supplying two arguments is one direct call followed by one partial application.
    ///
    /// For a facade signature, which has no body to count parameters off, it is the
    /// number of arrows in the signature: the companion's export [takes a plain parameter
    /// list](../../../docs/spec/interop.md#the-javascript-companion) of exactly that
    /// length, and a signature with no arrow at all is [a facade
    /// constant](../../../docs/spec/interop.md#facade-constants).
    pub arity: usize,
    /// The declaration's own type, as inference solved it — or, for a facade, as the
    /// signature declares it.
    pub tpe: Type,
    /// The parameters and the expression they are in scope over, for a declaration that
    /// has a body.
    ///
    /// `None` is a `module foreign` facade's signature. It is the whole reason this is an
    /// `Option`: a facade declares what crosses the boundary and the code is in the
    /// companion, so there is nothing here to emit and
    /// [`javascript::emit`](crate::compiler::javascript::emit) reads the signature
    /// instead.
    pub body: Option<Body>,
    /// Where the declaration was written, annotation and body together.
    pub span: NodeSpan,
}

/// A declaration's parameters and the expression they are in scope over.
#[derive(Debug)]
pub struct Body {
    /// The declaration's parameters, outermost first, each with the type inference
    /// solved for it.
    pub parameters: Vec<TypeBinder>,
    /// What the declaration evaluates to once its parameters are bound.
    pub expression: TypedTerm,
}

/// A declaration the typer could not type, and which therefore has no IR.
///
/// Why it could not is decided in [`build`], from the [`Solved`] entry, and is not
/// carried here: a construct the translation cannot represent
/// ([`Solved::Untranslatable`]) or a name the typer's environment does not hold
/// ([`Solved::UnboundName`]). Neither is a mistake in the user's source, and neither is
/// an error; both are gaps in today's typer, and `ERR-8`'s warning is what will need the
/// reason carried this far.
#[derive(Debug, Clone, PartialEq)]
pub struct Unchecked {
    pub name: Name,
    /// Where the declaration was written.
    pub span: NodeSpan,
}

// ── Names ─────────────────────────────────────────────────────────────────────

/// A reference to a name, and what kind of name it is.
///
/// The kind is the point. `canonical::ExpressionKind` distinguishes a local, a
/// top-level, an imported name and a constructor, and those are four different things to
/// emit — a parameter, a binding in this module's scope, a named import, an object with
/// a `$` field. The spelling left behind once they are flattened to a string is
/// qualified for some of them and not others, so the distinction cannot be recovered
/// afterwards.
#[derive(Debug, Clone, PartialEq)]
pub struct Reference {
    /// The spelling inference looks the name up by.
    ///
    /// The typer's environment is a `HashMap<String, Type>` keyed by the spelling the
    /// canonical AST carries — bare for a local, qualified for everything else — so this
    /// is that key and not a display name. A backend reads [`kind`](Self::kind), which
    /// says what the name *is*.
    pub name: String,
    pub kind: ReferenceKind,
}

impl Reference {
    /// A reference to a name bound inside the declaration: a parameter, or a name a
    /// pattern introduced.
    pub fn local<S: Into<String>>(name: S) -> Reference {
        Reference {
            name: name.into(),
            kind: ReferenceKind::Local,
        }
    }
}

/// Which of the four things a [`Reference`] names.
#[derive(Debug, Clone, PartialEq)]
pub enum ReferenceKind {
    /// A parameter of the enclosing declaration, or a name one of its patterns bound.
    /// It is in scope for that declaration alone.
    Local,
    /// A declaration of the module being compiled, named in full.
    TopLevel(QualName),
    /// A declaration of another module, named in full: a named import of whatever that
    /// module emitted.
    ///
    /// The typer checks it against the type that module's interface declares.
    Foreign(QualName),
    /// A union constructor: it builds a tagged value rather than reading a binding.
    Constructor(Constructor),
}

/// A constructor, and its place in the declaration that declares it.
///
/// Both targets need all four: the name is the `$` field's value and the WIT case name,
/// the index is the case's position in the `variant`, the arity says how many arguments
/// a saturated application supplies, and the union is which declaration the case belongs
/// to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constructor {
    /// The union this constructor builds, named by the module that declared it.
    pub union: QualName,
    /// The constructor's own name, unqualified.
    pub name: Name,
    /// Its position in that union's declaration, counted from zero.
    pub index: usize,
    /// How many arguments it takes.
    pub arity: usize,
}

/// Whether an application supplies every argument its callee takes.
///
/// A saturated application at a callee whose arity is known emits as a direct call;
/// everything else goes through the runtime's `$curry` helper ([`DEC-18` decision
/// 3](../../../docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)).
/// An [`Apply`](TermKind::Apply) node supplies one argument, so this is a property of a
/// node within the application spine and not of the spine as a whole: `f a b` at a
/// two-parameter `f` is [`Partial`](Saturation::Partial) on the inner node and
/// [`Saturated`](Saturation::Saturated) on the outer one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Saturation {
    /// This node supplies the last of the arguments the callee takes.
    Saturated,
    /// It does not — either because arguments are still missing, because the spine has
    /// already run past the callee's arity, or because the callee's arity is not known
    /// here at all. A callee that is a parameter is a value rather than a declaration and
    /// has no arity; an imported one has an arity this module cannot see.
    Partial,
}

// ── Terms ─────────────────────────────────────────────────────────────────────

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
    pub(crate) fn bare(kind: TermKind) -> Term {
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
    /// An integer literal, at the width [`Int` *is*](../../../docs/spec/evaluation-semantics.md#numbers)
    /// ([`DEC-16`](../../../docs/decisions/dec-16.md)). Inference never reads the value
    /// — every literal is a `number` whatever it says — but code generation does.
    Int(i64),
    Char(char),
    Float(f64),
    Identifier(Reference), // VAR
    Fun {
        param: String,
        body: Box<Term>,
    },
    Apply {
        fun: Box<Term>,
        arg: Box<Term>,
        saturation: Saturation,
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
        /// What the source wrote that this match was built from.
        form: CaseForm,
    },
}

/// What the source wrote that a `Case` term was built from.
///
/// A parameter written as a pattern — `first (x, _) = x` — is a match like any other,
/// and it is translated as one: the parameter becomes a plain one named by
/// [`pattern_parameter`], and the declaration's body a single-branch `Case` on it. So a
/// backend lowers a pattern in either position the same way, and the term language keeps
/// one binding construct, [`TermKind::Fun`], that only ever binds a name.
///
/// This is what lets a diagnostic about the match speak about what the user wrote: a
/// type error in a parameter's pattern names the parameter and not a `case`, one in the
/// body under it names the declaration's body and not a `case` branch, and a
/// backend refusing the match can say which of the two it is refusing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseForm {
    /// A `case … of` expression.
    Expression,
    /// A parameter the declaration wrote as a pattern. The `Case` has exactly one
    /// branch, and its scrutinee is the local reference [`pattern_parameter`] names.
    Parameter,
}

/// The name a parameter written as a pattern is bound under: `$` and the parameter's
/// position in the declaration, counted from zero — `$0`, `$1`, ….
///
/// It is the scrutinee of the [`CaseForm::Parameter`] match the parameter becomes, and
/// nothing else refers to it. It cannot meet a name the source wrote, because a Zelkova
/// identifier never contains a `$`, and it cannot meet another parameter's, because two
/// parameters of one declaration never share a position. A declaration's parameters are
/// the only names its body is in scope of besides the ones a pattern binds, which are
/// source names, so that is every name it could meet.
pub(crate) fn pattern_parameter(position: usize) -> String {
    format!("${}", position)
}

/// Simplified pattern used inside a [`Term`], and where it was written.
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
    /// Matches one specific value; constrains the scrutinee to the type carried here.
    ///
    /// That type is a [`Type::Literal`] for an `Int` or a `Char` pattern, and the
    /// [`Type::Adt`] `typer::bool_type` builds for a `true`/`false` one — `Bool` is the
    /// union `Basics` declares, not a literal type.
    Literal(Type),
    /// Matches an ADT constructor; carries the fresh ADT args and field bindings.
    Constructor {
        /// Which constructor, and where it sits in its declaration. `ctor.union` is the
        /// name the [`Type::Adt`] this pattern constrains the scrutinee to is built from.
        ctor: Constructor,
        adt_args: Vec<Type>,
        /// `(variable_name, its_type_var)` for each bound constructor argument.
        bindings: Vec<(String, Type)>,
    },
    /// Matches a tuple of two or three elements; carries a fresh type per element and
    /// the bindings its elements introduce.
    Tuple {
        /// One type per element, which the matched value's tuple type is built from.
        elements: Tuple<Type>,
        /// `(variable_name, its_type)` for each element written as a variable.
        bindings: Vec<(String, Type)>,
    },
}

#[derive(Debug, Clone)]
/// Bind a name and a type together.
/// Used in function and let expression
pub struct TypeBinder {
    pub name: String,
    pub tpe: Type,
}

impl TypeBinder {
    pub(crate) fn new(name: String, tpe: Type) -> TypeBinder {
        TypeBinder { name, tpe }
    }
}

/// Like a [Term] but with an associated [Type], and still with its position.
/// Any term introducing a name will have a TypeBinder instead.
///
/// This is what [`typer::type_check`](crate::compiler::typer::type_check) hands back for
/// a declaration it typed, and the types on it are the *solved* ones: `infer_annotated`
/// applies `unify`'s final substitution to every node before returning. Between
/// `annotate` and that point they are inference variables and mean nothing on their own.
#[derive(Debug)]
pub struct TypedTerm {
    pub span: NodeSpan,
    pub tpe: Type,
    pub kind: TypedTermKind,
}

#[derive(Debug)]
pub enum TypedTermKind {
    /// See [`TermKind::Int`] for the width.
    Int(i64),
    Bool(bool),
    Char(char),
    Float(f64),
    Identifier(Reference),
    Fun {
        param: TypeBinder,
        body: Box<TypedTerm>,
    },
    Apply {
        fun: Box<TypedTerm>,
        arg: Box<TypedTerm>,
        saturation: Saturation,
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
        /// See [`TermKind::Case`].
        form: CaseForm,
    },
}

// ── What the typer answers with ───────────────────────────────────────────────

/// What the typer has to say about one declaration.
///
/// A phase that answers with types has to answer for *every* declaration it was given,
/// including the ones it could not type: a caller handed only the ones that worked
/// cannot tell a declaration the typer verified from one it walked past, and emitting
/// code for the second is a miscompile. So the three ways a declaration goes untyped
/// each get a variant, and [`typer::type_check`](crate::compiler::typer::type_check)
/// returns one entry per declaration either way.
#[derive(Debug)]
pub enum Solved {
    /// The declaration's typed term, with the final substitution applied to every node
    /// — the *zonk*. Its own `tpe` is the declaration's type; each node below it
    /// carries the type inference solved for that sub-expression.
    Typed(TypedTerm),
    /// A declaration of a `module foreign` facade, whose body is a synthetic
    /// placeholder rather than anything the user wrote. Nothing about it is inferred.
    ///
    /// This variant carries nothing, so [`Solved::typed`] answers `None` for it: the
    /// declaration's type is the signature the facade declares, and it stays where
    /// canonicalization put it, on `canonical::Value::TypedValue`'s `tpe`. A consumer
    /// reading these entries is walking the same `canonical::Module` the map was
    /// solved from — `type_check` takes one and keys the map the way its `values`
    /// is keyed — so the signature is one lookup away, and repeating it here would be
    /// a second copy of it rather than something inference established. [`build`] is
    /// what reads it back out.
    NoBody,
    /// `value_to_term_and_annotation` could not translate the declaration into the
    /// typer's term language — a `VarKernel` reference, a float pattern, or a pattern
    /// nested inside a constructor or tuple pattern, whether a `case` branch or a
    /// parameter wrote it. Nothing about the declaration was checked.
    ///
    /// Not an [`Error`](crate::compiler::typer::Error): it is a gap in the typer rather
    /// than a mistake in the source. What it wants is a warning, which the compiler does not have yet (`ERR-8`, see
    /// `docs/tickets/README.md`) — hence the span, so that the warning has a caret the
    /// day it exists. *Which* of the constructs tripped it is not carried:
    /// `value_to_term_and_annotation` answers `Option`, so the reason does not survive
    /// the return.
    Untranslatable {
        /// Where the declaration was written, annotation and body together — the only
        /// position available, since the construct that stopped the translation is not
        /// reported back.
        span: NodeSpan,
    },
    /// Inference reached a name the typer's environment does not hold, and nothing
    /// about the declaration was checked.
    ///
    /// That environment holds a declared type for every value in reach that has one:
    /// the values and constructors every imported interface exposes, and this module's
    /// own constructors and annotated declarations. A declaration of this module
    /// written without an annotation has no declared type, and a name reaching one
    /// lands here. That is not a mistake in the source, which is why this is not an
    /// [`Error`](crate::compiler::typer::Error) either. A name that genuinely does not
    /// exist is caught earlier, by canonicalization, as
    /// `canonical::Error::VariableNotFound`, with a caret under the name.
    UnboundName {
        /// The name as inference looked it up.
        name: String,
        /// Where it was written.
        span: NodeSpan,
    },
}

impl Solved {
    /// The typed term, for a declaration that has one.
    pub fn typed(&self) -> Option<&TypedTerm> {
        match self {
            Solved::Typed(term) => Some(term),
            _ => None,
        }
    }
}

// ── Building a module ─────────────────────────────────────────────────────────

/// Turn a checked module and what the typer solved for it into the IR a backend reads.
///
/// `solved` is consumed rather than borrowed: a [`Declaration`] owns its body, and the
/// only other holder of these terms is the caller that just received them.
///
/// Every value of `module` ends up in exactly one of [`Module::declarations`] and
/// [`Module::unchecked`] — see the second field for why nothing may merely go missing.
pub fn build(module: &canonical::Module, solved: HashMap<Name, Solved>) -> Module {
    let mut unions: Vec<Union> = module
        .types
        .iter()
        .map(|(name, union)| Union {
            name: module.name.qualify_name(name),
            variables: union.variables.clone(),
            variants: variants_of(union),
        })
        .collect();
    unions.sort_by(|left, right| {
        left.name
            .to_name()
            .as_str()
            .cmp(right.name.to_name().as_str())
    });

    // Type variables in a facade signature are numbered from here. Each signature is
    // translated on its own and is never unified with anything, so the numbering only
    // has to be consistent within one declaration.
    let mut counter = 0u32;

    let mut solved = solved;
    let mut names: Vec<&Name> = module.values.keys().collect();
    names.sort_by(|left, right| left.as_str().cmp(right.as_str()));

    let mut declarations = Vec::new();
    let mut unchecked = Vec::new();

    for name in names {
        let value = &module.values[name];
        let span = value.span();

        match solved.remove(name) {
            Some(Solved::Typed(term)) => {
                let tpe = term.tpe.clone();
                let (parameters, expression) = peel(term, value.arity());

                declarations.push(Declaration {
                    name: name.clone(),
                    arity: parameters.len(),
                    tpe,
                    body: Some(Body {
                        parameters,
                        expression,
                    }),
                    span,
                });
            }
            // A facade signature: the type is the one canonicalization recorded, and
            // the arity is what the companion's parameter list has to be.
            Some(Solved::NoBody) => match facade_signature(value, &mut counter) {
                Some((arity, tpe)) => declarations.push(Declaration {
                    name: name.clone(),
                    arity,
                    tpe,
                    body: None,
                    span,
                }),
                None => unchecked.push(Unchecked {
                    name: name.clone(),
                    span,
                }),
            },
            // Untranslatable, UnboundName, and — impossible today, since `type_check`
            // answers for every value it was given — a declaration with no entry at all.
            _ => unchecked.push(Unchecked {
                name: name.clone(),
                span,
            }),
        }
    }

    let initialisation_order = canonical::initialisation_order(module);

    Module {
        name: module.name.clone(),
        foreign: module.binding_foreign,
        unions,
        declarations,
        unchecked,
        initialisation_order,
    }
}

/// A union's constructors, each with the position and the argument count both targets
/// need.
pub(crate) fn variants_of(union: &canonical::UnionType) -> Vec<Variant> {
    union
        .variants
        .iter()
        .enumerate()
        .map(|(index, ctor)| Variant {
            name: ctor.name.clone(),
            index,
            arity: ctor.type_parameters.len(),
        })
        .collect()
}

/// Split `arity` parameters off the front of a typed term.
///
/// `value_to_term_and_annotation` wraps a declaration's body in one `Fun` per parameter,
/// so the first `arity` nodes of the spine are exactly those parameters and nothing else
/// builds a `Fun` — the language has no lambda. Stopping early on anything that is not a
/// `Fun` keeps a term that somehow disagreed with its declaration from being read past
/// its end; the parameters handed back are the ones that were really there, which is why
/// [`Declaration::arity`] is their count.
fn peel(term: TypedTerm, arity: usize) -> (Vec<TypeBinder>, TypedTerm) {
    let mut parameters = Vec::with_capacity(arity);
    let mut term = term;

    while parameters.len() < arity {
        let TypedTerm { span, tpe, kind } = term;

        match kind {
            TypedTermKind::Fun { param, body } => {
                parameters.push(param);
                term = *body;
            }
            kind => {
                term = TypedTerm { span, tpe, kind };
                break;
            }
        }
    }

    (parameters, term)
}

/// The arity and the type of a facade's signature, when it has one to read.
///
/// A facade declaration has no parameters to count — it is a signature and a synthetic
/// body — so its arity is the number of arrows in the type it declares: the companion's
/// export takes a parameter list of that length, and no arrow at all is [a facade
/// constant](../../../docs/spec/interop.md#facade-constants).
///
/// `None` is a facade declaration carrying no annotation, which nothing produces today —
/// a facade's declarations are signatures — and which would have no type to emit against.
fn facade_signature(value: &canonical::Value, counter: &mut u32) -> Option<(usize, Type)> {
    match value {
        canonical::Value::Value { .. } => None,
        canonical::Value::TypedValue { tpe, .. } => {
            let mut variables = HashMap::new();
            let translated =
                crate::compiler::typer::canonical_type_to_typer_type(tpe, &mut variables, counter)?;

            Some((signature_arity(tpe), translated))
        }
    }
}

/// How many arguments a value of this type takes: the length of its arrow spine.
fn signature_arity(tpe: &canonical::Type) -> usize {
    match tpe {
        canonical::Type::Arrow(_, result) => 1 + signature_arity(result),
        _ => 0,
    }
}
