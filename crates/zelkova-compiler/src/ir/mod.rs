//! The intermediate representation code generation reads.
//!
//! The typer produces it. It is the only phase that knows a node's type, and every node
//! here carries one ([`DEC-18` decision
//! 1](../../docs/decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)).
//! [`Term`] is the untyped half — what the translation from the canonical AST builds —
//! and [`TypedTerm`] is the same tree once inference has solved a type for each of its
//! nodes. A [`Module`] holds the unions a module declares and one [`Declaration`] per
//! value: everything about a module that only emission asks for.
//!
//! It is not the whole of what a backend is handed. `check_module` answers with a
//! [`CheckedModule`](crate::CheckedModule), which is this beside the
//! [`canonical::Module`] it was built from, and two of the things emission needs are
//! still only on that half — a module's `exports`, which is what a JavaScript module has
//! to export, and `canonical::Value::TypedValue`'s `marked_unsafe`, which
//! `zelkova_js::emit` reads because an `unsafe`
//! signature and an effectful one emit differently. Nothing here duplicates them.
//!
//! The type language itself is still [`typer::Type`](crate::typer::Type). It is
//! the typer's own representation and unification is written against it, so it stays
//! there; this module names it and adds nothing to it.
//!
//! # What this shape owes WebAssembly
//!
//! One IR serves both targets and JavaScript is written first ([`DEC-18` decision
//! 2](../../docs/decisions/dec-18.md#2--one-ir-serves-both-targets-and-javascript-is-written-first)),
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
//!   passed](../../docs/decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed).
//! - **A constructor's index within its declaration**, and not only its name. A union is
//!   a WIT `variant` with one case per constructor and a tuple is a `tuple` ([A union
//!   crosses as a tagged
//!   value](../../docs/spec/interop.md#a-union-crosses-as-a-tagged-value)), so a
//!   constructor is reached by its position there. JavaScript writes the name into the
//!   `$` field and never asks for the index.
//! - **Arity as a fact and saturation per call site.** A declaration emits as a plain
//!   n-ary function on both targets ([`DEC-18` decision
//!   3](../../docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)),
//!   and a facade's [plain parameter
//!   list](../../docs/spec/interop.md#the-javascript-companion) is the same call shape
//!   either way. WebAssembly has no closure primitive, so how a partial application is
//!   represented there is open — and an IR that made a call site's saturation something
//!   to re-derive would make that question harder rather than leaving it open.
//!
//! [`GEN-15`](../../docs/tickets/gen-15.md) holds the questions a WebAssembly backend
//! still has to answer — linear memory or WasmGC, how a partial application is
//! represented, whether monomorphisation is whole-program — and is unscheduled. Nothing
//! in this module is a JavaScript decision, and a change that makes one of the four
//! facts above unavailable is a change that closes that ticket's options.
//!
//! # What is not here yet
//!
//! One ticket adds a mark to this shape and is deliberately not written into it yet: a
//! self tail call ([`GEN-6`](../../docs/tickets/gen-6.md)). [`Module`] holds its
//! declarations in a `Vec` sorted by name, which is a deterministic order and not an
//! evaluation order; [`Module::initialisation_order`] is the evaluation order, over the
//! parameterless ones alone, and `zelkova_js::emit`
//! emits them in it.
//!
//! A `case`'s branches keep [`TypedTermKind::Case`]'s own flat shape rather than
//! growing a tree in place: the typer still wants a flat list, since every branch is
//! checked against the same scrutinee type and order does not matter to it. The
//! `decision` module is a second, additional view of the same branches — built from
//! them on demand rather than stored alongside them — and [`Decision`] is the tree a
//! backend walks instead of re-deriving, at emission time, which test distinguishes
//! which branch (`GEN-5`). See that module's doc comment.

use std::collections::HashMap;

use super::canonical;
use super::name::{Name, QualName};
use super::typer::Type;
use super::{ModuleName, PackageName};
use zelkova_syntax::position::NodeSpan;
use zelkova_syntax::tuple::Tuple;

mod decision;
pub use decision::{build as decision_tree, Binding, Decision, Occurrence, Outcome, Step};

// ── A module ──────────────────────────────────────────────────────────────────

/// One checked module's emittable shape: what a backend reads that no earlier phase
/// carried.
///
/// Not everything emission needs — `exports` and `marked_unsafe` stay on the
/// [`canonical::Module`] this was built from, and a backend is handed both halves as a
/// [`CheckedModule`](crate::CheckedModule). See this module's doc comment.
#[derive(Debug)]
pub struct Module {
    pub name: ModuleName,
    /// True when this module is a `module foreign` facade: every one of its
    /// declarations is a signature with no body, and the code behind them is in the
    /// companion beside it ([Foreign
    /// interoperability](../../docs/spec/interop.md)).
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
    /// 1](../../docs/decisions/dec-18.md#1--the-backend-reads-a-typed-ir-and-the-typer-is-what-produces-it)
    /// is about. Every value of the canonical module, in `values` or in `broken`, is in
    /// exactly one of the two lists.
    ///
    /// Sorted by name, the same as [`declarations`](Self::declarations).
    pub unchecked: Vec<Unchecked>,
    /// The names of [`declarations`](Self::declarations) that take no parameter, in the
    /// order they must be initialised: each only after every parameterless declaration it
    /// depends on, whether its own body mentions that declaration or reaches it through a
    /// function it mentions
    /// (`docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once`).
    /// Empty for a module whose declarations all take parameters, and for a `module
    /// foreign` facade, whose constants are evaluated on whatever schedule the target
    /// gives them (`docs/spec/interop.md#facade-constants`).
    ///
    /// Sorted so that two runs of the compiler over one unchanged module produce the same
    /// order, same as [`declarations`](Self::declarations) above — including among
    /// bindings with no path between them, where dependencies alone leave the order
    /// unconstrained.
    ///
    /// `canonical::initialisation_order` computes it, from the same dependency graph
    /// `canonical::canonicalize` reads to reject a cycle (`LANG-35`) rather than a second
    /// one built from the same rule; see that function's doc comment for what it does with
    /// a module whose cycle was rejected.
    /// `zelkova_js::emit` is what emits declarations
    /// in this order — this only computes it.
    pub initialisation_order: Vec<Name>,
}

/// A union declaration: the type a constructor builds, and the constructors that build
/// it.
#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    /// The union, named by the package and module that declared it.
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
    /// list](../../docs/spec/interop.md#the-javascript-companion) of exactly that
    /// length, and a signature with no arrow at all is [a facade
    /// constant](../../docs/spec/interop.md#facade-constants).
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
    /// `zelkova_js::emit` reads the signature
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

/// A declaration the typer did not type, and which therefore has no IR.
///
/// Why is decided in [`build`]. Canonicalization recorded it as broken
/// ([`canonical::Module::broken`]), or inference reported an error for it
/// ([`Solved::Rejected`]), and that error is the user's to fix; or the typer walked past
/// it: a construct the translation cannot represent
/// ([`Solved::Untranslatable`]), a name the typer's environment does not hold
/// ([`Solved::UnboundName`]), or a facade declaration with no signature to read. The
/// second kind is not a mistake in the user's source and not an error, but a gap in
/// today's typer. [`reported`](Self::reported) is the one distinction carried this far,
/// so that `ERR-8`'s warning about a gap is not also given to a declaration the user has
/// already been shown an error for.
#[derive(Debug, Clone, PartialEq)]
pub struct Unchecked {
    pub name: Name,
    /// Where the declaration was written.
    pub span: NodeSpan,
    /// Whether an error stands behind this entry: `true` for a declaration
    /// canonicalization recorded as broken or inference rejected, `false` for one the
    /// typer walked past.
    pub reported: bool,
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
    /// The typer's environment is a `HashMap<String, Type>` keyed bare for a local and,
    /// for everything else, by the declaration's package and qualified name —
    /// `test-project:Test.echo` — so this is that key and not a display name. A backend
    /// reads [`kind`](Self::kind), which says what the name *is*.
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
    /// The typer checks it against the type that module's interface declares. The
    /// [`PackageName`] is the package that declares the module, which is where the
    /// import is read from: a module's name says which file it is within its package,
    /// and the package says which package's directory holds that file.
    ///
    /// The `usize` is the declaration's arity, read from the same interface
    /// ([`Interface::arities`](crate::Interface::arities)): the count a call has
    /// to supply to be a direct call, exactly as [`Declaration::arity`] is for a
    /// [`TopLevel`](Self::TopLevel) name. A module exports each declaration as the
    /// plain n-ary function it emitted, so an importer needs the arity to call it.
    Foreign(QualName, PackageName, usize),
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
    /// The union this constructor builds, named by the package and module that
    /// declared it.
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
/// 3](../../docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)).
/// An [`Apply`](TermKind::Apply) node supplies one argument, so this is a property of a
/// node within the application spine and not of the spine as a whole: `f a b` at a
/// two-parameter `f` is [`Partial`](Saturation::Partial) on the inner node and
/// [`Saturated`](Saturation::Saturated) on the outer one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Saturation {
    /// This node supplies the last of the arguments the callee takes.
    Saturated,
    /// It does not — either because arguments are still missing, because the spine has
    /// already run past the callee's arity, or because the callee has no arity at all. A
    /// callee that is a parameter, or any other expression that is not a name, is a value
    /// rather than a declaration and has no arity.
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
    /// An integer literal, at the width [`Int` *is*](../../docs/spec/evaluation-semantics.md#numbers)
    /// ([`DEC-16`](../../docs/decisions/dec-16.md)). Inference never reads the value
    /// — every literal is a `number` whatever it says — but code generation does.
    Int(i64),
    Char(char),
    /// A string literal's value, its escape sequences already read.
    String(String),
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
    /// The unit value, `()`.
    Unit,
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
/// `pattern_parameter`, and the declaration's body a single-branch `Case` on it. So a
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
    /// branch, and its scrutinee is the local reference `pattern_parameter` names.
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
    /// Matches one specific value; constrains the scrutinee to the type carried here
    /// and, unlike the type alone, says which value of it.
    ///
    /// `tpe` is a [`Type::Literal`] for an `Int` or a `Char` pattern, and the
    /// [`Type::Adt`] `typer::bool_type` builds for a `Basics.True`/`Basics.False` one —
    /// `Bool` is the union `Basics` declares, not a literal type. Two patterns of that
    /// same kind — two `Int`s, say — share that one type, so `value` is what tells `1`
    /// from `2`, or `True` from `False`; [`decision_tree`] reads it to build the [`Outcome`] a
    /// [`Decision::Test`] checks for.
    Literal { tpe: Type, value: LiteralValue },
    /// Matches an ADT constructor; carries the fresh ADT args and one sub-pattern per
    /// argument.
    Constructor {
        /// Which constructor, and where it sits in its declaration. `ctor.union` is the
        /// name the [`Type::Adt`] this pattern constrains the scrutinee to is built from.
        ctor: Constructor,
        adt_args: Vec<Type>,
        /// One per argument the pattern writes, in order, so an argument's place in
        /// this `Vec` is its position in the constructor.
        args: Vec<SubPattern>,
    },
    /// Matches a tuple of two or three elements; carries one sub-pattern per element.
    Tuple {
        /// One per element, in order. The matched value's tuple type is built from
        /// their types.
        elements: Tuple<SubPattern>,
    },
    /// Matches the one value of the unit type: constrains the matched value to
    /// [`Type::Unit`] and binds nothing. Since that
    /// type has a single value, [`decision_tree`] builds no test for it.
    Unit,
}

/// A pattern written in a position inside another one — a constructor's argument or a
/// tuple's element — and the type of the value found there.
///
/// The position's type travels with the pattern because a sub-pattern has no scrutinee
/// of its own to take one from: a variable written there binds a value of this type, and
/// any other pattern written there constrains this type the way a `case` branch's
/// pattern constrains the scrutinee's.
///
/// Today the pattern is only ever a [`TermPatternKind::Bind`], a
/// [`TermPatternKind::Anything`] or a [`TermPatternKind::Unit`]:
/// `typer::translate_pattern` refuses anything else nested (`LANG-16`). The shape does
/// not assume it.
#[derive(Debug, Clone)]
pub struct SubPattern {
    pub tpe: Type,
    pub pattern: TermPattern,
}

impl TermPattern {
    /// Every name this pattern binds, at any depth, in source order, with the type of
    /// the value it binds. A variable written at the top is bound at `scrutinee`, the
    /// type of the value the pattern is matched against.
    pub(crate) fn bindings(&self, scrutinee: &Type) -> Vec<(String, Type)> {
        let mut bindings = Vec::new();
        self.collect_bindings(scrutinee, &mut bindings);
        bindings
    }

    fn collect_bindings(&self, tpe: &Type, bindings: &mut Vec<(String, Type)>) {
        match &self.kind {
            TermPatternKind::Anything | TermPatternKind::Literal { .. } | TermPatternKind::Unit => {
            }
            TermPatternKind::Bind(name) => bindings.push((name.clone(), tpe.clone())),
            TermPatternKind::Constructor { args, .. } => {
                for arg in args {
                    arg.pattern.collect_bindings(&arg.tpe, bindings);
                }
            }
            TermPatternKind::Tuple { elements } => {
                for element in elements.iter() {
                    element.pattern.collect_bindings(&element.tpe, bindings);
                }
            }
        }
    }
}

/// The concrete value a [`TermPatternKind::Literal`] pattern tests for.
///
/// The pattern's own `tpe` cannot tell `1` from `2`, or `'a'` from `'b'`: both share one
/// type, and only this says which value the scrutinee has to equal. A `Bool` is tested
/// by its value too: `Basics`' own `True`/`False` constructors arrive here as a
/// `Bool(..)`, never as a [`TermPatternKind::Constructor`] (`typer::translate_pattern`
/// does the normalising).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralValue {
    Bool(bool),
    Int(i64),
    Char(char),
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
/// This is what [`typer::type_check`](crate::typer::type_check) hands back for
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
    Char(char),
    /// See [`TermKind::String`].
    String(String),
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
    /// The unit value, `()`. Its type is always [`Type::Unit`].
    ///
    /// `zelkova_js::emit` emits it as `undefined`
    /// ([The unit value crosses as
    /// `undefined`](../../docs/spec/interop.md#the-unit-value-crosses-as-undefined)).
    Unit,
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
/// code for the second is a miscompile. So each way a declaration goes untyped gets a
/// variant, and [`typer::type_check_recovering`](crate::typer::type_check_recovering)
/// returns one entry per declaration either way.
#[derive(Debug)]
pub enum Solved {
    /// The declaration's typed term, with the final substitution applied to every node
    /// — the *zonk*. Its own `tpe` is the declaration's type; each node below it
    /// carries the type inference solved for that sub-expression.
    ///
    /// Boxed because a term carries a whole [`QualName`] — package included — for every
    /// union and constructor it names, and every other variant would otherwise pay for
    /// its size.
    Typed(Box<TypedTerm>),
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
    /// Not an [`Error`](crate::typer::Error): it is a gap in the typer rather
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
    /// [`Error`](crate::typer::Error) either. A name that genuinely does not
    /// exist is caught earlier, by canonicalization, as
    /// `canonical::Error::VariableNotFound`, with a caret under the name.
    UnboundName {
        /// The name as inference looked it up.
        name: String,
        /// Where it was written.
        span: NodeSpan,
    },
    /// Inference reported an error for this declaration.
    ///
    /// The error itself is in [`TypeCheck::errors`](crate::typer::TypeCheck::errors),
    /// beside the map this entry is in. The entry is what keeps the declaration from
    /// being merely absent, so a module with a type error still answers for every
    /// declaration it holds.
    Rejected,
}

impl Solved {
    /// The typed term, for a declaration that has one.
    pub fn typed(&self) -> Option<&TypedTerm> {
        match self {
            Solved::Typed(term) => Some(term.as_ref()),
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
/// Every value of `module`, in [`values`](canonical::Module::values) or in
/// [`broken`](canonical::Module::broken), ends up in exactly one of
/// [`Module::declarations`] and [`Module::unchecked`] — see the second field for why
/// nothing may merely go missing. A broken one is always unchecked, with an error
/// reported behind it: canonicalization's, or the syntax error of a declaration chunk
/// that did not parse.
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
                let (parameters, expression) = peel(*term, value.arity());

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
                Some(tpe) => declarations.push(Declaration {
                    name: name.clone(),
                    arity: module.emitted_arity(value),
                    tpe,
                    body: None,
                    span,
                }),
                None => unchecked.push(Unchecked {
                    name: name.clone(),
                    span,
                    reported: false,
                }),
            },
            // The error inference reported for it is the caller's to report.
            Some(Solved::Rejected) => unchecked.push(Unchecked {
                name: name.clone(),
                span,
                reported: true,
            }),
            // Untranslatable, UnboundName, and — impossible today, since the typer
            // answers for every value it was given — a declaration with no entry at all.
            Some(Solved::Untranslatable { .. }) | Some(Solved::UnboundName { .. }) | None => {
                unchecked.push(Unchecked {
                    name: name.clone(),
                    span,
                    reported: false,
                })
            }
        }
    }

    // A broken declaration has no canonical form for the typer to have read, and the
    // error behind it — canonicalization's, or a syntax error — is the caller's to report.
    unchecked.extend(module.broken.iter().map(|broken| Unchecked {
        name: broken.name.clone(),
        span: broken.span,
        reported: true,
    }));
    unchecked.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));

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

/// The type of a facade's signature, when it has one to read.
///
/// Its arity is not read here: a facade declaration has no parameters to count — it is a
/// signature and a synthetic body — and [`canonical::Module::emitted_arity`] counts the
/// arrows of the type it declares instead, since the companion's export takes a parameter
/// list of that length and no arrow at all is [a facade
/// constant](../../docs/spec/interop.md#facade-constants). That is the count a module
/// importing the facade reads from its interface too.
///
/// `None` is a facade declaration carrying no annotation, which nothing produces today —
/// a facade's declarations are signatures — and which would have no type to emit against.
fn facade_signature(value: &canonical::Value, counter: &mut u32) -> Option<Type> {
    match value {
        canonical::Value::Value { .. } => None,
        canonical::Value::TypedValue { tpe, .. } => {
            let mut variables = HashMap::new();
            crate::typer::canonical_type_to_typer_type(tpe, &mut variables, counter)
        }
    }
}
