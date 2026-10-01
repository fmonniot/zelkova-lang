//! The canonical representation of a zelkova programs is a translation of a local
//! source into the broader world.
//!
//! This phase is where we integrate the local parsed module into the rest of the
//! program. We do the following steps:
//!
//! - Resolve all imports
//! - Qualify all `Name` (eg. a local value `test` in a `Mod.A` module will be renamed `Mod.A.test`)
//! - Checks that exported names are actually present in the module
//! - Checks there is none cyclic dependency between this module and others (Might be done earlier, let's see)
//!
//! Note that we use `HashMap`'s a lot in this module's structures. This is because later phases will
//! want to have cheap access to the different components of a `Module`.
//!
//! TODO Rename this to core ? I feel it's going to be te main internal representation of the language.
use super::resolve::CORE_PACKAGE;
use super::scalars;
use super::Interface;
use super::PhaseError;
use super::SpanLabel;
use super::{ModuleName, PackageName};
use crate::utils::collect_accumulate;
use log::{debug, trace};
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::Direction;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use zelkova_syntax::parser;

mod environment;
/// Part of [`Error::AmbiguousVariables`] and [`Error::AmbiguousVariants`]'s public
/// shape, so it is re-exported alongside the error rather than left behind a
/// private module.
pub use environment::ImportOrigin;
/// Part of [`Error::AmbiguousOperatorPrecedence`]'s public shape, so it is
/// re-exported alongside the error rather than left behind a private module.
pub use environment::InfixDeclaration;
use environment::{
    new_environment, suggest_name, EnvError, Environment, InfixFunction, RootEnvironment, ValueType,
};

// Some elements which are common to both AST
use crate::name::{Name, QualName};
use crate::source::files::SourceFileId;
pub use parser::Associativity;
use zelkova_syntax::position::NodeSpan;
use zelkova_syntax::tuple::Tuple;

// begin AST

/// A resolved module
#[derive(Debug)]
pub struct Module {
    pub name: ModuleName,
    pub exports: Exports,
    /// Where the header's `exposing (...)` was written — `parser::Module::exposing_span`,
    /// carried through. A diagnostic about what the module exposes as a whole points here.
    pub exposing_span: NodeSpan,
    /// Operator name to infix details
    pub infixes: HashMap<Name, Infix>,
    pub types: HashMap<Name, UnionType>,
    pub values: HashMap<Name, Value>,
    /// True when this module is a `module foreign` facade.
    /// Such modules have synthetic placeholder bodies and must not be type-checked.
    pub binding_foreign: bool,
}

impl Module {
    /// Build the trimmed-down view of this module other modules import against.
    ///
    /// `file` is the [`SourceFileId`] this module was read from, which is what lets
    /// a later module's diagnostic point back into *this* module's source — see
    /// [`Interface::file`](super::Interface::file). It is a parameter rather than
    /// something this method could work out because a `canonical::Module` does not
    /// know it: only driver code does, and the sole caller
    /// (`dependencies::ModuleWalker::check_in_order`) is driver code that has the
    /// map from module name to file in hand. A caller with nothing to give — every
    /// test that hand-builds an interface — passes `None`, and
    /// [`Interface::source_span`](super::Interface::source_span) then declines to
    /// build a cross-module label rather than building one at the wrong place.
    ///
    /// # What the `exposing (...)` header removes here
    ///
    /// This is the only place a module's own [`Exports`] is read, and reading it
    /// here is what makes a declaration left out of the header private (`BUG-9`).
    /// `self.values`, `self.types` and `self.infixes` stay complete — the typer,
    /// exhaustiveness and every later phase run against the whole
    /// [`Module`](Self), because a private declaration is still callable from
    /// inside the module that wrote it. Only this external view is trimmed, and
    /// only by three rules:
    ///
    /// - a value reaches the interface when the header exposes its name as
    ///   [`ExportType::Value`], on top of the pre-existing requirement that it
    ///   carry a type annotation (`Value::TypedValue`). `do_exports` is what
    ///   makes that requirement real (`SPEC-5`): it rejects a module that
    ///   exposes an unannotated declaration before this method ever runs, so
    ///   the `Value::Value` arm in the `filter_map` below is unreachable
    ///   through normal use — its own comment says why it stays rather than
    ///   becoming an `unwrap`;
    /// - an infix reaches it when the header exposes the operator;
    /// - a union type reaches it when the header names it either way, but a
    ///   `Size` entry ([`ExportType::UnionPrivate`]) hands over the declaration
    ///   with its `variants` emptied. That is the opaque type: importers still
    ///   get the name and its type variables — [`Interface::unions`] is where
    ///   `process_import`'s `Privacy::Private` arm reads the arity from — and no
    ///   constructor to build or match one with.
    ///
    /// [`Exports::Everything`] — a `exposing (..)` header — exposes every
    /// declaration with every constructor, so nothing is dropped in that case.
    ///
    /// An exposed infix whose backing function is not itself separately exposed
    /// — `infix left 6 (+) = add` with `(+)` in the header and `add` not, which
    /// is every operator `std/core` declares — still needs `add`'s type
    /// reachable, or an importer that brings `(+)` into scope has no type to
    /// give a use of it. [`Interface::infix_functions`] carries exactly that,
    /// kept out of `values` itself so `add` stays unreachable under its own
    /// name.
    pub fn to_interface(&self, file: Option<SourceFileId>) -> super::Interface {
        let values: HashMap<Name, (NodeSpan, Type)> = self
            .values
            .iter()
            .filter(|(name, _)| self.exports.exposes(name, &ExportType::Value))
            .filter_map(|(name, value)| match value {
                // Unreachable through normal use: `do_exports` (`SPEC-5`)
                // rejects an exposed, unannotated declaration before
                // canonicalization ever produces a `Module` for this method
                // to run on (`BUG-14`). Kept rather than `unwrap`ped so a
                // future caller that hands this a hand-built `Module` — a
                // test, most likely — gets a value silently absent from the
                // interface instead of a panic.
                Value::Value { .. } => None,
                Value::TypedValue { tpe, span, .. } => Some((name.clone(), (*span, tpe.clone()))),
            })
            .collect();

        let unions = self
            .types
            .iter()
            .filter_map(|(name, union)| match self.exports.union_visibility(name) {
                UnionVisibility::Hidden => None,
                UnionVisibility::Transparent => Some((name.clone(), union.clone())),
                UnionVisibility::Opaque => Some((
                    name.clone(),
                    UnionType {
                        variables: union.variables.clone(),
                        variants: Vec::new(),
                        span: union.span,
                    },
                )),
            })
            .collect();

        let infixes: HashMap<Name, Infix> = self
            .infixes
            .iter()
            .filter(|(name, _)| self.exports.exposes(name, &ExportType::Infix))
            .map(|(name, infix)| (name.clone(), infix.clone()))
            .collect();

        // See `Interface::infix_functions`'s doc comment: only a backing function
        // *not* already reaching `values` on its own needs a supplementary entry
        // here — one that is already exposed by name is inserted twice
        // otherwise, which `insert_foreign_value` reads as the same value
        // imported from two places.
        let infix_functions: HashMap<Name, (NodeSpan, Type)> = infixes
            .values()
            .filter(|infix| !values.contains_key(&infix.function_name))
            .filter_map(|infix| match self.values.get(&infix.function_name) {
                Some(Value::TypedValue { tpe, span, .. }) => {
                    Some((infix.function_name.clone(), (*span, tpe.clone())))
                }
                _ => None,
            })
            .collect();

        let arities = values
            .keys()
            .chain(infix_functions.keys())
            .filter_map(|name| {
                self.values
                    .get(name)
                    .map(|value| (name.clone(), self.emitted_arity(value)))
            })
            .collect();

        super::Interface {
            module_name: self.name.clone(),
            values,
            unions,
            infixes,
            infix_functions,
            arities,
            file,
        }
    }

    /// How many parameters `value`, one of this module's declarations, is emitted with:
    /// the count a call has to supply to be a direct call.
    ///
    /// For a declaration with a body that is [`Value::arity`], the parameters it was
    /// written with. A `module foreign` facade's declaration is a signature with no
    /// parameters to count, and its forwarding function takes one parameter per arrow in
    /// the signature ([`Type::arrow_count`]), the [plain parameter
    /// list](../../docs/spec/interop.md#the-javascript-companion) its companion's
    /// export takes.
    ///
    /// [`to_interface`](Self::to_interface) records it for each exported value, so that
    /// an importer's call site is saturated at the same count this module's own call
    /// sites are.
    pub fn emitted_arity(&self, value: &Value) -> usize {
        match value {
            Value::TypedValue { tpe, .. } if self.binding_foreign => tpe.arrow_count(),
            value => value.arity(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Exports {
    Everything,
    /// Non qualified name to its export type
    Specifics(HashMap<Name, ExportType>),
}

impl Exports {
    /// Whether the `exposing (...)` header exposes `name` *as* `kind`.
    ///
    /// The kind is part of the question rather than a detail of the answer
    /// because the three namespaces a module exports into are separate: a
    /// header naming `map` exposes the value `map`, and says nothing about an
    /// operator or a type that happened to share the spelling. Union types ask
    /// [`union_visibility`](Self::union_visibility) instead, which has a third
    /// answer for the opaque case.
    pub(crate) fn exposes(&self, name: &Name, kind: &ExportType) -> bool {
        match self {
            Exports::Everything => true,
            Exports::Specifics(specifics) => specifics.get(name) == Some(kind),
        }
    }

    /// How far a union type declared in this module crosses the module boundary.
    fn union_visibility(&self, name: &Name) -> UnionVisibility {
        match self {
            Exports::Everything => UnionVisibility::Transparent,
            Exports::Specifics(specifics) => match specifics.get(name) {
                Some(ExportType::UnionPublic) => UnionVisibility::Transparent,
                Some(ExportType::UnionPrivate) => UnionVisibility::Opaque,
                Some(ExportType::Value) | Some(ExportType::Infix) | None => UnionVisibility::Hidden,
            },
        }
    }
}

/// The three answers [`Exports::union_visibility`] can give for one of the
/// module's own union types, in the order a reader of an `exposing (...)` list
/// meets them: no entry at all, a bare `Size` entry, a `Size(..)` entry.
#[derive(Debug)]
enum UnionVisibility {
    /// Not in the header: other modules cannot name the type at all.
    Hidden,
    /// `Size` — the type's name and arity cross the boundary, its constructors
    /// do not.
    Opaque,
    /// `Size(..)` — the declaration crosses whole, constructors included.
    Transparent,
}

#[derive(Debug, PartialEq)]
pub enum ExportType {
    Value,
    Infix,
    UnionPublic,
    UnionPrivate,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Infix {
    pub associativity: Associativity,
    pub precedence: u8,
    pub function_name: Name,
    /// Where the `infix` declaration this came from was written.
    pub span: NodeSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionType {
    pub variables: Vec<Name>,
    pub variants: Vec<TypeConstructor>,
    /// Where the `type` declaration this came from was written.
    pub span: NodeSpan,
}

// TODO Once we have most of the pipeline built, revisit the decision of
// having Vec<Type> + Type instead of Type::Arrow(Box<Type>, Box<Type>)
// as it's essentially what a constructor is, a function from parameter
// to the resulting type.
// Later me: well, that's only true at the type level. The value also need
// to tag what variant it represent.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstructor {
    /// Constructor name. eg. in `type A = B`, the name is `B`
    pub name: Name,
    /// The types of the parameters
    pub type_parameters: Vec<Type>,
    /// The type's name once constructed, qualified by the package and module that
    /// declared it — `Widget.Size` for a `type Size = …` written in `Widget`.
    ///
    /// Qualified for the same reason [`Type::Type`]'s head is (`AST-4`): a
    /// constructor travels into every module that imports it, and the type it
    /// builds has to stay the declaring module's type there. The `Type` this
    /// field is turned into at a use site — `Expression::from_parser`'s
    /// `TypeConstructor` arm — is built straight out of it, so the two agree by
    /// construction.
    pub tpe: QualName,
}

/// A canonical type.
///
/// # Why there is no span on this type, or on [`TypeConstructor`]
///
/// Every other canonical node this module builds carries a [`NodeSpan`] taken from
/// the parser node it came from. A `Type` deliberately does not.
///
/// The reason `ERR-3` recorded is that a `Type` does not always come from the module
/// being canonicalized: `Type::from_parser_type` resolves a name through the
/// `Environment`, which clones types straight out of the [`Interface`]s of the
/// modules this one imports, so the `Type` handed back may well have been *written
/// in a different file* — and a bare span on it would be read as a position in the
/// importing module's source. `ERR-5` settled that half: a [`SpanLabel`] can now
/// carry a file of its own, so "written in another file" is no longer the obstacle.
///
/// The other half is unfinished work, not an impossibility. `parser::Type` carries a
/// [`NodeSpan`] per node like every other parser node, and `Type::from_parser_type`
/// is the recursive walk over it — `tpe.span` is in hand at every level, and each
/// branch of an `Arrow` arrives as its own spanned `parser::Type`. Those spans are
/// discarded here by choice. Keeping them means a span field on every canonical
/// `Type` node, plus a decision about what a `Type` cloned out of an `Environment`
/// should then report — where it was written, or where it was used. Nobody has
/// written that.
///
/// What exists instead is coarser and enough for the diagnostics that exist: the
/// *declaration* a `Type` is the type of — [`Value`]'s own `span`, [`UnionType`]'s,
/// [`Infix`]'s — is what a diagnostic wants to underline ("defined here"), not a
/// position inside the type expression itself. [`Interface::values`] pairs each
/// value's `Type` with that declaration's [`NodeSpan`], and
/// [`Interface::source_span`] pairs it with the file, once [`Interface::file`] is
/// set. A type error still points at the whole declaration that failed rather than
/// the sub-expression that disagrees, which is `ERR-4`, a separate limit in the
/// typer's own `Term`/`Constraint` translation.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Variable(Name),
    /// A named type applied to its arguments, the name being the [`QualName`] of
    /// the declaration it resolved to.
    ///
    /// The module is part of the identity of the type, not decoration on it: a
    /// `type Size` in `Widget` and a `type Size` in `Gadget` are two types, and
    /// two values of them are interchangeable nowhere. Carrying only the
    /// spelling made them one value here, which is what `AST-4` closed. The
    /// package is part of it for the same reason: a local `Size` and a wrapped
    /// dependency's, reached as `AcmeWidgets.Size`, both declare `Size.Size`.
    ///
    /// The name is the *declaration's*, never the spelling that reached it.
    /// `import Widget as W` followed by `W.Size` records `Widget.Size`, the same
    /// rule [name resolution](../../docs/spec/name-resolution.md) states for
    /// values: an alias names a route to a declaration and not a second
    /// declaration. A dependency's namespace is a route too.
    Type(QualName, Vec<Type>),
    // Record
    Arrow(Box<Type>, Box<Type>),
    /// A tuple type. Zelkova keeps Elm's restriction of two or three elements,
    /// which [`Tuple`] carries in its shape.
    Tuple(Tuple<Type>),
    /// [The unit type](../../docs/spec/types.md#the-unit-type), `()`.
    ///
    /// A form of its own rather than a [`Type::Type`] naming a declaration: `()` is
    /// syntax, declared nowhere and resolved through no environment, so no module can
    /// declare a type that shares its spelling and there is no name for a lookup to
    /// find. Contrast the [`scalars`], which are declared in `std/core` and known by
    /// the qualified name of that declaration.
    Unit,
    // Alias
}

impl Type {
    /// The one conversion in this module that reads a span and produces none.
    ///
    /// `parser::Type` carries a [`NodeSpan`] like every other parser node, and it is
    /// dropped here on purpose — see this type's documentation for why a canonical
    /// `Type` holds no span at all, and what `ERR-5` built instead.
    fn from_parser_type(env: &dyn Environment, tpe: &parser::Type) -> Result<Type, Error> {
        match &tpe.kind {
            parser::TypeKind::Unqualified(name, vars) => {
                let args = vars
                    .iter()
                    .map(|t| Type::from_parser_type(env, t))
                    .collect::<Result<Vec<_>, Error>>()?;

                match env.find_type(name) {
                    // `name` resolves to a declared type: `args` is what was
                    // written after it, and has to match the declaration's own
                    // arity (`BUG-17`) — nothing else here re-derives that check.
                    // The head is the *declaration's* qualified name, not the one
                    // written: `Lib.Option`, `Option` and an alias' `L.Option` are
                    // three spellings of one entry, and every one of them records
                    // the module that declared it.
                    Some(declared) if declared.arity() == args.len() => {
                        Ok(Type::Type(declared.name.clone(), args))
                    }
                    Some(declared) => Err(Error::TypeArityMismatch(
                        name.clone(),
                        declared.arity(),
                        args.len(),
                        tpe.span,
                    )),
                    // `name` resolves to nothing, so there is no type here to
                    // build one out of. The name is reported as written, dotted
                    // prefix and all: a written `W.Thing` says which *route* was
                    // written and not which module declared anything — under
                    // `import Widget as W` that half is an alias — so there is no
                    // module to attribute it to, and quoting the spelling back is
                    // what lets the reader find it in their own source.
                    None => Err(Error::TypeNotFound(name.clone(), tpe.span)),
                }
            }
            parser::TypeKind::Arrow(t1, t2) => Ok(Type::Arrow(
                Box::new(Type::from_parser_type(env, t1)?),
                Box::new(Type::from_parser_type(env, t2)?),
            )),
            parser::TypeKind::Variable(n) => Ok(Type::Variable(n.clone())),
            parser::TypeKind::Tuple(tuple) => Ok(Type::Tuple(
                tuple.try_map(|t| Type::from_parser_type(env, t))?,
            )),
            parser::TypeKind::Unit => Ok(Type::Unit),
        }
    }

    /// How many arguments a value of this type takes before its result is not a
    /// function: the length of its arrow spine. `Int -> Int -> Bool` is 2, `Int` is 0.
    pub fn arrow_count(&self) -> usize {
        match self {
            Type::Arrow(_, result) => 1 + result.arrow_count(),
            _ => 0,
        }
    }

    // TODO Write some tests
    fn to_linear_types(tpe: &Type) -> Vec<Type> {
        match tpe {
            Type::Arrow(a, b) => {
                let mut next = Type::to_linear_types(b);

                next.insert(0, *a.clone());

                next
            }
            _ => vec![tpe.clone()],
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Value {
        name: Name,
        patterns: Vec<Pattern>,
        body: Expression,
        /// Where the declaration was written, annotation and body together.
        span: NodeSpan,
    },
    TypedValue {
        name: Name,
        patterns: Vec<(Pattern, Type)>,
        body: Expression,
        tpe: Type,
        /// True when the annotation was written `unsafe name : Type`.
        ///
        /// The word is only meaningful on a facade signature, where it declares a
        /// plain function in place of the effect a facade declares by default —
        /// see [Foreign interoperability](../../docs/spec/interop.md). Only a
        /// module whose `binding_foreign` is set can carry it: [`canonicalize`]
        /// reports [`Error::UnsafeOutsideFacade`] for one written anywhere else,
        /// so this is `false` on every value of an ordinary module.
        marked_unsafe: bool,
        /// Where the declaration was written, annotation and body together.
        span: NodeSpan,
        /// Where the `name : Type` annotation alone was written.
        ///
        /// [`Type`] itself carries no span — it may have been cloned out of another
        /// module's `Interface` — so this is the only thing that can answer "where
        /// was the expected type declared". A type error uses it for the secondary
        /// label that says the annotation is what the body is being held to.
        annotation_span: NodeSpan,
    },
}

impl Value {
    /// Where this declaration was written, whichever variant it is.
    ///
    /// The typer falls back to this when a type error has no finer position of its
    /// own — see `typer::Error::labels`.
    pub fn span(&self) -> NodeSpan {
        match self {
            Value::Value { span, .. } | Value::TypedValue { span, .. } => *span,
        }
    }

    /// How many parameters this declaration was written with.
    ///
    /// This is the count of patterns on the left of the `=`, and deliberately not the
    /// number of arrows in the declaration's type: `f : Int -> Int -> Int` written `f a =
    /// add a` has arity 1, and returns a function for the second argument.
    ///
    /// The rule lives here rather than at any of its readers because they feed fields a
    /// backend reads together, from different phases. `typer`'s `Translation` uses it as
    /// the callee's arity at every call site naming this declaration, which is what
    /// decides [`ir::Saturation`](crate::ir::Saturation); `ir::build` uses it as
    /// [`ir::Declaration::arity`](crate::ir::Declaration::arity), the count a
    /// direct call has to supply; and [`Module::to_interface`] records it, through
    /// [`Module::emitted_arity`], for a module that imports the declaration. Copies that
    /// drifted apart would mark a call site saturated at a count the emitted function
    /// does not take. A facade signature has no patterns to count, and
    /// [`Module::emitted_arity`] reads its arrows instead.
    pub fn arity(&self) -> usize {
        // Two arms rather than an or-pattern: a `TypedValue`'s patterns each carry the
        // type the annotation gave them, so the two fields are different types.
        match self {
            Value::Value { patterns, .. } => patterns.len(),
            Value::TypedValue { patterns, .. } => patterns.len(),
        }
    }
}

/// A canonical pattern, and where it was written.
///
/// Same shape as [`parser::Pattern`] — a [`NodeSpan`] beside a kind — and for the
/// same reason: the children stay plain `Pattern`s, so a reader matches `&p.kind`.
#[derive(Debug, PartialEq)]
pub struct Pattern {
    pub span: NodeSpan,
    pub kind: PatternKind,
}

#[derive(Debug, PartialEq)]
pub enum PatternKind {
    Anything,
    Variable(Name), // TODO Name or QualName ?
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Bool(bool),
    /// A tuple pattern. Zelkova keeps Elm's restriction of two or three
    /// elements, which [`Tuple`] carries in its shape.
    Tuple(Tuple<Pattern>),
    /// The unit pattern, `()`: it matches the one value of the unit type and binds
    /// nothing.
    Unit,

    Constructor {
        ctor: TypeConstructor,
        args: Vec<Pattern>,
    },
}

impl Pattern {
    /// A pattern canonicalized from source, keeping the parser node's position.
    pub fn new(span: NodeSpan, kind: PatternKind) -> Pattern {
        Pattern { span, kind }
    }

    /// A pattern with no position — hand-built by a test. See [`NodeSpan`].
    pub fn bare(kind: PatternKind) -> Pattern {
        Pattern {
            span: NodeSpan::none(),
            kind,
        }
    }

    fn from_parser(p: &parser::Pattern, env: &dyn Environment) -> Result<Pattern, Error> {
        let kind = match &p.kind {
            parser::PatternKind::Anything => PatternKind::Anything,
            parser::PatternKind::Variable(name) => PatternKind::Variable(name.clone()),
            parser::PatternKind::Literal(parser::Literal::Int(i)) => PatternKind::Int(*i),
            parser::PatternKind::Literal(parser::Literal::Float(f)) => PatternKind::Float(*f),
            parser::PatternKind::Literal(parser::Literal::Char(c)) => PatternKind::Char(*c),
            parser::PatternKind::Literal(parser::Literal::String(s)) => {
                PatternKind::String(s.clone())
            }
            parser::PatternKind::Literal(parser::Literal::Bool(b)) => PatternKind::Bool(*b),
            parser::PatternKind::Tuple(tuple) => {
                PatternKind::Tuple(tuple.try_map(|p| Pattern::from_parser(p, env))?)
            }
            parser::PatternKind::Unit => PatternKind::Unit,
            parser::PatternKind::Constructor(name, args) => {
                // `p.span` covers the constructor and its arguments, which is the
                // text a "no such constructor" caret should sit under.
                let ctor = env
                    .find_type_constructor(name)
                    .ok_or_else(|| {
                        let suggestion =
                            suggest_name(name, env.type_constructor_names().into_iter());
                        Error::VariantNotFound(
                            env.module_name().qualify_name(name),
                            p.span,
                            suggestion,
                        )
                    })?
                    .clone();

                let args = args
                    .iter()
                    .map(|p| Pattern::from_parser(p, env))
                    .collect::<Result<Vec<_>, Error>>()?;

                PatternKind::Constructor { ctor, args }
            }
        };

        Ok(Pattern::new(p.span, kind))
    }
}

/// A canonical expression, and where it was written.
///
/// Same shape as [`parser::Expression`] — a [`NodeSpan`] beside a kind — and for the
/// same reason: the children stay `Box<Expression>`, so a reader matches `&e.kind`.
#[derive(Debug, PartialEq)]
pub struct Expression {
    pub span: NodeSpan,
    pub kind: ExpressionKind,
}

// TODO Find a way to detect recursive functions (even indirect recursivity,
// eg. `a` calls `b` calls `a`)
/// Expression is an optimized version for checks and caches.
///
/// Elm declare those expressions:
/// ```haskell
/// data Expr_
///   = VarLocal Name
///   | VarTopLevel ModuleName.Canonical Name
///   | VarKernel Name Name
///   | VarForeign ModuleName.Canonical Name Annotation
///   | VarCtor CtorOpts ModuleName.Canonical Name Index.ZeroBased Annotation
///   | VarDebug ModuleName.Canonical Name Annotation
///   | VarOperator Name ModuleName.Canonical Name Annotation -- CACHE real name for optimization
///   | Chr ES.String
///   | Str ES.String
///   | Int Int
///   | Float EF.Float
///   | List [Expr]
///   | Negate Expr
///   | Binop Name ModuleName.Canonical Name Annotation Expr Expr -- CACHE real name for optimization
///   | Lambda [Pattern] Expr
///   | Call Expr [Expr]
///   | If [(Expr, Expr)] Expr
///   | Let Def Expr
///   | LetRec [Def] Expr
///   | LetDestruct Pattern Expr Expr
///   | Case Expr [CaseBranch]
///   | Accessor Name
///   | Access Expr (A.Located Name)
///   | Update Name Expr (Map.Map Name FieldUpdate)
///   | Record (Map.Map Name Expr)
///   | Unit
///   | Tuple Expr Expr (Maybe Expr)
/// ```
#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    VarLocal(Name),
    VarTopLevel(QualName),
    VarKernel(QualName),
    /// A value another module declares, named by that module, beside the package that
    /// declares it. The package is what a backend builds the import path from: a
    /// module's name is unique within its package and not across a build, and the
    /// output keeps one directory per package
    /// ([`DEC-18` decision 5](../../docs/decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest)).
    VarForeign(QualName, PackageName, Type),
    /// A union constructor, named by the package and module that declared its union
    /// whichever module the reference is written in. The [`QualName`] carries the
    /// package: `AcmeWidgets.Size.Small` and a local `Size.Small` are two constructors.
    VarConstructor(QualName, Type),
    Char(char),
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    // List
    // Lambda
    Apply(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    // Let
    // LetRec
    // LetDestruct (eg. `(a,b) = someTuple`)
    Case(Box<Expression>, Vec<CaseBranch>),
    // Accessor
    // Access
    // Update (record)
    /// The unit value, `()`.
    Unit,
    /// A tuple expression. Zelkova keeps Elm's restriction of two or three
    /// elements, which [`Tuple`] carries in its shape.
    Tuple(Tuple<Expression>),
}

impl Expression {
    /// An expression canonicalized from source, keeping the parser node's position.
    pub fn new(span: NodeSpan, kind: ExpressionKind) -> Expression {
        Expression { span, kind }
    }

    /// An expression with no position: hand-built by a test, or synthesised by the
    /// compiler with nothing in the user's text behind it. See [`NodeSpan`].
    pub fn bare(kind: ExpressionKind) -> Expression {
        Expression {
            span: NodeSpan::none(),
            kind,
        }
    }

    fn from_parser(e: &parser::Expression, env: &dyn Environment) -> Result<Expression, Error> {
        // Every arm builds a kind and every kind gets `e.span`, so a name the
        // environment cannot resolve is underlined where it was written rather than
        // somewhere up the tree.
        let kind = match &e.kind {
            parser::ExpressionKind::Lit(parser::Literal::Int(i)) => ExpressionKind::Int(*i),
            parser::ExpressionKind::Lit(parser::Literal::Float(f)) => ExpressionKind::Float(*f),
            parser::ExpressionKind::Lit(parser::Literal::Char(c)) => ExpressionKind::Char(*c),
            parser::ExpressionKind::Lit(parser::Literal::String(s)) => {
                ExpressionKind::String(s.clone())
            }
            parser::ExpressionKind::Lit(parser::Literal::Bool(b)) => ExpressionKind::Bool(*b),
            parser::ExpressionKind::Variable(name) => {
                match env.find_value(name).ok_or_else(|| {
                    let suggestion = suggest_name(name, env.value_names().into_iter());
                    Error::VariableNotFound(
                        env.module_name().qualify_name(name),
                        e.span,
                        suggestion,
                    )
                })? {
                    ValueType::Local => ExpressionKind::VarLocal(name.clone()),
                    ValueType::TopLevel => {
                        ExpressionKind::VarTopLevel(env.module_name().qualify_name(name))
                    }
                    // Named by the package and module that declared the value, `m`. The
                    // written spelling is bare for an exposed value and carries a module
                    // or an alias for a qualified one (`Js.Basics.add`), and only its
                    // last segment is the value's own name.
                    ValueType::Foreign(m, _source, tpe, _origin) => {
                        let declared = name.last_segment();
                        ExpressionKind::VarForeign(
                            m.qualify_name(&declared),
                            m.package().clone(),
                            tpe.clone(),
                        )
                    }
                    ValueType::Foreigns(candidates) => {
                        return Err(Error::AmbiguousVariables(
                            name.clone(),
                            candidates.clone(),
                            e.span,
                        ))
                    }
                }
            }
            parser::ExpressionKind::TypeConstructor(name) => {
                let ctor = env.find_type_constructor(name).ok_or_else(|| {
                    let suggestion = suggest_name(name, env.type_constructor_names().into_iter());
                    Error::VariantNotFound(env.module_name().qualify_name(name), e.span, suggestion)
                })?;

                let tpe = if ctor.type_parameters.is_empty() {
                    Type::Type(ctor.tpe.clone(), vec![])
                } else {
                    // TODO Rework that part. ctor.types is only for the type parameters of the constructor, not for the overall type.
                    let mut iter = ctor.type_parameters.iter().rev();
                    let first = iter.next().unwrap().clone();

                    // TODO tests this, 99.999% I'm wrong about it (like everytime I try to implement arrows, plus
                    // the foldr in this particular case)
                    let tpe = iter.fold(first, |acc, t| {
                        Type::Arrow(Box::new(t.clone()), Box::new(acc))
                    });

                    Type::Arrow(
                        Box::new(Type::Type(ctor.tpe.clone(), vec![])),
                        Box::new(tpe),
                    )
                };

                // Named by the package and module that declared the union, the way
                // `VarForeign` is named by the ones that declared the value — not by the
                // spelling the source wrote, which is bare for an exposed constructor and
                // carries the importer's alias or a dependency's namespace for a
                // qualified one.
                let name = ctor.tpe.sibling(&ctor.name);

                ExpressionKind::VarConstructor(name, tpe)
            }
            parser::ExpressionKind::Application(a, b) => {
                let a = Expression::from_parser(a, env)?;
                let b = Expression::from_parser(b, env)?;

                ExpressionKind::Apply(Box::new(a), Box::new(b))
            }
            parser::ExpressionKind::InfixChain(first, rest) => {
                let first = Expression::from_parser(first, env)?;

                let rest = rest
                    .iter()
                    .map(|(op_name, op_span, operand)| {
                        let op = resolve_infix_operator(op_name, *op_span, env)?;
                        let operand = Expression::from_parser(operand, env)?;
                        Ok((op, operand))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                let mut rest = rest.into_iter().peekable();
                let reassociated = reassociate_infix_chain(first, &mut rest, 0)?;

                reassociated.kind
            }
            parser::ExpressionKind::Tuple(tuple) => {
                ExpressionKind::Tuple(tuple.try_map(|e| Expression::from_parser(e, env))?)
            }
            parser::ExpressionKind::Unit => ExpressionKind::Unit,
            parser::ExpressionKind::Case(expr, branches) => {
                let expr = Expression::from_parser(expr, env)?;

                let b = branches.iter().map::<Result<CaseBranch, Error>, _>(|cb| {
                    let pattern = Pattern::from_parser(&cb.pattern, env)?;
                    let mut scoped = env.new_scope();

                    scoped.expose_pattern(&pattern);

                    let expression = Expression::from_parser(&cb.expression, &scoped)?;

                    Ok(CaseBranch {
                        pattern,
                        expression,
                        span: cb.span,
                    })
                });

                let branches = collect_accumulate(b)?;

                ExpressionKind::Case(Box::new(expr), branches)
            }
            parser::ExpressionKind::If(cond, then, els) => {
                let cond = Expression::from_parser(cond, env)?;
                let then = Expression::from_parser(then, env)?;
                let els = Expression::from_parser(els, env)?;

                ExpressionKind::If(Box::new(cond), Box::new(then), Box::new(els))
            }
        };

        Ok(Expression::new(e.span, kind))
    }
}

/// One operator of an `ExpressionKind::InfixChain`, resolved against the infix
/// environment: its own `Infix` (precedence, associativity, and where it was
/// declared) alongside the canonical `Expression` it resolves to as a value —
/// a reference to the function its `infix` declaration names, qualified with the
/// module that wrote that declaration. `reassociate_infix_chain` consults `infix`
/// to decide how to nest, and `span` and `expr` to build the invented
/// `Application` nodes.
struct ResolvedInfixOp {
    name: Name,
    /// Where the operator itself was written — not its `infix` declaration.
    span: NodeSpan,
    expr: Expression,
    infix: Infix,
    /// Where the `infix` declaration was written, and in which module's file —
    /// the operator may well have been declared by an imported module, in which
    /// case `infix.span` is a byte range in *that* module's source. See
    /// [`InfixDeclaration`].
    declaration: InfixDeclaration,
}

/// Resolves one operator of an `InfixChain` into the function its `infix`
/// declaration names, plus the `Infix` re-association needs.
///
/// The `infixes` map is the whole of an operator's scope: nothing else can hold
/// a key spelled like one, since a function's own name always comes from
/// `VarIdent`, a distinct token from `Op`. So `find_infix` failing is exactly
/// "no operator by that name is in scope", and gets the `VariableNotFound` a
/// reader would expect for a name they wrote and nothing declares.
///
/// The function behind the operator is resolved from the entry
/// ([`InfixFunction`]) rather than looked up in the scope the operator is *used*
/// in. An operator has no qualified spelling, so naming it in an `exposing` list
/// is the only way to reach one across a module boundary, and whether the
/// exporting module's backing function is separately in scope is neither the
/// importer's choice nor visible to them. The resulting `VarTopLevel`/`VarForeign`
/// is qualified with the module that wrote the `infix` declaration and names the
/// function, not the operator symbol, so it matches the binding a later phase
/// looks it up against.
fn resolve_infix_operator(
    name: &Name,
    span: NodeSpan,
    env: &dyn Environment,
) -> Result<ResolvedInfixOp, Error> {
    let entry = env.find_infix(name).cloned().ok_or_else(|| {
        let suggestion = suggest_name(name, env.value_names().into_iter());
        Error::VariableNotFound(env.module_name().qualify_name(name), span, suggestion)
    })?;

    let function_name = &entry.infix.function_name;
    let kind = match &entry.function {
        InfixFunction::Local => {
            ExpressionKind::VarTopLevel(env.module_name().qualify_name(function_name))
        }
        InfixFunction::Imported(module, tpe) => ExpressionKind::VarForeign(
            module.qualify_name(function_name),
            module.package().clone(),
            tpe.clone(),
        ),
        // The exporting module declared the backing function without an
        // annotation, so its interface carries no type for it. The name that
        // failed to resolve is the function's, and that is what is reported —
        // the operator is in scope, and saying otherwise would send the reader
        // to check their own `import` line.
        InfixFunction::ImportedUntyped(module) => {
            return Err(Error::VariableNotFound(
                module.qualify_name(function_name),
                span,
                None,
            ))
        }
    };

    Ok(ResolvedInfixOp {
        name: name.clone(),
        span,
        expr: Expression::new(span, kind),
        infix: entry.infix,
        declaration: entry.declaration,
    })
}

/// Re-associates a flat run of operator applications into a tree of
/// `Application` nodes, in one pass over the operators — no operator or operand
/// is visited twice — using precedence climbing (a standard algorithm; see e.g.
/// https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method).
///
/// `min_prec` is the lowest precedence this call is willing to fold into `lhs`;
/// the outer, top-level call passes 0, admitting every operator. A recursive
/// call raises it to bind only the tighter operators that belong on the right
/// of the operator being folded — `op.infix.precedence + 1` for strictly higher
/// precedence, or `op.infix.precedence` itself when the next operator ties and
/// both associate right, which is what lets a right-associative run keep
/// folding at the same level (`a <| b <| c` → `a <| (b <| c)`).
///
/// Two adjacent operators of equal precedence are rejected rather than guessed
/// at unless they agree — both `left` (fold `lhs`, the usual case) or both
/// `right` (recurse into `rhs`). Anything else — a `left` against a `right`, or
/// either against an `infix non` operator, including one against itself — is
/// `Error::AmbiguousOperatorPrecedence`: nothing here says which one should
/// bind first, and guessing would silently pick a grouping the user did not
/// write.
fn reassociate_infix_chain(
    mut lhs: Expression,
    rest: &mut std::iter::Peekable<impl Iterator<Item = (ResolvedInfixOp, Expression)>>,
    min_prec: u8,
) -> Result<Expression, Error> {
    while let Some((op, mut rhs)) = rest.next_if(|(op, _)| op.infix.precedence >= min_prec) {
        loop {
            let next_min = match rest.peek() {
                None => None,
                Some((next_op, _)) => {
                    if next_op.infix.precedence > op.infix.precedence {
                        Some(op.infix.precedence + 1)
                    } else if next_op.infix.precedence == op.infix.precedence {
                        match (op.infix.associativity, next_op.infix.associativity) {
                            (Associativity::Left, Associativity::Left) => None,
                            (Associativity::Right, Associativity::Right) => {
                                Some(op.infix.precedence)
                            }
                            _ => {
                                return Err(Error::AmbiguousOperatorPrecedence(
                                    AmbiguousOperator::new(&op),
                                    AmbiguousOperator::new(next_op),
                                    op.span.merge(next_op.span),
                                ))
                            }
                        }
                    } else {
                        None
                    }
                }
            };

            match next_min {
                Some(min) => rhs = reassociate_infix_chain(rhs, rest, min)?,
                None => break,
            }
        }

        lhs = apply_infix(op, lhs, rhs);
    }

    Ok(lhs)
}

/// Builds the two `Application` nodes one step of infix re-association adds,
/// following the same span convention the grammar's right-recursive rewrite
/// used before this: the partial application of the operator to `lhs` — a node
/// with nothing in the user's text of its own — takes the operator's own span,
/// and the outer application, which stands for `lhs op rhs` as the user wrote
/// it, spans from wherever `lhs` starts to wherever `rhs` ends.
fn apply_infix(op: ResolvedInfixOp, lhs: Expression, rhs: Expression) -> Expression {
    let span = lhs.span.merge(rhs.span);

    let partial = Expression::new(
        op.span,
        ExpressionKind::Apply(Box::new(op.expr), Box::new(lhs)),
    );

    Expression::new(
        span,
        ExpressionKind::Apply(Box::new(partial), Box::new(rhs)),
    )
}

/// One side of an [`Error::AmbiguousOperatorPrecedence`]: the operator as the user
/// spelled it, how its `infix` declaration said it associates, and where that
/// declaration was written.
///
/// `associativity` is carried rather than re-derived because the message depends on
/// it — an `infix non` operator does not chain at all, where a `left` against a
/// `right` is a choice the user can make with parentheses — and the error outlives
/// the environment the declaration was looked up in.
#[derive(Debug)]
pub struct AmbiguousOperator {
    pub name: Name,
    pub associativity: Associativity,
    pub declaration: InfixDeclaration,
}

impl AmbiguousOperator {
    fn new(op: &ResolvedInfixOp) -> Self {
        AmbiguousOperator {
            name: op.name.clone(),
            associativity: op.infix.associativity,
            declaration: op.declaration,
        }
    }

    fn is_non_associative(&self) -> bool {
        self.associativity == Associativity::None
    }
}

#[derive(Debug, PartialEq)]
pub struct CaseBranch {
    pub pattern: Pattern,
    pub expression: Expression,
    /// The whole branch, pattern and expression together.
    pub span: NodeSpan,
}

// end AST

/// Everything canonicalization can reject.
///
/// # Why only some variants carry a span
///
/// A variant carries a [`NodeSpan`] when its construction site has one in hand — it
/// is looking at a `parser::Import`, `parser::Infix`, `parser::UnionType`,
/// `parser::Function` or `parser::Exposed`, all of which the grammar gives a span.
/// Those are the variants a diagnostic can put a caret under, and today that is every
/// variant below except three: the two group variants, `EnvironmentErrors` and
/// `Many`, have no position of their own and flatten their members' labels instead;
/// and `InvalidTupleSize` carries none for an unrelated reason — see its own doc
/// comment. Writing `NodeSpan::none()` into a variant that lacks a real position
/// would say "this error has a position we happen not to know", which is a lie;
/// leaving the field off says "this error has nowhere to point", which is true, and
/// the reporter renders it as message-plus-notes with no caret.
#[derive(Debug)]
pub enum Error {
    /// A name in the `module … exposing (…)` header that nothing in the module
    /// declares, and where that name was written (`ERR-9`).
    ExportNotFound(Name, ExportType, NodeSpan),
    /// A value exposed by this module — named explicitly in its `exposing` list,
    /// or implicitly by `exposing (..)` — whose declaration carries no type
    /// annotation (`SPEC-5`: an exposed declaration must be annotated).
    ///
    /// The first [`NodeSpan`] is where the `exposing` list names it — a real
    /// position for an explicit list, `NodeSpan::none()` for `exposing (..)`,
    /// which names nothing individually and so has no per-value span to point
    /// at (`parser::Exposing::Open` carries none at all). The second is the
    /// declaration's own span, always real: `labels()` falls back to it as the
    /// primary label when the first is absent, rather than pointing nowhere.
    ExportedValueNotAnnotated(Name, NodeSpan, NodeSpan),
    EnvironmentErrors(Vec<EnvError>),
    /// (infix, function), and where the `infix` declaration was written
    InfixReferenceInvalidValue(Name, Name, NodeSpan),
    /// Infix re-association (`reassociate_infix_chain`) found two adjacent
    /// operators of equal precedence that do not agree how to group: the left
    /// one, the right one, and the ambiguous pair's own span — from the left
    /// operator through the right one.
    ///
    /// Three shapes reach this, and [`PhaseError::message`] says which:
    /// `left.name == right.name` is one `infix non` operator chained with
    /// itself (`a == b == c`); either side being `infix non` against a
    /// different operator is a non-associative operator that does not chain at
    /// all (`a < b > c`); and a `left` against a `right` is the genuine
    /// disagreement about which side groups first (`a << b >> c`).
    AmbiguousOperatorPrecedence(AmbiguousOperator, AmbiguousOperator, NodeSpan),
    BindingPatternsInvalidLen(NodeSpan),
    /// A declaration with a type annotation and no body, and where the annotation
    /// was written — which is the only part of it there is to point at.
    NoBindings(NodeSpan),
    /// A name used as a value that nothing in scope declares, where it was
    /// written — the identifier alone, not the declaration around it — and,
    /// when one name in scope is a close enough typo-distance match, a
    /// suggestion for what was meant (`ERR-7`).
    VariableNotFound(QualName, NodeSpan, Option<Name>),
    /// A name exposed unqualified by more than one imported module, and where it
    /// was used. Each candidate module is paired with where the name is declared
    /// there — `Some` when that module's `Interface` knows both its file and the
    /// declaration's span, `None` for a hand-built interface (a test) or one built
    /// before its module's file was known (`ERR-5`) — and with whether that import
    /// was written by the module under check or supplied by [the default import
    /// list](../../docs/spec/modules.md#the-default-imports), which a default
    /// entry participates in exactly as a written import does
    /// (`SPEC-32`/[`DEC-15`](../../docs/decisions/dec-15.md)); the note calls out
    /// a default contributor as implicit.
    AmbiguousVariables(
        Name,
        Vec<(ModuleName, Option<super::SourceSpan>, ImportOrigin)>,
        NodeSpan,
    ),
    /// A constructor used in an expression or a pattern that nothing in scope
    /// declares, where it was written, and an optional "did you mean …?"
    /// suggestion (`ERR-7`).
    VariantNotFound(QualName, NodeSpan, Option<Name>),
    /// Nothing constructs this today — `Environment::find_type_constructor` returns
    /// at most one constructor per name, so it has no way to report an ambiguity.
    /// It is the designated rejection path once it can, and carries the span the
    /// construction site would have, alongside each candidate's declaration
    /// location — see [`Error::AmbiguousVariables`].
    AmbiguousVariants(
        Name,
        Vec<(ModuleName, Option<super::SourceSpan>, ImportOrigin)>,
        NodeSpan,
    ),
    /// A tuple type, pattern or expression had a size other than 2 or 3 (the
    /// only sizes the language supports).
    ///
    /// Nothing constructs this today, and it is kept deliberately. Both ASTs
    /// now hold their tuples in [`Tuple`], which cannot represent another
    /// arity, and the grammar has one production per arity — so a bad tuple is
    /// a parse error and never reaches canonicalization. This variant is the
    /// designated rejection path should a future source of tuples (a REPL, a
    /// desugaring pass) build one from a list.
    InvalidTupleSize(usize),
    /// A function was declared with multiple bindings (multi-clause definitions),
    /// which the compiler does not support yet.
    MultipleBindingsUnsupported(Name, NodeSpan),
    /// A type name applied to the wrong number of arguments: the name, its
    /// declaration's own arity, the number of arguments actually written, and
    /// `tpe.span` — the whole application, so the caret covers every argument
    /// along with the name (`BUG-17`).
    TypeArityMismatch(Name, usize, usize, NodeSpan),
    /// A type name written in a type expression that nothing in scope declares:
    /// the name exactly as it was written, and `tpe.span` — the application, so
    /// the caret covers the name along with whatever it was applied to.
    ///
    /// The declarations of the module under check are in scope here as much as
    /// its imports are, so this is a name neither half offers. Only the
    /// `TypeKind::Unqualified` arm of `Type::from_parser_type` raises it: a
    /// type *variable* arrives as `TypeKind::Variable` and is bound by the
    /// annotation it appears in, so it resolves through nothing and cannot
    /// reach here.
    TypeNotFound(Name, NodeSpan),
    /// Something other than a constructor name and its arguments written in a
    /// `type` declaration's variant position: what was written there, and that
    /// variant's own span rather than the declaration's, so the caret sits under
    /// the offending variant alone (`BUG-18`).
    InvalidVariant(InvalidVariantKind, NodeSpan),
    /// An opaque scalar's `type` declaration — `Basics.Int`, `Basics.Float`,
    /// `Char.Char` or `String.String` — whose body is something other than
    /// exactly its own name with no arguments (`LANG-59`,
    /// [`DEC-15` decision
    /// 2](../../docs/decisions/dec-15.md#2--a-scalar-type-is-declared-in-zelkova-and-an-opaque-ones-declaration-names-itself)).
    /// Nothing in the language constructs or inspects a value of one of these
    /// four, so the declaration exists to be read rather than built from, and a
    /// body naming anything else — a different type, extra variants, arguments —
    /// would describe a representation the language does not have.
    ///
    /// Carries the declaration's qualified name and the whole declaration's span
    /// (`tpe.span`), the same position [`Error::InvalidVariant`] would rather than
    /// a per-variant one — the check runs on the shape of the whole variant list,
    /// not on one variant that is wrong among otherwise-good ones.
    InvalidScalarDeclaration(QualName, NodeSpan),
    /// Something other than a constraint written in front of an annotation's
    /// `=>`: what was written, and that piece's own span — one constraint of a
    /// parenthesised list rather than the whole context, so the caret sits under
    /// the part that is wrong.
    ///
    /// The grammar parses a context as a type (see `parser::FunType::context`),
    /// so every shape a type can take arrives here; `validate_context` is what
    /// narrows it to one constraint or a tuple of them.
    InvalidConstraint(InvalidConstraintKind, NodeSpan),

    // Binding module
    InfixDeclared(Name, NodeSpan),
    TypeDeclared(Name, NodeSpan),
    NoTypeInBinding(Name, NodeSpan),
    /// An annotation outside a `module foreign` facade was marked `unsafe`: the
    /// name it annotates, and the annotation's span — which the grammar takes
    /// from the modifier, so the caret starts on the word itself.
    ///
    /// `unsafe` asserts something about the companion behind a facade signature
    /// (`LANG-53`, [`DEC-12`](../../docs/decisions/dec-12.md)). There is no
    /// companion behind an ordinary declaration for it to be a claim about, and
    /// accepting the word there would make it mean nothing in half the places it
    /// can be written.
    UnsafeOutsideFacade(Name, NodeSpan),
    /// A `module foreign` facade signature naming a type variable or a
    /// function type, in a parameter or in the result, once the arrows
    /// separating a facade's own parameters are stripped
    /// (`docs/spec/interop.md#what-a-facade-signature-may-not-name`): the
    /// value's name, which form was found, and `function.annotation_span`.
    ///
    /// The span is the whole annotation rather than the offending piece of it,
    /// because a canonical `Type` carries no span of its own — see
    /// `Type::from_parser_type`'s doc comment — and the annotation is the
    /// finest caret available without first teaching that conversion to keep
    /// per-node spans.
    FacadeTypeNotAdmitted(Name, FacadeRejectedKind, NodeSpan),
    /// An unmarked `module foreign` facade signature whose result type — the
    /// last piece `facade_signature_pieces` returns, once
    /// `check_facade_admitted_type` has already accepted it — is not exactly
    /// `Task (Result Failure a)`: the value's name, and `function.annotation_span`.
    ///
    /// [An effectful facade](../../docs/spec/interop.md#an-effectful-facade)
    /// settles the shape: a facade declares an effect unless its signature is
    /// marked [`unsafe`](../../docs/spec/interop.md#an-unsafe-facade), and an
    /// effectful facade's result type must be exactly `Task (Result Failure a)`.
    /// Never raised for a signature `function.marked_unsafe` — `unsafe` removes
    /// the requirement, not the boundary check itself.
    ///
    /// The span is the whole annotation rather than the offending piece of it,
    /// for the same reason [`Error::FacadeTypeNotAdmitted`] uses it — a
    /// canonical `Type` carries no span of its own.
    FacadeResultNotEffect(Name, NodeSpan),
    /// `Task` named anywhere in a `module foreign` facade signature other than
    /// the whole of an unmarked facade's result: the value's name, and
    /// `function.annotation_span`.
    ///
    /// [An effectful facade](../../docs/spec/interop.md#an-effectful-facade)
    /// confines `Task` to that one position — never as an argument, and never
    /// nested inside another type, whether or not the signature is marked
    /// [`unsafe`](../../docs/spec/interop.md#an-unsafe-facade). `Task` is
    /// recognised by the qualified name of its declaration
    /// (`is_task_declaration`) and never by spelling, so a module's own type
    /// named `Task` is an ordinary union here, not this.
    ///
    /// The span is the whole annotation, as [`Error::FacadeTypeNotAdmitted`]'s
    /// is.
    FacadeTaskMisplaced(Name, NodeSpan),
    /// A `module foreign` facade signature written with a constraint context:
    /// the value's name, and the context's span.
    ///
    /// A facade is monomorphic — its signature names the types the code behind
    /// it really handles ([Type
    /// classes](../../docs/spec/type-classes.md#a-constrained-function-may-not-be-a-foreign-facade),
    /// [`DEC-2` decision 6](../../docs/decisions/dec-2.md)) — and a constrained
    /// function is specialised per instance out of a body a facade does not have.
    /// Reported whatever the context's shape, a well-formed one included.
    FacadeConstrained(Name, NodeSpan),

    /// A parameterless binding that depends on itself — [evaluation
    /// semantics](../../docs/spec/evaluation-semantics.md#a-binding-may-not-depend-on-itself):
    /// such a binding is evaluated once, before the program runs, after everything it
    /// depends on, and a cycle through it describes no such order. *Depends on* is
    /// transitive mention: the declarations its body mentions, the ones *their* bodies
    /// mention, and so on, through functions as well as parameterless bindings — see
    /// `dependency_graph`.
    ///
    /// One entry per member of the cycle — every declaration of one strongly-connected
    /// component of that graph — each carrying its own declaration's span
    /// (`Value::span()`) and whether it is a function. Length 1 for a parameterless
    /// binding that mentions itself (`x = x`), length 2 or more for a cycle running
    /// through several declarations (`a = b` beside `b = a`, or `a = f 1` beside `f x =
    /// a`). `check_self_dependency` orders the members parameterless bindings first,
    /// then functions, each group by name, so the first entry is always a parameterless
    /// binding and the order is not an artifact of traversal. `labels()` gives that first
    /// entry the primary label and every other entry a secondary one.
    ///
    /// A cycle of functions only is never reported: a function's value exists before its
    /// body runs, so `f n = f n` and mutual recursion between two functions are untouched
    /// by this check.
    SelfDependency(Vec<CycleMember>),

    // Utility error
    Many(Vec<Error>),
}

/// One declaration of a cycle [`Error::SelfDependency`] reports.
#[derive(Debug, Clone)]
pub struct CycleMember {
    pub name: Name,
    /// The whole declaration, as `Value::span()` gives it.
    pub span: NodeSpan,
    /// Whether the declaration names a parameter. A cycle is only ever reported when it
    /// holds at least one member for which this is `false`.
    pub function: bool,
}

/// What was written where a `type` declaration expected a variant — see
/// [`Error::InvalidVariant`].
///
/// A variant is a constructor name followed by zero or more type arguments, and
/// nothing else. The grammar does not enforce that: it parses a variant list with
/// the general `Type` production, so every shape a type expression can take reaches
/// `do_types`. This enum names the four that are not a variant, one per remaining
/// [`parser::TypeKind`], so each can say what it is in the words of the source.
#[derive(Debug, PartialEq, Clone)]
pub enum InvalidVariantKind {
    /// A name beginning with a lowercase letter, which the grammar reads as a type
    /// variable — `type Colour = red`, and a mistyped constructor name along with
    /// it. Carries the name so the message can quote it.
    LowercaseName(Name),
    /// A tuple type — `type Pair = (Int, Int)`.
    Tuple,
    /// The unit type — `type Nothing = ()`.
    Unit,
    /// A function type — `type Wrapper = Wrap Int -> Int`. The arrow is the *whole*
    /// variant rather than a suffix of it, `Wrap Int` being its left operand, so
    /// there is no constructor here to keep either.
    Arrow,
}

/// What was written in front of `=>` in place of a constraint — see
/// [`Error::InvalidConstraint`].
///
/// A constraint is an uppercase name applied to one or more type arguments, and a
/// context is one constraint or a parenthesised, comma-separated list of them,
/// which the grammar reads as a tuple type. This names every other shape, one per
/// remaining [`parser::TypeKind`] case.
#[derive(Debug, PartialEq, Clone)]
pub enum InvalidConstraintKind {
    /// A type variable on its own — `a => a`.
    Variable(Name),
    /// An uppercase name with no argument — `Int => a`, and so each half of
    /// `(Int, Char) => a`. Carries the name so the message can quote it.
    Unapplied(Name),
    /// A function type — `Int -> Int => a`.
    Arrow,
    /// A tuple inside the parenthesised list — `((Eq a, Eq b), Eq c) => a`. The
    /// outermost tuple *is* the list, so only one nested in it reaches here.
    Tuple,
    /// The unit type — `() => a`.
    Unit,
}

/// The two forms [What a facade signature may not
/// name](../../docs/spec/interop.md#what-a-facade-signature-may-not-name)
/// rejects — see [`Error::FacadeTypeNotAdmitted`].
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FacadeRejectedKind {
    /// A type variable, which excludes no value: there is nothing for a
    /// runtime predicate to decide.
    Variable,
    /// A function type, wherever it is found — as the signature's own result
    /// or nested inside a parameter, a tuple or a union's argument.
    Function,
}

/// Canonicalization errors name source constructs — a value, a type, an operator —
/// so their messages can be written in the same words the user wrote.
///
/// The variants whose construction site had a declaration in hand also point at it;
/// see the enum's own documentation for why the others do not.
impl PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::ExportNotFound(name, tpe, _) => format!(
                "`{}` is exposed by this module but no {} of that name is declared in it",
                name,
                export_type_noun(tpe)
            ),
            Error::ExportedValueNotAnnotated(name, _, _) => format!(
                "`{}` is exposed by this module but has no type annotation",
                name
            ),
            Error::EnvironmentErrors(errors) => match errors.as_slice() {
                [only] => only.message(),
                many => format!("{} of this module's imports could not be resolved", many.len()),
            },
            Error::InfixReferenceInvalidValue(infix, function, _) => format!(
                "the infix operator `{}` is declared as `{}`, which is not a value declared in this module",
                infix, function
            ),
            Error::AmbiguousOperatorPrecedence(left, right, _) => {
                // Three shapes, and the everyday one is the middle: `Basics`
                // declares six operators `infix non 4`, so `a < b > c` reaches
                // here with two *different* operators that do not disagree about
                // anything — both say they do not chain. Calling that a
                // disagreement about which side groups first would name a reason
                // that is not the reason.
                if left.name == right.name {
                    format!(
                        "`{}` is declared `infix non`, so it cannot be chained with itself without parentheses to say which application comes first",
                        left.name
                    )
                } else if left.is_non_associative() && right.is_non_associative() {
                    format!(
                        "`{}` and `{}` are both declared `infix non` at the same precedence, so neither groups the other and this needs parentheses",
                        left.name, right.name
                    )
                } else if left.is_non_associative() || right.is_non_associative() {
                    let (non, other) = if left.is_non_associative() {
                        (&left.name, &right.name)
                    } else {
                        (&right.name, &left.name)
                    };

                    format!(
                        "`{}` is declared `infix non`, so it does not chain with `{}` at the same precedence without parentheses",
                        non, other
                    )
                } else {
                    format!(
                        "`{}` and `{}` have the same precedence but disagree on which side groups first, so this needs parentheses to say which one applies first",
                        left.name, right.name
                    )
                }
            }
            Error::BindingPatternsInvalidLen(_) => {
                "the arguments of this declaration do not line up with its type annotation"
                    .to_owned()
            }
            Error::NoBindings(_) => {
                "this declaration has a type annotation but no body".to_owned()
            }
            Error::VariableNotFound(name, _, _) => {
                format!("cannot find a value named `{}`", name.to_name())
            }
            Error::AmbiguousVariables(name, _, _) => {
                format!("`{}` is exposed by several imported modules", name)
            }
            Error::VariantNotFound(name, _, _) => {
                format!("cannot find a type constructor named `{}`", name.to_name())
            }
            Error::AmbiguousVariants(name, _, _) => format!(
                "the type constructor `{}` is exposed by several imported modules",
                name
            ),
            Error::InvalidTupleSize(size) => format!(
                "a tuple has two or three elements, this one has {}",
                size
            ),
            Error::MultipleBindingsUnsupported(name, _) => format!(
                "`{}` is declared over several bindings, which is not supported yet",
                name
            ),
            Error::TypeArityMismatch(name, declared, written, _) => format!(
                "`{}` takes {}, but is applied to {} here",
                name,
                type_argument_count(*declared),
                type_argument_count(*written)
            ),
            Error::TypeNotFound(name, _) => {
                format!("cannot find a type named `{}`", name)
            }
            Error::InvalidVariant(kind, _) => match kind {
                InvalidVariantKind::LowercaseName(name) => format!(
                    "`{}` is not a constructor name: a constructor name begins with an uppercase letter",
                    name
                ),
                InvalidVariantKind::Tuple => {
                    "a variant is a constructor name followed by its arguments, and this one is a tuple type"
                        .to_owned()
                }
                InvalidVariantKind::Unit => {
                    "a variant is a constructor name followed by its arguments, and this one is the unit type"
                        .to_owned()
                }
                InvalidVariantKind::Arrow => {
                    "a variant is a constructor name followed by its arguments, and this one is a function type"
                        .to_owned()
                }
            },
            Error::InvalidScalarDeclaration(name, _) => format!(
                "`{}` is an opaque scalar type, so its declaration must be exactly `type {} = {}`",
                name.to_name(),
                name.unqualified_name(),
                name.unqualified_name()
            ),
            Error::InvalidConstraint(kind, _) => match kind {
                InvalidConstraintKind::Variable(name) => format!(
                    "only constraints may be written before `=>`, and the type variable `{}` is not one",
                    name
                ),
                InvalidConstraintKind::Unapplied(name) => format!(
                    "only constraints may be written before `=>`, and `{}` on its own is not one",
                    name
                ),
                InvalidConstraintKind::Arrow => {
                    "only constraints may be written before `=>`, and a function type is not one"
                        .to_owned()
                }
                InvalidConstraintKind::Tuple => {
                    "only constraints may be written before `=>`, and a tuple inside the list of constraints is not one"
                        .to_owned()
                }
                InvalidConstraintKind::Unit => {
                    "only constraints may be written before `=>`, and the unit type is not one"
                        .to_owned()
                }
            },
            Error::InfixDeclared(name, _) => format!(
                "a `module foreign` facade cannot declare an infix operator, but declares `{}`",
                name
            ),
            Error::TypeDeclared(name, _) => format!(
                "a `module foreign` facade cannot declare a type, but declares `{}`",
                name
            ),
            Error::NoTypeInBinding(name, _) => format!(
                "`{}` has no type annotation, and a `module foreign` facade is annotations only",
                name
            ),
            Error::UnsafeOutsideFacade(name, _) => format!(
                "`{}` is marked `unsafe`, which only a signature in a `module foreign` facade may be",
                name
            ),
            Error::FacadeTypeNotAdmitted(name, kind, _) => format!(
                "`{}` is a `module foreign` facade signature naming {}, which no target can check at the boundary",
                name,
                match kind {
                    FacadeRejectedKind::Variable => "a type variable",
                    FacadeRejectedKind::Function => "a function type",
                }
            ),
            Error::FacadeResultNotEffect(name, _) => format!(
                "`{}` is a `module foreign` facade signature with no `unsafe`, so it must return `Task (Result Failure a)`",
                name
            ),
            Error::FacadeTaskMisplaced(name, _) => format!(
                "`{}` names `Task` somewhere other than the whole of an unmarked facade's result",
                name
            ),
            Error::FacadeConstrained(name, _) => format!(
                "`{}` is a `module foreign` facade signature, and a facade signature may not carry a constraint",
                name
            ),
            // A one-binding cycle reads better as its own sentence than as "a
            // cycle of length one" — see the enum's own doc comment.
            // A larger cycle may hold functions, so it is described by its first member —
            // always a parameterless binding — and every member is named for what it is.
            Error::SelfDependency(members) => match members.as_slice() {
                [only] => format!(
                    "`{}` is a parameterless binding whose value depends on itself",
                    only.name
                ),
                [first, ..] => {
                    let mut named: Vec<String> = members
                        .iter()
                        .map(|member| {
                            if member.function {
                                format!("the function `{}`", member.name)
                            } else {
                                format!("`{}`", member.name)
                            }
                        })
                        .collect();
                    let last = named.pop().unwrap_or_default();
                    format!(
                        "`{}` needs its own value before it has one: {} and {} depend on each other",
                        first.name,
                        named.join(", "),
                        last
                    )
                }
                [] => "a parameterless binding depends on itself".to_owned(),
            },
            Error::Many(errors) => match errors.as_slice() {
                [only] => only.message(),
                many => format!("{} errors while canonicalizing this module", many.len()),
            },
        }
    }

    fn labels(&self) -> Vec<SpanLabel> {
        // One helper per shape: a variant either has a span and one thing to say
        // about it, or it delegates to the errors it wraps.
        let primary = |span: &NodeSpan, message: &str| match span.span() {
            Some(span) => vec![SpanLabel {
                span,
                message: message.to_owned(),
                primary: true,
                file: None,
            }],
            None => Vec::new(),
        };

        // A secondary label pointing at where one ambiguous candidate is declared,
        // in *its own* module's file rather than the one being checked — the
        // mechanism `ERR-5` adds. `None` when that candidate's `Interface` cannot
        // say (a hand-built interface, or one built before its file was known)
        // yields no label for that candidate rather than one at the wrong place.
        let ambiguous_candidates =
            |candidates: &[(ModuleName, Option<super::SourceSpan>, ImportOrigin)]| {
                candidates
                    .iter()
                    .filter_map(|(module, source, _origin)| {
                        source.map(|s| SpanLabel {
                            span: s.span,
                            message: format!("also exposed here, by `{}`", module.name()),
                            primary: false,
                            file: Some(s.file),
                        })
                    })
                    .collect::<Vec<_>>()
            };

        match self {
            Error::ExportNotFound(name, _, span) => primary(
                span,
                &format!("`{}` is not declared anywhere in this module", name),
            ),
            Error::ExportedValueNotAnnotated(name, exposed_span, declared_span) => {
                let mut labels = primary(
                    exposed_span,
                    &format!("`{}` is exposed here but has no type annotation", name),
                );
                if labels.is_empty() {
                    // Reached only for `exposing (..)`: it names nothing
                    // individually, so there is no exposing-list position to put
                    // the primary label under — the declaration becomes the
                    // primary (and only) label instead of a secondary one.
                    labels = primary(
                        declared_span,
                        &format!(
                            "`{}` is exposed by `exposing (..)` but has no type annotation",
                            name
                        ),
                    );
                } else if let Some(span) = declared_span.span() {
                    labels.push(SpanLabel {
                        span,
                        message: "declared here, with no type annotation".to_owned(),
                        primary: false,
                        file: None,
                    });
                }
                labels
            }
            Error::InfixReferenceInvalidValue(_, _, span) => primary(span, "declared here"),
            // The two "declared here" labels go through `InfixDeclaration`, which
            // is what knows whether the declaration is in the module under check
            // or in an imported one. An operator's `infix` declaration is very
            // often not local — `Basics` declares every one the standard library
            // uses — and `Infix::span` is then a byte range in *that* module's
            // file, so a label built with `file: None` would underline unrelated
            // text in the importing module (`ERR-5` is the mechanism that avoids
            // it).
            Error::AmbiguousOperatorPrecedence(left, right, span) => {
                let mut labels = primary(span, "ambiguous without parentheses");

                labels.extend(
                    left.declaration
                        .label(format!("`{}` declared here", left.name)),
                );

                // Both sides naming the same operator is the `infix non`
                // self-conflict — they point at the very same declaration, so a
                // second label there would only repeat the first one.
                if left.name != right.name {
                    labels.extend(
                        right
                            .declaration
                            .label(format!("`{}` declared here", right.name)),
                    );
                }

                labels
            }
            // The four that name an identifier: the caret sits under the name the
            // user wrote, which is the whole point of spanning expressions and
            // patterns rather than only declarations. `VariableNotFound` and
            // `VariantNotFound` append their suggestion, when they have one, to
            // this same label rather than a free-floating note, so the caret and
            // the suggestion agree about which name is meant (`ERR-7`).
            Error::VariableNotFound(_, span, suggestion) => primary(
                span,
                &format!(
                    "no value of this name is in scope{}",
                    suggestion_suffix(suggestion)
                ),
            ),
            Error::AmbiguousVariables(_, candidates, span) => {
                let mut labels = primary(span, "this name is ambiguous");
                labels.extend(ambiguous_candidates(candidates));
                labels
            }
            Error::VariantNotFound(_, span, suggestion) => primary(
                span,
                &format!(
                    "no type constructor of this name is in scope{}",
                    suggestion_suffix(suggestion)
                ),
            ),
            Error::AmbiguousVariants(_, candidates, span) => {
                let mut labels = primary(span, "this type constructor is ambiguous");
                labels.extend(ambiguous_candidates(candidates));
                labels
            }
            Error::BindingPatternsInvalidLen(span) => primary(span, "declared here"),
            Error::NoBindings(span) => primary(span, "this annotation has no body"),
            Error::MultipleBindingsUnsupported(_, span) => primary(span, "declared here"),
            Error::TypeArityMismatch(name, declared, written, span) => primary(
                span,
                &format!(
                    "`{}` takes {}, this application has {}",
                    name,
                    type_argument_count(*declared),
                    type_argument_count(*written)
                ),
            ),
            Error::TypeNotFound(_, span) => primary(span, "no type of this name is in scope"),
            Error::InvalidVariant(kind, span) => primary(
                span,
                match kind {
                    InvalidVariantKind::LowercaseName(_) => "this begins with a lowercase letter",
                    InvalidVariantKind::Tuple => "a tuple type, written where a variant belongs",
                    InvalidVariantKind::Unit => "the unit type, written where a variant belongs",
                    InvalidVariantKind::Arrow => "a function type, written where a variant belongs",
                },
            ),
            Error::InvalidScalarDeclaration(name, span) => primary(
                span,
                &format!(
                    "an opaque scalar's body must be exactly `{}`, with no other variant and no arguments",
                    name.unqualified_name()
                ),
            ),
            Error::InvalidConstraint(kind, span) => primary(
                span,
                match kind {
                    InvalidConstraintKind::Variable(_) => {
                        "a type variable, written where a constraint belongs"
                    }
                    InvalidConstraintKind::Unapplied(_) => {
                        "a name with no argument, written where a constraint belongs"
                    }
                    InvalidConstraintKind::Arrow => {
                        "a function type, written where a constraint belongs"
                    }
                    InvalidConstraintKind::Tuple => "a tuple, written where a constraint belongs",
                    InvalidConstraintKind::Unit => {
                        "the unit type, written where a constraint belongs"
                    }
                },
            ),
            Error::InfixDeclared(_, span) => primary(span, "declared here"),
            Error::TypeDeclared(_, span) => primary(span, "declared here"),
            Error::NoTypeInBinding(_, span) => primary(span, "declared here"),
            Error::UnsafeOutsideFacade(_, span) => primary(span, "marked `unsafe` here"),
            Error::FacadeTypeNotAdmitted(_, kind, span) => primary(
                span,
                match kind {
                    FacadeRejectedKind::Variable => "this signature names a type variable",
                    FacadeRejectedKind::Function => "this signature names a function type",
                },
            ),
            Error::FacadeResultNotEffect(_, span) => {
                primary(span, "this signature must return `Task (Result Failure a)`")
            }
            Error::FacadeTaskMisplaced(_, span) => primary(
                span,
                "this signature names `Task` outside the one position it may occupy",
            ),
            Error::FacadeConstrained(_, span) => primary(span, "a constraint on a facade signature"),
            Error::SelfDependency(members) => members
                .iter()
                .enumerate()
                .filter_map(|(i, member)| {
                    let name = &member.name;
                    let message = if members.len() == 1 {
                        format!("`{}` refers to its own value here", name)
                    } else if i == 0 {
                        format!(
                            "`{}` depends on itself through the declarations it mentions",
                            name
                        )
                    } else if member.function {
                        format!("the function `{}` is also part of the cycle", name)
                    } else {
                        format!("`{}` is also part of the cycle", name)
                    };

                    member.span.span().map(|s| SpanLabel {
                        span: s,
                        message,
                        primary: i == 0,
                        file: None,
                    })
                })
                .collect(),
            // A group has no position of its own; the errors it swallowed do.
            Error::EnvironmentErrors(errors) => errors.iter().flat_map(|e| e.labels()).collect(),
            Error::Many(errors) => errors.iter().flat_map(|e| e.labels()).collect(),
            _ => Vec::new(),
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Error::UnsafeOutsideFacade(..) => vec![
                "`unsafe` asserts that the companion behind a facade signature is a function of its arguments and that it returns"
                    .to_owned(),
            ],
            Error::FacadeTypeNotAdmitted(..) => vec![
                "a facade signature may only name a type whose values a target can decide from the value alone, which admits the primitives, tuples and union types applied to admitted types"
                    .to_owned(),
            ],
            Error::InvalidConstraint(..) => vec![
                "a constraint is a class name followed by the type it constrains, as in `Comparable a`, and several are written in parentheses separated by commas, as in `(Comparable k, Eq v)`"
                    .to_owned(),
            ],
            Error::FacadeResultNotEffect(..) => vec![
                "a facade declares an effect unless its signature is marked `unsafe`, and an effectful facade's result type must be exactly `Task (Result Failure a)` — any other result type needs `unsafe` to write"
                    .to_owned(),
            ],
            Error::FacadeTaskMisplaced(..) => vec![
                "`Task` may appear only as the whole of an unmarked facade's result type — not as an argument, and not nested inside another type"
                    .to_owned(),
            ],
            Error::FacadeConstrained(..) => vec![
                "a facade signature names the types the code behind it really handles, so the constraint belongs on an ordinary function that calls the facade at those types"
                    .to_owned(),
            ],
            Error::InvalidScalarDeclaration(..) => vec![
                "nothing in the language constructs or inspects a value of an opaque scalar, so its declaration exists to be read rather than built from"
                    .to_owned(),
            ],
            Error::AmbiguousVariables(_, candidates, _)
            | Error::AmbiguousVariants(_, candidates, _) => vec![ambiguous_note(candidates)],
            // A group renders as a summary, so every message it swallowed becomes a
            // note. A group of one is rendered by its own message and adds nothing.
            Error::EnvironmentErrors(errors) => match errors.as_slice() {
                [only] => only.notes(),
                many => many.iter().flat_map(|e| e.message_and_notes()).collect(),
            },
            Error::Many(errors) => match errors.as_slice() {
                [only] => only.notes(),
                many => many.iter().flat_map(|e| e.message_and_notes()).collect(),
            },
            _ => Vec::new(),
        }
    }
}

/// A "did you mean `X`?" suffix for a label message, when a suggestion was
/// found — empty otherwise, so callers can always append the result without
/// checking `is_some()` first (`ERR-7`). Mirrors the identically-named helper
/// in `environment.rs`; kept separate rather than shared because the two
/// modules' `EnvError`/`Error` types are unrelated and neither should reach
/// into the other for a two-line formatter.
fn suggestion_suffix(suggestion: &Option<Name>) -> String {
    match suggestion {
        Some(name) => format!(" — did you mean `{}`?", name),
        None => String::new(),
    }
}

/// The `it is exposed by: …` note shared by `Error::AmbiguousVariables` and
/// `Error::AmbiguousVariants` — one clause for the contributors the module wrote
/// an `import` for, one for those supplied by [the default import
/// list](../../docs/spec/modules.md#the-default-imports) because it wrote
/// none. Naming the latter as implicit is what removes the surprise a module can
/// otherwise get from colliding with an import it never wrote (`SPEC-32`).
///
/// Every candidate this ever runs over comes from `ValueType::Foreigns`, so
/// `written`/`implicit` are never both empty in practice — `Foreigns` has at
/// least two entries and each is one or the other — but the match is written to
/// say something sensible even if that stopped holding.
fn ambiguous_note(candidates: &[(ModuleName, Option<super::SourceSpan>, ImportOrigin)]) -> String {
    let name_of =
        |(m, _, _): &(ModuleName, Option<super::SourceSpan>, ImportOrigin)| m.name().to_string();
    let written: Vec<String> = candidates
        .iter()
        .filter(|(_, _, origin)| *origin == ImportOrigin::Written)
        .map(name_of)
        .collect();
    let implicit: Vec<String> = candidates
        .iter()
        .filter(|(_, _, origin)| *origin == ImportOrigin::Default)
        .map(name_of)
        .collect();

    match (written.is_empty(), implicit.is_empty()) {
        (false, true) => format!("it is exposed by: {}", written.join(", ")),
        (true, false) => format!("it is exposed implicitly by: {}", implicit.join(", ")),
        (true, true) => "it is exposed by nothing this module can see".to_owned(),
        (false, false) => format!(
            "it is exposed by: {}, and implicitly by {}",
            written.join(", "),
            implicit.join(", ")
        ),
    }
}

/// "1 type argument" or "N type arguments" — the two counts an
/// `Error::TypeArityMismatch` message quotes.
fn type_argument_count(n: usize) -> String {
    if n == 1 {
        "1 type argument".to_owned()
    } else {
        format!("{} type arguments", n)
    }
}

/// How a name is exposed, said in the words the source uses for it.
fn export_type_noun(tpe: &ExportType) -> &'static str {
    match tpe {
        ExportType::Value => "value",
        ExportType::Infix => "infix operator",
        ExportType::UnionPublic | ExportType::UnionPrivate => "type",
    }
}

impl From<Vec<EnvError>> for Error {
    fn from(errors: Vec<EnvError>) -> Self {
        Error::EnvironmentErrors(errors)
    }
}

impl From<Vec<Error>> for Error {
    fn from(errors: Vec<Error>) -> Self {
        Error::Many(errors)
    }
}

/// Every piece of a facade signature [`check_facade_admitted_type`] walks: its
/// parameters and its result, the top-level `Arrow`s separating them stripped
/// away first. A facade is itself a function, so those are the one place an
/// arrow is admitted — a parameter or a result that is itself a function type
/// is what [`check_facade_admitted_type`] then rejects. A facade constant has
/// no top-level arrow to strip, so it is returned whole, as its own one piece.
fn facade_signature_pieces(tpe: &Type) -> Vec<&Type> {
    match tpe {
        Type::Arrow(param, rest) => {
            let mut pieces = vec![param.as_ref()];
            pieces.extend(facade_signature_pieces(rest));
            pieces
        }
        _ => vec![tpe],
    }
}

/// Whether `tpe` — one piece of a facade signature, as
/// [`facade_signature_pieces`] cuts it up — is one of [the admitted
/// types](../../docs/spec/interop.md#which-types-may-cross-the-boundary).
///
/// A bare `Type::Variable` or a `Type::Arrow` anywhere inside `tpe` is
/// rejected, the latter regardless of depth — a function type is inadmissible
/// wherever it is found, not only at the top of the signature.
/// `Type::Type` and `Type::Tuple` recurse into their own arguments, which may
/// still hide either form. `Type::Unit` is admitted: it has one value, and the
/// table gives it a predicate like any other admitted type.
fn check_facade_admitted_type(tpe: &Type) -> Result<(), FacadeRejectedKind> {
    match tpe {
        Type::Variable(_) => Err(FacadeRejectedKind::Variable),
        Type::Arrow(_, _) => Err(FacadeRejectedKind::Function),
        Type::Type(_, args) => args.iter().try_for_each(check_facade_admitted_type),
        Type::Tuple(tuple) => tuple.iter().try_for_each(check_facade_admitted_type),
        Type::Unit => Ok(()),
    }
}

/// Whether `name` is the declaration `module` of `zelkova-core` writes `item`
/// as — `is_core_declaration(name, "Task", "Failure")` for `Task.Failure`.
///
/// All three parts have to match, the same way [`scalars::Scalar::declares`]
/// recognises a scalar: a module named `Task` can only belong to
/// `zelkova-core` ([`default_imports`](super::default_imports)'s doc comment
/// says why), so this is what keeps a package's own `Task` module — were one
/// ever allowed — or a differently-named module's own `Task` type from being
/// misread as the one [`Error::FacadeResultNotEffect`] and
/// [`Error::FacadeTaskMisplaced`] care about.
fn is_core_declaration(name: &QualName, module: &str, item: &str) -> bool {
    name.package().as_str() == CORE_PACKAGE
        && name.module_name().as_str() == module
        && name.unqualified_name().as_str() == item
}

/// Whether `name` is `Task.Task`'s declaration.
fn is_task_declaration(name: &QualName) -> bool {
    is_core_declaration(name, "Task", "Task")
}

/// Whether `name` is `Result.Result`'s declaration.
fn is_result_declaration(name: &QualName) -> bool {
    is_core_declaration(name, "Result", "Result")
}

/// Whether `name` is `Task.Failure`'s declaration.
fn is_failure_declaration(name: &QualName) -> bool {
    is_core_declaration(name, "Task", "Failure")
}

/// Whether `Task` — recognised by [`is_task_declaration`], never by spelling —
/// appears anywhere inside `tpe`, at the top or nested inside a tuple or
/// another type's arguments.
///
/// What [`Error::FacadeTaskMisplaced`] is raised from: `Task` may appear only
/// as the whole of an unmarked facade's result
/// (`docs/spec/interop.md#an-effectful-facade`), so every other piece of a
/// facade signature, and the payload inside an accepted `Task (Result Failure
/// a)` result, is walked with this rather than left to
/// [`check_facade_admitted_type`], which does not know `Task` from any other
/// union.
fn contains_task(tpe: &Type) -> bool {
    match tpe {
        Type::Type(name, args) => is_task_declaration(name) || args.iter().any(contains_task),
        Type::Tuple(tuple) => tuple.iter().any(contains_task),
        Type::Arrow(param, rest) => contains_task(param) || contains_task(rest),
        Type::Variable(_) | Type::Unit => false,
    }
}

/// Whether `tpe`'s own outermost constructor is `Task` — true of `Task Int`
/// and of `Task (Result Failure a)` alike, and false of `Maybe (Task Int)`,
/// where `Task` is present but not in that position.
///
/// [`effectful_result_payload`] tells the two admitted shapes apart; this is
/// what lets the caller tell "the right shape with the wrong contents" apart
/// from "`Task` in a position that was never going to be it", which is what
/// distinguishes [`Error::FacadeResultNotEffect`] from
/// [`Error::FacadeTaskMisplaced`].
fn is_task_applied(tpe: &Type) -> bool {
    matches!(tpe, Type::Type(name, _) if is_task_declaration(name))
}

/// The payload `a` of `tpe`, when `tpe` is exactly the shape an unmarked
/// facade's result must be — `Task (Result Failure a)`, `Task`, `Result` and
/// `Failure` each recognised by the qualified name of their declaration and
/// never by spelling. `None` for any other shape, `Task Int` and
/// `Maybe (Task Int)` included — those are [`Error::FacadeResultNotEffect`],
/// not this function's business to name.
pub fn effectful_result_payload(tpe: &Type) -> Option<&Type> {
    let Type::Type(task, task_args) = tpe else {
        return None;
    };
    if !is_task_declaration(task) {
        return None;
    }
    let [result_tpe] = task_args.as_slice() else {
        return None;
    };
    let Type::Type(result, result_args) = result_tpe else {
        return None;
    };
    if !is_result_declaration(result) {
        return None;
    }
    let [failure_tpe, payload] = result_args.as_slice() else {
        return None;
    };
    let Type::Type(failure, failure_args) = failure_tpe else {
        return None;
    };
    if !is_failure_declaration(failure) || !failure_args.is_empty() {
        return None;
    }
    Some(payload)
}

/// Check that `context` — what an annotation wrote in front of `=>` — is one
/// constraint or a parenthesised list of them, and hand back each constraint's
/// class name and arguments.
///
/// A constraint here is an uppercase name applied to one or more arguments.
/// Nothing is resolved: whether the name is a class, and whether its arguments
/// are types in scope, is not checked, because no class can be declared yet. A
/// two- or three-tuple is the list; the grammar has no tuple of any other size,
/// so a single constraint and a list of two or three are the shapes that reach
/// here. Every malformed constraint of a list is reported, each at its own span.
fn validate_context(context: &parser::Type) -> Result<Vec<(&Name, &[parser::Type])>, Vec<Error>> {
    let constraints: Vec<&parser::Type> = match &context.kind {
        parser::TypeKind::Tuple(tuple) => tuple.iter().collect(),
        _ => vec![context],
    };

    collect_accumulate(constraints.into_iter().map(|constraint| {
        let kind = match &constraint.kind {
            parser::TypeKind::Unqualified(class, args) if !args.is_empty() => {
                return Ok((class, args.as_slice()));
            }
            parser::TypeKind::Unqualified(name, _) => {
                InvalidConstraintKind::Unapplied(name.clone())
            }
            parser::TypeKind::Variable(name) => InvalidConstraintKind::Variable(name.clone()),
            parser::TypeKind::Arrow(..) => InvalidConstraintKind::Arrow,
            parser::TypeKind::Tuple(..) => InvalidConstraintKind::Tuple,
            parser::TypeKind::Unit => InvalidConstraintKind::Unit,
        };

        Err(Error::InvalidConstraint(kind, constraint.span))
    }))
}

/// Transform a given `parser::Module` into a `canonical::Module`.
///
/// Whether this module is exempt from the default imports is not this function's
/// question to answer: `new_environment` derives it from `package` itself
/// ([`PackageName::is_core`]) once `name` is built.
pub fn canonicalize(
    package: &PackageName,
    interfaces: &HashMap<Name, Interface>,
    source: &parser::Module,
) -> Result<Module, Vec<Error>> {
    let name = ModuleName {
        package: package.clone(),
        name: source.name.clone(),
    };

    let mut errors: Vec<Error> = vec![];
    let mut env =
        new_environment(&name, interfaces, &source.imports).map_err(|e| vec![e.into()])?;

    // `unsafe` is a claim about the companion standing behind a facade signature,
    // so it has nothing to say on a declaration with a body above it. The grammar
    // accepts the word on any annotation — it has no way to know the module's
    // header — which leaves this as the only place that can reject one. Reported
    // for every marked declaration, then canonicalization carries on, so a module
    // with a stray `unsafe` still reports whatever else is wrong with it.
    if !source.binding_foreign {
        errors.extend(
            source
                .functions
                .iter()
                .filter(|f| f.marked_unsafe)
                .map(|f| Error::UnsafeOutsideFacade(f.name.clone(), f.annotation_span)),
        );
    }

    // A constraint context is validated here, reported on, and then dropped — the
    // constraints `validate_context` hands back included. The canonical `Type` has
    // no place for one and nothing downstream reads a context yet, so the type
    // checker sees only the type after `=>`. Resolving the class names and keeping
    // the context on the canonical value is the next step of the type-class
    // program (`LANG-70`, after `LANG-39`'s class table), not an oversight here.
    for function in source.functions.iter() {
        if let Some(context) = &function.context {
            if let Err(malformed) = validate_context(context) {
                errors.extend(malformed);
            }

            if source.binding_foreign {
                errors.push(Error::FacadeConstrained(
                    function.name.clone(),
                    context.span,
                ));
            }
        }
    }

    let (infixes, types, values) = if source.binding_foreign {
        // A `module foreign` facade runs a parallel canonicalization process as the constraints are a bit different:
        // - Only functions without bindings are authorized.
        // - Infixes and types are forbidden.
        // The idea being to have the facade stand for the companion file shipped beside it.
        // Assuming Json types are part of the prelude, this should goes well with the restriction on what types are available for bindings.

        // Verify no infix present
        if !source.infixes.is_empty() {
            let e = source
                .infixes
                .iter()
                .map(|i| Error::InfixDeclared(i.operator.clone(), i.span));
            errors.extend(e);
        }
        // Verify no types present
        if !source.types.is_empty() {
            let e = source
                .types
                .iter()
                .map(|t| Error::TypeDeclared(t.name.clone(), t.span));
            errors.extend(e);
        }

        // Register each binding as a top-level value before resolving any of
        // them, the same as the non-`foreign` branch below — otherwise
        // `do_exports`'s existence check (`BUG-8`) would reject a facade
        // exposing its own declared binding, since nothing would have told
        // `env` the binding exists.
        for f in source.functions.iter() {
            env.insert_top_level_value(f.name.clone());
        }

        // Iterate on values
        let iter = source.functions.iter().map(|function| {
            // Make sure there is no binding
            if !function.bindings.is_empty() {
                //println!("bindings = {:?} (js module)", function.bindings);
                Err(Error::BindingPatternsInvalidLen(function.span))? // TODO More specific error
            }

            // Make sure there is a type
            let tpe = function
                .tpe
                .as_ref()
                .ok_or_else(|| Error::NoTypeInBinding(function.name.clone(), function.span))?;
            let tpe = Type::from_parser_type(&env, tpe)?;

            // Every parameter and the result — the arrows a facade's own
            // parameter list contributes stripped first, since a facade is
            // itself a function and that is the one place an arrow is
            // admitted — must be one of the admitted types
            // (`docs/spec/interop.md#which-types-may-cross-the-boundary`).
            // One bad signature must not hide the next, so this pushes onto
            // `errors` the same way the checks above do rather than
            // returning early out of the whole facade. Walked once and
            // reused below for the `Task`-placement check.
            let pieces = facade_signature_pieces(&tpe);
            if let Some(kind) = pieces
                .iter()
                .copied()
                .find_map(|piece| check_facade_admitted_type(piece).err())
            {
                Err(Error::FacadeTypeNotAdmitted(
                    function.name.clone(),
                    kind,
                    function.annotation_span,
                ))?
            }

            // `Task` is confined to the whole of an unmarked facade's result
            // (`docs/spec/interop.md#an-effectful-facade`), so a parameter
            // piece may never hold one, whether or not the signature is
            // `unsafe`. Reached only once the loop above admits every piece,
            // so nothing here still hides a bare type variable or function
            // type for `contains_task` to misread.
            if let Some((&result, parameters)) = pieces.split_last() {
                if parameters.iter().copied().any(contains_task) {
                    Err(Error::FacadeTaskMisplaced(
                        function.name.clone(),
                        function.annotation_span,
                    ))?
                }

                // The result is held to the required effect shape unless the
                // signature says `unsafe` (`DEC-12` decision 1 and 7): an
                // unmarked facade's result must be exactly
                // `Task (Result Failure a)`, and `unsafe` is what removes that
                // requirement rather than any shape `check_facade_admitted_type`
                // already accepted. `Task` still may not appear anywhere else —
                // nested in the payload `a` above, nested under some other type
                // (`Maybe (Task Int)`), or anywhere at all in an `unsafe`
                // facade's result, which gets no exemption from this.
                //
                // `is_task_applied(result)` tells "the right shape with the
                // wrong contents" (`Task Int`) apart from "`Task` in a position
                // that was never going to be it" (`Maybe (Task Int)`), which is
                // what keeps the two error variants pointed at what each is
                // actually about.
                if function.marked_unsafe {
                    if contains_task(result) {
                        Err(Error::FacadeTaskMisplaced(
                            function.name.clone(),
                            function.annotation_span,
                        ))?
                    }
                } else if is_task_applied(result) {
                    match effectful_result_payload(result) {
                        Some(payload) if contains_task(payload) => {
                            Err(Error::FacadeTaskMisplaced(
                                function.name.clone(),
                                function.annotation_span,
                            ))?
                        }
                        Some(_) => {}
                        None => Err(Error::FacadeResultNotEffect(
                            function.name.clone(),
                            function.annotation_span,
                        ))?,
                    }
                } else if contains_task(result) {
                    Err(Error::FacadeTaskMisplaced(
                        function.name.clone(),
                        function.annotation_span,
                    ))?
                } else {
                    Err(Error::FacadeResultNotEffect(
                        function.name.clone(),
                        function.annotation_span,
                    ))?
                }
            }

            let name = function.name.clone();
            // TODO Think how it's going to be represented. Currently canonical values assume an expression is present
            //      I'd like to not introduce a trait or new struct for binding. Should we fake an expression or create
            //      a new type of value ? New type of value will be annoying for regular modules as they aren't present
            //      there. Fake expression might be ok as MVP. We have to make sure that binding module are removed from
            //      some phase of the compilation pipeline.
            let value = Value::TypedValue {
                name: name.clone(),
                patterns: vec![],
                // A `module foreign` facade has no body in the source, so this
                // stand-in has nothing to point at (see the TODO above).
                body: Expression::bare(ExpressionKind::Bool(true)),
                tpe,
                marked_unsafe: function.marked_unsafe,
                span: function.span,
                annotation_span: function.annotation_span,
            };

            Ok((name, value))
        });
        let values = crate::utils::collect_accumulate(iter).unwrap_or_else(|err| {
            errors.extend(err);
            HashMap::new()
        });

        (HashMap::new(), HashMap::new(), values)
    } else {
        // Because we are rewriting infixes in this phase, we must do this check before
        // resolving values.
        let infixes =
            do_infixes(&source.infixes, &mut env, &source.functions).unwrap_or_else(|err| {
                errors.extend(err);
                HashMap::new()
            });

        // Every `type` declaration of this module is in scope for every one of
        // them, its own body included, so all of their names are registered
        // before any body is canonicalized. Doing it as `do_types` produced each
        // union instead would make a declaration resolvable only from the ones
        // written below it, and leave a self-referential declaration —
        // `type Never = JustOneMore Never` — naming a type nothing has heard of.
        // The name and its arity are all a use site needs; `insert_union_type`
        // below fills the constructors in once the bodies are built.
        for tpe in source.types.iter() {
            env.insert_declared_type(&tpe.name, tpe.type_arguments.clone());
        }

        let types = do_types(&env, &source.types).unwrap_or_else(|err| {
            errors.extend(err);
            HashMap::new()
        });

        for (n, t) in types.iter() {
            env.insert_union_type(n.clone(), t.clone());
        }

        trace!("Environment after do_types: {:#?}", env);

        // TODO Should I manage infixes rewrite here too ?
        // Yes I should do it here
        let values = do_values(&mut env, &source.functions).unwrap_or_else(|err| {
            errors.extend(err);
            HashMap::new()
        });

        (infixes, types, values)
    };

    // A parameterless binding's value has to exist before it can be used, so a
    // cycle that holds one — through other bindings or through functions — is an
    // error (`docs/spec/evaluation-semantics.md#a-binding-may-not-depend-on-itself`).
    // Independent of exports, so this runs regardless of what `do_exports` below
    // finds.
    if let Err(err) = check_self_dependency(&values) {
        errors.extend(err);
    }

    // We do exports at the end, and verify that all exported value do
    // have a reference within the current module
    let exports = do_exports(&source.exposing, &env, &values).unwrap_or_else(|err| {
        errors.extend(err);
        Exports::Everything // Never exposed, as we will return the errors instead
    });

    if errors.is_empty() {
        Ok(Module {
            name,
            exports,
            exposing_span: source.exposing_span,
            infixes,
            types,
            values,
            binding_foreign: source.binding_foreign,
        })
    } else {
        Err(errors)
    }
}

/// Whether `value` names no parameter — the kind of binding a cycle
/// [`check_self_dependency`] reports must hold, and the only kind
/// [`initialisation_order`] schedules. Both `Value` variants carry
/// their patterns under a different shape (`Vec<Pattern>` vs. `Vec<(Pattern,
/// Type)>`), so this is the one place that reaches past the difference to ask
/// how many there are.
fn is_parameterless(value: &Value) -> bool {
    match value {
        Value::Value { patterns, .. } => patterns.is_empty(),
        Value::TypedValue { patterns, .. } => patterns.is_empty(),
    }
}

/// Every `VarTopLevel` reference `expr` makes, walking every subexpression a
/// body can hold — a reference is a reference wherever it sits, including
/// inside a `case` branch or an `if` arm
/// (`docs/spec/evaluation-semantics.md#a-binding-may-not-depend-on-itself`).
///
/// `VarTopLevel` is the only expression kind this needs to look for: it is
/// what `Expression::from_parser`'s `Variable` arm produces for a name
/// `Environment::find_value` resolves to `ValueType::TopLevel`, which
/// `insert_top_level_value` sets only for a declaration of *this* module — a
/// local pattern binding resolves to `VarLocal` and an imported name to
/// `VarForeign`, neither of which this pass has any business following.
fn collect_top_level_refs(expr: &Expression, out: &mut Vec<Name>) {
    match &expr.kind {
        ExpressionKind::VarTopLevel(qual) => out.push(qual.unqualified_name()),
        ExpressionKind::VarLocal(_)
        | ExpressionKind::VarKernel(_)
        | ExpressionKind::VarForeign(_, _, _)
        | ExpressionKind::VarConstructor(_, _)
        | ExpressionKind::Char(_)
        | ExpressionKind::String(_)
        | ExpressionKind::Int(_)
        | ExpressionKind::Float(_)
        | ExpressionKind::Bool(_)
        | ExpressionKind::Unit => {}
        ExpressionKind::Apply(a, b) => {
            collect_top_level_refs(a, out);
            collect_top_level_refs(b, out);
        }
        ExpressionKind::If(cond, then, els) => {
            collect_top_level_refs(cond, out);
            collect_top_level_refs(then, out);
            collect_top_level_refs(els, out);
        }
        ExpressionKind::Case(scrutinee, branches) => {
            collect_top_level_refs(scrutinee, out);
            for branch in branches {
                collect_top_level_refs(&branch.expression, out);
            }
        }
        ExpressionKind::Tuple(tuple) => {
            for e in tuple.iter() {
                collect_top_level_refs(e, out);
            }
        }
    }
}

/// The graph *depends on* is read off: one node per top-level declaration of `values`,
/// functions included, and an edge from `u` to `v` for every [`collect_top_level_refs`]
/// reference `u`'s body makes to `v`. A binding depends on everything it can reach along
/// these edges ([evaluation
/// semantics](../../docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once)):
/// initialising a binding runs its body and whatever functions that body calls, and a
/// function can only be called by code that names it or was handed it by code that did,
/// so what can run while a binding is initialised is contained in what it reaches. The
/// relation over-approximates — `a = f` beside `f x = a` mentions `f` without calling it —
/// and [`DEC-19`](../../docs/decisions/dec-19.md) is why that is the rule.
///
/// A reference to an imported name is never an edge: it never canonicalizes to
/// `VarTopLevel` in the first place (see [`collect_top_level_refs`]), and a cross-module
/// cycle is [`dependencies::ModuleWalker`](super::dependencies::ModuleWalker)'s to report.
/// A body that mentions `v` twice gives one edge.
///
/// Shared by [`check_self_dependency`], which asks which strongly-connected components hold
/// a parameterless binding and a cycle, and [`initialisation_order`] (`GEN-7`), which
/// orders the components — one edge set read by two passes, rather than two graphs built
/// from the same rule.
///
/// Nodes are added in name-sorted order, not `values`' raw `HashMap` iteration order, so
/// node indices — and everything `petgraph` derives from them, such as the order
/// `tarjan_scc` returns components in — depend on the names in the source rather than on
/// `values`' hash seed.
fn dependency_graph(values: &HashMap<Name, Value>) -> DiGraph<(&Name, &Value), ()> {
    let mut graph: DiGraph<(&Name, &Value), ()> = DiGraph::new();
    let mut nodes: HashMap<&Name, NodeIndex> = HashMap::new();

    let mut sorted: Vec<(&Name, &Value)> = values.iter().collect();
    sorted.sort_by(|(l, _), (r, _)| l.as_str().cmp(r.as_str()));

    for &(name, value) in &sorted {
        let idx = graph.add_node((name, value));
        nodes.insert(name, idx);
    }

    for &(name, value) in &sorted {
        let Some(&from) = nodes.get(name) else {
            continue;
        };

        let body = match value {
            Value::Value { body, .. } | Value::TypedValue { body, .. } => body,
        };

        let mut refs = Vec::new();
        collect_top_level_refs(body, &mut refs);

        for referenced in &refs {
            if let Some(&to) = nodes.get(referenced) {
                graph.update_edge(from, to, ());
            }
        }
    }

    graph
}

/// `LANG-35`: a strict, parameterless binding is evaluated once, before the program runs,
/// after everything it depends on — so a binding that depends on itself describes no such
/// order, whether the cycle is one binding long (`x = x`) or runs through other bindings
/// and functions (`a = f 1` beside `f x = a`).
///
/// Reports one [`Error::SelfDependency`] per strongly-connected component of
/// [`dependency_graph`] that both holds a parameterless binding and is a cycle: two or
/// more members, or one member with an edge to itself. `tarjan_scc` puts a single node in
/// its own component whether or not it has that edge, which is why the one-member case is
/// asked separately. A component of functions only is never reported — a function's value
/// exists before its body runs — so `isEven`/`isOdd` and `f n = f n` pass. A binding with
/// an edge to itself inside a larger component (`a = (a, b)` beside `b = a`) is reported
/// once, with that component.
///
/// Members are ordered parameterless bindings first, then functions, each group by name,
/// so the report does not depend on traversal order and its first member — the one the
/// message and the primary label are about — is always a parameterless binding.
fn check_self_dependency(values: &HashMap<Name, Value>) -> Result<(), Vec<Error>> {
    let graph = dependency_graph(values);

    let mut errors = Vec::new();

    for component in petgraph::algo::tarjan_scc(&graph) {
        let cyclic = match component.as_slice() {
            [only] => graph.contains_edge(*only, *only),
            _ => true,
        };
        let holds_binding = component.iter().any(|&idx| is_parameterless(graph[idx].1));

        if !(cyclic && holds_binding) {
            continue;
        }

        let mut members: Vec<CycleMember> = component
            .iter()
            .map(|&idx| {
                let (name, value) = graph[idx];
                CycleMember {
                    name: name.clone(),
                    span: value.span(),
                    function: !is_parameterless(value),
                }
            })
            .collect();
        members.sort_by(|l, r| {
            l.function
                .cmp(&r.function)
                .then_with(|| l.name.as_str().cmp(r.name.as_str()))
        });

        errors.push(Error::SelfDependency(members));
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// `GEN-7`: the order `module`'s parameterless bindings must be initialised in — each
/// only after every parameterless binding it depends on, through functions as well as
/// directly
/// (`docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once`).
///
/// Reads [`dependency_graph`], the graph [`check_self_dependency`] (`LANG-35`) reads, and
/// orders its strongly-connected components dependency-first, keeping only the
/// parameterless bindings. A plain topological sort of the graph itself would not do: a
/// cycle of functions only is legal, and a topological sort refuses any cycle. So the
/// components are condensed to one node each, which is acyclic, and that is what is
/// sorted. Functions take part in the sort — an edge through one is still an edge — but
/// are left out of what comes back.
///
/// Among components with no path between them, the one whose smallest member name sorts
/// first comes first, so the order is fixed by the names in the source: two runs of the
/// compiler over one unchanged module produce the same order, and two independent
/// constants come back in name order.
///
/// After `check_self_dependency` accepts a module, every component holding a
/// parameterless binding is that binding alone, so each binding has one well-defined
/// place. `check_module` only reaches `ir::build` — this function's sole caller — after
/// `canonicalize` returned `Ok`, and `canonicalize` runs that check on this same value map
/// first; that is what discharges the assumption. A component holding several
/// parameterless bindings regardless (which should be unreachable) does not panic — this
/// codebase holds `panic!`/`unwrap()`/`expect()` off every non-test path — its bindings
/// come back together, in name order.
///
/// A `module foreign` facade has no parameterless declarations to order for this purpose:
/// `unsafe pi : Float` names a foreign binding directly, with no Zelkova body to place,
/// and is evaluated on whatever schedule the target gives it
/// (`docs/spec/interop.md#facade-constants`), so this returns the empty list for one
/// without inspecting its (structurally parameterless, but synthetic) values.
pub(crate) fn initialisation_order(module: &Module) -> Vec<Name> {
    if module.binding_foreign {
        return Vec::new();
    }

    // `make_acyclic` drops the edges inside a component, so the condensation is a DAG.
    let condensed = petgraph::algo::condensation(dependency_graph(&module.values), true);

    let first_name = |idx: NodeIndex| -> &str {
        condensed[idx]
            .iter()
            .map(|(name, _)| name.as_str())
            .min()
            .unwrap_or_default()
    };

    // An edge `u -> v` means `u` depends on `v`, so a component is ready once every edge
    // out of it has been satisfied. `condensation(g, true)` calls `update_edge` for every
    // original edge between two distinct components, which updates an existing
    // component-to-component edge's weight rather than duplicating it, so no parallel
    // edges between distinct components ever reach `condensed` — the count below and the
    // one edge released per neighbor in the loop are counting the same, already
    // deduplicated set.
    let mut unsatisfied: Vec<usize> = condensed
        .node_indices()
        .map(|idx| condensed.edges_directed(idx, Direction::Outgoing).count())
        .collect();

    let mut ready: BinaryHeap<Reverse<(&str, NodeIndex)>> = condensed
        .node_indices()
        .filter(|idx| unsatisfied[idx.index()] == 0)
        .map(|idx| Reverse((first_name(idx), idx)))
        .collect();

    let mut order = Vec::new();

    while let Some(Reverse((_, idx))) = ready.pop() {
        let mut bindings: Vec<&Name> = condensed[idx]
            .iter()
            .filter(|(_, value)| is_parameterless(value))
            .map(|(name, _)| *name)
            .collect();
        bindings.sort_by(|l, r| l.as_str().cmp(r.as_str()));
        order.extend(bindings.into_iter().cloned());

        for dependent in condensed.neighbors_directed(idx, Direction::Incoming) {
            let count = &mut unsatisfied[dependent.index()];
            *count = count.saturating_sub(1);
            if *count == 0 {
                ready.push(Reverse((first_name(dependent), dependent)));
            }
        }
    }

    order
}

fn do_values(
    env: &mut RootEnvironment,
    functions: &[parser::Function],
) -> Result<HashMap<Name, Value>, Vec<Error>> {
    // Before resolving expressions, we store the top-level values in the environment.
    // We do so first because their expression below could refer to them.
    for f in functions.iter() {
        env.insert_top_level_value(f.name.clone());
    }

    let iter = functions.iter().map(|function| {
        // Bindings to expression

        // TODO Better error message with position of mismatch
        // TODO Error when bindings is empty
        let bindings_size = function
            .bindings
            .iter()
            .all(|v| v.patterns.len() == function.bindings[0].patterns.len());

        if !bindings_size {
            //println!("bindings = {:?} (bindings_size)", function.bindings);
            Err(Error::BindingPatternsInvalidLen(function.span))?
        }

        let (patterns, body): (Vec<Pattern>, Expression) = match function.bindings.len() {
            0 => Err(Error::NoBindings(function.span)),
            1 => {
                // if one binding, we can convert directly to canonical format
                let binding = &function.bindings[0];

                let mut scoped = env.new_scope();

                let patterns: Vec<Pattern> = binding
                    .patterns
                    .iter()
                    .map(|p| Pattern::from_parser(p, env))
                    .collect::<Result<Vec<_>, Error>>()?;

                for p in &patterns {
                    scoped.expose_pattern(p);
                }

                // Maybe create a case_branch function and make it common with Expression::Case ?
                // Or maybe not at the case_branch level, as here we can have multiple patterns
                // whereas cases cannot.
                // eg. a: Int -> Int -> Int  ==>  a b c = b + c
                //println!("Env before transforming expression: {:?}", scoped);
                let body = Expression::from_parser(&binding.body, &scoped)?;

                Ok((patterns, body))
            }
            _ => {
                // if multiple bindings, we need to create synthetics variables and put all bindings into a case expression
                Err(Error::MultipleBindingsUnsupported(
                    function.name.clone(),
                    function.span,
                ))
            }
        }?;

        let name = function.name.clone();

        match &function.tpe {
            Some(t) => {
                let tpe = Type::from_parser_type(env, t)?;
                let linear = Type::to_linear_types(&tpe);

                // Linear is a list of types making the function. Because it includes the return type,
                // it will always be bigger than the number of patterns by one.
                if !patterns.is_empty() && (linear.len() - 1 != patterns.len()) {
                    // TODO Better error message
                    debug!(
                        "linear = {:#?}\nbindings = {:#?} (linear.len ({}) != patterns.len ({}))",
                        linear,
                        function.bindings,
                        linear.len(),
                        patterns.len()
                    );
                    Err(Error::BindingPatternsInvalidLen(function.span))?
                }

                let patterns = patterns.into_iter().zip(linear).collect();

                Ok((
                    function.name.clone(),
                    Value::TypedValue {
                        name,
                        patterns,
                        body,
                        tpe,
                        // `do_values` only runs for a module that is not a facade,
                        // and `canonicalize` has already rejected a marked
                        // annotation there.
                        marked_unsafe: false,
                        span: function.span,
                        annotation_span: function.annotation_span,
                    },
                ))
            }
            None => Ok((
                function.name.clone(),
                Value::Value {
                    name,
                    patterns,
                    body,
                    span: function.span,
                },
            )),
        }
    });

    collect_accumulate(iter)
}

fn do_types(
    env: &dyn Environment,
    types: &[parser::UnionType],
) -> Result<HashMap<Name, UnionType>, Vec<Error>> {
    let iter = types.iter().map(|tpe| {
        let tpe_name = tpe.name.clone();
        // The declaration is this module's, so this is where its variants learn
        // which module the type they build belongs to (`AST-4`).
        let qualified_tpe_name = env.module_name().qualify_name(&tpe_name);
        let variables = tpe.type_arguments.clone();

        trace!("do_types(in:{:?})", tpe);

        // An opaque scalar's declaration writes only its own name and contributes
        // no constructor (`LANG-59`, `DEC-15` decision 2): nothing in the language
        // builds or inspects an `Int`, a `Float`, a `Char` or a `String`, so the
        // only body worth accepting is the one that says so. This runs on the
        // *qualified* name, the same way `scalars::Scalar::declares` always does —
        // a module other than the one a scalar is declared in shares its spelling
        // and nothing else (`BUG-26`), so its own `type Int = …` reaches the
        // ordinary path below untouched.
        if let Some(scalar) = scalars::opaque_scalar_of(&qualified_tpe_name) {
            let is_self_naming = matches!(
                tpe.variants.as_slice(),
                [variant] if matches!(
                    &variant.kind,
                    parser::TypeKind::Unqualified(name, args) if *name == tpe_name && args.is_empty()
                )
            );

            if !is_self_naming {
                return Err(Error::InvalidScalarDeclaration(
                    scalar.qual_name(),
                    tpe.span,
                ));
            }

            return Ok((
                tpe_name,
                UnionType {
                    variables,
                    variants: Vec::new(),
                    span: tpe.span,
                },
            ));
        }

        // A variant is a constructor name and its arguments, which the parser spells
        // `TypeKind::Unqualified`. It is the grammar's general `Type` production that
        // parses a variant list, though, so the four other kinds arrive here too —
        // and each is a declaration the user wrote that has no meaning, not a variant
        // this pass may leave out. Skipping one deletes a constructor from the
        // declaration and reports nothing, which was `BUG-18`.
        //
        // The `collect` below short-circuits on the first `Err`, so only the first bad
        // variant in a single `type` declaration is reported — `type T = c | (Int,
        // Int)` names only `c`. The enclosing `collect_accumulate` still reports the
        // next `type` declaration independently, matching what the `Unqualified` arm
        // already did for `from_parser_type`.
        let variants = tpe
            .variants
            .iter()
            .map(|t| match &t.kind {
                // TODO It might actually make more sense to put Type::from_parser_type
                // on `Environment`.
                parser::TypeKind::Unqualified(name, vars) => {
                    let type_parameters = vars
                        .iter()
                        .map(|t| Type::from_parser_type(env, t))
                        .collect::<Result<Vec<_>, Error>>()?;

                    Ok(TypeConstructor {
                        name: name.clone(),
                        type_parameters,
                        tpe: qualified_tpe_name.clone(),
                    })
                }
                parser::TypeKind::Variable(name) => Err(Error::InvalidVariant(
                    InvalidVariantKind::LowercaseName(name.clone()),
                    t.span,
                )),
                parser::TypeKind::Tuple(_) => {
                    Err(Error::InvalidVariant(InvalidVariantKind::Tuple, t.span))
                }
                parser::TypeKind::Unit => {
                    Err(Error::InvalidVariant(InvalidVariantKind::Unit, t.span))
                }
                parser::TypeKind::Arrow(_, _) => {
                    Err(Error::InvalidVariant(InvalidVariantKind::Arrow, t.span))
                }
            })
            .collect::<Result<Vec<_>, Error>>()?;

        Ok((
            tpe_name,
            UnionType {
                variables,
                variants,
                span: tpe.span,
            },
        ))
    });

    collect_accumulate(iter)
}

fn do_infixes(
    infixes: &[parser::Infix],
    env: &mut RootEnvironment,
    functions: &[parser::Function],
) -> Result<HashMap<Name, Infix>, Vec<Error>> {
    let iter = infixes.iter().map(|infix| {
        let op_name = infix.operator.clone();
        let function_name = infix.function_name.clone();

        let function_exist = functions
            .iter()
            .find(|f| f.name == infix.function_name)
            .is_some();

        if function_exist {
            let infix = Infix {
                associativity: infix.associativity,
                precedence: infix.precedence,
                function_name,
                span: infix.span,
            };

            env.insert_local_infix(op_name.clone(), infix.clone());

            Ok((op_name, infix))
        } else {
            Err(Error::InfixReferenceInvalidValue(
                op_name,
                function_name,
                infix.span,
            ))
        }
    });

    collect_accumulate(iter)
}

// Every arm below checks that the name it names actually resolves in `env`
// before accepting it — a `Lower`/`Upper`/`Operator` name in a module's own
// `exposing (...)` header that nothing declares is `Error::ExportNotFound`
// rather than silently accepted (`BUG-8`). `Upper`'s two arms differ only in
// which `ExportType` they report on success: `Privacy` governs whether the
// type's constructors are exposed, not whether the type itself exists, so
// both check existence with `find_type` the same way.
//
// `values` is this module's own declarations (from `do_values`/the
// facade iterator above), separate from `env`: `env.find_value` also
// answers `Some` for a name resolved through an import, and a re-exported
// foreign value is already guaranteed typed by its own module's `do_exports`
// (`SPEC-5` applies there, transitively, through `Interface::values` only
// ever holding typed entries) — so `Lower`'s annotation check is scoped to a
// name this module declares itself, and leaves a name it does not declare to
// the existence check above.
fn do_exports(
    source_exposing: &parser::Exposing,
    env: &dyn Environment,
    values: &HashMap<Name, Value>,
) -> Result<Exports, Vec<Error>> {
    match source_exposing {
        // `exposing (..)` exposes every top-level declaration this module
        // makes, so `SPEC-5` applies to every one of them, not just those
        // named individually — and there is no per-name span in this header
        // to blame (`parser::Exposing::Open` carries none), so
        // `ExportedValueNotAnnotated`'s first span is `NodeSpan::none()` and
        // the declaration itself is what a diagnostic points at.
        parser::Exposing::Open => {
            let checked = values.values().map(|value| match value {
                Value::TypedValue { .. } => Ok(()),
                Value::Value { name, span, .. } => Err(Error::ExportedValueNotAnnotated(
                    name.clone(),
                    NodeSpan::none(),
                    *span,
                )),
            });

            let _: Vec<()> = collect_accumulate(checked)?;

            Ok(Exports::Everything)
        }
        parser::Exposing::Explicit(exposed) => {
            let specifics =
                exposed.iter().map(|exposed| match &exposed.kind {
                    parser::ExposedKind::Lower(name) => {
                        if env.find_value(name).is_none() {
                            return Err(Error::ExportNotFound(
                                name.clone(),
                                ExportType::Value,
                                exposed.span,
                            ));
                        }

                        match values.get(name) {
                            Some(Value::Value { span, .. }) => Err(
                                Error::ExportedValueNotAnnotated(name.clone(), exposed.span, *span),
                            ),
                            // `Some(TypedValue)` is declared locally and annotated;
                            // `None` is resolved through an import instead of a
                            // local declaration, already covered above.
                            Some(Value::TypedValue { .. }) | None => {
                                Ok((name.clone(), ExportType::Value))
                            }
                        }
                    }
                    // Privacy governs whether the type's constructors are exposed,
                    // not whether the type itself exists — both arms check
                    // existence the same way, and only the `ExportType` they
                    // report on success differs.
                    parser::ExposedKind::Upper(name, parser::Privacy::Public) => {
                        if env.find_type(name).is_some() {
                            Ok((name.clone(), ExportType::UnionPublic))
                        } else {
                            Err(Error::ExportNotFound(
                                name.clone(),
                                ExportType::UnionPublic,
                                exposed.span,
                            ))
                        }
                    }
                    parser::ExposedKind::Upper(name, parser::Privacy::Private) => {
                        if env.find_type(name).is_some() {
                            Ok((name.clone(), ExportType::UnionPrivate))
                        } else {
                            Err(Error::ExportNotFound(
                                name.clone(),
                                ExportType::UnionPrivate,
                                exposed.span,
                            ))
                        }
                    }
                    parser::ExposedKind::Operator(name) => {
                        if env.local_infix_exists(name) {
                            Ok((name.clone(), ExportType::Infix))
                        } else {
                            Err(Error::ExportNotFound(
                                name.clone(),
                                ExportType::Infix,
                                exposed.span,
                            ))
                        }
                    }
                });

            let specifics = collect_accumulate(specifics)?;

            Ok(Exports::Specifics(specifics))
        }
    }
}
