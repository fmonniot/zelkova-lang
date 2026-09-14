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
use super::parser;
use super::Interface;
use super::PhaseError;
use super::SpanLabel;
use super::{ModuleName, PackageName};
use crate::utils::collect_accumulate;
use log::{debug, trace};
use std::collections::HashMap;

mod environment;
/// Part of [`Error::AmbiguousOperatorPrecedence`]'s public shape, so it is
/// re-exported alongside the error rather than left behind a private module.
pub use environment::InfixDeclaration;
use environment::{
    new_environment, suggest_name, EnvError, Environment, InfixFunction, RootEnvironment, ValueType,
};

// Some elements which are common to both AST
use crate::compiler::name::{Name, QualName};
use crate::compiler::position::NodeSpan;
use crate::compiler::source::files::SourceFileId;
use crate::compiler::tuple::Tuple;
pub use parser::Associativity;

// begin AST

/// A resolved module
#[derive(Debug)]
pub struct Module {
    pub name: ModuleName,
    pub exports: Exports,
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
        let infix_functions = infixes
            .values()
            .filter(|infix| !values.contains_key(&infix.function_name))
            .filter_map(|infix| match self.values.get(&infix.function_name) {
                Some(Value::TypedValue { tpe, span, .. }) => {
                    Some((infix.function_name.clone(), (*span, tpe.clone())))
                }
                _ => None,
            })
            .collect();

        super::Interface {
            module_name: self.name.clone(),
            values,
            unions,
            infixes,
            infix_functions,
            file,
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
    fn exposes(&self, name: &Name, kind: &ExportType) -> bool {
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
    /// The type's name once constructed
    pub tpe: Name,
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
/// [`Environment`], which clones types straight out of the [`Interface`]s of the
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
    Type(Name, Vec<Type>),
    // Record
    // Unit
    Arrow(Box<Type>, Box<Type>),
    /// A tuple type. Zelkova keeps Elm's restriction of two or three elements,
    /// which [`Tuple`] carries in its shape.
    Tuple(Tuple<Type>),
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
                    // The head is the *declaration's* name, not the one written:
                    // `Lib.Option` and `Option` are two spellings of one entry, and
                    // normalizing here is what lets the two unify downstream.
                    Some(declared) if declared.arity() == args.len() => {
                        Ok(Type::Type(declared.name.clone(), args))
                    }
                    Some(declared) => Err(Error::TypeArityMismatch(
                        name.clone(),
                        declared.arity(),
                        args.len(),
                        tpe.span,
                    )),
                    // `name` resolves to nothing: BUG-16 is the ticket for reporting
                    // this instead of fabricating a type for it.
                    None => Ok(Type::Type(name.clone(), args)),
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
        /// see [Foreign interoperability](../../../docs/spec/interop.md). Only a
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
    Bool(bool),
    /// A tuple pattern. Zelkova keeps Elm's restriction of two or three
    /// elements, which [`Tuple`] carries in its shape.
    Tuple(Tuple<Pattern>),

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
            parser::PatternKind::Literal(parser::Literal::Bool(b)) => PatternKind::Bool(*b),
            parser::PatternKind::Tuple(tuple) => {
                PatternKind::Tuple(tuple.try_map(|p| Pattern::from_parser(p, env))?)
            }
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
    VarForeign(QualName, Type),
    VarConstructor(QualName, Type),
    Char(char),
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
    // Unit
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
                    ValueType::Foreign(m, _source, tpe) => {
                        ExpressionKind::VarForeign(m.qualify_name(name), tpe.clone())
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

                let name = name
                    .to_qual()
                    .unwrap_or_else(|| env.module_name().qualify_name(name));

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
        InfixFunction::Imported(module, tpe) => {
            ExpressionKind::VarForeign(module.qualify_name(function_name), tpe.clone())
        }
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
    /// before its module's file was known (`ERR-5`).
    AmbiguousVariables(Name, Vec<(ModuleName, Option<super::SourceSpan>)>, NodeSpan),
    /// A constructor used in an expression or a pattern that nothing in scope
    /// declares, where it was written, and an optional "did you mean …?"
    /// suggestion (`ERR-7`).
    VariantNotFound(QualName, NodeSpan, Option<Name>),
    /// Nothing constructs this today — `Environment::find_type_constructor` returns
    /// at most one constructor per name, so it has no way to report an ambiguity.
    /// It is the designated rejection path once it can, and carries the span the
    /// construction site would have, alongside each candidate's declaration
    /// location — see [`Error::AmbiguousVariables`].
    AmbiguousVariants(Name, Vec<(ModuleName, Option<super::SourceSpan>)>, NodeSpan),
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
    /// Something other than a constructor name and its arguments written in a
    /// `type` declaration's variant position: what was written there, and that
    /// variant's own span rather than the declaration's, so the caret sits under
    /// the offending variant alone (`BUG-18`).
    InvalidVariant(InvalidVariantKind, NodeSpan),

    // Binding module
    InfixDeclared(Name, NodeSpan),
    TypeDeclared(Name, NodeSpan),
    NoTypeInBinding(Name, NodeSpan),
    /// An annotation outside a `module foreign` facade was marked `unsafe`: the
    /// name it annotates, and the annotation's span — which the grammar takes
    /// from the modifier, so the caret starts on the word itself.
    ///
    /// `unsafe` asserts something about the companion behind a facade signature
    /// (`LANG-53`, [`DEC-12`](../../../docs/decisions/dec-12.md)). There is no
    /// companion behind an ordinary declaration for it to be a claim about, and
    /// accepting the word there would make it mean nothing in half the places it
    /// can be written.
    UnsafeOutsideFacade(Name, NodeSpan),

    // Utility error
    Many(Vec<Error>),
}

/// What was written where a `type` declaration expected a variant — see
/// [`Error::InvalidVariant`].
///
/// A variant is a constructor name followed by zero or more type arguments, and
/// nothing else. The grammar does not enforce that: it parses a variant list with
/// the general `Type` production, so every shape a type expression can take reaches
/// [`do_types`]. This enum names the three that are not a variant, one per remaining
/// [`parser::TypeKind`], so each can say what it is in the words of the source.
#[derive(Debug, PartialEq, Clone)]
pub enum InvalidVariantKind {
    /// A name beginning with a lowercase letter, which the grammar reads as a type
    /// variable — `type Colour = red`, and a mistyped constructor name along with
    /// it. Carries the name so the message can quote it.
    LowercaseName(Name),
    /// A tuple type — `type Pair = (Int, Int)`.
    Tuple,
    /// A function type — `type Wrapper = Wrap Int -> Int`. The arrow is the *whole*
    /// variant rather than a suffix of it, `Wrap Int` being its left operand, so
    /// there is no constructor here to keep either.
    Arrow,
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
            Error::InvalidVariant(kind, _) => match kind {
                InvalidVariantKind::LowercaseName(name) => format!(
                    "`{}` is not a constructor name: a constructor name begins with an uppercase letter",
                    name
                ),
                InvalidVariantKind::Tuple => {
                    "a variant is a constructor name followed by its arguments, and this one is a tuple type"
                        .to_owned()
                }
                InvalidVariantKind::Arrow => {
                    "a variant is a constructor name followed by its arguments, and this one is a function type"
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
        let ambiguous_candidates = |candidates: &[(ModuleName, Option<super::SourceSpan>)]| {
            candidates
                .iter()
                .filter_map(|(module, source)| {
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
            Error::InvalidVariant(kind, span) => primary(
                span,
                match kind {
                    InvalidVariantKind::LowercaseName(_) => "this begins with a lowercase letter",
                    InvalidVariantKind::Tuple => "a tuple type, written where a variant belongs",
                    InvalidVariantKind::Arrow => "a function type, written where a variant belongs",
                },
            ),
            Error::InfixDeclared(_, span) => primary(span, "declared here"),
            Error::TypeDeclared(_, span) => primary(span, "declared here"),
            Error::NoTypeInBinding(_, span) => primary(span, "declared here"),
            Error::UnsafeOutsideFacade(_, span) => primary(span, "marked `unsafe` here"),
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
            Error::AmbiguousVariables(_, candidates, _)
            | Error::AmbiguousVariants(_, candidates, _) => {
                vec![format!(
                    "it is exposed by: {}",
                    candidates
                        .iter()
                        .map(|(m, _)| m.name().to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )]
            }
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

/// Transform a given `parser::Module` into a `canonical::Module`
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
            infixes,
            types,
            values,
            binding_foreign: source.binding_foreign,
        })
    } else {
        Err(errors)
    }
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
        let variables = tpe.type_arguments.clone();

        trace!("do_types(in:{:?})", tpe);

        // A variant is a constructor name and its arguments, which the parser spells
        // `TypeKind::Unqualified`. It is the grammar's general `Type` production that
        // parses a variant list, though, so the three other kinds arrive here too —
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
                        tpe: tpe_name.clone(),
                    })
                }
                parser::TypeKind::Variable(name) => Err(Error::InvalidVariant(
                    InvalidVariantKind::LowercaseName(name.clone()),
                    t.span,
                )),
                parser::TypeKind::Tuple(_) => {
                    Err(Error::InvalidVariant(InvalidVariantKind::Tuple, t.span))
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
