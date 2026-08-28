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
use environment::{new_environment, EnvError, Environment, RootEnvironment, ValueType};

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
    /// True when this module is a JavaScript binding module.
    /// Such modules have synthetic placeholder bodies and must not be type-checked.
    pub binding_javascript: bool,
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
    pub fn to_interface(&self, file: Option<SourceFileId>) -> super::Interface {
        let values = self
            .values
            .iter()
            .filter_map(|(name, value)| match value {
                Value::Value { .. } => None,
                Value::TypedValue { tpe, span, .. } => Some((name.clone(), (*span, tpe.clone()))),
            })
            .collect();

        super::Interface {
            module_name: self.name.clone(),
            values,
            unions: self.types.clone(),
            infixes: self.infixes.clone(),
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
            parser::TypeKind::Unqualified(name, vars) => match env.find_type(name) {
                Some(t) => Ok(t.clone()),
                None => {
                    let types = vars
                        .iter()
                        .map(|t| Type::from_parser_type(env, t))
                        .collect::<Result<Vec<_>, Error>>()?;

                    // TODO Insert back into Environment ?
                    Ok(Type::Type(name.clone(), types))
                }
            },
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
                        Error::VariantNotFound(env.module_name().qualify_name(name), p.span)
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
                    Error::VariableNotFound(env.module_name().qualify_name(name), e.span)
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
                    Error::VariantNotFound(env.module_name().qualify_name(name), e.span)
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
    EnvironmentErrors(Vec<EnvError>),
    /// (infix, function), and where the `infix` declaration was written
    InfixReferenceInvalidValue(Name, Name, NodeSpan),
    BindingPatternsInvalidLen(NodeSpan),
    /// A declaration with a type annotation and no body, and where the annotation
    /// was written — which is the only part of it there is to point at.
    NoBindings(NodeSpan),
    /// A name used as a value that nothing in scope declares, and where it was
    /// written — the identifier alone, not the declaration around it.
    VariableNotFound(QualName, NodeSpan), // add name suggestion ?
    /// A name exposed unqualified by more than one imported module, and where it
    /// was used. Each candidate module is paired with where the name is declared
    /// there — `Some` when that module's `Interface` knows both its file and the
    /// declaration's span, `None` for a hand-built interface (a test) or one built
    /// before its module's file was known (`ERR-5`).
    AmbiguousVariables(Name, Vec<(ModuleName, Option<super::SourceSpan>)>, NodeSpan),
    /// A constructor used in an expression or a pattern that nothing in scope
    /// declares, and where it was written.
    VariantNotFound(QualName, NodeSpan),
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

    // Binding module
    InfixDeclared(Name, NodeSpan),
    TypeDeclared(Name, NodeSpan),
    NoTypeInBinding(Name, NodeSpan),

    // Utility error
    Many(Vec<Error>),
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
            Error::EnvironmentErrors(errors) => match errors.as_slice() {
                [only] => only.message(),
                many => format!("{} of this module's imports could not be resolved", many.len()),
            },
            Error::InfixReferenceInvalidValue(infix, function, _) => format!(
                "the infix operator `{}` is declared as `{}`, which is not a value declared in this module",
                infix, function
            ),
            Error::BindingPatternsInvalidLen(_) => {
                "the arguments of this declaration do not line up with its type annotation"
                    .to_owned()
            }
            Error::NoBindings(_) => {
                "this declaration has a type annotation but no body".to_owned()
            }
            Error::VariableNotFound(name, _) => {
                format!("cannot find a value named `{}`", name.to_name())
            }
            Error::AmbiguousVariables(name, _, _) => {
                format!("`{}` is exposed by several imported modules", name)
            }
            Error::VariantNotFound(name, _) => {
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
            Error::InfixDeclared(name, _) => format!(
                "a `module javascript` facade cannot declare an infix operator, but declares `{}`",
                name
            ),
            Error::TypeDeclared(name, _) => format!(
                "a `module javascript` facade cannot declare a type, but declares `{}`",
                name
            ),
            Error::NoTypeInBinding(name, _) => format!(
                "`{}` has no type annotation, and a `module javascript` facade is annotations only",
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
            Error::InfixReferenceInvalidValue(_, _, span) => primary(span, "declared here"),
            // The four that name an identifier: the caret sits under the name the
            // user wrote, which is the whole point of spanning expressions and
            // patterns rather than only declarations.
            Error::VariableNotFound(_, span) => primary(span, "no value of this name is in scope"),
            Error::AmbiguousVariables(_, candidates, span) => {
                let mut labels = primary(span, "this name is ambiguous");
                labels.extend(ambiguous_candidates(candidates));
                labels
            }
            Error::VariantNotFound(_, span) => {
                primary(span, "no type constructor of this name is in scope")
            }
            Error::AmbiguousVariants(_, candidates, span) => {
                let mut labels = primary(span, "this type constructor is ambiguous");
                labels.extend(ambiguous_candidates(candidates));
                labels
            }
            Error::BindingPatternsInvalidLen(span) => primary(span, "declared here"),
            Error::NoBindings(span) => primary(span, "this annotation has no body"),
            Error::MultipleBindingsUnsupported(_, span) => primary(span, "declared here"),
            Error::InfixDeclared(_, span) => primary(span, "declared here"),
            Error::TypeDeclared(_, span) => primary(span, "declared here"),
            Error::NoTypeInBinding(_, span) => primary(span, "declared here"),
            // A group has no position of its own; the errors it swallowed do.
            Error::EnvironmentErrors(errors) => errors.iter().flat_map(|e| e.labels()).collect(),
            Error::Many(errors) => errors.iter().flat_map(|e| e.labels()).collect(),
            _ => Vec::new(),
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
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

    let (infixes, types, values) = if source.binding_javascript {
        // Javascript modules run a parallel canonicalization process as the constraints are a bit different:
        // - Only functions without bindings are authorized.
        // - Infixes and types are forbidden.
        // The idea being to have the js module be a facade for the actual Javascript module.
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
                // A `module javascript` facade has no body in the source, so this
                // stand-in has nothing to point at (see the TODO above).
                body: Expression::bare(ExpressionKind::Bool(true)),
                tpe,
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
    let exports = do_exports(&source.exposing, &env).unwrap_or_else(|err| {
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
            binding_javascript: source.binding_javascript,
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

        // variants are represented as parser::Type::Unqualified. Other types
        // can be safely ignored in this context.

        let variants = tpe
            .variants
            .iter()
            .filter_map(|t| match &t.kind {
                // TODO It might actually make more sense to put Type::from_parser_type
                // on `Environment`.
                parser::TypeKind::Unqualified(name, vars) => Some((name, vars)),
                _ => None,
            })
            .map(|(name, vars)| {
                let type_parameters = vars
                    .iter()
                    .map(|t| Type::from_parser_type(env, t))
                    .collect::<Result<Vec<_>, Error>>()?;

                Ok(TypeConstructor {
                    name: name.clone(),
                    type_parameters,
                    tpe: tpe_name.clone(),
                })
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

// `BUG-8`: only the `Operator` arm below checks that the exposed name actually
// exists — a `Lower` or `Upper` name in a module's own `exposing (...)` header is
// accepted unconditionally even when nothing by that name is declared.
fn do_exports(
    source_exposing: &parser::Exposing,
    env: &dyn Environment,
) -> Result<Exports, Vec<Error>> {
    match source_exposing {
        parser::Exposing::Open => Ok(Exports::Everything),
        parser::Exposing::Explicit(exposed) => {
            let specifics = exposed.iter().map(|exposed| match &exposed.kind {
                parser::ExposedKind::Lower(name) => Ok((name.clone(), ExportType::Value)),
                parser::ExposedKind::Upper(name, parser::Privacy::Public) => {
                    Ok((name.clone(), ExportType::UnionPublic))
                }
                parser::ExposedKind::Upper(name, parser::Privacy::Private) => {
                    Ok((name.clone(), ExportType::UnionPrivate))
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
