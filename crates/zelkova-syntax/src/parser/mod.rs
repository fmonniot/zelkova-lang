//! This module contains all the data types representing the source
//! of our language.
//!
//! This is the part the user interact with, and contains some syntax sugar
//! to make their life easier (for example, pattern matching in function
//! declaration)
//!
//! TODO Fill in the sections below
//!
//! ## Compiler phase
//! `parse`
//! ## AST
//! TODO
//! ## Modules
//! TODO hierarchy and modules.
use codespan_reporting::files::SimpleFile;

pub(crate) mod chunk;
pub mod error;
pub mod layout;
pub mod tokenizer;

use crate::name::Name;
use crate::position::{BytePos, NodeSpan, Span};
use crate::tuple::Tuple;
pub use error::Error;
use lalrpop_util::ParseError;

use std::collections::HashMap;

lalrpop_mod!(
    #[allow(clippy::all, non_fmt_panics, unreachable_pub)]
    grammar,
    "/parser/grammar.rs"
);

/// Parse `source_file` into a `Module`, or report the first syntax error in it.
///
/// This is [`parse_recovering`] reduced to its first failure: a source with none is
/// `Ok` of its module, and any other is `Err` of the first failure's error, in source
/// order. Every caller that only wants to know whether a file parses, and why not, keeps
/// that meaning.
pub fn parse(source_file: &SimpleFile<String, String>) -> Result<Module, Error> {
    let (header, declarations, failures) = parse_chunks(source_file.source());

    match (header, failures.into_iter().next()) {
        (Err(failure), _) | (Ok(_), Some(failure)) => Err(failure.error),
        (Ok((modifier, name, exposing, exposing_span)), None) => Ok(Module::from_declarations(
            modifier,
            name,
            exposing,
            exposing_span,
            declarations,
            vec![],
        )),
    }
}

/// What [`parse_recovering`] makes of a source: every declaration that parsed, and every
/// chunk that did not.
#[derive(Debug)]
pub struct Parsed {
    /// The module, holding every declaration that parsed and, in
    /// [`Module::failed`], a record of every declaration that did not. `None` exactly
    /// when the module header is among the `failures`.
    pub module: Option<Module>,
    /// Every chunk that failed to parse, in source order: the header first if it failed,
    /// then each declaration.
    pub failures: Vec<Failure>,
}

/// One top-level declaration, or the module header, that failed to parse.
#[derive(Debug, Clone, PartialEq)]
pub struct Failure {
    /// The source text of the chunk: from its first token to the first token of the
    /// next chunk, or to the end of the source. The header's chunk starts at byte 0.
    pub span: Span<BytePos>,
    /// The first error the chunk's tokenizer, layout pass or grammar raised. Nothing
    /// after it in the same chunk is reported.
    pub error: Error,
    /// The value the chunk declares, when its tokens say so: `Some(name)` for a
    /// declaration chunk whose first item is the token `LowerIdentifier(name)`, `None`
    /// for every other chunk and for the header.
    ///
    /// A chunk opening on a lowercase identifier is the annotation or a binding of the
    /// value of that name, because those are the only two declaration forms the grammar
    /// opens on one. Every other opening — `type`, `infix`, `import`, `unsafe` or another
    /// soft keyword — is read as naming nothing: past the first token what the chunk
    /// would have declared is a guess (`docs/decisions/dec-23.md`, decision 4).
    pub declares: Option<Name>,
}

/// A record, kept on [`Module::failed`], of one declaration chunk that failed to parse.
#[derive(Debug, Clone, PartialEq)]
pub struct Failed {
    /// The chunk's source text, as [`Failure::span`].
    pub span: Span<BytePos>,
    /// The value the chunk declares, as [`Failure::declares`].
    pub declares: Option<Name>,
}

/// Parse `source_file`, carrying on past a syntax error to the next top-level
/// declaration.
///
/// The token stream is cut into one chunk per top-level declaration before the layout
/// pass sees it (`chunk.rs` has where it cuts and why there), and each chunk gets its own
/// `Layout` and its own grammar entry point: `Header` for the first, `Decls` for every
/// other. A chunk reports the first error raised in it and nothing after, which is the
/// rule a whole module followed before the cut. The other chunks are parsed regardless,
/// for their declarations or their errors.
pub fn parse_recovering(source_file: &SimpleFile<String, String>) -> Parsed {
    let (header, declarations, failures) = parse_chunks(source_file.source());

    match header {
        Ok((modifier, name, exposing, exposing_span)) => Parsed {
            module: Some(Module::from_declarations(
                modifier,
                name,
                exposing,
                exposing_span,
                declarations,
                failures
                    .iter()
                    .map(|failure| Failed {
                        span: failure.span,
                        declares: failure.declares.clone(),
                    })
                    .collect(),
            )),
            failures,
        },
        Err(failure) => Parsed {
            module: None,
            failures: std::iter::once(failure).chain(failures).collect(),
        },
    }
}

/// The parts of a module header the `Header` entry point parses: the arguments
/// `Module::from_declarations` takes before the declarations.
type Header = (Option<tokenizer::Token>, Name, Exposing, NodeSpan);

/// Cut `source` into chunks and parse each: the header, the declarations that parsed in
/// source order, and the failures among the chunks after the header, in source order.
fn parse_chunks(source: &str) -> (Result<Header, Failure>, Vec<Declaration>, Vec<Failure>) {
    let end = chunk::end_of(source);
    let tokens = tokenizer::make_tokenizer(source).map(|r| r.map_err(Error::from));
    let mut chunks = chunk::Chunks::new(tokens, end);
    let mut declarations = vec![];
    let mut failures = vec![];

    // A source with no tokens at all still has a header to fail, at its end.
    let header_chunk = chunks.next().unwrap_or_else(|| chunk::Chunk::empty(end));
    let header = match parse_chunk(&header_chunk, |tokens| {
        grammar::HeaderParser::new().parse(tokens)
    }) {
        Ok((header, header_declarations)) => {
            declarations.extend(header_declarations);
            Ok(header)
        }
        Err(error) => Err(Failure {
            span: header_chunk.span(),
            error,
            declares: None,
        }),
    };

    for chunk in chunks {
        match parse_chunk(&chunk, |tokens| grammar::DeclsParser::new().parse(tokens)) {
            Ok(chunk_declarations) => declarations.extend(chunk_declarations),
            Err(error) => failures.push(Failure {
                span: chunk.span(),
                error,
                declares: chunk.declares(),
            }),
        }
    }

    (header, declarations, failures)
}

/// The tokens a grammar entry point reads: the layout pass's output.
type LayoutItem = Result<(BytePos, tokenizer::Token, BytePos), Error>;

/// Run one chunk through its own `Layout` and the grammar entry point `entry`.
///
/// A grammar error can be a layout rule seen from the grammar's side, which only the
/// layout pass can recognise, so it gets to explain the error first. What it asks back —
/// whether the chunk's tokens before a line parse on their own — is answered with the
/// same entry point, on a `Layout` that ends where that line starts.
fn parse_chunk<T>(
    chunk: &chunk::Chunk,
    entry: impl Fn(
        &mut dyn Iterator<Item = LayoutItem>,
    ) -> Result<T, ParseError<BytePos, tokenizer::Token, Error>>,
) -> Result<T, Error> {
    let mut indented = layout::Layout::new(chunk.tokens.iter().cloned(), chunk.end);

    entry(&mut indented).map_err(|e| {
        let error = after_a_name(e.into(), &chunk.tokens);

        indented.explain(error, |line_start| {
            let before = chunk.tokens.iter().take_while(|item| {
                matches!(item, Ok(token) if token.span.start.absolute.0 < line_start.absolute.0)
            });
            entry(&mut layout::Layout::new(before.cloned(), line_start)).is_ok()
        })
    })
}

/// `error`, with an [`Error::SpacedDot`]'s `continues_a_name` kept only when the token
/// right before the `.` is an uppercase name.
///
/// `From<ParseError>` sets the flag when the grammar would have accepted a `Dot` where it
/// met the `.`, and that holds after every operand a field access may be written on —
/// `a . b`, `1 .`, `r. name` — as well as after a name a qualification continues. Only
/// the second has a qualified name for the message to speak of, and the grammar's
/// expected tokens cannot tell the two apart, so this reads the token itself.
fn after_a_name(error: Error, tokens: &[chunk::RawToken]) -> Error {
    match error {
        Error::SpacedDot {
            dot,
            continues_a_name: true,
        } => {
            let before = tokens
                .iter()
                .filter_map(|item| item.as_ref().ok())
                .take_while(|token| token.span.start.absolute.0 < dot.start.0)
                .last();

            Error::SpacedDot {
                dot,
                continues_a_name: matches!(
                    before,
                    Some(token) if matches!(token.value, tokenizer::Token::UpperIdentifier(_))
                ),
            }
        }
        error => error,
    }
}

/// A part of a declared type. This is also used in type annotations.
///
/// Types with no arguments will be composed of exactly one `Type`.
/// As their name indicates, a type with arguments will requires more
/// `Type` as arguments. For example the optional type will require
/// one no-arg type: `Maybe Int` (this is another name for higher-kinded types)
///
/// # Why the span is a field rather than a wrapper
///
/// The three recursive nodes of this AST — `Type`, [`Pattern`] and [`Expression`] —
/// are each a struct pairing a [`NodeSpan`] with a `…Kind` enum, rather than the
/// enum wrapped in a `Spanned<BytePos, _>`. The difference shows up in the children:
/// here they stay `Box<Type>` and `Vec<Type>`, each carrying its own span, so a
/// reader matches `&t.kind` once per function instead of unwrapping a `.value` at
/// every child. `canonical/mod.rs` is that reader.
#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    pub span: NodeSpan,
    pub kind: TypeKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeKind {
    /// Type constructor
    Unqualified(Name, Vec<Type>),
    // TODO Qualified type eg Maybe.Maybe (or is it already merged in Unqualified ?)
    /// Type constructor →
    ///
    /// Applications of the type constructor → are written infix and
    /// associate to the right, so T → T' → T" stands for T → (T' → T").
    Arrow(Box<Type>, Box<Type>),
    /// Type variable
    Variable(Name),
    /// A tuple type, of two or three elements — see [`Tuple`].
    Tuple(Tuple<Type>),
    /// The unit type, `()`. Not a tuple: [`Tuple`] has no arity of zero.
    Unit,
    /// A record type, `{ label : Type, … }`, its fields in the order they were
    /// written. The grammar never builds one with no field.
    ///
    /// The order means nothing — a record type is a set of fields
    /// (`docs/spec/records.md`) — and it is kept only because this is the source as
    /// written: canonicalization is where the list becomes a set and where a repeated
    /// label is reported.
    Record(Vec<Field<Type>>),
}

/// One field of a record type, a record or an update: a label and what it was given,
/// `label : Type` or `label = expr`.
#[derive(Debug, PartialEq, Clone)]
pub struct Field<T> {
    pub label: Name,
    /// Where the label alone was written. A diagnostic about the label — a repeated
    /// one — points here, and the value carries a span of its own.
    pub label_span: NodeSpan,
    pub value: T,
}

impl<T> Field<T> {
    /// A field the parser built, its label at `label_span`.
    pub fn new(label: Name, label_span: NodeSpan, value: T) -> Field<T> {
        Field {
            label,
            label_span,
            value,
        }
    }
}

impl Type {
    /// A type the parser built, at the position its production captured.
    pub fn new(span: NodeSpan, kind: TypeKind) -> Type {
        Type { span, kind }
    }

    /// A type with no position — hand-built by a test. See [`NodeSpan`].
    pub fn bare(kind: TypeKind) -> Type {
        Type {
            span: NodeSpan::none(),
            kind,
        }
    }

    pub fn unqualified(span: NodeSpan, name: Name) -> Type {
        Type::new(span, TypeKind::Unqualified(name, Vec::default()))
    }

    pub fn unqualified_with(span: NodeSpan, name: Name, types: Vec<Type>) -> Type {
        Type::new(span, TypeKind::Unqualified(name, types))
    }
}

/// A Module is the top-level structure for a source file.
///
/// It contains everything in a source file.
///
///
/// The elm compiler declare a module as follow:
/// ```haskell
/// data Module =
///   Module
///     { _name    :: ModuleName.Canonical
///     , _exports :: Exports
///     , _docs    :: Src.Docs
///     , _decls   :: Decls
///     , _unions  :: Map.Map Name Union
///     , _aliases :: Map.Map Name Alias
///     , _binops  :: Map.Map Name Binop
///     , _effects :: Effects
///     }
/// ```
#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: Name,
    pub binding_foreign: bool,
    pub exposing: Exposing,
    /// Where the header's `exposing (...)` was written, the keyword and the list together.
    /// A diagnostic about what the module exposes as a whole, rather than about one name
    /// in the list, points here.
    pub exposing_span: NodeSpan,
    pub imports: Vec<Import>,
    pub infixes: Vec<Infix>,
    pub types: Vec<UnionType>,
    pub functions: Vec<Function>,
    /// The declaration chunks that failed to parse, in source order.
    ///
    /// Only [`parse_recovering`] fills it: [`parse`] has no module to return when any
    /// declaration failed. A failed chunk that names a value is how canonicalization
    /// knows the name exists while nothing of it parsed, and one that names nothing is
    /// how it knows a name could be missing from the module's scope.
    pub failed: Vec<Failed>,
}

impl Module {
    fn from_declarations(
        modifier: Option<tokenizer::Token>,
        name: Name,
        exposing: Exposing,
        exposing_span: NodeSpan,
        declarations: Vec<Declaration>,
        failed: Vec<Failed>,
    ) -> Module {
        let binding_foreign = matches!(modifier, Some(tokenizer::Token::Foreign));

        let mut imports = vec![];
        let mut types = vec![];
        let mut infixes = vec![];
        let mut functions = HashMap::<Name, Vec<Declaration>>::new();

        for declaration in declarations {
            match declaration {
                Declaration::Function(FunBinding { ref name, .. })
                | Declaration::FunctionType(FunType { ref name, .. }) => {
                    match functions.get_mut(name) {
                        Some(decls) => decls.push(declaration),
                        None => {
                            functions.insert(name.clone(), vec![declaration]);
                        }
                    }
                }
                Declaration::Import(i) => imports.push(i),
                Declaration::Infix(i) => infixes.push(i),
                Declaration::Union(t) => types.push(t),
            }
        }

        let functions = functions.into_iter().map(|(name, decls)| {
            let mut tpe = None;
            let mut context = None;
            let mut marked_unsafe = false;
            let mut bindings = vec![];
            // The declarations that make one function were parsed independently, so
            // the function's span is the union of theirs: the annotation merged with
            // every binding. `merge` tolerates a missing half, which is what an
            // annotation with no body (a `module foreign` facade) needs.
            let mut span = NodeSpan::none();
            // The annotation on its own, kept beside the merged span because a type
            // error points at the annotation to say where the expected type came from.
            let mut annotation_span = NodeSpan::none();

            // TODO Error if more than function type is defined

            for d in decls {
                match d {
                    Declaration::Function(b) => {
                        span = span.merge(b.span);
                        bindings.push(b.pattern);
                    }
                    Declaration::FunctionType(t) => {
                        span = span.merge(t.span);
                        annotation_span = t.span;
                        marked_unsafe = t.marked_unsafe;
                        context = t.context;
                        tpe.replace(t.tpe);
                    }
                    _ => panic!("Invalid kind of declaration used in functions, report this error ({:?})", d),
                }
            }

            Function { name, tpe, context, marked_unsafe, bindings, span, annotation_span }
        }).collect::<Vec<_>>();

        Module {
            name,
            binding_foreign,
            exposing,
            exposing_span,
            imports,
            infixes,
            types,
            functions,
            failed,
        }
    }
}

/// `Function` represent a function declaration in the source code.
///
/// In _zelkova_, like in _Haskell_ but as opposed to _Elm_, we can
/// use pattern matching and multiple line to declare a function:
///
/// ```zel
/// map : (a -> b) -> Maybe a -> Maybe b
/// map f (Just value) = f value
/// map _ Nothing => Nothing
/// ```
///
/// Because of this syntax, a function is defined as a list of `Match`
/// statement, which each entry specifying a line. The parser will happily let
/// us define a function which doesn't match all cases, a later phase will
/// need to check this.
#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Name,
    pub tpe: Option<Type>,
    /// What the annotation wrote in front of `=>`, if anything — see
    /// [`FunType::context`]. Always `None` when `tpe` is.
    pub context: Option<Type>,
    /// True when this function's annotation was written `unsafe name : Type`.
    ///
    /// Read off the [`FunType`] that contributed the annotation, so a function
    /// with no annotation is never marked. See [`FunType::marked_unsafe`].
    pub marked_unsafe: bool,
    pub bindings: Vec<Match>,
    /// The annotation and every binding, merged into one span.
    ///
    /// A `Function` is assembled in `Module::from_declarations` out of
    /// declarations the grammar saw separately, so this covers the annotation
    /// *and* the body rather than either alone — which is what a type mismatch
    /// between the two is actually about.
    pub span: NodeSpan,
    /// Where the `name : Type` annotation alone was written, or
    /// [`NodeSpan::none`] when the function has no annotation.
    ///
    /// The merged `span` above cannot answer "where does the expected type come
    /// from", because it also covers the body that contradicts it. A type error
    /// draws its secondary label — *expected because of this type annotation* —
    /// here.
    pub annotation_span: NodeSpan,
}

/// Exposing represent whether an import (or export) expose terms.
///
/// `Exposing::Explicit` represents a selection of term exported
/// (or imported) for a given module.
///
/// `Exposing::Open` means every top-level terms are exported, or when
/// used in imports, all exported terms are imported.
#[derive(Debug, PartialEq)]
pub enum Exposing {
    Open,
    Explicit(Vec<Exposed>),
}

/// A single name in an `exposing` list — either a module's header
/// (`module Foo exposing (bar)`) or an import's (`import Foo exposing (bar)`).
///
/// Like [`Type`], [`Pattern`] and [`Expression`], this is a span plus a kind rather
/// than a spanned enum; see [`Type`] for why. `ERR-9` added the span: before it, an
/// error naming one of these — `EnvError::ValueNotFound`, `Error::ExportNotFound` —
/// had nowhere finer than the whole `import`/`module` line to point at.
#[derive(Debug, PartialEq)]
pub struct Exposed {
    pub span: NodeSpan,
    pub kind: ExposedKind,
}

#[derive(Debug, PartialEq)]
pub enum ExposedKind {
    Lower(Name),
    Upper(Name, Privacy),
    Operator(Name),
}

impl Exposed {
    /// An exposed name the parser built, at the position its production captured.
    pub fn new(span: NodeSpan, kind: ExposedKind) -> Exposed {
        Exposed { span, kind }
    }

    /// An exposed name with no position — hand-built by a test. See [`NodeSpan`].
    pub fn bare(kind: ExposedKind) -> Exposed {
        Exposed {
            span: NodeSpan::none(),
            kind,
        }
    }
}

/// Privacy control how a custom type is exposed.
///
/// For example, given the following type:
/// ```zel
/// type MyType = VariantA | VariantB
/// ```
///
/// When importing or exporting this type, we have
/// two privacy settings:
/// - public: `MyType(..)`. In this mode the variant
///   constructors are made public to other modules.
/// - private: `MyType`. In this mode the custom type
///   is behaving as an opaque type. Other module can't
///   know what's inside this type.
#[derive(Debug, PartialEq)]
pub enum Privacy {
    Public,
    Private,
}

/// A Declaration is a top-level block and is the basis for a `Module`.
#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function(FunBinding),
    FunctionType(FunType),
    Import(Import),
    /// Union types are also called custom types in Elm
    Union(UnionType),
    Infix(Infix),
    // type aliases, infixes and ports will end up here
}

/// A representation of the `import` declaration
#[derive(Debug, PartialEq)]
pub struct Import {
    /// The name of the module being imported
    pub name: Name,
    pub alias: Option<Name>,
    pub exposing: Exposing,
    /// Where the whole `import …` line was written.
    pub span: NodeSpan,
}

/// Represents the type signature of a particular function
#[derive(Debug, PartialEq)]
pub struct FunType {
    pub name: Name,
    /// The type after `=>`, or the whole annotation when there is no context.
    pub tpe: Type,
    /// What was written in front of `=>` — `Comparable a` in
    /// `min : Comparable a => a -> a -> a` — or `None` for an unconstrained
    /// annotation.
    ///
    /// It is a [`Type`] and not yet a list of constraints, because the grammar
    /// cannot tell the two apart: `(Comparable k, Eq v)` is the same tokens as a
    /// two-tuple type, so the `ConstrainedType` production parses the context as a
    /// type and anything type-shaped reaches here, `Int -> Int` included.
    /// Canonicalization is what checks that it is one constraint or a tuple of
    /// them, where its errors carry a span like every other.
    ///
    /// A context is a property of a *signature*, so it lives on the signature
    /// rather than as a [`TypeKind`] variant. `TypeKind` is the shape of every type
    /// the parser builds, variants and nested arguments included, and a
    /// `Constrained` case there would be one that every match over it had to handle
    /// while only ever being legal at the top of an annotation. Here the grammar
    /// cannot put one anywhere else, and a `class` or `instance` head — the other
    /// place `=>` is written — reuses the same production and the same pair.
    pub context: Option<Type>,
    /// True when the annotation was written `unsafe name : Type`.
    ///
    /// The word only means something on a [facade](../../docs/spec/interop.md)
    /// signature, where it declares a plain function instead of the effect a
    /// facade declares by default. The grammar accepts it on any annotation, and
    /// canonicalization is what rejects one outside a `module foreign` header.
    pub marked_unsafe: bool,
    /// Where the annotation — `unsafe name : Type`, modifier included — was
    /// written.
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq)]
pub struct UnionType {
    pub name: Name,
    pub type_arguments: Vec<Name>,
    pub variants: Vec<Type>, // TODO Restrict to Type::Unqualified
    /// Where the whole `type … = …` declaration was written.
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq)]
pub struct Infix {
    pub operator: Name,
    pub associativity: Associativity,
    /// The operator's declared precedence, exactly as written after `infix`.
    ///
    /// The parser does nothing with this beyond carrying it forward — `InfixExpr`
    /// has no operator table to consult, since an operator's declaration may live
    /// in another module not yet canonicalized (see [`ExpressionKind::InfixChain`]).
    /// Canonicalization is where a higher precedence ends up binding tighter
    /// (grouped first), by re-associating a flat run of operators against this
    /// field once every operator in it has resolved through the infix
    /// environment.
    pub precedence: u8,
    pub function_name: Name,
    /// Where the whole `infix … = …` declaration was written.
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Associativity {
    Left,
    None,
    Right,
}

/// A `FunBinding` is one of the (possibly multiple) function declaration.
///
/// ## Examples
///
///
/// `const = 42` in Zelkova will result in the following AST:
/// ```text
/// Declaration::Function(
///     FunBinding {
///         name: Name("const"),
///         patterns: Match {
///             pattern: [],
///             body: Expression::Lit(Literal::Int(42)),
///         }
///     }
/// )
/// ```
///
///
/// `identity x y = x` in Zelkova will result in the following AST:
/// ```text
/// Declaration::Function(
///     FunBinding {
///         name: Name("identity"),
///         pattern: Match {
///             patterns: [ Pattern::Var(Name("x")), Pattern::Var(Name("y")) ],
///             body: Expression::Var(Name("x")),
///         ]
///     }
/// )
/// ```
#[derive(Debug, PartialEq)]
pub struct FunBinding {
    pub name: Name,
    pub pattern: Match,
    /// Where this one binding — patterns and body — was written.
    pub span: NodeSpan,
}

impl FunType {
    /// Build a signature out of the pieces the grammar captured. The grammar has
    /// one production per way of spelling the name and the `unsafe` modifier, and
    /// this is the body they share; `constrained` is what `ConstrainedType`
    /// produced, the context first.
    fn assemble(
        name: Name,
        constrained: (Option<Type>, Type),
        marked_unsafe: bool,
        span: NodeSpan,
    ) -> FunType {
        let (context, tpe) = constrained;

        FunType {
            name,
            tpe,
            context,
            marked_unsafe,
            span,
        }
    }
}

impl FunBinding {
    /// Build a binding out of the pieces the grammar captured.
    ///
    /// The grammar has one production per way of spelling the name — an ordinary
    /// lowercase name, or the soft keyword `unsafe` used as one — and this is the
    /// body they share. `l` is the start of the name, `ml` the start of the
    /// patterns; both ends come from the expression rather than an `@R`, for the
    /// reason `FunBinding`'s productions in `grammar.lalrpop` set out.
    fn assemble(
        name: Name,
        l: crate::position::BytePos,
        ml: crate::position::BytePos,
        patterns: Vec<Pattern>,
        expr: Expression,
    ) -> FunBinding {
        let span = NodeSpan::to_end_of(l, expr.span);
        let match_span = NodeSpan::to_end_of(ml, expr.span);

        FunBinding {
            name,
            pattern: Match {
                patterns,
                body: expr,
                span: match_span,
            },
            span,
        }
    }
}

/// The match structure is composed of a serie of patterns and an associated expression
#[derive(Debug, PartialEq)]
pub struct Match {
    pub patterns: Vec<Pattern>,
    pub body: Expression,
    /// The patterns and the body, from the first pattern to the last byte of the
    /// expression. It excludes the function's name, which the enclosing
    /// [`FunBinding`] span covers.
    pub span: NodeSpan,
}

/// A pattern is the left handside of a pattern-match expression
///
/// A pattern-matching expression can be present in function declaration
/// or as part of the `case of` syntax.
///
/// ## Missing Patterns
/// - `Record [Name]`
/// - `Alias Pattern (Name)`
/// - `Ctor Name [Pattern]`
/// - `CtorQual Name Name [Pattern]`
/// - `List [Pattern]`
/// - `Cons Pattern Pattern`
#[derive(Debug, PartialEq, Clone)]
pub struct Pattern {
    pub span: NodeSpan,
    pub kind: PatternKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PatternKind {
    Variable(Name),
    Literal(Literal),
    /// A tuple pattern, of two or three elements — see [`Tuple`].
    Tuple(Tuple<Pattern>),
    /// The unit pattern, `()`, which matches the one value of the unit type.
    Unit,
    Constructor(Name, Vec<Pattern>),
    Anything,
}

impl Pattern {
    /// A pattern the parser built, at the position its production captured.
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
}

/// An Expression
///
/// Like [`Type`] and [`Pattern`], this is a span plus a kind rather than a spanned
/// enum; see [`Type`] for why.
#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub span: NodeSpan,
    pub kind: ExpressionKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionKind {
    Lit(Literal),                                  // Literal, as other are fully named
    Application(Box<Expression>, Box<Expression>), // TODO Rename Apply ?
    Variable(Name),                                // TODO Qualified variable
    TypeConstructor(Name),
    /// A tuple expression, of two or three elements — see [`Tuple`].
    Tuple(Tuple<Expression>),
    /// The unit value, `()`.
    Unit,
    Case(Box<Expression>, Vec<CaseBranch>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    /// A run of infix operator applications, exactly as the grammar saw them —
    /// `a * b + c` is `InfixChain(a, [(*, _, b), (+, _, c)])`, not a tree.
    ///
    /// The grammar has no operator table: an operator's `infix` declaration may
    /// live in another module, resolved only once canonicalization has that
    /// module's `Interface` in hand, so `InfixExpr` cannot decide precedence or
    /// associativity while parsing. It hands this flat sequence to
    /// canonicalization instead — each tuple is one operator's name, the span it
    /// was written at (which an invented `Application` node takes, the same way
    /// the previous right-recursive rewrite did), and the operand to its right —
    /// and canonicalization re-associates it into nested `Application` nodes
    /// once it has resolved every operator through the infix environment. See
    /// `canonical::Expression::from_parser`'s `InfixChain` arm.
    InfixChain(Box<Expression>, Vec<(Name, NodeSpan, Expression)>),
    /// A record, `{ label = expr, … }`. The grammar never builds one with no field.
    ///
    /// The fields stay in the order they were written, here and in the canonical
    /// AST. Unlike a record type's, that order means something: each field is a
    /// subexpression, and subexpressions are evaluated left to right
    /// (`docs/spec/evaluation-semantics.md`).
    Record(Vec<Field<Expression>>),
    /// An update, `{ expr | label = expr, … }`: the record `expr` evaluates to, with
    /// the named fields replaced. The fields keep their written order, as a
    /// [`Record`](ExpressionKind::Record)'s do.
    Update(Box<Expression>, Vec<Field<Expression>>),
    /// A field access, `r.name`: the record, the label, and where the label alone was
    /// written. The expression's own span covers the whole `r.name`.
    ///
    /// The record is never a bare constructor name: `Widget.size` is a qualified name,
    /// and the grammar's `Accessible` says why that leaves the constructor out.
    Access(Box<Expression>, Name, NodeSpan),
    /// An accessor, `.name`, the function reading that field: the label, and where the
    /// label alone was written. The expression's own span covers the `.` as well.
    Accessor(Name, NodeSpan),
}

impl Expression {
    /// An expression the parser built, at the position its production captured.
    pub fn new(span: NodeSpan, kind: ExpressionKind) -> Expression {
        Expression { span, kind }
    }

    /// An expression with no position — hand-built by a test. See [`NodeSpan`].
    pub fn bare(kind: ExpressionKind) -> Expression {
        Expression {
            span: NodeSpan::none(),
            kind,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseBranch {
    pub pattern: Pattern,
    pub expression: Expression,
    /// The whole branch, from the pattern to the last byte of its expression.
    pub span: NodeSpan,
}

/// A literal
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
    /// A string literal, its escape sequences already replaced by the characters
    /// they name.
    String(String),
}
