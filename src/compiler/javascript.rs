//! The JavaScript backend: one checked module in, the text of one ES module out.
//!
//! [`emit`] reads the [`ir::Module`] a [`CheckedModule`] carries, beside the
//! [`canonical::Module`] it was built from for the one thing the IR does not hold — which
//! names the module exports. It produces text and writes nothing: `compile_package` writes
//! it, to the path [`module_file`] gives, once the whole build has checked and emitted.
//!
//! # The shape of an emitted module
//!
//! In this order, each section separated from the next by a blank line and left out when
//! it is empty:
//!
//! 1. **Imports** — the runtime helpers the module calls, from the runtime module
//!    ([`RUNTIME`]), then its companion's exports for a facade, then one named import per
//!    value another module declares, from that module's package.
//! 2. **Hoisted constructors** — one `const` per constructor of no arguments this module
//!    declares, then one per such constructor of another module's that it mentions, which
//!    every mention of it refers to ([`DEC-18` decision
//!    4](../../../docs/decisions/dec-18.md#4--a-constructor-of-no-arguments-is-hoisted-to-one-module-level-constant)).
//! 3. **Functions** — in a facade, first the union predicates its boundary checks call
//!    ([*The boundary check*](#the-boundary-check)), in name order; then one `function`
//!    per declaration that takes parameters, with exactly as many JavaScript parameters
//!    as it was written with ([`DEC-18` decision
//!    3](../../../docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)).
//! 4. **Parameterless bindings** — one `const` each, in
//!    [`ir::Module::initialisation_order`], so each is initialised after every other one
//!    it depends on — the ones it mentions, and the ones a function it mentions reaches —
//!    and no `const` is read before it is initialised.
//! 5. **Exports** — one `export { … }` naming each exported value by its Zelkova name.
//!
//! # Representations
//!
//! An `Int` literal is a `BigInt` (`1n`), since [`Int` is 64
//! bits](../../../docs/spec/evaluation-semantics.md#numbers); a `Float` is a number, a
//! `Char` a one-character string and a `String` a string. `True` and `False` — the
//! constructors of [`scalars::BOOL`], recognised by the union's qualified name — are
//! `true` and `false`.
//! `()` is `undefined` ([The unit value crosses as
//! `undefined`](../../../docs/spec/interop.md#the-unit-value-crosses-as-undefined)) and a
//! pattern that names it, at any depth, tests nothing and binds nothing — `undefined`
//! among `RESERVED` is what keeps a Zelkova binding of that name from changing what a `()`
//! in its scope reads as. Every other union value is `{$: "Ctor", a: …, b: …}`, arguments
//! in declaration order ([A union crosses as a tagged
//! value](../../../docs/spec/interop.md#a-union-crosses-as-a-tagged-value)); a
//! constructor's arguments past the 26th continue `aa`, `ab`, … — see `field`. A tuple
//! is an array.
//!
//! A constructor is not exported. An importer that builds one builds its own object of
//! the same shape, and hoists its own constant for one of no arguments. [Equality is
//! structural](../../../docs/spec/evaluation-semantics.md#what-structural-equality-computes),
//! so nothing observes which module allocated it.
//!
//! # Calls
//!
//! An application whose IR node is [`Saturation::Saturated`] is a direct call,
//! `f(a, b)`, or for a constructor the object itself. Every other application calls a
//! *function value*, one argument per JavaScript call: `g(a)(b)`. What makes that
//! correct is that every function value the emitted code hands around accepts being
//! called one argument at a time. A declaration of two or more parameters — this
//! module's or another's — and a constructor of two or more arguments, is `$curry(f, n)`
//! wherever it is used as a value rather than called, so a partial application such as
//! `pick 1` is `$curry(pick, 2)(1n)`; a one-parameter function already takes its
//! argument one at a time.
//!
//! A value another module declares is called the same way as one of this module's
//! ([`ir::ReferenceKind::Foreign`] carries its arity). A module exports each declaration
//! as it emitted it — a declaration with parameters as the plain n-ary `function`, a
//! parameterless binding as its `const` — and its [`Interface`](zelkova_compiler::Interface)
//! records how many parameters each takes, so an importer's `Lib.pick a b` is a direct
//! call of its import, `app$Lib$pick(a, b)`, as `pick a b` is inside `Lib`. A
//! parameterless binding has arity 0 and is called one argument at a time wherever it
//! is called, which is why a binding whose value is a function of two or more
//! parameters, such as `Basics`' `add = Js.Basics.addInt`, holds that function
//! `$curry`'d.
//!
//! One argument per call is also what keeps the [order of
//! evaluation](../../../docs/spec/evaluation-semantics.md#order-of-evaluation): `g a b`
//! applies `g a` before it evaluates `b`, and `g(a)(b)` does too where `g(a, b)` would
//! not. Nothing here reorders a subexpression, and [nothing
//! short-circuits](../../../docs/spec/evaluation-semantics.md#nothing-short-circuits):
//! `&&` and `||` reach this module as ordinary applications of the functions their
//! `infix` declarations name, so they are emitted as calls, never as JavaScript's own
//! operators.
//!
//! # A facade
//!
//! [`emit`] answers a module for a `module foreign` facade too, so that an importer
//! needs no special case. Every [`unsafe`](../../../docs/spec/interop.md#an-unsafe-facade)
//! declaration becomes forwarding code that imports the companion's export under an
//! alias and calls it with exactly the parameters the signature's arrow count gives —
//! a plain function at arity one or more, a `const` at arity zero — the same
//! [plain-parameter-list promise](../../../docs/spec/interop.md#the-javascript-companion)
//! an ordinary declaration's call already keeps. [`Error::MissingCompanion`] is
//! answered instead when the caller says no companion sits beside this module for the
//! target being built ([*A facade names a boundary, not a
//! backend*](../../../docs/spec/interop.md#a-facade-names-a-boundary-not-a-backend)) —
//! [`emit`] has no path of its own to check that with, so the caller decides.
//!
//! A signature not marked `unsafe` declares an effect
//! ([An effectful facade](../../../docs/spec/interop.md#an-effectful-facade)), and its
//! forwarding code builds a `Task` instead of calling the companion: the `Task`'s run
//! function calls the runtime's `$effect` with a function that calls the companion, the
//! predicate of the payload type `a` of the signature's `Task (Result Failure a)` (`null`
//! for a `()` payload, which is discarded), the export's name and the continuation it was
//! handed. The companion is therefore called when the `Task` is run, never when it is built,
//! and `$effect` is what catches a throw or a rejection as `Err (Threw ..)` and turns a
//! value that fails the predicate into `Err (Malformed ..)`
//! ([DEC-22 decision 4](../../../docs/decisions/dec-22.md#4--the-wrapper-is-one-runtime-helper-and-a-synchronous-companion-continues-synchronously)).
//! The emitted text builds the `Task` as `{$: "Task", a: ($k) => $effect(..)}` and knows
//! nothing of `Done`. A facade constant naming a `Task` gets the same `Task`, as one
//! module-level `const`, and its companion export is called with no arguments each time the
//! `Task` is run ([DEC-22 decision
//! 7](../../../docs/decisions/dec-22.md#7--a-facade-constant-naming-a-task-gets-the-same-wrapper-with-no-arguments)).
//!
//! The companion is imported from [`companion_file`], beside the facade's own emitted
//! file and renamed so that the two do not share one path.
//!
//! # The boundary check
//!
//! A value a companion hands back is run through the predicate of the type its signature
//! declares ([Which types may cross the
//! boundary](../../../docs/spec/interop.md#which-types-may-cross-the-boundary)): the
//! forwarding code binds the companion's result to `$returned` and returns it unchanged
//! when the predicate holds, and calls the runtime's `$abort` otherwise, naming the
//! export — module and value — whose companion returned it ([When a program
//! aborts](../../../docs/spec/evaluation-semantics.md#when-a-program-aborts)). An
//! effectful facade's `$effect` runs the same predicate over the payload and routes a
//! failure to `Err (Malformed ..)` where this check aborts.
//!
//! **Only the result is checked.** An argument on its way out to the companion is a
//! Zelkova value the type checker already proved to be of its declared type, so nothing
//! is run over it.
//!
//! **A result of `()` is discarded, not checked**: the companion is called, and the
//! forwarding code returns `undefined` whatever it answered ([The unit value crosses as
//! `undefined`](../../../docs/spec/interop.md#the-unit-value-crosses-as-undefined)). A
//! facade constant of type `()` is `undefined`, its export still imported so that a
//! companion missing it fails to link.
//!
//! Every other admitted type is decided by an expression over the value, built by
//! `Predicates::test`: a scalar by its `typeof` — an `Int` also by `BigInt.asIntN(64, v)
//! === v`, which holds exactly for a `bigint` the 64-bit range holds, and a `Char` by
//! being a string of exactly one code point — a tuple as an array of its length whose
//! elements each pass, and a `()` nested anywhere as `v === undefined`. A union is
//! decided by a function of its own, `$is$<package>$<module>$<union>` (see `predicate`),
//! emitted into the facade that needs it: it reads `$` against the declaration's
//! constructors and checks each argument against the type that constructor declares for
//! it, and it takes one predicate per type variable of the union, so `Maybe Int` is
//! `$is$…$Maybe(v, ($v0) => …)`. A recursive union's function calls itself, which
//! terminates because a Zelkova value holds no cycle; the walk costs the size of the
//! value at every crossing.
//!
//! The declaration a union predicate is read off is the one its declaring module
//! canonicalized — every constructor of it, whether or not the module exposes them —
//! which is why [`emit`] is handed [`Unions`], every union of the build, rather than
//! reading the facade's imports: an importer's view of an opaque type has no
//! constructors. A result type with no predicate at all is [`Error::NoPredicate`].
//!
//! # A `case`
//!
//! [`ir::decision_tree`] turns a `case`'s branches — and a parameter written as a
//! pattern, which the IR holds as a single-branch match on it
//! ([`ir::CaseForm::Parameter`]) and which reaches `Emitter::case_expression` the
//! same way — into the [`Decision`] tree a backend walks instead of re-deriving which
//! test distinguishes which branch. `Emitter::case_expression` binds the scrutinee to
//! `$scrutinee` once, since the tree tests it more than once and re-evaluating it per
//! test would evaluate it once per test — observable through non-termination ([Order of
//! evaluation](../../../docs/spec/evaluation-semantics.md#order-of-evaluation)) — and
//! walks the tree into an `if`/`else` chain inside an immediately invoked function,
//! since a `case` is an expression and JavaScript's `if` is a statement. A
//! [`Decision::Test`] becomes an `if` on the value `occurrence_expr` reads off
//! `$scrutinee`: `.$ === "Ctor"` for a constructor, an equality check for a literal. A
//! [`Decision::Leaf`] declares its bindings as `const`s ahead of a `return`, all of it
//! inside its own block — a binding may repeat a name the scrutinee expression reads
//! ([Variable patterns](../../../docs/spec/patterns.md#variable-patterns)), and without
//! that block the two would share a scope, putting the earlier read in the later
//! binding's temporal dead zone. A [`Decision::Fail`] — the fall-through a `case`
//! missing a branch reaches, because [coverage is not checked
//! yet](../../../docs/spec/evaluation-semantics.md#two-outcomes) — calls the runtime's
//! `$abort`, naming the declaration the `case` was written in and, for a parameter
//! written as a pattern, saying so rather than naming a `case` the source never wrote.
//!
//! # What is refused
//!
//! [`emit`] answers an [`Error`] rather than a module missing a part: for a declaration
//! with no IR ([`ir::Module::unchecked`]), for a facade declaration with no type signature,
//! for a facade signature not marked `unsafe` whose result is not `Task (Result Failure a)`,
//! for a facade with no companion for the target being built, for a facade result no
//! predicate can decide, and for a construct it does not emit yet ([`Construct`]).

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::PathBuf;

use zelkova_compiler::canonical::{self, ExportType, Exports, Value};
use zelkova_compiler::ir::{
    self, decision_tree, CaseForm, Decision, LiteralValue, Occurrence, Outcome, ReferenceKind,
    Saturation, Step, TypedTerm, TypedTermKind,
};
use zelkova_compiler::name::{Name, QualName};
use zelkova_compiler::typer::Type;
use zelkova_compiler::{scalars, CheckedModule, ModuleName, PackageName, PhaseError, SpanLabel};
use zelkova_syntax::position::NodeSpan;

// ── Errors ────────────────────────────────────────────────────────────────────

/// Why a module could not be emitted.
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    /// A `module foreign` facade with no companion for the target being built
    /// ([*A facade names a boundary, not a
    /// backend*](../../../docs/spec/interop.md#a-facade-names-a-boundary-not-a-backend)).
    ///
    /// [`emit`] has no path of its own to check a companion's presence on disk with —
    /// its caller does, and says so through [`emit`]'s `has_companion` parameter.
    MissingCompanion { module: Name, target: &'static str },
    /// A facade signature not marked `unsafe` whose result type is not `Task (Result
    /// Failure a)`, so there is no payload for the wrapper to check
    /// ([An effectful facade](../../../docs/spec/interop.md#an-effectful-facade)).
    /// Canonicalization refuses such a signature first
    /// (`canonical::Error::FacadeResultNotEffect`); this is what is left if one reaches
    /// the backend anyway.
    NotAnEffect { name: Name, span: NodeSpan },
    /// A facade declaration the canonical module holds no type signature for, so there
    /// is no result type to build a wrapper or a boundary check from. Canonicalization
    /// refuses an unannotated facade declaration first; this is what is left if one
    /// reaches the backend anyway.
    NoSignature { name: Name, span: NodeSpan },
    /// A declaration the typer could not check has no IR to emit, and a module emitted
    /// without it would be missing a value its source declares.
    Unchecked { name: Name, span: NodeSpan },
    /// A facade signature whose result type has no predicate, so the value its
    /// companion hands back cannot be checked ([Which types may cross the
    /// boundary](../../../docs/spec/interop.md#which-types-may-cross-the-boundary)).
    ///
    /// The signature's own type expression is held to the admitted forms before this
    /// phase (`canonical::Error::FacadeTypeNotAdmitted`), but a union it names is
    /// checked through its constructors' declared arguments, which that check does not
    /// read — so a union holding a function type is refused here, where its predicate
    /// is built. Catching it in canonicalization instead is `BUG-45`. `constructor` is
    /// the innermost constructor the offending type was declared in, when it was one.
    NoPredicate {
        name: Name,
        span: NodeSpan,
        found: Unpredicated,
        constructor: Option<QualName>,
    },
    /// A construct this backend does not emit yet.
    Unsupported {
        construct: Construct,
        /// The declaration it was written in.
        declaration: Name,
        span: NodeSpan,
    },
}

/// The expression forms [`Error::Unsupported`] names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Construct {
    /// A `let` expression. The front end does not accept one yet, so the IR never holds
    /// one; it is named so that meeting one is an error rather than a panic.
    Let,
    /// A function nested inside a declaration's body. The language has no lambda and
    /// [`ir::build`] takes every parameter off the body, so the IR never holds one either.
    Lambda,
}

impl Construct {
    fn describe(self) -> &'static str {
        match self {
            Construct::Let => "a `let` expression",
            Construct::Lambda => "an anonymous function",
        }
    }
}

/// The part of a facade's result type that no predicate decides — see
/// [`Error::NoPredicate`].
#[derive(Debug, Clone, PartialEq)]
pub enum Unpredicated {
    /// A function type: `typeof v === "function"` decides that a value is *some*
    /// function, not the one declared.
    Function,
    /// A type variable, which excludes no value. Only a union's own variables are
    /// bound, each to the predicate of the type it is applied to.
    Variable(Name),
    /// A union no module of the build declares, so there are no constructors to read
    /// `$` against. [`Unions::of`] over every checked module leaves none.
    Undeclared(QualName),
}

impl Unpredicated {
    fn describe(&self) -> String {
        match self {
            Unpredicated::Function => {
                "a function type, which no predicate can tell apart from another".to_string()
            }
            Unpredicated::Variable(name) => format!(
                "the type variable `{}`, which excludes no value",
                name.as_str()
            ),
            Unpredicated::Undeclared(union) => format!(
                "`{}`, whose declaration is not part of this build",
                union.to_name().as_str()
            ),
        }
    }
}

impl PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::MissingCompanion { module, target } => format!(
                "the facade `{}` has no companion for the `{}` target",
                module.as_str(),
                target
            ),
            Error::NotAnEffect { name, .. } => format!(
                "`{}` is not marked `unsafe`, and its result is not `Task (Result Failure a)`, so no wrapper can be built for it",
                name.as_str()
            ),
            Error::NoSignature { name, .. } => format!(
                "the facade declaration `{}` has no type signature, so its boundary cannot be built",
                name.as_str()
            ),
            Error::Unchecked { name, .. } => format!(
                "`{}` cannot be compiled to JavaScript, because the type checker could not check it",
                name.as_str()
            ),
            Error::NoPredicate {
                name,
                found,
                constructor,
                ..
            } => {
                let within = match constructor {
                    Some(constructor) => format!(
                        ", declared as an argument of the constructor `{}`",
                        constructor.to_name().as_str()
                    ),
                    None => String::new(),
                };
                format!(
                    "the value `{}`'s companion returns cannot be checked where it crosses into Zelkova: its type holds {}{}",
                    name.as_str(),
                    found.describe(),
                    within
                )
            }
            Error::Unsupported {
                construct,
                declaration,
                ..
            } => format!(
                "`{}` cannot be compiled to JavaScript yet: it uses {}",
                declaration.as_str(),
                construct.describe()
            ),
        }
    }

    fn labels(&self) -> Vec<SpanLabel> {
        let (span, message) = match self {
            // No span exists for a `module foreign` line today — nothing in the parsed
            // or canonical AST carries the module header's position, only each
            // declaration's — so this names the facade and the target in `message()`
            // alone.
            Error::MissingCompanion { .. } => return Vec::new(),
            Error::NotAnEffect { span, .. } => (span, "not marked `unsafe`"),
            Error::NoSignature { span, .. } => (span, "this declaration"),
            Error::Unchecked { span, .. } => (span, "this declaration"),
            Error::NoPredicate { span, .. } => (span, "this signature"),
            Error::Unsupported { span, .. } => (span, "not supported by the JavaScript backend"),
        };

        match span.span() {
            Some(span) => vec![SpanLabel {
                span,
                message: message.to_owned(),
                primary: true,
                file: None,
            }],
            None => Vec::new(),
        }
    }
}

// ── Names ─────────────────────────────────────────────────────────────────────

/// The words a Zelkova name is renamed away from, because JavaScript does not accept
/// them as a binding's name in module code, or because a local binding of that name
/// would change what an unrelated expression emits as.
///
/// Every reserved word of ECMAScript — including the ones reserved only in strict
/// mode, which module code always is, and `await`, reserved in a module — plus `eval`
/// and `arguments`, which strict mode forbids binding. The Zelkova keywords among
/// them never reach here as a name; they are listed anyway, so that this is the
/// JavaScript list and not a guess at which part of it Zelkova can spell.
///
/// `undefined` is the one entry that is not a keyword: it is an ordinary global that a
/// local binding can shadow, and `undefined` is a legal Zelkova name. Every `()` this
/// module emits is the identifier `undefined`
/// ([*The unit value crosses as
/// `undefined`*](../../../docs/spec/interop.md#the-unit-value-crosses-as-undefined)), so
/// a Zelkova binding called `undefined` left unmangled would shadow the global and change
/// what every `()` in its scope reads as.
const RESERVED: &[&str] = &[
    "arguments",
    "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "eval",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "implements",
    "import",
    "in",
    "instanceof",
    "interface",
    "let",
    "new",
    "null",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "static",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "undefined",
    "var",
    "void",
    "while",
    "with",
    "yield",
];

/// The JavaScript name of a name the Zelkova source wrote: a declaration of this module,
/// a parameter, or a name a pattern bound.
///
/// **The scheme.** A name in [`RESERVED`] gets a `$` prefix — `class` is `$class` — and
/// every other name is left as it is: `classy` is `classy`. A Zelkova identifier is
/// letters and digits as Unicode classifies them, and `_` — what JavaScript accepts in an
/// identifier too — and never contains a `$`. That is what makes the scheme unable to merge two names: a name left
/// alone has no `$`, a renamed one starts with the only `$` it has, and the prefix is
/// removed by reading past it.
///
/// Every other name an emitted module declares is the emitter's own, and each is built
/// so that it cannot be one this function returns, nor one another item here returns:
///
/// - the runtime's helpers, `$curry`, `$abort` and `$effect`, the `$scrutinee` a `case`
///   binds, the `$k` an effectful facade's `Task` is handed, and
///   the names a boundary check binds — `$returned`, the value a companion handed back,
///   and inside a union predicate `$v0`, `$v1`, … for the value under test and `$p0`,
///   `$p1`, … for the predicates of the union's type variables: a `$` and a word that is
///   not in [`RESERVED`], and no other `$`;
/// - a union predicate, [`predicate`]: `$is`, then the same segments as a hoisted
///   constructor's, so its second segment is a package — lowercase, where the second
///   segment of a hoisted constructor, even of a package named `is`, is a module's and
///   uppercase;
/// - a value imported from another module, [`imported`]: it contains a `$`, does not
///   start with one, and its first segment — the package — is lowercase, where a module
///   segment is uppercase;
/// - a hoisted constructor, [`hoisted`]: a `$` followed by at least three more segments,
///   each separated from the next by a `$` — package, module segments, constructor;
/// - the facade's alias for its companion's export, [`companion_alias`]: `$companion$`
///   and the value's name, which holds no `$` — a `$`, a word not in [`RESERVED`], and
///   exactly one more `$`, so one segment fewer than any hoisted constructor;
/// - a wildcard parameter, [`wildcard`]: `$_` and a number, and `_0`, `_1`, … are not in
///   [`RESERVED`];
/// - a parameter written as a pattern, [`ir::pattern_parameter`]: `$` and a number,
///   which this function leaves as it is, since nothing in [`RESERVED`] holds a `$`.
///
/// Two imports, or two hoisted constructors, are told apart by their segments: none of
/// them holds a `$`, so the segments come back out by splitting at it, and together
/// they are the declaration's full identity — package, module and name — which is
/// unique in a build.
fn mangle(name: &str) -> String {
    if RESERVED.contains(&name) {
        format!("${}", name)
    } else {
        name.to_string()
    }
}

/// The name of the `position`th parameter of a declaration, when the source wrote `_`
/// there: `$_0`, `$_1`, ….
///
/// Nothing reads it; it exists because two parameters of one JavaScript function may not
/// share a name, and `f _ _` has two. See [`mangle`] for why it cannot meet another name.
fn wildcard(position: usize) -> String {
    format!("$_{}", position)
}

/// A package's name as the first segment of a local name: each `-` replaced by `_` —
/// `zelkova-core` is `zelkova_core`.
///
/// This is injective because a legal package name holds no `_`, and always a valid start
/// of a JavaScript identifier because a package name starts with a lowercase letter.
/// It is the package's own name and never its [namespace](zelkova_compiler::PackageName::namespace),
/// which is how one dependent spells the package rather than what the package is.
fn package_segment(package: &str) -> String {
    package.replace('-', "_")
}

/// The module-level constant a constructor of no arguments is hoisted to: `$`, then the
/// package declaring its union ([`package_segment`]), the segments of the module
/// declaring it and its own name, joined by `$` — `Test`'s `Red` in package `app` is
/// `$app$Test$Red`.
///
/// It is the same whether the module declaring the union hoists it or a module
/// mentioning it does.
fn hoisted(union: &QualName, constructor: &Name) -> String {
    format!(
        "${}${}${}",
        package_segment(union.package().as_str()),
        union.module_name().as_str().replace('.', "$"),
        constructor.as_str()
    )
}

/// The function deciding whether a value belongs to `union`: `$is`, then the package
/// declaring it ([`package_segment`]), its module's segments and its own name, joined by
/// `$` — `Maybe.Maybe` of `zelkova-core` is `$is$zelkova_core$Maybe$Maybe`.
///
/// Emitted by the facade whose boundary check calls it, never exported: two facades
/// checking one union each hold their own copy, as two importers of a constructor each
/// hoist their own constant.
fn predicate(union: &QualName) -> String {
    format!(
        "$is${}${}${}",
        package_segment(union.package().as_str()),
        union.module_name().as_str().replace('.', "$"),
        union.unqualified_name().as_str()
    )
}

/// The local name a value another module declares is imported under: the package
/// declaring it ([`package_segment`]), the module's segments and the value's name,
/// joined by `$` — `Maybe.withDefault` is `zelkova_core$Maybe$withDefault`.
///
/// A value is never imported under its own name, because this module may declare the
/// same name itself; and never under its module's name alone, because two packages may
/// each declare a module of that name, and this module may import both.
fn imported(package: &str, module: &str, name: &str) -> String {
    format!(
        "{}${}${}",
        package_segment(package),
        module.replace('.', "$"),
        name
    )
}

/// The local name a facade imports its companion's export `name` under:
/// `$companion$<name>`.
fn companion_alias(name: &str) -> String {
    format!("$companion${}", name)
}

/// The name of the field a constructor's argument at `index` is stored in: `a`, `b`, …
/// `z`, then `aa`, `ab`, … — the spreadsheet column sequence, which carries on the
/// published `a`, `b`, `c` past the alphabet without two indices sharing a name.
fn field(index: usize) -> String {
    let mut letters = Vec::new();
    let mut remaining = index + 1;

    while remaining > 0 {
        remaining -= 1;
        letters.push((b'a' + (remaining % 26) as u8) as char);
        remaining /= 26;
    }

    letters.iter().rev().collect()
}

// ── Paths ─────────────────────────────────────────────────────────────────────
//
// The output of a build is one tree, `build/out/js/` beside the root package's manifest
// ([`DEC-18` decision 5](../../../docs/decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest)):
//
// ```text
// build/out/js/
//   zelkova.mjs                      the runtime, RUNTIME_FILE
//   main.mjs                         the entry point `zelkova run` adds, program_runner::MAIN_FILE
//   zelkova-core/                    one directory per package of the build
//     Maybe.mjs                      one file per module, module_file
//     Js/Basics.mjs                  a facade's module, like any other
//     Js/Basics.companion.mjs        that facade's companion, companion_file
// ```
//
// The functions below are the only places a path into that tree is built, and the
// specifiers are built from the same shape, so what is written and what is imported
// cannot disagree. `compile_package` writes the tree.

/// The runtime's file name, at the root of `build/out/js/`, above every package's directory.
pub const RUNTIME_FILE: &str = "zelkova.mjs";

/// The runtime module's text, the one file of the output that is not generated.
///
/// It is embedded in the compiler binary when the compiler is built, from
/// `runtime/js/zelkova.mjs` in the compiler's own tree, and written out from here. A
/// compiler that looked for that file at run time would have to find its own source
/// tree — relative to the binary, the working directory or an environment variable —
/// and every one of those is wrong for a binary run from anywhere else. Embedding it
/// leaves nothing to look up and no way to fail but writing, and a compiler always
/// writes the runtime it was built with.
pub const RUNTIME: &str = include_str!("../../runtime/js/zelkova.mjs");

/// How many directories below its package's output directory the emitted file for
/// `module` sits: one per segment of its name but the last.
fn depth(module: &Name) -> usize {
    module.as_str().split('.').count() - 1
}

/// Where the module named `module` is written, relative to its package's directory:
/// one directory per segment of its name but the last — `Js.Basics` is
/// `Js/Basics.mjs`.
///
/// This is the module's name **within its own package**, never the namespace a
/// dependent reaches it by, so each package's modules have one path whichever package
/// imports them.
pub fn module_file(module: &Name) -> PathBuf {
    let mut path: PathBuf = module.as_str().split('.').collect();
    path.set_extension("mjs");
    path
}

/// Where the companion of the facade named `module` is written, relative to its
/// package's directory: beside the facade's own module, named after it with
/// `.companion.mjs` — `Js.Basics`'s is `Js/Basics.companion.mjs`.
///
/// It cannot keep the name it has beside the `.zel` source, `Basics.mjs`, because that
/// is the facade's own [`module_file`]. It is the companion that is renamed rather than
/// the facade, because the facade's path is the one every importer builds from a module
/// name. No module's file can be called this: a segment of a module name holds no `.`,
/// so no [`module_file`] has two in its file name.
///
/// The companion is copied byte for byte, so one that imports a sibling `.mjs` by a
/// relative specifier finds the emitted module of that name there, not the sibling it
/// was written beside. The one import a build rewrites is a test companion's of the
/// companion it checks ([`test_companion_import`]).
pub fn companion_file(module: &Name) -> PathBuf {
    let mut path = module_file(module);
    path.set_extension("companion.mjs");
    path
}

/// The specifier the module `from` imports the module named `to`, declared by
/// `package`, by.
///
/// Within one package it is a path inside that package's directory. Across a package
/// boundary it climbs out of `from`'s package to `build/out/js/` and into `package`'s
/// sibling directory — which is why every package of the build has its own directory
/// rather than the whole build sharing one tree.
fn module_specifier(from: &ModuleName, package: &str, to: &Name) -> String {
    let to_path = to.as_str().replace('.', "/");

    if from.package().as_str() == package {
        let up = match depth(from.name()) {
            0 => "./".to_string(),
            depth => "../".repeat(depth),
        };
        format!("{}{}.mjs", up, to_path)
    } else {
        format!(
            "{}{}/{}.mjs",
            "../".repeat(depth(from.name()) + 1),
            package,
            to_path
        )
    }
}

/// The specifier the module named `from` imports the runtime by: [`RUNTIME_FILE`] at
/// the root of `build/out/js/`, one level above `from`'s package directory.
fn runtime_specifier(from: &Name) -> String {
    format!("{}{}", "../".repeat(depth(from) + 1), RUNTIME_FILE)
}

/// The specifier a facade named `module` imports its own companion by: its
/// [`companion_file`], which sits in the same directory — `Js.Basics` imports
/// `./Basics.companion.mjs`.
fn companion_specifier(module: &Name) -> String {
    let file = companion_file(module);
    let name = file
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_default();
    format!("./{}", name)
}

/// How the companion of the test facade `checks`, a module of a package's `tests/` root,
/// imports the companion of the facade `target`, a module of the same package's `src/`
/// root: the specifier it spells in its source, then the one that replaces it in the
/// build.
///
/// A test companion imports the companion it checks by the relative path between the two
/// files as they sit in the package's source
/// ([*Testing a companion*](../../../docs/spec/interop.md#testing-a-companion)): climb
/// out of `tests/` to the package's root, then down into `src/` — from
/// `tests/Js/BasicsChecks.mjs`, `Js.Basics`'s is `../../src/Js/Basics.mjs`. The build
/// holds both roots' modules in one package directory and renames each companion
/// ([`companion_file`]), so that path reaches nothing there. What does is the path from
/// `checks`' [`companion_file`] to `target`'s, climbing to the package's directory and
/// down again — `../Js/Basics.companion.mjs` — which is the companion the build wrote
/// for `target`, the one the facade `target` calls.
///
/// Only the shortest spelling is produced: a specifier that reaches the same file by a
/// longer path, `./../../src/Js/Basics.mjs`, is not this one.
pub fn test_companion_import(checks: &Name, target: &Name) -> (String, String) {
    let target_path = target.as_str().replace('.', "/");
    let source = format!("{}src/{}.mjs", "../".repeat(depth(checks) + 1), target_path);
    let build = match depth(checks) {
        0 => format!("./{}.companion.mjs", target_path),
        depth => format!("{}{}.companion.mjs", "../".repeat(depth), target_path),
    };
    (source, build)
}

/// `text` with every string literal spelling one of `imports`' first specifiers replaced
/// by the same literal spelling its second, with the quote it was written with.
///
/// Only a literal delimited by `'` or `"` and holding exactly that specifier is replaced —
/// a template literal, or the specifier as part of a longer string, is left as it is. The
/// text is not parsed, so a literal inside a comment is replaced along with the ones in
/// `import` declarations: a comment holding the exact quoted path to a companion under
/// test is the only thing that changes that should not.
pub fn rewrite_imports(text: &str, imports: &[(String, String)]) -> String {
    let mut text = text.to_string();
    for (from, to) in imports {
        for quote in ['\'', '"'] {
            text = text.replace(
                &format!("{quote}{from}{quote}"),
                &format!("{quote}{to}{quote}"),
            );
        }
    }
    text
}

// ── Literals ──────────────────────────────────────────────────────────────────

/// A JavaScript string literal holding `c` and nothing else.
fn char_literal(c: char) -> String {
    format!("\"{}\"", escaped(c))
}

/// A JavaScript string literal holding `text`, each character escaped as
/// [`char_literal`] escapes it.
fn string_literal(text: &str) -> String {
    format!("\"{}\"", text.chars().map(escaped).collect::<String>())
}

/// `c` as it is written inside a JavaScript string literal.
fn escaped(c: char) -> String {
    match c {
        '"' => "\\\"".to_string(),
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        // Control characters, and the two line terminators JavaScript source treats
        // specially, as escapes rather than raw.
        c if c.is_control() || c == '\u{2028}' || c == '\u{2029}' => {
            format!("\\u{{{:x}}}", c as u32)
        }
        c => c.to_string(),
    }
}

/// A JavaScript number literal for `f`.
///
/// `{:?}` is Rust's shortest representation that reads back as the same `f64`, and
/// every finite one it writes — `1.0`, `0.1`, `1e300` — is also a JavaScript number
/// literal of that value. No literal a source can write is infinite or `NaN`; they are
/// covered so that nothing here can write something that is not JavaScript.
fn float_literal(f: f64) -> String {
    if f.is_nan() {
        "NaN".to_string()
    } else if f == f64::INFINITY {
        "Infinity".to_string()
    } else if f == f64::NEG_INFINITY {
        "-Infinity".to_string()
    } else {
        format!("{:?}", f)
    }
}

// ── Emitting a module ─────────────────────────────────────────────────────────

/// The JavaScript text of `module`, or every reason it cannot be emitted.
///
/// `has_companion` is irrelevant to any module but a `module foreign` facade, for which
/// it says whether a JavaScript companion sits beside it for the target being built.
/// [`emit`] has no path of its own to check that with — the caller does, since only it
/// knows where `module`'s source came from — so a facade with no companion is
/// [`Error::MissingCompanion`] rather than something this function discovers.
///
/// `unions` is where a facade's boundary checks read each union they reach off its
/// declaration; see [`Unions`]. A module that is not a facade reads nothing from it.
///
/// See this module's documentation for the shape of the text.
pub fn emit(
    module: &CheckedModule,
    has_companion: bool,
    unions: &Unions,
) -> Result<String, Vec<Error>> {
    let ir = &module.ir;

    if ir.foreign && !has_companion {
        return Err(vec![Error::MissingCompanion {
            module: ir.name.name().clone(),
            target: "javascript",
        }]);
    }

    let mut emitter = Emitter {
        arities: ir
            .declarations
            .iter()
            .map(|declaration| (declaration.name.clone(), declaration.arity))
            .collect(),
        runtime: BTreeSet::new(),
        imports: BTreeMap::new(),
        module: ir.name.clone(),
        imported_constructors: BTreeMap::new(),
        errors: ir
            .unchecked
            .iter()
            .map(|unchecked| Error::Unchecked {
                name: unchecked.name.clone(),
                span: unchecked.span,
            })
            .collect(),
        declaration: None,
    };

    let mut functions = Vec::new();
    let mut constants: HashMap<&Name, String> = HashMap::new();
    let mut companion_imports: Vec<String> = Vec::new();
    let mut predicates = Predicates {
        unions,
        functions: BTreeMap::new(),
    };

    for declaration in &ir.declarations {
        emitter.declaration = Some(declaration.name.clone());

        if ir.foreign {
            emitter.facade_declaration(
                &module.canonical,
                declaration,
                &mut predicates,
                &mut functions,
                &mut constants,
                &mut companion_imports,
            );
            continue;
        }

        // Every declaration of a module that is not a facade has a body; one without
        // would be a facade signature, handled above.
        let Some(body) = &declaration.body else {
            continue;
        };

        let expression = emitter.expression(&body.expression);
        let name = mangle(declaration.name.as_str());

        if body.parameters.is_empty() {
            constants.insert(
                &declaration.name,
                format!("const {} = {};", name, expression),
            );
        } else {
            let parameters: Vec<String> = body
                .parameters
                .iter()
                .enumerate()
                .map(|(position, parameter)| match parameter.name.as_str() {
                    "_" => wildcard(position),
                    other => mangle(other),
                })
                .collect();

            functions.push(format!(
                "function {}({}) {{\n  return {};\n}}",
                name,
                parameters.join(", "),
                expression
            ));
        }
    }

    if !emitter.errors.is_empty() {
        return Err(emitter.errors);
    }

    // The union predicates the boundary checks call go ahead of every other function,
    // in name order — a function declaration is hoisted, so the order is for the reader.
    let functions: Vec<String> = predicates
        .functions
        .into_values()
        .chain(functions)
        .collect();

    // The parameterless bindings, in the order they have to be initialised. Every one
    // of them is in that order; a binding that somehow was not would still be emitted,
    // after the rest and in name order, rather than lost.
    let mut ordered = Vec::new();
    for name in &ir.initialisation_order {
        if let Some(constant) = constants.remove(name) {
            ordered.push(constant);
        }
    }
    let mut leftover: Vec<(&Name, String)> = constants.into_iter().collect();
    leftover.sort_by(|left, right| left.0.as_str().cmp(right.0.as_str()));
    ordered.extend(leftover.into_iter().map(|(_, constant)| constant));

    let mut hoisted = hoisted_constructors(ir);
    hoisted.extend(
        emitter
            .imported_constructors
            .iter()
            .map(|(local, name)| format!("const {} = {{$: \"{}\"}};", local, name.as_str())),
    );

    let mut sections: Vec<String> = Vec::new();

    let mut imports = Vec::new();
    if !emitter.runtime.is_empty() {
        let helpers: Vec<&str> = emitter.runtime.iter().copied().collect();
        imports.push(format!(
            "import {{ {} }} from \"{}\";",
            helpers.join(", "),
            runtime_specifier(ir.name.name())
        ));
    }
    if !companion_imports.is_empty() {
        imports.push(format!(
            "import {{ {} }} from \"{}\";",
            companion_imports.join(", "),
            companion_specifier(ir.name.name())
        ));
    }
    for ((package, from), names) in &emitter.imports {
        let specifiers: Vec<String> = names
            .iter()
            .map(|name| format!("{} as {}", name, imported(package, from, name)))
            .collect();
        imports.push(format!(
            "import {{ {} }} from \"{}\";",
            specifiers.join(", "),
            module_specifier(&ir.name, package, &Name::new(from.as_str()))
        ));
    }

    for section in [imports, hoisted, ordered_functions(functions), ordered] {
        if !section.is_empty() {
            sections.push(section.join("\n"));
        }
    }

    let exports = exports(module);
    if !exports.is_empty() {
        sections.push(format!("export {{ {} }};", exports.join(", ")));
    }

    let mut text = sections.join("\n\n");
    text.push('\n');
    Ok(text)
}

/// Functions are separated from each other by a blank line, which joining the section
/// with one more newline gives.
fn ordered_functions(functions: Vec<String>) -> Vec<String> {
    functions
        .into_iter()
        .enumerate()
        .map(|(index, function)| {
            if index == 0 {
                function
            } else {
                format!("\n{}", function)
            }
        })
        .collect()
}

/// One `const` per constructor of no arguments the module declares, unions in the IR's
/// name order and each union's constructors in declaration order.
///
/// `Bool`'s two are not among them: they are `true` and `false`.
fn hoisted_constructors(ir: &ir::Module) -> Vec<String> {
    ir.unions
        .iter()
        .filter(|union| !scalars::BOOL.declares(&union.name))
        .flat_map(|union| {
            union
                .variants
                .iter()
                .filter(|variant| variant.arity == 0)
                .map(move |variant| {
                    format!(
                        "const {} = {{$: \"{}\"}};",
                        hoisted(&union.name, &variant.name),
                        variant.name.as_str()
                    )
                })
        })
        .collect()
}

/// The export specifiers for the values `module` exposes, in name order: a value its
/// `exposing` header names, or the function an exposed operator stands for.
///
/// Each is exported under its Zelkova name, so a renamed one reads `$class as class` —
/// an export's name may be a reserved word where a binding's may not.
fn exports(module: &CheckedModule) -> Vec<String> {
    let exposed = |name: &Name| match &module.canonical.exports {
        Exports::Everything => true,
        Exports::Specifics(specifics) => {
            specifics.get(name) == Some(&ExportType::Value)
                || specifics.iter().any(|(symbol, kind)| {
                    *kind == ExportType::Infix
                        && module
                            .canonical
                            .infixes
                            .get(symbol)
                            .is_some_and(|infix| &infix.function_name == name)
                })
        }
    };

    module
        .ir
        .declarations
        .iter()
        .filter(|declaration| exposed(&declaration.name))
        .map(|declaration| {
            let local = mangle(declaration.name.as_str());
            if local == declaration.name.as_str() {
                local
            } else {
                format!("{} as {}", local, declaration.name.as_str())
            }
        })
        .collect()
}

// ── The boundary check ────────────────────────────────────────────────────────

/// Every union a build declares, by its qualified name, as its declaring module
/// canonicalized it: what a facade's boundary check reads a union's constructors, and
/// the type each declares for each of its arguments, off.
///
/// It is built from the checked modules themselves, never from an [`Interface`](zelkova_compiler::Interface):
/// an interface exposing a type without its constructors hands over none of them, and
/// the predicate of such a type still reads `$` against every constructor its
/// declaration has.
#[derive(Debug, Default)]
pub struct Unions(HashMap<QualName, canonical::UnionType>);

impl Unions {
    /// Every union declared by one of `modules`. `compile_package` passes every module
    /// of the build, so each union a facade can name is one of them.
    pub fn of<'a>(modules: impl IntoIterator<Item = &'a CheckedModule>) -> Unions {
        let mut unions = HashMap::new();
        for module in modules {
            for (name, union) in &module.canonical.types {
                unions.insert(module.canonical.name.qualify_name(name), union.clone());
            }
        }
        Unions(unions)
    }
}

/// Why no predicate could be built: what was found, and the innermost constructor whose
/// declared argument held it, if it was inside one. Boxed, since it is the error half of
/// every `Result` the predicate builders return and is much larger than the text half.
type NoPredicate = Box<(Unpredicated, Option<QualName>)>;

/// The union predicates one facade's boundary checks call, built as the checks need
/// them.
struct Predicates<'a> {
    unions: &'a Unions,
    /// Each union predicate's text, by its name ([`predicate`]). A union's entry is
    /// inserted, empty, before its body is built, so a recursive union meets its own
    /// name here and calls it rather than building it again.
    functions: BTreeMap<String, String>,
}

impl Predicates<'_> {
    /// A JavaScript expression that is `true` exactly when `value` — an expression
    /// without side effects, evaluated as often as the test needs it — belongs to
    /// `tpe`. A conjunction of `===` tests and calls, so it needs no parentheses beside
    /// another `&&` or ahead of a `?`.
    ///
    /// `parameters` binds each type variable in scope — inside a union predicate, the
    /// union's own — to the name of the predicate its argument was given as. `depth`
    /// is how many parameters of nested arrow functions are already in scope, so each
    /// one gets its own name.
    fn test(
        &mut self,
        tpe: &canonical::Type,
        value: &str,
        depth: usize,
        parameters: &HashMap<&Name, String>,
    ) -> Result<String, NoPredicate> {
        match tpe {
            canonical::Type::Unit => Ok(format!("{} === undefined", value)),
            canonical::Type::Tuple(tuple) => {
                let mut tests = vec![
                    format!("Array.isArray({})", value),
                    format!("{}.length === {}", value, tuple.iter().count()),
                ];
                for (index, element) in tuple.iter().enumerate() {
                    tests.push(self.test(
                        element,
                        &format!("{}[{}]", value, index),
                        depth,
                        parameters,
                    )?);
                }
                Ok(tests.join(" && "))
            }
            canonical::Type::Variable(name) => match parameters.get(name) {
                Some(predicate) => Ok(format!("{}({})", predicate, value)),
                None => Err(Box::new((Unpredicated::Variable(name.clone()), None))),
            },
            canonical::Type::Arrow(..) => Err(Box::new((Unpredicated::Function, None))),
            canonical::Type::Type(name, arguments) => match scalars::scalar_of(name) {
                Some(scalars::INT) => Ok(format!(
                    "typeof {v} === \"bigint\" && BigInt.asIntN(64, {v}) === {v}",
                    v = value
                )),
                Some(scalars::FLOAT) => Ok(format!("typeof {} === \"number\"", value)),
                Some(scalars::BOOL) => Ok(format!("typeof {} === \"boolean\"", value)),
                // One code point: a string of one UTF-16 unit, or of the two a code
                // point past `0xFFFF` takes. An empty string's `codePointAt(0)` is
                // `undefined`, which asks for a length of one and fails it.
                Some(scalars::CHAR) => Ok(format!(
                    "typeof {v} === \"string\" && {v}.length === ({v}.codePointAt(0) > 0xFFFF ? 2 : 1)",
                    v = value
                )),
                Some(scalars::STRING) => Ok(format!("typeof {} === \"string\"", value)),
                _ => {
                    let function = self.union(name)?;
                    let mut call = vec![value.to_string()];
                    for argument in arguments {
                        let parameter = format!("$v{}", depth);
                        let test = self.test(argument, &parameter, depth + 1, parameters)?;
                        call.push(format!("({}) => {}", parameter, test));
                    }
                    Ok(format!("{}({})", function, call.join(", ")))
                }
            },
        }
    }

    /// The name of the predicate deciding `union`, building its function first when
    /// this facade has not needed it yet.
    ///
    /// The function takes the value as `$v0` and one predicate per type variable of the
    /// union, `$p0`, `$p1`, … in declaration order. It answers `false` for anything but
    /// a non-null object, and otherwise switches on `$`: each constructor tests its
    /// arguments, read off the fields [`field`] names, against the types it declares
    /// for them, and a `$` naming no constructor of the declaration is `false`.
    fn union(&mut self, union: &QualName) -> Result<String, NoPredicate> {
        let name = predicate(union);
        if self.functions.contains_key(&name) {
            return Ok(name);
        }

        let unions = self.unions;
        let Some(declaration) = unions.0.get(union) else {
            return Err(Box::new((Unpredicated::Undeclared(union.clone()), None)));
        };
        self.functions.insert(name.clone(), String::new());

        // A union no predicate can be built for leaves no entry behind, so another facade
        // declaration naming *that* union meets the same error rather than a call to a
        // function that was never finished. That covers this union only: a mutually
        // recursive union built successfully while this one was still in progress (it
        // found this union's placeholder and used it) keeps its own `functions` entry,
        // even though the removal below just deleted the predicate that entry calls — a
        // facade naming only the other union gets no diagnostic for that.
        match self.union_function(union, &name, declaration) {
            Ok(function) => {
                self.functions.insert(name.clone(), function);
                Ok(name)
            }
            Err(error) => {
                self.functions.remove(&name);
                Err(error)
            }
        }
    }

    /// The text of the function [`Predicates::union`] names `name`, deciding `union`
    /// against `declaration`.
    fn union_function(
        &mut self,
        union: &QualName,
        name: &str,
        declaration: &canonical::UnionType,
    ) -> Result<String, NoPredicate> {
        let predicates: Vec<String> = (0..declaration.variables.len())
            .map(|index| format!("$p{}", index))
            .collect();
        let parameters: HashMap<&Name, String> = declaration
            .variables
            .iter()
            .zip(predicates.iter().cloned())
            .collect();

        let mut cases = Vec::new();
        for constructor in &declaration.variants {
            let mut tests = Vec::new();
            for (index, argument) in constructor.type_parameters.iter().enumerate() {
                let value = format!("$v0.{}", field(index));
                let test =
                    self.test(argument, &value, 1, &parameters)
                        .map_err(|mut no_predicate| {
                            if no_predicate.1.is_none() {
                                no_predicate.1 = Some(union.sibling(&constructor.name));
                            }
                            no_predicate
                        })?;
                tests.push(test);
            }
            let test = if tests.is_empty() {
                "true".to_string()
            } else {
                tests.join(" && ")
            };
            cases.push(format!(
                "    case {}:\n      return {};",
                string_literal(constructor.name.as_str()),
                test
            ));
        }
        cases.push("    default:\n      return false;".to_string());

        let signature: Vec<&str> = std::iter::once("$v0")
            .chain(predicates.iter().map(String::as_str))
            .collect();
        Ok(format!(
            "function {}({}) {{\n  if (typeof $v0 !== \"object\" || $v0 === null) {{\n    return false;\n  }}\n  switch ($v0.$) {{\n{}\n  }}\n}}",
            name,
            signature.join(", "),
            cases.join("\n")
        ))
    }
}

/// The canonical type left once `arity` arrows are taken off the front of `tpe` — a
/// facade signature's result, `tpe` itself for a facade constant.
fn canonical_result(tpe: &canonical::Type, arity: usize) -> &canonical::Type {
    let mut result = tpe;
    for _ in 0..arity {
        match result {
            canonical::Type::Arrow(_, rest) => result = rest,
            _ => break,
        }
    }
    result
}

/// [`canonical_result`] over the typer's type of the same signature, which is the one
/// that can be written out the way the source spells it.
fn typer_result(tpe: &Type, arity: usize) -> &Type {
    let mut result = tpe;
    for _ in 0..arity {
        match result {
            Type::Fun { return_tpe, .. } => result = return_tpe,
            _ => break,
        }
    }
    result
}

// ── Emitting an expression ────────────────────────────────────────────────────

struct Emitter {
    /// How many parameters each of this module's declarations takes.
    arities: HashMap<Name, usize>,
    /// The runtime helpers the emitted text calls.
    runtime: BTreeSet<&'static str>,
    /// The values of other modules the emitted text mentions, by the package and the
    /// module declaring each.
    imports: BTreeMap<(String, String), BTreeSet<String>>,
    /// The module being emitted, package included, which tells a constructor it declares
    /// from one another module does — of this package or of another.
    module: ModuleName,
    /// The constructors of no arguments another module declares that the emitted text
    /// mentions, by the name each is hoisted to. This module hoists its own constant for
    /// each, since the declaring module exports none.
    imported_constructors: BTreeMap<String, Name>,
    errors: Vec<Error>,
    /// The declaration being emitted, which an [`Error::Unsupported`] names.
    declaration: Option<Name>,
}

impl Emitter {
    fn unsupported(&mut self, construct: Construct, span: NodeSpan) -> String {
        self.errors.push(Error::Unsupported {
            construct,
            declaration: self.declaration.clone().unwrap_or_else(|| Name::new("")),
            span,
        });
        // Never part of an emitted module: an error means `emit` answers `Err`.
        String::new()
    }

    fn curry(&mut self, function: &str, arity: usize) -> String {
        self.runtime.insert("$curry");
        format!("$curry({}, {})", function, arity)
    }

    /// One `module foreign` facade declaration's forwarding code, appended to
    /// `functions` or `constants` — a plain function at arity one or more, a `const`
    /// at arity zero — plus the companion import it needs, appended to
    /// `companion_imports`, and any union predicate its boundary check calls, added to
    /// `predicates`. See this module's doc comment, "The boundary check", for what the
    /// forwarding code does with the companion's result.
    ///
    /// A signature marked `unsafe` calls the companion directly. One that is not declares
    /// an effect ([An effectful
    /// facade](../../../docs/spec/interop.md#an-effectful-facade)), and its forwarding
    /// code is the `Task` this module's *A facade* section describes. A result type no predicate decides
    /// pushes [`Error::NoPredicate`] and appends nothing.
    fn facade_declaration<'a>(
        &mut self,
        canonical: &canonical::Module,
        declaration: &'a ir::Declaration,
        predicates: &mut Predicates,
        functions: &mut Vec<String>,
        constants: &mut HashMap<&'a Name, String>,
        companion_imports: &mut Vec<String>,
    ) {
        let (signature, marked_unsafe) = match canonical.values.get(&declaration.name) {
            Some(Value::TypedValue {
                marked_unsafe, tpe, ..
            }) => (tpe, *marked_unsafe),
            _ => {
                self.errors.push(Error::NoSignature {
                    name: declaration.name.clone(),
                    span: declaration.span,
                });
                return;
            }
        };

        // What the companion hands back: the signature with the arrows of its own
        // parameter list — `arity` of them — taken off the front. For an effect, the
        // payload of the `Task (Result Failure a)` that remains.
        let result = canonical_result(signature, declaration.arity);
        let result = if marked_unsafe {
            result
        } else {
            match canonical::effectful_result_payload(result) {
                Some(payload) => payload,
                None => {
                    self.errors.push(Error::NotAnEffect {
                        name: declaration.name.clone(),
                        span: declaration.span,
                    });
                    return;
                }
            }
        };

        // The companion's export is imported under an alias — never the plain name,
        // which this method is about to declare a local binding under, and a module
        // cannot import and declare the same name twice.
        let local = mangle(declaration.name.as_str());
        let alias = companion_alias(declaration.name.as_str());
        let parameters: Vec<String> = (0..declaration.arity).map(field).collect();
        let call = format!("{}({})", alias, parameters.join(", "));

        // `None` is a `()` result: discarded, so nothing is checked.
        let test = if matches!(result, canonical::Type::Unit) {
            None
        } else {
            match predicates.test(result, "$returned", 0, &HashMap::new()) {
                Ok(test) => Some(test),
                Err(no_predicate) => {
                    let (found, constructor) = *no_predicate;
                    self.errors.push(Error::NoPredicate {
                        name: declaration.name.clone(),
                        span: declaration.span,
                        found,
                        constructor,
                    });
                    return;
                }
            }
        };

        companion_imports.push(format!("{} as {}", declaration.name.as_str(), alias));

        if !marked_unsafe {
            let export = format!(
                "{}.{}",
                self.module.name().as_str(),
                declaration.name.as_str()
            );
            self.runtime.insert("$effect");
            let check = match &test {
                Some(test) => format!("($returned) => {}", test),
                None => "null".to_string(),
            };
            let task = format!(
                "{{$: \"Task\", a: ($k) => $effect(() => {}, {}, {}, $k)}}",
                call,
                check,
                string_literal(&export)
            );
            if declaration.arity == 0 {
                constants.insert(&declaration.name, format!("const {} = {};", local, task));
            } else {
                functions.push(format!(
                    "function {}({}) {{\n  return {};\n}}",
                    local,
                    parameters.join(", "),
                    task
                ));
            }
            return;
        }

        let Some(test) = test else {
            if declaration.arity == 0 {
                constants.insert(&declaration.name, format!("const {} = undefined;", local));
            } else {
                functions.push(format!(
                    "function {}({}) {{\n  {};\n  return undefined;\n}}",
                    local,
                    parameters.join(", "),
                    call
                ));
            }
            return;
        };

        self.runtime.insert("$abort");
        let description = string_literal(&format!(
            "`{}.{}`'s companion returned a value its declared type, `{}`, does not admit",
            self.module.name().as_str(),
            declaration.name.as_str(),
            typer_result(&declaration.tpe, declaration.arity)
        ));
        let checked = format!("{} ? $returned : $abort({})", test, description);

        if declaration.arity == 0 {
            constants.insert(
                &declaration.name,
                format!("const {} = (($returned) => {})({});", local, checked, alias),
            );
        } else {
            functions.push(format!(
                "function {}({}) {{\n  const $returned = {};\n  return {};\n}}",
                local,
                parameters.join(", "),
                call,
                checked
            ));
        }
    }

    fn expression(&mut self, term: &TypedTerm) -> String {
        match &term.kind {
            TypedTermKind::Int(i) => format!("{}n", i),
            TypedTermKind::Float(f) => float_literal(*f),
            TypedTermKind::Char(c) => char_literal(*c),
            TypedTermKind::String(s) => string_literal(s),
            TypedTermKind::Bool(b) => b.to_string(),
            TypedTermKind::Identifier(reference) => self.value(&reference.name, &reference.kind),
            TypedTermKind::Apply { .. } => self.application(term),
            TypedTermKind::If {
                cond,
                true_branch,
                false_branch,
            } => {
                let cond = self.operand(cond);
                let true_branch = self.expression(true_branch);
                let false_branch = self.expression(false_branch);
                format!("{} ? {} : {}", cond, true_branch, false_branch)
            }
            TypedTermKind::Tuple(tuple) => {
                let elements: Vec<String> = tuple
                    .iter()
                    .map(|element| self.expression(element))
                    .collect();
                format!("[{}]", elements.join(", "))
            }
            TypedTermKind::Case {
                scrutinee,
                branches,
                form,
            } => self.case_expression(scrutinee, branches, *form),
            TypedTermKind::Unit => "undefined".to_string(),
            TypedTermKind::Let { .. } => self.unsupported(Construct::Let, term.span),
            TypedTermKind::Fun { .. } => self.unsupported(Construct::Lambda, term.span),
        }
    }

    /// An expression in a position where a conditional has to be parenthesised: the
    /// condition of another conditional, and the function of a call.
    fn operand(&mut self, term: &TypedTerm) -> String {
        let text = self.expression(term);
        match term.kind {
            TypedTermKind::If { .. } => format!("({})", text),
            _ => text,
        }
    }

    /// A name used as a value rather than called.
    fn value(&mut self, name: &str, kind: &ReferenceKind) -> String {
        match kind {
            ReferenceKind::Local => mangle(name),
            ReferenceKind::TopLevel(qname) => {
                let unqualified = qname.unqualified_name();
                let local = mangle(unqualified.as_str());
                match self.arities.get(&unqualified).copied() {
                    Some(arity) if arity >= 2 => self.curry(&local, arity),
                    _ => local,
                }
            }
            ReferenceKind::Foreign(qname, package, arity) => {
                let local = self.import(qname, package);
                if *arity >= 2 {
                    self.curry(&local, *arity)
                } else {
                    local
                }
            }
            ReferenceKind::Constructor(ctor) => {
                if scalars::BOOL.declares(&ctor.union) {
                    match ctor.name.as_str() {
                        "True" => return "true".to_string(),
                        "False" => return "false".to_string(),
                        _ => {}
                    }
                }

                match ctor.arity {
                    0 => {
                        let local = hoisted(&ctor.union, &ctor.name);
                        // Compares the full identity field-wise instead of allocating a
                        // throwaway `ModuleName` just to compare it against `self.module`.
                        let declared_here = *ctor.union.package() == *self.module.package()
                            && ctor.union.module_name() == *self.module.name();
                        if !declared_here {
                            self.imported_constructors
                                .insert(local.clone(), ctor.name.clone());
                        }
                        local
                    }
                    arity => {
                        let parameters: Vec<String> = (0..arity).map(field).collect();
                        let function = format!(
                            "({}) => ({})",
                            parameters.join(", "),
                            tagged(&ctor.name, parameters.clone())
                        );
                        if arity >= 2 {
                            self.curry(&function, arity)
                        } else {
                            function
                        }
                    }
                }
            }
        }
    }

    /// The local name the value `qname` of another module is imported under, recording
    /// the import so the module's import section names it.
    fn import(&mut self, qname: &QualName, package: &PackageName) -> String {
        let module = qname.module_name().as_str().to_string();
        let name = qname.unqualified_name().as_str().to_string();
        let local = imported(package.as_str(), &module, &name);
        self.imports
            .entry((package.as_str().to_string(), module))
            .or_default()
            .insert(name);
        local
    }

    /// An application spine: a direct call up to the node the IR marks saturated, if
    /// there is one, then one call per remaining argument.
    fn application(&mut self, term: &TypedTerm) -> String {
        let mut arguments = Vec::new();
        let mut head = term;
        while let TypedTermKind::Apply {
            fun,
            arg,
            saturation,
        } = &head.kind
        {
            arguments.push((arg.as_ref(), *saturation));
            head = fun;
        }
        arguments.reverse();

        let saturated = arguments
            .iter()
            .position(|(_, saturation)| *saturation == Saturation::Saturated);

        // Evaluated in the order written: the function, then each argument.
        let (mut callee, rest) = match (&head.kind, saturated) {
            (TypedTermKind::Identifier(reference), Some(last)) => match &reference.kind {
                ReferenceKind::TopLevel(qname) => {
                    let supplied = self.arguments(&arguments[..=last]);
                    (
                        format!(
                            "{}({})",
                            mangle(qname.unqualified_name().as_str()),
                            supplied.join(", ")
                        ),
                        &arguments[last + 1..],
                    )
                }
                ReferenceKind::Foreign(qname, package, _) => {
                    let local = self.import(qname, package);
                    let supplied = self.arguments(&arguments[..=last]);
                    (
                        format!("{}({})", local, supplied.join(", ")),
                        &arguments[last + 1..],
                    )
                }
                ReferenceKind::Constructor(ctor) => {
                    let supplied = self.arguments(&arguments[..=last]);
                    let fields: Vec<String> = supplied;
                    (tagged(&ctor.name, fields), &arguments[last + 1..])
                }
                _ => (self.operand(head), &arguments[..]),
            },
            _ => (self.operand(head), &arguments[..]),
        };

        for (argument, _) in rest {
            let argument = self.expression(argument);
            callee = format!("{}({})", callee, argument);
        }

        callee
    }

    fn arguments(&mut self, arguments: &[(&TypedTerm, Saturation)]) -> Vec<String> {
        arguments
            .iter()
            .map(|(argument, _)| self.expression(argument))
            .collect()
    }

    /// A `case … of`, or a parameter written as a pattern — the IR's single-branch
    /// match on it ([`ir::CaseForm::Parameter`]), which lowers and emits the same way.
    /// See this module's doc comment, "A `case`", for the shape.
    ///
    /// `scrutinee` is evaluated exactly once — bound to `$scrutinee` before the tree is
    /// walked — never once per test, which is what a decision tree built by
    /// [`decision_tree`] wants: [Order of
    /// evaluation](../../../docs/spec/evaluation-semantics.md#order-of-evaluation).
    ///
    /// `form` says whether the source wrote a `case … of` or a parameter pattern
    /// ([`ir::CaseForm`]) and reaches [`abort_description`] unchanged, so a
    /// [`Decision::Fail`] this tree needs describes itself in the vocabulary the source
    /// actually used.
    ///
    /// Mutation-checked by re-emitting `self.expression(scrutinee)` at every
    /// [`Decision::Test`] instead of binding it once: a `case` whose scrutinee is a call
    /// then contains that call's text more than once.
    fn case_expression(
        &mut self,
        scrutinee: &TypedTerm,
        branches: &[(ir::TermPattern, Box<TypedTerm>)],
        form: CaseForm,
    ) -> String {
        let declaration = self.declaration.clone().unwrap_or_else(|| Name::new(""));
        let tree = decision_tree(&scrutinee.tpe, branches, &declaration);
        let scrutinee_expr = self.expression(scrutinee);
        let body = self.decision(&tree, "$scrutinee", 1, form);

        format!(
            "(() => {{\n  const $scrutinee = {};\n{}\n}})()",
            scrutinee_expr, body
        )
    }

    /// One [`Decision`] node as the statements of an `if`/`else` chain, each line
    /// indented `depth` levels of two spaces — the body of [`case_expression`]'s
    /// immediately invoked function. `root` is the name the scrutinee is bound to,
    /// which [`occurrence_expr`] reads a test's or a binding's value off; `form` is
    /// passed through unchanged to [`abort_description`].
    ///
    /// A [`Decision::Test`] nests: its `matched` and `default` are each a full
    /// [`Decision`] emitted one level deeper, so a chain of tests on the scrutinee comes
    /// out as `if`s nested in one another's `else`, never merged or reordered — the
    /// order [Conditional
    /// evaluation](../../../docs/spec/evaluation-semantics.md#conditional-evaluation)
    /// tries branches in. A [`Decision::Leaf`] declares its bindings as `const`s, each
    /// read off `root` by [`occurrence_expr`], ahead of a `return` of its body — all of
    /// it inside its own block, so a binding never shares a scope with the
    /// `$scrutinee` line above it. Sharing that scope is observable: a binding may name
    /// anything the pattern it comes from could ([Variable
    /// patterns](../../../docs/spec/patterns.md#variable-patterns) lets one repeat a
    /// name already in scope), and a `const` anywhere in a block puts every reference to
    /// that name earlier in the *same* block in its temporal dead zone — so without the
    /// leaf's own block, a scrutinee expression that happens to read a name a leaf also
    /// binds would throw `ReferenceError` before ever reaching the leaf. A
    /// [`Decision::Fail`] returns the runtime's `$abort`, importing it the way `$curry`
    /// is imported; it carries no binding, so it needs no block of its own.
    fn decision(&mut self, tree: &Decision, root: &str, depth: usize, form: CaseForm) -> String {
        let pad = "  ".repeat(depth);

        match tree {
            Decision::Test {
                scrutinee,
                outcome,
                matched,
                default,
            } => {
                let condition = test_condition(root, scrutinee, outcome);
                let matched = self.decision(matched, root, depth + 1, form);
                let default = self.decision(default, root, depth + 1, form);
                format!(
                    "{pad}if ({condition}) {{\n{matched}\n{pad}}} else {{\n{default}\n{pad}}}",
                    pad = pad,
                    condition = condition,
                    matched = matched,
                    default = default,
                )
            }
            Decision::Leaf { bindings, body } => {
                let inner_pad = "  ".repeat(depth + 1);
                let mut lines: Vec<String> = bindings
                    .iter()
                    .map(|binding| {
                        format!(
                            "{}const {} = {};",
                            inner_pad,
                            mangle(&binding.name),
                            occurrence_expr(root, &binding.occurrence)
                        )
                    })
                    .collect();
                lines.push(format!("{}return {};", inner_pad, self.expression(body)));
                format!("{pad}{{\n{}\n{pad}}}", lines.join("\n"), pad = pad)
            }
            Decision::Fail { declaration } => {
                self.runtime.insert("$abort");
                format!(
                    "{}return $abort({});",
                    pad,
                    abort_description(declaration, form)
                )
            }
        }
    }
}

/// The condition a [`Decision::Test`] compiles to: an equality check against the value
/// [`occurrence_expr`] reads off `root`. A constructor is tested by its `$` field — a
/// `Bool`'s constructors never reach here, since `typer::translate_pattern` normalises
/// `True`/`False` to the same [`Outcome::Literal`] a `true`/`false` pattern is — and a
/// literal by the value itself, which is also how a `case` on a `Bool` tests it (see
/// this module's doc comment, "Representations").
fn test_condition(root: &str, occurrence: &Occurrence, outcome: &Outcome) -> String {
    let value = occurrence_expr(root, occurrence);

    match outcome {
        Outcome::Literal(LiteralValue::Bool(b)) => format!("{} === {}", value, b),
        Outcome::Literal(LiteralValue::Int(i)) => format!("{} === {}n", value, i),
        Outcome::Literal(LiteralValue::Char(c)) => format!("{} === {}", value, char_literal(*c)),
        Outcome::Constructor(ctor) => format!("{}.$ === \"{}\"", value, ctor.name.as_str()),
    }
}

/// The JavaScript expression reading the value at `occurrence` off `root`, the name the
/// scrutinee is bound to: a constructor argument is a field ([`field`], the same one
/// [`tagged`] builds an object under), a tuple element an index — the representations
/// [A union crosses as a tagged
/// value](../../../docs/spec/interop.md#a-union-crosses-as-a-tagged-value) gives them.
fn occurrence_expr(root: &str, occurrence: &Occurrence) -> String {
    match occurrence {
        Occurrence::Root => root.to_string(),
        Occurrence::At(base, step) => {
            let base = occurrence_expr(root, base);
            match step {
                Step::ConstructorArgument(index) => format!("{}.{}", base, field(*index)),
                Step::TupleElement(index) => format!("{}[{}]", base, index),
            }
        }
    }
}

/// The description a [`Decision::Fail`]'s `$abort` call carries: which declaration's
/// `case` — or, for [`CaseForm::Parameter`], parameter pattern — matched no branch. That
/// leaf exists only because [coverage is not checked
/// yet](../../../docs/spec/evaluation-semantics.md#two-outcomes) (`LANG-19`) — reaching
/// it aborts rather than falling through to `undefined`. `form` keeps the message in the
/// vocabulary the source actually used: a parameter written as a pattern has no `case`
/// for the message to name.
fn abort_description(declaration: &Name, form: CaseForm) -> String {
    match form {
        CaseForm::Expression => {
            format!("\"`{}`'s case matched no branch\"", declaration.as_str())
        }
        CaseForm::Parameter => format!(
            "\"`{}`'s parameter pattern matched no branch\"",
            declaration.as_str()
        ),
    }
}

/// The tagged object a constructor builds, each argument in the field [`field`] names
/// for its position.
fn tagged(name: &Name, arguments: Vec<String>) -> String {
    let mut fields = vec![format!("$: \"{}\"", name.as_str())];
    fields.extend(
        arguments
            .into_iter()
            .enumerate()
            .map(|(index, argument)| format!("{}: {}", field(index), argument)),
    );
    format!("{{{}}}", fields.join(", "))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A reserved word is renamed and a name that merely starts with one is not.
    #[test]
    fn only_a_reserved_word_is_renamed() {
        assert_eq!(mangle("class"), "$class");
        assert_eq!(mangle("classy"), "classy");
        assert_eq!(mangle("eval"), "$eval");
        assert_eq!(mangle("curry"), "curry");
    }

    /// A value import and a hoisted constructor are named by the package, the module and
    /// the name, the package spelled with `_` for each `-`.
    ///
    /// Mutation-checked by leaving `-` as it is in `package_segment`: the names then hold
    /// a `-`, which no JavaScript identifier can, and the test goes red.
    #[test]
    fn a_local_name_carries_its_package() {
        assert_eq!(
            imported("zelkova-core", "Maybe", "withDefault"),
            "zelkova_core$Maybe$withDefault"
        );
        assert_eq!(
            hoisted(
                &QualName::in_module(
                    zelkova_compiler::PackageName::new("acme-widgets").unwrap(),
                    "Page.Size",
                    "Size"
                ),
                &Name::new("Small")
            ),
            "$acme_widgets$Page$Size$Small"
        );
    }

    /// The fields continue past `z` without reusing a name.
    #[test]
    fn fields_continue_past_the_alphabet() {
        assert_eq!(field(0), "a");
        assert_eq!(field(2), "c");
        assert_eq!(field(25), "z");
        assert_eq!(field(26), "aa");
        assert_eq!(field(27), "ab");

        let names: BTreeSet<String> = (0..1000).map(field).collect();
        assert_eq!(names.len(), 1000);
    }

    fn module(package: &str, name: &str) -> ModuleName {
        ModuleName::new(
            zelkova_compiler::PackageName::new(package).unwrap(),
            Name::new(name),
        )
    }

    /// A module one directory down reaches a module of its own package and the runtime by
    /// climbing one more level than a top-level module does.
    #[test]
    fn a_specifier_climbs_out_of_the_importers_directory() {
        assert_eq!(
            module_specifier(&module("app", "Test"), "app", &Name::new("Js.Basics")),
            "./Js/Basics.mjs"
        );
        assert_eq!(
            module_specifier(&module("app", "Js.Basics"), "app", &Name::new("Maybe")),
            "../Maybe.mjs"
        );
        assert_eq!(runtime_specifier(&Name::new("Test")), "../zelkova.mjs");
        assert_eq!(
            runtime_specifier(&Name::new("Js.Basics")),
            "../../zelkova.mjs"
        );
    }

    /// A module of another package is reached in that package's sibling directory, by
    /// climbing out of the importer's own package directory first.
    ///
    /// Mutation-checked by dropping the package comparison in `module_specifier`, so
    /// every import is built as if it were within one package: both assertions go red.
    #[test]
    fn a_specifier_across_packages_reaches_a_sibling_directory() {
        assert_eq!(
            module_specifier(&module("app", "Test"), "zelkova-core", &Name::new("Maybe")),
            "../zelkova-core/Maybe.mjs"
        );
        assert_eq!(
            module_specifier(
                &module("app", "Page.Home"),
                "zelkova-core",
                &Name::new("Js.Basics")
            ),
            "../../zelkova-core/Js/Basics.mjs"
        );
    }

    /// A facade's companion is written beside it under a name no module's file can
    /// have, and the facade imports it by that name.
    #[test]
    fn a_companion_is_written_and_imported_beside_its_facade() {
        let name = Name::new("Js.Basics");
        assert_eq!(module_file(&name), PathBuf::from("Js/Basics.mjs"));
        assert_eq!(
            companion_file(&name),
            PathBuf::from("Js/Basics.companion.mjs")
        );
        assert_eq!(companion_specifier(&name), "./Basics.companion.mjs");
    }

    /// A test companion's source-relative import of the companion it checks climbs out
    /// of `tests/` and into `src/`, and becomes the path between the two companions'
    /// files in the build — at any depth of either module.
    #[test]
    fn a_test_companion_imports_its_target_by_the_build_path() {
        assert_eq!(
            test_companion_import(&Name::new("Js.BasicsChecks"), &Name::new("Js.Basics")),
            (
                "../../src/Js/Basics.mjs".to_string(),
                "../Js/Basics.companion.mjs".to_string()
            )
        );
        assert_eq!(
            test_companion_import(&Name::new("PrimChecks"), &Name::new("Core.Prim")),
            (
                "../src/Core/Prim.mjs".to_string(),
                "./Core/Prim.companion.mjs".to_string()
            )
        );
    }

    /// Both quotes are rewritten, and a specifier inside a longer string or a template
    /// literal is not.
    #[test]
    fn an_import_is_rewritten_only_where_a_whole_literal_spells_it() {
        let imports = vec![("../src/A.mjs".to_string(), "./A.companion.mjs".to_string())];
        let text = "import { a } from '../src/A.mjs';\n\
                    import { b } from \"../src/A.mjs\";\n\
                    const c = '../src/A.mjs.bak';\n\
                    const d = `../src/A.mjs`;\n";
        assert_eq!(
            rewrite_imports(text, &imports),
            "import { a } from './A.companion.mjs';\n\
             import { b } from \"./A.companion.mjs\";\n\
             const c = '../src/A.mjs.bak';\n\
             const d = `../src/A.mjs`;\n"
        );
    }

    #[test]
    fn a_char_is_a_one_character_string() {
        assert_eq!(char_literal('a'), "\"a\"");
        assert_eq!(char_literal('"'), "\"\\\"\"");
        assert_eq!(char_literal('\n'), "\"\\n\"");
        assert_eq!(char_literal('\u{0}'), "\"\\u{0}\"");
    }
}
