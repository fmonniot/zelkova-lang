//! The JavaScript backend: one checked module in, the text of one ES module out.
//!
//! [`emit`] reads the [`ir::Module`] a [`CheckedModule`] carries, beside the
//! [`canonical::Module`] it was built from for the one thing the IR does not hold — which
//! names the module exports. It produces text and writes nothing; where that text goes is
//! [`GEN-13`](../../../docs/tickets/gen-13.md)'s.
//!
//! # The shape of an emitted module
//!
//! In this order, each section separated from the next by a blank line and left out when
//! it is empty:
//!
//! 1. **Imports** — the runtime helpers the module calls, from the runtime module
//!    (`runtime/js/zelkova.mjs`), then one named import per value another module declares.
//! 2. **Hoisted constructors** — one `const` per constructor of no arguments this module
//!    declares, then one per such constructor of another module's that it mentions, which
//!    every mention of it refers to ([`DEC-18` decision
//!    4](../../../docs/decisions/dec-18.md#4--a-constructor-of-no-arguments-is-hoisted-to-one-module-level-constant)).
//! 3. **Functions** — one `function` per declaration that takes parameters, with exactly
//!    as many JavaScript parameters as it was written with ([`DEC-18` decision
//!    3](../../../docs/decisions/dec-18.md#3--a-function-emits-as-a-plain-n-ary-function-and-currying-is-a-runtime-helper)).
//! 4. **Parameterless bindings** — one `const` each, in
//!    [`ir::Module::initialisation_order`], so each is initialised after every other one
//!    it mentions directly. One that reaches another only through a function it calls is
//!    not ordered after it, and the emitted module throws at load
//!    ([`BUG-38`](../../../docs/tickets/bug-38.md)).
//! 5. **Exports** — one `export { … }` naming each exported value by its Zelkova name.
//!
//! # Representations
//!
//! An `Int` literal is a `BigInt` (`1n`), since [`Int` is 64
//! bits](../../../docs/spec/evaluation-semantics.md#numbers); a `Float` is a number, a
//! `Char` a one-character string. `True` and `False` — the constructors of
//! [`scalars::BOOL`], recognised by the union's qualified name — are `true` and `false`.
//! Every other union value is `{$: "Ctor", a: …, b: …}`, arguments in declaration order
//! ([A union crosses as a tagged
//! value](../../../docs/spec/interop.md#a-union-crosses-as-a-tagged-value)); a
//! constructor's arguments past the 26th continue `aa`, `ab`, … — see [`field`]. A tuple
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
//! called one argument at a time. A declaration of two or more parameters, and a
//! constructor of two or more arguments, is `$curry(f, n)` wherever it is used as a value
//! rather than called, so a partial application such as `pick 1` is `$curry(pick,
//! 2)(1n)`; a one-parameter function already takes its argument one at a time.
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
//! A value another module declares has an arity this module cannot see, so it is called
//! one argument at a time like any other value ([`ir::ReferenceKind::Foreign`]).
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
//! ([An effectful facade](../../../docs/spec/interop.md#an-effectful-facade)), and
//! wrapping one in the `Result` its call site is owed is
//! [`GEN-16`](../../../docs/tickets/gen-16.md)'s, blocked on `Task` existing at all.
//! [`emit`] answers [`Error::Effectful`] for one rather than emitting the `unsafe`
//! shape for it.
//!
//! [`companion_specifier`] assumes the companion sits beside this module's own emitted
//! file, sharing the last segment of its name, which is where it sits beside the
//! `.zel` source today. Whether the two can share that output path once one is written
//! there is [`GEN-13`](../../../docs/tickets/gen-13.md)'s to settle, along with copying
//! the companion into place at all — nothing here does either.
//!
//! # What is refused
//!
//! [`emit`] answers an [`Error`] rather than a module missing a part: for a declaration
//! with no IR ([`ir::Module::unchecked`]), for a `case` ([`GEN-10`](../../../docs/tickets/gen-10.md)
//! emits it), for a facade signature not marked `unsafe`, and for a facade with no
//! companion for the target being built.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use super::canonical::{ExportType, Exports, Value};
use super::ir::{self, ReferenceKind, Saturation, TypedTerm, TypedTermKind};
use super::name::{Name, QualName};
use super::position::NodeSpan;
use super::{scalars, CheckedModule, PhaseError, SpanLabel};

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
    /// A facade signature not marked `unsafe`. It declares an effect
    /// ([An effectful facade](../../../docs/spec/interop.md#an-effectful-facade)), and
    /// the wrapper its call site is owed is [`GEN-16`](../../../docs/tickets/gen-16.md)'s,
    /// blocked on `Task` existing at all — so this is refused rather than emitted as
    /// if it were `unsafe`.
    Effectful { name: Name, span: NodeSpan },
    /// A declaration the typer could not check has no IR to emit, and a module emitted
    /// without it would be missing a value its source declares.
    Unchecked { name: Name, span: NodeSpan },
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
    /// A `case` expression, which [`GEN-10`](../../../docs/tickets/gen-10.md) emits.
    Case,
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
            Construct::Case => "a `case` expression",
            Construct::Let => "a `let` expression",
            Construct::Lambda => "an anonymous function",
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
            Error::Effectful { name, .. } => format!(
                "`{}` declares an effect, which the JavaScript backend does not wrap yet",
                name.as_str()
            ),
            Error::Unchecked { name, .. } => format!(
                "`{}` cannot be compiled to JavaScript, because the type checker could not check it",
                name.as_str()
            ),
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
            Error::Effectful { span, .. } => (span, "not marked `unsafe`"),
            Error::Unchecked { span, .. } => (span, "this declaration"),
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
/// them as a binding's name in module code.
///
/// Every reserved word of ECMAScript — including the ones reserved only in strict
/// mode, which module code always is, and `await`, reserved in a module — plus `eval`
/// and `arguments`, which strict mode forbids binding. The Zelkova keywords among
/// them never reach here as a name; they are listed anyway, so that this is the
/// JavaScript list and not a guess at which part of it Zelkova can spell.
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
/// so that it cannot be one this function returns:
///
/// - the runtime's helpers, `$curry` and `$abort`: a `$` and a word that is not in
///   [`RESERVED`];
/// - a hoisted constructor, [`hoisted`]: a `$` followed by a name that contains
///   another `$`;
/// - a value imported from another module, [`imported`]: a `$` that is not the first
///   character;
/// - a wildcard parameter, [`wildcard`]: `$_` and a number, and `_0`, `_1`, … are not in
///   [`RESERVED`].
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

/// The module-level constant a constructor of no arguments is hoisted to: `$`, then the
/// segments of the module declaring its union and its own name, joined by `$` — `Test`'s
/// `Red` is `$Test$Red`.
fn hoisted(union: &QualName, constructor: &Name) -> String {
    format!(
        "${}${}",
        union.module_name().as_str().replace('.', "$"),
        constructor.as_str()
    )
}

/// The local name a value another module declares is imported under: the module's
/// segments and the value's name, joined by `$` — `Maybe.withDefault` is
/// `Maybe$withDefault`.
///
/// A value is never imported under its own name, because this module may declare the
/// same name itself.
fn imported(module: &str, name: &str) -> String {
    format!("{}${}", module.replace('.', "$"), name)
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

/// How many directories below its package's output directory the emitted file for
/// `module` sits: one per segment of its name but the last.
fn depth(module: &Name) -> usize {
    module.as_str().split('.').count() - 1
}

/// The specifier the module named `from` imports the module named `to` by.
///
/// **Provisional, until [`GEN-13`](../../../docs/tickets/gen-13.md) settles the output
/// layout**, and the only function that would change when it does. Each module is
/// assumed to be at `<module path>.mjs` below one directory per package, as [`DEC-18`
/// decision 5](../../../docs/decisions/dec-18.md#5--output-is-written-per-package-beside-the-root-manifest)
/// has it, and both modules are assumed to be in the same package: a module name is all
/// an [`ir::ReferenceKind::Foreign`] carries, and it does not say which package declared
/// it.
fn module_specifier(from: &Name, to: &Name) -> String {
    let up = match depth(from) {
        0 => "./".to_string(),
        depth => "../".repeat(depth),
    };

    format!("{}{}.mjs", up, to.as_str().replace('.', "/"))
}

/// The specifier the module named `from` imports the runtime by.
///
/// **Provisional**, like [`module_specifier`]: the runtime is assumed to be at the root
/// of the output, above every package's directory, which is where
/// [`GEN-13`](../../../docs/tickets/gen-13.md) names as the obvious candidate.
fn runtime_specifier(from: &Name) -> String {
    format!("{}zelkova.mjs", "../".repeat(depth(from) + 1))
}

/// The specifier a facade named `module` imports its own companion by.
///
/// **Provisional**, like [`module_specifier`]: assumes the companion sits beside this
/// module's own emitted file, sharing the last segment of its name — `Js.Basics`
/// imports `./Basics.mjs` — which is where it sits beside the `.zel` source today
/// ([*A facade names a boundary, not a
/// backend*](../../../docs/spec/interop.md#a-facade-names-a-boundary-not-a-backend)).
/// Whether the facade's own emitted file and the companion can share that same output
/// path, and copying the companion there at all, is
/// [`GEN-13`](../../../docs/tickets/gen-13.md)'s to settle.
fn companion_specifier(module: &Name) -> String {
    let last = module
        .as_str()
        .rsplit('.')
        .next()
        .unwrap_or(module.as_str());
    format!("./{}.mjs", last)
}

// ── Literals ──────────────────────────────────────────────────────────────────

/// A JavaScript string literal holding `c` and nothing else.
fn char_literal(c: char) -> String {
    let escaped = match c {
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
    };

    format!("\"{}\"", escaped)
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
/// See this module's documentation for the shape of the text.
pub fn emit(module: &CheckedModule, has_companion: bool) -> Result<String, Vec<Error>> {
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
        module: ir.name.name().clone(),
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

    for declaration in &ir.declarations {
        emitter.declaration = Some(declaration.name.clone());

        if ir.foreign {
            emitter.facade_declaration(
                &module.canonical,
                declaration,
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
    for (from, names) in &emitter.imports {
        let specifiers: Vec<String> = names
            .iter()
            .map(|name| format!("{} as {}", name, imported(from, name)))
            .collect();
        imports.push(format!(
            "import {{ {} }} from \"{}\";",
            specifiers.join(", "),
            module_specifier(ir.name.name(), &Name::new(from.as_str()))
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

// ── Emitting an expression ────────────────────────────────────────────────────

struct Emitter {
    /// How many parameters each of this module's declarations takes.
    arities: HashMap<Name, usize>,
    /// The runtime helpers the emitted text calls.
    runtime: BTreeSet<&'static str>,
    /// The values of other modules the emitted text mentions, by the module declaring
    /// each.
    imports: BTreeMap<String, BTreeSet<String>>,
    /// The module being emitted, which tells a constructor it declares from one another
    /// module does.
    module: Name,
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
    /// `companion_imports`.
    ///
    /// Only for a signature marked `unsafe`: one that is not declares an effect
    /// ([An effectful facade](../../../docs/spec/interop.md#an-effectful-facade)),
    /// which this backend does not wrap yet, so [`Error::Effectful`] is pushed instead
    /// and nothing is appended for it.
    fn facade_declaration<'a>(
        &mut self,
        canonical: &super::canonical::Module,
        declaration: &'a ir::Declaration,
        functions: &mut Vec<String>,
        constants: &mut HashMap<&'a Name, String>,
        companion_imports: &mut Vec<String>,
    ) {
        let marked_unsafe = matches!(
            canonical.values.get(&declaration.name),
            Some(Value::TypedValue {
                marked_unsafe: true,
                ..
            })
        );

        if !marked_unsafe {
            self.errors.push(Error::Effectful {
                name: declaration.name.clone(),
                span: declaration.span,
            });
            return;
        }

        // The companion's export is imported under an alias — never the plain name,
        // which this method is about to declare a local binding under, and a module
        // cannot import and declare the same name twice.
        let local = mangle(declaration.name.as_str());
        let alias = imported(self.module.as_str(), declaration.name.as_str());
        companion_imports.push(format!("{} as {}", declaration.name.as_str(), alias));

        if declaration.arity == 0 {
            constants.insert(&declaration.name, format!("const {} = {};", local, alias));
        } else {
            let parameters: Vec<String> = (0..declaration.arity).map(field).collect();
            functions.push(format!(
                "function {}({}) {{\n  return {}({});\n}}",
                local,
                parameters.join(", "),
                alias,
                parameters.join(", ")
            ));
        }
    }

    fn expression(&mut self, term: &TypedTerm) -> String {
        match &term.kind {
            TypedTermKind::Int(i) => format!("{}n", i),
            TypedTermKind::Float(f) => float_literal(*f),
            TypedTermKind::Char(c) => char_literal(*c),
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
            TypedTermKind::Case { .. } => self.unsupported(Construct::Case, term.span),
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
            ReferenceKind::Foreign(qname) => {
                let module = qname.module_name().as_str().to_string();
                let name = qname.unqualified_name().as_str().to_string();
                let local = imported(&module, &name);
                self.imports.entry(module).or_default().insert(name);
                local
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
                        if ctor.union.module_name() != self.module {
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

    /// A module one directory down reaches a sibling package module and the runtime by
    /// climbing one more level than a top-level module does.
    #[test]
    fn a_specifier_climbs_out_of_the_importers_directory() {
        assert_eq!(
            module_specifier(&Name::new("Test"), &Name::new("Js.Basics")),
            "./Js/Basics.mjs"
        );
        assert_eq!(
            module_specifier(&Name::new("Js.Basics"), &Name::new("Maybe")),
            "../Maybe.mjs"
        );
        assert_eq!(runtime_specifier(&Name::new("Test")), "../zelkova.mjs");
        assert_eq!(
            runtime_specifier(&Name::new("Js.Basics")),
            "../../zelkova.mjs"
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
