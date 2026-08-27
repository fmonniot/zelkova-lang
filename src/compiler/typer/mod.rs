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

/// What went wrong, with no claim about where.
///
/// Inference works on the typer's own `Term`/`Constraint` language, which is built
/// from the canonical AST and keeps no positions, so none of the three places that
/// raise one of these — `annotate`, `unifier`, `infer` — has a span to attach. The
/// position is put on by [`Error`], one level up. See its documentation.
#[derive(Debug)]
pub enum ErrorKind {
    TypeMismatch { expected: Type, actual: Type },
    UnificationFailed { left: Type, right: Type },
    CircularType,
    UnboundVariable(String),
}

impl ErrorKind {
    fn message(&self) -> String {
        match self {
            ErrorKind::TypeMismatch { expected, actual } => {
                format!("type mismatch: expected `{}`, found `{}`", expected, actual)
            }
            ErrorKind::UnificationFailed { left, right } => {
                format!("cannot match `{}` with `{}`", left, right)
            }
            ErrorKind::CircularType => {
                "circular type: a type variable would have to contain itself".to_owned()
            }
            ErrorKind::UnboundVariable(name) => format!("cannot find a value named `{}`", name),
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            ErrorKind::TypeMismatch { expected, .. } => vec![format!(
                "the type annotation says `{}`; the body infers to something else",
                expected
            )],
            _ => Vec::new(),
        }
    }
}

/// A type error, together with the declaration it was found in.
///
/// # Why the span lives here and not on the variants
///
/// The four places that raise an [`ErrorKind`] all sit below the last point that
/// knows a position. `annotate` and `unifier` work on `Term`s and `Constraint`s —
/// a translation of the canonical AST that drops positions on the way in — so a
/// `span` field on the variants would only move the problem to a construction site
/// with nothing to put in it. [`type_check`]'s third pass is the one frame that
/// still has the `canonical::Value` in hand, so it is the only place this struct is
/// built, and it is where the span comes from.
///
/// # Declaration granularity is the ceiling
///
/// That span is the whole declaration — its annotation and its body — because that
/// is the finest thing the typer can still name. Pointing at the *sub-expression*
/// that disagrees means carrying positions through the `Term` translation, which is
/// `ERR-4` (see `docs/tickets/INDEX.md`).
#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    /// Where the declaration the error was found in was written.
    pub span: NodeSpan,
    /// That declaration's name. The struct's two other fields say what went wrong
    /// and where; this is what lets the label say *which* declaration, which is the
    /// one thing a reader of a hundred-declaration module still needs.
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
        self.kind.notes()
    }

    fn labels(&self) -> Vec<SpanLabel> {
        match self.span.span() {
            Some(span) => vec![SpanLabel {
                span,
                message: format!("in `{}`", self.declaration),
                primary: true,
            }],
            None => Vec::new(),
        }
    }
}

/// Type check one canonical module, reporting every value whose inference produced a
/// reportable error rather than stopping at the first — the shape `compile_package` is
/// built to accumulate.
///
/// Two classes of failure are deliberately *not* reported today, both in the third pass
/// below: a value whose term cannot be built at all (an unsupported construct) is skipped,
/// and an `ErrorKind::UnboundVariable` is discarded. A module whose only problems are of
/// those two kinds returns `Ok(())`.
///
/// The third pass is also the only place an [`Error`] is built, because it is the last
/// frame that still holds the `canonical::Value` — and therefore the declaration's span.
/// See [`Error`] for why the span is attached here rather than carried by the variants.
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
        // The declaration this error would be about, ready for whichever of the two
        // failure paths below fires.
        let at = |kind: ErrorKind| Error {
            kind,
            span: value.span(),
            declaration: name.clone(),
        };

        let Some((term, annotation)) =
            value_to_term_and_annotation(value, &module.types, &mut counter)
        else {
            continue; // unsupported construct — skip for now
        };

        let inferred = match infer(term, global.clone()) {
            // An unbound variable means the term references a top-level function whose
            // type we couldn't represent (e.g. Float-typed functions). Skip silently.
            Err(ErrorKind::UnboundVariable(_)) => continue,
            Err(e) => {
                errors.push(at(e));
                continue;
            }
            Ok(t) => t,
        };

        // If there's a type annotation, verify inferred type is compatible
        if let Some(ann) = annotation {
            let mut constraints = std::collections::HashSet::new();
            constraints.insert(Constraint(inferred.clone(), ann.clone()));
            if unifier::unify(constraints).is_err() {
                errors.push(at(ErrorKind::TypeMismatch {
                    expected: ann,
                    actual: inferred,
                }));
            }
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

/// Convert a canonical expression to a Term.
/// Returns None for constructs the inference engine doesn't yet handle
/// (VarKernel, VarForeign, complex patterns inside Case).
fn canonical_expr_to_term(
    expr: &canonical::Expression,
    module_types: &HashMap<Name, canonical::UnionType>,
    counter: &mut u32,
) -> Option<Term> {
    match &expr.kind {
        canonical::ExpressionKind::Int(i) => Some(Term::Int(*i as u32)),
        canonical::ExpressionKind::Bool(b) => Some(Term::Bool(*b)),
        canonical::ExpressionKind::Char(c) => Some(Term::Char(*c)),
        canonical::ExpressionKind::Float(f) => Some(Term::Float(*f)),
        canonical::ExpressionKind::VarLocal(name) => {
            Some(Term::Identifier(name.as_str().to_string()))
        }
        canonical::ExpressionKind::VarTopLevel(qname) => {
            Some(Term::Identifier(qname.to_name().as_str().to_string()))
        }
        canonical::ExpressionKind::Apply(f, a) => {
            let fun = canonical_expr_to_term(f, module_types, counter)?;
            let arg = canonical_expr_to_term(a, module_types, counter)?;
            Some(Term::Apply {
                fun: Box::new(fun),
                arg: Box::new(arg),
            })
        }
        canonical::ExpressionKind::If(cond, t, f) => {
            let cond = canonical_expr_to_term(cond, module_types, counter)?;
            let t = canonical_expr_to_term(t, module_types, counter)?;
            let f = canonical_expr_to_term(f, module_types, counter)?;
            Some(Term::If {
                cond: Box::new(cond),
                true_branch: Box::new(t),
                false_branch: Box::new(f),
            })
        }
        canonical::ExpressionKind::Tuple(tuple) => {
            let elements = tuple
                .try_map(|elem| canonical_expr_to_term(elem, module_types, counter).ok_or(()))
                .ok()?;
            Some(Term::Tuple(elements))
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
            Some(Term::Case {
                scrutinee: Box::new(scrutinee),
                branches: term_branches,
            })
        }
        // Constructors are resolved as identifiers looked up in the global env.
        canonical::ExpressionKind::VarConstructor(qname, _) => {
            Some(Term::Identifier(qname.to_name().as_str().to_string()))
        }
        // VarForeign: not in the module's global env, skip
        canonical::ExpressionKind::VarForeign(_, _) => None,
        // Not yet supported: VarKernel
        _ => None,
    }
}

/// Translate a canonical pattern into a `TermPattern` plus any variable bindings
/// introduced by the pattern.  Returns `None` for unsupported pattern shapes.
fn translate_pattern(
    pattern: &canonical::Pattern,
    module_types: &HashMap<Name, canonical::UnionType>,
    counter: &mut u32,
) -> Option<(TermPattern, Vec<(String, Type)>)> {
    match &pattern.kind {
        canonical::PatternKind::Anything => Some((TermPattern::Anything, vec![])),
        canonical::PatternKind::Variable(name) => {
            // The binding's actual type will be unified with the scrutinee type in annotate.
            Some((TermPattern::Bind(name.as_str().to_string()), vec![]))
        }
        canonical::PatternKind::Bool(_) => Some((
            TermPattern::Literal(Type::Literal(TypeLiteral::Bool)),
            vec![],
        )),
        canonical::PatternKind::Int(_) => Some((
            TermPattern::Literal(Type::Literal(TypeLiteral::Int)),
            vec![],
        )),
        canonical::PatternKind::Char(_) => Some((
            TermPattern::Literal(Type::Literal(TypeLiteral::Char)),
            vec![],
        )),
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

            let term_pattern = TermPattern::Constructor {
                adt_name: ctor.tpe.as_str().to_string(),
                adt_args,
                bindings: bindings.clone(),
            };
            Some((term_pattern, bindings))
        }
        _ => None, // Tuple, Float patterns — not yet supported
    }
}

/// Convert a canonical Value into a (Term, optional annotation) pair.
/// The body is wrapped in nested Fun nodes for each pattern parameter.
/// Returns None if any part of the value cannot be translated.
fn value_to_term_and_annotation(
    value: &canonical::Value,
    module_types: &HashMap<Name, canonical::UnionType>,
    counter: &mut u32,
) -> Option<(Term, Option<Type>)> {
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
            ..
        } => {
            let body_term = canonical_expr_to_term(body, module_types, counter)?;
            let pattern_iter = patterns.iter().map(|(p, _)| p);
            let term = wrap_with_patterns(pattern_iter, body_term)?;
            let mut var_map = HashMap::new();
            let annotation = canonical_type_to_typer_type(tpe, &mut var_map, counter);
            Some((term, annotation))
        }
    }
}

/// Wrap a body Term in nested Fun nodes for each pattern, outermost first.
/// Returns None if any pattern is not translatable (e.g. constructor patterns).
fn wrap_with_patterns<'a>(
    patterns: impl Iterator<Item = &'a canonical::Pattern>,
    body: Term,
) -> Option<Term> {
    let names: Vec<String> = patterns
        .map(|p| match &p.kind {
            canonical::PatternKind::Variable(name) => Some(name.as_str().to_string()),
            canonical::PatternKind::Anything => Some("_".to_string()),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;

    let term = names.iter().rev().fold(body, |acc, param| Term::Fun {
        param: param.clone(),
        body: Box::new(acc),
    });

    Some(term)
}

// First try of an implementation. Not linked to the rest of the code base for simplicity's sake.

mod annotate;
mod constraint;
mod unifier;

/// Simplified pattern used inside the typer's Term.
#[derive(Debug, Clone)]
pub enum TermPattern {
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

#[derive(Debug, Clone)] // TODO Remove clone when not needed anymore
/// untyped term. In zelkova that would be the parsed source (or canonical, not sure yet)
pub enum Term {
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

/// Like a [Term] but with an associated [Type].
/// Any term introducing a name will have a TypeBinder instead.
#[derive(Debug)]
#[allow(dead_code)]
enum TypedTerm {
    Int {
        tpe: Type,
        value: u32,
    },
    Bool {
        tpe: Type,
        value: bool,
    },
    Char {
        tpe: Type,
        value: char,
    },
    Float {
        tpe: Type,
        value: f64,
    },
    // TODO Do I want to keep this name ? Or named Variable ? Something else ?
    Identifier {
        tpe: Type,
        name: String,
    }, // This is basically a TypeBinder
    Fun {
        tpe: Type,
        param: TypeBinder,
        body: Box<TypedTerm>,
    },
    Apply {
        tpe: Type,
        fun: Box<TypedTerm>,
        arg: Box<TypedTerm>,
    },
    If {
        tpe: Type,
        cond: Box<TypedTerm>,
        true_branch: Box<TypedTerm>,
        false_branch: Box<TypedTerm>,
    },
    Let {
        tpe: Type,
        binding: TypeBinder,
        value: Box<TypedTerm>,
        body: Box<TypedTerm>,
    },
    Tuple {
        tpe: Type,
        elements: Tuple<TypedTerm>,
    },
    Case {
        tpe: Type,
        scrutinee: Box<TypedTerm>,
        branches: Vec<(TermPattern, Box<TypedTerm>)>,
    },
}

impl TypedTerm {
    fn tpe(&self) -> &Type {
        match &self {
            TypedTerm::Int { tpe, .. } => tpe,
            TypedTerm::Bool { tpe, .. } => tpe,
            TypedTerm::Char { tpe, .. } => tpe,
            TypedTerm::Float { tpe, .. } => tpe,
            TypedTerm::Identifier { tpe, .. } => tpe,
            TypedTerm::Fun { tpe, .. } => tpe,
            TypedTerm::Apply { tpe, .. } => tpe,
            TypedTerm::If { tpe, .. } => tpe,
            TypedTerm::Let { tpe, .. } => tpe,
            TypedTerm::Tuple { tpe, .. } => tpe,
            TypedTerm::Case { tpe, .. } => tpe,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct Constraint(Type, Type);

#[derive(Debug, Eq, PartialEq)]
struct Substitution {
    solutions: HashMap<TypeVariable, Type>,
}

impl Substitution {
    // constructors

    fn empty() -> Substitution {
        Substitution {
            solutions: HashMap::new(),
        }
    }

    fn one(tvar: TypeVariable, tpe: Type) -> Substitution {
        let mut sub = Substitution::empty();

        sub.solutions.insert(tvar, tpe);

        sub
    }

    // methods

    fn apply(&self, c: &Constraint) -> Constraint {
        Constraint(self.apply_type(&c.0), self.apply_type(&c.1))
    }

    fn apply_type(&self, tpe: &Type) -> Type {
        self.solutions
            .iter()
            .fold(tpe.clone(), |tpe, (tvar, solution_tpe)| {
                Substitution::substitute(tpe, tvar, solution_tpe)
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

        let self_solutions = self
            .solutions
            .iter()
            .map(|(k, v)| (k.clone(), other.apply_type(v)));

        let mut sub = Substitution::empty();

        sub.solutions.extend(self_solutions);
        sub.solutions.extend(other.solutions);

        sub
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
    let mut env = Types::new();
    env.extends_with(global);

    let typed_term = annotate::annotate(term, &mut env)?;
    debug!("typed term: {:#?}", typed_term);

    let constraints = constraint::collect(&typed_term);
    debug!("Constraints: {:#?}", constraints);

    let substitution = unifier::unify(constraints)?;

    Ok(substitution.apply_type(typed_term.tpe()))
}

// TODO Once we have changed the Term to the zelkova primitives, rewrite the tests
// to use actual source code instead of AST. It's a pain to write them but it's even
// more of a pain to read them :)
// TODO Also write some assertions on the type instead of just printing XD
// TODO Import remaining tests. Plus the one for the modules above.
#[cfg(test)]
mod tests {
    use super::*;

    fn bool(b: bool) -> Term {
        Term::Bool(b)
    }
    fn int(i: u32) -> Term {
        Term::Int(i)
    }
    fn var(n: &str) -> Term {
        Term::Identifier(n.to_string())
    }
    fn fun(arg: &str, body: Term) -> Term {
        Term::Fun {
            param: arg.to_owned(),
            body: Box::new(body),
        }
    }
    fn if_(cond: Term, true_branch: Term, false_branch: Term) -> Term {
        Term::If {
            cond: Box::new(cond),
            true_branch: Box::new(true_branch),
            false_branch: Box::new(false_branch),
        }
    }
    fn apply(fun: Term, arg: Term) -> Term {
        Term::Apply {
            fun: Box::new(fun),
            arg: Box::new(arg),
        }
    }
    fn let_(binding: &str, value: Term, body: Term) -> Term {
        Term::Let {
            binding: binding.to_owned(),
            value: Box::new(value),
            body: Box::new(body),
        }
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
