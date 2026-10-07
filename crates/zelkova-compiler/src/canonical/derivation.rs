//! Derivation: what a class says about how it is derived, and what a `derived` instance is
//! made of.
//!
//! [Type classes](../../../docs/spec/type-classes.md#an-instance-may-be-derived) is the
//! rule, and this module is its three halves, each run during canonicalization.
//!
//! # A derivation is checked where it is written
//!
//! [`class_derivations`] holds each `derived member` of a class body to what the chapter
//! asks of it: it names a member, once; the member's signature is `a -> a -> R` or
//! `a -> R` with `R` free of the class variable; its bindings are exactly the two or three
//! that signature calls for; and the derivations cover every member or none. What passes
//! is kept on the [`ClassSignature`], so an importing module reads it from the interface.
//!
//! # What a derived instance requires
//!
//! [`derive_all`] takes every `derived` instance of a module together. Each is read for
//! the shape its head gives a walk — a union's constructors, a tuple, `()` — and for what
//! every argument of every variant needs, which is an instance of the derived class at the
//! argument's type. An argument that is a variable of the head is a constraint on the
//! instance: that is the context, inferred. An argument of a concrete type needs an
//! instance in scope, and one that is an application needs the instance of its head and
//! whatever that instance's context asks of the arguments, down to the variables.
//!
//! An instance that writes a context has that context and no other. What its arguments
//! need is still read, and each constraint of it has to be one the written context
//! provides, itself or through a class it is a superclass of ([`provides`]); one that is
//! not is [`Error::DerivedInstanceContextTooNarrow`].
//!
//! A recursive type asks for its own instance, and a group of types may ask for each
//! other's, so the instances being derived count as in scope with the contexts being
//! inferred, and the contexts are computed together as a fixed point: each starts empty and
//! grows by what the others' contexts ask, until nothing grows. A written context is where
//! its instance starts and stays. A context holds pairs of a
//! class and one of the head's variables, and the classes that can appear in one are not
//! only the derived one: a written or imported instance's context names whatever its
//! parameters need, and that is followed too. The set of such pairs is finite, the classes
//! in scope times the head's variables, so it stops. What the instance ends with
//! is read off once more from scratch, which is what makes its order the order the type's
//! arguments are written in and not the order the iteration happened to find them.
//!
//! # A derived instance is given its members
//!
//! The walk is written out as canonical code ([`Generated`]) and the instance carries it
//! as bindings, where a written instance's would be. From there the typer checks them
//! against the member's signature with the inferred context given, and nothing after
//! canonicalization knows the instance was derived.
//!
//! The class's bindings are *placed*, not called
//! ([*The bindings are inlined*](../../../docs/spec/type-classes.md#the-bindings-are-inlined-not-called)).
//! `combine`'s first parameter is bound once, by a `case` of one branch whose scrutinee is
//! the answer for one part, and its second is replaced by the rest of the walk, so that
//! the rest is evaluated where the body reaches it and nowhere else
//! ([`DEC-24` decision 8](../../../docs/decisions/dec-24.md#8--combines-first-parameter-is-a-value-and-its-second-is-the-rest-of-the-walk)).
//! `matched`, `differed` and `atConstructor` are placed the same way, their parameters
//! bound to the values the walk hands them.
//!
//! Three things make that safe.
//!
//! - **Capture.** The rest of the walk mentions the names the definition gave the
//!   arguments, and a class author's binding may bind any name a source file can spell.
//!   The names the definition gives are `$left`, `$right`, `$value`, `$a1`, `$b1`, and so
//!   on, and every name a placed body binds is replaced by one of its own, `$3$x`: a `$`, a
//!   serial and the name the class wrote. No source file can write a `$`, so no binding of
//!   a class can bind one of these. What that makes true is that no generated binder is ever
//!   in the scope of another binding of the same name, however many copies of a body nest in
//!   each other. A name is still bound in more than one place: each alternative of a walk
//!   binds `$a1`, `$b1` and so on in its own branch, and the rest of the walk is copied at
//!   every mention of `combine`'s second parameter, carrying the names it binds with it. Those
//!   are sibling scopes, in which nothing is captured. The JavaScript emitter leaves a name
//!   that is not a reserved word as it is, so each is an identifier there, and none is a name
//!   the emitter makes for itself: those are a `$` and a word (`$curry`), a `$` and digits,
//!   and names with a package, `companion` or `is` as their first segment, where the first
//!   segment of these is a serial.
//! - **`Position` values.** A constructor's place in its declaration is a value of
//!   `Basics.Position`, whose one constructor is exposed to no module. The definition
//!   names it by [`scalars::POSITION`], the way the compiler names a scalar, and never
//!   through scope.
//! - **Spans.** A class's bindings were written in the class's file and the instance is in
//!   another's, so a span carried over would point at unrelated text. Every node of a
//!   generated definition, the placed bodies included, is given the span of the word
//!   `derived`, which is where an error in one is blamed.
//!
//! What a placed body may name is what its home module exposes, and a body that names a
//! value its home module does not expose is not checked where it is placed: the typer
//! reads only what the module it checks imports, so such a binding is left unchecked, as
//! one naming any other name the typer cannot read is.
//!
//! # What the code costs
//!
//! The rest of the walk is copied at every place `combine`'s body names its second
//! parameter. A body that names it `k` times, in a constructor of `n` arguments, makes
//! `k^n` copies of the rest, so the size of a derived member is exponential in a
//! constructor's arity whenever `k` is more than one, and nothing here bounds it. At run
//! time each path evaluates the rest at most once, which is what the rule asks; it is the
//! generated code that is large. `LANG-88` tracks it.

use super::classes::mentions;
use super::environment::{Environment, RootEnvironment};
use super::{
    binding_value, CaseBranch, ClassSignature, Constraint, DeclarationSite, Derivation,
    DerivationBindings, DerivationRole, Error, Expression, ExpressionKind, Field, HeadName,
    InstanceHead, Member, Pattern, PatternField, PatternKind, Type, TypeConstructor, UnionType,
    Value,
};
use crate::name::{Name, QualName};
use crate::scalars;
use std::cell::Cell;
use std::collections::HashMap;
use std::convert::TryFrom;
use zelkova_syntax::parser;
use zelkova_syntax::position::NodeSpan;
use zelkova_syntax::tuple::Tuple;

// ── The class side ───────────────────────────────────────────────────────────

/// What is wrong with the signature of a member a derivation was written for — see
/// [`Error::DerivationSignature`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DerivationSignatureProblem {
    /// No parameter of the member holds the class variable: `bottom : a` and
    /// `allValues : List a`. A derivation walks values of the type, and there is none to
    /// walk, so it would have to build one from a description of the type's constructors.
    NoClassValue,
    /// The class variable is in the answer: `add : a -> a -> a`. The walk would be asked
    /// for another value of the type it is walking.
    ResultMentionsClassVariable,
    /// Some other shape: a derivation is for `a -> a -> R` and `a -> R`.
    Shape,
}

/// Which walk a member's signature gives its derivation, and the type it answers with.
enum Walk {
    /// `a -> a -> R`.
    Pair(Type),
    /// `a -> R`.
    Single(Type),
}

/// The walk the signature `tpe` of a member of a class over `variable` admits, or why it
/// admits none.
fn walk_of(tpe: &Type, variable: &Name) -> Result<Walk, DerivationSignatureProblem> {
    let is_class_variable = |tpe: &Type| matches!(tpe, Type::Variable(name) if name == variable);

    if let Type::Arrow(first, rest) = tpe {
        if is_class_variable(first) {
            if let Type::Arrow(second, result) = rest.as_ref() {
                if is_class_variable(second) && !mentions(result, variable) {
                    return Ok(Walk::Pair(result.as_ref().clone()));
                }
            }
            if !mentions(rest, variable) {
                return Ok(Walk::Single(rest.as_ref().clone()));
            }
        }
    }

    let mut parameters = Vec::new();
    let mut result = tpe;
    while let Type::Arrow(parameter, rest) = result {
        parameters.push(parameter.as_ref());
        result = rest;
    }

    Err(
        if !parameters
            .iter()
            .any(|parameter| mentions(parameter, variable))
        {
            DerivationSignatureProblem::NoClassValue
        } else if mentions(result, variable) {
            DerivationSignatureProblem::ResultMentionsClassVariable
        } else {
            DerivationSignatureProblem::Shape
        },
    )
}

/// The names a derivation's bindings are written under for `walk`, in the order the
/// chapter lists them.
fn required(walk: &Walk) -> &'static [DerivationRole] {
    match walk {
        Walk::Pair(_) => &[
            DerivationRole::Matched,
            DerivationRole::Differed,
            DerivationRole::Combine,
        ],
        Walk::Single(_) => &[DerivationRole::AtConstructor, DerivationRole::Combine],
    }
}

/// The derivations of `class`, each held to the rules a derivation follows, and every error
/// of the ones that do not pass.
///
/// `signature` is `class` as `class_signature` built it, so the members are known. Every
/// binding of every derivation is canonicalized, a derivation that fails its checks
/// included, so that a name one cannot resolve is reported whatever else is wrong with it;
/// every such name is pushed onto `unresolved`.
///
/// A derivation that fails is left out of what comes back, and its member is one the class
/// does not derive. A class that derives some of its members and not all is its own
/// error, naming the ones left out ([`Error::DerivationIncomplete`]); a member whose
/// derivation is there and failed is not named in it, since that derivation has its own.
pub(super) fn class_derivations(
    env: &RootEnvironment,
    class: &parser::ClassDecl,
    class_name: &Name,
    signature: &ClassSignature,
    unresolved: &mut Vec<Error>,
) -> (Vec<Derivation>, Vec<Error>) {
    let mut errors = Vec::new();
    let mut derivations: Vec<Derivation> = Vec::new();
    // The span of the first derivation written for each member, valid or not.
    let mut first: HashMap<&Name, NodeSpan> = HashMap::new();

    for derivation in class.members.iter().filter_map(|member| match member {
        parser::ClassMember::Derivation(derivation) => Some(derivation),
        parser::ClassMember::Signature(_) => None,
    }) {
        // Every binding is read, so that what it names is resolved and reported. A binding
        // that did not canonicalize is a binding that was written: its name is kept, so
        // the derivation is not also reported as missing it.
        let mut bindings: Vec<(&parser::FunBinding, Option<Value>)> = Vec::new();
        for binding in &derivation.bindings {
            match binding_value(env, binding, unresolved) {
                Ok(value) => bindings.push((binding, Some(value))),
                Err(error) => {
                    errors.push(error);
                    bindings.push((binding, None));
                }
            }
        }

        let Some(member) = signature
            .members
            .iter()
            .find(|member| member.name == derivation.member)
        else {
            errors.push(Error::DerivationForNonMember(
                derivation.member.clone(),
                class_name.clone(),
                derivation.span,
            ));
            continue;
        };

        if let Some(earlier) = first.get(&derivation.member) {
            errors.push(Error::DerivationRepeated(
                derivation.member.clone(),
                derivation.span,
                *earlier,
            ));
            continue;
        }
        first.insert(&derivation.member, derivation.span);

        let walk = match walk_of(&member.tpe, &signature.variable) {
            Ok(walk) => walk,
            Err(problem) => {
                errors.push(Error::DerivationSignature(
                    derivation.member.clone(),
                    problem,
                    derivation.span,
                    member.span,
                ));
                continue;
            }
        };

        let before = errors.len();
        let mut found: HashMap<DerivationRole, Value> = HashMap::new();
        let mut written: HashMap<DerivationRole, NodeSpan> = HashMap::new();
        let mut failed: Vec<DerivationRole> = Vec::new();

        for (binding, value) in bindings {
            let Some(role) = required(&walk)
                .iter()
                .find(|role| role.name() == binding.name.as_str())
            else {
                errors.push(Error::DerivationBindingUnexpected(
                    binding.name.clone(),
                    derivation.member.clone(),
                    required(&walk)
                        .iter()
                        .map(|role| Name::new(role.name()))
                        .collect(),
                    binding.span,
                ));
                continue;
            };

            if let Some(earlier) = written.get(role) {
                errors.push(Error::DerivationBindingRepeated(
                    binding.name.clone(),
                    derivation.member.clone(),
                    binding.span,
                    *earlier,
                ));
                continue;
            }
            written.insert(*role, binding.span);

            match value {
                Some(value) => {
                    if value.arity() > role.parameters() {
                        errors.push(Error::DerivationBindingTakesTooMany(
                            binding.name.clone(),
                            derivation.member.clone(),
                            role.parameters(),
                            binding.span,
                        ));
                    } else {
                        found.insert(*role, value);
                    }
                }
                None => failed.push(*role),
            }
        }

        for role in required(&walk) {
            if !written.contains_key(role) {
                errors.push(Error::DerivationBindingMissing(
                    Name::new(role.name()),
                    derivation.member.clone(),
                    derivation.span,
                ));
            }
        }

        if errors.len() > before || !failed.is_empty() {
            continue;
        }

        let bindings = match walk {
            Walk::Pair(_) => match (
                found.remove(&DerivationRole::Matched),
                found.remove(&DerivationRole::Differed),
                found.remove(&DerivationRole::Combine),
            ) {
                (Some(matched), Some(differed), Some(combine)) => DerivationBindings::Pair {
                    matched: Box::new(matched),
                    differed: Box::new(differed),
                    combine: Box::new(combine),
                },
                _ => continue,
            },
            Walk::Single(_) => match (
                found.remove(&DerivationRole::AtConstructor),
                found.remove(&DerivationRole::Combine),
            ) {
                (Some(at_constructor), Some(combine)) => DerivationBindings::Single {
                    at_constructor: Box::new(at_constructor),
                    combine: Box::new(combine),
                },
                _ => continue,
            },
        };
        let result = match walk {
            Walk::Pair(result) | Walk::Single(result) => result,
        };

        derivations.push(Derivation {
            member: derivation.member.clone(),
            result,
            bindings,
            span: derivation.span,
        });
    }

    // A class that derives some of its members and not the rest could only be half derived
    // and half written.
    let covered = signature
        .members
        .iter()
        .filter(|member| first.contains_key(&member.name))
        .count();
    if covered > 0 && covered < signature.members.len() {
        let left_out: Vec<Name> = signature
            .members
            .iter()
            .filter(|member| !first.contains_key(&member.name))
            .map(|member| member.name.clone())
            .collect();
        errors.push(Error::DerivationIncomplete(
            class_name.clone(),
            left_out,
            class.span,
        ));
    }

    (derivations, errors)
}

// ── The instance side ────────────────────────────────────────────────────────

/// What is wrong with the type a `derived` instance is for, that it has no shape to walk —
/// see [`Error::DerivedInstanceNoShape`].
#[derive(Debug, Clone, PartialEq)]
pub enum DerivedShapeProblem {
    /// A [scalar type](../../docs/spec/types.md#scalar-types) has no shape for a walk to
    /// read, so its instances are written: the type.
    Scalar(Name),
    /// A type whose constructors are not in scope — one imported without them: the type.
    NoConstructors(Name),
    /// `()` for a class with a member whose derivation walks one value, which has no
    /// element to begin at: the class.
    UnitHasNoElement(Name),
}

/// The part of a type a walk found an argument in: where a requirement of a derived
/// instance came from.
#[derive(Debug, Clone, PartialEq)]
pub enum DerivedPart {
    /// An argument of a variant of a union.
    Variant(Name),
    /// An element of a tuple, counting from one.
    Element(usize),
}

/// An argument of a derived instance's type with no instance of the derived class — see
/// [`Error::DerivedInstanceRequires`].
#[derive(Debug, Clone, PartialEq)]
pub struct DerivedRequirement {
    /// The class being derived.
    pub class: Name,
    /// What the argument was found in.
    pub part: DerivedPart,
    /// The type of the argument, as written in the type's declaration.
    pub argument: String,
    /// The type that has no instance: the argument itself, or what an instance an
    /// application of it needs asks for.
    pub missing: String,
    /// Whether `missing` is a function type, which no instance can be declared for.
    pub function: bool,
    /// Where the type being derived for was declared, when that is in this module.
    pub declared: DeclarationSite,
}

/// A constraint a `derived` instance needs and the context written on it does not provide —
/// see [`Error::DerivedInstanceContextTooNarrow`].
#[derive(Debug, Clone, PartialEq)]
pub struct DerivedContextGap {
    /// The class being derived.
    pub class: Name,
    /// The constraint the context does not provide, as a context writes it: `Eq a`.
    pub missing: String,
    /// The first argument that needs it was found in this.
    pub part: DerivedPart,
    /// The type of that argument, as written in the type's declaration.
    pub argument: String,
}

/// A `derived` instance of the module under check, as [`derive_all`] reads it.
pub(super) struct Candidate<'a> {
    /// The class being derived.
    pub class: &'a QualName,
    pub head: &'a InstanceHead,
    /// The context written on the instance and where, when one is written and resolved.
    pub context: Option<(&'a [Constraint], NodeSpan)>,
    /// Where the word `derived` was written.
    pub span: NodeSpan,
}

/// What a derived instance comes to: its context and its members.
pub(super) struct Derived {
    /// The context the instance wrote, when it wrote one. Otherwise the constraints the
    /// instance needs of its head's variables, in the order the type's arguments are
    /// written in, each once.
    pub context: Vec<Constraint>,
    /// One binding per member of the class, in the order the class declares them.
    pub bindings: Vec<Value>,
}

/// What the walk of one derived instance reads: the alternatives a value may be and, for
/// each, the types of its arguments.
enum Shape<'a> {
    /// A declared union and where it was declared, when that is the module under check.
    Union(&'a UnionType, DeclarationSite),
    /// A tuple of two or three elements.
    Tuple(usize),
    /// `()`.
    Unit,
}

/// One way a value of a type may be built: a constructor of a union, or the one way a
/// tuple or `()` is.
struct Alternative<'a> {
    /// The constructor, for a union.
    constructor: Option<&'a TypeConstructor>,
    /// How many arguments it has.
    arity: usize,
}

impl Shape<'_> {
    fn alternatives(&self) -> Vec<Alternative<'_>> {
        match self {
            Shape::Union(union, _) => union
                .variants
                .iter()
                .map(|constructor| Alternative {
                    constructor: Some(constructor),
                    arity: constructor.type_parameters.len(),
                })
                .collect(),
            Shape::Tuple(arity) => vec![Alternative {
                constructor: None,
                arity: *arity,
            }],
            Shape::Unit => vec![Alternative {
                constructor: None,
                arity: 0,
            }],
        }
    }
}

/// An argument a derived instance's type holds, and what it asks.
struct Requirement {
    part: DerivedPart,
    /// The argument's type, over the head's variables.
    tpe: Type,
}

/// One derived instance, read for its shape and what that shape asks.
struct Plan<'a> {
    candidate: &'a Candidate<'a>,
    shape: Shape<'a>,
    requirements: Vec<Requirement>,
}

/// What an instance in scope needs of its head's variables.
struct Known {
    /// The head's variables, in the order the head writes them.
    variables: Vec<Name>,
    /// The constraints on them, each a class and a variable of the head.
    context: Vec<(QualName, Name)>,
}

type Table = HashMap<(QualName, HeadName), Known>;

/// An instance of the module under check as it is written: its class and head, and the
/// context it writes of its own. A derived instance that writes none has an empty one here,
/// which is where its inferred context starts.
pub(super) struct WrittenInstance {
    pub class: QualName,
    pub head: InstanceHead,
    pub context: Vec<(QualName, Name)>,
}

/// Derive every instance of `candidates`: the context each needs, and the members each is
/// made of, or why it cannot be.
///
/// The answer is one entry per candidate, in order. `written` is every instance of the module
/// with the context it writes, which an argument's type may need; the instances imported are
/// read from `env`. An instance that has an error in its own right — `signature` has no derivation
/// for it, the type has no shape — is one entry's errors, and is still counted as in scope
/// for the others, so that one failure is not restated by every type that holds it.
///
/// `signature` is the class each candidate derives, as the module that declares it
/// published it, or `None` when that declaration did not canonicalize and the error behind
/// it has been reported.
pub(super) fn derive_all<'a>(
    env: &'a RootEnvironment,
    candidates: &'a [Candidate<'a>],
    signatures: &[Option<&ClassSignature>],
    written: &[WrittenInstance],
) -> Vec<Result<Derived, Vec<Error>>> {
    let mut table: Table = HashMap::new();
    for published in env.imported_instances() {
        let signature = &published.signature;
        table.insert(
            (signature.class.clone(), signature.head.name()),
            Known {
                variables: signature.head.variables().into_iter().cloned().collect(),
                context: signature
                    .context
                    .iter()
                    .map(|constraint| (constraint.class.clone(), constraint.variable.clone()))
                    .collect(),
            },
        );
    }
    for instance in written {
        table.insert(
            (instance.class.clone(), instance.head.name()),
            Known {
                variables: instance.head.variables().into_iter().cloned().collect(),
                context: instance.context.clone(),
            },
        );
    }
    // Every candidate is in scope for the others, needing what it writes of its own and
    // nothing more yet.
    for candidate in candidates {
        table
            .entry((candidate.class.clone(), candidate.head.name()))
            .or_insert_with(|| Known {
                variables: candidate.head.variables().into_iter().cloned().collect(),
                context: Vec::new(),
            });
    }

    let plans: Vec<Result<Plan, Vec<Error>>> = candidates
        .iter()
        .zip(signatures)
        .map(|(candidate, signature)| plan(env, candidate, *signature))
        .collect();

    // The fixed point. A context only grows, from nothing, by what an instance asks given
    // every context as it stands. A written context is the whole of its instance's and does
    // not grow.
    loop {
        let mut grown = false;

        for plan in plans.iter().filter_map(|plan| plan.as_ref().ok()) {
            if plan.candidate.context.is_some() {
                continue;
            }
            let mut needed = Vec::new();
            for requirement in &plan.requirements {
                // A failure is the final pass's to report; here it asks nothing more.
                let _ = reduce(&table, plan.candidate.class, &requirement.tpe, &mut needed);
            }

            let key = (plan.candidate.class.clone(), plan.candidate.head.name());
            if let Some(known) = table.get_mut(&key) {
                for constraint in needed {
                    if !known.context.contains(&constraint) {
                        known.context.push(constraint);
                        grown = true;
                    }
                }
            }
        }

        if !grown {
            break;
        }
    }

    plans
        .into_iter()
        .zip(signatures)
        .map(|(plan, signature)| {
            let plan = plan?;
            let candidate = plan.candidate;

            // What is read now is the context the fixed point reached, read from scratch.
            let mut context: Vec<(QualName, Name)> = Vec::new();
            let mut errors = Vec::new();
            for requirement in &plan.requirements {
                let read = context.len();
                let reduced = reduce(&table, candidate.class, &requirement.tpe, &mut context);

                // `context` holds each constraint once, so what this argument added is what
                // no earlier argument asked, and a gap is reported at the first that needs it.
                if let Some((written, written_span)) = candidate.context {
                    for (class, variable) in &context[read..] {
                        if !provides(env, written, class, variable) {
                            errors.push(Error::DerivedInstanceContextTooNarrow(
                                Box::new(DerivedContextGap {
                                    class: candidate.class.unqualified_name(),
                                    missing: format!("{} {}", class.unqualified_name(), variable),
                                    part: requirement.part.clone(),
                                    argument: type_text(&requirement.tpe),
                                }),
                                written_span,
                                candidate.span,
                            ));
                        }
                    }
                }

                if let Err(missing) = reduced {
                    errors.push(Error::DerivedInstanceRequires(
                        Box::new(DerivedRequirement {
                            class: candidate.class.unqualified_name(),
                            part: requirement.part.clone(),
                            argument: type_text(&requirement.tpe),
                            function: matches!(missing, Type::Arrow(..)),
                            missing: type_text(&missing),
                            declared: match &plan.shape {
                                Shape::Union(_, declared) => *declared,
                                Shape::Tuple(_) | Shape::Unit => DeclarationSite::Unknown,
                            },
                        }),
                        candidate.span,
                    ));
                }
            }
            if !errors.is_empty() {
                return Err(errors);
            }

            // A class that is not derivable stopped the plan, so there is a signature.
            let Some(signature) = signature else {
                return Err(Vec::new());
            };
            let bindings = Generated::new(env, candidate, &plan.shape, signature).members();

            Ok(Derived {
                context: match candidate.context {
                    Some((written, _)) => written.to_vec(),
                    None => context
                        .into_iter()
                        .map(|(class, variable)| Constraint {
                            class,
                            variable,
                            span: candidate.span,
                        })
                        .collect(),
                },
                bindings,
            })
        })
        .collect()
}

/// Whether `context` provides `class` of `variable`: it writes that constraint, or one on
/// the same variable for a class `class` is a superclass of, however many classes away.
///
/// A class whose declaration is not in scope has no superclasses to follow here.
fn provides(
    env: &RootEnvironment,
    context: &[Constraint],
    class: &QualName,
    variable: &Name,
) -> bool {
    let mut pending: Vec<&QualName> = context
        .iter()
        .filter(|constraint| &constraint.variable == variable)
        .map(|constraint| &constraint.class)
        .collect();
    let mut seen: Vec<&QualName> = Vec::new();

    while let Some(candidate) = pending.pop() {
        if candidate == class {
            return true;
        }
        if seen.contains(&candidate) {
            continue;
        }
        seen.push(candidate);
        if let Some(signature) = env.class_signature(candidate) {
            pending.extend(
                signature
                    .superclasses
                    .iter()
                    .map(|superclass| &superclass.class),
            );
        }
    }

    false
}

/// Read one derived instance for its shape and what that shape asks, or say why it cannot
/// be derived.
///
/// An empty list of errors is a failure already reported: the class's own declaration did
/// not canonicalize, or the head's type did not.
fn plan<'a>(
    env: &'a RootEnvironment,
    candidate: &'a Candidate<'a>,
    signature: Option<&ClassSignature>,
) -> Result<Plan<'a>, Vec<Error>> {
    let Some(signature) = signature else {
        return Err(Vec::new());
    };

    let class = candidate.class.unqualified_name();
    if !signature.derivable() {
        // A class whose derivations were rejected has said so where it is declared, and an
        // instance asking to be derived does not say it again.
        return Err(if signature.derivations_rejected {
            Vec::new()
        } else {
            vec![Error::DerivedInstanceNotDerivable(class, candidate.span)]
        });
    }

    let (shape, requirements) = match candidate.head {
        InstanceHead::Type(name, variables) => {
            if scalars::scalar_of(name).is_some() {
                return Err(vec![Error::DerivedInstanceNoShape(
                    DerivedShapeProblem::Scalar(name.unqualified_name()),
                    candidate.span,
                )]);
            }

            let Some(union) = env.find_union(name) else {
                return Err(Vec::new());
            };
            if union.variants.is_empty() {
                return Err(vec![Error::DerivedInstanceNoShape(
                    DerivedShapeProblem::NoConstructors(name.unqualified_name()),
                    candidate.span,
                )]);
            }

            // The union's own variables stand for the head's, whatever it names them.
            let renamed: HashMap<&Name, &Name> = union.variables.iter().zip(variables).collect();
            let requirements = union
                .variants
                .iter()
                .flat_map(|constructor| {
                    let renamed = &renamed;
                    constructor
                        .type_parameters
                        .iter()
                        .map(move |tpe| Requirement {
                            part: DerivedPart::Variant(constructor.name.clone()),
                            tpe: rename(tpe, renamed),
                        })
                })
                .collect();

            let declared = if super::classes::declaring_module(name) == *env.module_name() {
                DeclarationSite::InThisModule(union.span)
            } else {
                DeclarationSite::Unknown
            };
            (Shape::Union(union, declared), requirements)
        }
        InstanceHead::Tuple(tuple) => {
            let requirements = tuple
                .iter()
                .enumerate()
                .map(|(index, variable)| Requirement {
                    part: DerivedPart::Element(index + 1),
                    tpe: Type::Variable(variable.clone()),
                })
                .collect();
            (Shape::Tuple(tuple.iter().count()), requirements)
        }
        InstanceHead::Unit => {
            if signature.walks_one_value() {
                return Err(vec![Error::DerivedInstanceNoShape(
                    DerivedShapeProblem::UnitHasNoElement(class),
                    candidate.span,
                )]);
            }
            (Shape::Unit, Vec::new())
        }
    };

    Ok(Plan {
        candidate,
        shape,
        requirements,
    })
}

/// `tpe` with each variable of `renamed` written as the variable it maps to.
fn rename(tpe: &Type, renamed: &HashMap<&Name, &Name>) -> Type {
    match tpe {
        Type::Variable(name) => Type::Variable(
            renamed
                .get(name)
                .map(|name| (*name).clone())
                .unwrap_or_else(|| name.clone()),
        ),
        Type::Type(name, args) => Type::Type(
            name.clone(),
            args.iter().map(|arg| rename(arg, renamed)).collect(),
        ),
        Type::Record(fields) => Type::Record(
            fields
                .iter()
                .map(|(label, tpe)| (label.clone(), rename(tpe, renamed)))
                .collect(),
        ),
        Type::Arrow(parameter, result) => Type::Arrow(
            Box::new(rename(parameter, renamed)),
            Box::new(rename(result, renamed)),
        ),
        Type::Tuple(tuple) => Type::Tuple(tuple.map(|element| rename(element, renamed))),
        Type::Unit => Type::Unit,
    }
}

/// Reduce `class` required of `tpe` to what it asks of the variables of the instance being
/// derived, added to `needed` as a class and a variable, each once. `Err` is the type that
/// has no instance.
///
/// A variable is a constraint. A declared type, a tuple and `()` need the instance of
/// their head, and what that instance's context asks of its variables is asked of the type's
/// arguments in turn, which are smaller than the type. A function type has no instance, and
/// a record is [accepted without being checked](../../../docs/tickets/lang-85.md), as the
/// solver accepts an obligation at one.
fn reduce(
    table: &Table,
    class: &QualName,
    tpe: &Type,
    needed: &mut Vec<(QualName, Name)>,
) -> Result<(), Type> {
    let (head, arguments): (HeadName, Vec<&Type>) = match tpe {
        Type::Variable(variable) => {
            let constraint = (class.clone(), variable.clone());
            if !needed.contains(&constraint) {
                needed.push(constraint);
            }
            return Ok(());
        }
        Type::Arrow(..) => return Err(tpe.clone()),
        Type::Record(_) => return Ok(()),
        Type::Type(name, arguments) => (HeadName::Type(name.clone()), arguments.iter().collect()),
        Type::Tuple(Tuple::Two(first, second)) => (HeadName::TwoTuple, vec![first, second]),
        Type::Tuple(Tuple::Three(first, second, third)) => {
            (HeadName::ThreeTuple, vec![first, second, third])
        }
        Type::Unit => (HeadName::Unit, Vec::new()),
    };

    let Some(known) = table.get(&(class.clone(), head)) else {
        return Err(tpe.clone());
    };
    if known.variables.len() != arguments.len() {
        return Err(tpe.clone());
    }

    for (class, variable) in &known.context {
        let Some(position) = known.variables.iter().position(|bound| bound == variable) else {
            continue;
        };
        reduce(table, class, arguments[position], needed)?;
    }

    Ok(())
}

/// `tpe` as the source writes it, for a message.
fn type_text(tpe: &Type) -> String {
    fn go(tpe: &Type, nested: bool) -> String {
        match tpe {
            Type::Variable(name) => name.to_string(),
            Type::Type(name, args) if args.is_empty() => name.unqualified_name().to_string(),
            Type::Type(name, args) => {
                let text = std::iter::once(name.unqualified_name().to_string())
                    .chain(args.iter().map(|arg| go(arg, true)))
                    .collect::<Vec<_>>()
                    .join(" ");
                if nested {
                    format!("({})", text)
                } else {
                    text
                }
            }
            Type::Arrow(parameter, result) => {
                let text = format!("{} -> {}", go(parameter, true), go(result, false));
                if nested {
                    format!("({})", text)
                } else {
                    text
                }
            }
            Type::Tuple(tuple) => format!(
                "({})",
                tuple
                    .iter()
                    .map(|element| go(element, false))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Unit => "()".to_owned(),
            Type::Record(fields) => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(label, tpe)| format!("{} : {}", label, go(tpe, false)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }

    go(tpe, false)
}

// ── Generating the members ───────────────────────────────────────────────────

/// The names a generated definition gives its parameters and the arguments it takes apart.
/// Each starts with a `$`, which no source file can write and no class author's binding can
/// therefore bind.
const LEFT: &str = "$left";
const RIGHT: &str = "$right";
const VALUE: &str = "$value";

fn left_argument(index: usize) -> Name {
    Name::new(format!("$a{}", index + 1))
}

fn right_argument(index: usize) -> Name {
    Name::new(format!("$b{}", index + 1))
}

/// The members of one derived instance, written out as canonical code.
struct Generated<'a> {
    /// Where the word `derived` was written: the span of every node generated.
    span: NodeSpan,
    shape: &'a Shape<'a>,
    class: &'a QualName,
    signature: &'a ClassSignature,
    /// Whether the class is declared in the module the instance is in, which decides how a
    /// member is named.
    class_here: bool,
    /// How many fresh names the instance's members have used: see [`Generated::fresh`].
    serial: Cell<usize>,
}

/// What a placed body's names stand for: the fresh name each local the body bound has, and
/// the one local that is replaced by an expression.
#[derive(Clone, Default)]
struct Scope<'a> {
    renames: HashMap<Name, Name>,
    /// The second parameter of `combine`, and the rest of the walk it stands for.
    replaced: Option<(&'a Name, &'a Expression)>,
}

impl<'a> Scope<'a> {
    /// `name`, bound again from here on, to `fresh`.
    fn rename(&mut self, name: &Name, fresh: Name) {
        self.renames.insert(name.clone(), fresh);
        // What a pattern binds is what the name means now, and not what it replaced.
        if matches!(self.replaced, Some((replaced, _)) if replaced == name) {
            self.replaced = None;
        }
    }

    /// The fresh name `name` is bound to, or `name` where nothing here bound it.
    fn renamed(&self, name: &Name) -> Name {
        self.renames
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.clone())
    }
}

impl<'a> Generated<'a> {
    fn new(
        env: &RootEnvironment,
        candidate: &'a Candidate<'a>,
        shape: &'a Shape<'a>,
        signature: &'a ClassSignature,
    ) -> Generated<'a> {
        Generated {
            span: candidate.span,
            shape,
            class: candidate.class,
            signature,
            class_here: super::classes::declaring_module(candidate.class) == *env.module_name(),
            serial: Cell::new(0),
        }
    }

    /// One definition per member of the class, in the order it declares them.
    fn members(&self) -> Vec<Value> {
        self.signature
            .members
            .iter()
            .filter_map(|member| {
                let derivation = self.signature.derivation(&member.name)?;
                Some(self.member(member, derivation))
            })
            .collect()
    }

    fn member(&self, member: &Member, derivation: &Derivation) -> Value {
        let (parameters, body) = match &derivation.bindings {
            DerivationBindings::Pair {
                matched,
                differed,
                combine,
            } => (
                vec![LEFT, RIGHT],
                self.walk_pair(member, matched, differed, combine),
            ),
            DerivationBindings::Single {
                at_constructor,
                combine,
            } => (
                vec![VALUE],
                self.walk_single(member, at_constructor, combine),
            ),
        };

        Value::Value {
            name: member.name.clone(),
            patterns: parameters
                .into_iter()
                .map(|name| self.pattern(PatternKind::Variable(Name::new(name))))
                .collect(),
            body,
            span: self.span,
        }
    }

    /// The walk over two values: take the first apart, take the second apart, and fold the
    /// arguments of the pair when the two are of one constructor.
    fn walk_pair(
        &self,
        member: &Member,
        matched: &Value,
        differed: &Value,
        combine: &Value,
    ) -> Expression {
        let alternatives = self.shape.alternatives();

        let branches = alternatives
            .iter()
            .enumerate()
            .map(|(index, alternative)| {
                let left_names: Vec<Name> = (0..alternative.arity).map(left_argument).collect();
                let right_names: Vec<Name> = (0..alternative.arity).map(right_argument).collect();

                // The answer for each pair of arguments, folded right-nested onto what the
                // walk ends at when nothing told the two values apart.
                let mut rest = self.place(matched, Vec::new());
                for (left, right) in left_names.iter().zip(&right_names).rev() {
                    let answer = self.apply(
                        self.apply(self.member_reference(member), self.local(left)),
                        self.local(right),
                    );
                    rest = self.place_combine(combine, answer, rest);
                }

                // The second value is the same constructor, or it is not and `differed`
                // answers for the pair, handed where each was declared.
                let mut same_or_not = vec![(
                    self.alternative_pattern(alternative, Some(&right_names)),
                    rest,
                )];
                for (other_index, other) in alternatives.iter().enumerate() {
                    if other_index != index {
                        same_or_not.push((
                            self.alternative_pattern(other, None),
                            self.place(
                                differed,
                                vec![self.position(index), self.position(other_index)],
                            ),
                        ));
                    }
                }

                (
                    self.alternative_pattern(alternative, Some(&left_names)),
                    self.case(self.local_str(RIGHT), same_or_not),
                )
            })
            .collect();

        self.case(self.local_str(LEFT), branches)
    }

    /// The walk over one value: take it apart, and fold the answer for its constructor with
    /// the answer for each argument.
    fn walk_single(&self, member: &Member, at_constructor: &Value, combine: &Value) -> Expression {
        let alternatives = self.shape.alternatives();

        let branches = alternatives
            .iter()
            .enumerate()
            .map(|(index, alternative)| {
                let names: Vec<Name> = (0..alternative.arity).map(left_argument).collect();

                // `combine p (combine a1 (… an))`: the constructor's answer when it has
                // one, and each argument's, the last of them standing for the rest of the
                // walk where the one before it asks for it.
                let mut answers: Vec<Expression> = Vec::new();
                if alternative.constructor.is_some() {
                    answers.push(self.place(at_constructor, vec![self.position(index)]));
                }
                for name in &names {
                    answers.push(self.apply(self.member_reference(member), self.local(name)));
                }

                let body = match answers.pop() {
                    Some(last) => answers.into_iter().rev().fold(last, |rest, answer| {
                        self.place_combine(combine, answer, rest)
                    }),
                    // A tuple or `()` with no element to begin at has no answer; a class
                    // that walks one value is not derived for `()`, and a tuple has
                    // elements, so this is not reached.
                    None => self.expression(ExpressionKind::Unit),
                };

                (self.alternative_pattern(alternative, Some(&names)), body)
            })
            .collect();

        self.case(self.local_str(VALUE), branches)
    }

    /// The binding of the derivation placed for `arguments`: each parameter bound, by a
    /// `case` of one branch, to the argument it receives, and any argument beyond the
    /// parameters the binding was written with applied to the body.
    fn place(&self, binding: &Value, arguments: Vec<Expression>) -> Expression {
        let (patterns, body) = value_parts(binding);

        let mut scope = Scope::default();
        let mut arguments = arguments.into_iter();
        let mut bound: Vec<(Pattern, Expression)> = Vec::new();
        for pattern in patterns {
            let Some(argument) = arguments.next() else {
                break;
            };
            bound.push((self.bind_pattern(pattern, &mut scope), argument));
        }

        let applied = arguments.fold(self.rewrite(body, &scope), |function, argument| {
            self.apply(function, argument)
        });

        self.bind(bound, applied)
    }

    /// `combine` placed for the answer to one part and the rest of the walk.
    ///
    /// The first parameter is bound once, to the answer. The second is replaced by the
    /// rest, wherever the body names it and nowhere else, so that the rest is evaluated
    /// where the body reaches it. A binding written with fewer parameters is applied to
    /// what it did not bind, which makes the rest an argument as a call would.
    fn place_combine(&self, combine: &Value, answer: Expression, rest: Expression) -> Expression {
        let (patterns, body) = value_parts(combine);
        let mut scope = Scope::default();

        match patterns.as_slice() {
            [] => self.apply(self.apply(self.rewrite(body, &scope), answer), rest),
            [first] => {
                let first = self.bind_pattern(first, &mut scope);
                let applied = self.apply(self.rewrite(body, &scope), rest);
                self.bind(vec![(first, answer)], applied)
            }
            [first, second, ..] => {
                let first = self.bind_pattern(first, &mut scope);
                match &second.kind {
                    PatternKind::Variable(name) => {
                        scope.replaced = Some((name, &rest));
                        let placed = self.rewrite(body, &scope);
                        self.bind(vec![(first, answer)], placed)
                    }
                    // The rest is never named, so it is never evaluated.
                    PatternKind::Anything => {
                        let placed = self.rewrite(body, &scope);
                        self.bind(vec![(first, answer)], placed)
                    }
                    // A pattern that takes the rest apart has to have it to do so.
                    _ => {
                        let second = self.bind_pattern(second, &mut scope);
                        let placed = self.rewrite(body, &scope);
                        self.bind(vec![(first, answer), (second, rest)], placed)
                    }
                }
            }
        }
    }

    /// `body` under one `case` of one branch for each of `bound`, the first outermost.
    fn bind(&self, bound: Vec<(Pattern, Expression)>, body: Expression) -> Expression {
        bound
            .into_iter()
            .rev()
            .fold(body, |body, (pattern, scrutinee)| {
                self.case(scrutinee, vec![(pattern, body)])
            })
    }

    // ── Rewriting a placed body ──────────────────────────────────────────────

    /// A name for one variable a placed body binds, which no other binder of the instance
    /// shares: `$` and a serial, then the name the class wrote — `$3$x`.
    ///
    /// A body is placed once for every part the walk meets, and a part's body is placed
    /// inside the one before it, so copies of one body would each bind the same spelling
    /// inside the others' scope. No generated binder is ever in the scope of another binding
    /// of the same name, so generated code does not depend on how a later phase scopes a name
    /// that is bound again, which matters while the typer drops a shadowed outer binder when
    /// an inner scope ends (`BUG-49`). The same name is bound in more than one place all the same, since
    /// the rest of the walk is copied at each mention of `combine`'s second parameter; those
    /// are sibling scopes.
    fn fresh(&self, original: &Name) -> Name {
        let serial = self.serial.get();
        self.serial.set(serial + 1);

        Name::new(format!("${}${}", serial, original))
    }

    /// `pattern` as a placed body binds it: every node carries the span of `derived`, and
    /// every variable is a fresh one, which `scope` now maps the written name to.
    fn bind_pattern(&self, pattern: &Pattern, scope: &mut Scope) -> Pattern {
        let kind = match &pattern.kind {
            PatternKind::Variable(name) => {
                let fresh = self.fresh(name);
                scope.rename(name, fresh.clone());
                PatternKind::Variable(fresh)
            }
            PatternKind::Constructor { ctor, args } => PatternKind::Constructor {
                ctor: ctor.clone(),
                args: args
                    .iter()
                    .map(|arg| self.bind_pattern(arg, scope))
                    .collect(),
            },
            PatternKind::Tuple(tuple) => {
                PatternKind::Tuple(tuple.map(|element| self.bind_pattern(element, scope)))
            }
            PatternKind::Hole(args) => PatternKind::Hole(
                args.iter()
                    .map(|arg| self.bind_pattern(arg, scope))
                    .collect(),
            ),
            PatternKind::Record(fields) => PatternKind::Record(
                fields
                    .iter()
                    .map(|field| PatternField {
                        label: field.label.clone(),
                        label_span: self.span,
                        pattern: self.bind_pattern(&field.pattern, scope),
                    })
                    .collect(),
            ),
            kind @ (PatternKind::Anything
            | PatternKind::Int(_)
            | PatternKind::Float(_)
            | PatternKind::Char(_)
            | PatternKind::String(_)
            | PatternKind::Unit) => kind.clone(),
        };

        self.pattern(kind)
    }

    /// `expression` as it is placed here: every node carries the span of `derived`, a
    /// local is the fresh name `scope` gives it, and a reference to the local `scope`
    /// replaces is the expression it replaces it with.
    fn rewrite(&self, expression: &Expression, scope: &Scope) -> Expression {
        let again = |expression: &Expression| self.rewrite(expression, scope);

        let kind = match &expression.kind {
            ExpressionKind::VarLocal(name) => match scope.replaced {
                Some((replaced, with)) if replaced == name => return with.clone(),
                _ => ExpressionKind::VarLocal(scope.renamed(name)),
            },
            ExpressionKind::Apply(function, argument) => {
                ExpressionKind::Apply(Box::new(again(function)), Box::new(again(argument)))
            }
            ExpressionKind::If(condition, then, otherwise) => ExpressionKind::If(
                Box::new(again(condition)),
                Box::new(again(then)),
                Box::new(again(otherwise)),
            ),
            ExpressionKind::Case(scrutinee, branches) => ExpressionKind::Case(
                Box::new(again(scrutinee)),
                branches
                    .iter()
                    .map(|branch| {
                        // A branch binds its own names, which shadow what the scope
                        // renames or replaces.
                        let mut inner = scope.clone();
                        let pattern = self.bind_pattern(&branch.pattern, &mut inner);
                        CaseBranch {
                            pattern,
                            expression: self.rewrite(&branch.expression, &inner),
                            span: self.span,
                        }
                    })
                    .collect(),
            ),
            ExpressionKind::Accessor(label, _) => {
                ExpressionKind::Accessor(label.clone(), self.span)
            }
            ExpressionKind::Access(record, label, _) => {
                ExpressionKind::Access(Box::new(again(record)), label.clone(), self.span)
            }
            ExpressionKind::Record(fields) => ExpressionKind::Record(
                fields
                    .iter()
                    .map(|field| self.rewrite_field(field, scope))
                    .collect(),
            ),
            ExpressionKind::Update(record, fields) => ExpressionKind::Update(
                Box::new(again(record)),
                fields
                    .iter()
                    .map(|field| self.rewrite_field(field, scope))
                    .collect(),
            ),
            ExpressionKind::Tuple(tuple) => ExpressionKind::Tuple(tuple.map(again)),
            kind @ (ExpressionKind::VarTopLevel(_)
            | ExpressionKind::VarKernel(_)
            | ExpressionKind::VarForeign(..)
            | ExpressionKind::VarConstructor(..)
            | ExpressionKind::Char(_)
            | ExpressionKind::String(_)
            | ExpressionKind::Int(_)
            | ExpressionKind::Float(_)
            | ExpressionKind::Unit
            | ExpressionKind::Hole) => kind.clone(),
        };

        self.expression(kind)
    }

    fn rewrite_field(&self, field: &Field, scope: &Scope) -> Field {
        Field {
            label: field.label.clone(),
            label_span: self.span,
            value: self.rewrite(&field.value, scope),
        }
    }

    // ── Building code ────────────────────────────────────────────────────────

    fn expression(&self, kind: ExpressionKind) -> Expression {
        Expression::new(self.span, kind)
    }

    fn pattern(&self, kind: PatternKind) -> Pattern {
        Pattern::new(self.span, kind)
    }

    fn local(&self, name: &Name) -> Expression {
        self.expression(ExpressionKind::VarLocal(name.clone()))
    }

    fn local_str(&self, name: &str) -> Expression {
        self.local(&Name::new(name))
    }

    fn apply(&self, function: Expression, argument: Expression) -> Expression {
        self.expression(ExpressionKind::Apply(
            Box::new(function),
            Box::new(argument),
        ))
    }

    fn case(&self, scrutinee: Expression, branches: Vec<(Pattern, Expression)>) -> Expression {
        self.expression(ExpressionKind::Case(
            Box::new(scrutinee),
            branches
                .into_iter()
                .map(|(pattern, expression)| CaseBranch {
                    pattern,
                    expression,
                    span: self.span,
                })
                .collect(),
        ))
    }

    /// A reference to `member`, which the module declaring the class names the way it names
    /// any value of its own, and every other module the way it names an imported one.
    fn member_reference(&self, member: &Member) -> Expression {
        let name = self.class.sibling(&member.name);

        self.expression(if self.class_here {
            ExpressionKind::VarTopLevel(name)
        } else {
            ExpressionKind::VarForeign(name, self.class.package().clone(), member.tpe.clone())
        })
    }

    /// The `Position` of the `index`th alternative of the type: the constructor of
    /// `Basics.Position`, which is exposed to no module and so is named by its
    /// declaration, applied to the place as an `Int`.
    fn position(&self, index: usize) -> Expression {
        let position = scalars::POSITION.qual_name();
        let int = scalars::INT.qual_name();

        let constructor = self.expression(ExpressionKind::VarConstructor(
            position.sibling(&Name::new(scalars::POSITION.name)),
            Type::Arrow(
                Box::new(Type::Type(int, Vec::new())),
                Box::new(Type::Type(position, Vec::new())),
            ),
        ));
        // A place that does not fit an `Int` is a type with more constructors than a
        // module can declare.
        let place = i64::try_from(index).unwrap_or(i64::MAX);

        self.apply(constructor, self.expression(ExpressionKind::Int(place)))
    }

    /// The pattern for `alternative`, with each argument bound to the name of `names` or,
    /// with none, matching anything.
    fn alternative_pattern(&self, alternative: &Alternative, names: Option<&[Name]>) -> Pattern {
        let arguments: Vec<Pattern> = (0..alternative.arity)
            .map(|index| {
                self.pattern(match names.and_then(|names| names.get(index)) {
                    Some(name) => PatternKind::Variable(name.clone()),
                    None => PatternKind::Anything,
                })
            })
            .collect();

        match alternative.constructor {
            Some(constructor) => self.pattern(PatternKind::Constructor {
                ctor: constructor.clone(),
                args: arguments,
            }),
            None => match tuple_of(arguments) {
                Ok(tuple) => self.pattern(PatternKind::Tuple(tuple)),
                Err(_) => self.pattern(PatternKind::Unit),
            },
        }
    }
}

/// The patterns of `value` and its body, whichever form it was canonicalized as.
fn value_parts(value: &Value) -> (Vec<&Pattern>, &Expression) {
    match value {
        Value::Value { patterns, body, .. } => (patterns.iter().collect(), body),
        Value::TypedValue { patterns, body, .. } => {
            (patterns.iter().map(|(pattern, _)| pattern).collect(), body)
        }
    }
}

/// `items` as a tuple of two or three, or handed back when it is neither.
fn tuple_of<T>(mut items: Vec<T>) -> Result<Tuple<T>, Vec<T>> {
    match items.len() {
        2 | 3 => {
            let third = if items.len() == 3 { items.pop() } else { None };
            match (items.pop(), items.pop(), third) {
                (Some(second), Some(first), Some(third)) => Ok(Tuple::three(first, second, third)),
                (Some(second), Some(first), None) => Ok(Tuple::two(first, second)),
                _ => Err(Vec::new()),
            }
        }
        _ => Err(items),
    }
}
