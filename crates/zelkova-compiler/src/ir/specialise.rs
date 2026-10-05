//! Specialisation: which instance a use of a class member is, and which copy of a constrained
//! function a use needs, worked out once for the whole build.
//!
//! Every phase before this one reads a module. This one reads all of them, because a
//! constrained function is written in one module, used in another and answered by an
//! instance in a third, and none of the three can see what the others hold.
//! [`specialise`] is handed every checked module of a build and changes each [`Module`]
//! in place; nothing else about the build moves. A backend then reads the result. One IR
//! serves both targets ([`DEC-18` decision
//! 2](../../../docs/decisions/dec-18.md#2--one-ir-serves-both-targets-and-javascript-is-written-first)),
//! so the pass is here and not in `zelkova_js`.
//!
//! # What it makes
//!
//! No dictionary is built or passed at run time
//! ([`DEC-2` decision 7](../../../docs/decisions/dec-2.md#7--dictionaries-are-erased-by-specialisation-not-passed)).
//! A reference that carried obligations is rewritten to name the function that answers them:
//!
//! - **A class member** at a type whose instance has no context becomes
//!   [`ReferenceKind::InstanceMember`], a direct reference to that instance's binding, an
//!   ordinary declaration of the module that declares the instance.
//! - **A constrained function**, or a class member at a type whose instance has a context,
//!   becomes [`ReferenceKind::Specialised`]: a [`Specialisation`] of the module the reference
//!   is in, which is a copy of the declaration with its constrained variables at the ground
//!   types the reference gave them. Its body is read the same way, so the obligation on the
//!   declaration's own variable that the type checker discharged as given is, in the copy,
//!   one at a ground type, and its instance a lookup.
//!
//! Where an obligation's instance is found is [the lookup the type checker
//! did](crate::typer): the class and the name at the front of the type, with at most one
//! answer by [the head
//! rule](../../../docs/spec/type-classes.md#what-an-instance-is-declared-for). A superclass
//! needs nothing of its own. A use of `eq` inside a function constrained by `Comparable a`
//! asks for `Eq a`, which at a ground type is an instance like any other, and
//! `instance Comparable Colour` could not have been declared without it.
//!
//! # Where a copy goes, and why
//!
//! A specialisation is placed in **the module that uses it**. Its body calls the members of
//! an instance, and the instance may be declared by a module that *imports* the declaring
//! one: `Basics.min` at `App.Colour` calls a `compare` that `App` declares. Placed beside
//! `min`, the copy would make `Basics` import `App`, and the build's modules could no
//! longer be initialised in dependency order. The using module is downstream of everything
//! the copy mentions.
//!
//! The cost is a copy in each module that uses a key. Nothing a program can observe: a copy
//! of a function is code written twice, and for a binding with no parameters it is one
//! evaluation for each using module where [the chapter
//! says](../../../docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once)
//! "once", which changes work done and never an answer, a Zelkova value having no identity.
//! A reading of that promise that covers it is a `SPEC-` ticket to file, and not a design to
//! change here.
//!
//! A copy of a body written in another module names that module's values, so its
//! references are carried across: a [`TopLevel`](ReferenceKind::TopLevel) name of the
//! declaring module becomes a [`Foreign`](ReferenceKind::Foreign) one of the using module,
//! with the arity its declaration was written with, and a `Foreign` one that names the using
//! module itself becomes a `TopLevel`. A backend therefore needs the declaring module to make
//! such a value reachable even where its `exposing` list does not, which is
//! [`mentioned_by_copies`].
//!
//! # What a key is
//!
//! A specialisation is one declaration at one assignment of ground types to its **constrained**
//! variables ([`Specialisation::key`]). A variable with no constraint needs no copy, since
//! nothing a JavaScript module emits depends on it; two uses at one key in one module are one
//! specialisation. The assignment is found by matching the types the declaration's
//! [`context`](Declaration::context) is over against the ground ones the reference gave them.
//! Where the declaration's body forced a constrained variable to a concrete type — the width
//! of the hole an annotation has until
//! [`LANG-12`](../../../docs/tickets/lang-12.md) — the context holds that type, there is no
//! variable to bind, and the key is shorter by it.
//!
//! A member of an instance with a context is specialised at the ground types of the variables
//! its own context constrains. They are found by reading the member's declared type against
//! the type of the use, and not by reading the instance's head: the member's IR is over the
//! variables its own inference gave it, which are the head's only when its patterns happen to
//! pin them there (`same = sameWrap` does not).
//!
//! Only what the key is made of has to be a type. The instance a use asks for is found from
//! the name at the front of its type, so a use at `Phantom b`, which no one can make ground,
//! is the instance of `Phantom` all the same; and a variable with no constraint is not in the
//! key. A constrained variable that is still a variable is [`Error::NotGround`].
//!
//! # The limit
//!
//! A constrained function that asks for itself at an ever larger type has no finite set of
//! specialisations. `f x = f (Box x)` under `Eq a` needs `f` at `Colour`, at `Box Colour`, at
//! `Box (Box Colour)`, without end
//! ([`DEC-24` decision 9](../../../docs/decisions/dec-24.md#9--a-constrained-function-whose-specialisations-never-end-is-an-error)).
//! The pass reads one chain of specialisations to its end before the next, and a chain
//! that holds one declaration [`SPECIALISATION_LIMIT`] times is stopped and reported as
//! [`Error::Unbounded`], naming the declaration and the type it had reached. The count is of
//! one declaration within one chain: a chain through a hundred different functions is a finite
//! set, and the same loop written through two functions is caught the same way.
//!
//! The first such chain stops the module's work: what was waiting to be read is dropped and
//! nothing new is found, because a loop that grows at two types has a leaf for each way down,
//! about 2^32 of them, and a build that has failed has no use for any. A declaration is
//! reported once, however many modules reach its limit.
//!
//! # Order
//!
//! Specialisations are numbered in the order they were found, which is fixed by the order of
//! the modules and of what each holds: every module's declarations in name order, then its
//! instances in the order written. Nothing here iterates a hash table to produce an order,
//! so two runs over one unchanged build produce the same text.
//!
//! [`initialisation_items`] is the order a module's parameterless items are initialised in
//! once the new ones are among them. It reads what the items mention, through functions as
//! well as directly, because an instance's bindings are not in the dependency graph
//! canonicalization read: which of them a name reaches is a fact about types.

use std::collections::{BTreeSet, HashMap, HashSet};

use super::{
    Body, Declaration, Field, InstanceMember, Module, Predicate, Reference, ReferenceKind,
    Saturation, SubPattern, Subject, TermPattern, TermPatternKind, TypeBinder, TypedTerm,
    TypedTermKind,
};
use crate::canonical::HeadName;
use crate::name::{Name, QualName};
use crate::typer::{head_of, Type, TypeVariable};
use crate::{CheckedModule, ModuleName, PhaseError, SpanLabel};
use zelkova_syntax::position::NodeSpan;
use zelkova_syntax::tuple::Tuple;

/// How many times one declaration may appear in one chain of specialisations before the chain
/// is reported as having no end.
///
/// Generous on purpose: a type is nested this deep in no program anyone writes, and a legal
/// chain holds a declaration once for each level of the type it walks, so `show` at
/// `List (List (List Int))` holds it three times. The count is of one declaration, which is
/// why a program with more than this many constrained functions is not affected.
pub const SPECIALISATION_LIMIT: usize = 32;

// ── Errors ────────────────────────────────────────────────────────────────────

/// Why the specialisations of a build could not be found.
///
/// [`Unbounded`](Self::Unbounded) is a mistake in a program the type checker cannot see.
/// [`NotGround`](Self::NotGround) is a program it accepted whose use asks for a constrained
/// function at a type that is still a variable, one that has no copy to make. The other two
/// are what is left if a module reaches this pass that the type checker did not accept, which
/// nothing does today: a build with an error never gets here. They are errors and not panics
/// so that a caller holding one is told which declaration it was reading.
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    /// A constrained declaration whose specialisations never end: it needs itself, directly
    /// or through other declarations, at a type larger each time.
    Unbounded {
        /// The declaration, as the source names it: a function, or the member an instance
        /// binds.
        declaration: Name,
        /// The ground types it had been reached at when the limit was: its
        /// [key](super::Specialisation::key).
        types: Vec<Type>,
        /// Where the declaration was written.
        span: NodeSpan,
        /// The module whose use began the chain.
        used_in: Name,
    },
    /// A use that asks for an instance of `class` at `tpe` and no instance of it is part of
    /// the build.
    NoInstance {
        class: Name,
        tpe: Type,
        /// The declaration the use was written in.
        within: Name,
        used_in: Name,
        span: NodeSpan,
    },
    /// A use of a constrained name whose constrained variable was still a type variable once
    /// the declaration it is in was specialised, so there is no key to copy at; or a use of a
    /// class member at a bare variable, which has no name to look an instance up by. A use at
    /// `Phantom b` is neither, whatever `b` is.
    NotGround {
        class: Name,
        tpe: Type,
        within: Name,
        used_in: Name,
        span: NodeSpan,
    },
    /// A use of a constrained name whose declaration, or whose instance's member, has no IR to
    /// copy: the type checker could not check it.
    NoDeclaration {
        name: Name,
        within: Name,
        used_in: Name,
        span: NodeSpan,
    },
}

impl PhaseError for Error {
    fn message(&self) -> String {
        match self {
            Error::Unbounded { declaration, .. } => format!(
                "`{}` needs itself at ever larger types, so it has no finite set of specialisations to compile",
                declaration.as_str()
            ),
            Error::NoInstance {
                class,
                tpe,
                within,
                ..
            } => format!(
                "no instance of `{}` at `{}` was found for the use in `{}`",
                class.as_str(),
                tpe.written_with_letters(),
                within.as_str()
            ),
            Error::NotGround {
                class,
                tpe,
                within,
                ..
            } => format!(
                "the use of an instance of `{}` in `{}` is at `{}`, which is not a type yet",
                class.as_str(),
                within.as_str(),
                tpe.written_with_letters()
            ),
            Error::NoDeclaration { name, within, .. } => format!(
                "`{}` has nothing to specialise, but `{}` uses it as a name that asks for an instance",
                name.as_str(),
                within.as_str()
            ),
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Error::Unbounded {
                declaration,
                types,
                used_in,
                ..
            } => vec![
                format!(
                    "{} nested specialisations of `{}`, the limit, were reached at `{}`, while compiling `{}`",
                    SPECIALISATION_LIMIT,
                    declaration.as_str(),
                    join(types),
                    used_in.as_str()
                ),
                "a constrained function is compiled once for each type it is used at, so the types it is used at have to be finite".to_string(),
            ],
            Error::NoInstance { tpe, used_in, .. } => {
                let mut notes = vec![format!("while compiling `{}`", used_in.as_str())];
                if matches!(tpe, Type::Record(_)) {
                    notes.push(
                        "a class at a record type is not compiled yet (`LANG-85`)".to_string(),
                    );
                }
                notes
            }
            Error::NotGround { used_in, .. } | Error::NoDeclaration { used_in, .. } => {
                vec![format!("while compiling `{}`", used_in.as_str())]
            }
        }
    }

    fn labels(&self) -> Vec<SpanLabel> {
        let (span, message) = match self {
            Error::Unbounded { span, .. } => (span, "this declaration"),
            Error::NoInstance { span, .. }
            | Error::NotGround { span, .. }
            | Error::NoDeclaration { span, .. } => (span, "this use"),
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

/// Types as a message writes a list of them: `Box Colour, Int`.
fn join(types: &[Type]) -> String {
    types
        .iter()
        .map(|tpe| tpe.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

/// The errors one module of the build has, by the module the code that raised them was
/// written in: the module whose file the labels point into.
#[derive(Debug)]
pub struct ModuleErrors {
    pub module: ModuleName,
    pub errors: Vec<Error>,
}

// ── The pass ──────────────────────────────────────────────────────────────────

/// Find every specialisation `modules` need and resolve every reference that carried
/// obligations, in place.
///
/// `modules` is the whole of a build, or at least of one tree of it: a module is read
/// against the others by name, and what one of them refers to that is not here is
/// [`Error::NoDeclaration`] or [`Error::NoInstance`]. A build that emits a second tree holding
/// more modules passes all of them, once: what a module of the first tree holds does not
/// depend on the others, since a copy is made for the module that uses it and for none
/// other.
///
/// The roots are every declaration with no context and every member of an instance with no
/// context, in every module. The body of each is read, and each reference in it that
/// carries obligations is resolved at the types it is now at: to a member of an instance
/// directly, or to a specialisation of the module the body is in, which is read in its turn
/// if its key is new. See the module's documentation for what a key is and for the limit.
///
/// On `Err` nothing has been changed, and each error is in the bucket of the module its
/// labels point into, in the order the modules were handed in.
pub fn specialise(modules: &mut [&mut CheckedModule]) -> Result<(), Vec<ModuleErrors>> {
    let outcome = {
        let world = World::of(modules.iter().map(|module| &**module).collect());
        world.read()
    };

    if outcome.errors.iter().any(|errors| !errors.is_empty()) {
        return Err(outcome
            .errors
            .into_iter()
            .enumerate()
            .filter(|(_, errors)| !errors.is_empty())
            .map(|(index, errors)| ModuleErrors {
                module: modules[index].ir.name.clone(),
                errors,
            })
            .collect());
    }

    for (module, found) in modules.iter_mut().zip(outcome.found) {
        for (place, body) in found.roots {
            let declaration = match place {
                Place::Declaration(index) => module.ir.declarations.get_mut(index),
                Place::Member { instance, member } => module
                    .ir
                    .instances
                    .get_mut(instance)
                    .and_then(|instance| instance.members.get_mut(member)),
            };
            if let Some(declaration) = declaration {
                declaration.body = Some(body);
            }
        }
        module.ir.specialisations = found.specialisations;
    }

    Ok(())
}

/// What the pass reads: every module, and the three indexes it looks things up by.
struct World<'a> {
    modules: Vec<&'a CheckedModule>,
    /// A module's place in `modules`, by name.
    index: HashMap<ModuleName, usize>,
    /// Each module's declarations, by name, as a place in `ir.declarations`.
    declarations: Vec<HashMap<&'a Name, usize>>,
    /// The class each class member belongs to, by the member's qualified name. A member is a
    /// value of the module that declares its class.
    members: HashMap<QualName, QualName>,
    /// Every instance of the build, by class and the name at the front of its head: the
    /// module, and the place in that module's `ir.instances`.
    instances: HashMap<(QualName, HeadName), (usize, usize)>,
}

/// What reading the build found: for each module, what to put back, and the errors.
struct Outcome {
    found: Vec<Found>,
    errors: Vec<Vec<Error>>,
}

/// What one module is given back.
#[derive(Default)]
struct Found {
    /// The body of each root, resolved.
    roots: Vec<(Place, Body)>,
    specialisations: Vec<super::Specialisation>,
}

/// Where a declaration is in a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Place {
    Declaration(usize),
    Member { instance: usize, member: usize },
}

/// Where a declaration to be copied is: the module, and the place in it.
#[derive(Debug, Clone, Copy)]
struct Location {
    module: usize,
    place: Place,
}

/// A map from a type variable to the ground type it is at.
type Assignment = HashMap<TypeVariable, Type>;

impl<'a> World<'a> {
    fn of(modules: Vec<&'a CheckedModule>) -> World<'a> {
        let mut index = HashMap::new();
        let mut declarations = Vec::new();
        let mut members = HashMap::new();
        let mut instances = HashMap::new();

        for (position, module) in modules.iter().enumerate() {
            index.insert(module.ir.name.clone(), position);
            declarations.push(
                module
                    .ir
                    .declarations
                    .iter()
                    .enumerate()
                    .map(|(at, declaration)| (&declaration.name, at))
                    .collect(),
            );

            let name = &module.canonical.name;
            for (class_name, class) in &module.canonical.classes {
                let class_qualified = name.qualify_name(class_name);
                for member in &class.signature.members {
                    members.insert(name.qualify_name(&member.name), class_qualified.clone());
                }
            }

            for (at, instance) in module.ir.instances.iter().enumerate() {
                instances.insert(
                    (instance.class.clone(), instance.head_name.clone()),
                    (position, at),
                );
            }
        }

        World {
            modules,
            index,
            declarations,
            members,
            instances,
        }
    }

    /// The module a name is declared in, as a place in `modules`.
    fn module_of(&self, name: &QualName) -> Option<usize> {
        self.index
            .get(&ModuleName::new(name.package().clone(), name.module_name()))
            .copied()
    }

    /// The declaration at `location`.
    fn declaration(&self, location: Location) -> Option<&'a Declaration> {
        let module = self.modules.get(location.module)?;
        match location.place {
            Place::Declaration(index) => module.ir.declarations.get(index),
            Place::Member { instance, member } => {
                module.ir.instances.get(instance)?.members.get(member)
            }
        }
    }

    fn read(&self) -> Outcome {
        let mut found = Vec::new();
        let mut errors: Vec<Vec<Error>> = self.modules.iter().map(|_| Vec::new()).collect();

        for using in 0..self.modules.len() {
            found.push(self.read_module(using, &mut errors));
        }

        Outcome { found, errors }
    }

    /// The roots of module `using`, and every specialisation they lead to.
    fn read_module(&self, using: usize, errors: &mut [Vec<Error>]) -> Found {
        let ir = &self.modules[using].ir;
        let mut used = Used::default();
        let mut found = Found::default();

        for (index, declaration) in ir.declarations.iter().enumerate() {
            if !declaration.context.is_empty() || declaration.body.is_none() {
                continue;
            }
            let location = Location {
                module: using,
                place: Place::Declaration(index),
            };
            if let Some(body) =
                self.read_body(using, location, declaration, None, &mut used, errors)
            {
                found.roots.push((location.place, body));
            }
        }

        for (instance_index, instance) in ir.instances.iter().enumerate() {
            if instance.rejected || !instance.context.is_empty() {
                continue;
            }
            for (member_index, member) in instance.members.iter().enumerate() {
                if !member.context.is_empty() || member.body.is_none() {
                    continue;
                }
                let location = Location {
                    module: using,
                    place: Place::Member {
                        instance: instance_index,
                        member: member_index,
                    },
                };
                if let Some(body) = self.read_body(using, location, member, None, &mut used, errors)
                {
                    found.roots.push((location.place, body));
                }
            }
        }

        // One chain at a time: the last found is read first, so a chain that never ends
        // reaches the limit along its own length and not after every sibling of every step.
        while let Some(index) = used.pending.pop() {
            let (location, assignment) = {
                let spec = &used.specs[index];
                (spec.location, spec.assignment.clone())
            };
            let Some(source) = self.declaration(location) else {
                continue;
            };
            let declaration = self.read_copy(
                using,
                location,
                source,
                &assignment,
                Some(index),
                &mut used,
                errors,
            );
            used.specs[index].declaration = declaration;
        }

        found.specialisations = used
            .specs
            .into_iter()
            .filter_map(|spec| {
                Some(super::Specialisation {
                    of: spec.subject,
                    key: spec.key,
                    declaration: spec.declaration?,
                })
            })
            .collect();
        found
    }

    /// The body of `declaration`, with every reference that carried obligations resolved,
    /// for a root: nothing is assigned, and nothing is copied.
    fn read_body(
        &self,
        using: usize,
        location: Location,
        declaration: &Declaration,
        parent: Option<usize>,
        used: &mut Used,
        errors: &mut [Vec<Error>],
    ) -> Option<Body> {
        let body = declaration.body.as_ref()?;
        let assignment = Assignment::new();
        let mut reader = Reader {
            world: self,
            using,
            source: location.module,
            assignment: &assignment,
            within: &declaration.name,
            parent,
            used,
            errors,
        };
        Some(reader.body(body))
    }

    /// `source` copied for module `using` with `assignment` applied.
    #[allow(clippy::too_many_arguments)]
    fn read_copy(
        &self,
        using: usize,
        location: Location,
        source: &Declaration,
        assignment: &Assignment,
        parent: Option<usize>,
        used: &mut Used,
        errors: &mut [Vec<Error>],
    ) -> Option<Declaration> {
        let body = source.body.as_ref()?;
        let mut reader = Reader {
            world: self,
            using,
            source: location.module,
            assignment,
            within: &source.name,
            parent,
            used,
            errors,
        };
        let body = reader.body(body);

        Some(Declaration {
            name: source.name.clone(),
            arity: source.arity,
            tpe: apply(&source.tpe, assignment),
            context: Vec::new(),
            body: Some(body),
            span: source.span,
        })
    }
}

/// The specialisations of the module being read.
#[derive(Default)]
struct Used {
    specs: Vec<Spec>,
    /// The specialisation at a key, by what it is a copy of.
    table: HashMap<(Subject, Vec<Type>), usize>,
    /// The specialisations found and not yet read, the last found on top.
    pending: Vec<usize>,
    /// Whether a chain of this module has reached the limit. The build has failed by then, so
    /// nothing more is found: see [`Reader::specialise`].
    stopped: bool,
}

/// A specialisation, from the moment it is found until its body has been read.
struct Spec {
    subject: Subject,
    /// Where the declaration it is a copy of is.
    location: Location,
    key: Vec<Type>,
    assignment: Assignment,
    /// The specialisation whose body mentioned this one first, which is how the limit
    /// follows a chain back.
    parent: Option<usize>,
    /// How many arguments a direct call supplies, which a use that is a class member needs
    /// before the copy has been read.
    arity: usize,
    declaration: Option<Declaration>,
}

// ── Reading a body ────────────────────────────────────────────────────────────

/// One declaration being copied: its body is rebuilt node by node, each type with the
/// assignment applied and each reference resolved.
struct Reader<'r, 'a> {
    world: &'r World<'a>,
    /// The module the copy is for.
    using: usize,
    /// The module the declaration was written in.
    source: usize,
    assignment: &'r Assignment,
    /// The declaration being read, which an error names.
    within: &'r Name,
    /// The specialisation being read, if it is one.
    parent: Option<usize>,
    used: &'r mut Used,
    errors: &'r mut [Vec<Error>],
}

impl Reader<'_, '_> {
    fn ty(&self, tpe: &Type) -> Type {
        apply(tpe, self.assignment)
    }

    fn binder(&self, binder: &TypeBinder) -> TypeBinder {
        TypeBinder {
            name: binder.name.clone(),
            tpe: self.ty(&binder.tpe),
        }
    }

    fn body(&mut self, body: &Body) -> Body {
        Body {
            parameters: body
                .parameters
                .iter()
                .map(|parameter| self.binder(parameter))
                .collect(),
            expression: self.term(&body.expression),
        }
    }

    fn term(&mut self, term: &TypedTerm) -> TypedTerm {
        let kind = match &term.kind {
            TypedTermKind::Int(value) => TypedTermKind::Int(*value),
            TypedTermKind::Char(value) => TypedTermKind::Char(*value),
            TypedTermKind::String(value) => TypedTermKind::String(value.clone()),
            TypedTermKind::Float(value) => TypedTermKind::Float(*value),
            TypedTermKind::Unit => TypedTermKind::Unit,
            TypedTermKind::Hole => TypedTermKind::Hole,
            TypedTermKind::Identifier { reference, context } => {
                return self.identifier(term, reference, context);
            }
            TypedTermKind::Apply { .. } => return self.application(term),
            TypedTermKind::Fun { param, body } => TypedTermKind::Fun {
                param: self.binder(param),
                body: Box::new(self.term(body)),
            },
            TypedTermKind::If {
                cond,
                true_branch,
                false_branch,
            } => TypedTermKind::If {
                cond: Box::new(self.term(cond)),
                true_branch: Box::new(self.term(true_branch)),
                false_branch: Box::new(self.term(false_branch)),
            },
            TypedTermKind::Let {
                binding,
                value,
                body,
            } => TypedTermKind::Let {
                binding: self.binder(binding),
                value: Box::new(self.term(value)),
                body: Box::new(self.term(body)),
            },
            TypedTermKind::Tuple(tuple) => TypedTermKind::Tuple(match tuple {
                Tuple::Two(a, b) => Tuple::two(self.term(a), self.term(b)),
                Tuple::Three(a, b, c) => Tuple::three(self.term(a), self.term(b), self.term(c)),
            }),
            TypedTermKind::Case {
                scrutinee,
                branches,
                form,
            } => TypedTermKind::Case {
                scrutinee: Box::new(self.term(scrutinee)),
                branches: branches
                    .iter()
                    .map(|(pattern, body)| (self.pattern(pattern), Box::new(self.term(body))))
                    .collect(),
                form: *form,
            },
            TypedTermKind::Record(fields) => TypedTermKind::Record(self.fields(fields)),
            TypedTermKind::Update { record, fields } => TypedTermKind::Update {
                record: Box::new(self.term(record)),
                fields: self.fields(fields),
            },
            TypedTermKind::Access {
                record,
                label,
                label_span,
            } => TypedTermKind::Access {
                record: Box::new(self.term(record)),
                label: label.clone(),
                label_span: *label_span,
            },
            TypedTermKind::Accessor { label, label_span } => TypedTermKind::Accessor {
                label: label.clone(),
                label_span: *label_span,
            },
        };

        TypedTerm {
            span: term.span,
            tpe: self.ty(&term.tpe),
            kind,
        }
    }

    fn fields(&mut self, fields: &[Field<TypedTerm>]) -> Vec<Field<TypedTerm>> {
        fields
            .iter()
            .map(|field| Field {
                label: field.label.clone(),
                label_span: field.label_span,
                value: self.term(&field.value),
            })
            .collect()
    }

    fn pattern(&self, pattern: &TermPattern) -> TermPattern {
        let kind = match &pattern.kind {
            TermPatternKind::Anything => TermPatternKind::Anything,
            TermPatternKind::Bind(name) => TermPatternKind::Bind(name.clone()),
            TermPatternKind::Unit => TermPatternKind::Unit,
            TermPatternKind::Literal { tpe, value } => TermPatternKind::Literal {
                tpe: self.ty(tpe),
                value: *value,
            },
            TermPatternKind::Constructor {
                ctor,
                adt_args,
                args,
            } => TermPatternKind::Constructor {
                ctor: ctor.clone(),
                adt_args: adt_args.iter().map(|tpe| self.ty(tpe)).collect(),
                args: args.iter().map(|arg| self.sub_pattern(arg)).collect(),
            },
            TermPatternKind::Tuple { elements } => TermPatternKind::Tuple {
                elements: elements.map(|element| self.sub_pattern(element)),
            },
            TermPatternKind::Hole { args } => TermPatternKind::Hole {
                args: args.iter().map(|arg| self.sub_pattern(arg)).collect(),
            },
            TermPatternKind::Record { fields } => TermPatternKind::Record {
                fields: fields
                    .iter()
                    .map(|field| Field {
                        label: field.label.clone(),
                        label_span: field.label_span,
                        value: self.sub_pattern(&field.value),
                    })
                    .collect(),
            },
        };

        TermPattern {
            span: pattern.span,
            kind,
        }
    }

    fn sub_pattern(&self, sub: &SubPattern) -> SubPattern {
        SubPattern {
            tpe: self.ty(&sub.tpe),
            pattern: self.pattern(&sub.pattern),
        }
    }

    /// An application spine, rebuilt: the head, then each argument in the order written.
    ///
    /// A class member's reference was left unsaturated by the type checker, which knew no
    /// arity for it — a member is not a declaration of any module — so a head that resolves to
    /// one is where the spine's saturation is worked out: the node that supplies the member's
    /// last parameter is the direct call, as for any other declaration. Every other spine
    /// keeps the flags it was given.
    fn application(&mut self, term: &TypedTerm) -> TypedTerm {
        let mut spine = Vec::new();
        let mut head = term;
        while let TypedTermKind::Apply {
            fun,
            arg,
            saturation,
        } = &head.kind
        {
            spine.push((head, arg.as_ref(), *saturation));
            head = fun;
        }
        spine.reverse();

        let mut built = self.term(head);
        let member_arity = self.member_arity(&built);

        for (position, (node, argument, saturation)) in spine.into_iter().enumerate() {
            let argument = self.term(argument);
            let saturation = match member_arity {
                Some(arity) if arity == position + 1 => Saturation::Saturated,
                Some(_) => Saturation::Partial,
                None => saturation,
            };
            built = TypedTerm {
                span: node.span,
                tpe: self.ty(&node.tpe),
                kind: TypedTermKind::Apply {
                    fun: Box::new(built),
                    arg: Box::new(argument),
                    saturation,
                },
            };
        }

        built
    }

    /// The arity of the function `head` is, when it is a class member resolved.
    fn member_arity(&self, head: &TypedTerm) -> Option<usize> {
        let TypedTermKind::Identifier { reference, .. } = &head.kind else {
            return None;
        };
        match &reference.kind {
            ReferenceKind::InstanceMember(member) => Some(member.arity),
            ReferenceKind::Specialised(index) => {
                let spec = self.used.specs.get(*index)?;
                matches!(spec.subject, Subject::InstanceMember { .. }).then_some(spec.arity)
            }
            _ => None,
        }
    }

    /// An error about a use, which is written in the module the code being read was written
    /// in: that module's file is the one the label points into.
    fn error(&mut self, error: Error) {
        self.errors[self.source].push(error);
    }

    fn used_in(&self) -> Name {
        self.world.modules[self.using].ir.name.name().clone()
    }

    /// A reference, and what it asks. One that asks nothing is carried across modules; one
    /// that does is resolved at the types it is now at.
    fn identifier(
        &mut self,
        term: &TypedTerm,
        reference: &Reference,
        context: &[Predicate],
    ) -> TypedTerm {
        let tpe = self.ty(&term.tpe);
        let span = term.span;

        if context.is_empty() {
            return TypedTerm {
                span,
                tpe,
                kind: TypedTermKind::Identifier {
                    reference: self.rebase(reference),
                    context: Vec::new(),
                },
            };
        }

        let ground: Vec<Predicate> = context
            .iter()
            .map(|predicate| Predicate {
                class: predicate.class.clone(),
                tpe: self.ty(&predicate.tpe),
            })
            .collect();

        let resolved = self.resolve(reference, &ground, &tpe, span);

        // A reference no obligation could be resolved for keeps its obligations, and an
        // error stands behind it, so the build that holds it writes nothing.
        let (reference, context) = match resolved {
            Some(kind) => (
                Reference {
                    name: reference.name.clone(),
                    kind,
                },
                Vec::new(),
            ),
            None => (self.rebase(reference), ground),
        };

        TypedTerm {
            span,
            tpe,
            kind: TypedTermKind::Identifier { reference, context },
        }
    }

    /// What a reference that asks for instances at `ground` is, once they are found.
    fn resolve(
        &mut self,
        reference: &Reference,
        ground: &[Predicate],
        use_tpe: &Type,
        span: NodeSpan,
    ) -> Option<ReferenceKind> {
        let name = match &reference.kind {
            ReferenceKind::TopLevel(name) | ReferenceKind::Foreign(name, _, _) => name,
            _ => {
                self.no_declaration(Name::new(reference.name.clone()), span);
                return None;
            }
        };

        match self.world.members.get(name) {
            Some(class) => self.resolve_member(name, class, ground, use_tpe, span),
            None => self.resolve_declaration(reference, name, ground, span),
        }
    }

    /// The types `assignment` gives the variables `context` is over, in the order they occur:
    /// a declaration's key. Only these variables matter, so a type that holds others, or none
    /// of them, is as good as any: what has to be a type is what the key is made of.
    ///
    /// A variable that is still a variable here, or still holds one, is [`Error::NotGround`]
    /// and `None`. A variable `assignment` has nothing for is left out of the key: where a
    /// declaration's body forced it to a type, there is nothing to bind.
    fn bind(
        &mut self,
        context: &[Predicate],
        assignment: &Assignment,
        span: NodeSpan,
    ) -> Option<Vec<(TypeVariable, Type)>> {
        let bound: Vec<(TypeVariable, Type)> = constrained(context)
            .into_iter()
            .filter_map(|variable| {
                let tpe = assignment.get(&variable)?.clone();
                Some((variable, tpe))
            })
            .collect();

        let Some((variable, _)) = bound.iter().find(|(_, tpe)| !is_ground(tpe)) else {
            return Some(bound);
        };
        if let Some(predicate) = context
            .iter()
            .find(|predicate| constrained(std::slice::from_ref(predicate)).contains(variable))
        {
            let tpe = apply(&predicate.tpe, assignment);
            self.not_ground(&predicate.class, &tpe, span);
        }
        None
    }

    fn not_ground(&mut self, class: &QualName, tpe: &Type, span: NodeSpan) {
        let error = Error::NotGround {
            class: class.unqualified_name(),
            tpe: tpe.clone(),
            within: self.within.clone(),
            used_in: self.used_in(),
            span,
        };
        self.error(error);
    }

    fn no_declaration(&mut self, name: Name, span: NodeSpan) {
        let error = Error::NoDeclaration {
            name,
            within: self.within.clone(),
            used_in: self.used_in(),
            span,
        };
        self.error(error);
    }

    fn no_instance(&mut self, class: &QualName, tpe: &Type, span: NodeSpan) {
        let error = Error::NoInstance {
            class: class.unqualified_name(),
            tpe: tpe.clone(),
            within: self.within.clone(),
            used_in: self.used_in(),
            span,
        };
        self.error(error);
    }

    /// A class member at a type: the binding of the instance for the type's head, or a
    /// specialisation of it when the instance has a context.
    fn resolve_member(
        &mut self,
        member: &QualName,
        class: &QualName,
        ground: &[Predicate],
        use_tpe: &Type,
        span: NodeSpan,
    ) -> Option<ReferenceKind> {
        let predicate = ground
            .iter()
            .find(|predicate| &predicate.class == class)
            .or_else(|| ground.first())?;

        // An instance is found by the name at the front of the type and nothing else, so the
        // rest of the type may still hold variables. A bare variable has no name to look up.
        let Some((head, _)) = head_of(&predicate.tpe) else {
            if matches!(predicate.tpe, Type::Variable(_)) {
                self.not_ground(class, &predicate.tpe, span);
            } else {
                self.no_instance(class, &predicate.tpe, span);
            }
            return None;
        };
        let Some(&(module, instance_index)) =
            self.world.instances.get(&(class.clone(), head.clone()))
        else {
            self.no_instance(class, &predicate.tpe, span);
            return None;
        };

        let ir = &self.world.modules[module].ir;
        let instance = &ir.instances[instance_index];
        let member_name = member.unqualified_name();
        let Some(member_index) = instance
            .members
            .iter()
            .position(|declaration| declaration.name == member_name)
        else {
            self.no_declaration(member_name, span);
            return None;
        };
        if instance.rejected {
            self.no_instance(class, &predicate.tpe, span);
            return None;
        }
        let declaration = &instance.members[member_index];

        if declaration.context.is_empty() {
            return Some(ReferenceKind::InstanceMember(Box::new(InstanceMember {
                module: ir.name.clone(),
                class: class.clone(),
                head,
                member: member_name,
                arity: declaration.arity,
            })));
        }

        // What each constrained variable of the member is at comes from the member's own
        // type read against the type of the use, and not from the instance's head: the
        // member's IR is over the variables its own inference gave it, which are the head's
        // only when the member's patterns pinned them to it (`same = sameWrap` does not).
        let mut assignment = Assignment::new();
        match_type(&declaration.tpe, use_tpe, &mut assignment);
        let bound = self.bind(&declaration.context, &assignment, span)?;

        let subject = Subject::InstanceMember {
            class: class.clone(),
            head,
            member: member_name,
        };
        let location = Location {
            module,
            place: Place::Member {
                instance: instance_index,
                member: member_index,
            },
        };
        self.specialise(subject, location, declaration, bound)
            .map(ReferenceKind::Specialised)
    }

    /// A constrained function at the types its context was given: a specialisation of it.
    fn resolve_declaration(
        &mut self,
        reference: &Reference,
        name: &QualName,
        ground: &[Predicate],
        span: NodeSpan,
    ) -> Option<ReferenceKind> {
        let found = self.world.module_of(name).and_then(|module| {
            let at = *self.world.declarations[module].get(&name.unqualified_name())?;
            Some((module, at, &self.world.modules[module].ir.declarations[at]))
        });
        let Some((module, at, declaration)) = found else {
            self.no_declaration(name.unqualified_name(), span);
            return None;
        };

        // A name whose declaration asks nothing has nothing to copy, whatever its use carried.
        if declaration.context.is_empty() {
            return Some(self.rebase(reference).kind);
        }

        let mut assignment = Assignment::new();
        for (pattern, predicate) in declaration.context.iter().zip(ground) {
            // Where a body forced a constrained variable to a type the pattern is that type
            // and has nothing to bind; a use at another one binds nothing either.
            match_type(&pattern.tpe, &predicate.tpe, &mut assignment);
        }
        let bound = self.bind(&declaration.context, &assignment, span)?;

        let location = Location {
            module,
            place: Place::Declaration(at),
        };
        self.specialise(
            Subject::Declaration(name.clone()),
            location,
            declaration,
            bound,
        )
        .map(ReferenceKind::Specialised)
    }

    /// The specialisation of `subject` at `bound`, found if it is new, or `None` when the
    /// chain it ends has reached the limit.
    fn specialise(
        &mut self,
        subject: Subject,
        location: Location,
        declaration: &Declaration,
        bound: Vec<(TypeVariable, Type)>,
    ) -> Option<usize> {
        let key: Vec<Type> = bound.iter().map(|(_, tpe)| tpe.clone()).collect();

        if let Some(&index) = self.used.table.get(&(subject.clone(), key.clone())) {
            return Some(index);
        }

        let mut occurrences = 0;
        let mut ancestor = self.parent;
        while let Some(index) = ancestor {
            let spec = &self.used.specs[index];
            if spec.subject == subject {
                occurrences += 1;
            }
            ancestor = spec.parent;
        }
        if occurrences >= SPECIALISATION_LIMIT {
            // The build has failed, and everything still waiting to be read could only add
            // more of the same: a loop that grows at two types is a tree of keys with a
            // leaf for each way down, 2^32 of them. So the module's work stops here.
            self.used.stopped = true;
            self.used.pending.clear();

            // The declaration is in its own module's file, and its span is that file's. One
            // declaration is one error, however many modules use it.
            let reported = self.errors[location.module].iter().any(|error| {
                matches!(error, Error::Unbounded { declaration: reported, span, .. }
                    if reported == &declaration.name && span.to_range() == declaration.span.to_range())
            });
            if !reported {
                let used_in = self.used_in();
                self.errors[location.module].push(Error::Unbounded {
                    declaration: declaration.name.clone(),
                    types: key,
                    span: declaration.span,
                    used_in,
                });
            }
            return None;
        }
        if self.used.stopped {
            return None;
        }

        let index = self.used.specs.len();
        self.used
            .table
            .insert((subject.clone(), key.clone()), index);
        self.used.specs.push(Spec {
            subject,
            location,
            key,
            assignment: bound.into_iter().collect(),
            parent: self.parent,
            arity: declaration.arity,
            declaration: None,
        });
        self.used.pending.push(index);
        Some(index)
    }

    /// `reference` as it reads in the module the copy is for, when it was written in
    /// another.
    ///
    /// A name of the declaring module is the using module's import, with the arity its
    /// declaration was written with; a name of the using module that the declaring module
    /// imported is the using module's own.
    fn rebase(&self, reference: &Reference) -> Reference {
        let kind = match &reference.kind {
            ReferenceKind::TopLevel(name) if self.source != self.using => {
                let arity = self.world.declarations[self.source]
                    .get(&name.unqualified_name())
                    .map(|at| self.world.modules[self.source].ir.declarations[*at].arity)
                    .unwrap_or(0);
                ReferenceKind::Foreign(name.clone(), name.package().clone(), arity)
            }
            ReferenceKind::Foreign(name, _, _)
                if self.world.module_of(name) == Some(self.using) =>
            {
                ReferenceKind::TopLevel(name.clone())
            }
            other => other.clone(),
        };

        Reference {
            name: reference.name.clone(),
            kind,
        }
    }
}

// ── Types ─────────────────────────────────────────────────────────────────────

/// `tpe` with each variable `assignment` has a type for replaced by it.
fn apply(tpe: &Type, assignment: &Assignment) -> Type {
    if assignment.is_empty() {
        return tpe.clone();
    }

    match tpe {
        Type::Variable(variable) => assignment
            .get(variable)
            .cloned()
            .unwrap_or_else(|| tpe.clone()),
        Type::Literal(_) | Type::Unit => tpe.clone(),
        Type::Fun {
            param_tpe,
            return_tpe,
        } => Type::Fun {
            param_tpe: Box::new(apply(param_tpe, assignment)),
            return_tpe: Box::new(apply(return_tpe, assignment)),
        },
        Type::Tuple(tuple) => Type::Tuple(tuple.map(|element| apply(element, assignment))),
        Type::Adt(name, arguments) => Type::Adt(
            name.clone(),
            arguments
                .iter()
                .map(|argument| apply(argument, assignment))
                .collect(),
        ),
        Type::Record(fields) => Type::Record(
            fields
                .iter()
                .map(|(label, tpe)| (label.clone(), apply(tpe, assignment)))
                .collect(),
        ),
    }
}

/// Whether `tpe` holds no variable.
fn is_ground(tpe: &Type) -> bool {
    match tpe {
        Type::Variable(_) => false,
        Type::Literal(_) | Type::Unit => true,
        Type::Fun {
            param_tpe,
            return_tpe,
        } => is_ground(param_tpe) && is_ground(return_tpe),
        Type::Tuple(tuple) => tuple.iter().all(is_ground),
        Type::Adt(_, arguments) => arguments.iter().all(is_ground),
        Type::Record(fields) => fields.values().all(is_ground),
    }
}

/// Read `ground` against `pattern`, binding each variable of `pattern` to the type it meets
/// in `assignment`. Whether the two have the same shape.
///
/// A variable met twice must meet one type both times. What is bound before a mismatch stays
/// bound, which is why a caller that cares about the answer reads the whole of `pattern`.
fn match_type(pattern: &Type, ground: &Type, assignment: &mut Assignment) -> bool {
    match (pattern, ground) {
        (Type::Variable(variable), ground) => match assignment.get(variable) {
            Some(bound) => bound == ground,
            None => {
                assignment.insert(variable.clone(), ground.clone());
                true
            }
        },
        (Type::Literal(a), Type::Literal(b)) => a == b,
        (Type::Unit, Type::Unit) => true,
        (
            Type::Fun {
                param_tpe: a_param,
                return_tpe: a_return,
            },
            Type::Fun {
                param_tpe: b_param,
                return_tpe: b_return,
            },
        ) => {
            // Both halves are read even when the first mismatches, so the assignment holds
            // all that can be bound.
            let param = match_type(a_param, b_param, assignment);
            let result = match_type(a_return, b_return, assignment);
            param && result
        }
        (Type::Tuple(a), Type::Tuple(b)) => match (a, b) {
            (Tuple::Two(a1, a2), Tuple::Two(b1, b2)) => {
                let first = match_type(a1, b1, assignment);
                let second = match_type(a2, b2, assignment);
                first && second
            }
            (Tuple::Three(a1, a2, a3), Tuple::Three(b1, b2, b3)) => {
                let first = match_type(a1, b1, assignment);
                let second = match_type(a2, b2, assignment);
                let third = match_type(a3, b3, assignment);
                first && second && third
            }
            _ => false,
        },
        (Type::Adt(a_name, a_args), Type::Adt(b_name, b_args)) => {
            if a_name != b_name || a_args.len() != b_args.len() {
                return false;
            }
            let mut matched = true;
            for (a, b) in a_args.iter().zip(b_args) {
                matched &= match_type(a, b, assignment);
            }
            matched
        }
        (Type::Record(a), Type::Record(b)) => {
            if a.len() != b.len() || !a.keys().eq(b.keys()) {
                return false;
            }
            let mut matched = true;
            for (a, b) in a.values().zip(b.values()) {
                matched &= match_type(a, b, assignment);
            }
            matched
        }
        _ => false,
    }
}

/// The variables a context is over, each once, in the order they first occur.
fn constrained(context: &[Predicate]) -> Vec<TypeVariable> {
    fn collect(tpe: &Type, into: &mut Vec<TypeVariable>) {
        match tpe {
            Type::Variable(variable) => {
                if !into.contains(variable) {
                    into.push(variable.clone());
                }
            }
            Type::Literal(_) | Type::Unit => {}
            Type::Fun {
                param_tpe,
                return_tpe,
            } => {
                collect(param_tpe, into);
                collect(return_tpe, into);
            }
            Type::Tuple(tuple) => tuple.iter().for_each(|element| collect(element, into)),
            Type::Adt(_, arguments) => arguments
                .iter()
                .for_each(|argument| collect(argument, into)),
            Type::Record(fields) => fields.values().for_each(|field| collect(field, into)),
        }
    }

    let mut variables = Vec::new();
    for predicate in context {
        collect(&predicate.tpe, &mut variables);
    }
    variables
}

// ── What a module mentions ────────────────────────────────────────────────────

/// Every reference in `term`, in the order a reader meets them.
fn references<'a>(term: &'a TypedTerm, into: &mut Vec<&'a Reference>) {
    match &term.kind {
        TypedTermKind::Identifier { reference, .. } => into.push(reference),
        TypedTermKind::Apply { fun, arg, .. } => {
            references(fun, into);
            references(arg, into);
        }
        TypedTermKind::Fun { body, .. } => references(body, into),
        TypedTermKind::If {
            cond,
            true_branch,
            false_branch,
        } => {
            references(cond, into);
            references(true_branch, into);
            references(false_branch, into);
        }
        TypedTermKind::Let { value, body, .. } => {
            references(value, into);
            references(body, into);
        }
        TypedTermKind::Tuple(tuple) => tuple.iter().for_each(|element| references(element, into)),
        TypedTermKind::Case {
            scrutinee,
            branches,
            ..
        } => {
            references(scrutinee, into);
            for (_, body) in branches {
                references(body, into);
            }
        }
        TypedTermKind::Record(fields) => fields
            .iter()
            .for_each(|field| references(&field.value, into)),
        TypedTermKind::Update { record, fields } => {
            references(record, into);
            fields
                .iter()
                .for_each(|field| references(&field.value, into));
        }
        TypedTermKind::Access { record, .. } => references(record, into),
        TypedTermKind::Int(_)
        | TypedTermKind::Char(_)
        | TypedTermKind::String(_)
        | TypedTermKind::Float(_)
        | TypedTermKind::Unit
        | TypedTermKind::Hole
        | TypedTermKind::Accessor { .. } => {}
    }
}

/// The values of `module` that a copy of its code, placed in another module, names: the
/// declarations its constrained declarations and the bindings of its instances mention.
///
/// A copy is made from what was written in `module`, and it can name a value `module` does
/// not expose — `exposing` is a rule about what another module's source may write, and
/// canonicalization enforces it whatever this returns. So a backend that makes each copy
/// reachable exports these as well as what the header lists. A constrained declaration
/// has no function under its own name, so it is never in the answer; the instance bindings
/// of a module are every one of its instances', whether or not the instance has a context,
/// since a binding that is not copied is not harmed by being named. A derivation's bindings
/// are placed in the instances that derive them, so they are among those.
///
/// Sorted by name.
pub fn mentioned_by_copies(module: &Module) -> Vec<Name> {
    let emitted: HashSet<&Name> = module
        .declarations
        .iter()
        .filter(|declaration| declaration.context.is_empty() && declaration.body.is_some())
        .map(|declaration| &declaration.name)
        .collect();

    let constrained = module
        .declarations
        .iter()
        .filter(|declaration| !declaration.context.is_empty());
    let bindings = module
        .instances
        .iter()
        .flat_map(|instance| instance.members.iter());

    let mut found = BTreeSet::new();
    for declaration in constrained.chain(bindings) {
        let Some(body) = &declaration.body else {
            continue;
        };
        let mut mentioned = Vec::new();
        references(&body.expression, &mut mentioned);

        for reference in mentioned {
            if let ReferenceKind::TopLevel(name) = &reference.kind {
                let name = name.unqualified_name();
                if emitted.contains(&name) {
                    found.insert(name);
                }
            }
        }
    }

    found.into_iter().collect()
}

// ── Initialisation order ──────────────────────────────────────────────────────

/// One thing a module emits under a name of its own: where it is, in the module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Item {
    /// A declaration: a place in [`Module::declarations`].
    Declaration(usize),
    /// A member of an instance that has no context.
    InstanceMember { instance: usize, member: usize },
    /// A specialisation: a place in [`Module::specialisations`].
    Specialisation(usize),
}

/// The items of `module` that take no parameter, in the order they must be initialised: each
/// only after every parameterless item it depends on, directly or through a function it
/// mentions.
///
/// [`Module::initialisation_order`] is that order for the declarations alone, and is where
/// this starts: every declaration in it keeps its place, and the others follow in the order
/// they were found, each placed after what it depends on. An instance's bindings are not in
/// the graph that order was computed from, since which instance a name reaches is a fact
/// about types; and a specialisation was not there to be found. So this reads what each item
/// mentions in the IR, now that every mention names its target, and puts what a parameterless
/// item reaches ahead of it. An item in a cycle is placed once and the cycle is left as it
/// is, in name order: canonicalization rejects a cycle of declarations, and one through an
/// instance is a language question and not this function's.
///
/// A module that holds no instance member and no specialisation gets exactly
/// [`Module::initialisation_order`], as [`Item::Declaration`]s.
pub fn initialisation_items(module: &Module) -> Vec<Item> {
    let mut items: Vec<(Item, &Declaration)> = Vec::new();
    let mut declarations: HashMap<Name, Item> = HashMap::new();

    for (index, declaration) in module.declarations.iter().enumerate() {
        if declaration.context.is_empty() && declaration.body.is_some() {
            let item = Item::Declaration(index);
            declarations.insert(declaration.name.clone(), item);
            items.push((item, declaration));
        }
    }

    let mut members: HashMap<(QualName, HeadName, Name), Item> = HashMap::new();
    for (instance_index, instance) in module.instances.iter().enumerate() {
        if instance.rejected || !instance.context.is_empty() {
            continue;
        }
        let head = &instance.head_name;
        for (member_index, member) in instance.members.iter().enumerate() {
            if member.context.is_empty() && member.body.is_some() {
                let item = Item::InstanceMember {
                    instance: instance_index,
                    member: member_index,
                };
                members.insert(
                    (instance.class.clone(), head.clone(), member.name.clone()),
                    item,
                );
                items.push((item, member));
            }
        }
    }

    for (index, specialisation) in module.specialisations.iter().enumerate() {
        items.push((Item::Specialisation(index), &specialisation.declaration));
    }

    // What each item mentions that this module emits, in the order a reader meets it.
    let mut mentions: HashMap<Item, Vec<Item>> = HashMap::new();
    let mut constants: HashSet<Item> = HashSet::new();
    for (item, declaration) in &items {
        let Some(body) = &declaration.body else {
            continue;
        };
        if body.parameters.is_empty() {
            constants.insert(*item);
        }

        let mut found = Vec::new();
        references(&body.expression, &mut found);
        let reached: Vec<Item> = found
            .into_iter()
            .filter_map(|reference| match &reference.kind {
                ReferenceKind::TopLevel(name) => {
                    declarations.get(&name.unqualified_name()).copied()
                }
                ReferenceKind::Specialised(index) => {
                    (*index < module.specialisations.len()).then_some(Item::Specialisation(*index))
                }
                ReferenceKind::InstanceMember(member) if member.module == module.name => members
                    .get(&(
                        member.class.clone(),
                        member.head.clone(),
                        member.member.clone(),
                    ))
                    .copied(),
                _ => None,
            })
            .collect();
        mentions.insert(*item, reached);
    }

    // The parameterless items an item reaches, through functions and not through another
    // parameterless item, whose own dependencies are placed when it is.
    let dependencies = |item: Item| -> Vec<Item> {
        let mut seen = HashSet::from([item]);
        let mut reached = Vec::new();
        let mut pending: Vec<Item> = mentions
            .get(&item)
            .map(|mentioned| mentioned.iter().rev().copied().collect())
            .unwrap_or_default();

        while let Some(next) = pending.pop() {
            if !seen.insert(next) {
                continue;
            }
            if constants.contains(&next) {
                reached.push(next);
            } else if let Some(mentioned) = mentions.get(&next) {
                pending.extend(mentioned.iter().rev().copied());
            }
        }
        reached
    };

    let mut sequence: Vec<Item> = module
        .initialisation_order
        .iter()
        .filter_map(|name| declarations.get(name).copied())
        .filter(|item| constants.contains(item))
        .collect();
    let placed: HashSet<Item> = sequence.iter().copied().collect();
    sequence.extend(
        items
            .iter()
            .map(|(item, _)| *item)
            .filter(|item| constants.contains(item) && !placed.contains(item)),
    );

    let mut order = Vec::new();
    let mut started: HashSet<Item> = HashSet::new();
    for root in sequence {
        if !started.insert(root) {
            continue;
        }
        let mut stack = vec![(root, dependencies(root).into_iter())];

        while let Some((current, remaining)) = stack.last_mut() {
            let current = *current;

            match remaining.next() {
                Some(dependency) => {
                    if started.insert(dependency) {
                        stack.push((dependency, dependencies(dependency).into_iter()));
                    }
                }
                None => {
                    order.push(current);
                    stack.pop();
                }
            }
        }
    }

    order
}
