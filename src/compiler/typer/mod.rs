//! This module contains the type checker pass of the language
//!
//! It works with the source AST and will perform two jobs:
//! - type checks the different declarations and expression
//! - infer the types when not declared in the source
//!
//! I have no idea how that works; so bear with me while I explore
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
//!     (i) under the assumption that z has type X1, t has type X2, and
//!     (ii) T is a supertype of X1 → X2."
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
use super::canonical::Module;
use log::debug;
use std::collections::HashMap;

pub enum Error {}

pub fn type_check(_module: &Module) -> Result<(), Error> {
    Ok(())
}

// First try of an implementation. Not linked to the rest of the code base for simplicity's sake.

type Identifier = String;

#[derive(Debug, Clone)] // TODO Remove clone when not needed anymore
/// untyped term. In zelkova that would be the parsed source (or canonical, not sure yet)
pub enum Term {
    // literals
    Bool(bool),
    Int(u32),
    Identifier(Identifier), // VAR
    Fun {
        param: Identifier,
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
        binding: Identifier,
        value: Box<Term>,
        body: Box<Term>,
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
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Literal(TypeLiteral),
    Variable(TypeVariable),
    Fun {
        param_tpe: Box<Type>,
        return_tpe: Box<Type>,
    },
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Literal(lit) => write!(f, "Lit({:?})", lit),
            Type::Variable(TypeVariable { id }) => write!(f, "Var(#{})", id),
            Type::Fun {
                param_tpe,
                return_tpe,
            } => write!(f, "Fun({:?} -> {:?})", param_tpe, return_tpe),
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

#[derive(Debug)]
enum TypedTerm {
    Int {
        tpe: Type,
        value: u32,
    },
    Bool {
        tpe: Type,
        value: bool,
    },
    // TODO Do I want to keep this name ? Or named Variable ? Something else ?
    Identifier {
        tpe: Type,
        name: Identifier,
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
}

impl TypedTerm {
    fn tpe(&self) -> &Type {
        match &self {
            TypedTerm::Int { tpe, .. } => tpe,
            TypedTerm::Bool { tpe, .. } => tpe,
            TypedTerm::Identifier { tpe, .. } => tpe,
            TypedTerm::Fun { tpe, .. } => tpe,
            TypedTerm::Apply { tpe, .. } => tpe,
            TypedTerm::If { tpe, .. } => tpe,
            TypedTerm::Let { tpe, .. } => tpe,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct Constraint(Type, Type);

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
            Type::Literal(TypeLiteral::Bool) => tpe,
            Type::Literal(TypeLiteral::Int) => tpe,
            Type::Fun {
                param_tpe,
                return_tpe,
            } => Type::Fun {
                param_tpe: Box::new(Substitution::substitute(*param_tpe, tvar, replacement)),
                return_tpe: Box::new(Substitution::substitute(*return_tpe, tvar, replacement)),
            },
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

    fn by_name(&self, name: &String) -> Option<Type> {
        self.env.get(name).cloned()
    }
}

/// infer the type of the given term given known function defined in the outer scopes.
/// This is a translation of the algorithm demonstrated by
/// Ionut Gan at I T.A.K.E Unconference 2015 (https://www.youtube.com/watch?v=oPVTNxiMcSU)
pub fn infer(term: Term, global: HashMap<String, Type>) -> Type {
    let mut env = Types::new();
    env.extends_with(global);

    let typed_term = annotate::annotate(term, &mut env);
    debug!("typed term: {:#?}", typed_term);

    let constraints = constraint::collect(&typed_term);
    debug!("Constraints: {:#?}", constraints);

    let substitution = unifier::unify(constraints);

    substitution.apply_type(typed_term.tpe())
}

// TODO Exports annotate/constraint/unifier into their own files
// TODO Add unit-tests for each modules
mod annotate {
    use super::{Term, TypeBinder, TypedTerm, Types};

    pub(super) fn annotate(term: Term, types: &mut Types) -> TypedTerm {
        match term {
            Term::Int(value) => TypedTerm::Int {
                tpe: types.fresh_var(),
                value,
            },
            Term::Bool(value) => TypedTerm::Bool {
                tpe: types.fresh_var(),
                value,
            },
            Term::Fun { param, body } => {
                let param = TypeBinder::new(param, types.fresh_var());
                types.add_binder(param.clone());

                let body = annotate(*body, types);

                TypedTerm::Fun {
                    tpe: types.fresh_var(),
                    param,
                    body: Box::new(body),
                }
            }
            Term::Identifier(name) => match types.by_name(&name) {
                None => panic!("unbound identifier: {}", name),
                Some(tpe) => TypedTerm::Identifier { tpe, name },
            },
            Term::Apply { fun, arg } => {
                let fun = Box::new(annotate(*fun, types));
                let arg = Box::new(annotate(*arg, types));

                TypedTerm::Apply {
                    tpe: types.fresh_var(),
                    fun,
                    arg,
                }
            }
            Term::If {
                cond,
                true_branch,
                false_branch,
            } => {
                let cond = Box::new(annotate(*cond, types));
                let true_branch = Box::new(annotate(*true_branch, types));
                let false_branch = Box::new(annotate(*false_branch, types));

                TypedTerm::If {
                    tpe: types.fresh_var(),
                    cond,
                    true_branch,
                    false_branch,
                }
            }
            Term::Let {
                binding,
                value,
                body,
            } => {
                let binding_tpe = types.fresh_var();
                let binding = TypeBinder::new(binding, binding_tpe);
                let value = Box::new(annotate(*value, types));

                // scoping: We need to add the binding before evaluating the body but after the value
                types.add_binder(binding.clone());
                let body = Box::new(annotate(*body, types));

                TypedTerm::Let {
                    tpe: types.fresh_var(),
                    binding,
                    value,
                    body,
                }
            }
        }
    }
}

mod constraint {
    use std::collections::HashSet;

    use super::{Constraint, Type, TypeLiteral, TypedTerm};

    pub(super) fn collect(term: &TypedTerm) -> HashSet<Constraint> {
        let mut constraints = HashSet::new();

        match term {
            TypedTerm::Bool { tpe, .. } => {
                constraints.insert(Constraint(tpe.clone(), Type::Literal(TypeLiteral::Bool)));
            }
            TypedTerm::Int { tpe, .. } => {
                constraints.insert(Constraint(tpe.clone(), Type::Literal(TypeLiteral::Int)));
            }
            TypedTerm::Fun { tpe, param, body } => {
                constraints.extend(collect(&body));

                let param_tpe = Box::new(param.tpe.clone());
                let return_tpe = Box::new(body.tpe().clone());
                constraints.insert(Constraint(
                    tpe.clone(),
                    Type::Fun {
                        param_tpe,
                        return_tpe,
                    },
                ));
            }
            TypedTerm::Identifier { .. } => (),
            TypedTerm::Apply { tpe, fun, arg } => {
                constraints.extend(collect(fun));
                constraints.extend(collect(arg));

                /*
                        collect(fn) ++ collect(arg) ++ Set(
                  Constraint(fn.ty, Type.FUN(arg.ty, ty))
                )
                        */

                let param_tpe = Box::new(arg.tpe().clone());
                let return_tpe = Box::new(tpe.clone());
                constraints.insert(Constraint(
                    fun.tpe().clone(),
                    Type::Fun {
                        param_tpe,
                        return_tpe,
                    },
                ));
            }
            TypedTerm::If {
                tpe,
                cond,
                true_branch,
                false_branch,
            } => {
                constraints.extend(collect(cond));
                constraints.extend(collect(true_branch));
                constraints.extend(collect(false_branch));

                // If put a constraint on the condition and the branches should resolve to the same type
                constraints.insert(Constraint(
                    cond.tpe().clone(),
                    Type::Literal(TypeLiteral::Bool),
                ));
                constraints.insert(Constraint(true_branch.tpe().clone(), tpe.clone()));
                constraints.insert(Constraint(false_branch.tpe().clone(), tpe.clone()));
            }
            TypedTerm::Let {
                tpe,
                binding,
                value,
                body,
            } => {
                constraints.extend(collect(value));
                constraints.extend(collect(body));

                // TODO take some time to really think about the 2 following constraints

                // The let expression has the body type.
                constraints.insert(Constraint(tpe.clone(), body.tpe().clone()));
                // The binding type is the one of the value.
                constraints.insert(Constraint(binding.tpe.clone(), value.tpe().clone()));
            }
        };

        constraints
    }
}

mod unifier {
    use log::debug;
    use std::collections::HashSet;

    use super::{Constraint, Substitution, Type, TypeLiteral, TypeVariable};

    pub(super) fn unify(constraints: HashSet<Constraint>) -> Substitution {
        debug!("unify: {:?}", constraints);
        let mut iter = constraints.iter();

        match iter.next() {
            None => Substitution::empty(),
            Some(first) => {
                // TODO Understand what the scala code is actually doing.
                // There might be a simpler way in Rust with mutability
                /*
                val subst: Substitution = unifyOne(constraints.head)
                val substitutedTail = subst.apply(constraints.tail)
                val substTail: Substitution = unify(substitutedTail)
                subst.compose(substTail)
                       */

                let sub_head = unify_one_constraint(first);

                // Apply this substitution to the remaining constraints
                let constraints_tail: HashSet<_> = iter.map(|c| sub_head.apply(c)).collect();

                // Then recursively unify the substituted constraints
                let sub_tail = unify(constraints_tail);

                // And finally merged the unified substitution with the first one
                sub_head.merge(sub_tail)
            }
        }
    }

    fn unify_one_constraint(Constraint(a, b): &Constraint) -> Substitution {
        debug!("unify_one_constraint: {:?} to {:?}", a, b);
        match (a, b) {
            (Type::Literal(TypeLiteral::Bool), Type::Literal(TypeLiteral::Bool)) => {
                Substitution::empty()
            }
            (Type::Literal(TypeLiteral::Int), Type::Literal(TypeLiteral::Int)) => {
                Substitution::empty()
            }
            (
                Type::Fun {
                    param_tpe: p1,
                    return_tpe: r1,
                },
                Type::Fun {
                    param_tpe: p2,
                    return_tpe: r2,
                },
            ) => {
                let mut constraints = HashSet::new();

                constraints.insert(Constraint(*p1.clone(), *p2.clone()));
                constraints.insert(Constraint(*r1.clone(), *r2.clone()));

                unify(constraints)
            }
            (Type::Variable(tvar), tpe) => unify_variable(tvar, tpe),
            (tpe, Type::Variable(tvar)) => unify_variable(tvar, tpe),
            (a, b) => panic!("Cannot unify {:?} with {:?}", a, b),
        }
    }

    fn unify_variable(tvar: &TypeVariable, tpe: &Type) -> Substitution {
        // TODO Use pattern guard instead of inlined if
        match tpe {
            Type::Variable(tvar2) => {
                if tvar == tvar2 {
                    Substitution::empty()
                } else {
                    Substitution::one(tvar.clone(), tpe.clone())
                }
            }
            _ => {
                if occurs(tvar, tpe) {
                    panic!("circular use: {:?} occurs in {:?}", tvar, tpe)
                } else {
                    Substitution::one(tvar.clone(), tpe.clone())
                }
            }
        }
    }

    fn occurs(tvar: &TypeVariable, tpe: &Type) -> bool {
        match tpe {
            Type::Fun {
                param_tpe,
                return_tpe,
            } => occurs(tvar, param_tpe) || occurs(tvar, return_tpe),
            Type::Variable(tvar2) => tvar == tvar2,
            _ => false,
        }
    }
}

