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
use super::{ModuleName, PackageName};
use crate::utils::collect_accumulate;
use log::trace;
use std::collections::HashMap;

mod environment;
use environment::{new_environment, EnvError, Environment, RootEnvironment, ValueType};

// Some elements which are common to both AST
use crate::compiler::name::{Name, QualName};
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
}

impl Module {
    pub fn to_interface(&self) -> super::Interface {
        let values = self
            .values
            .iter()
            .filter_map(|(name, value)| match value {
                Value::Value { .. } => None,
                Value::TypedValue { tpe, .. } => Some((name.clone(), tpe.clone())),
            })
            .collect();

        super::Interface {
            module_name: self.name.clone(),
            values,
            unions: self.types.clone(),
            infixes: self.infixes.clone(),
        }
    }
}

#[derive(Debug)]
pub enum Exports {
    Everything,
    /// Non qualified name to its export type
    Specifics(HashMap<Name, ExportType>),
}

#[derive(Debug)]
pub enum ExportType {
    Value,
    Infix,
    UnionPublic,
    UnionPrivate,
}

#[derive(Debug, Clone)]
pub struct Infix {
    associativity: Associativity,
    precedence: u8,
    function_name: Name,
}

#[derive(Debug, Clone)]
pub struct UnionType {
    variables: Vec<Name>,
    variants: Vec<TypeConstructor>,
}

// TODO Once we have most of the pipeline built, revisit the decision of
// having Vec<Type> + Type instead of Type::Arrow(Box<Type>, Box<Type>)
// as it's essentially what a constructor is, a function from parameter
// to the resulting type.
// Later me: well, that's only true at the type level. The value also need
// to tag what variant it represent.
// TODO Do we need QualName here ? Would Name be enough ? 
#[derive(Debug, Clone)]
pub struct TypeConstructor {
    /// Constructor name. eg. in `type A = B`, the name is `B`
    name: QualName,
    /// The types of the parameters
    type_parameters: Vec<Type>,
    /// The type's name once constructed
    tpe: QualName,
}

#[derive(Debug, Clone)]
pub enum Type {
    Variable(Name),
    Type(Name, Vec<Type>),
    // Record
    // Unit
    Arrow(Box<Type>, Box<Type>),
    /// Tuple in elm have size 2 or 3, so the third argument is optional.
    /// Should we keep the same restriction in zelkova ?
    Tuple(Box<Type>, Box<Type>, Option<Box<Type>>),
    // Alias
}

impl Type {
    fn from_parser_type(env: &dyn Environment, tpe: &parser::Type) -> Type {
        match tpe {
            parser::Type::Unqualified(name, vars) => {
                match env.find_type(&name) {
                    Some(t) => t.clone(),
                    None => {
                        let types = vars
                            .iter()
                            .map(|t| Type::from_parser_type(env, &t))
                            .collect();

                        // TODO Insert back into Environment ?
                        Type::Type(name.clone(), types)
                    }
                }
            }
            parser::Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(Type::from_parser_type(env, t1)),
                Box::new(Type::from_parser_type(env, t2)),
            ),
            parser::Type::Variable(n) => Type::Variable(n.clone()),
            parser::Type::Tuple(t1, t_many) => {
                let (t2, t3) = match t_many.len() {
                    0 => panic!("Tuple of length 1 is not suported by the parser"),
                    1 | 2 => (t_many.get(0).unwrap().clone(), t_many.get(1).clone()),
                    _ => panic!("For now we restrict tuple to sizes 2 and 3"),
                };

                Type::Tuple(
                    Box::new(Type::from_parser_type(env, t1)),
                    Box::new(Type::from_parser_type(env, &t2)),
                    t3.map(|t| Box::new(Type::from_parser_type(env, t))),
                )
            }
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

#[derive(Debug)]
pub enum Value {
    Value {
        name: Name,
        patterns: Vec<Pattern>,
        body: Expression,
    },
    TypedValue {
        name: Name,
        patterns: Vec<(Pattern, Type)>,
        body: Expression,
        tpe: Type,
    },
}

#[derive(Debug)]
pub enum Pattern {
    Anything,
    Variable(Name), // TODO Name or QualName ?
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    /// Tuple in elm have size 2 or 3, so the third argument is optional.
    /// Should we keep the same restriction in zelkova ?
    Tuple(Box<Pattern>, Box<Pattern>, Option<Box<Pattern>>),

    Constructor {
        ctor: TypeConstructor,
        args: Vec<Pattern>,
    },
}

impl Pattern {
    fn from_parser(p: &parser::Pattern, env: &dyn Environment) -> Pattern {
        match p {
            parser::Pattern::Anything => Pattern::Anything,
            parser::Pattern::Variable(name) => Pattern::Variable(name.clone()),
            parser::Pattern::Literal(parser::Literal::Int(i)) => Pattern::Int(*i),
            parser::Pattern::Literal(parser::Literal::Float(f)) => Pattern::Float(*f),
            parser::Pattern::Literal(parser::Literal::Char(c)) => Pattern::Char(*c),
            parser::Pattern::Literal(parser::Literal::Bool(b)) => Pattern::Bool(*b),
            parser::Pattern::Tuple(a, b, c) => Pattern::Tuple(
                Box::new(Pattern::from_parser(a, env)),
                Box::new(Pattern::from_parser(b, env)),
                c.get(0).map(|p| Pattern::from_parser(p, env)).map(Box::new),
            ),
            parser::Pattern::Constructor(name, args) => {
                // TODO Return Result instead
                let ctor = env
                    .find_type_constructor(&name)
                    .ok_or_else(|| Error::VariantNotFound(env.module_name().qualify_name(&name)))
                    .unwrap()
                    .clone();

                let args = args.iter().map(|p| Pattern::from_parser(p, env)).collect();

                Pattern::Constructor { ctor, args }
            }
        }
    }
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
#[derive(Debug)]
pub enum Expression {
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
    Tuple(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
}

impl Expression {
    fn from_parser(e: &parser::Expression, env: &dyn Environment) -> Result<Expression, Error> {
        match e {
            parser::Expression::Lit(parser::Literal::Int(i)) => Ok(Expression::Int(*i)),
            parser::Expression::Lit(parser::Literal::Float(f)) => Ok(Expression::Float(*f)),
            parser::Expression::Lit(parser::Literal::Char(c)) => Ok(Expression::Char(*c)),
            parser::Expression::Lit(parser::Literal::Bool(b)) => Ok(Expression::Bool(*b)),
            parser::Expression::Variable(name) => {
                match env
                    .find_value(&name)
                    .ok_or_else(|| Error::VariableNotFound(env.module_name().qualify_name(&name)))?
                {
                    ValueType::Local => Ok(Expression::VarLocal(name.clone())),
                    ValueType::TopLevel => Ok(Expression::VarTopLevel(
                        env.module_name().qualify_name(&name),
                    )),
                    ValueType::Foreign(m, tpe) => {
                        Ok(Expression::VarForeign(m.qualify_name(name), tpe.clone()))
                    }
                    ValueType::Foreigns(modules) => {
                        Err(Error::AmbiguousVariables(name.clone(), modules.clone()))
                    }
                }
            }
            parser::Expression::TypeConstructor(name) => {
                let ctor = env
                    .find_type_constructor(&name)
                    .ok_or_else(|| Error::VariantNotFound(env.module_name().qualify_name(&name)))?;

                let tpe = if ctor.type_parameters.is_empty() {
                    Type::Type(ctor.tpe.unqualified_name(), vec![])
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
                        Box::new(Type::Type(ctor.tpe.unqualified_name(), vec![])),
                        Box::new(tpe),
                    )
                };

                let name = name
                    .to_qual()
                    .unwrap_or_else(|| env.module_name().qualify_name(&name));

                Ok(Expression::VarConstructor(name, tpe))
            }
            parser::Expression::Application(a, b) => {
                let a = Expression::from_parser(a, env)?;
                let b = Expression::from_parser(b, env)?;

                Ok(Expression::Apply(Box::new(a), Box::new(b)))
            }
            parser::Expression::Tuple(vec) => match vec.as_slice() {
                [one, two] => {
                    let one = Expression::from_parser(one, env)?;
                    let two = Expression::from_parser(two, env)?;

                    Ok(Expression::Tuple(Box::new(one), Box::new(two), None))
                }
                [one, two, three] => {
                    let one = Expression::from_parser(one, env)?;
                    let two = Expression::from_parser(two, env)?;
                    let three = Expression::from_parser(three, env)?;

                    Ok(Expression::Tuple(
                        Box::new(one),
                        Box::new(two),
                        Some(Box::new(three)),
                    ))
                }
                _ => {
                    panic!(
                        "Tuple of size {} found. Should be forbidden at parsing.",
                        vec.len()
                    )
                }
            },
            parser::Expression::Case(expr, branches) => {
                let expr = Expression::from_parser(expr, env)?;

                let b = branches.iter().map::<Result<CaseBranch, Error>, _>(|cb| {
                    let pattern = Pattern::from_parser(&cb.pattern, env);
                    let scoped = env.new_scope();

                    // TODO From pattern result, insert required variables into scoped
                    let expression = Expression::from_parser(&cb.expression, &scoped)?;

                    Ok(CaseBranch {
                        pattern,
                        expression,
                    })
                });

                let branches = collect_accumulate(b)?;

                Ok(Expression::Case(Box::new(expr), branches))
            }
            parser::Expression::If(cond, then, els) => {
                let cond = Expression::from_parser(cond, env)?;
                let then = Expression::from_parser(then, env)?;
                let els = Expression::from_parser(els, env)?;

                Ok(Expression::If(
                    Box::new(cond),
                    Box::new(then),
                    Box::new(els),
                ))
            }
        }
    }
}

#[derive(Debug)]
pub struct CaseBranch {
    pattern: Pattern,
    expression: Expression,
}

// end AST

#[derive(Debug)]
pub enum Error {
    ExportNotFound(Name, ExportType),
    EnvironmentErrors(Vec<EnvError>),
    InfixReferenceInvalidValue(Name, Name), // (infix, function)
    BindingPatternsInvalidLen,
    NoBindings,
    VariableNotFound(QualName), // add name suggestion ?
    AmbiguousVariables(Name, Vec<ModuleName>),
    VariantNotFound(QualName),
    AmbiguousVariants(Name, Vec<ModuleName>),

    // Binding module
    InfixDeclared(Name),
    TypeDeclared(Name),
    NoTypeInBinding(Name),

    // Utility error
    Many(Vec<Error>),
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
        new_environment(&name, &interfaces, &source.imports).map_err(|e| vec![e.into()])?;

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
                .map(|i| Error::InfixDeclared(i.operator.clone()));
            errors.extend(e);
        }
        // Verify no types present
        if !source.types.is_empty() {
            let e = source
                .types
                .iter()
                .map(|t| Error::TypeDeclared(t.name.clone()));
            errors.extend(e);
        }

        // Iterate on values
        let iter = source.functions.iter().map(|function| {
            // Make sure there is no binding
            if !function.bindings.is_empty() {
                //println!("bindings = {:?} (js module)", function.bindings);
                Err(Error::BindingPatternsInvalidLen)? // TODO More specific error
            }

            // Make sure there is a type
            let tpe = function
                .tpe
                .as_ref()
                .ok_or_else(|| Error::NoTypeInBinding(function.name.clone()))?;
            let tpe = Type::from_parser_type(&env, tpe);

            let name = function.name.clone();
            // TODO Think how it's going to be represented. Currently canonical values assume an expression is present
            //      I'd like to not introduce a trait or new struct for binding. Should we fake an expression or create
            //      a new type of value ? New type of value will be annoying for regular modules as they aren't present
            //      there. Fake expression might be ok as MVP. We have to make sure that binding module are removed from
            //      some phase of the compilation pipeline.
            let value = Value::TypedValue {
                name: name.clone(),
                patterns: vec![],
                body: Expression::Bool(true),
                tpe,
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

        let types = do_types(&env, &source.types, &name).unwrap_or_else(|err| {
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
        })
    } else {
        Err(errors)
    }
}

fn do_values(
    env: &mut RootEnvironment,
    functions: &Vec<parser::Function>,
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
            Err(Error::BindingPatternsInvalidLen)?
        }

        let (patterns, body): (Vec<Pattern>, Expression) = match function.bindings.len() {
            0 => Err(Error::NoBindings),
            1 => {
                // if one binding, we can convert directly to canonical format
                let binding = &function.bindings[0];

                let mut scoped = env.new_scope();

                let patterns = binding
                    .patterns
                    .iter()
                    .map(|p| Pattern::from_parser(p, env))
                    .map(|p| {
                        scoped.expose_pattern(&p);

                        p
                    })
                    .collect();

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

                todo!("multiple bindings not implemented")
            }
        }?;

        let name = function.name.clone();

        match &function.tpe {
            Some(t) => {
                let tpe = Type::from_parser_type(env, &t);
                let linear = Type::to_linear_types(&tpe);

                // Linear is a list of types making the function. Because it includes the return type,
                // it will always be bigger than the number of patterns by one.
                if !patterns.is_empty() && (linear.len() - 1 != patterns.len()) {
                    // TODO Better error message
                    println!(
                        "linear = {:#?}\nbindings = {:#?} (linear.len ({}) != patterns.len ({}))",
                        linear,
                        function.bindings,
                        linear.len(),
                        patterns.len()
                    );
                    Err(Error::BindingPatternsInvalidLen)?
                }

                let patterns = patterns.into_iter().zip(linear).collect();

                Ok((
                    function.name.clone(),
                    Value::TypedValue {
                        name,
                        patterns,
                        body,
                        tpe,
                    },
                ))
            }
            None => Ok((
                function.name.clone(),
                Value::Value {
                    name,
                    patterns,
                    body,
                },
            )),
        }
    });

    collect_accumulate(iter)
}

fn do_types(
    env: &dyn Environment,
    types: &Vec<parser::UnionType>,
    module_name: &ModuleName,
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
            .filter_map(|t| {
                match t {
                    parser::Type::Unqualified(name, vars) => {
                        // TODO It might actually make more sense to put Type::from_parser_type
                        // on `Environment`.
                        Some(TypeConstructor {
                            name: module_name.qualify_name(&name),
                            type_parameters: vars
                                .iter()
                                .map(|t| Type::from_parser_type(env, t))
                                .collect(),
                            tpe: module_name.qualify_name(&tpe_name),
                        })
                    }
                    _ => None,
                }
            })
            .collect();

        Ok((
            tpe_name,
            UnionType {
                variables,
                variants,
            },
        ))
    });

    collect_accumulate(iter)
}

fn do_infixes(
    infixes: &Vec<parser::Infix>,
    env: &mut RootEnvironment,
    functions: &Vec<parser::Function>,
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
                associativity: infix.associativy,
                precedence: infix.precedence,
                function_name,
            };

            env.insert_local_infix(op_name.clone(), infix.clone());

            Ok((op_name, infix))
        } else {
            Err(Error::InfixReferenceInvalidValue(op_name, function_name))
        }
    });

    collect_accumulate(iter)
}

// TODO Add existence checks for values and types
fn do_exports(
    source_exposing: &parser::Exposing,
    env: &dyn Environment,
) -> Result<Exports, Vec<Error>> {
    match source_exposing {
        parser::Exposing::Open => Ok(Exports::Everything),
        parser::Exposing::Explicit(exposed) => {
            let specifics = exposed.into_iter().map(|exposed| match exposed {
                parser::Exposed::Lower(name) => Ok((name.clone(), ExportType::Value)),
                parser::Exposed::Upper(name, parser::Privacy::Public) => {
                    Ok((name.clone(), ExportType::UnionPublic))
                }
                parser::Exposed::Upper(name, parser::Privacy::Private) => {
                    Ok((name.clone(), ExportType::UnionPrivate))
                }
                parser::Exposed::Operator(name) => {
                    if env.local_infix_exists(&name) {
                        Ok((name.clone(), ExportType::Infix))
                    } else {
                        Err(Error::ExportNotFound(name.clone(), ExportType::Infix))
                    }
                }
            });

            let specifics = collect_accumulate(specifics)?;

            Ok(Exports::Specifics(specifics))
        }
    }
}
