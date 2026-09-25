//! Layer 4: what `check_module` hands a backend.
//!
//! These go through the whole pipeline — parse → canonicalize → type_check → the IR —
//! and assert on the facts a code generator needs and a type checker never did: how many
//! parameters a declaration takes, whether a call supplies every argument its callee
//! wants, what kind of name a reference is, and where a constructor sits in its
//! declaration.
//!
//! That the translation tells the four kinds of name apart is pinned one level down, by
//! `typer::tests::the_four_kinds_of_name_stay_apart`.

use std::collections::HashMap;

use indoc::indoc;
use zelkova_lang::compiler::dependencies::ModuleWalker;
use zelkova_lang::compiler::ir::{
    self, decision_tree, Binding, CaseForm, Constructor, Decision, Declaration, LiteralValue,
    Occurrence, Outcome, Reference, ReferenceKind, Saturation, Step, TypedTerm, TypedTermKind,
};
use zelkova_lang::compiler::name::{Name, QualName};
use zelkova_lang::compiler::source::{load_package_sources, SourceRoot};
use zelkova_lang::compiler::typer::{Type, TypeLiteral};
use zelkova_lang::compiler::{check_module, parser, Interface, PackageName};

mod support;

use support::*;

/// The IR `check_module` produces for `source`, insisting that it checked.
fn ir_of(source: &str) -> ir::Module {
    let parsed = parse_source(source);
    let interfaces = HashMap::from([basics_interface(), char_interface(), maybe_interface()]);

    check_module(&test_package(), &interfaces, &parsed, false)
        .unwrap_or_else(|error| panic!("expected the module to check, got {:?}", error))
        .ir
}

/// The one declaration named `name`, or a panic naming what is in the module instead.
fn declaration<'a>(module: &'a ir::Module, name: &str) -> &'a Declaration {
    module
        .declarations
        .iter()
        .find(|declaration| declaration.name == Name::new(name))
        .unwrap_or_else(|| {
            panic!(
                "`{}` should have an IR declaration; the module has {:?} and could not check {:?}",
                name,
                module
                    .declarations
                    .iter()
                    .map(|d| d.name.as_str())
                    .collect::<Vec<_>>(),
                module
                    .unchecked
                    .iter()
                    .map(|u| u.name.as_str())
                    .collect::<Vec<_>>(),
            )
        })
}

/// The expression a declaration's parameters are in scope over.
fn body<'a>(declaration: &'a Declaration, name: &str) -> &'a TypedTerm {
    &declaration
        .body
        .as_ref()
        .unwrap_or_else(|| panic!("`{}` should have a body", name))
        .expression
}

/// A declaration's body, insisting that it is a `case … of` expression, and its
/// branches' bodies in source order beside the decision tree `GEN-5`'s
/// `decision::build` lowers them to — handed `name` as the declaration a `Fail` leaf
/// carries, the way a backend walking that declaration would hand it.
fn case_tree<'a>(declaration: &'a Declaration, name: &str) -> (Decision<'a>, Vec<&'a TypedTerm>) {
    match &body(declaration, name).kind {
        TypedTermKind::Case {
            scrutinee,
            branches,
            ..
        } => (
            decision_tree(&scrutinee.tpe, branches, &Name::new(name)),
            branches.iter().map(|(_, body)| &**body).collect(),
        ),
        other => panic!("expected `{}`'s body to be a `case`, got {:?}", name, other),
    }
}

/// What is being applied, at the head of an application spine.
fn head_of(term: &TypedTerm) -> &TypedTerm {
    let mut head = term;
    while let TypedTermKind::Apply { fun, .. } = &head.kind {
        head = fun;
    }
    head
}

/// The reference a term is, or a panic.
fn reference(term: &TypedTerm) -> &Reference {
    match &term.kind {
        TypedTermKind::Identifier(reference) => reference,
        other => panic!("expected a name, got {:?}", other),
    }
}

/// A declaration's arity is the number of parameters it was written with, carried as a
/// fact rather than counted back off a spine of nested functions.
///
/// The translation nests one `Fun` per parameter, so a two-parameter declaration is a
/// function returning a function, and a backend emitting [a plain n-ary
/// function](../docs/decisions/dec-18.md) has to know how many of those nodes are its
/// parameter list.
///
/// Mutation-checked by making `canonical::Value::arity` answer `0`, which is what
/// counting nothing looks like: both assertions go red, since `second` then has no
/// parameters and its whole nested-function body is left as the expression to emit.
#[test]
fn a_declarations_arity_is_the_number_of_parameters_it_was_written_with() {
    let module = ir_of(indoc! {r#"
        module Test exposing (first, second)

        second : Int -> Int -> Int
        second a b =
          b

        first : Int -> Int
        first a =
          a
    "#});

    let second = declaration(&module, "second");
    assert_eq!(second.arity, 2);
    assert_eq!(
        second
            .body
            .as_ref()
            .map(|body| body.parameters.len())
            .unwrap_or_default(),
        2,
        "the parameters are the ones the arity counts"
    );

    assert_eq!(declaration(&module, "first").arity, 1);
}

/// A parameter written as a pattern is still a parameter: the declaration's arity counts
/// it, it is bound under the name `ir::pattern_parameter` gives its position, and the
/// body is a single-branch match on that name, marked as a parameter's so that a backend
/// and a diagnostic can tell it from a `case` the source wrote.
///
/// Mutation-checked twice: nesting each parameter's match directly inside its own `Fun`,
/// rather than inside all of them, leaves `pick` with one parameter and the arity
/// assertion red; building the match with `CaseForm::Expression` turns the form
/// assertion red.
#[test]
fn a_parameter_written_as_a_pattern_is_a_parameter_and_a_match() {
    let module = ir_of(indoc! {r#"
        module Test exposing (pick)

        pick : (Int, Char) -> Int -> Int
        pick (a, c) n =
          a
    "#});

    let pick = declaration(&module, "pick");
    assert_eq!(pick.arity, 2);
    let parameters: Vec<&str> = pick
        .body
        .as_ref()
        .map(|body| body.parameters.iter().map(|p| p.name.as_str()).collect())
        .unwrap_or_default();
    assert_eq!(parameters, vec!["$0", "n"]);

    match &body(pick, "pick").kind {
        TypedTermKind::Case {
            scrutinee,
            branches,
            form,
        } => {
            assert_eq!(*form, CaseForm::Parameter);
            assert_eq!(branches.len(), 1);
            assert_eq!(*reference(scrutinee), Reference::local("$0"));
        }
        other => panic!(
            "expected a match on the patterned parameter, got {:?}",
            other
        ),
    }
}

/// A call that supplies every argument its callee takes is marked saturated, and one
/// that does not is not.
///
/// That is the difference between emitting a direct call and going through the runtime's
/// `$curry` helper ([`DEC-18` decision 3](../docs/decisions/dec-18.md)), and an `Apply`
/// node supplies one argument at a time, so nothing in the shape of the tree says which
/// a given application is.
///
/// `half` is the shape that decides the difference on its own: the *outermost* node of
/// its body stays `Partial`, so it is the declaration a backend has to reach for `$curry`
/// on rather than emit a direct call for. `both` and `one` both saturate at their
/// outermost node, and pin `Partial` only on an inner one.
///
/// Mutation-checked by returning `Saturation::Partial` unconditionally from the
/// application arm of `canonical_expr_to_term` — the `both` assertion goes red — and,
/// separately, by returning `Saturation::Saturated` unconditionally, which turns `one`
/// and `half` red (each on its own, with the other neutralised). Either mutation alone
/// leaves the other direction green, which is why both are asserted.
///
/// The `half.arity` assertion is documentation rather than a second check: `arity` is the
/// parameter count `peel` actually took, and a body that is not a `Fun` yields none
/// whatever the rule says, so no mutation of the arity rule moves it. It is here because
/// this is the declaration where arity (0 patterns) and the type's arrow count (1)
/// disagree, which is the rule [`Declaration::arity`]'s doc comment states.
#[test]
fn an_application_says_whether_it_supplies_every_argument() {
    let module = ir_of(indoc! {r#"
        module Test exposing (both, one, half)

        pick : Int -> Int -> Int
        pick a b =
          a

        both : Int
        both =
          pick 1 2

        one : Int -> Int
        one b =
          pick 1 b

        half : Int -> Int
        half =
          pick 1
    "#});

    let both = declaration(&module, "both");
    match &body(both, "both").kind {
        TypedTermKind::Apply { saturation, .. } => {
            assert_eq!(*saturation, Saturation::Saturated)
        }
        other => panic!("expected an application, got {:?}", other),
    }

    // The same spine one argument short: `pick 1` supplies one of the two `pick` takes.
    let one = declaration(&module, "one");
    match &body(one, "one").kind {
        TypedTermKind::Apply {
            fun, saturation, ..
        } => {
            assert_eq!(
                *saturation,
                Saturation::Saturated,
                "`pick 1 b` does supply both"
            );

            match &fun.kind {
                TypedTermKind::Apply { saturation, .. } => assert_eq!(
                    *saturation,
                    Saturation::Partial,
                    "`pick 1` on its own supplies one of two"
                ),
                other => panic!("expected the inner application, got {:?}", other),
            }
        }
        other => panic!("expected an application, got {:?}", other),
    }

    // A spine whose outermost node is itself short: `half` returns the function `pick 1`
    // is, and supplies one of the two arguments `pick` takes.
    let half = declaration(&module, "half");
    assert_eq!(
        half.arity, 0,
        "`half` was written with no parameters, whatever its type's arrows say"
    );
    match &body(half, "half").kind {
        TypedTermKind::Apply { saturation, .. } => assert_eq!(
            *saturation,
            Saturation::Partial,
            "`pick 1` is the whole body, and supplies one of two"
        ),
        other => panic!("expected an application, got {:?}", other),
    }
}

/// A parameter, a declaration of this module and a constructor are three different
/// references, and a backend can tell which is which.
///
/// All three used to be one `TermKind::Identifier(String)`. See this file's header for
/// the fourth kind and why it is pinned elsewhere.
///
/// Mutation-checked by building every arm of `canonical_expr_to_term`'s name arms as
/// `ReferenceKind::Local`: each of the three assertions below then names the same thing.
#[test]
fn a_local_a_top_level_and_a_constructor_are_three_references() {
    let module = ir_of(indoc! {r#"
        module Test exposing (Size, echo, chain, build)

        type Size
          = Small
          | Large

        echo : Int -> Int
        echo a =
          a

        chain : Int -> Int
        chain a =
          echo a

        build : Size
        build =
          Large
    "#});

    let chain = declaration(&module, "chain");
    match &body(chain, "chain").kind {
        TypedTermKind::Apply { fun, arg, .. } => {
            assert_eq!(
                reference(fun).kind,
                ReferenceKind::TopLevel(qual("Test.echo"))
            );
            assert_eq!(reference(arg).kind, ReferenceKind::Local);
        }
        other => panic!("expected an application, got {:?}", other),
    }

    let build = declaration(&module, "build");
    match &reference(body(build, "build")).kind {
        ReferenceKind::Constructor(ctor) => assert_eq!(ctor.name, Name::new("Large")),
        other => panic!("expected a constructor, got {:?}", other),
    }
}

/// A constructor node carries how many arguments it takes and where it sits in its
/// declaration.
///
/// The name alone is what a companion reads out of the `$` field, and it is not enough
/// for the other target: a union is [a WIT `variant`](../docs/spec/interop.md) whose
/// cases are reached by position.
///
/// Mutation-checked by fixing `index` at `0` in `constructors_of`, which turns the
/// `Rgb` assertion red, and by fixing `arity` at `0`, which turns the argument-count
/// assertion red.
#[test]
fn a_constructor_carries_its_argument_count_and_its_index() {
    let module = ir_of(indoc! {r#"
        module Test exposing (Colour, paint, plain)

        type Colour
          = Named Int
          | Rgb Int Int Int

        paint : Colour
        paint =
          Rgb 1 2 3

        plain : Int -> Colour
        plain a =
          Named a
    "#});

    // `Rgb 1 2 3` is three applications deep; the constructor is at the head of them.
    let paint = declaration(&module, "paint");

    match &reference(head_of(body(paint, "paint"))).kind {
        ReferenceKind::Constructor(ctor) => {
            assert_eq!(ctor.name, Name::new("Rgb"));
            assert_eq!(ctor.index, 1, "`Rgb` is the second case of `Colour`");
            assert_eq!(ctor.arity, 3);
        }
        other => panic!("expected a constructor, got {:?}", other),
    }

    match &reference(head_of(body(declaration(&module, "plain"), "plain"))).kind {
        ReferenceKind::Constructor(ctor) => {
            assert_eq!(ctor.index, 0, "`Named` is the first case of `Colour`");
            assert_eq!(ctor.arity, 1);
        }
        other => panic!("expected a constructor, got {:?}", other),
    }

    // And the declaration itself is in the module's unions, with the same numbering.
    let colour = module
        .unions
        .iter()
        .find(|union| union.name == qual("Test.Colour"))
        .expect("`Test` declares `Colour`");

    assert_eq!(
        colour
            .variants
            .iter()
            .map(|variant| (
                variant.name.as_str().to_string(),
                variant.index,
                variant.arity
            ))
            .collect::<Vec<_>>(),
        vec![("Named".to_string(), 0, 1), ("Rgb".to_string(), 1, 3),]
    );
}

/// A facade declares signatures and no bodies, and its arity is the parameter list its
/// companion exports.
///
/// A facade's declarations have no patterns to count — there is nothing but the
/// signature — so an arity read off the parameters would make every one of them a
/// constant. [The JavaScript companion](../docs/spec/interop.md#the-javascript-companion)
/// takes a plain parameter list of the length the signature's arrows say, and [a facade
/// constant](../docs/spec/interop.md#facade-constants) is the zero-arrow case.
///
/// Mutation-checked by making `facade_signature` answer `0` instead of the signature's
/// arrow count, which turns the `combine` assertion red.
#[test]
fn a_facade_declaration_has_a_signature_and_no_body() {
    let module = ir_of(indoc! {r#"
        module foreign Test exposing (combine, constant)

        unsafe combine : Int -> Int -> Int

        unsafe constant : Int
    "#});

    assert!(module.foreign);

    let combine = declaration(&module, "combine");
    assert_eq!(combine.arity, 2);
    assert!(
        combine.body.is_none(),
        "a facade signature has no body to emit"
    );

    assert_eq!(declaration(&module, "constant").arity, 0);
    assert!(
        module.initialisation_order.is_empty(),
        "a facade constant is evaluated on whatever schedule the target gives it, not this \
         one, so a facade module has nothing to schedule here: got {:?}",
        module.initialisation_order
    );
}

/// `GEN-7`: a top-level binding that names no parameters is initialised only after every
/// parameterless binding its own body mentions, so `base` — the chapter's own example —
/// comes before `shifted` whichever order the two declarations are written in
/// (`docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once`).
/// `other` takes a parameter and never enters the order at all.
///
/// Mutation-checked by replacing `canonical::initialisation_order`'s topological sort with
/// the declarations in their `HashMap` order (`values.keys().cloned().collect()`): one of
/// the two source orderings below puts `shifted` before `base`, so its half of the loop
/// goes red.
#[test]
fn a_parameterless_binding_is_initialised_after_what_it_mentions() {
    let written_base_first = indoc! {r#"
        module Test exposing ()

        type Colour
          = Red
          | Green

        base =
          Red

        shifted =
          other base

        other c =
          case c of
            Red ->
              Green

            Green ->
              Red
    "#};

    let written_shifted_first = indoc! {r#"
        module Test exposing ()

        type Colour
          = Red
          | Green

        shifted =
          other base

        base =
          Red

        other c =
          case c of
            Red ->
              Green

            Green ->
              Red
    "#};

    for source in [written_base_first, written_shifted_first] {
        let module = ir_of(source);
        let order: Vec<String> = module
            .initialisation_order
            .iter()
            .map(|name| name.as_str().to_string())
            .collect();

        let base_pos = order.iter().position(|n| n == "base").unwrap_or_else(|| {
            panic!(
                "`base` should be in the initialisation order, got {:?}",
                order
            )
        });
        let shifted_pos = order
            .iter()
            .position(|n| n == "shifted")
            .unwrap_or_else(|| {
                panic!(
                    "`shifted` should be in the initialisation order, got {:?}",
                    order
                )
            });

        assert!(
            base_pos < shifted_pos,
            "`base` should be initialised before `shifted`, got {:?}",
            order
        );
        assert!(
            !order.contains(&"other".to_string()),
            "`other` takes a parameter and is never scheduled, got {:?}",
            order
        );
    }
}

/// `GEN-7`: a module whose declarations all take parameters has nothing to schedule — the
/// order only ever holds parameterless bindings, and this module declares none.
///
/// Mutation-checked by dropping the `is_parameterless` filter `canonical`'s shared
/// dependency graph builds its nodes from: `first` and `second` would then both appear.
#[test]
fn a_module_of_only_functions_has_an_empty_initialisation_order() {
    let module = ir_of(indoc! {r#"
        module Test exposing (first, second)

        first : Int -> Int
        first a =
          a

        second : Int -> Int -> Int
        second a b =
          first a
    "#});

    assert!(
        module.initialisation_order.is_empty(),
        "a module of only functions has nothing to initialise, got {:?}",
        module.initialisation_order
    );
}

/// `GEN-7`: a reference to a binding with parameters is not an edge — that value already
/// exists as a function, so it is never a node in the graph being sorted and never
/// constrains when the parameterless binding that calls it may run. `usesHelper` mentions
/// only `helper`, a function, so it has no dependency at all and is the whole order.
///
/// Mutation-checked by dropping the `is_parameterless` filter `canonical`'s shared
/// dependency graph builds its nodes from: `helper` would then be a node too, and the
/// exact-list assertion below would fail.
#[test]
fn a_reference_to_a_function_is_not_an_edge_in_the_initialisation_order() {
    let module = ir_of(indoc! {r#"
        module Test exposing (usesHelper, helper)

        helper : Int -> Int
        helper a =
          a

        usesHelper : Int
        usesHelper =
          helper 1
    "#});

    let order: Vec<String> = module
        .initialisation_order
        .iter()
        .map(|name| name.as_str().to_string())
        .collect();

    assert_eq!(
        order,
        vec!["usesHelper".to_string()],
        "`helper` takes a parameter and is never scheduled, so it may be initialised first \
         (there being nothing else to initialise)"
    );
}

/// `GEN-7`: parameterless bindings with no edge between them at all — `a` through `e`
/// below each reference nothing but a literal — still come back in the same order on
/// every run, not whatever order `values`' backing `HashMap` happened to iterate them in.
/// `petgraph::algo::toposort` only orders an edge's source before its target, so among
/// five bindings unconstrained by any edge, the order falls out of `dependency_graph`'s
/// node-insertion order; declaring them out of alphabetical order here (`c`, `e`, `a`,
/// `d`, `b`) checks that the result tracks name order rather than source order.
///
/// With no edges, `initialisation_order`'s two reversals — `toposort`'s own DFS-finish
/// reversal, then this function's edge-direction reversal — cancel out, so a name-sorted
/// insertion order comes back as plain name-sorted output: `a` .. `e`.
///
/// Mutation-checked by reverting `dependency_graph`'s node insertion to raw `HashMap`
/// order (dropping the sort added for this fix): the exact-list assertion below pins one
/// specific order out of the 5! = 120 raw `HashMap` orders reachable across process runs,
/// so — unlike an assertion that only compares two runs to each other, which would pass
/// on a nondeterministic build whenever a single run happens to iterate consistently with
/// itself — it fails on all but the roughly one in 120 unlucky runs where raw order
/// already happens to be alphabetical. Five bindings, not three, is deliberate: with only
/// three (1-in-6) a false pass from a nondeterministic build shows up often enough in
/// practice to make a single run of this check unconvincing.
#[test]
fn independent_parameterless_bindings_come_back_in_name_sorted_order() {
    let module = ir_of(indoc! {r#"
        module Test exposing (a, b, c, d, e)

        c : Int
        c =
          3

        e : Int
        e =
          5

        a : Int
        a =
          1

        d : Int
        d =
          4

        b : Int
        b =
          2
    "#});

    let order: Vec<String> = module
        .initialisation_order
        .iter()
        .map(|name| name.as_str().to_string())
        .collect();

    assert_eq!(
        order,
        vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "d".to_string(),
            "e".to_string(),
        ],
        "independent bindings have no edge between them, so the only correct order is a \
         fixed, name-sorted one — not whatever `HashMap` iteration happened to visit them \
         in: got {:?}",
        order
    );
}

/// Every value of the canonical module reaches the IR, as a declaration or as one it
/// could not build.
///
/// A backend handed only the declarations that worked cannot tell a module it may emit
/// whole from one that quietly lost a declaration, which is the mistake `DEC-18`'s first
/// decision is about. `helper` below matches a tuple pattern nested inside another
/// tuple pattern, which the typer does not translate, so it is exactly such a
/// declaration.
///
/// Mutation-checked by dropping the `unchecked.push` in `ir::build`'s catch-all arm:
/// `helper` then goes missing from both lists and the count assertion goes red.
#[test]
fn a_declaration_with_no_ir_is_named_rather_than_dropped() {
    let module = ir_of(indoc! {r#"
        module Test exposing (answer)

        answer : Int
        answer =
          1

        helper : ((Int, Int), Int) -> Int
        helper pair =
          case pair of
            ((a, b), c) ->
              a
    "#});

    assert_eq!(
        module
            .unchecked
            .iter()
            .map(|entry| entry.name.as_str().to_string())
            .collect::<Vec<_>>(),
        vec!["helper".to_string()],
    );
    assert_eq!(
        module
            .declarations
            .iter()
            .map(|entry| entry.name.as_str().to_string())
            .collect::<Vec<_>>(),
        vec!["answer".to_string()],
    );
}

/// Every module of the standard library comes back with an IR.
///
/// This is `cargo run`'s eight modules, checked the way the compiler checks them —
/// dependency order, each against the interfaces of the ones before it — and it is the
/// only test here that runs over real source rather than a module written for it. What
/// it establishes is coverage: the shape above is not one that only holds for four-line
/// examples, and a facade is in the list beside four ordinary modules.
///
/// Every declaration of every module has one, too: nothing in `std/core` is beyond the
/// typer, so a module's `unchecked` list being non-empty is a regression. The accounting
/// assertion is kept beside that one, since it is what says nothing was lost on the way
/// should a declaration ever land in `unchecked` again;
/// `a_declaration_with_no_ir_is_named_rather_than_dropped` is what exercises it.
///
/// Mutation-checked twice: restoring `wrap_with_patterns`'s `_ => None` for a parameter
/// written as a pattern leaves `Basics` and `Tuple` with unchecked declarations and the
/// emptiness assertion red, and routing `Solved::NoBody` to `unchecked` instead of to a
/// signature turns it and the `Js.Basics` assertions red.
#[test]
fn every_module_of_the_standard_library_gets_an_ir() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let root = std::path::Path::new(&manifest).join("std/core");

    let sources = load_package_sources(&root, SourceRoot::Src)
        .unwrap_or_else(|e| panic!("failed to load {:?}: {:?}", root, e));
    let modules: Vec<parser::Module> = sources
        .iter()
        .map(|(_, file)| {
            parser::parse(file.file())
                .unwrap_or_else(|e| panic!("parse error in {:?}: {:?}", file.file().name(), e))
        })
        .collect();
    assert_eq!(modules.len(), 8, "std/core holds eight modules");

    let module_files = HashMap::new();
    let walker = ModuleWalker::new(&modules, &module_files).expect("no cycle in std/core");
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let package = PackageName::new("zelkova-core").unwrap();
    let (checked, errors): (Vec<_>, Vec<_>) =
        walker.check_in_order(&package, &mut interfaces, &module_files, check_module);

    assert!(errors.is_empty(), "std/core must check: {:?}", errors);
    assert_eq!(checked.len(), 8);

    for module in &checked {
        assert_eq!(
            module.ir.declarations.len() + module.ir.unchecked.len(),
            module.canonical.values.len(),
            "`{}`: every value of a checked module has to be in one list or the other",
            module.ir.name.name(),
        );
        assert!(
            module.ir.unchecked.is_empty(),
            "`{}`: every declaration of std/core should be typed, but {:?} were not",
            module.ir.name.name(),
            module.ir.unchecked,
        );
    }

    let module_named = |name: &str| {
        checked
            .iter()
            .find(|module| module.ir.name.name() == &Name::new(name))
            .unwrap_or_else(|| panic!("std/core holds `{}`", name))
    };

    // A facade: thirty signatures, no bodies, nothing it could not represent.
    let js_basics = module_named("Js.Basics");
    assert!(js_basics.ir.foreign);
    assert!(js_basics.ir.unchecked.is_empty());
    assert!(!js_basics.ir.declarations.is_empty());
    assert!(
        js_basics
            .ir
            .declarations
            .iter()
            .all(|declaration| declaration.body.is_none()),
        "a facade declares signatures and no bodies"
    );

    // And an ordinary module, whose declarations do carry one.
    let maybe = module_named("Maybe");
    assert!(
        maybe
            .ir
            .declarations
            .iter()
            .any(|declaration| declaration.body.is_some()),
        "`Maybe` should have declarations to emit"
    );
}

// ── GEN-5: a `case` becomes a decision tree ─────────────────────────────────────

fn int() -> Type {
    Type::Literal(TypeLiteral::Int)
}

fn leaf<'a>(bindings: Vec<Binding>, body: &'a TypedTerm) -> Decision<'a> {
    Decision::Leaf { bindings, body }
}

fn binding(name: &str, occurrence: Occurrence, tpe: Type) -> Binding {
    Binding {
        name: name.to_string(),
        occurrence,
        tpe,
    }
}

fn test_root<'a>(outcome: Outcome, matched: Decision<'a>, default: Decision<'a>) -> Decision<'a> {
    Decision::Test {
        scrutinee: Occurrence::Root,
        outcome,
        matched: Box::new(matched),
        default: Box::new(default),
    }
}

fn fail<'a>(declaration: &str) -> Decision<'a> {
    Decision::Fail {
        declaration: Name::new(declaration),
    }
}

/// Constructor `name` of the union `union` this test module declares.
fn test_constructor(union: &str, name: &str, index: usize, arity: usize) -> Outcome {
    Outcome::Constructor(Constructor {
        union: QualName::in_module("Test", union),
        name: Name::new(name),
        index,
        arity,
    })
}

/// A wildcard branch matches unconditionally, so the tree it lowers to is one leaf —
/// no `Test`, since there is nothing to test — and that leaf binds nothing.
///
/// Mutation-checked by having `decision::lower`'s `Anything` arm bind the value under
/// `_` the way its `Bind` arm binds a name: the leaf's bindings come out non-empty and
/// the assertion goes red.
#[test]
fn a_wildcard_branch_is_a_leaf_with_no_bindings() {
    let module = ir_of(indoc! {r#"
        module Test exposing (always_one)

        always_one : Int -> Int
        always_one n =
          case n of
            _ ->
              1
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "always_one"), "always_one");

    assert!(matches!(bodies[0].kind, TypedTermKind::Int(1)));
    assert_eq!(tree, leaf(vec![], bodies[0]));
}

/// A variable branch also matches unconditionally, and binds the whole scrutinee under
/// its own name — at `Occurrence::Root`, since a variable pattern reads the value
/// `case` was given rather than any part of it, and at the scrutinee's type.
///
/// Mutation-checked twice: having `decision::lower`'s `Bind` arm build its binding at
/// `occurrence.field(Step::TupleElement(0))` instead of `occurrence`, and having
/// `decision::build` hand the root pattern a `Type::Number` instead of the scrutinee's
/// type. Each turns the assertion red.
#[test]
fn a_variable_branch_is_a_leaf_that_binds_the_whole_scrutinee() {
    let module = ir_of(indoc! {r#"
        module Test exposing (identity)

        identity : Int -> Int
        identity n =
          case n of
            x ->
              x
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "identity"), "identity");

    assert_eq!(
        tree,
        leaf(vec![binding("x", Occurrence::Root, int())], bodies[0])
    );
}

/// An `Int` pattern is refutable, so it becomes a `Test` of the scrutinee itself
/// against the value it names, with everything after it in `default` — which is where
/// the next branch's own `Test` lives, keeping the branches' source order as nested
/// `default`s rather than one table of edges. The wildcard ends the chain.
///
/// Mutation-checked by having `decision::lower`'s `Literal` arm test
/// `Outcome::Literal(LiteralValue::Int(0))` regardless of the pattern's own value: the
/// assertion goes red.
#[test]
fn an_int_pattern_becomes_a_test_on_its_value() {
    let module = ir_of(indoc! {r#"
        module Test exposing (label)

        label : Int -> Int
        label n =
          case n of
            1 ->
              10

            2 ->
              20

            _ ->
              0
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "label"), "label");

    assert!(matches!(bodies[0].kind, TypedTermKind::Int(10)));
    assert!(matches!(bodies[1].kind, TypedTermKind::Int(20)));
    assert!(matches!(bodies[2].kind, TypedTermKind::Int(0)));
    assert_eq!(
        tree,
        test_root(
            Outcome::Literal(LiteralValue::Int(1)),
            leaf(vec![], bodies[0]),
            test_root(
                Outcome::Literal(LiteralValue::Int(2)),
                leaf(vec![], bodies[1]),
                leaf(vec![], bodies[2]),
            ),
        )
    );
}

/// A `Char` pattern is a `Test` the same way an `Int` one is, tested by value rather
/// than by its type alone — both `'a'` and `'b'` share the type `Char` — and the
/// wildcard written after it is its `default`, not the other way round.
///
/// Mutation-checked by having `translate_pattern` record every `Char` pattern's value
/// as `'z'`, as if it kept only the pattern's type: the assertion goes red.
#[test]
fn a_char_pattern_becomes_a_test_on_its_value() {
    let module = ir_of(indoc! {r#"
        module Test exposing (code)

        code : Char -> Int
        code c =
          case c of
            'a' ->
              1

            _ ->
              0
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "code"), "code");

    assert!(matches!(bodies[0].kind, TypedTermKind::Int(1)));
    assert!(matches!(bodies[1].kind, TypedTermKind::Int(0)));
    assert_eq!(
        tree,
        test_root(
            Outcome::Literal(LiteralValue::Char('a')),
            leaf(vec![], bodies[0]),
            leaf(vec![], bodies[1]),
        )
    );
}

/// A `Bool` pattern — `true` or `false` — is a literal like `Int` and `Char` are, not a
/// constructor. Two literal branches cover `Bool`, but coverage is not checked
/// (`LANG-19`), so the `false` branch's `Test` still has a `default`: the `Fail` leaf.
///
/// Mutation-checked by having `translate_pattern`'s `Bool` arm read `true` regardless
/// of the pattern it was given: the assertion goes red.
#[test]
fn a_bool_pattern_becomes_a_test_on_its_value() {
    let module = ir_of(indoc! {r#"
        module Test exposing (choose)

        choose : Bool -> Int
        choose flag =
          case flag of
            true ->
              1

            false ->
              0
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "choose"), "choose");

    assert!(matches!(bodies[0].kind, TypedTermKind::Int(1)));
    assert!(matches!(bodies[1].kind, TypedTermKind::Int(0)));
    assert_eq!(
        tree,
        test_root(
            Outcome::Literal(LiteralValue::Bool(true)),
            leaf(vec![], bodies[0]),
            test_root(
                Outcome::Literal(LiteralValue::Bool(false)),
                leaf(vec![], bodies[1]),
                fail("choose"),
            ),
        )
    );
}

/// A tuple pattern tests nothing — a value of a tuple type is always a tuple, and every
/// element here is a name or `_` — so it lowers straight to a leaf, with one binding per
/// named element, each at the occurrence its position in the tuple gives it and at that
/// element's type. The wildcard element contributes no binding, and does not shift the
/// position of the one after it.
///
/// Mutation-checked by having `decision::lower`'s `Tuple` arm give every element the
/// occurrence `Step::TupleElement(0)`: `b`'s occurrence comes out as element `0` rather
/// than `1`, and the assertion goes red.
#[test]
fn a_tuple_pattern_is_a_leaf_that_binds_each_named_element_by_position() {
    let module = ir_of(indoc! {r#"
        module Test exposing (second)

        second : (Int, Int) -> Int
        second pair =
          case pair of
            (_, b) ->
              b
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "second"), "second");

    assert_eq!(
        tree,
        leaf(
            vec![binding(
                "b",
                Occurrence::Root.field(Step::TupleElement(1)),
                int()
            )],
            bodies[0],
        )
    );
}

/// A constructor pattern is refutable — it becomes a `Test` of the union's case — and,
/// unlike a literal, may bind names too: one per argument the pattern names, each at
/// the occurrence its position in the constructor gives it. A nullary constructor
/// (`Nothing`) is the same `Test`, whose leaf binds nothing. The two branches cover
/// `Box`, but coverage is not checked (`LANG-19`), so the last `default` is `Fail`.
///
/// Mutation-checked by having `translate_pattern` record every constructor's `index` as
/// `0`: `Nothing`'s outcome then names the wrong case of `Box`, and the assertion goes
/// red. A position mix-up in a binding is what
/// `two_branches_on_the_same_constructor_keep_source_order` below catches instead, since
/// `Just`'s one argument already sits at `0`.
#[test]
fn a_constructor_pattern_binds_its_arguments_by_position() {
    let module = ir_of(indoc! {r#"
        module Test exposing (Box, withDefault)

        type Box
          = Just Int
          | Nothing

        withDefault : Int -> Box -> Int
        withDefault default box =
          case box of
            Just n ->
              n

            Nothing ->
              default
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "withDefault"), "withDefault");

    assert_eq!(
        tree,
        test_root(
            test_constructor("Box", "Just", 0, 1),
            leaf(
                vec![binding(
                    "n",
                    Occurrence::Root.field(Step::ConstructorArgument(0)),
                    int()
                )],
                bodies[0],
            ),
            test_root(
                test_constructor("Box", "Nothing", 1, 0),
                leaf(vec![], bodies[1]),
                fail("withDefault"),
            ),
        )
    );
}

/// Two branches naming the same constructor keep source order: `Cons a _ -> a` is tried
/// before `Cons _ b -> b`, so its `Test` is the outer one and the second's sits in its
/// `default`. That second `Test` is dead — any `Cons` value takes the first — but it is
/// still built; telling that it is dead is coverage checking's question (`LANG-19`).
///
/// The second branch's name is bound at argument `1` rather than `0`, so a lowering that
/// mixed up which argument a name reads from cannot pass by accident the way it could if
/// both branches bound position `0`.
///
/// Mutation-checked by having `decision::lower`'s `Constructor` arm give every argument
/// the occurrence `Step::ConstructorArgument(0)`:
/// `a_constructor_pattern_binds_its_arguments_by_position` cannot tell this from a
/// correct lowering, but `b`'s binding here comes out at `0`, and the assertion goes red.
#[test]
fn two_branches_on_the_same_constructor_keep_source_order() {
    let module = ir_of(indoc! {r#"
        module Test exposing (Item, first)

        type Item
          = Cons Int Int
          | Nil

        first : Item -> Int
        first item =
          case item of
            Cons a _ ->
              a

            Cons _ b ->
              b

            Nil ->
              0
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "first"), "first");

    let argument = |position| Occurrence::Root.field(Step::ConstructorArgument(position));
    assert_eq!(
        tree,
        test_root(
            test_constructor("Item", "Cons", 0, 2),
            leaf(vec![binding("a", argument(0), int())], bodies[0]),
            test_root(
                test_constructor("Item", "Cons", 0, 2),
                leaf(vec![binding("b", argument(1), int())], bodies[1]),
                test_root(
                    test_constructor("Item", "Nil", 1, 0),
                    leaf(vec![], bodies[2]),
                    fail("first"),
                ),
            ),
        )
    );
}

/// A `case` missing a branch for some value of its type — accepted today only because
/// coverage is not checked yet (`LANG-19`) — lowers to a tree whose last `default` is
/// an explicit `Fail` leaf rather than running off the end of the branches with nothing
/// to evaluate.
///
/// The declaration the leaf names is the one `case_tree` handed `decision_tree`: the
/// tree does not find it, and this does not pretend to check that it does. What it
/// checks is the `On` test, its leaf, and that the `default` after it is `Fail` and
/// carries the name it was given.
///
/// Mutation-checked by having `decision::build`, when no branch is left after the one
/// it is lowering, fall back to that branch's own body — a leaf binding nothing — instead
/// of to `Fail`, which is what running off the end would amount to: the assertion goes
/// red.
#[test]
fn a_case_missing_a_branch_has_a_fall_through_leaf() {
    let module = ir_of(indoc! {r#"
        module Test exposing (Flag, ignore)

        type Flag
          = On
          | Off

        ignore : Flag -> Flag
        ignore flag =
          case flag of
            On ->
              Off
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "ignore"), "ignore");

    assert_eq!(
        tree,
        test_root(
            test_constructor("Flag", "On", 0, 0),
            leaf(vec![], bodies[0]),
            fail("ignore"),
        )
    );
}

/// `Basics`' `True` and `False` constructors are tested by value exactly as `true` and
/// `false` are, so a `case` mixing the two spellings lowers to `Test`s in one
/// vocabulary: a backend never has to recognise `Basics.Bool` among constructors.
///
/// Mutation-checked by deleting `translate_pattern`'s `True`/`False` arm, so `True`
/// goes through the general constructor path: the first `Test`'s outcome comes out as
/// `Outcome::Constructor(Basics.Bool.True)`, and the assertion goes red.
#[test]
fn a_bool_constructor_is_tested_by_value_like_a_bool_literal() {
    let module = ir_of(indoc! {r#"
        module Test exposing (choose)

        choose : Bool -> Int
        choose flag =
          case flag of
            True ->
              1

            false ->
              0
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "choose"), "choose");

    assert_eq!(
        tree,
        test_root(
            Outcome::Literal(LiteralValue::Bool(true)),
            leaf(vec![], bodies[0]),
            test_root(
                Outcome::Literal(LiteralValue::Bool(false)),
                leaf(vec![], bodies[1]),
                fail("choose"),
            ),
        )
    );
}
