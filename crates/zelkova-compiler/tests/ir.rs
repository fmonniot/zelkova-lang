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
use zelkova_compiler::dependencies::ModuleWalker;
use zelkova_compiler::ir::{
    self, decision_tree, Binding, CaseForm, Constructor, Decision, Declaration, LiteralValue,
    Occurrence, Outcome, Reference, ReferenceKind, Saturation, Step, TypedTerm, TypedTermKind,
};
use zelkova_compiler::name::{Name, QualName};
use zelkova_compiler::source::{load_package_sources, SourceRoot};
use zelkova_compiler::typer::{Type, TypeLiteral};
use zelkova_compiler::{check_module, check_module_recovering, Interface, PackageName};
use zelkova_syntax::parser;

mod support;

use support::*;

/// The IR `check_module` produces for `source`, insisting that it checked.
fn ir_of(source: &str) -> ir::Module {
    let parsed = parse_source(source);
    let interfaces = HashMap::from([basics_interface(), char_interface(), maybe_interface()]);

    check_module(&test_package(), &interfaces, &parsed)
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
        TypedTermKind::Identifier { reference, .. } => reference,
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
                ReferenceKind::TopLevel(test_qual("Test.echo"))
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
        .find(|union| union.name == test_qual("Test.Colour"))
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
/// Mutation-checked by dropping the `is_parameterless` filter `canonical::initialisation_order`
/// keeps a component's bindings through: `first` and `second` would then both appear.
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

/// `GEN-7`: a function is a node of the graph the order is read from, and a reference to
/// one is an edge, but the function itself is never scheduled — its value exists before
/// its body runs. `usesHelper` depends on `helper`, whose body mentions no other
/// declaration, so `usesHelper` is the whole order.
///
/// Mutation-checked by dropping the `is_parameterless` filter `canonical::initialisation_order`
/// keeps a component's bindings through: `helper` then comes back in the order too, and
/// the exact-list assertion below fails.
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
/// Among components with no path between them, `canonical::initialisation_order` takes
/// the one whose name sorts first; declaring them out of alphabetical order here (`c`,
/// `e`, `a`, `d`, `b`) checks that the result tracks name order rather than source order.
///
/// Mutation-checked by dropping the name from the key `initialisation_order` pops ready
/// components by, so ties fall to the condensed graph's own node order: the order comes
/// back `e` .. `a`. The exact-list assertion pins one order out of the 5! = 120 possible
/// ones, so a nondeterministic order would fail it on all but one in 120 runs, where an
/// assertion comparing two runs to each other could pass whenever a single run happened
/// to iterate consistently with itself.
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
/// decision is about. `helper` below matches a float pattern, which the typer does not
/// translate, so it is exactly such a declaration.
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

        helper : Float -> Int
        helper x =
          case x of
            1.5 ->
              1

            _ ->
              0
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
/// This is `cargo run`'s ten modules, checked the way the compiler checks them —
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
    let root = std::path::Path::new(&manifest)
        .join("../..")
        .join("std/core");

    let sources = load_package_sources(&root, SourceRoot::Src)
        .unwrap_or_else(|e| panic!("failed to load {:?}: {:?}", root, e));
    let modules: Vec<parser::Module> = sources
        .iter()
        .map(|(_, file)| {
            parser::parse(file.file())
                .unwrap_or_else(|e| panic!("parse error in {:?}: {:?}", file.file().name(), e))
        })
        .collect();
    assert_eq!(modules.len(), 10, "std/core holds ten modules");

    let module_files = HashMap::new();
    let package = PackageName::new("zelkova-core").unwrap();
    let walker =
        ModuleWalker::new(&modules, &module_files, &package).expect("no cycle in std/core");
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let (checked, errors) = checked_and_errors(walker.check_in_order(
        &package,
        &mut interfaces,
        &module_files,
        check_module_recovering,
    ));

    assert!(errors.is_empty(), "std/core must check: {:?}", errors);
    assert_eq!(checked.len(), 10);

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
        union: QualName::in_module(test_package(), "Test", union),
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
/// `decision::build` hand the root pattern a `Type::Unit` instead of the scrutinee's
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

/// A constructor nested inside another is a `Test` of its own, one step below
/// [`Occurrence::Root`]: `Wrapper (Circle n)` tests the scrutinee for `Wrapper`, then
/// `Wrapper`'s argument for `Circle`, and binds `n` two steps down, at the type `Circle`
/// declares for it. Each of the two `Test`s falls back to the wildcard branch's leaf.
///
/// Mutation-checked two ways, each red on its own: having `decision::lower`'s
/// `Constructor` arm push each argument at `occurrence` itself rather than at
/// `occurrence.field(..)` (the inner `Test` is then on the root, and the assertion goes
/// red), and restoring `translate_sub_pattern`'s refusal of anything but a variable, `_`
/// or `()` (`inner` has no IR, and `declaration` panics).
#[test]
fn a_nested_constructor_is_a_test_below_the_root() {
    let module = ir_of(indoc! {r#"
        module Test exposing (Count, Shape, Wrapper, inner)

        type Count
          = One
          | Many

        type Shape
          = Dot
          | Circle Count

        type Wrapper
          = Wrapper Shape

        inner : Wrapper -> Count
        inner w =
          case w of
            Wrapper (Circle n) ->
              n

            _ ->
              One
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "inner"), "inner");

    let count = Type::Adt(QualName::in_module(test_package(), "Test", "Count"), vec![]);
    let argument = Occurrence::Root.field(Step::ConstructorArgument(0));
    assert_eq!(
        tree,
        test_root(
            test_constructor("Wrapper", "Wrapper", 0, 1),
            Decision::Test {
                scrutinee: argument.clone(),
                outcome: test_constructor("Shape", "Circle", 1, 1),
                matched: Box::new(leaf(
                    vec![binding(
                        "n",
                        argument.field(Step::ConstructorArgument(0)),
                        count
                    )],
                    bodies[0],
                )),
                default: Box::new(leaf(vec![], bodies[1])),
            },
            leaf(vec![], bodies[1]),
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

/// `Basics`' `True` and `False` constructors are tested by value, as an `Int` or a
/// `Char` literal is, so a backend never has to recognise `Basics.Bool` among
/// constructors. Two branches cover `Bool`, but coverage is not checked (`LANG-19`),
/// so the `False` branch's `Test` still has a `default`: the `Fail` leaf.
///
/// Mutation-checked by deleting `translate_pattern`'s `True`/`False` arm, so `True`
/// goes through the general constructor path: the first `Test`'s outcome comes out as
/// `Outcome::Constructor(Basics.Bool.True)`, and the assertion goes red.
#[test]
fn a_bool_constructor_is_tested_by_its_value() {
    let module = ir_of(indoc! {r#"
        module Test exposing (choose)

        choose : Bool -> Int
        choose flag =
          case flag of
            True ->
              1

            False ->
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

/// A `()` branch matches unconditionally — the unit type has one value — so the tree
/// it lowers to is one leaf with no `Test` and no binding, the way a wildcard's is.
///
/// Mutation-checked by having `decision::lower`'s `Unit` arm bind the value the way
/// its `Bind` arm binds a name: the leaf's bindings come out non-empty and the
/// assertion goes red.
#[test]
fn a_unit_branch_is_a_leaf_with_no_bindings() {
    let module = ir_of(indoc! {r#"
        module Test exposing (always_one)

        always_one : () -> Int
        always_one u =
          case u of
            () ->
              1
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "always_one"), "always_one");

    assert!(matches!(bodies[0].kind, TypedTermKind::Int(1)));
    assert_eq!(tree, leaf(vec![], bodies[0]));
}

// ── LANG-84: a record pattern's entries are reached by their labels ─────────────

/// A record pattern tests nothing — a record has one shape — so a `case` over a record
/// with one branch `{ x, y }` is one leaf, with no `Decision::Test`, binding each entry
/// at the occurrence its label reaches, at its field's type.
///
/// Mutation-checked by having `decision::lower`'s `Record` arm push each entry at
/// `occurrence` itself rather than at `occurrence.field(Step::Field(..))`, which makes
/// the field step a no-op: both bindings then sit at the root, and the assertion goes
/// red.
#[test]
fn a_record_pattern_is_a_leaf_that_binds_each_entry_by_its_label() {
    let module = ir_of(indoc! {r#"
        module Test exposing (sum)

        sum : { x : Int, y : Char } -> Int
        sum r =
          case r of
            { x, y } ->
              x
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "sum"), "sum");

    let field = |label: &str| Occurrence::Root.field(Step::Field(Name::new(label)));
    assert_eq!(
        tree,
        leaf(
            vec![
                binding("x", field("x"), int()),
                binding("y", field("y"), Type::Literal(TypeLiteral::Char)),
            ],
            bodies[0],
        )
    );
}

/// A refutable entry is a `Test` below the record, at the occurrence its label reaches,
/// falling back to the next branch like any other `Test`; the record itself is still not
/// tested, and an irrefutable entry beside it binds once the test has passed.
///
/// Mutation-checked by having `decision::lower`'s `Record` arm push none of the entries,
/// as if a record pattern were irrefutable for being a record: the `Test` on `x` is gone,
/// and the assertion goes red.
#[test]
fn a_refutable_entry_is_a_test_below_its_field() {
    let module = ir_of(indoc! {r#"
        module Test exposing (pick)

        pick : { x : Int, y : Int } -> Int
        pick r =
          case r of
            { x = 0, y } ->
              y

            _ ->
              1
    "#});

    let (tree, bodies) = case_tree(declaration(&module, "pick"), "pick");

    let field = |label: &str| Occurrence::Root.field(Step::Field(Name::new(label)));
    assert_eq!(
        tree,
        Decision::Test {
            scrutinee: field("x"),
            outcome: Outcome::Literal(LiteralValue::Int(0)),
            matched: Box::new(leaf(vec![binding("y", field("y"), int())], bodies[0])),
            default: Box::new(leaf(vec![], bodies[1])),
        }
    );
}

// ── Classes: what a constrained name asks, and the instances a module declares ─────────

/// A class, an instance of it at `Int` and at a box of anything, a constrained function,
/// and a use of that function: the module the three tests below read different parts of.
const CLASSES: &str = indoc! {r#"
    module Test exposing (..)

    type Box a
      = Box a

    class Eq a where
      eq : a -> a -> Bool

    instance Eq Int where
      eq a b =
        True

    instance Eq a => Eq (Box a) where
      eq (Box left) (Box right) =
        eq left right

    same : Eq a => a -> a -> Bool
    same x y =
      eq x y

    plain : Int -> Int
    plain n =
      n

    use : Bool
    use =
      same (plain 1) 2
"#};

/// The `(class, type)` pairs of a context, with the class as its bare name and the type as
/// the typer writes it.
fn context_of(context: &[ir::Predicate]) -> Vec<(String, String)> {
    context
        .iter()
        .map(|predicate| {
            (
                predicate.class.unqualified_name().to_string(),
                format!("{}", predicate.tpe),
            )
        })
        .collect()
}

/// The context a name carries at the one use of it in `term`'s spine head, or a panic.
fn use_context(term: &TypedTerm) -> &[ir::Predicate] {
    match &head_of(term).kind {
        TypedTermKind::Identifier { context, .. } => context,
        other => panic!("expected the head to be a name, got {:?}", other),
    }
}

/// A reference to a constrained function carries its context as instantiated at the use,
/// with the final substitution applied: `same` used at `Int` carries `Eq Int`. A reference
/// to a function with no constraint carries none.
///
/// Mutation-checked by leaving the context of an `Identifier` unzonked in
/// `Substitution::apply_term`: the first assertion sees a `Eq t..` variable and goes red.
#[test]
fn a_reference_to_a_constrained_function_carries_its_instantiated_context() {
    let module = ir_of(CLASSES);
    let use_ = declaration(&module, "use");

    // `same (plain 1) 2`: the outer application's head is `same`.
    assert_eq!(
        context_of(use_context(body(use_, "use"))),
        vec![("Eq".to_string(), "Int".to_string())]
    );

    // `plain 1` has no constraint to carry.
    let TypedTermKind::Apply { fun, .. } = &body(use_, "use").kind else {
        panic!("expected an application");
    };
    let TypedTermKind::Apply { arg, .. } = &fun.kind else {
        panic!("expected `same (plain 1)` to be an application");
    };
    assert_eq!(use_context(arg), &[]);
}

/// A declaration carries its own context, as the variables of its solved type, and the
/// reference to a member inside it carries the very same variable.
///
/// Mutation-checked by having `infer_annotated` answer an empty context: the first
/// assertion goes red.
#[test]
fn a_declaration_carries_its_own_context() {
    let module = ir_of(CLASSES);
    let same = declaration(&module, "same");

    let [predicate] = same.context.as_slice() else {
        panic!("expected one constraint, got {:?}", same.context);
    };
    assert_eq!(predicate.class.unqualified_name().as_str(), "Eq");

    // It is the variable the declaration's type is built from.
    let Type::Variable(_) = &predicate.tpe else {
        panic!("expected a variable, got {:?}", predicate.tpe);
    };
    let Type::Fun { param_tpe, .. } = &same.tpe else {
        panic!("expected a function type, got {:?}", same.tpe);
    };
    assert_eq!(**param_tpe, predicate.tpe);

    // `eq x y` inside it asks the same of the same variable.
    assert_eq!(use_context(body(same, "same")), same.context.as_slice());

    // A declaration with no constraint has an empty context.
    assert_eq!(declaration(&module, "plain").context, vec![]);
}

/// A module carries its instances, each with its class, head, context and one checked body
/// per member.
///
/// Mutation-checked by building `Module::instances` empty in `ir::build`: the first
/// assertion goes red; and by leaving an instance's `context` empty in
/// `InstanceCheck::of`: the `Box` instance's context assertion does.
#[test]
fn a_module_carries_its_instances_with_their_bodies() {
    let module = ir_of(CLASSES);

    assert_eq!(module.instances.len(), 2);
    let [int, boxed] = module.instances.as_slice() else {
        unreachable!()
    };

    assert_eq!(int.class, test_qual("Test.Eq"));
    assert_eq!(int.head, Type::Literal(TypeLiteral::Int));
    assert_eq!(int.context, vec![]);
    assert!(!int.rejected);

    assert_eq!(boxed.class, test_qual("Test.Eq"));
    let Type::Adt(name, args) = &boxed.head else {
        panic!("expected a declared type, got {:?}", boxed.head);
    };
    assert_eq!(name, &test_qual("Test.Box"));
    let [Type::Variable(variable)] = args.as_slice() else {
        panic!("expected one variable argument, got {:?}", args);
    };

    // The context is over the head's own variable.
    assert_eq!(
        boxed.context,
        vec![ir::Predicate {
            class: test_qual("Test.Eq"),
            tpe: Type::Variable(variable.clone()),
        }]
    );

    // One checked body for the one member, with its context carried the way a
    // declaration's is, and no binding left unchecked.
    assert!(boxed.unchecked.is_empty());
    let [member] = boxed.members.as_slice() else {
        panic!("expected one member, got {:?}", boxed.members);
    };
    assert_eq!(member.name, Name::new("eq"));
    assert_eq!(member.arity, 2);
    assert_eq!(member.context.len(), 1);
    assert_eq!(member.context[0].class, test_qual("Test.Eq"));
    assert!(member.body.is_some());
}

/// An instance whose body is the word `derived` is in the IR with the members its class's
/// derivation stands for, checked like a written instance's and not told apart from one.
///
/// Mutation-checked by generating no members for a derived instance in
/// `derivation::derive_all`: `instance.members` is empty and the destructuring panics.
#[test]
fn a_derived_instance_is_in_the_ir_with_its_members() {
    let module = ir_of(indoc! {r#"
        module Test exposing (..)

        type Colour
          = Red

        class Eq a where
          eq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
            combine x y =
              case x of
                True ->
                  y

                False ->
                  False

        instance Eq Colour where
          derived
    "#});

    let [instance] = module.instances.as_slice() else {
        panic!("expected one instance, got {:?}", module.instances);
    };
    assert!(instance.unchecked.is_empty(), "{:?}", instance.unchecked);
    let [member] = instance.members.as_slice() else {
        panic!("expected one member, got {:?}", instance.members);
    };
    assert_eq!(member.name, Name::new("eq"));
    assert_eq!(member.arity, 2);
    assert!(member.body.is_some());
}

/// A binding of an instance that did not check is accounted for the way a value is: it is
/// unchecked, with an error standing behind it, and is not among the members.
///
/// Mutation-checked by clearing the unchecked entries `build_instance` collects: the
/// binding is accounted for nowhere and `unchecked.len()` is 0.
#[test]
fn an_instance_binding_that_did_not_check_is_unchecked() {
    let source = indoc! {r#"
        module Test exposing (..)

        type Colour
          = Red

        class Eq a where
          eq : a -> a -> Bool

        instance Eq Colour where
          eq a b =
            1
    "#};
    let interfaces = HashMap::from([basics_interface(), char_interface()]);
    let zelkova_compiler::dependencies::Outcome::Module(module, errors) =
        check_module_recovering(&test_package(), &interfaces, &parse_source(source))
    else {
        panic!("the module should come back");
    };
    assert_eq!(errors.len(), 1, "{:?}", errors);

    let [instance] = module.ir.instances.as_slice() else {
        panic!("expected one instance");
    };
    assert!(instance.members.is_empty(), "got {:?}", instance.members);
    assert_eq!(instance.unchecked.len(), 1);
    assert_eq!(instance.unchecked[0].name, Name::new("eq"));
    assert!(instance.unchecked[0].reported);
}

// ── LANG-83: what a derived instance's members are ──

/// A term as one line, for reading a generated definition: a name is its last segment, an
/// application is its function and its argument, a `case` is its scrutinee and its
/// branches.
fn shown(term: &TypedTerm) -> String {
    use ir::TermPatternKind;

    fn pattern(p: &ir::TermPattern) -> String {
        match &p.kind {
            TermPatternKind::Anything => "_".to_owned(),
            TermPatternKind::Bind(name) => name.clone(),
            TermPatternKind::Literal { value, .. } => format!("{:?}", value),
            TermPatternKind::Constructor { ctor, args, .. } => {
                let mut parts = vec![ctor.name.to_string()];
                parts.extend(args.iter().map(|arg| pattern(&arg.pattern)));
                if args.is_empty() {
                    parts.remove(0)
                } else {
                    format!("({})", parts.join(" "))
                }
            }
            TermPatternKind::Tuple { elements } => format!(
                "({})",
                elements
                    .iter()
                    .map(|element| pattern(&element.pattern))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TermPatternKind::Unit => "()".to_owned(),
            other => format!("{:?}", other),
        }
    }

    fn argument(term: &TypedTerm) -> String {
        match &term.kind {
            TypedTermKind::Apply { .. } | TypedTermKind::Case { .. } => {
                format!("({})", shown(term))
            }
            _ => shown(term),
        }
    }

    match &term.kind {
        TypedTermKind::Identifier { reference, .. } => match &reference.kind {
            ReferenceKind::Local => reference.name.clone(),
            _ => reference
                .name
                .rsplit('.')
                .next()
                .unwrap_or(&reference.name)
                .to_owned(),
        },
        TypedTermKind::Apply { fun, arg, .. } => format!("{} {}", shown(fun), argument(arg)),
        TypedTermKind::Case {
            scrutinee,
            branches,
            ..
        } => format!(
            "case {} of {{ {} }}",
            match &scrutinee.kind {
                TypedTermKind::Case { .. } => format!("({})", shown(scrutinee)),
                _ => shown(scrutinee),
            },
            branches
                .iter()
                .map(|(p, body)| format!("{} -> {}", pattern(p), shown(body)))
                .collect::<Vec<_>>()
                .join("; ")
        ),
        TypedTermKind::Int(i) => i.to_string(),
        TypedTermKind::Unit => "()".to_owned(),
        TypedTermKind::Tuple(tuple) => format!(
            "({})",
            tuple.iter().map(shown).collect::<Vec<_>>().join(", ")
        ),
        other => format!("{:?}", other),
    }
}

/// The module a test of a derived instance reads: `Comparable` with a derivation whose
/// `combine` mentions its first parameter twice, an instance at `Int` and at `Position`,
/// and `declarations` after them.
fn comparable_with(declarations: &str) -> String {
    format!(
        "{}\n{}",
        indoc! {r#"
            module Test exposing (..)

            type Order
              = LT
              | EQ
              | GT

            class Comparable a where
              compare : a -> a -> Order

              derived compare
                matched = EQ
                differed i j =
                  compare i j

                combine x y =
                  case x of
                    EQ ->
                      y

                    _ ->
                      x

            instance Comparable Int where
              compare a b =
                EQ

            instance Comparable Position where
              compare a b =
                EQ
        "#},
        declarations
    )
}

/// The one member of the one instance of `module` that is not written in `comparable_with`.
fn derived_member(module: &ir::Module, nth: usize) -> &Declaration {
    let instance = &module.instances[nth];
    assert!(instance.unchecked.is_empty(), "{:?}", instance.unchecked);
    let [member] = instance.members.as_slice() else {
        panic!("expected one member, got {:?}", instance.members);
    };
    member
}

/// `shown`, with the serial each fresh name starts with taken off, so that a test can write
/// what a generated definition says without writing how many names came before: `$3$x` is
/// `x`.
fn plain(term: &TypedTerm) -> String {
    let text = shown(term);
    let mut out = String::new();
    let mut rest = text.as_str();

    while let Some(at) = rest.find('$') {
        out.push_str(&rest[..at]);
        let tail = &rest[at + 1..];
        let digits = tail.chars().take_while(|c| c.is_ascii_digit()).count();

        if digits > 0 && tail[digits..].starts_with('$') {
            // `$3$x`: the serial and its two `$` go.
            rest = &tail[digits + 1..];
        } else {
            out.push('$');
            rest = tail;
        }
    }
    out.push_str(rest);
    out
}

/// Every term of `term`, itself included, in the order a reader meets them.
fn walk<'a>(term: &'a TypedTerm, into: &mut Vec<&'a TypedTerm>) {
    into.push(term);
    match &term.kind {
        TypedTermKind::Apply { fun, arg, .. } => {
            walk(fun, into);
            walk(arg, into);
        }
        TypedTermKind::Case {
            scrutinee,
            branches,
            ..
        } => {
            walk(scrutinee, into);
            for (_, branch) in branches {
                walk(branch, into);
            }
        }
        TypedTermKind::Tuple(tuple) => tuple.iter().for_each(|element| walk(element, into)),
        _ => {}
    }
}

/// The applications of the member `compare` in `term`, each as the names of its two
/// arguments.
fn compare_calls(term: &TypedTerm) -> Vec<(String, String)> {
    let mut terms = Vec::new();
    walk(term, &mut terms);

    terms
        .into_iter()
        .filter_map(|term| {
            let TypedTermKind::Apply {
                fun, arg: second, ..
            } = &term.kind
            else {
                return None;
            };
            let TypedTermKind::Apply {
                fun: head,
                arg: first,
                ..
            } = &fun.kind
            else {
                return None;
            };
            let TypedTermKind::Identifier { reference, .. } = &head.kind else {
                return None;
            };
            if !reference.name.ends_with("Test.compare") {
                return None;
            }

            Some((shown(first), shown(second)))
        })
        .collect()
}

/// For `Comparable`'s `combine` — `case x of EQ -> y; _ -> x`, which mentions `x` twice — the
/// definition applies `compare` to each pair of arguments **once**: `x` names the value of the
/// answer for a part, and is not the expression that computes it. Substituted, each level
/// would compute its comparison twice, and the work of a list would be exponential in its
/// length ([`DEC-24` decision 8](../docs/decisions/dec-24.md)).
///
/// Mutation-checked by binding the answer twice in `Generated::place_combine`, which answers
/// each pair twice as substituting `x` would at its two mentions: every pair is then compared
/// twice and the assertion on the calls goes red.
#[test]
fn a_derived_member_answers_each_pair_of_arguments_once() {
    let module = ir_of(&comparable_with(indoc! {r#"
        type Triple
          = Triple Int Int Int

        instance Comparable Triple where
          derived
    "#}));
    let member = derived_member(&module, 2);

    // One comparison for each pair of arguments, left to right, and none for anything else
    // (`Triple` has one constructor, so `differed` is never reached).
    let calls: Vec<(String, String)> = compare_calls(body(member, "compare"));
    let expected: Vec<(String, String)> = [("$a1", "$b1"), ("$a2", "$b2"), ("$a3", "$b3")]
        .iter()
        .map(|(a, b)| (a.to_string(), b.to_string()))
        .collect();
    let mut found = calls.clone();
    found.sort();
    assert_eq!(found, expected, "got {:?}", calls);
}

/// The rest of the fold sits inside the branch of `combine`'s body that reaches `y`, and not
/// ahead of it: the second pair is compared in the `EQ` branch of the case on the first
/// answer, and the branch that returns `x` compares nothing.
///
/// Mutation-checked by binding `y` to the rest of the walk with a `case` ahead of the body in
/// `place_combine`, so that the rest is computed whether the body reaches it or not: the
/// second comparison is then no longer under the `EQ` branch.
#[test]
fn the_rest_of_the_walk_is_where_the_body_reaches_it() {
    let module = ir_of(&comparable_with(indoc! {r#"
        type Pair
          = Pair Int Int

        instance Comparable Pair where
          derived
    "#}));
    let member = derived_member(&module, 2);

    assert_eq!(
        plain(body(member, "compare")),
        "case $left of { (Pair $a1 $a2) -> case $right of { (Pair $b1 $b2) -> \
         case compare $a1 $b1 of { x -> case x of { EQ -> \
         case compare $a2 $b2 of { x -> case x of { EQ -> EQ; _ -> x } }; _ -> x } } } }"
    );
}

/// A `combine` that binds the names a generated definition uses does not capture them: the
/// definition's names are ones no source file can write, so `a2` and `b2` in `combine` are
/// its own. Here the rest of the walk is placed under a branch binding `a2`, and still
/// compares the second pair of arguments, not the answer.
///
/// Mutation-checked by naming the arguments `a1`, `b1`, … (`left_argument` and
/// `right_argument` without the `$`) and not renaming a class's binders: the second pair is
/// then compared under the names `combine` bound, the module no longer type checks, and
/// `ir_of` panics.
#[test]
fn a_combine_that_binds_the_names_a_definition_uses_does_not_capture_them() {
    let module = ir_of(&comparable_with(indoc! {r#"
        class Ordered a where
          order : a -> a -> Order

          derived order
            matched = EQ
            differed i j =
              order i j

            combine x y =
              case x of
                a2 ->
                  case y of
                    b2 ->
                      a2

        instance Ordered Int where
          order a b =
            EQ

        instance Ordered Position where
          order a b =
            EQ

        type Pair
          = Pair Int Int

        instance Ordered Pair where
          derived
    "#}));
    // The instances of the module, in the order written: `Comparable Int`, `Comparable
    // Position` and `Ordered Int`, `Ordered Position`, and the derived `Ordered Pair`.
    let instance = &module.instances[4];
    let [member] = instance.members.as_slice() else {
        panic!("expected one member, got {:?}", instance.members);
    };

    // The second pair is compared where `y` is reached, under the branch that binds `a2` —
    // and what it compares is `$a2` and `$b2`, the arguments the definition took apart,
    // and not the `a2` the class's own pattern bound.
    assert_eq!(
        plain(body(member, "order")),
        "case $left of { (Pair $a1 $a2) -> case $right of { (Pair $b1 $b2) -> \
         case order $a1 $b1 of { x -> case x of { a2 -> \
         case (case order $a2 $b2 of { x -> case x of { a2 -> \
         case EQ of { b2 -> a2 } } }) of { b2 -> a2 } } } } }"
    );
}

/// A member at `a -> R` is derived over one value: `combine p (combine a1 (… an))`, with `p`
/// the answer `atConstructor` gives for the constructor's place and each `a` the member
/// applied to one argument, the last standing for the rest of the walk where the one before
/// it asks for it. A constructor with no argument is `p` alone.
///
/// Mutation-checked by placing the constructor's answer last, after the arguments' (the
/// `answers.push(at_constructor …)` moved below the arguments' loop in
/// `Generated::walk_single`): the exact text goes red.
#[test]
fn a_member_over_one_value_folds_the_constructors_answer_with_each_argument() {
    let module = ir_of(indoc! {r#"
        module Test exposing (..)

        class Hashable a where
          hash : a -> Int

          derived hash
            atConstructor p =
              seed p

            combine x y =
              mix x y

        seed : Position -> Int
        seed p =
          7

        mix : Int -> Int -> Int
        mix a b =
          a

        instance Hashable Int where
          hash n =
            n

        type Shape
          = Two Int Int
          | One Int
          | Zero

        instance Hashable Shape where
          derived
    "#});
    let member = derived_member(&module, 1);

    assert_eq!(member.arity, 1);
    assert_eq!(
        plain(body(member, "hash")),
        "case $value of { \
         (Two $a1 $a2) -> case (case Position 0 of { p -> seed p }) of { \
         x -> mix x (case hash $a1 of { x -> mix x (hash $a2) }) }; \
         (One $a1) -> case (case Position 1 of { p -> seed p }) of { x -> mix x (hash $a1) }; \
         Zero -> case Position 2 of { p -> seed p } }"
    );
}

/// `differed` is handed the places the two constructors are declared at, as `Position`
/// values built by the compiler: the constructor of `Basics.Position`, which no module
/// exposes, named by its declaration.
///
/// Mutation-checked by handing `differed` the places in the opposite order in
/// `Generated::walk_pair`: the `Red`-then-`Green` definition goes red.
#[test]
fn differed_is_handed_the_position_each_constructor_is_declared_at() {
    let module = ir_of(&comparable_with(indoc! {r#"
        type Colour
          = Red
          | Green

        instance Comparable Colour where
          derived
    "#}));
    let member = derived_member(&module, 2);

    assert_eq!(
        plain(body(member, "compare")),
        "case $left of { \
         Red -> case $right of { Red -> EQ; Green -> \
         case Position 0 of { i -> case Position 1 of { j -> compare i j } } }; \
         Green -> case $right of { Green -> EQ; Red -> \
         case Position 1 of { i -> case Position 0 of { j -> compare i j } } } }"
    );

    // Each `Position` is the constructor of `Basics.Position`, the first of its one
    // constructor's, taking an `Int`.
    let mut terms = Vec::new();
    walk(body(member, "compare"), &mut terms);
    let positions: Vec<&Constructor> = terms
        .iter()
        .filter_map(|term| match &term.kind {
            TypedTermKind::Identifier {
                reference:
                    Reference {
                        kind: ReferenceKind::Constructor(constructor),
                        ..
                    },
                ..
            } if constructor.union == core_qual("Basics.Position") => Some(constructor),
            _ => None,
        })
        .collect();
    assert_eq!(positions.len(), 4);
    for constructor in positions {
        assert_eq!(constructor.name, Name::new("Position"));
        assert_eq!(constructor.index, 0);
        assert_eq!(constructor.arity, 1);
    }
}

/// A tuple has one shape: the fold of its elements and nothing before it, so there is no
/// position to hand anyone.
///
/// Mutation-checked by giving a tuple a second way to be built in `Shape::alternatives`, which
/// makes the walk hand `differed` two places: the `Position` in the text goes red.
#[test]
fn a_tuple_is_walked_as_one_shape_with_only_the_fold() {
    let module = ir_of(&comparable_with(indoc! {r#"
        instance Comparable (a, b) where
          derived
    "#}));
    let instance = &module.instances[2];

    // What the instance needs of the tuple's two elements.
    let classes: Vec<_> = instance
        .context
        .iter()
        .map(|predicate| predicate.class.unqualified_name().to_string())
        .collect();
    assert_eq!(classes, vec!["Comparable", "Comparable"]);

    let member = derived_member(&module, 2);
    assert_eq!(
        plain(body(member, "compare")),
        "case $left of { ($a1, $a2) -> case $right of { ($b1, $b2) -> \
         case compare $a1 $b1 of { x -> case x of { EQ -> \
         case compare $a2 $b2 of { x -> case x of { EQ -> EQ; _ -> x } }; _ -> x } } } }"
    );
}

/// A tuple walked over one value starts at its first element's answer: with no constructor
/// there is no `atConstructor` to begin at and no position to hand it, so the fold is
/// `combine (hash a1) (hash a2)` and nothing before it.
///
/// Mutation-checked by answering for a constructor whether or not the alternative has one
/// (`if true` for `alternative.constructor.is_some()` in `Generated::walk_single`): the fold then
/// starts at `atConstructor (Position 0)` and the exact text goes red.
#[test]
fn a_tuple_walked_over_one_value_starts_at_its_first_elements_answer() {
    let module = ir_of(indoc! {r#"
        module Test exposing (..)

        class Hashable a where
          hash : a -> Int

          derived hash
            atConstructor p =
              seed p

            combine x y =
              mix x y

        seed : Position -> Int
        seed p =
          7

        mix : Int -> Int -> Int
        mix a b =
          a

        instance Hashable Int where
          hash n =
            n

        instance Hashable (a, b) where
          derived
    "#});
    let instance = &module.instances[1];

    let classes: Vec<_> = instance
        .context
        .iter()
        .map(|predicate| predicate.class.unqualified_name().to_string())
        .collect();
    assert_eq!(classes, vec!["Hashable", "Hashable"]);

    let member = derived_member(&module, 1);
    let text = plain(body(member, "hash"));
    assert_eq!(
        text,
        "case $value of { ($a1, $a2) -> case hash $a1 of { x -> mix x (hash $a2) } }"
    );
    assert!(!text.contains("Position"), "{}", text);
}

/// An instance derived in another module than its class names the class's member as it names
/// any imported value, and carries the context it inferred, given to each member.
///
/// Mutation-checked by naming a member `VarTopLevel` whatever module the class is in, in
/// `Generated::member_reference`: the reference is then a top-level one of a module that is not
/// this one, and the `Foreign` assertion goes red.
#[test]
fn an_instance_derived_beside_its_type_calls_the_members_of_a_class_it_imports() {
    let classes = indoc! {r#"
        module Classes exposing (Eq)

        class Eq a where
          eq : a -> a -> Bool

          derived eq
            matched = True
            differed _ _ = False
            combine x y =
              case x of
                True ->
                  y

                False ->
                  False
    "#};
    let types = indoc! {r#"
        module Types exposing (Box(..))

        import Classes exposing (Eq)

        type Box a
          = Box a

        instance Eq (Box a) where
          derived
    "#};

    let mut interfaces = HashMap::from([basics_interface(), char_interface()]);
    let classes = check_module(&test_package(), &interfaces, &parse_source(classes))
        .unwrap_or_else(|error| panic!("expected Classes to check, got {:?}", error));
    interfaces.insert("Classes".into(), classes.to_interface(None));
    let module = check_module(&test_package(), &interfaces, &parse_source(types))
        .unwrap_or_else(|error| panic!("expected Types to check, got {:?}", error))
        .ir;

    let [instance] = module.instances.as_slice() else {
        panic!("expected one instance, got {:?}", module.instances);
    };
    assert!(instance.unchecked.is_empty(), "{:?}", instance.unchecked);
    let [predicate] = instance.context.as_slice() else {
        panic!("expected one constraint, got {:?}", instance.context);
    };
    assert_eq!(predicate.class, test_qual("Classes.Eq"));

    let [member] = instance.members.as_slice() else {
        panic!("expected one member, got {:?}", instance.members);
    };
    assert_eq!(member.context.len(), 1);
    assert_eq!(member.context[0].class, test_qual("Classes.Eq"));

    // The member is an imported value, named by the module that declared it.
    let mut terms = Vec::new();
    walk(body(member, "eq"), &mut terms);
    let members: Vec<&Reference> = terms
        .iter()
        .filter_map(|term| match &term.kind {
            TypedTermKind::Identifier { reference, .. }
                if reference.name.ends_with("Classes.eq") =>
            {
                Some(reference)
            }
            _ => None,
        })
        .collect();
    assert_eq!(members.len(), 1);
    assert!(
        matches!(&members[0].kind, ReferenceKind::Foreign(name, _, _) if *name == test_qual("Classes.eq")),
        "{:?}",
        members[0]
    );
}

// ── GEN-24: which instance a use is, and which copy a use needs ──

/// `Classes`: a class and its instances at `Int`, `Bool` and a box of anything, a class that
/// needs the first, and the constrained functions the tests below use — one that calls
/// another at its own variable and one that reaches the superclass's member through the
/// subclass's constraint.
const SPECIALISED_CLASSES: &str = indoc! {r#"
    module Classes exposing (..)

    type Box a
      = Box a

    class Eq a where
      eq : a -> a -> Bool

    class Eq a => Ord a where
      lt : a -> a -> Bool

    instance Eq Int where
      eq a b =
        True

    instance Eq Bool where
      eq a b =
        True

    instance Eq a => Eq (Box a) where
      eq (Box left) (Box right) =
        eq left right

    instance Ord Int where
      lt a b =
        True

    same : Eq a => a -> a -> Bool
    same x y =
      eq x y

    both : Eq a => a -> a -> Bool
    both x y =
      same x y

    sameOrLess : Ord a => a -> a -> Bool
    sameOrLess x y =
      eq x y
"#};

/// A module using `Classes`: `uses` is the body of the one declaration it holds, `use`.
fn using_classes(module: &str, uses: &str) -> String {
    format!(
        "module {}\n\nimport Classes exposing (..)\n\nuse : Bool\nuse =\n  {}\n",
        module, uses
    )
}

/// What a name resolved to, as a test reads it: `spec 2`, `Eq@Int.eq`, or the name.
fn resolution(reference: &Reference) -> String {
    let head = |head: &zelkova_compiler::canonical::HeadName| match head {
        zelkova_compiler::canonical::HeadName::Type(name) => {
            name.unqualified_name().as_str().to_string()
        }
        other => other.describe(),
    };

    match &reference.kind {
        ReferenceKind::Specialised(index) => format!("spec {}", index),
        ReferenceKind::InstanceMember(member) => format!(
            "{}@{}.{} of {}",
            member.class.unqualified_name(),
            head(&member.head),
            member.member,
            member.module.name()
        ),
        _ => reference.name.clone(),
    }
}

/// Every name in `term` that is not a local, as [`resolution`] writes it, in the order a reader
/// meets them.
fn resolutions(term: &TypedTerm) -> Vec<String> {
    let mut terms = Vec::new();
    walk(term, &mut terms);
    terms
        .into_iter()
        .filter_map(|term| match &term.kind {
            TypedTermKind::Identifier { reference, .. }
                if !matches!(reference.kind, ReferenceKind::Local) =>
            {
                Some(resolution(reference))
            }
            _ => None,
        })
        .collect()
}

/// What a specialisation is a copy of, and the types it is at: `same [Int]`.
fn spec_text(specialisation: &ir::Specialisation) -> String {
    let of = match &specialisation.of {
        ir::Subject::Declaration(name) => name.unqualified_name().as_str().to_string(),
        ir::Subject::InstanceMember {
            class,
            head,
            member,
        } => format!(
            "{}@{}.{}",
            class.unqualified_name(),
            match head {
                zelkova_compiler::canonical::HeadName::Type(name) =>
                    name.unqualified_name().as_str().to_string(),
                other => other.describe(),
            },
            member
        ),
    };
    let key: Vec<String> = specialisation.key.iter().map(|t| t.to_string()).collect();
    format!("{} [{}]", of, key.join(", "))
}

fn specs(modules: &[zelkova_compiler::CheckedModule], name: &str) -> Vec<String> {
    module_named(modules, name)
        .ir
        .specialisations
        .iter()
        .map(spec_text)
        .collect()
}

/// A constrained function used at two types in one module is two specialisations of that
/// module, and one used twice at one type is one. The copies carry the ground type, not a
/// variable, and nothing but the constrained variable is in the key.
///
/// Mutation-checked twice: dropping the `table` lookup in `Reader::specialise` makes the
/// second use of `Int` a third specialisation; making the key empty makes the two types one.
#[test]
fn a_constrained_function_at_two_types_is_two_specialisations_and_at_one_type_is_one() {
    let main = format!(
        "{}\n\nsecond : Bool\nsecond =\n  same 3 4\n\nthird : Bool\nthird =\n  same True False\n",
        using_classes("Main exposing (..)", "same 1 2")
    );
    let modules = specialised_package(&[SPECIALISED_CLASSES, &main]);

    // `Classes` is used by no one inside itself, and holds a copy of nothing.
    assert_eq!(specs(&modules, "Classes"), Vec::<String>::new());
    assert_eq!(
        specs(&modules, "Main"),
        vec!["same [Int]".to_string(), "same [Bool]".to_string()]
    );

    // Each copy is an ordinary declaration at the ground type, with the declaration's own
    // name and arity and no context left.
    let main = module_named(&modules, "Main");
    let spec = &main.ir.specialisations[0];
    assert_eq!(spec.declaration.name, Name::new("same"));
    assert_eq!(spec.declaration.arity, 2);
    assert_eq!(spec.declaration.context, vec![]);
    assert_eq!(
        format!("{}", spec.declaration.tpe),
        "Int -> Int -> Bool",
        "the key is applied to the declaration's type"
    );
    assert_eq!(
        spec.of,
        ir::Subject::Declaration(test_qual("Classes.same")),
        "a copy says which declaration it is of"
    );
}

/// A use inside a constrained function, at that function's own variable, is resolved once the
/// function is specialised: the `eq` in `same`, asked at `a`, is the instance's member at
/// `Int` in the copy at `Int`. The declaration itself is not touched, so another module's
/// copy at another type resolves it differently; and `both`, which calls `same` at its own
/// variable, is a copy that calls the copy of `same`.
///
/// Mutation-checked by applying no assignment to a reference's context in `Reader::identifier`:
/// the obligation is on a variable, and the build reports it as not a type yet.
#[test]
fn a_use_at_the_functions_own_variable_is_resolved_in_the_copy() {
    let ints = using_classes("Ints exposing (..)", "both 1 2");
    let bools = using_classes("Bools exposing (..)", "same True False");
    let modules = specialised_package(&[SPECIALISED_CLASSES, &ints, &bools]);

    // `both` at `Int` is found first, and reads `same` at `Int`, which is new.
    assert_eq!(
        specs(&modules, "Ints"),
        vec!["both [Int]".to_string(), "same [Int]".to_string()]
    );
    let ints = module_named(&modules, "Ints");
    let [both, same] = ints.ir.specialisations.as_slice() else {
        panic!("expected two specialisations");
    };
    assert_eq!(
        resolutions(&both.declaration.body.as_ref().unwrap().expression),
        vec!["spec 1".to_string()]
    );
    assert_eq!(
        resolutions(&same.declaration.body.as_ref().unwrap().expression),
        vec!["Eq@Int.eq of Classes".to_string()]
    );

    // Another module's copy of `same`, at `Bool`, resolves the same `eq` to the other instance.
    let bools = module_named(&modules, "Bools");
    assert_eq!(
        resolutions(
            &bools.ir.specialisations[0]
                .declaration
                .body
                .as_ref()
                .unwrap()
                .expression
        ),
        vec!["Eq@Bool.eq of Classes".to_string()]
    );

    // The declaration still asks: nothing about it was changed.
    let classes = module_named(&modules, "Classes");
    let same = declaration(&classes.ir, "same");
    assert_eq!(use_context(body(same, "same")), same.context.as_slice());
}

/// A use through an instance with a context asks for the member at the type the instance's
/// variable is at: `eq` at `Box Colour` is a specialisation of the `Box` instance's member
/// at `Colour`, placed in the module that uses it, and its `eq` at the box's contents is
/// `Colour`'s own instance member — declared in the using module, which the declaring
/// one could not have named. Nested boxes ask the same of each layer.
///
/// Mutation-checked by resolving a member whose instance has a context as a direct reference
/// in `Reader::resolve_member`: the specialisations are empty and the first assertion goes red.
#[test]
fn a_use_through_an_instance_with_a_context_is_the_member_specialised_at_its_argument() {
    let main = indoc! {r#"
        module Main exposing (..)

        import Classes exposing (..)

        type Colour
          = Red

        instance Eq Colour where
          eq a b =
            True

        use : Bool
        use =
          eq (Box Red) (Box Red)

        nested : Bool
        nested =
          eq (Box (Box Red)) (Box (Box Red))
    "#};
    let modules = specialised_package(&[SPECIALISED_CLASSES, main]);

    // Roots are read in name order, so `nested` is found before `use`.
    assert_eq!(
        specs(&modules, "Main"),
        vec![
            "Eq@Box.eq [Box Colour]".to_string(),
            "Eq@Box.eq [Colour]".to_string(),
        ]
    );

    let main = module_named(&modules, "Main");
    let [nested, colour] = main.ir.specialisations.as_slice() else {
        panic!("expected two specialisations");
    };
    // The copy at `Colour` calls `Colour`'s member, which `Main` declares.
    assert_eq!(
        resolutions(&colour.declaration.body.as_ref().unwrap().expression),
        vec!["Eq@Colour.eq of Main".to_string()]
    );
    // The copy at `Box Colour` calls the copy at `Colour`: both are specialisations of the
    // same member, at a key each.
    assert_eq!(
        resolutions(&nested.declaration.body.as_ref().unwrap().expression),
        vec!["spec 1".to_string()]
    );
    assert_eq!(colour.declaration.arity, 2);
}

/// A use of a superclass's member through a subclass's constraint resolves like any other:
/// in the copy of `sameOrLess` at `Int`, `eq` is asked at `Int`, whose instance exists because
/// `instance Ord Int` could not have been declared without it.
///
/// Mutation-checked by looking the instance up under the class of the enclosing function's
/// first constraint, `Ord`, instead of the member's own class, in `Reader::resolve_member`: the
/// `Ord Int` instance has no member named `eq`, and the build does not specialise.
#[test]
fn a_superclass_member_through_the_subclass_constraint_resolves() {
    let main = using_classes("Main exposing (..)", "sameOrLess 1 2");
    let modules = specialised_package(&[SPECIALISED_CLASSES, &main]);

    assert_eq!(
        specs(&modules, "Main"),
        vec!["sameOrLess [Int]".to_string()]
    );
    let main = module_named(&modules, "Main");
    assert_eq!(
        resolutions(
            &main.ir.specialisations[0]
                .declaration
                .body
                .as_ref()
                .unwrap()
                .expression
        ),
        vec!["Eq@Int.eq of Classes".to_string()]
    );
}

/// A specialisation belongs to the module that uses it, and two modules that use one key
/// each hold a copy: the declaring module holds none, and does not come to import anything
/// its users declare.
///
/// Mutation-checked by pushing every specialisation onto the module that declares the
/// function in `World::read_module`: `Classes` then holds both and `Left` and `Right` none.
#[test]
fn a_specialisation_is_assigned_to_the_module_that_uses_it() {
    let left = using_classes("Left exposing (..)", "same 1 2");
    let right = using_classes("Right exposing (..)", "same 3 4");
    let modules = specialised_package(&[SPECIALISED_CLASSES, &left, &right]);

    assert_eq!(specs(&modules, "Classes"), Vec::<String>::new());
    assert_eq!(specs(&modules, "Left"), vec!["same [Int]".to_string()]);
    assert_eq!(specs(&modules, "Right"), vec!["same [Int]".to_string()]);

    // Each copy is its own declaration: a different module's, not a shared one.
    let [left, right] = [
        &module_named(&modules, "Left").ir.specialisations[0],
        &module_named(&modules, "Right").ir.specialisations[0],
    ];
    assert_eq!(left.of, right.of);
    assert_eq!(left.key, right.key);
}

/// A copy of a body written in one module, in another, names what the first declared as an
/// import of it, with the arity it was written with; and a reference of the declaring module
/// to the using module, which it imports, is the using module's own.
///
/// Mutation-checked by returning the reference unchanged from `Reader::rebase`: `helper`
/// is then a `TopLevel` name of a module that is not the one the copy is in.
#[test]
fn a_copy_names_what_its_declaring_module_declared_as_an_import() {
    let classes = indoc! {r#"
        module Lib exposing (..)

        class Eq a where
          eq : a -> a -> Bool

        instance Eq Int where
          eq a b =
            True

        helper : Int -> Int -> Int
        helper a b =
          a

        same : Eq a => a -> a -> Int
        same x y =
          helper 1 2
    "#};
    let main = indoc! {r#"
        module Main exposing (..)

        import Lib exposing (..)

        use : Int
        use =
          same 1 2
    "#};
    let modules = specialised_package(&[classes, main]);

    let main = module_named(&modules, "Main");
    let expression = &main.ir.specialisations[0]
        .declaration
        .body
        .as_ref()
        .unwrap()
        .expression;
    let names = resolutions(expression);
    assert_eq!(names.len(), 1, "got {:?}", names);
    assert!(names[0].ends_with("helper"), "{}", names[0]);

    let mut terms = Vec::new();
    walk(expression, &mut terms);
    let helper = terms
        .iter()
        .find_map(|term| match &term.kind {
            TypedTermKind::Identifier { reference, .. } => Some(reference),
            _ => None,
        })
        .expect("a reference to helper");
    assert!(
        matches!(&helper.kind, ReferenceKind::Foreign(name, _, 2) if *name == test_qual("Lib.helper")),
        "{:?}",
        helper
    );
}

/// What a class member's application is saturated at is the member's arity, once its instance
/// is known: the type checker could not say, a member being no declaration of any module, so
/// the pass does. The node that supplies the second argument of a two-parameter instance
/// binding is the direct call, and the one before it is a partial application.
///
/// Mutation-checked by leaving the flags the type checker gave in `Reader::application`:
/// both are `Partial` and the outer assertion goes red.
#[test]
fn a_member_applied_to_all_its_arguments_is_saturated_once_its_instance_is_known() {
    let main = using_classes("Main exposing (..)", "eq 1 2");
    let modules = specialised_package(&[SPECIALISED_CLASSES, &main]);

    let main = module_named(&modules, "Main");
    let expression = body(declaration(&main.ir, "use"), "use");
    let TypedTermKind::Apply {
        fun,
        saturation: outer,
        ..
    } = &expression.kind
    else {
        panic!("expected an application, got {:?}", expression.kind);
    };
    let TypedTermKind::Apply {
        saturation: inner, ..
    } = &fun.kind
    else {
        panic!("expected a partial application beneath it");
    };
    assert_eq!(*outer, Saturation::Saturated);
    assert_eq!(*inner, Saturation::Partial);

    // The head is a direct reference to the instance's member, which takes two parameters.
    let ReferenceKind::InstanceMember(member) = &reference(head_of(expression)).kind else {
        panic!("expected an instance member");
    };
    assert_eq!(member.arity, 2);
    assert_eq!(member.module.name().as_str(), "Classes");
}

/// A constrained function that asks for itself at an ever larger type is the limit's error,
/// naming the function and the type it had reached, and it is the same error when the chain
/// runs through two functions. Nothing is changed in a build that has one.
///
/// Mutation-checked by raising `SPECIALISATION_LIMIT` to `usize::MAX`: the pass does not
/// return, since the chain has no end, which is what the limit exists to stop.
#[test]
fn a_function_that_needs_itself_at_a_larger_type_is_the_limits_error() {
    let direct = indoc! {r#"
        module Loop exposing (..)

        type Box a
          = Box a

        class Eq a where
          eq : a -> a -> Bool

        instance Eq Int where
          eq a b =
            True

        instance Eq a => Eq (Box a) where
          eq (Box left) (Box right) =
            eq left right

        f : Eq a => a -> Bool
        f x =
          f (Box x)

        use : Bool
        use =
          f 1
    "#};
    let through_two = direct.replace(
        "f : Eq a => a -> Bool\nf x =\n  f (Box x)\n",
        "f : Eq a => a -> Bool\nf x =\n  g x\n\ng : Eq a => a -> Bool\ng y =\n  f (Box y)\n",
    );
    assert_ne!(direct, through_two, "the second source has to differ");

    for source in [direct.to_string(), through_two] {
        let mut modules = check_package_modules(&[&source]);
        let Err(failures) = specialise_all(&mut modules) else {
            panic!("expected the limit's error");
        };

        let [failure] = failures.as_slice() else {
            panic!("expected one module's errors, got {:?}", failures);
        };
        assert_eq!(failure.module.name().as_str(), "Loop");
        let [ir::SpecialiseError::Unbounded {
            declaration,
            types,
            span,
            ..
        }] = failure.errors.as_slice()
        else {
            panic!("expected one limit error, got {:?}", failure.errors);
        };
        assert_eq!(declaration, &Name::new("f"));

        // The type that kept growing is a box around a box, as many deep as the limit.
        let [tpe] = types.as_slice() else {
            panic!("expected one type, got {:?}", types);
        };
        assert_eq!(
            format!("{}", tpe).matches("Box").count(),
            ir::SPECIALISATION_LIMIT
        );

        // The caret is on `f`'s declaration.
        let start = source.find("f : Eq a").expect("f is declared");
        assert_eq!(span.to_range().map(|range| range.start), Some(start));

        // The message names the declaration and says what the limit is.
        let message = zelkova_compiler::PhaseError::message(&failure.errors[0]);
        assert!(message.contains("`f`"), "{}", message);
        let notes = zelkova_compiler::PhaseError::notes(&failure.errors[0]);
        assert!(
            notes[0].contains(&ir::SPECIALISATION_LIMIT.to_string()),
            "{:?}",
            notes
        );

        // Nothing was changed: the module still has none of what the pass would have made.
        assert!(module_named(&modules, "Loop").ir.specialisations.is_empty());
    }
}

/// A chain through many different functions is a finite set and is not the limit's error: the
/// count is of one declaration in one chain.
///
/// Mutation-checked by counting every specialisation of a chain and not one declaration's, in
/// `Reader::specialise`: a hundred functions then hit the limit and the build does not
/// specialise.
#[test]
fn a_long_chain_of_different_functions_is_not_the_limit() {
    let mut source = String::from(indoc! {r#"
        module Chain exposing (..)

        class Eq a where
          eq : a -> a -> Bool

        instance Eq Int where
          eq a b =
            True

    "#});
    let count = ir::SPECIALISATION_LIMIT * 3;
    for index in 0..count {
        let call = if index + 1 == count {
            "eq x x".to_string()
        } else {
            format!("f{} x", index + 1)
        };
        source.push_str(&format!(
            "f{index} : Eq a => a -> Bool\nf{index} x =\n  {call}\n\n"
        ));
    }
    source.push_str("use : Bool\nuse =\n  f0 1\n");

    let modules = specialised_package(&[&source]);
    assert_eq!(
        module_named(&modules, "Chain").ir.specialisations.len(),
        count
    );
}

/// The result for one build is the same however often it is asked for: the same
/// specialisations in the same order, with the same bodies. The three roots below find three
/// different keys, so an order that depended on anything but the source would show.
///
/// Mutation-checked by reading the roots of a module in the order of a `HashMap` in
/// `World::read_module` (the `declarations` index, which is unordered): the numbering differs
/// between two of the eight runs.
#[test]
fn the_result_for_one_build_is_the_same_on_every_run() {
    let main = indoc! {r#"
        module Main exposing (..)

        import Classes exposing (..)

        type Colour
          = Red

        instance Eq Colour where
          eq a b =
            True

        a : Bool
        a =
          same 1 2

        b : Bool
        b =
          same True False

        c : Bool
        c =
          same (Box Red) (Box Red)

        d : Bool
        d =
          both 1 2

        e : Bool
        e =
          sameOrLess 1 2
    "#};

    let summary = || {
        let modules = specialised_package(&[SPECIALISED_CLASSES, main]);
        modules
            .iter()
            .map(|module| format!("{:?}", module.ir))
            .collect::<Vec<_>>()
    };

    let first = summary();
    for run in 1..8 {
        assert_eq!(summary(), first, "run {} differs from the first", run);
    }
}

/// A class at a record type is accepted by the type checker, which looks no instance up for
/// one, and has no instance to be resolved to: the pass says so, naming the class and the
/// record, and points at the ticket that implements it.
///
/// Mutation-checked by treating a record as resolved with no instance in `Reader::resolve_member`:
/// the pass returns `Ok` and the first assertion goes red.
#[test]
fn a_class_at_a_record_type_has_no_instance_to_resolve_to() {
    let source = indoc! {r#"
        module Rec exposing (..)

        class Eq a where
          eq : a -> a -> Bool

        same : { x : Int } -> Bool
        same r =
          eq r r
    "#};
    let mut modules = check_package_modules(&[source]);
    let Err(failures) = specialise_all(&mut modules) else {
        panic!("expected an error for a class at a record");
    };

    let [ir::SpecialiseError::NoInstance {
        class, tpe, within, ..
    }] = failures[0].errors.as_slice()
    else {
        panic!(
            "expected one missing instance, got {:?}",
            failures[0].errors
        );
    };
    assert_eq!(class, &Name::new("Eq"));
    assert!(matches!(tpe, Type::Record(_)), "{:?}", tpe);
    assert_eq!(within, &Name::new("same"));

    let notes = zelkova_compiler::PhaseError::notes(&failures[0].errors[0]);
    assert!(
        notes.iter().any(|note| note.contains("LANG-85")),
        "{:?}",
        notes
    );
}

/// The members of an instance with no context are ordinary declarations of its module, and
/// what a copy elsewhere names of the module's private values is what the module has to hand
/// over: the declarations its constrained functions and its instances' bindings mention, and
/// no constrained one, since none has a function under its own name.
///
/// Mutation-checked by returning an empty list from `mentioned_by_copies`: the first assertion
/// goes red.
#[test]
fn what_a_copy_names_is_what_the_declaring_module_has_to_hand_over() {
    let source = indoc! {r#"
        module Lib exposing (Eq, same)

        class Eq a where
          eq : a -> a -> Bool

        instance Eq Int where
          eq a b =
            private a b

        same : Eq a => a -> a -> Bool
        same x y =
          helper (eq x y)

        helper : Bool -> Bool
        helper b =
          b

        private : Int -> Int -> Bool
        private a b =
          True

        unrelated : Int
        unrelated =
          1

        constrained : Eq a => a -> a -> Bool
        constrained x y =
          same x y
    "#};
    let modules = check_package_modules(&[source]);
    let lib = &module_named(&modules, "Lib").ir;

    assert_eq!(
        zelkova_compiler::ir::mentioned_by_copies(lib),
        vec![Name::new("helper"), Name::new("private")]
    );
}

/// An instance member with no parameters is a parameterless item of its module, and is
/// initialised after what it mentions and before what mentions it. The declaration it
/// mentions comes first in the order canonicalization worked out, and the one that mentions
/// the member too, so the member lands between them.
///
/// Mutation-checked by returning `module.initialisation_order` as items, ignoring the instance
/// members, in `initialisation_items`: the member is last and the assertion goes red.
#[test]
fn an_instance_member_that_takes_no_parameter_is_initialised_between_its_neighbours() {
    let source = indoc! {r#"
        module Defaults exposing (..)

        class Default a where
          def : a

        instance Default Int where
          def =
            base

        base : Int
        base =
          3

        value : Int
        value =
          def
    "#};
    let modules = specialised_package(&[source]);
    let module = &module_named(&modules, "Defaults").ir;

    let items = ir::initialisation_items(module);
    let names: Vec<String> = items
        .iter()
        .map(|item| match item {
            ir::Item::Declaration(index) => module.declarations[*index].name.to_string(),
            ir::Item::InstanceMember { instance, member } => format!(
                "instance {}",
                module.instances[*instance].members[*member].name
            ),
            ir::Item::Specialisation(index) => format!("spec {}", index),
        })
        .collect();
    assert_eq!(names, vec!["base", "instance def", "value"]);
}

/// The initialisation order of a module with no instance member and no specialisation is the
/// order canonicalization gave it, whole.
///
/// Mutation-checked by reordering the sequence `initialisation_items` starts from: the two
/// independent bindings, which are in name order, swap.
#[test]
fn a_module_without_either_keeps_the_order_canonicalization_gave_it() {
    let module = ir_of(indoc! {r#"
        module Test exposing (..)

        b : Int
        b =
          1

        a : Int
        a =
          2

        c : Int
        c =
          b
    "#});

    let names: Vec<String> = ir::initialisation_items(&module)
        .iter()
        .map(|item| match item {
            ir::Item::Declaration(index) => module.declarations[*index].name.to_string(),
            other => panic!("expected only declarations, got {:?}", other),
        })
        .collect();
    let expected: Vec<String> = module
        .initialisation_order
        .iter()
        .map(|name| name.to_string())
        .collect();
    assert_eq!(names, expected);
    assert_eq!(names, vec!["a", "b", "c"]);
}
