//! Layer 4: what `check_module` hands a backend.
//!
//! These go through the whole pipeline — parse → canonicalize → type_check → the IR —
//! and assert on the facts a code generator needs and a type checker never did: how many
//! parameters a declaration takes, whether a call supplies every argument its callee
//! wants, what kind of name a reference is, and where a constructor sits in its
//! declaration.
//!
//! The fourth kind of name — a value another module declares — is not merely unasserted
//! here: `ReferenceKind::Foreign` is currently unreachable in any `ir::Module` at all.
//! The typer's environment is built from the module under check alone, so every
//! declaration that would produce one comes back `Solved::UnboundName` and lands in
//! `ir::Module::unchecked` rather than becoming a declaration (`BUG-36`). That the
//! translation tells it apart from the other three is pinned one level down, by
//! `typer::tests::the_four_kinds_of_name_stay_apart`.

use std::collections::HashMap;

use indoc::indoc;
use zelkova_lang::compiler::dependencies::ModuleWalker;
use zelkova_lang::compiler::ir::{
    self, Declaration, Reference, ReferenceKind, Saturation, TypedTerm, TypedTermKind,
};
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::source::{load_package_sources, SourceRoot};
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
}

/// Every value of the canonical module reaches the IR, as a declaration or as one it
/// could not build.
///
/// A backend handed only the declarations that worked cannot tell a module it may emit
/// whole from one that quietly lost a declaration, which is the mistake `DEC-18`'s first
/// decision is about. `helper` below mentions an imported value, which the typer's
/// environment does not hold (`BUG-36`), so it is exactly such a declaration.
///
/// Mutation-checked by dropping the `unchecked.push` in `ir::build`'s catch-all arm:
/// `helper` then goes missing from both lists and the count assertion goes red.
#[test]
fn a_declaration_with_no_ir_is_named_rather_than_dropped() {
    let module = ir_of(indoc! {r#"
        module Test exposing (answer)

        import Maybe

        answer : Int
        answer =
          1

        helper : Int
        helper =
          Maybe.withDefault
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
/// Every declaration is not expected to have an IR: `Bitwise` forwards each of its seven
/// to `Js.Bitwise`, and an imported name goes untyped today (`BUG-36`), so all seven are
/// `unchecked`. What is asserted is that nothing is lost either way.
///
/// Mutation-checked twice: dropping `ir::build`'s `unchecked.push` turns the accounting
/// assertion red for five of the eight modules, and routing `Solved::NoBody` to
/// `unchecked` instead of to a signature turns the `Js.Basics` assertions red.
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
