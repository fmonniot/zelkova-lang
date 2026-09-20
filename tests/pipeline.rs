//! Layer 3: End-to-end pipeline tests.
//!
//! These tests exercise `check_module` — the full canonicalize → type_check →
//! exhaustiveness pipeline — on known inputs, including real modules under
//! `std/core/src/`. A failure here can implicate canonicalization or
//! `typer::type_check`. `exhaustiveness::check` is still a stub returning
//! `Ok(())`, so a pipeline test cannot fail for exhaustiveness reasons yet.
//!
//! The last section goes one level up and drives `compile_package`, the
//! whole-package entry point, over the fixture packages in `tests/fixtures/`.

use std::collections::HashMap;
use std::path::Path;

use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::canonical;
use zelkova_lang::compiler::dependencies::{self, ModuleWalker};
use zelkova_lang::compiler::manifest;
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::resolve;
use zelkova_lang::compiler::source::{
    load_package_sources, load_package_sources_into, SourceFiles,
};
use zelkova_lang::compiler::{
    check_module, compile_package, parser, CompilationError, Interface, PackageName, PhaseError,
};

mod support;

use support::*;

// ── Helpers ──────────────────────────────────────────────────────────────────

fn std_package() -> PackageName {
    PackageName::new("zelkova-core").unwrap()
}

fn parse_file(path: &Path) -> parser::Module {
    let source = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("failed to read {:?}: {}", path, e));
    let file = SimpleFile::new(
        path.file_name().unwrap().to_string_lossy().to_string(),
        source,
    );
    parser::parse(&file).unwrap_or_else(|e| panic!("parse error in {:?}: {:?}", path, e))
}

fn std_src() -> std::path::PathBuf {
    // The workspace root is `CARGO_MANIFEST_DIR` at build time.
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    Path::new(&manifest).join("std/core/src")
}

/// `std/core`, the package directory `compile_package` now takes — `zelkova.toml`
/// beside `src/`, as opposed to [`std_src`], which stays pointed at the source
/// root itself for the tests here that parse individual files or drive
/// `load_package_sources` directly.
fn std_package_root() -> std::path::PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    Path::new(&manifest).join("std/core")
}

/// Root of one of the small package fixtures under `tests/fixtures/`.
///
/// `compile_package` takes a package directory — `zelkova.toml` beside `src/` —
/// so the whole-package tests need real directories on disk rather than the
/// source strings the other layers use. These fixtures stay small and
/// single-purpose; `std/core` is exercised separately by
/// `stdlib_package_compiles`.
fn fixture_package(name: &str) -> std::path::PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    Path::new(&manifest).join("tests/fixtures").join(name)
}

/// The `.zel` modules `compile_package` would pick up under `root`, sorted.
///
/// An *existing* root holding no `.zel` files at all still loads as zero sources
/// and zero errors, indistinguishable from a package with no modules. Any test
/// that reads a green `compile_package` as evidence the modules were fine has to
/// establish first that there were modules. (A root that doesn't exist is a
/// different case: `load_package_sources` reports that as an error — see
/// `BUG-21` in `docs/tickets/README.md`.)
fn module_names(root: &Path) -> Vec<String> {
    let sources = load_package_sources(root)
        .unwrap_or_else(|e| panic!("failed to load sources from {:?}: {:?}", root, e));
    let mut names: Vec<String> = sources
        .iter()
        .map(|(_, file)| file.file().name().clone())
        .collect();
    names.sort();
    names
}

/// The phase error inside a `compile_package` result, past the file it was tagged with.
///
/// Errors that come back from `check_in_order` are wrapped in
/// `CompilationError::InFile` by `compile_package`, which is what pairs the spans a
/// phase produced with the file to underline. A test that asserts on the phase
/// variant looks through that wrapper rather than at it.
fn unwrap_in_file(error: &CompilationError) -> &CompilationError {
    match error {
        CompilationError::InFile(inner, _) => unwrap_in_file(inner),
        other => other,
    }
}

// ── Test 1: Minimal passing module ───────────────────────────────────────────

#[test]
fn minimal_passing_module() {
    let source = indoc::indoc! {r#"
        module Test exposing ()
        x = 42
    "#};
    let parsed = parse_source(source);
    let interfaces = HashMap::new();
    let result = check_module(&test_package(), &interfaces, &parsed, false);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

// ── Test 2: Module with multiple values ──────────────────────────────────────

#[test]
fn module_with_typed_and_untyped_values() {
    let source = indoc::indoc! {r#"
        module Test exposing (identity, add)
        answer = 42
        identity : a -> a
        identity x = x
        add : Int -> Int -> Int
        add a b = a
    "#};
    let parsed = parse_source(source);
    let interfaces = HashMap::from([basics_interface()]);
    let result = check_module(&test_package(), &interfaces, &parsed, false);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
    let module = result.unwrap();
    assert_eq!(module.values.len(), 3);
}

// ── Test 3: Module with union type ───────────────────────────────────────────

#[test]
fn module_with_union_type() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        type Shape = Circle | Square | Triangle
        count : Int
        count = 42
    "#};
    let parsed = parse_source(source);
    let interfaces = HashMap::from([basics_interface()]);
    let result = check_module(&test_package(), &interfaces, &parsed, false);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
    let module = result.unwrap();
    assert!(module.types.contains_key(&"Shape".into()));
}

// ── Test 4: Module importing Maybe (using manually-built interface) ───────────

#[test]
fn module_importing_maybe_interface() {
    let (iface_name, iface) = maybe_interface();
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    interfaces.insert(iface_name, iface);

    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import Maybe exposing (Maybe(..))
        wrap : a -> Maybe a
        wrap x = Just x
    "#};
    let parsed = parse_source(source);
    let result = check_module(&test_package(), &interfaces, &parsed, false);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
    let module = result.unwrap();
    assert!(module.values.contains_key(&"wrap".into()));
}

// ── Test 5: check_module produces a valid interface usable by dependents ─────

#[test]
fn check_module_interface_can_be_used_by_dependent() {
    let pkg = test_package();
    // `App` annotates an `Option Int`, so `Basics` has to be resolvable for the
    // `Int` inside it as much as `Lib` does for the `Option` around it.
    let mut interfaces: HashMap<Name, Interface> = HashMap::from([basics_interface()]);

    // First module: defines a local Maybe
    let source_a = indoc::indoc! {r#"
        module Lib exposing (..)
        type Option a = Some a | None
        wrap : a -> Option a
        wrap x = Some x
    "#};
    let parsed_a = parse_source(source_a);
    let module_a = check_module(&pkg, &interfaces, &parsed_a, false).expect("Lib should compile");
    interfaces.insert(module_a.name.name().clone(), module_a.to_interface(None));

    // Second module: imports and uses Lib
    let source_b = indoc::indoc! {r#"
        module App exposing (..)
        import Lib exposing (Option(..))
        answer : Option Int
        answer = Some 42
    "#};
    let parsed_b = parse_source(source_b);
    let result_b = check_module(&pkg, &interfaces, &parsed_b, false);
    assert!(result_b.is_ok(), "App should compile, got {:?}", result_b);
}

// ── Test 6: check_module fails on canonicalization error ─────────────────────

#[test]
fn check_module_fails_on_missing_import() {
    // This module imports a module that isn't in the interfaces map.
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        import NonExistent exposing (..)
        x = 42
    "#};
    let parsed = parse_source(source);
    let interfaces = HashMap::new();
    let result = check_module(&test_package(), &interfaces, &parsed, false);
    assert!(result.is_err(), "expected Err for missing import, got Ok");
}

// ── Test 7: Tuple.zel from the standard library ──────────────────────────────

#[test]
fn stdlib_tuple_compiles() {
    let path = std_src().join("Tuple.zel");
    if !path.exists() {
        eprintln!("Skipping: {:?} not found", path);
        return;
    }
    let parsed = parse_file(&path);
    let interfaces = HashMap::new();
    let result = check_module(&std_package(), &interfaces, &parsed, true);
    assert!(result.is_ok(), "Tuple.zel should compile, got {:?}", result);
}

// ── Test 8: Standard library Js binding modules + Basics ─────────────────────

#[test]
fn stdlib_basics_chain_compiles() {
    let src = std_src();
    let pkg = std_package();
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();

    // The Js binding modules have no imports of their own — process them first.
    for js_module in &["Js/Basics.zel", "Js/Utils.zel"] {
        let path = src.join(js_module);
        if !path.exists() {
            eprintln!("Skipping stdlib chain: {:?} not found", path);
            return;
        }
        let parsed = parse_file(&path);
        let module = check_module(&pkg, &interfaces, &parsed, true)
            .unwrap_or_else(|e| panic!("{} failed: {:?}", js_module, e));
        interfaces.insert(module.name.name().clone(), module.to_interface(None));
    }

    // Basics depends on Js.Basics and Js.Utils
    let basics_path = src.join("Basics.zel");
    if !basics_path.exists() {
        eprintln!("Skipping: Basics.zel not found");
        return;
    }
    let parsed_basics = parse_file(&basics_path);
    let basics_module = check_module(&pkg, &interfaces, &parsed_basics, true)
        .unwrap_or_else(|e| panic!("Basics.zel failed: {:?}", e));
    interfaces.insert(
        basics_module.name.name().clone(),
        basics_module.to_interface(None),
    );

    // Maybe depends on Basics
    let maybe_path = src.join("Maybe.zel");
    if !maybe_path.exists() {
        eprintln!("Skipping: Maybe.zel not found");
        return;
    }
    let parsed_maybe = parse_file(&maybe_path);
    let maybe_module = check_module(&pkg, &interfaces, &parsed_maybe, true)
        .unwrap_or_else(|e| panic!("Maybe.zel failed: {:?}", e));
    interfaces.insert(
        maybe_module.name.name().clone(),
        maybe_module.to_interface(None),
    );

    // Result depends on Basics and Maybe
    let result_path = src.join("Result.zel");
    if !result_path.exists() {
        eprintln!("Skipping: Result.zel not found");
        return;
    }
    let parsed_result = parse_file(&result_path);
    let result_module = check_module(&pkg, &interfaces, &parsed_result, true)
        .unwrap_or_else(|e| panic!("Result.zel failed: {:?}", e));
    interfaces.insert(
        result_module.name.name().clone(),
        result_module.to_interface(None),
    );

    // At this point we've successfully compiled the core stdlib chain.
    // Verify Basics, Maybe, and Result are all in the interface map.
    assert!(interfaces.contains_key(&"Basics".into()));
    assert!(interfaces.contains_key(&"Maybe".into()));
    assert!(interfaces.contains_key(&"Result".into()));
}

// ── Test 9: compile_package reports success only when it compiled ────────────

/// A package whose every module checks must be reported as a success.
///
/// This is the half of `BUG-1` that keeps the fix from over-reaching: it is easy
/// to make a compiler fail, and this pins that `compile_package` still returns
/// `Ok(())` when nothing went wrong. Mutation-checked by making the tail of
/// `compile_package` return `Err(CompilationError::Many(errors))`
/// unconditionally, which turns this test red.
#[test]
fn compile_package_succeeds_when_every_module_checks() {
    let result = compile_package(&fixture_package("package_checks"));

    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

// ── Test 10: compile_package reports failure when a module fails ─────────────

/// `BUG-1`: a package with a module that fails to canonicalize must fail.
///
/// Before the fix `compile_package` rendered the diagnostics to stderr and then
/// returned `Ok(())` regardless of how many there were, so a package that did
/// not compile was indistinguishable from one that did. Mutation-checked by
/// restoring the unconditional `Ok(())` at the end of `compile_package`, which
/// turns this test red.
///
/// The assertion goes down to the variant on purpose: the point of the change is
/// that the accumulated, still-typed errors survive to the return value, so
/// `is_err()` alone would pass against an `Err` carrying nothing useful.
///
/// The `InFile` unwrapping is `ERR-3`: `compile_package` pairs each check error with
/// the `SourceFileId` of the file its module was read from, so the labels the phase
/// produced have a file to point into. The phase error underneath is unchanged.
#[test]
fn compile_package_fails_when_a_module_fails_to_canonicalize() {
    let root = fixture_package("package_canonicalize_fails");

    // The fixture deliberately holds a second, *passing* module: it is what
    // `BUG-2` (see `docs/tickets/README.md`) was about — the modules that checked
    // being discarded when a sibling fails — and this directory is its
    // reproduction. Nothing else asserts `Fine.zel` exists, so pin it here —
    // silently losing it would leave that regression with a repro that proves
    // nothing. It does not change what this test checks: one broken module is
    // still exactly one error.
    assert_eq!(
        module_names(&root.join("src")),
        vec!["Broken.zel", "Fine.zel"]
    );

    let result = compile_package(&root);

    match result {
        Err(CompilationError::Many(errors)) => {
            assert_eq!(
                errors.len(),
                1,
                "expected exactly one error for the one broken module, got {:?}",
                errors
            );
            match unwrap_in_file(&errors[0]) {
                CompilationError::Canonical(canonical_errors, module) => {
                    assert_eq!(module, &Name::from("Broken"));
                    assert!(
                        !canonical_errors.is_empty(),
                        "expected the canonical errors to be carried through"
                    );
                }
                other => panic!("expected a Canonical error, got {:?}", other),
            }
        }
        other => panic!("expected Err(CompilationError::Many(..)), got {:?}", other),
    }
}

// ── Test 11: a failing module does not discard its passing siblings ──────────

/// `BUG-2`: `check_in_order` must hand back the modules that checked *and* the
/// errors from the ones that didn't.
///
/// `tests/fixtures/package_canonicalize_fails` is the ticket's scenario: `Broken.zel`
/// imports a module that does not exist, `Fine.zel` checks cleanly. Before the fix
/// every success was discarded as soon as one module failed, which is why
/// `compile_package` had no list of checked modules to report — the user-visible half
/// of the ticket's Acceptance.
///
/// This drives the *real* `check_module`, which is what the `dummy_check` unit test in
/// `src/compiler/dependencies.rs` cannot do. Mutation-checked by clearing `modules` at
/// the end of `check_in_order` whenever `errors` is non-empty: that turns the `Fine`
/// assertion below red.
#[test]
fn check_in_order_keeps_passing_siblings_with_the_real_checker() {
    let root = fixture_package("package_canonicalize_fails");
    let sources = load_package_sources(&root.join("src"))
        .unwrap_or_else(|e| panic!("failed to load sources from {:?}: {:?}", root, e));

    let modules: Vec<parser::Module> = sources
        .iter()
        .map(|(_, file)| {
            parser::parse(file.file())
                .unwrap_or_else(|e| panic!("parse error in {:?}: {:?}", file.file().name(), e))
        })
        .collect();
    // Same reasoning as `module_names`: an empty fixture would make every assertion
    // below vacuous, so establish there are two modules before checking them.
    assert_eq!(
        modules.len(),
        2,
        "fixture should hold Broken.zel and Fine.zel"
    );

    let module_files = HashMap::new();
    let walker =
        ModuleWalker::new(&modules, &module_files).expect("no dependency cycle in the fixture");
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let (checked, errors) =
        walker.check_in_order(&std_package(), &mut interfaces, &module_files, check_module);

    let checked_names: Vec<String> = checked
        .iter()
        .map(|m| m.name.name().as_str().to_string())
        .collect();
    assert_eq!(
        checked_names,
        vec!["Fine".to_string()],
        "the module that checks must survive its broken sibling"
    );

    // The error half, asserted down to the variant so that "an error was reported"
    // cannot be satisfied by an error about the wrong module.
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);
    match &errors[0] {
        CompilationError::Canonical(canonical_errors, module) => {
            assert_eq!(module, &Name::from("Broken"));
            assert!(
                !canonical_errors.is_empty(),
                "expected the canonical errors to be carried through"
            );
        }
        other => panic!("expected a Canonical error for Broken, got {:?}", other),
    }

    // Reporting the survivors must not turn the package green: the errors still flow
    // into `compile_package`'s accumulator, so the package as a whole still fails.
    assert!(
        compile_package(&root).is_err(),
        "a package with a broken module must still fail overall"
    );
}

// ── Test 12: Bitwise checks against the Js.Bitwise facade ────────────────────

/// `Bitwise.zel` resolves its primitives through `Js.Bitwise`, not Elm's kernel.
///
/// `Bitwise` was carried over from Elm's `core` unchanged, so it imported
/// `Elm.Kernel.Bitwise` — a module Zelkova has no equivalent of — and failed
/// canonicalization with `InterfaceNotFound` on every run. Mutation-checked by
/// pointing the import in `std/core/src/Bitwise.zel` back at
/// `Elm.Kernel.Bitwise`, which turns this test red on the `Bitwise` step.
///
/// Like the other stdlib tests here it skips rather than fails when a file is
/// missing, so deleting `Js/Bitwise.zel` outright would not be caught here —
/// `stdlib_package_compiles` is what covers that.
#[test]
fn stdlib_bitwise_compiles() {
    let src = std_src();
    let pkg = std_package();
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();

    // Bitwise needs the facade it binds to, and `Basics` for `Int`. `Basics` in
    // turn needs its own two Js modules.
    for module in &[
        "Js/Basics.zel",
        "Js/Utils.zel",
        "Js/Bitwise.zel",
        "Basics.zel",
        "Bitwise.zel",
    ] {
        let path = src.join(module);
        if !path.exists() {
            eprintln!("Skipping stdlib Bitwise chain: {:?} not found", path);
            return;
        }
        let parsed = parse_file(&path);
        let checked = check_module(&pkg, &interfaces, &parsed, true)
            .unwrap_or_else(|e| panic!("{} failed: {:?}", module, e));
        interfaces.insert(checked.name.name().clone(), checked.to_interface(None));
    }

    assert!(interfaces.contains_key(&"Js.Bitwise".into()));
    assert!(interfaces.contains_key(&"Bitwise".into()));
}

// ── Test 13: the standard library is a package that compiles ─────────────────

/// `std/core` — what `cargo run` compiles — must compile cleanly.
///
/// This is the smoke test as an assertion. Until `Bitwise.zel` stopped importing
/// `Elm.Kernel.Bitwise` the standard library was a package that always failed, so
/// `cargo run` said "fail" on a healthy tree and told you nothing. Mutation-checked
/// the same way as `stdlib_bitwise_compiles`.
///
/// The `.ignored` modules under `std/core/src` are invisible to the source loader,
/// which only collects `.zel`, so this covers exactly the modules `cargo run` does.
///
/// The module list is asserted before compiling, and that is not decoration: an
/// *existing* but empty `std/core/src` would still yield zero modules and a
/// green `compile_package`, so `is_ok()` on its own would pass on a tree with no
/// standard library at all. Adding a `.zel` module to the package is expected to
/// fail this list; extend it, do not weaken it.
#[test]
fn stdlib_package_compiles() {
    let src = std_src();

    assert_eq!(
        module_names(&src),
        vec![
            "Basics.zel",
            "Bitwise.zel",
            "Js/Basics.zel",
            "Js/Bitwise.zel",
            "Js/Utils.zel",
            "Maybe.zel",
            "Result.zel",
            "Tuple.zel",
        ]
    );

    let result = compile_package(&std_package_root());

    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

// ── Test 14: a type error reaches the user as a real diagnostic ──────────────

/// `ERR-2`: a type error must render as an `error` naming both types.
///
/// This is the whole point of the ticket. `From<typer::Error> for CompilationError`
/// used to return `CompilationError::PlaceHolder`, which discarded the typer error
/// and rendered as `Diagnostic::bug()` with the message "A non implemented error
/// message have been emitted" — so every type error in the language reached the user
/// as the same sentence, naming nothing. The assertions below are therefore on the
/// *rendered* diagnostic, not on `is_err()`: which error is raised, and what it says,
/// is the behaviour that changed.
///
/// `!message.contains("TypeMismatch")` is not redundant with the two `contains`
/// above it: `format!("{:?}", e)` on the same error also contains "Int" and "Bool".
/// It is what tells a real message from the `Debug` dump the other phases used to
/// emit.
///
/// Mutation-checked three ways, each of which turns it red on its own: replacing the
/// `Type` arm of `as_diagnostic` with the old `Debug`-dump-in-a-note rendering; making
/// `phase_diagnostic` build `Diagnostic::warning()`; and dropping the `expected`/
/// `actual` types out of `typer::Error::message`.
#[test]
fn type_error_renders_as_an_error_naming_both_types() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
        answer : Int
        answer = true
    "#};
    let parsed = parse_source(source);
    let interfaces = HashMap::from([basics_interface()]);

    let error = check_module(&test_package(), &interfaces, &parsed, false)
        .expect_err("`answer : Int` with a `Bool` body must not type-check");

    // The phase is part of the contract: a type error must not be reported as, say,
    // a canonicalization failure that happened to mention the same names.
    match &error {
        CompilationError::Type(errors, module) => {
            assert_eq!(module, &Name::from("Test"));
            assert_eq!(errors.len(), 1, "expected one type error, got {:?}", errors);
        }
        other => panic!("expected a Type error, got {:?}", other),
    }

    let diagnostic = error.as_diagnostic();

    assert_eq!(diagnostic.severity, Severity::Error);

    let message = &diagnostic.message;
    assert!(
        message.contains("Int"),
        "the annotated type should be named, got {:?}",
        message
    );
    assert!(
        message.contains("Bool"),
        "the inferred type should be named, got {:?}",
        message
    );
    assert!(
        !message.contains("TypeMismatch"),
        "the message should be prose, not a Debug dump, got {:?}",
        message
    );
}

// ── Test 15: every phase error renders as prose, not a Debug dump ────────────

/// `ERR-2`: the canonical arm of `as_diagnostic` used to say "Canonical error
/// messages are not implemented yet" and put `format!("{:?}", e)` in a note.
///
/// `package_canonicalize_fails` is the existing fixture for a module that fails to
/// canonicalize (`Broken.zel` imports a module that does not exist), so this asserts
/// on the same failure the two tests above already produce — only on what it *says*.
///
/// Mutation-checked by restoring that message and the `{:?}` note in the `Canonical`
/// arm: `NonExistent` then appears only inside the `Debug` dump in a note, so the
/// message assertion goes red.
#[test]
fn canonical_error_renders_as_prose_naming_the_missing_module() {
    let root = fixture_package("package_canonicalize_fails");

    let error = compile_package(&root).expect_err("the fixture must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    let message = &errors[0].as_diagnostic().message;
    assert!(
        message.contains("Broken"),
        "the failing module should be named, got {:?}",
        message
    );
    assert!(
        message.contains("NonExistent"),
        "the module that could not be found should be named, got {:?}",
        message
    );
    assert!(
        !message.contains("not implemented yet"),
        "the message should describe the failure, got {:?}",
        message
    );
}

// ── Test 16: a type error underlines the expression that disagrees ──────────

/// `ERR-4`: a type error points at the sub-expression, with the annotation behind it.
///
/// `ERR-3` landed this test asserting a single label across the whole declaration —
/// annotation and body together — because that was the finest thing the typer could
/// name. It can do better now: the caret is under `true`, and a *secondary* label
/// under `answer : Int` says where `Int` was expected from. Both ranges are computed
/// from the fixture text, and both matter: a primary label that had widened back out
/// to the declaration would still be "a label", and a missing secondary would leave
/// the reader to guess why `Int` was expected at all.
///
/// Mutation-checked four ways, each red on its own: making `canonical_expr_to_term`
/// build its terms with `NodeSpan::none()` (the primary falls back to the whole
/// declaration); dropping `annotation_span` from `Value::TypedValue` in favour of
/// `NodeSpan::none()` (the secondary disappears); pushing the annotation constraint
/// *after* `constraint::collect` in `infer_annotated` (the primary moves off `true`);
/// and having `Substitution::apply` return `c.origin.clone()` unchanged, so nothing
/// is ever explained (the secondary disappears).
#[test]
fn type_error_labels_the_expression_that_disagrees() {
    let root = fixture_package("package_type_error");
    assert_eq!(
        module_names(&root.join("src")),
        vec!["Basics.zel", "Mismatch.zel"]
    );

    let source = std::fs::read_to_string(root.join("src").join("Mismatch.zel"))
        .expect("fixture is readable");
    let annotation = "answer : Int";
    let annotation_start = source
        .find(annotation)
        .expect("fixture declares `answer : Int`");
    let body = "true";
    let body_start = source
        .rfind(body)
        .expect("fixture's body is the literal `true`");

    let error =
        compile_package(&root).expect_err("`answer : Int` with a `Bool` body must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    // The phase is part of the contract: this must be the type error, not a
    // canonicalization failure that happened to land on the same line.
    match unwrap_in_file(&errors[0]) {
        CompilationError::Type(type_errors, module) => {
            assert_eq!(module, &Name::from("Mismatch"));
            assert_eq!(type_errors.len(), 1, "got {:?}", type_errors);
        }
        other => panic!("expected a Type error, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    assert_eq!(
        diagnostic.labels.len(),
        2,
        "expected a primary and a secondary label, got {:?}",
        diagnostic.labels
    );
    assert_eq!(diagnostic.labels[0].style, LabelStyle::Primary);
    assert_eq!(
        diagnostic.labels[0].range,
        body_start..(body_start + body.len()),
        "the caret must be under the body that disagrees, not across the declaration"
    );
    assert_eq!(diagnostic.labels[1].style, LabelStyle::Secondary);
    assert_eq!(
        diagnostic.labels[1].range,
        annotation_start..(annotation_start + annotation.len()),
        "the annotation must be underlined as the reason `Int` was expected"
    );
}

// ── Test 17: a canonicalization error underlines the import that failed ──────

/// `ERR-3`: an unresolvable `import` renders with a caret under the `import` line.
///
/// The type error above only exercises the path through `typer::Error`, where the
/// span is attached one level up in `type_check`. This is the other shape: a
/// `canonical::Error` whose span was carried on the AST node itself, from
/// `parser::Import` through `new_environment` to `EnvError::InterfaceNotFound`.
///
/// `package_canonicalize_fails` is the existing fixture — `Broken.zel` imports a
/// module that does not exist — so this asserts on a failure two other tests here
/// already produce, only on where it points.
///
/// Mutation-checked two ways: making the `Import` production emit `NodeSpan::none()`,
/// and making `EnvError::labels` return `Vec::new()`. Either empties `labels`.
#[test]
fn missing_import_labels_the_import_line() {
    let root = fixture_package("package_canonicalize_fails");

    let source =
        std::fs::read_to_string(root.join("src").join("Broken.zel")).expect("fixture is readable");
    let line = "import NonExistent exposing (..)";
    let start = source.find(line).expect("fixture imports NonExistent");

    let error = compile_package(&root).expect_err("the fixture must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    let diagnostic = errors[0].as_diagnostic();

    assert_eq!(
        diagnostic.labels.len(),
        1,
        "expected one label, got {:?}",
        diagnostic.labels
    );
    assert_eq!(diagnostic.labels[0].range, start..(start + line.len()));
}

// ── Test 18: a caret under the identifier, not the declaration ───────────────

/// `ERR-3`, commit 2: an unknown *variable* is underlined where the name was
/// written, not across the whole declaration it sits in.
///
/// Commit 1 gave the five declaration productions a span, so a diagnostic could
/// already point at `answer = mystery` in its entirety. This asserts the narrower
/// thing that expression spans buy: the range is `mystery` alone. Asserting the
/// range rather than `!labels.is_empty()` is the whole difference — the declaration
/// span would satisfy a non-emptiness check just as well.
///
/// Mutation-checked two ways, each red on its own: making the `AtomicExpr`
/// `QualVarIdent` production emit `NodeSpan::none()` (the label disappears, since
/// `Expression::from_parser` has nothing to attach), and dropping the span from
/// `canonical::Error::VariableNotFound`'s `labels` arm.
#[test]
fn unknown_variable_labels_the_identifier() {
    let root = fixture_package("package_unknown_variable");
    assert_eq!(module_names(&root.join("src")), vec!["Unknown.zel"]);

    let source =
        std::fs::read_to_string(root.join("src").join("Unknown.zel")).expect("fixture is readable");
    let identifier = "mystery";
    let start = source
        .find(identifier)
        .expect("fixture uses an undefined `mystery`");

    let error = compile_package(&root).expect_err("an undefined name must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(&errors[0]) {
        CompilationError::Canonical(canonical_errors, module) => {
            assert_eq!(module, &Name::from("Unknown"));
            assert_eq!(canonical_errors.len(), 1, "got {:?}", canonical_errors);
        }
        other => panic!("expected a Canonical error, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    assert_eq!(
        diagnostic.labels.len(),
        1,
        "expected one label, got {:?}",
        diagnostic.labels
    );
    assert_eq!(
        diagnostic.labels[0].range,
        start..(start + identifier.len()),
        "the caret must sit under `{}` alone, not the declaration around it",
        identifier
    );
}

// ── Test 19: the same, for a constructor in a pattern ────────────────────────

/// `ERR-3`, commit 2: an unknown *constructor* in a pattern is underlined where the
/// name was written.
///
/// The variable case above goes through `Expression::from_parser`; this is the other
/// conversion, `Pattern::from_parser`, and the other grammar site — `DeclPattern`,
/// which spans a bare constructor used as a function argument. Taking it from a
/// binding pattern rather than a `case` branch keeps this error out of
/// `Error::Many`, so it pins the pattern span on its own; the grouping is
/// `grouped_canonical_error_keeps_every_label` below.
///
/// Mutation-checked two ways, each red on its own: making the `DeclPattern`
/// `QualTypeIdent` production emit `NodeSpan::none()`, and dropping the span from
/// `canonical::Error::VariantNotFound`'s `labels` arm.
#[test]
fn unknown_constructor_labels_the_pattern() {
    let root = fixture_package("package_unknown_constructor");
    assert_eq!(module_names(&root.join("src")), vec!["Ctor.zel"]);

    let source =
        std::fs::read_to_string(root.join("src").join("Ctor.zel")).expect("fixture is readable");
    let constructor = "Purple";
    let start = source
        .find(constructor)
        .expect("fixture matches on an undeclared `Purple`");

    let error = compile_package(&root).expect_err("an undeclared constructor must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    let diagnostic = errors[0].as_diagnostic();

    assert_eq!(
        diagnostic.labels.len(),
        1,
        "expected one label, got {:?}",
        diagnostic.labels
    );
    assert_eq!(
        diagnostic.labels[0].range,
        start..(start + constructor.len()),
        "the caret must sit under `{}` alone",
        constructor
    );
}

// ── Test 20: a grouped error keeps the labels of everything it swallowed ─────

/// `ERR-3`, commit 2: `canonical::Error::Many` flattens its members' labels.
///
/// The two case branches in the fixture each name an undeclared constructor, and
/// `Expression::from_parser` collects both through `collect_accumulate`, so what
/// reaches the reporter is a *single* `Error::Many` holding two `VariantNotFound`s.
/// `Many` has no position of its own, so if it did not flatten it would render as a
/// summary with no caret at all and both carets would vanish silently — the failure
/// mode is invisible, which is why this is asserted rather than assumed.
///
/// Mutation-checked by replacing the `Error::Many` arm of `canonical::Error::labels`
/// with `Vec::new()`: the diagnostic keeps its message and its notes and loses both
/// labels.
#[test]
fn grouped_canonical_error_keeps_every_label() {
    let root = fixture_package("package_two_unknown_constructors");
    assert_eq!(module_names(&root.join("src")), vec!["Grouped.zel"]);

    let source =
        std::fs::read_to_string(root.join("src").join("Grouped.zel")).expect("fixture is readable");
    let ranges: Vec<_> = ["Purple", "Crimson"]
        .iter()
        .map(|ctor| {
            let start = source.find(ctor).unwrap_or_else(|| {
                panic!("fixture matches on an undeclared `{}`", ctor);
            });
            start..(start + ctor.len())
        })
        .collect();

    let error = compile_package(&root).expect_err("two undeclared constructors must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    // One phase error — the group — carrying two members.
    match unwrap_in_file(&errors[0]) {
        CompilationError::Canonical(canonical_errors, _) => {
            assert_eq!(
                canonical_errors.len(),
                1,
                "the two failures should arrive as one group, got {:?}",
                canonical_errors
            );
        }
        other => panic!("expected a Canonical error, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    let rendered: Vec<_> = diagnostic.labels.iter().map(|l| l.range.clone()).collect();
    assert_eq!(
        rendered, ranges,
        "the group must carry a caret for each constructor it swallowed"
    );
}

// ── Test 21: the span of a case-bodied declaration stops at the case ────────

/// `ERR-3`: a `case` body must not push the declaration's span past its own end.
///
/// Every other span test here has a one-line body, which is exactly the shape that
/// hides this: `Expr`'s `case` alternative finishes by consuming a layout
/// `CloseBlock`, and the layout pass positions an implicitly-closed block *at the
/// token that closed it* — the first token of the next declaration, or `EndOfFile`
/// (whose `BytePos` is 0) at end of file. An `@R` taken after such a nonterminal
/// therefore produced `26..66` here — a caret running into `other` — and inverted
/// spans like `26..0` for a case at the end of the file. `NodeSpan::to_end_of`
/// reads the end off the node instead, and that is what this pins.
///
/// The fixture deliberately puts a second declaration *after* the case-bodied one,
/// so an end taken one token too far is visible as a range that overruns rather than
/// as a range that merely ends late.
///
/// It is asserted on `typer::Error::span` — the declaration the error was found in —
/// rather than on the rendered label, because `ERR-4` narrowed the label to the
/// sub-expression that disagrees (`1`, in the first branch). That span is no longer
/// what is drawn in the common case, but it is still what a type error falls back to
/// when its constraint has no position of its own, and it is still built by merging
/// the declaration's parts. The labels are checked here too, for the branch shape the
/// test above does not cover.
///
/// Mutation-checked by restoring the old shape — `<r:@R>` after `<expr:Expr>` in
/// `FunBinding`, with `NodeSpan::new(l, r)`: the declaration span then reaches into
/// `other` and the first assertion below fails.
#[test]
fn case_bodied_declaration_label_stops_at_the_case() {
    let root = fixture_package("package_case_type_error");
    assert_eq!(module_names(&root.join("src")), vec!["CaseBody.zel"]);

    let source = std::fs::read_to_string(root.join("src").join("CaseBody.zel"))
        .expect("fixture is readable");
    let annotation = "classify : Color -> Color";
    let annotation_start = source
        .find(annotation)
        .expect("fixture declares `classify`");
    let start = annotation_start;
    let last = "Blue -> 2";
    let end = source.find(last).expect("fixture has a second branch") + last.len();

    let error = compile_package(&root)
        .expect_err("`classify : Color -> Color` returning an `Int` must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(&errors[0]) {
        CompilationError::Type(type_errors, module) => {
            assert_eq!(module, &Name::from("CaseBody"));
            assert_eq!(type_errors.len(), 1, "got {:?}", type_errors);

            let declaration = type_errors[0]
                .span
                .to_range()
                .expect("the declaration was parsed from source");
            assert_eq!(
                declaration,
                start..end,
                "the declaration's span must stop at the last branch, not run into the \
                 declaration after it"
            );
            assert!(
                !source[declaration].contains("other"),
                "the declaration's span must not reach the following declaration"
            );
        }
        other => panic!("expected a Type error, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    // `Red -> 1` is the first branch to contradict the `Color` result type, and
    // constraints are solved in source order, so it is the one reported.
    let branch = source.find("Red -> 1").expect("fixture has a first branch") + "Red -> ".len();
    assert_eq!(
        diagnostic.labels.len(),
        2,
        "expected a primary and a secondary label, got {:?}",
        diagnostic.labels
    );
    assert_eq!(diagnostic.labels[0].range, branch..(branch + 1));
    assert_eq!(
        diagnostic.labels[1].range,
        annotation_start..(annotation_start + annotation.len())
    );
}

// ── Test 22: an ambiguous import points at each defining module ─────────────

/// `ERR-5`: a diagnostic can carry labels in more than one file.
///
/// `Main.zel` imports `foo` unqualified from both `A.zel` and `B.zel`, so
/// `canonical::Error::AmbiguousVariables` fires while checking `Main` — but the
/// two declarations it is ambiguous *between* were written in `A` and `B`, not in
/// `Main`. Before `ERR-5` a `SpanLabel` had no file of its own and `Interface`
/// carried `canonical::Type` with no position at all (see that type's own
/// documentation for why), so there was nothing to build such a label from.
/// `Interface::file`, filled in by `ModuleWalker::check_in_order`, plus
/// `Interface::values` now carrying each value's declaration span, are what let
/// `AmbiguousVariables::labels` build one secondary label per candidate in that
/// candidate's *own* file.
///
/// This is the ticket's acceptance check verbatim: not just that a second label
/// exists, but that the two secondary labels' `file_id`s actually differ from
/// each other and from the primary label's.
///
/// Mutation-checked by making `Interface::source_span` always return `None` —
/// the state before `Interface::file` was threaded through `check_in_order`.
/// `is_err()` alone would not catch it: `AmbiguousVariables` still fires and the
/// primary label still renders, so only the `labels.len() == 3` assertion below
/// goes red.
#[test]
fn ambiguous_import_labels_point_into_each_defining_module() {
    let root = fixture_package("package_ambiguous_import");
    assert_eq!(
        module_names(&root.join("src")),
        vec!["A.zel", "B.zel", "Main.zel"]
    );

    let a_source =
        std::fs::read_to_string(root.join("src").join("A.zel")).expect("fixture is readable");
    let b_source =
        std::fs::read_to_string(root.join("src").join("B.zel")).expect("fixture is readable");
    let a_start = a_source.find("foo : LabelA").expect("A declares foo");
    let a_end = a_source.find("foo = LabelA").expect("A defines foo") + "foo = LabelA".len();
    let b_start = b_source.find("foo : LabelB").expect("B declares foo");
    let b_end = b_source.find("foo = LabelB").expect("B defines foo") + "foo = LabelB".len();

    let error = compile_package(&root).expect_err("an ambiguous import must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(&errors[0]) {
        CompilationError::Canonical(canonical_errors, module) => {
            assert_eq!(module, &Name::from("Main"));
            assert_eq!(canonical_errors.len(), 1, "got {:?}", canonical_errors);
        }
        other => panic!("expected a Canonical error, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    assert_eq!(
        diagnostic.labels.len(),
        3,
        "expected one primary label in Main plus one secondary label per candidate, got {:?}",
        diagnostic.labels
    );

    let secondary: Vec<_> = diagnostic
        .labels
        .iter()
        .filter(|l| l.style == codespan_reporting::diagnostic::LabelStyle::Secondary)
        .collect();
    assert_eq!(
        secondary.len(),
        2,
        "expected two secondary labels, one per candidate module, got {:?}",
        secondary
    );

    let primary_label = diagnostic
        .labels
        .iter()
        .find(|l| l.style == codespan_reporting::diagnostic::LabelStyle::Primary)
        .expect("expected a primary label at the use site");

    // The ticket's acceptance check, verbatim: the two secondary labels sit in two
    // different files, and neither is the file the primary label is in.
    assert_ne!(
        secondary[0].file_id, secondary[1].file_id,
        "the two candidates must be labeled in their own, different files"
    );
    assert_ne!(
        secondary[0].file_id, primary_label.file_id,
        "a candidate's label must not be in the same file as the use site"
    );
    assert_ne!(
        secondary[1].file_id, primary_label.file_id,
        "a candidate's label must not be in the same file as the use site"
    );

    // And each secondary label underlines the candidate's actual declaration, not
    // a zero-width guess or the other candidate's span.
    let ranges: Vec<_> = secondary.iter().map(|l| l.range.clone()).collect();
    assert!(
        ranges.contains(&(a_start..a_end)),
        "expected a label at A's declaration {:?}, got {:?}",
        a_start..a_end,
        ranges
    );
    assert!(
        ranges.contains(&(b_start..b_end)),
        "expected a label at B's declaration {:?}, got {:?}",
        b_start..b_end,
        ranges
    );
}

// ── Test 22a: a default import is named as implicit in the ambiguity note ───

/// A stand-in `Helper` interface exposing a polymorphic `add`, so it collides
/// with `Basics.add` on name alone and not on type.
fn helper_interface() -> (Name, Interface) {
    use zelkova_lang::compiler::position::NodeSpan;

    let mut values = HashMap::new();
    values.insert(
        "add".into(),
        (
            NodeSpan::none(),
            canonical::Type::Arrow(
                Box::new(canonical::Type::Variable("a".into())),
                Box::new(canonical::Type::Variable("a".into())),
            ),
        ),
    );

    let interface = Interface {
        module_name: zelkova_lang::compiler::ModuleName::new(
            PackageName::new("zelkova-core").unwrap(),
            "Helper".into(),
        ),
        values,
        unions: HashMap::new(),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        file: None,
    };

    ("Helper".into(), interface)
}

/// `SPEC-32`: colliding with a *default* import is `AmbiguousVariables`, the
/// same as colliding with two written ones — `docs/spec/modules.md`'s *The
/// default imports* section says a default entry participates in ambiguity
/// exactly as a written import does. What is worth pinning at this layer is
/// the note: a reader of `Main` never wrote `import Basics`, so the note names
/// it differently from `Helper`, which `Main` did write.
///
/// `Basics` and `Helper` both expose `add`; `Main` writes only `import Helper
/// exposing (add)`, so the `Basics` half of the collision is supplied by the
/// default import list rather than written — the ticket's reproduction, with
/// both interfaces hand-built (`basics_interface_with_plus`, `helper_interface`) rather
/// than real sibling modules: a real `Basics.zel` in `Main`'s own package would
/// make that package the one the eight belong to (`LANG-57`), which gets none
/// of them and could never reproduce this collision in the first place.
///
/// Mutation-checked by reverting `ambiguous_note` to the old unconditional
/// `"it is exposed by: {}".join(", ")` over every candidate's name: the
/// assertion below goes red because the note no longer says "implicitly".
#[test]
fn ambiguous_variable_note_calls_out_the_implicit_default_import() {
    let mut interfaces = HashMap::new();
    let (basics_name, basics_iface) = basics_interface_with_plus();
    interfaces.insert(basics_name, basics_iface);
    let (helper_name, helper_iface) = helper_interface();
    interfaces.insert(helper_name, helper_iface);

    let source = indoc::indoc! {r#"
        module Main exposing (x)
        import Helper exposing (add)
        x : Int
        x = add 1 2
    "#};

    let error = check_module(&test_package(), &interfaces, &parse_source(source), false)
        .expect_err("a name colliding with a default import must not compile");

    match &error {
        CompilationError::Canonical(canonical_errors, module) => {
            assert_eq!(module, &Name::from("Main"));
            assert_eq!(canonical_errors.len(), 1, "got {:?}", canonical_errors);
        }
        other => panic!("expected a Canonical error naming Main, got {:?}", other),
    }

    let diagnostic = error.as_diagnostic();
    assert_eq!(
        diagnostic.notes,
        vec!["it is exposed by: Helper, and implicitly by Basics".to_string()],
        "the note must name the written contributor plainly and the default one \
         as implicit, got {:?}",
        diagnostic.notes
    );
}

// ── Test 22b: an ambiguous operator pair points at the declaring module ─────

/// `BUG-22`: `AmbiguousOperatorPrecedence`'s "declared here" labels have to land
/// in the module that wrote the `infix` declaration, which is usually not the
/// module being checked.
///
/// An operator's declaration is very often imported — `Basics` declares every one
/// the standard library uses — and `canonical::Infix::span` is then a byte range
/// in the *exporting* module's file. A `SpanLabel` built with `file: None` means
/// "the module under check", so such a label underlines whatever text happens to
/// sit at those bytes in the importing module: in this fixture, `User.zel`'s
/// `import` line and the head of `chain`.
///
/// `Ops.zel` declares `<` and `>` both `infix non 4`, `User.zel` imports them
/// unqualified and writes `a < b > c`. Mutation-checked by replacing
/// `InfixDeclaration::InImportedModule`'s arm in
/// `InfixDeclaration::label` with the `file: None` the code had before: the
/// error, its message and all three labels survive that, and only the `file_id`
/// assertions below go red.
#[test]
fn ambiguous_imported_operators_are_labeled_in_their_own_module() {
    let root = fixture_package("package_imported_operator_ambiguity");
    assert_eq!(
        module_names(&root.join("src")),
        vec!["Basics.zel", "Ops.zel", "User.zel"]
    );

    let ops_source =
        std::fs::read_to_string(root.join("src").join("Ops.zel")).expect("fixture is readable");
    let lt_decl = ops_source
        .find("infix non 4 (<)")
        .expect("Ops declares `<`");
    let gt_decl = ops_source
        .find("infix non 4 (>)")
        .expect("Ops declares `>`");

    let error = compile_package(&root).expect_err("an ambiguous operator pair must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(&errors[0]) {
        CompilationError::Canonical(canonical_errors, module) => {
            assert_eq!(module, &Name::from("User"));
            assert_eq!(canonical_errors.len(), 1, "got {:?}", canonical_errors);
        }
        other => panic!("expected a Canonical error, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    let primary = diagnostic
        .labels
        .iter()
        .find(|l| l.style == LabelStyle::Primary)
        .expect("expected a primary label under the ambiguous expression");
    let secondary: Vec<_> = diagnostic
        .labels
        .iter()
        .filter(|l| l.style == LabelStyle::Secondary)
        .collect();

    assert_eq!(
        secondary.len(),
        2,
        "expected one `declared here` label per operator, got {:?}",
        diagnostic.labels
    );

    // The point of the test: both declarations were written in `Ops.zel`, so
    // neither label belongs in `User.zel` where the primary caret sits.
    assert_eq!(
        secondary[0].file_id, secondary[1].file_id,
        "both operators are declared in the same module, so both labels share a file"
    );
    assert_ne!(
        secondary[0].file_id, primary.file_id,
        "a `declared here` label must sit in the declaring module, not the one under check"
    );

    // And it underlines the `infix` declaration itself rather than whatever byte
    // range happens to line up.
    let starts: Vec<_> = secondary.iter().map(|l| l.range.start).collect();
    assert!(
        starts.contains(&lt_decl) && starts.contains(&gt_decl),
        "expected labels at {:?} and {:?}, got {:?}",
        lt_decl,
        gt_decl,
        starts
    );
}

// ── Test 23: a cross-module label does not need the checked module's file ────

/// A label that carries its own file renders even when the diagnostic has none.
///
/// `phase_diagnostic` takes the module's `SourceFileId` as the *fallback* for a
/// label that does not name one, not as a precondition for rendering labels at
/// all. The distinction only became meaningful with `ERR-5`: a `SpanLabel` built
/// from an `Interface`'s `SourceSpan` already knows which file to underline and
/// needs nothing from the module under check.
///
/// `compile_package` always wraps in `CompilationError::InFile`, so this is not
/// reachable from the driver — but `as_diagnostic` is public precisely so a test
/// can assert on what a user is shown, and unwrapping the `InFile` here is how
/// that public entry point behaves on a `CompilationError` built by hand.
///
/// Mutation-checked by putting the old `match file { Some(id) => .., None =>
/// Vec::new() }` gate back in `phase_diagnostic`, which drops every label and
/// turns the `secondary.len() == 2` assertion red.
#[test]
fn cross_module_labels_render_without_the_checked_module_file() {
    let root = fixture_package("package_ambiguous_import");

    let error = compile_package(&root).expect_err("an ambiguous import must not compile");
    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };

    // The bare phase error, with no `InFile` wrapper: nothing tells it which file
    // `Main` was read from.
    let bare = unwrap_in_file(&errors[0]);
    let diagnostic = bare.as_diagnostic();

    let secondary: Vec<_> = diagnostic
        .labels
        .iter()
        .filter(|l| l.style == codespan_reporting::diagnostic::LabelStyle::Secondary)
        .collect();
    assert_eq!(
        secondary.len(),
        2,
        "the two candidate labels carry their own file and must survive, got {:?}",
        diagnostic.labels
    );
    assert_ne!(
        secondary[0].file_id, secondary[1].file_id,
        "each candidate is still underlined in its own file"
    );

    // The primary label is about `Main` itself, and there is no file for it, so it
    // is the one thing that drops.
    assert!(
        !diagnostic
            .labels
            .iter()
            .any(|l| l.style == codespan_reporting::diagnostic::LabelStyle::Primary),
        "a label with neither its own file nor a fallback has nothing to underline, got {:?}",
        diagnostic.labels
    );
}

// ── Test 24: a dependency cycle labels each import that forms it ────────────

/// `ERR-6`: a circular-dependency diagnostic underlines the specific `import`
/// line that created each edge of the cycle, one label per edge, rather than
/// only naming the modules in a note.
///
/// `CycleA.zel` imports `CycleB`, which imports `CycleA` back — the smallest
/// possible cycle, so there is no ambiguity about which two edges it has to
/// label. Before this ticket `dependencies::Error::CycleDetected` rendered with
/// no labels at all: `CompilationError::DependenciesError`'s arm of
/// `as_diagnostic_in` called `.with_notes(..)` but never `.with_labels(..)`.
///
/// Each edge's label is expected in its *own* module's file — `CycleA`'s import
/// of `CycleB` is underlined in `CycleA.zel`, not in `CycleB.zel` — which is the
/// same cross-file labeling `ERR-5` introduced, applied here to
/// `dependencies::CycleEdge::file` instead of `Interface::source_span`.
///
/// Mutation-checked two ways, each independently red: (1) reverting the
/// `.with_labels(spans_to_labels(err.labels(), None))` call in the
/// `DependenciesError` arm back to no `.with_labels(..)` at all empties
/// `diagnostic.labels`; (2) reverting `cycle_walk` in `dependencies.rs` to
/// return `members` verbatim (`tarjan_scc`'s raw, edge-agnostic order) instead
/// of walking real edges does not change anything observable for this
/// particular two-module fixture (a two-node cycle has only one possible walk
/// either way), which is exactly why `dependencies_with_two_cycles` in
/// `dependencies.rs`'s own tests — a three-node cycle, where raw SCC order and
/// a real edge walk diverge — is the test that actually pins that half.
#[test]
fn dependency_cycle_labels_each_import() {
    let root = fixture_package("package_dependency_cycle");
    assert_eq!(
        module_names(&root.join("src")),
        vec!["CycleA.zel", "CycleB.zel"]
    );

    let a_source =
        std::fs::read_to_string(root.join("src").join("CycleA.zel")).expect("fixture is readable");
    let b_source =
        std::fs::read_to_string(root.join("src").join("CycleB.zel")).expect("fixture is readable");
    let a_import = "import CycleB exposing (..)";
    let b_import = "import CycleA exposing (..)";
    let a_start = a_source.find(a_import).expect("CycleA imports CycleB");
    let b_start = b_source.find(b_import).expect("CycleB imports CycleA");

    let error = compile_package(&root).expect_err("a dependency cycle must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    // Asserted down to the variant, and that there is exactly one cycle, so this
    // cannot be satisfied by some other kind of failure.
    match &errors[0] {
        CompilationError::DependenciesError(dependencies::Error::CycleDetected(cycles)) => {
            assert_eq!(
                cycles.len(),
                1,
                "expected exactly one cycle, got {:?}",
                cycles
            );
        }
        other => panic!("expected a DependenciesError, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    assert_eq!(
        diagnostic.labels.len(),
        2,
        "expected one label per edge in the two-module cycle, got {:?}",
        diagnostic.labels
    );
    assert!(
        diagnostic
            .labels
            .iter()
            .all(|l| l.style == codespan_reporting::diagnostic::LabelStyle::Primary),
        "every edge in the cycle is equally the cause, got {:?}",
        diagnostic.labels
    );

    // Each edge is labeled in the *importing* module's own file, matching the
    // ticket's acceptance criterion verbatim.
    assert_ne!(
        diagnostic.labels[0].file_id, diagnostic.labels[1].file_id,
        "the two edges must be labeled in their own, different files"
    );

    let ranges: Vec<_> = diagnostic.labels.iter().map(|l| l.range.clone()).collect();
    assert!(
        ranges.contains(&(a_start..(a_start + a_import.len()))),
        "expected a label at CycleA's import of CycleB {:?}, got {:?}",
        a_start..(a_start + a_import.len()),
        ranges
    );
    assert!(
        ranges.contains(&(b_start..(b_start + b_import.len()))),
        "expected a label at CycleB's import of CycleA {:?}, got {:?}",
        b_start..(b_start + b_import.len()),
        ranges
    );
}

// ── Test 25: an exposed-but-missing import name is underlined alone ──────────

/// `ERR-9`: `import Foo exposing (bar)` naming a value `Foo` does not export is
/// underlined at `bar` alone, not across the whole `import` line.
///
/// Before this ticket `parser::Exposed` carried no span, so the best
/// `EnvError::ValueNotFound` could point at was the `import` line handed to
/// `process_import` — a whole-line caret on a many-name exposing list. `Lib`
/// genuinely exports `value`, so this exercises the "found the module, not the
/// name" path through `new_environment` rather than `InterfaceNotFound`.
///
/// Mutation-checked two ways, each red on its own: making the `Exposed`
/// productions in `grammar.lalrpop` emit `NodeSpan::none()` (the label disappears,
/// since `EnvError::labels` has nothing to attach), and reverting
/// `EnvError::ValueNotFound`'s `labels` arm to `Vec::new()`.
#[test]
fn missing_exposed_import_name_labels_the_name_alone() {
    let root = fixture_package("package_exposing_missing_value");
    assert_eq!(module_names(&root.join("src")), vec!["Lib.zel", "Main.zel"]);

    let source =
        std::fs::read_to_string(root.join("src").join("Main.zel")).expect("fixture is readable");
    let identifier = "missing";
    let start = source
        .find(identifier)
        .expect("fixture imports an undeclared `missing`");

    let error = compile_package(&root).expect_err("importing an unexported name must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(&errors[0]) {
        CompilationError::Canonical(canonical_errors, module) => {
            assert_eq!(module, &Name::from("Main"));
            assert_eq!(canonical_errors.len(), 1, "got {:?}", canonical_errors);
        }
        other => panic!("expected a Canonical error, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    assert_eq!(
        diagnostic.labels.len(),
        1,
        "expected one label, got {:?}",
        diagnostic.labels
    );
    assert_eq!(
        diagnostic.labels[0].range,
        start..(start + identifier.len()),
        "the caret must sit under `{}` alone, not the `import` line around it",
        identifier
    );
}

// ── Test 26: a name exposed by the module header that it never declares ─────

/// `ERR-9`: `module Foo exposing (bar)` naming something `Foo` never declares is
/// underlined at `bar` alone.
///
/// This exercises the `Operator` case specifically; `do_exports`
/// (`canonical/mod.rs`) checks existence the same way for `Lower` and `Upper`
/// names too (`BUG-8`), and `tests/compiler/canonical.rs`'s
/// `export_nonexistent_value_is_error`/`export_nonexistent_type_is_error` cover
/// those directly against `canonical::Error` rather than through the whole
/// package pipeline.
///
/// Mutation-checked two ways, each red on its own: making the `Exposed`
/// productions in `grammar.lalrpop` emit `NodeSpan::none()`, and reverting
/// `Error::ExportNotFound`'s `labels` arm to fall through to the default `Vec::new()`.
#[test]
fn export_not_found_labels_the_exposed_name_alone() {
    let root = fixture_package("package_export_not_found");
    assert_eq!(module_names(&root.join("src")), vec!["Main.zel"]);

    let source =
        std::fs::read_to_string(root.join("src").join("Main.zel")).expect("fixture is readable");
    // The exposed name for an operator, `(<+>)`, includes its wrapping parens —
    // there is no way to write a bare operator in an exposing list, so the parens
    // are as much "the name the user wrote" as the symbol between them.
    let operator = "(<+>)";
    let start = source
        .find(operator)
        .expect("fixture's header exposes an undeclared infix");

    let error = compile_package(&root).expect_err("exposing an undeclared infix must not compile");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(&errors[0]) {
        CompilationError::Canonical(canonical_errors, module) => {
            assert_eq!(module, &Name::from("Main"));
            assert_eq!(canonical_errors.len(), 1, "got {:?}", canonical_errors);
        }
        other => panic!("expected a Canonical error, got {:?}", other),
    }

    let diagnostic = errors[0].as_diagnostic();

    assert_eq!(
        diagnostic.labels.len(),
        1,
        "expected one label, got {:?}",
        diagnostic.labels
    );
    assert_eq!(
        diagnostic.labels[0].range,
        start..(start + operator.len()),
        "the caret must sit under `{}` alone, not the `module` header around it",
        operator
    );
}

// ── Test 26b: an exposed value with no annotation (`BUG-14`) ────────────────
//
// `Module::to_interface` keeps a value only when it carries a type
// (`Value::TypedValue`), so an unannotated declaration named in `exposing`
// used to vanish from the interface silently — the importer then failed with
// `VariableNotFound` for a name `Widget` plainly declares, blaming the wrong
// module for the wrong reason. `SPEC-5` closes this at the source: `Widget`
// itself is now rejected, before it ever publishes an interface.

/// `Widget` fails to canonicalize on its own — `label` is exposed with no
/// annotation — and `Main`, which imports it, gets an error too, but not
/// `VariableNotFound`: `Widget` never published an `Interface` for it to
/// resolve against, so the import itself is what fails.
///
/// Mutation-checked: reverting `do_exports`'s `Lower` arm to accept `label`
/// once `env.find_value` succeeds (the pre-fix behaviour, before the
/// `values.get(name)` annotation check was added) turns `Widget`'s check
/// green again, and with it this test — `Main` would then fail with
/// `VariableNotFound` instead, the exact symptom `BUG-14` was filed over.
#[test]
fn unannotated_export_is_rejected_at_the_declaration_not_the_importer() {
    let widget = indoc::indoc! {r#"
        module Widget exposing (label)
        label = 1
    "#};
    let main = indoc::indoc! {r#"
        module Main exposing (x)
        import Widget
        x = Widget.label
    "#};

    let pkg = test_package();

    let widget_error = check_module(&pkg, &HashMap::new(), &parse_source(widget), false)
        .expect_err("an exposed, unannotated value must not compile");

    match &widget_error {
        CompilationError::Canonical(errors, module) => {
            assert_eq!(module, &Name::from("Widget"));
            assert_eq!(errors.len(), 1, "got {:?}", errors);
            match &errors[0] {
                canonical::Error::ExportedValueNotAnnotated(name, _, _) => {
                    assert_eq!(name.as_str(), "label");
                }
                other => panic!("expected ExportedValueNotAnnotated, got {:?}", other),
            }
        }
        other => panic!("expected a Canonical error naming Widget, got {:?}", other),
    }

    // `Widget` never checked, so there is no `Interface` for it in scope —
    // exactly what the real pipeline would have, since `check_in_order` only
    // inserts an `Interface` for a module that canonicalized.
    let main_error = check_module(&pkg, &HashMap::new(), &parse_source(main), false)
        .expect_err("Main imports a module that never checked");

    match &main_error {
        CompilationError::Canonical(errors, module) => {
            assert_eq!(module, &Name::from("Main"));
            assert!(
                !errors
                    .iter()
                    .any(|e| matches!(e, canonical::Error::VariableNotFound(..))),
                "the importer must not blame a missing variable for a name \
                 `Widget` plainly declares: {:?}",
                errors
            );
        }
        other => panic!("expected a Canonical error naming Main, got {:?}", other),
    }
}

// ── Test 27: an `exposing` list is the whole of what other modules reach ─────
//
// `BUG-9`: `Module::to_interface` built the view other modules import against
// out of every top-level declaration and never read `Module::exports`, so a
// module's own `exposing (...)` header restricted nothing once the module had
// been checked. These tests all drive the real path — `check_module`, then
// `to_interface`, then `check_module` again on the importer — because a
// hand-built `Interface` would sidestep the only place the filtering happens.

/// One module's source checked into an `Interface`, and a second checked against
/// it. `Lib` compiling is a precondition rather than part of what is asserted,
/// so a failure there panics instead of returning.
///
/// Both are checked with [`basics_interface`] in scope, which is what gives their
/// `Int` the scalar through the default imports.
fn check_importer(lib: &str, main: &str) -> Result<(), CompilationError> {
    let pkg = test_package();
    let mut interfaces: HashMap<Name, Interface> = HashMap::from([basics_interface()]);
    let lib_module = check_module(&pkg, &interfaces, &parse_source(lib), false)
        .unwrap_or_else(|e| panic!("the exporting module should compile: {:?}", e));

    interfaces.insert(
        lib_module.name.name().clone(),
        lib_module.to_interface(None),
    );

    check_module(&pkg, &interfaces, &parse_source(main), false).map(|_| ())
}

/// A module exposing three of its five declarations: a value, an opaque type and
/// a transparent one. `hidden` and `Secret` are the two nothing outside `Lib`
/// may reach, and `Opaque`'s constructor `Wrapped` is the third.
///
/// `Opaque` takes a type variable on purpose. A nullary opaque type would let
/// an importer write the annotation either way and no assertion below could
/// tell the interface carrying the type from the import fabricating it. With
/// an arity of one, `Opaque Int` only resolves when the real declaration
/// crossed.
fn privacy_lib() -> &'static str {
    indoc::indoc! {r#"
        module Lib exposing (visible, Opaque, Clear(..))
        type Opaque a = Wrapped a
        type Clear = Plain
        type Secret = Kept
        visible : Opaque Int
        visible = Wrapped 1
        hidden : Opaque Int
        hidden = Wrapped 1
    "#}
}

/// The one canonicalization error a `check_module` failure is expected to be,
/// with its rendered message — the `EnvError` variants are private to
/// `canonical::environment`, so the message is how a test names which one fired.
fn only_canonical_error(error: &CompilationError) -> String {
    use zelkova_lang::compiler::PhaseError;

    match error {
        CompilationError::Canonical(errors, module) => {
            assert_eq!(module, &Name::from("Main"));
            assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);
            errors[0].message()
        }
        other => panic!("expected a Canonical error, got {:?}", other),
    }
}

/// A value the exporting module declares but does not expose cannot be named in
/// an import's `exposing` list.
///
/// Mutation-checked by dropping the `exports.exposes(..)` filter from
/// `to_interface`'s `values`, which makes this import resolve and the test go
/// red.
#[test]
fn unexposed_value_is_not_importable() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (hidden)
        answer = 1
    "#};

    let error = check_importer(privacy_lib(), main).expect_err("`hidden` is not exposed by `Lib`");

    assert_eq!(
        only_canonical_error(&error),
        "the imported module does not expose a value named `hidden`"
    );
}

/// Nor reached through the qualified spelling, which is the half `exposing` has
/// to cover to mean anything: every import brings the exported names into scope
/// under the module's own prefix whether or not the `import` line lists them.
///
/// Mutation-checked the same way as `unexposed_value_is_not_importable`.
///
/// The name in the message reads `Main.Lib.hidden` because
/// `Error::VariableNotFound` qualifies whatever failed to resolve with the module
/// that was being checked, and `Lib.hidden` is already a qualified spelling —
/// unrelated to this ticket, and asserted as it is rather than worked around.
#[test]
fn unexposed_value_is_not_reachable_qualified() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib
        answer : Lib.Opaque Int
        answer = Lib.hidden
    "#};

    let error =
        check_importer(privacy_lib(), main).expect_err("`Lib.hidden` is not exposed by `Lib`");

    assert_eq!(
        only_canonical_error(&error),
        "cannot find a value named `Main.Lib.hidden`"
    );
}

/// The positive control: what `Lib` does expose still crosses, unqualified and
/// qualified alike. Without this the two tests above would also pass on an
/// interface that exported nothing at all.
///
/// Mutation-checked by making `Exports::exposes` answer `false` for every
/// `Specifics` header, which is that over-reach; five tests here go red, this one
/// among them.
#[test]
fn exposed_value_still_imports() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (visible)
        unqualified : Lib.Opaque Int
        unqualified = visible
        qualified : Lib.Opaque Int
        qualified = Lib.visible
    "#};

    assert!(
        check_importer(privacy_lib(), main).is_ok(),
        "`visible` is exposed by `Lib` and must still resolve both ways"
    );
}

/// A type left out of the header is not a type other modules have.
///
/// Mutation-checked by making `union_visibility` answer `Transparent` for a name
/// with no entry, which puts `Secret` back in the interface.
#[test]
fn unexposed_type_is_not_importable() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (Secret(..))
        answer = 1
    "#};

    let error = check_importer(privacy_lib(), main).expect_err("`Secret` is not exposed by `Lib`");

    assert_eq!(
        only_canonical_error(&error),
        "the imported module does not expose a type named `Secret`"
    );
}

/// A bare `Opaque` entry in the header is not the same as leaving it out: the
/// type still crosses, with its arity, and only its constructors are withheld.
/// This is the `ExportType::UnionPrivate` half of the filter, and the one a
/// "hide everything not exposed" reading of the header would get wrong.
///
/// Mutation-checked two ways, each red on its own: collapsing
/// `union_visibility`'s `Opaque` answer into `Hidden`, which drops `Lib.Opaque`
/// from the interface and leaves the unqualified `Opaque Int` at the wrong arity;
/// and collapsing it into `Transparent`, which stops emptying `variants` and lets
/// `Wrapped` through.
#[test]
fn opaquely_exposed_type_crosses_without_its_constructors() {
    let uses_the_type = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (Opaque)
        keep : Opaque Int -> Opaque Int
        keep x = x
        keepQualified : Lib.Opaque Int -> Lib.Opaque Int
        keepQualified x = x
    "#};

    assert!(
        check_importer(privacy_lib(), uses_the_type).is_ok(),
        "an opaque export is still a type importers can name, at its declared arity"
    );

    // The constructor is reached through the *qualified* spelling, and that is
    // the whole point of the block: an `import Lib exposing (Opaque)` line
    // withholds constructors on the import side too, so an unqualified `Wrapped`
    // would fail whether or not `to_interface` had emptied `variants` — the test
    // would pass without the fix. A bare `import Lib` asks for everything `Lib`
    // exports under its own prefix, so `Lib.Wrapped` resolving or not is a
    // question only the interface answers.
    let uses_a_constructor = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib
        answer : Lib.Opaque Int
        answer = Lib.Wrapped 1
    "#};

    let error = check_importer(privacy_lib(), uses_a_constructor)
        .expect_err("`Lib` exposes `Opaque` without its constructors");

    assert_eq!(
        only_canonical_error(&error),
        "cannot find a type constructor named `Main.Lib.Wrapped`"
    );
}

/// And a `Clear(..)` entry hands over the constructors, so the two forms are
/// told apart rather than both being read as opaque.
///
/// Mutation-checked by collapsing `union_visibility`'s `Transparent` answer for
/// `ExportType::UnionPublic` into `Opaque`.
#[test]
fn transparently_exposed_type_carries_its_constructors() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (Clear(..))
        answer : Clear
        answer = Plain
    "#};

    assert!(
        check_importer(privacy_lib(), main).is_ok(),
        "`Clear(..)` exposes `Plain` too"
    );
}

/// Operators are filtered by the same header. `Lib` below declares two of them
/// and exposes one, so an import naming the other is an unresolved entry rather
/// than a working operator.
///
/// Both halves are asserted because the exposed half is what shows the filter
/// keyed on the operator's own entry: `(<+>)` crosses only because the header
/// lists it.
///
/// Mutation-checked by dropping the `exports.exposes(..)` filter from
/// `to_interface`'s `infixes`.
#[test]
fn unexposed_operator_is_not_importable() {
    let lib = indoc::indoc! {r#"
        module Lib exposing (plus, (<+>))
        infix left 6 (<+>) = plus
        infix left 6 (<->) = minus
        plus : Int -> Int -> Int
        plus a b = a
        minus : Int -> Int -> Int
        minus a b = a
    "#};

    let exposed = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (plus, (<+>))
        answer : Int
        answer = 1 <+> 2
    "#};

    assert!(
        check_importer(lib, exposed).is_ok(),
        "`(<+>)` is exposed by `Lib` and must still resolve"
    );

    let unexposed = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (plus, (<->))
        answer : Int
        answer = 1 <-> 2
    "#};

    let error = check_importer(lib, unexposed).expect_err("`(<->)` is not exposed by `Lib`");

    assert_eq!(
        only_canonical_error(&error),
        "the imported module does not expose an infix operator named `<->`"
    );
}

/// The regression `to_interface`'s `infixes` filter almost reintroduced:
/// `std/core/src/Basics.zel` exposes every operator without separately
/// exposing its backing function by name — `infix left 6 (+) = add`, header
/// exposing `(+)` and not `add` — and `import Basics exposing (..)` relies on
/// `+` still resolving. The type of `add` has to cross with the operator, or a
/// use of `+` has nothing to be typed against; `Interface::infix_functions` is
/// what carries it once `values` is filtered down to what the header names.
///
/// `Lib` here has exactly `Basics`' shape: one operator exposed, its backing
/// function not exposed at all.
///
/// Mutation-checked by dropping the `.or_else(..)` over
/// `interface.infix_functions` in `imported_infix`
/// (`src/compiler/canonical/environment.rs`), which turns this red with
/// `cannot find a value named \`Lib.plus\``.
#[test]
fn exposed_operator_resolves_via_open_import_without_its_backing_function_exposed() {
    let lib = indoc::indoc! {r#"
        module Lib exposing ((<+>))
        infix left 6 (<+>) = plus
        plus : Int -> Int -> Int
        plus a b = a
    "#};

    let main = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (..)
        answer : Int
        answer = 1 <+> 2
    "#};

    assert!(
        check_importer(lib, main).is_ok(),
        "`(<+>)` is exposed by `Lib`, and `exposing (..)` must still resolve it even though `plus` is never exposed by name"
    );
}

/// The other half of the fix above: carrying `plus`'s type across with the
/// operator must not make `plus` importable *by its own name* — that would
/// reopen the hole `BUG-9` closed. `Interface::infix_functions` is kept
/// separate from `values` for exactly this reason.
///
/// Mutation-checked by dropping the `exports.exposes(..)` filter from
/// `to_interface`'s `values` (the original `BUG-9` fix, not the addition
/// above) — the same mutation `unexposed_value_is_not_importable` catches,
/// repeated here because this is the one shape a wrong reading of the
/// `infix_functions` fix (keeping the backing function in `values` itself)
/// would get wrong without failing that test.
#[test]
fn backing_function_of_an_exposed_operator_stays_unimportable_by_name() {
    let lib = indoc::indoc! {r#"
        module Lib exposing ((<+>))
        infix left 6 (<+>) = plus
        plus : Int -> Int -> Int
        plus a b = a
    "#};

    let main = indoc::indoc! {r#"
        module Main exposing (..)
        import Lib exposing (plus)
        answer = 1
    "#};

    let error =
        check_importer(lib, main).expect_err("`plus` is not exposed by `Lib`, only `(<+>)` is");

    assert_eq!(
        only_canonical_error(&error),
        "the imported module does not expose a value named `plus`"
    );
}

// ── Test 28: a missing source root is reported, not compiled as success ──────

/// `BUG-21`: `load_package_sources` walked the source root with `WalkDir` and
/// discarded every `Err` the walk produced (`.filter_map(|r| r.ok())`). A root
/// that doesn't exist makes `WalkDir` yield exactly one `Err` and then stop, so
/// that discard turned a missing source root into an empty `SourceFiles` —
/// zero modules, zero errors, and `compile_package` returning `Ok(())`.
///
/// `LANG-13` moved what this pins from "the package root does not exist" to "the
/// package root exists, with a manifest, but has no `src/`": `compile_package` now
/// reads the manifest before ever deriving `src/`, so a package root that is
/// missing outright is caught by [`compile_package_reports_a_missing_manifest`]
/// instead, one step earlier. `package_missing_src` is a fixture with a valid
/// `zelkova.toml` and no `src/` directory at all, which is what still reaches
/// `load_package_sources` on a path that does not exist.
///
/// The loading failure now arrives inside `CompilationError::Many`, because
/// loading a package's sources happens once per package *inside* the build's error
/// accumulator: a package that cannot be read pushes its failure onto that vector
/// and publishes nothing, rather than returning out of `compile_package` past the
/// packages whose diagnostics are already on it. See
/// [`a_package_that_cannot_be_read_does_not_hide_an_earlier_packages_errors`].
///
/// Mutation-checked by restoring the `filter_map(|r| r.ok())` discard in
/// `load_package_sources` (`src/compiler/source/mod.rs`): with the walk error
/// thrown away, `compile_package` returns `Ok(())` on this same fixture, and
/// `expect_err` below panics.
#[test]
fn compile_package_reports_a_missing_source_root() {
    let root = fixture_package("package_missing_src");
    let src_root = root.join("src");
    assert!(
        !src_root.exists(),
        "fixture must have no `src/` for this test to mean anything"
    );

    let error =
        compile_package(&root).expect_err("a package with no `src/` must not compile as success");

    let CompilationError::Many(accumulated) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(
        accumulated.len(),
        1,
        "expected exactly one accumulated error, got {:?}",
        accumulated
    );

    let CompilationError::LoadingFiles(errors) = &accumulated[0] else {
        panic!(
            "expected CompilationError::LoadingFiles(..), got {:?}",
            accumulated[0]
        );
    };
    assert_eq!(
        errors.len(),
        1,
        "expected exactly one error, got {:?}",
        errors
    );

    let message = errors[0].message();
    assert!(
        message.contains(&src_root.to_string_lossy().to_string()),
        "expected the missing `src/` path in the error message, got {:?}",
        message
    );
}

// ── Test 28a: a package with no manifest is reported, not compiled as success ─

/// `LANG-13`'s first acceptance check: a package directory with no `zelkova.toml`
/// is a `CompilationError` naming the missing manifest, raised before the file
/// database exists (so it comes back as `CompilationError::Manifest`, not
/// wrapped in `InFile`, the same way a `LoadingFiles` failure is unrendered).
///
/// `package_missing_manifest` holds nothing but a `README.md` explaining why —
/// no `zelkova.toml`, no `src/` — so a compile that got this far without failing
/// would have had to skip the manifest check entirely.
///
/// Mutation-checked by neutralising `manifest::load`'s `NotFound` arm to fall
/// through to `Unreadable` instead of `Missing`: the variant match below goes
/// red while `is_err()` alone would not have noticed.
#[test]
fn compile_package_reports_a_missing_manifest() {
    let root = fixture_package("package_missing_manifest");

    let error =
        compile_package(&root).expect_err("a package with no manifest must not compile as success");

    let CompilationError::Manifest(errors) = &error else {
        panic!(
            "expected Err(CompilationError::Manifest(..)), got {:?}",
            error
        );
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match &errors[0] {
        manifest::ManifestError::Missing { manifest_path } => {
            assert_eq!(manifest_path, &root.join("zelkova.toml"));
        }
        other => panic!("expected ManifestError::Missing, got {:?}", other),
    }

    let message = errors[0].message();
    assert!(
        message.contains("zelkova.toml"),
        "expected the missing manifest to be named, got {:?}",
        message
    );
}

// ── Test 28b: an illegal package name is reported, not compiled as success ───

/// `LANG-13`'s second acceptance check: a manifest whose `name` is not a legal
/// package name is a `CompilationError` naming the field.
///
/// `package_invalid_name` declares `name = "Not_Legal"` — uppercase and an
/// underscore, neither of which a package name may hold.
///
/// Mutation-checked by asserting on the variant and the offending string rather
/// than `is_err()` alone: with `is_legal_package_name` accepting everything, the
/// fixture — which has no `src/` at all — fails one step later instead, as
/// `LoadingFiles`, and it is the `let CompilationError::Manifest(..) = … else`
/// below that catches it rather than `expect_err`.
#[test]
fn compile_package_reports_an_invalid_package_name() {
    let root = fixture_package("package_invalid_name");

    let error = compile_package(&root)
        .expect_err("a manifest with an illegal package name must not compile as success");

    let CompilationError::Manifest(errors) = &error else {
        panic!(
            "expected Err(CompilationError::Manifest(..)), got {:?}",
            error
        );
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match &errors[0] {
        manifest::ManifestError::InvalidName { name, .. } => assert_eq!(name, "Not_Legal"),
        other => panic!("expected ManifestError::InvalidName, got {:?}", other),
    }

    let message = errors[0].message();
    assert!(
        message.contains("Not_Legal"),
        "expected the offending name in the message, got {:?}",
        message
    );
}

// ── Test 28c: `private-modules` naming a module the package never had ────────

/// `private-modules` is read and validated — every entry has to name a module the
/// package actually holds — separately from what the list is for, which is keeping
/// those modules out of what the package publishes to its dependents
/// (`a_dependencys_private_module_is_not_importable`, below).
///
/// Unlike a missing or malformed manifest, this check needs the package's real
/// module list, which is only known once sources are parsed — so it cannot be
/// raised by `manifest::load` itself, and reaches `compile_package`'s ordinary
/// error accumulation (`CompilationError::Many`) rather than the unrendered path
/// the other manifest failures take. `package_private_module_not_found` declares
/// `private-modules = ["Ghost"]` and holds one real module, `Answer`.
///
/// Mutation-checked by making the `held_modules.contains(name)` filter in
/// `compile_package` always `true` (as if every name were held): the
/// `PrivateModuleNotFound` push never happens and `expect_err` panics.
#[test]
fn compile_package_reports_a_private_module_that_does_not_exist() {
    let root = fixture_package("package_private_module_not_found");
    assert_eq!(module_names(&root.join("src")), vec!["Answer.zel"]);

    let error = compile_package(&root)
        .expect_err("a `private-modules` entry naming no real module must not compile as success");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    let CompilationError::Manifest(manifest_errors) = &errors[0] else {
        panic!("expected a CompilationError::Manifest, got {:?}", errors[0]);
    };
    assert_eq!(
        manifest_errors.len(),
        1,
        "expected one error, got {:?}",
        manifest_errors
    );
    match &manifest_errors[0] {
        manifest::ManifestError::PrivateModuleNotFound { name, .. } => {
            assert_eq!(name, &Name::from("Ghost"));
        }
        other => panic!("expected PrivateModuleNotFound, got {:?}", other),
    }
}

// ── Test 28d: a parse failure does not become a manifest error too ───────────

/// A module that fails to parse contributes nothing to the list of modules the
/// package holds, so a `private-modules` entry naming it would read as an entry
/// naming a module that does not exist — one broken file reported twice, once
/// truthfully and once as the manifest's fault.
///
/// `package_private_module_parse_failure` declares `private-modules = ["Broken"]`
/// and holds exactly one module, `Broken`, which does not parse. The only error is
/// the parse error.
///
/// Mutation-checked by removing the `parse_failures == 0` guard around the
/// `private-modules` check in `compile_package`: a second
/// `CompilationError::Manifest` joins the parse error and the length assertion
/// below fails.
#[test]
fn a_parse_failure_does_not_also_report_its_module_as_unheld() {
    let root = fixture_package("package_private_module_parse_failure");
    assert_eq!(module_names(&root.join("src")), vec!["Broken.zel"]);

    let error = compile_package(&root).expect_err("`Broken.zel` does not parse");

    let CompilationError::Many(errors) = &error else {
        panic!("expected Err(CompilationError::Many(..)), got {:?}", error);
    };
    assert_eq!(
        errors.len(),
        1,
        "the parse error is the only error the package has, got {:?}",
        errors
    );
    assert!(
        matches!(errors[0], CompilationError::Source(..)),
        "expected the parse error alone, got {:?}",
        errors[0]
    );
}

// ── Test 29: the default imports reach a module that wrote none ─────────────

/// Every checked module of the fixture package, in the order they were checked.
///
/// `compile_package` reports its modules to stderr and hands back only `Ok(())`,
/// so a test that has to look *inside* a checked module drives the walker with the
/// real `check_module` instead — the same seam
/// `check_in_order_keeps_passing_siblings_with_the_real_checker` uses.
fn check_fixture(name: &str) -> Vec<canonical::Module> {
    let root = fixture_package(name);
    let sources = load_package_sources(&root.join("src"))
        .unwrap_or_else(|e| panic!("failed to load sources from {:?}: {:?}", root, e));
    let modules: Vec<parser::Module> = sources
        .iter()
        .map(|(_, file)| {
            parser::parse(file.file())
                .unwrap_or_else(|e| panic!("parse error in {:?}: {:?}", file.file().name(), e))
        })
        .collect();

    let module_files = HashMap::new();
    let walker =
        ModuleWalker::new(&modules, &module_files).expect("no dependency cycle in the fixture");
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let (checked, _errors) = walker.check_in_order(
        &test_package(),
        &mut interfaces,
        &module_files,
        check_module,
    );

    checked
}

/// One declaration of one checked module, or a panic naming what went missing.
///
/// The failures `check_in_order` produced are deliberately dropped rather than
/// asserted empty: the fixture below holds two modules making two different claims,
/// and a shared "every module checked" assertion would make each of them go red
/// whenever the *other* one broke. A module that failed to check is simply absent
/// here, which is a failure of the test that asked for it and of no other.
fn checked_value<'a>(
    modules: &'a [canonical::Module],
    module: &str,
    value: &str,
) -> &'a canonical::Value {
    modules
        .iter()
        .find(|m| m.name.name() == &Name::from(module))
        .unwrap_or_else(|| panic!("`{}` should have checked, and did not", module))
        .values
        .get(&Name::from(value))
        .unwrap_or_else(|| panic!("`{}` declares `{}`", module, value))
}

/// Every imported name the expression tree under `value` resolved to, fully
/// qualified — `Basics.add` for a `+` that came from `Basics`.
///
/// `ExpressionKind::VarForeign` is what canonicalization builds for a name found
/// through an import, and it carries the *declaring* module, so this is what tells
/// "`+` resolved" apart from "`+` resolved to the right module". An operator
/// appears here under the function its `infix` declaration names — `Basics.add`
/// for `+` — which is the binding a later phase looks it up against; the symbol
/// itself is never a value name.
fn foreign_names(value: &canonical::Value) -> Vec<String> {
    fn walk(expr: &canonical::Expression, out: &mut Vec<String>) {
        match &expr.kind {
            canonical::ExpressionKind::VarForeign(qual, _) => out.push(qual.to_name().to_string()),
            canonical::ExpressionKind::Apply(f, arg) => {
                walk(f, out);
                walk(arg, out);
            }
            _ => (),
        }
    }

    let body = match value {
        canonical::Value::Value { body, .. } => body,
        canonical::Value::TypedValue { body, .. } => body,
    };
    let mut out = Vec::new();
    walk(body, &mut out);
    out
}

/// A stand-in `Basics` interface exposing just enough to write `1 + 2`, for a
/// test that needs `Basics` available as an already-checked dependency rather
/// than as a real sibling module on disk.
///
/// A real fixture module literally named `Basics` would make its own package
/// the one the eight default imports belong to (`LANG-57`) — the package-level
/// rule this file's `default_imports_resolve_without_an_import_line` exists to
/// exercise the *opposite* side of — so that scenario can only be reproduced
/// with a hand-built interface here, the same way `maybe_interface` in
/// `tests/support/mod.rs` stands in for a real `Maybe.zel`.
///
/// Named apart from [`support::basics_interface`], which carries no values —
/// this one adds `+`/`add` on purpose, and a plain `use support::*` item-level
/// shadowing would otherwise hand every other call in this file the wrong one.
fn basics_interface_with_plus() -> (Name, Interface) {
    use zelkova_lang::compiler::position::NodeSpan;

    let int_type = canonical::Type::Type(qual("Basics.Int"), vec![]);
    let add_type = canonical::Type::Arrow(
        Box::new(int_type.clone()),
        Box::new(canonical::Type::Arrow(
            Box::new(int_type.clone()),
            Box::new(int_type.clone()),
        )),
    );

    let mut values = HashMap::new();
    values.insert("add".into(), (NodeSpan::none(), add_type));

    let mut unions = HashMap::new();
    unions.insert(
        "Int".into(),
        canonical::UnionType {
            span: NodeSpan::none(),
            variables: vec![],
            variants: vec![canonical::TypeConstructor {
                name: "Int".into(),
                type_parameters: vec![],
                tpe: qual("Basics.Int"),
            }],
        },
    );

    let mut infixes = HashMap::new();
    infixes.insert(
        "+".into(),
        canonical::Infix {
            associativity: canonical::Associativity::Left,
            precedence: 6,
            function_name: "add".into(),
            span: NodeSpan::none(),
        },
    );

    let interface = Interface {
        module_name: zelkova_lang::compiler::ModuleName::new(
            PackageName::new("zelkova-core").unwrap(),
            "Basics".into(),
        ),
        values,
        unions,
        infixes,
        infix_functions: HashMap::new(),
        file: None,
    };

    ("Basics".into(), interface)
}

/// `LANG-8`: a module that writes no `import` at all still resolves `+`, and
/// resolves it to `Basics`, in an ordinary package (`package_declares_a_default:
/// false`).
///
/// Asserting on the `VarForeign` rather than on `is_ok()` is the difference
/// between "it compiled" and "it compiled because `Basics` was in scope": a
/// module that resolved `+` some other way would satisfy the first and not the
/// second.
///
/// Mutation-checked by dropping the `implicit` half of `new_environment`'s import
/// loop: this then fails to canonicalize with a `VariableNotFound` for `+`, and
/// `unwrap_or_else` panics.
#[test]
fn default_imports_resolve_without_an_import_line() {
    let (name, iface) = basics_interface_with_plus();
    let mut interfaces = HashMap::new();
    interfaces.insert(name, iface);

    let source = indoc::indoc! {r#"
        module Implicit exposing (x)
        x : Int
        x = 1 + 2
    "#};

    let checked = check_module(&test_package(), &interfaces, &parse_source(source), false)
        .unwrap_or_else(|e| panic!("expected the implicit default to resolve `+`: {:?}", e));
    let x = checked
        .values
        .get(&Name::from("x"))
        .expect("`Implicit` declares `x`");

    assert_eq!(
        foreign_names(x),
        vec!["Basics.add".to_string()],
        "`+` must resolve through `Basics`"
    );
}

// ── Test 30: writing a default import out changes nothing ───────────────────

/// `LANG-8`: an explicit `import Basics exposing (..)` still compiles.
///
/// The implicit import goes through `process_import` exactly as a written one
/// does, and `insert_foreign_value` turns a second registration of a name into
/// `ValueType::Foreigns` — which is `AmbiguousVariables` at every *use*. So a
/// module that writes the default import out is the case that would break, and
/// `Explicit.zel` writes `1 + 2` so that it breaks loudly rather than quietly.
///
/// Mutation-checked by dropping the `written.iter().any(..)` filter in
/// `default_imports::implicit_imports`: `Basics` is then registered twice, `Explicit`
/// fails with `AmbiguousVariables` on `+` and never reaches `checked`, while
/// `default_imports_resolve_without_an_import_line` above stays green — which is
/// why `check_fixture` does not assert the package compiled as a whole.
#[test]
fn an_explicit_default_import_still_compiles() {
    let checked = check_fixture("package_default_imports");
    let y = checked_value(&checked, "Explicit", "y");

    assert_eq!(
        foreign_names(y),
        vec!["Basics.add".to_string()],
        "a written default import must resolve the same way the implicit one does"
    );
}

// ── Test 32: an operator entry brings its backing function with it ──────────
//
// `BUG-15`: an operator has no qualified spelling, so naming it in an
// `exposing` list is the only way to reach one across a module boundary — and
// whether the exporting module's *backing* function is separately in scope is
// neither the importer's choice nor visible to them. These drive the real path,
// `check_module` → `to_interface` → `check_module`, because the backing
// function's type comes out of the exporting module's `Interface` and a
// hand-built one would decide the outcome instead.

/// An `exposing` list naming only the operator resolves it: `Main` below never
/// names `add`, and `one + one` still canonicalizes and type checks.
///
/// This is the ticket's reproduction. Mutation-checked by resolving the
/// operator through the importing scope again — replacing
/// `resolve_infix_operator`'s match on `entry.function` with the synthetic
/// `parser::ExpressionKind::Variable(name)` it used to hand to
/// `Expression::from_parser` — which turns this red with `cannot find a value
/// named `+``.
#[test]
fn an_operator_entry_resolves_without_its_backing_function_named() {
    let lib = indoc::indoc! {r#"
        module Lib exposing (Size, one, (+), add)
        type Size = Small
        one : Size
        one = Small
        infix left 6 (+) = add
        add : Size -> Size -> Size
        add a b = a
    "#};

    let main = indoc::indoc! {r#"
        module Main exposing (x)
        import Lib exposing (Size, one, (+))
        x : Size
        x = one + one
    "#};

    assert!(
        check_importer(lib, main).is_ok(),
        "naming `(+)` is enough — `add` need not be in the importer's scope"
    );
}

/// The same import with the backing function *also* named stays a working
/// import rather than an ambiguity: `add` reaches `Main` twice over — once as
/// a value entry, once behind the operator — and only the value entry puts it
/// in `Main`'s scope under that name.
///
/// Mutation-checked by making `process_import`'s `ExposedKind::Operator` arm
/// insert the backing function into `env.variables` as well (the ticket's
/// first approach): `add` then becomes a `ValueType::Foreigns` and `add one
/// one` is rejected as `AmbiguousVariables`.
#[test]
fn an_operator_entry_alongside_its_backing_function_is_not_ambiguous() {
    let lib = indoc::indoc! {r#"
        module Lib exposing (Size, one, (+), add)
        type Size = Small
        one : Size
        one = Small
        infix left 6 (+) = add
        add : Size -> Size -> Size
        add a b = a
    "#};

    let main = indoc::indoc! {r#"
        module Main exposing (x, y)
        import Lib exposing (Size, one, (+), add)
        x : Size
        x = one + one
        y : Size
        y = add one one
    "#};

    assert!(
        check_importer(lib, main).is_ok(),
        "`add` named alongside `(+)` must stay one unambiguous value"
    );
}

/// An operator entry naming an infix the exporting module does not declare is
/// still rejected at the `import` line, and named as the operator it is —
/// admitting an operator without its function in scope is not admitting an
/// operator nothing declares.
///
/// Mutation-checked by replacing the `ok_or_else` in `process_import`'s
/// `ExposedKind::Operator` arm with a `Some(..)`-guarded insert that skips an
/// unknown name: `Main` then fails on the *use* of `<->` instead, and the
/// message assertion goes red.
#[test]
fn an_operator_entry_naming_an_undeclared_infix_is_rejected() {
    let lib = indoc::indoc! {r#"
        module Lib exposing (Size, one, (+), add)
        type Size = Small
        one : Size
        one = Small
        infix left 6 (+) = add
        add : Size -> Size -> Size
        add a b = a
    "#};

    let main = indoc::indoc! {r#"
        module Main exposing (x)
        import Lib exposing (Size, one, (<->))
        x : Size
        x = one
    "#};

    let error = check_importer(lib, main).expect_err("`Lib` declares no `<->`");

    assert_eq!(
        only_canonical_error(&error),
        "the imported module does not expose an infix operator named `<->`"
    );
}

/// When an imported operator's backing function really is beyond reach — the
/// exporting module declared it without an annotation, so its `Interface`
/// carries no type for it — the error names *that* function, not the operator
/// symbol the user wrote. Reporting `+` would send the reader to check an
/// `import` line that is correct.
///
/// Mutation-checked by building the `ImportedUntyped` error from the operator
/// instead (`module.qualify_name(name)` in place of `function_name`), which
/// turns the message into ``cannot find a value named `Lib.+```.
#[test]
fn an_untyped_backing_function_is_reported_under_its_own_name() {
    let lib = indoc::indoc! {r#"
        module Lib exposing ((+))
        infix left 6 (+) = add
        add a b = a
    "#};

    let main = indoc::indoc! {r#"
        module Main exposing (x)
        import Lib exposing ((+))
        x = 1 + 2
    "#};

    let error = check_importer(lib, main).expect_err("`Lib.add` has no type to import");

    assert_eq!(
        only_canonical_error(&error),
        "cannot find a value named `Lib.add`"
    );
}

// ── Test 33: two modules' same-named types are two types ─────────────────────

/// Two exporting modules checked into `Interface`s, and a third checked against
/// both. The same shape as [`check_importer`], for the case where telling two
/// declarations apart needs two of them in scope at once.
fn check_importer_of_two(first: &str, second: &str, main: &str) -> Result<(), CompilationError> {
    let pkg = test_package();
    let mut interfaces: HashMap<Name, Interface> = HashMap::from([basics_interface()]);

    for lib in [first, second] {
        let module = check_module(&pkg, &interfaces, &parse_source(lib), false)
            .unwrap_or_else(|e| panic!("an exporting module should compile: {:?}", e));
        interfaces.insert(module.name.name().clone(), module.to_interface(None));
    }

    check_module(&pkg, &interfaces, &parse_source(main), false).map(|_| ())
}

/// A `Size` declared in `A` and a `Size` declared in `B` are two types, and a
/// function may not return the one it was handed.
///
/// `BUG-35`: the typer used to identify a union by its unqualified name, so both
/// annotations became the one type and this checked clean.
///
/// Mutation-checked by comparing only the unqualified halves in
/// `unify_one_constraint`'s `Adt`/`Adt` arm — `n1.unqualified_name() ==
/// n2.unqualified_name()` — which makes the two unify again and `expect_err` panic.
#[test]
fn two_modules_same_named_types_do_not_unify() {
    let error = check_importer_of_two(SIZE_A, SIZE_B, CROSSES_TWO_SIZES)
        .expect_err("`A.Size` and `B.Size` are two types");

    let CompilationError::Type(errors, module) = &error else {
        panic!("expected a Type error, got {:?}", error);
    };
    assert_eq!(module, &Name::from("Main"));
    assert_eq!(errors.len(), 1, "expected one type error, got {:?}", errors);

    // The variant, not merely `is_err`: the point of the change is that the two
    // types are compared and disagree, which is what `UnificationFailed` reports.
    assert!(
        matches!(
            errors[0].kind,
            zelkova_lang::compiler::typer::ErrorKind::UnificationFailed { .. }
        ),
        "expected a unification failure, got {:?}",
        errors[0].kind
    );

    // Unification is symmetric, so which side each type lands on is not part of
    // the contract; that both are named with the module that declared them is.
    let message = errors[0].message();
    assert!(
        message == "cannot match `A.Size` with `B.Size`"
            || message == "cannot match `B.Size` with `A.Size`",
        "both types must be named by their module, got {:?}",
        message
    );
}

/// The same module with one annotation changed, so that both sides name `A.Size`:
/// the two types agree, and nothing is reported.
///
/// Without this, the test above would also pass on a typer that rejected every
/// `Adt` pair outright.
#[test]
fn a_type_from_another_module_unifies_with_itself() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)

        import A
        import B

        f : A.Size -> A.Size
        f s = s
    "#};

    assert!(
        check_importer_of_two(SIZE_A, SIZE_B, main).is_ok(),
        "`A.Size` is `A.Size`"
    );
}

/// The ambiguity is in the names a message *contains*, not in the two whole
/// renderings: here the sides read `Size` and `Size Size`, so they differ as text
/// while still naming three declarations with one word.
///
/// Comparing the renderings for equality — which is what the first form of
/// `ErrorKind::message` did — leaves this sentence saying nothing, so the check is
/// made on the union names collected out of both sides instead.
///
/// Mutation-checked by restoring that comparison (`if l == r`), which puts both
/// sides down the unqualified arm and reports *cannot match `Size Size` with
/// `Size`*.
#[test]
fn a_shared_spelling_is_qualified_even_when_the_two_heads_differ() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)

        import A
        import Lib

        f : A.Size -> Lib.Size A.Size
        f s = s
    "#};

    let error = check_importer_of_two(SIZE_A, SIZE_LIB, main)
        .expect_err("`A.Size` is not `Lib.Size A.Size`");

    let CompilationError::Type(errors, _) = &error else {
        panic!("expected a Type error, got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one type error, got {:?}", errors);

    let message = errors[0].message();
    assert!(
        message == "cannot match `A.Size` with `Lib.Size A.Size`"
            || message == "cannot match `Lib.Size A.Size` with `A.Size`",
        "every `Size` in the sentence must be named by its module, got {:?}",
        message
    );
}

/// The counterpart: two unions whose spellings do not collide are still quoted the
/// way the source writes them, so qualifying is not simply always on.
#[test]
fn types_that_share_no_spelling_stay_unqualified() {
    let lib = indoc::indoc! {r#"
        module Lib exposing (Box(..))
        type Box a = Wrap a
    "#};
    let main = indoc::indoc! {r#"
        module Main exposing (..)

        import A
        import Lib

        f : A.Size -> Lib.Box A.Size
        f s = s
    "#};

    let error =
        check_importer_of_two(SIZE_A, lib, main).expect_err("`A.Size` is not `Lib.Box A.Size`");

    let CompilationError::Type(errors, _) = &error else {
        panic!("expected a Type error, got {:?}", error);
    };

    let message = errors[0].message();
    assert!(
        message == "cannot match `Size` with `Box Size`"
            || message == "cannot match `Box Size` with `Size`",
        "nothing is ambiguous here, so both sides keep the source's spelling, got {:?}",
        message
    );
}

/// A module declaring a nullary `Size`. [`SIZE_B`] is the same declaration under
/// another module's name, which is the whole point: the two differ only in the half
/// the typer used to throw away.
const SIZE_A: &str = indoc::indoc! {r#"
    module A exposing (Size(..))
    type Size = S
"#};

/// See [`SIZE_A`].
const SIZE_B: &str = indoc::indoc! {r#"
    module B exposing (Size(..))
    type Size = S
"#};

/// A third `Size`, this one taking a parameter, so that a mismatch against
/// [`SIZE_A`]'s can be written with two *different* heads.
const SIZE_LIB: &str = indoc::indoc! {r#"
    module Lib exposing (Size(..))
    type Size a = Wrap a
"#};

/// A declaration whose annotation names both modules' `Size`.
///
/// No constructor appears in it, deliberately: a value that mentions an *imported*
/// constructor is skipped by the typer altogether (`BUG-36`), so it could not show
/// the two types being compared.
const CROSSES_TWO_SIZES: &str = indoc::indoc! {r#"
    module Main exposing (..)

    import A
    import B

    f : A.Size -> B.Size
    f s = s
"#};

// ── Package boundaries: namespaces, unwrapping, and one name per module ──────
//
// `LANG-14`. Each of these drives `compile_package` over a fixture that depends on
// another fixture package through a `path` entry, which is the only source the
// compiler obtains today.

/// Every error a failed `compile_package` accumulated, past the `Many` that groups
/// them.
///
/// A resolution failure raised before the file database exists comes back on its own,
/// so both shapes have to be handled for a test to assert on what was reported.
fn accumulated(error: &CompilationError) -> Vec<&CompilationError> {
    match error {
        CompilationError::Many(errors) => errors.iter().collect(),
        other => vec![other],
    }
}

/// The resolution errors in a failed `compile_package`.
fn resolution_errors(error: &CompilationError) -> Vec<&resolve::Error> {
    accumulated(error)
        .into_iter()
        .filter_map(|error| match unwrap_in_file(error) {
            CompilationError::Resolution(errors) => Some(errors),
            _ => None,
        })
        .flatten()
        .collect()
}

/// A public module of a dependency is imported by writing that package's namespace
/// in front of the module's own name: `acme-widgets`' `Size` is `AcmeWidgets.Size`.
///
/// The fixture annotates and calls through the prefix, so the name has to resolve as
/// a module, as a type and as a value — not merely be accepted on the `import` line.
///
/// Mutation-checked by keying a wrapped dependency's modules by their own names in
/// `resolve::visible_modules`: `AcmeWidgets.Size` then names nothing and this goes
/// red.
#[test]
fn a_dependencys_module_is_imported_under_its_namespace() {
    let root = fixture_package("package_namespaced_dependency");

    assert_eq!(
        module_names(&root.join("src")),
        vec!["App.zel".to_string()],
        "the fixture must hold the module this test is about"
    );

    let result = compile_package(&root);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// A module `private-modules` names is not importable from outside its package, and
/// fails as a module that does not exist rather than as one that is refused: nothing
/// outside `acme-widgets` can tell `Hidden` from a module it never had.
///
/// Mutation-checked by dropping the `private` filter in `compile_in_build`'s
/// publication step, which makes the fixture compile.
#[test]
fn a_dependencys_private_module_is_not_importable() {
    let root = fixture_package("package_private_dependency_module");

    let error = compile_package(&root).expect_err("`AcmeWidgets.Hidden` is private");

    let messages: Vec<String> = accumulated(&error)
        .into_iter()
        .map(|error| unwrap_in_file(error).as_diagnostic().message)
        .collect();

    assert!(
        messages
            .iter()
            .any(|m| m.contains("cannot find a module named `AcmeWidgets.Hidden`")),
        "expected the import to fail as an unknown module, got {:?}",
        messages
    );
}

/// A `module foreign` facade is never importable from outside its package, whatever
/// the manifest says — `acme-widgets` does not list `Native` as private, and it is
/// still unreachable.
///
/// Mutation-checked by dropping the `facades` filter in `compile_in_build`'s
/// publication step: `AcmeWidgets.Native` then resolves and the fixture compiles.
#[test]
fn a_dependencys_facade_is_not_importable() {
    let root = fixture_package("package_facade_dependency_module");

    let error = compile_package(&root).expect_err("a facade is package-internal");

    let messages: Vec<String> = accumulated(&error)
        .into_iter()
        .map(|error| unwrap_in_file(error).as_diagnostic().message)
        .collect();

    assert!(
        messages
            .iter()
            .any(|m| m.contains("cannot find a module named `AcmeWidgets.Native`")),
        "expected the import to fail as an unknown module, got {:?}",
        messages
    );
}

/// `wrapped = false` in a dependency's entry names its modules by their own names
/// throughout the depending package: `Size`, not `AcmeWidgets.Size`.
///
/// Mutation-checked by ignoring the flag in `resolve::visible_modules` and always
/// qualifying, which turns this red and its counterpart below green.
#[test]
fn an_unwrapped_dependency_is_named_by_its_own_names() {
    let root = fixture_package("package_unwrapped_dependency");

    assert_eq!(
        module_names(&root.join("src")),
        vec!["App.zel".to_string()],
        "the fixture must hold the module this test is about"
    );

    let result = compile_package(&root);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// The other half of the same rule: a module has exactly one spelling in any file, so
/// once a dependency is unwrapped its namespace names nothing. The fixture is
/// `package_unwrapped_dependency` with the prefix written back in.
#[test]
fn an_unwrapped_dependencys_namespace_names_nothing() {
    let root = fixture_package("package_unwrapped_namespace_absent");

    let error =
        compile_package(&root).expect_err("`acme-widgets` is unwrapped, so the prefix is gone");

    let messages: Vec<String> = accumulated(&error)
        .into_iter()
        .map(|error| unwrap_in_file(error).as_diagnostic().message)
        .collect();

    assert!(
        messages
            .iter()
            .any(|m| m.contains("cannot find a module named `AcmeWidgets.Size`")),
        "expected the namespaced spelling to name nothing, got {:?}",
        messages
    );
}

/// Two modules answering to one name is reported when the build is resolved — before
/// any module of that package is compiled — and names both modules and the packages
/// they come from.
///
/// The fixture's `App.zel` writes `import Nowhere`, which nothing can resolve. That
/// is what pins the *when*: a canonicalization error for it would mean the package
/// was compiled after all. Only the collision may be reported.
///
/// Mutation-checked by making `visible_modules`' `claim` overwrite the earlier entry
/// instead of reporting it: the collision assertion goes red, and the package is then
/// compiled, so the `import Nowhere` assertion goes red too.
#[test]
fn two_modules_under_one_name_are_reported_before_anything_is_compiled() {
    let root = fixture_package("package_module_name_collision");

    let error = compile_package(&root).expect_err("`Size` is claimed twice");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::ModuleNameCollision {
        package,
        name,
        first,
        second,
    } = errors[0]
    else {
        panic!("expected a module name collision, got {:?}", errors[0]);
    };

    assert_eq!(name.as_str(), "Size");
    assert_eq!(package.as_str(), "package-module-name-collision");
    assert_eq!(first.package.as_str(), "package-module-name-collision");
    assert_eq!(second.package.as_str(), "acme-widgets");

    let notes = errors[0].notes();
    assert!(
        notes.iter().any(|n| n.contains("a module of this package"))
            && notes
                .iter()
                .any(|n| n.contains("acme-widgets 1.2.0") && n.contains("unwrapped")),
        "both modules and their packages must be named, got {:?}",
        notes
    );

    assert!(
        accumulated(&error)
            .into_iter()
            .all(|error| matches!(unwrap_in_file(error), CompilationError::Resolution(_))),
        "nothing in this package may be compiled: `App.zel` imports a module that \
         does not exist and must not be reported, got {:?}",
        error
    );
}

/// The collision case the scalar types depend on: an unwrapped dependency declaring
/// its own `Basics` against `zelkova-core`'s, which is always unwrapped.
///
/// `src/compiler/scalars.rs` recognises a scalar by the bare qualified name
/// `Basics.Int`, with no package in it ([`DEC-15` decision 1]). What keeps that name
/// pointing at one declaration is this rule: two modules named `Basics` in one
/// package is an error, so `Int` in any module resolves to exactly one `Basics`.
/// Both fixtures here declare `type Int = Int`, so without the rule the two would be
/// indistinguishable to every phase after canonicalization.
///
/// Mutation-checked the same way as the test above, and additionally by dropping the
/// `CORE_PACKAGE` arm of `seen_unwrapped`: `zelkova-core` is then wrapped, its
/// `Basics` becomes `ZelkovaCore.Basics`, and no collision is reported at all.
///
/// [`DEC-15` decision 1]: ../docs/decisions/dec-15.md
#[test]
fn a_dependencys_basics_collides_with_cores() {
    let root = fixture_package("package_core_basics_collision");

    let error = compile_package(&root).expect_err("`Basics` is claimed twice");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::ModuleNameCollision {
        name,
        first,
        second,
        ..
    } = errors[0]
    else {
        panic!("expected a module name collision, got {:?}", errors[0]);
    };

    assert_eq!(name.as_str(), "Basics");

    let mut packages = [first.package.as_str(), second.package.as_str()];
    packages.sort_unstable();
    assert_eq!(packages, ["acme-basics", "zelkova-core"]);

    assert!(
        accumulated(&error)
            .into_iter()
            .all(|error| matches!(unwrap_in_file(error), CompilationError::Resolution(_))),
        "nothing in this package may be compiled, got {:?}",
        error
    );
}

/// A package that depends on itself through a chain is reported rather than followed
/// round: the members of a cycle have no order they could be compiled in.
///
/// This test is also what keeps resolution terminating. Without the check, following
/// `package-cycle-a`'s entry into `package-cycle-b` and back again does not stop.
///
/// Mutation-checked by removing the `stack` check at the top of `Resolver::visit`,
/// which makes this test recurse until the stack overflows instead of failing.
#[test]
fn two_packages_depending_on_each_other_are_reported_as_a_cycle() {
    let root = fixture_package("package_cycle_a");

    let error = compile_package(&root).expect_err("the two packages depend on each other");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::Cycle(packages) = errors[0] else {
        panic!("expected a package cycle, got {:?}", errors[0]);
    };

    let mut names: Vec<&str> = packages.iter().map(|p| p.as_str()).collect();
    names.sort_unstable();
    assert_eq!(names, ["package-cycle-a", "package-cycle-b"]);
}

/// A `git` dependency is not obtained, and says so. The entry is legal — the manifest
/// accepts it — and what is missing is the fetching the toolchain appendix describes,
/// so the build stops naming the package it could not get rather than compiling
/// without it and reporting its modules as names that do not exist.
///
/// Mutation-checked by making the `Source::Git` arm of `Resolver::obtain` return
/// `None` without pushing the error: `compile_package` then returns `Ok` for a package
/// whose dependency was never read.
#[test]
fn a_git_dependency_is_reported_as_one_this_compiler_cannot_obtain() {
    let root = fixture_package("package_git_dependency");

    let error = compile_package(&root).expect_err("a `git` source cannot be obtained yet");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::UnsupportedSource { package, .. } = errors[0] else {
        panic!("expected an unsupported source, got {:?}", errors[0]);
    };
    assert_eq!(package.as_str(), "acme-widgets");
}

/// A dependency's module reaches the depending package through the default imports as
/// well as through a written one: `package-core-dependency` writes no `import` at all,
/// and `Int` resolves because `zelkova-core`'s `Basics` is in the map of names it can
/// import, spelled `Basics` because core is seen unwrapped.
///
/// This is the path the scalar types take across a package boundary. `dep_core`
/// declares `type Int = Int`, so the annotation below resolves to `Basics.Int` — the
/// qualified name `src/compiler/scalars.rs` recognises — and `42` unifies with it.
///
/// Mutation-checked by dropping the `CORE_PACKAGE` arm of `seen_unwrapped`: `Basics` is
/// then `ZelkovaCore.Basics`, no default import finds it, and `Int` names nothing.
#[test]
fn a_dependencys_basics_arrives_through_the_default_imports() {
    let root = fixture_package("package_core_dependency");

    let result = compile_package(&root);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// A package name is the build's, not each manifest's: one name given two directories
/// is an error naming both, whichever manifest wrote which entry.
///
/// Both `zelkova-core`s here declare `Int`, so without this the build would hold two
/// declarations of one scalar and every phase after canonicalization would read them
/// as the same type.
///
/// Mutation-checked by dropping the `previous != &package.root` comparison in
/// `Resolver::visit` and taking whichever copy was reached first, which makes the
/// fixture compile.
#[test]
fn one_package_name_may_not_have_two_sources() {
    let root = fixture_package("package_conflicting_sources");

    let error = compile_package(&root).expect_err("two directories answer to `zelkova-core`");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::ConflictingSources {
        package,
        first,
        second,
    } = errors[0]
    else {
        panic!("expected conflicting sources, got {:?}", errors[0]);
    };

    assert_eq!(package.as_str(), "zelkova-core");
    assert_ne!(first, second, "the two entries must name two directories");
}

/// The key a dependency is written under is the package's identity for the whole
/// build, so a package declaring another name is refused rather than taken under the
/// name that was asked for — the namespace a dependent derives from the key would
/// otherwise be put on modules that package has never heard of.
///
/// Mutation-checked by dropping the `manifest.name != name` check in
/// `Resolver::obtain`, which lets `../dep_core` into the build as `acme-widgets`.
#[test]
fn a_dependency_declaring_another_name_is_refused() {
    let root = fixture_package("package_name_mismatch");

    let error = compile_package(&root).expect_err("`../dep_core` is not `acme-widgets`");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::NameMismatch {
        expected, declared, ..
    } = errors[0]
    else {
        panic!("expected a name mismatch, got {:?}", errors[0]);
    };

    assert_eq!(expected.as_str(), "acme-widgets");
    assert_eq!(declared.as_str(), "zelkova-core");
}

/// A `path` entry naming a directory that is not there names the package and the path
/// it looked in, rather than failing as a module that cannot be found in whichever
/// file happened to import it first.
#[test]
fn a_path_dependency_that_is_not_there_names_the_directory() {
    let root = fixture_package("package_missing_dependency_path");

    let error = compile_package(&root).expect_err("`../nowhere` is not a directory");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::SourceUnreachable { package, path, .. } = errors[0] else {
        panic!("expected an unreachable source, got {:?}", errors[0]);
    };

    assert_eq!(package.as_str(), "acme-widgets");
    assert!(
        path.ends_with("nowhere"),
        "the path looked in must be named, got {:?}",
        path
    );
}

/// A package whose dependency did not compile is not compiled either, and says which
/// dependency that was.
///
/// The dependency's own diagnostics say what went wrong inside it; this one is the
/// reason nothing is said about the package the compiler was actually pointed at. It
/// is also what keeps the publish-nothing rule honest: `acme-broken` publishes no
/// interface, so without this arm `App` would be checked against a package that
/// exists in the build and offers no modules, and would fail on an import instead.
///
/// Mutation-checked by dropping the `_ =>` arm of the `(resolved, public)` match in
/// `compile_in_build` — with the dependency simply skipped, the build reports only
/// `acme-broken`'s own error and never names `package-dependency-not-compiled`.
#[test]
fn a_package_whose_dependency_did_not_compile_is_not_compiled() {
    let root = fixture_package("package_dependency_not_compiled");

    let error = compile_package(&root).expect_err("`acme-broken` does not canonicalize");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::DependencyNotCompiled {
        package,
        dependency,
    } = errors[0]
    else {
        panic!("expected an uncompiled dependency, got {:?}", errors[0]);
    };

    assert_eq!(package.as_str(), "package-dependency-not-compiled");
    assert_eq!(dependency.as_str(), "acme-broken");

    // The dependency's own failure is reported too, and not replaced by the sentence
    // above: a user has to be able to see *why* `acme-broken` did not compile.
    let canonicalization_failed = accumulated(&error)
        .into_iter()
        .any(|error| matches!(unwrap_in_file(error), CompilationError::Canonical(..)));
    assert!(
        canonicalization_failed,
        "`acme-broken`'s own diagnostic must survive alongside the one about its \
         dependent, got {:?}",
        error
    );
}

/// A package whose sources cannot be read does not take the diagnostics of the
/// packages compiled before it with it.
///
/// Loading a package's sources happens once per package, inside the loop that
/// accumulates the build's errors. Returning that failure out of `compile_package`
/// carried it past the reporting loop at the end and dropped everything already on
/// the accumulator — "nothing is rendered and then dropped", which the second
/// standing invariant names outright. The fixture has two sibling dependencies that
/// fail in two different ways, so exactly one of them is the one that used to be
/// lost.
///
/// Mutation-checked by restoring the `?` on `load_package_sources_into` in
/// `compile_in_build` (and the `Result` return it needs): `acme-broken`'s
/// canonicalization error disappears from what comes back, and the `Many` assertion
/// below goes red.
#[test]
fn a_package_that_cannot_be_read_does_not_hide_an_earlier_packages_errors() {
    let root = fixture_package("package_two_failing_dependencies");

    let error = compile_package(&root).expect_err("neither dependency compiles");

    let reported = accumulated(&error);

    let loading_failed = reported
        .iter()
        .any(|error| matches!(unwrap_in_file(error), CompilationError::LoadingFiles(..)));
    assert!(
        loading_failed,
        "`acme-no-src` has no `src/`, so its loading failure must be reported, got {:?}",
        error
    );

    let canonicalization_failed = reported
        .iter()
        .any(|error| matches!(unwrap_in_file(error), CompilationError::Canonical(..)));
    assert!(
        canonicalization_failed,
        "`acme-broken`'s canonicalization error must survive the sibling package's \
         loading failure, got {:?}",
        error
    );
}

/// Only the packages a manifest's own `dependencies` names are importable. A
/// transitive one is in the build and is not reachable by name.
///
/// `acme-widgets` is compiled here — `acme-mid` depends on it and imports it — so
/// this is not a package missing from the build, but one deliberately absent from
/// `package-transitive-dependency`'s map of names.
///
/// Mutation-checked by seeding `compile_in_build`'s `interfaces` from `published`
/// directly instead of from `visible`: every package in the build becomes importable
/// from every other, the fixture compiles, and this goes red while every other
/// boundary test stays green.
#[test]
fn a_transitive_dependency_is_not_importable() {
    let root = fixture_package("package_transitive_dependency");

    // The middle package really does reach `acme-widgets`, so what fails below is the
    // boundary rule and not a broken chain.
    assert!(
        compile_package(&fixture_package("dep_mid")).is_ok(),
        "`acme-mid` writes `acme-widgets` in its own dependencies and must compile"
    );

    let error =
        compile_package(&root).expect_err("`acme-widgets` is not a dependency of this package");

    let messages: Vec<String> = accumulated(&error)
        .into_iter()
        .map(|error| unwrap_in_file(error).as_diagnostic().message)
        .collect();

    assert!(
        messages
            .iter()
            .any(|message| message.contains("AcmeWidgets.Size")),
        "the import that reached across two boundaries must be named, got {:?}",
        messages
    );
}

/// Two packages' same-named files are told apart in a diagnostic.
///
/// A build shares one file database, and a file is named in it by the path relative
/// to its own package's `src/`. Two packages may each hold a `Size.zel`, so that name
/// alone cannot say which package a diagnostic is about; the package it belongs to is
/// prefixed onto it.
///
/// Mutation-checked by ignoring `load_package_sources_into`'s `package` argument in
/// `SourceFile::load_private`: both files render as `Size.zel` and the inequality
/// below goes red.
#[test]
fn a_file_in_a_shared_database_is_named_by_its_package() {
    let widgets = PackageName::new("acme-widgets").unwrap();
    let collision = PackageName::new("package-module-name-collision").unwrap();

    let mut sources = SourceFiles::new();
    load_package_sources_into(
        &fixture_package("dep_widgets").join("src"),
        Some(&widgets),
        &mut sources,
    )
    .expect("the fixture loads");
    load_package_sources_into(
        &fixture_package("package_module_name_collision").join("src"),
        Some(&collision),
        &mut sources,
    )
    .expect("the fixture loads");

    let names: Vec<String> = sources
        .iter()
        .map(|(_, file)| file.file().name().clone())
        .filter(|name| name.ends_with("Size.zel"))
        .collect();

    assert_eq!(
        names.len(),
        2,
        "both fixtures must hold a `Size.zel` for this test to mean anything, got {:?}",
        names
    );
    assert!(
        names.contains(&"acme-widgets:Size.zel".to_string()),
        "a file has to name the package it belongs to, got {:?}",
        names
    );
    assert!(
        names.contains(&"package-module-name-collision:Size.zel".to_string()),
        "a file has to name the package it belongs to, got {:?}",
        names
    );
}
