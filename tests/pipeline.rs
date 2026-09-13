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
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::source::load_package_sources;
use zelkova_lang::compiler::{
    check_module, compile_package, parser, CompilationError, Interface, PackageName, PhaseError,
};

mod support;

use support::*;

// ── Helpers ──────────────────────────────────────────────────────────────────

fn std_package() -> PackageName {
    PackageName::new("zelkova", "core")
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

/// Root of one of the small package fixtures under `tests/fixtures/`.
///
/// `compile_package` takes a package directory, so the whole-package tests need
/// real directories on disk rather than the source strings the other layers use.
/// These fixtures stay small and single-purpose; `std/core/src` is exercised
/// separately by `stdlib_package_compiles`.
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
    let result = check_module(&test_package(), &interfaces, &parsed);
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
    let interfaces = HashMap::new();
    let result = check_module(&test_package(), &interfaces, &parsed);
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
    let interfaces = HashMap::new();
    let result = check_module(&test_package(), &interfaces, &parsed);
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
    let result = check_module(&test_package(), &interfaces, &parsed);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
    let module = result.unwrap();
    assert!(module.values.contains_key(&"wrap".into()));
}

// ── Test 5: check_module produces a valid interface usable by dependents ─────

#[test]
fn check_module_interface_can_be_used_by_dependent() {
    let pkg = test_package();
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();

    // First module: defines a local Maybe
    let source_a = indoc::indoc! {r#"
        module Lib exposing (..)
        type Option a = Some a | None
        wrap : a -> Option a
        wrap x = Some x
    "#};
    let parsed_a = parse_source(source_a);
    let module_a = check_module(&pkg, &interfaces, &parsed_a).expect("Lib should compile");
    interfaces.insert(module_a.name.name().clone(), module_a.to_interface(None));

    // Second module: imports and uses Lib
    let source_b = indoc::indoc! {r#"
        module App exposing (..)
        import Lib exposing (Option(..))
        answer : Option Int
        answer = Some 42
    "#};
    let parsed_b = parse_source(source_b);
    let result_b = check_module(&pkg, &interfaces, &parsed_b);
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
    let result = check_module(&test_package(), &interfaces, &parsed);
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
    let result = check_module(&std_package(), &interfaces, &parsed);
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
        let module = check_module(&pkg, &interfaces, &parsed)
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
    let basics_module = check_module(&pkg, &interfaces, &parsed_basics)
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
    let maybe_module = check_module(&pkg, &interfaces, &parsed_maybe)
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
    let result_module = check_module(&pkg, &interfaces, &parsed_result)
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
    assert_eq!(module_names(&root), vec!["Broken.zel", "Fine.zel"]);

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
    let sources = load_package_sources(&root)
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
        let checked = check_module(&pkg, &interfaces, &parsed)
            .unwrap_or_else(|e| panic!("{} failed: {:?}", module, e));
        interfaces.insert(checked.name.name().clone(), checked.to_interface(None));
    }

    assert!(interfaces.contains_key(&"Js.Bitwise".into()));
    assert!(interfaces.contains_key(&"Bitwise".into()));
}

// ── Test 13: the standard library is a package that compiles ─────────────────

/// `std/core/src` — what `cargo run` compiles — must compile cleanly.
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

    let result = compile_package(&src);

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
    let interfaces = HashMap::new();

    let error = check_module(&test_package(), &interfaces, &parsed)
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
    assert_eq!(module_names(&root), vec!["Mismatch.zel"]);

    let source = std::fs::read_to_string(root.join("Mismatch.zel")).expect("fixture is readable");
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

    let source = std::fs::read_to_string(root.join("Broken.zel")).expect("fixture is readable");
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
    assert_eq!(module_names(&root), vec!["Unknown.zel"]);

    let source = std::fs::read_to_string(root.join("Unknown.zel")).expect("fixture is readable");
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
    assert_eq!(module_names(&root), vec!["Ctor.zel"]);

    let source = std::fs::read_to_string(root.join("Ctor.zel")).expect("fixture is readable");
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
    assert_eq!(module_names(&root), vec!["Grouped.zel"]);

    let source = std::fs::read_to_string(root.join("Grouped.zel")).expect("fixture is readable");
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
    assert_eq!(module_names(&root), vec!["CaseBody.zel"]);

    let source = std::fs::read_to_string(root.join("CaseBody.zel")).expect("fixture is readable");
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
    assert_eq!(module_names(&root), vec!["A.zel", "B.zel", "Main.zel"]);

    let a_source = std::fs::read_to_string(root.join("A.zel")).expect("fixture is readable");
    let b_source = std::fs::read_to_string(root.join("B.zel")).expect("fixture is readable");
    let a_start = a_source.find("foo : Int").expect("A declares foo");
    let a_end = a_source.find("foo = 1").expect("A defines foo") + "foo = 1".len();
    let b_start = b_source.find("foo : Int").expect("B declares foo");
    let b_end = b_source.find("foo = 2").expect("B defines foo") + "foo = 2".len();

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
    assert_eq!(module_names(&root), vec!["Ops.zel", "User.zel"]);

    let ops_source = std::fs::read_to_string(root.join("Ops.zel")).expect("fixture is readable");
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
    assert_eq!(module_names(&root), vec!["CycleA.zel", "CycleB.zel"]);

    let a_source = std::fs::read_to_string(root.join("CycleA.zel")).expect("fixture is readable");
    let b_source = std::fs::read_to_string(root.join("CycleB.zel")).expect("fixture is readable");
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
    assert_eq!(module_names(&root), vec!["Lib.zel", "Main.zel"]);

    let source = std::fs::read_to_string(root.join("Main.zel")).expect("fixture is readable");
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
    assert_eq!(module_names(&root), vec!["Main.zel"]);

    let source = std::fs::read_to_string(root.join("Main.zel")).expect("fixture is readable");
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

    let widget_error = check_module(&pkg, &HashMap::new(), &parse_source(widget))
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
    let main_error = check_module(&pkg, &HashMap::new(), &parse_source(main))
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
fn check_importer(lib: &str, main: &str) -> Result<(), CompilationError> {
    let pkg = test_package();
    let lib_module = check_module(&pkg, &HashMap::new(), &parse_source(lib))
        .unwrap_or_else(|e| panic!("the exporting module should compile: {:?}", e));

    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    interfaces.insert(
        lib_module.name.name().clone(),
        lib_module.to_interface(None),
    );

    check_module(&pkg, &interfaces, &parse_source(main)).map(|_| ())
}

/// A module exposing three of its five declarations: a value, an opaque type and
/// a transparent one. `hidden` and `Secret` are the two nothing outside `Lib`
/// may reach, and `Opaque`'s constructor `Wrapped` is the third.
///
/// `Opaque` takes a type variable on purpose. `process_import`'s
/// `Privacy::Private` arm invents a zero-variable type when the interface does
/// not know the name (`BUG-16`), so a nullary opaque type would let an importer
/// write the annotation either way and no assertion below could tell the
/// interface carrying the type from the import fabricating it. With an arity of
/// one, `Opaque Int` only resolves when the real declaration crossed.
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
/// lists it. `plus` is exposed alongside it because an imported operator is
/// resolved through the unqualified name its `infix` declaration points at,
/// which is `BUG-15` and not this ticket's to fix.
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
/// `+` still resolving. `find_value`'s redirect (`BUG-15`) looks `add` up
/// unqualified in the *importer's* own scope, and `Exposing::Open` copying
/// `interface.values` in wholesale is the only thing that ever put it there;
/// once `values` is filtered by the header, that copy no longer carries a
/// value never exposed by name, unless something else keeps it reachable.
///
/// `Lib` here has exactly `Basics`' shape: one operator exposed, its backing
/// function not exposed at all.
///
/// Mutation-checked by dropping the `interface.infix_functions` loop from
/// `process_import`'s `Exposing::Open` arm (`src/compiler/canonical/environment.rs`),
/// which turns this red with `cannot find a value named \`Main.<+>\``.
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

/// The other half of the fix above: keeping `plus` reachable for the operator
/// redirect must not make `plus` importable *by its own name* — that would
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

// ── Test 28: a missing package root is reported, not compiled as success ─────

/// `BUG-21`: `load_package_sources` walked the package root with `WalkDir` and
/// discarded every `Err` the walk produced (`.filter_map(|r| r.ok())`). A root
/// that doesn't exist makes `WalkDir` yield exactly one `Err` and then stop, so
/// that discard turned a missing package root into an empty `SourceFiles` —
/// zero modules, zero errors, and `compile_package` returning `Ok(())`.
///
/// Mutation-checked by restoring the `filter_map(|r| r.ok())` discard in
/// `load_package_sources` (`src/compiler/source/mod.rs`): with the walk error
/// thrown away, `compile_package` returns `Ok(())` on this same missing root,
/// and `expect_err` below panics.
#[test]
fn compile_package_reports_a_missing_root() {
    let root = fixture_package("this-package-root-does-not-exist");
    assert!(
        !root.exists(),
        "fixture path must not exist for this test to mean anything"
    );

    let error = compile_package(&root)
        .expect_err("a package root that does not exist must not compile as success");

    let CompilationError::LoadingFiles(errors) = &error else {
        panic!(
            "expected Err(CompilationError::LoadingFiles(..)), got {:?}",
            error
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
        message.contains(&root.to_string_lossy().to_string()),
        "expected the missing root's path in the error message, got {:?}",
        message
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
    let sources = load_package_sources(&root)
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
/// "`+` resolved" apart from "`+` resolved to the right module". An operator keeps
/// its own spelling here rather than its `infix` declaration's backing function:
/// `find_value` redirects through `infixes` to look the value up, and
/// `Expression::from_parser` then qualifies the name that was written.
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

/// `LANG-8`: a module that writes no `import` at all still resolves `+`, and
/// resolves it to `Basics`.
///
/// `Implicit.zel` in the fixture is three lines and none of them is an `import`,
/// so `+` can only have arrived through the default import list. Asserting on the
/// `VarForeign` rather than on `is_ok()` is the difference between "it compiled"
/// and "it compiled because `Basics` was in scope": a module that resolved `+`
/// some other way would satisfy the first and not the second.
///
/// Mutation-checked by dropping the `implicit` half of `new_environment`'s import
/// loop: `Implicit` then fails with a `VariableNotFound` for `+` and never reaches
/// `checked`, so `checked_value` panics.
#[test]
fn default_imports_resolve_without_an_import_line() {
    let root = fixture_package("package_default_imports");
    assert_eq!(
        module_names(&root),
        vec!["Basics.zel", "Explicit.zel", "Implicit.zel"]
    );

    let source = std::fs::read_to_string(root.join("Implicit.zel")).expect("fixture is readable");
    assert!(
        !source.contains("import"),
        "the point of this fixture is that `Implicit.zel` writes no import: {:?}",
        source
    );

    let checked = check_fixture("package_default_imports");
    let x = checked_value(&checked, "Implicit", "x");

    assert_eq!(
        foreign_names(x),
        vec!["Basics.+".to_string()],
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
        vec!["Basics.+".to_string()],
        "a written default import must resolve the same way the implicit one does"
    );

    assert!(
        compile_package(&fixture_package("package_default_imports")).is_ok(),
        "the fixture package must compile as a whole"
    );
}
