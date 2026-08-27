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

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::dependencies::ModuleWalker;
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::source::load_package_sources;
use zelkova_lang::compiler::{
    check_module, compile_package, parser, CompilationError, Interface, PackageName,
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
/// `load_package_sources` builds its `WalkDir` with `filter_map(|r| r.ok())`, so
/// a missing or empty root is indistinguishable from a package with no modules:
/// zero sources, zero errors, and `compile_package` returns `Ok(())` having
/// compiled nothing. Any test that reads a green `compile_package` as evidence
/// the modules were fine has to establish first that there were modules.
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

// ── Test 1: Minimal passing module ───────────────────────────────────────────

#[test]
fn minimal_passing_module() {
    let source = indoc::indoc! {r#"
        module Test exposing (..)
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
        module Test exposing (..)
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
    interfaces.insert(module_a.name.name().clone(), module_a.to_interface());

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
        interfaces.insert(module.name.name().clone(), module.to_interface());
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
        basics_module.to_interface(),
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
        maybe_module.to_interface(),
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
        result_module.to_interface(),
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
    // `BUG-2` (see `docs/tickets/INDEX.md`) was about — the modules that checked
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

    let walker = ModuleWalker::new(&modules).expect("no dependency cycle in the fixture");
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let (checked, errors) = walker.check_in_order(&std_package(), &mut interfaces, check_module);

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
        interfaces.insert(checked.name.name().clone(), checked.to_interface());
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
/// The module list is asserted before compiling, and that is not decoration: a
/// missing or empty `std/core/src` yields zero modules and a green
/// `compile_package`, so `is_ok()` on its own would pass on a tree with no
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

// ── Test 16: a type error underlines the declaration that failed ─────────────

/// `ERR-3`: a type error must render with a `Label` under the failing declaration.
///
/// This is the ticket's acceptance criterion, and it asserts the label's **range**
/// rather than just `!labels.is_empty()`: a zero-width span, or one taken around the
/// layout pass's `OpenBlock`/`CloseBlock` (which are emitted zero-width, at the wrong
/// offset), would satisfy a mere non-emptiness check while pointing at nothing.
///
/// The expected range is computed from the source text so it stays honest if the
/// fixture is edited: from the first byte of the annotation to the last byte of the
/// body, because `parser::Module::from_declarations` merges the `FunType` span with
/// every `FunBinding` span. A mismatch between the two is what the error is *about*,
/// so underlining only one half would be underlining half the problem.
///
/// Mutation-checked three ways, each red on its own: making the `FunBinding`
/// production emit `NodeSpan::none()` (the range drops to the annotation alone);
/// dropping the `merge` in `from_declarations` so the function keeps only the last
/// declaration's span; and making `typer::Error::labels` return `Vec::new()`.
#[test]
fn type_error_labels_the_failing_declaration() {
    let root = fixture_package("package_type_error");
    assert_eq!(module_names(&root), vec!["Mismatch.zel"]);

    let source = std::fs::read_to_string(root.join("Mismatch.zel")).expect("fixture is readable");
    let start = source
        .find("answer : Int")
        .expect("fixture declares `answer : Int`");
    let body = "answer = true";
    let end = source.find(body).expect("fixture has a body") + body.len();

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
        1,
        "expected one label, got {:?}",
        diagnostic.labels
    );
    assert_eq!(
        diagnostic.labels[0].range,
        start..end,
        "the label must cover the whole declaration, `{}`",
        &source[start..end]
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
