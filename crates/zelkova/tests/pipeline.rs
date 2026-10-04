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
use zelkova::{compile_package, compile_package_with_tests, BuildError, BUILD_DIRECTORY};
use zelkova_compiler::canonical;
use zelkova_compiler::dependencies::Outcome;
use zelkova_compiler::dependencies::{self, ModuleWalker};
use zelkova_compiler::ir::{TermPatternKind, TypedTermKind};
use zelkova_compiler::manifest;
use zelkova_compiler::name::Name;
use zelkova_compiler::resolve;
use zelkova_compiler::source::{
    load_package_sources, load_package_sources_into, Overlay, SourceFiles, SourceRoot,
};
use zelkova_compiler::typer;
use zelkova_compiler::{
    check_module, check_module_recovering, check_package, check_package_with_tests, CheckedModule,
    CompilationError, Interface, PackageName, PhaseError,
};
use zelkova_syntax::parser;
use zelkova_test_runner::collection;

#[path = "../../zelkova-compiler/tests/support/mod.rs"]
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
    // The repository root is two levels above `CARGO_MANIFEST_DIR` at build time.
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    Path::new(&manifest).join("../..").join("std/core/src")
}

/// `std/core`, the package directory `compile_package` now takes — `zelkova.toml`
/// beside `src/`, as opposed to [`std_src`], which stays pointed at the source
/// root itself for the tests here that parse individual files or drive
/// `load_package_sources` directly.
fn std_package_root() -> std::path::PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    Path::new(&manifest).join("../..").join("std/core")
}

/// `std/test`, the `zelkova-test` package — [`std_package_root`], for `zelkova-test`
/// rather than `zelkova-core`.
fn zelkova_test_package_root() -> std::path::PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    Path::new(&manifest).join("../..").join("std/test")
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
    Path::new(&manifest)
        .join("../..")
        .join("tests/fixtures")
        .join(name)
}

/// The `.zel` modules `compile_package` would pick up under one source root of the
/// package at `package_dir`, sorted, each named as a diagnostic names it.
///
/// An *existing* root holding no `.zel` files at all still loads as zero sources
/// and zero errors, indistinguishable from a package with no modules. Any test
/// that reads a green `compile_package` as evidence the modules were fine has to
/// establish first that there were modules. (A `src/` that doesn't exist is a
/// different case: `load_package_sources` reports that as an error — see
/// `BUG-21` in `docs/tickets/README.md`. A missing `tests/` is neither: it is a
/// package with no tests.)
fn module_names(package_dir: &Path, root: SourceRoot) -> Vec<String> {
    let sources = load_package_sources(package_dir, root).unwrap_or_else(|e| {
        panic!(
            "failed to load {} sources from {:?}: {:?}",
            root, package_dir, e
        )
    });
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
/// `CompilationError::InFile` by `check_package`, which is what pairs the spans a
/// phase produced with the file to underline. A test that asserts on the phase
/// variant looks through that wrapper rather than at it.
fn unwrap_in_file(error: &CompilationError) -> &CompilationError {
    match error {
        CompilationError::InFile(inner, _) => unwrap_in_file(inner),
        other => other,
    }
}

/// The errors of the check a failed build accumulated, each past the
/// [`BuildError::Check`] it is wrapped in.
///
/// `error` must be [`BuildError::Many`] — the accumulator, already rendered — and each of
/// its members a `Check`: a test that calls this expects the check, and not emitting or
/// writing the build, to have failed, so anything else panics with what it got.
fn many(error: &BuildError) -> Vec<&CompilationError> {
    let BuildError::Many(errors) = error else {
        panic!("expected Err(BuildError::Many(..)), got {:?}", error);
    };
    errors
        .iter()
        .map(|member| match member {
            BuildError::Check(error) => error,
            other => panic!("expected every member to be a Check, got {:?}", other),
        })
        .collect()
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
    let interfaces = HashMap::from([basics_interface()]);
    let result = check_module(&test_package(), &interfaces, &parsed);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
    let module = result.unwrap();
    assert_eq!(module.canonical.values.len(), 3);
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
    let result = check_module(&test_package(), &interfaces, &parsed);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
    let module = result.unwrap();
    assert!(module.canonical.types.contains_key(&"Shape".into()));
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
    assert!(module.canonical.values.contains_key(&"wrap".into()));
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
    let module_a = check_module(&pkg, &interfaces, &parsed_a).expect("Lib should compile");
    interfaces.insert(
        module_a.canonical.name.name().clone(),
        module_a.to_interface(None),
    );

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
        interfaces.insert(
            module.canonical.name.name().clone(),
            module.to_interface(None),
        );
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
        basics_module.canonical.name.name().clone(),
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
        maybe_module.canonical.name.name().clone(),
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
        result_module.canonical.name.name().clone(),
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
/// `compile_package` return `Err(BuildError::Many(errors))`
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
        module_names(&root, SourceRoot::Src),
        vec!["src/Broken.zel", "src/Fine.zel"]
    );

    let result = compile_package(&root);

    match result {
        Err(error @ BuildError::Many(_)) => {
            let errors = many(&error);
            assert_eq!(
                errors.len(),
                1,
                "expected exactly one error for the one broken module, got {:?}",
                errors
            );
            match unwrap_in_file(errors[0]) {
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
        other => panic!("expected Err(BuildError::Many(..)), got {:?}", other),
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
/// This drives the *real* `check_module_recovering`, which is what the `dummy_check` unit
/// test in `crates/zelkova-compiler/src/dependencies.rs` cannot do. Mutation-checked by
/// keeping only the `Failed` outcomes whenever one is present (`outcomes.retain(..)`
/// before `check_in_order` returns): that turns the `Fine` assertion below red.
#[test]
fn check_in_order_keeps_passing_siblings_with_the_real_checker() {
    let root = fixture_package("package_canonicalize_fails");
    let sources = load_package_sources(&root, SourceRoot::Src)
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
    let walker = ModuleWalker::new(&modules, &module_files, &std_package())
        .expect("no dependency cycle in the fixture");
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let (checked, errors) = checked_and_errors(walker.check_in_order(
        &std_package(),
        &mut interfaces,
        &module_files,
        check_module_recovering,
    ));

    let checked_names: Vec<String> = checked
        .iter()
        .map(|m| m.canonical.name.name().as_str().to_string())
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
        interfaces.insert(
            checked.canonical.name.name().clone(),
            checked.to_interface(None),
        );
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
    assert_eq!(
        module_names(&std_package_root(), SourceRoot::Src),
        vec![
            "src/Basics.zel",
            "src/Bitwise.zel",
            "src/Js/Basics.zel",
            "src/Js/Bitwise.zel",
            "src/Js/Utils.zel",
            "src/Maybe.zel",
            "src/Result.zel",
            "src/String.zel",
            "src/Task.zel",
            "src/Tuple.zel",
        ]
    );

    let result = compile_package(&std_package_root());

    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// `LANG-73`: `String` reaches an ordinary module through [the default
/// imports](../docs/spec/modules.md#the-default-imports) with no `import` written for
/// it, now that `std/core/src/String.zel` compiles — against the real `std/core`
/// rather than a fixture double, since the point under test is whether that module
/// resolves.
///
/// Mutation-checked by reverting `std/core/src/String.zel` to its pre-fix, non-compiling
/// state (renaming it back to `.ignored`): `String` then names nothing and `f`'s
/// annotation fails canonicalization with a `TypeNotFound`, and `compile_package`
/// comes back `Err` instead of `Ok`.
#[test]
fn a_dependent_of_std_core_can_annotate_string_with_no_import() {
    let result = compile_package(&fixture_package("package_uses_string"));

    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// `LANG-63`: `std/test`, the `zelkova-test` package, compiles on its own — the same
/// shape [`stdlib_package_compiles`] pins for `std/core`, and for the same reason: an
/// *existing* but empty `std/test/src` would still yield zero modules and a green
/// `compile_package`, so the module list is asserted first.
#[test]
fn zelkova_test_package_compiles() {
    assert_eq!(
        module_names(&zelkova_test_package_root(), SourceRoot::Src),
        vec!["src/Test.zel"]
    );

    let result = compile_package(&zelkova_test_package_root());

    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// Every checked module of `std/core`, the way [`check_fixture`] reads a smaller
/// fixture package — needed here rather than [`compile_package`] because
/// `zelkova_js::emit` reads a module's [`CheckedModule`], which `compile_package` does
/// not hand back.
fn check_std_core() -> Vec<CheckedModule> {
    let root = std_package_root();
    let sources = load_package_sources(&root, SourceRoot::Src)
        .unwrap_or_else(|e| panic!("failed to load sources from {:?}: {:?}", root, e));
    let modules: Vec<parser::Module> = sources
        .iter()
        .map(|(_, file)| {
            parser::parse(file.file())
                .unwrap_or_else(|e| panic!("parse error in {:?}: {:?}", file.file().name(), e))
        })
        .collect();

    let module_files = HashMap::new();
    let walker = ModuleWalker::new(&modules, &module_files, &std_package())
        .expect("no dependency cycle in std/core");
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let (checked, errors) = checked_and_errors(walker.check_in_order(
        &std_package(),
        &mut interfaces,
        &module_files,
        check_module_recovering,
    ));
    assert!(
        errors.is_empty(),
        "std/core must check clean, got {:?}",
        errors
    );

    checked
}

/// The three `Js/*` facades under `std/core` are each marked `unsafe` throughout
/// (`GEN-12`'s own `std/core` survey), so `zelkova_js::emit` answers a module for
/// every one of them now, rather than refusing the whole tree the way a blanket
/// `module foreign` check used to.
///
/// The unit tests in `crates/zelkova-js/tests/javascript.rs` pin the *shape* of what a facade emits as;
/// this only pins that the three real signatures do not hit an edge their small
/// fixtures miss — among them, that every result type they declare has a boundary
/// predicate, which is read off the result's type.
///
/// Mutation-checked by reverting `emit`'s `ir.foreign` branch to the old blanket
/// refusal: this test then panics on the first facade.
#[test]
fn the_stdlib_facades_emit() {
    let checked = check_std_core();

    for name in ["Js.Basics", "Js.Bitwise", "Js.Utils"] {
        let module = checked
            .iter()
            .find(|m| m.canonical.name.name().as_str() == name)
            .unwrap_or_else(|| panic!("{} did not check", name));

        zelkova_js::emit(module, true, &zelkova_js::Unions::of(&checked))
            .unwrap_or_else(|errors| panic!("{} failed to emit: {:?}", name, errors));
    }
}

/// `CLAUDE.md`'s architecture table claims `zelkova_js::emit` "emits every module of
/// `std/core`, `case` included" — true (`Basics`, `Maybe`, `Result` and `Tuple` all lean
/// on `case`), but until this test nothing pinned it: `the_stdlib_facades_emit` above
/// only loops over the three `Js/*` facades, and `the_stdlib_bitwise_forwards_to_its_
/// facade` below only names `Bitwise`. Looping over every module `check_std_core`
/// returns, rather than naming the four again by hand, means a ninth `std/core` module
/// is covered automatically the day one exists, and CLAUDE.md's "every module" is never
/// only hand-verified again the way it was for this PR.
///
/// Mutation-checked by reverting one of `Basics`, `Maybe`, `Result` or `Tuple`'s
/// `case`-using declarations to something `zelkova_js::emit` refuses (an easy probe:
/// temporarily making `case_expression` always push `Error::Unsupported`) — this test
/// panics on the first module that stops emitting; the loop below over the *complete*
/// set is what makes that regression visible here instead of staying silent because a
/// narrower list happened not to include the broken module.
#[test]
fn every_stdlib_module_emits() {
    let checked = check_std_core();

    for module in &checked {
        let name = module.canonical.name.name().as_str();
        zelkova_js::emit(module, true, &zelkova_js::Unions::of(&checked))
            .unwrap_or_else(|errors| panic!("{} failed to emit: {:?}", name, errors));
    }
}

/// `std/core`'s `Bitwise` forwards every one of its declarations to `Js.Bitwise`,
/// written qualified — `and = Js.Bitwise.and` — so every one is type checked against
/// the facade's interface and the module emits.
///
/// `and` is a parameterless binding, so an importer calls it one argument at a time, and
/// its value is `Js.Bitwise.and`, a function of two parameters used as a value: it is
/// `$curry`'d at its declaration. Emitting the bare import instead is mutation-checked by
/// making `Emitter::value`'s `ReferenceKind::Foreign` arm return `local` whatever the
/// arity.
///
/// Mutation-checked two ways, each making `emit` refuse all seven declarations as
/// unchecked: dropping the loop over `interfaces` from `type_check`'s first pass, and
/// qualifying a `VarForeign` with the whole written spelling in
/// `Expression::from_parser` (`m.qualify_name(name)`), which names
/// `Js.Bitwise.Js.Bitwise.and` and finds nothing.
#[test]
fn the_stdlib_bitwise_forwards_to_its_facade() {
    let checked = check_std_core();
    let bitwise = checked
        .iter()
        .find(|m| m.canonical.name.name().as_str() == "Bitwise")
        .expect("Bitwise did not check");

    let text = zelkova_js::emit(bitwise, false, &zelkova_js::Unions::of(&checked))
        .unwrap_or_else(|errors| panic!("Bitwise failed to emit: {:?}", errors));

    assert!(
        text.contains("const and = $curry(zelkova_core$Js$Bitwise$and, 2);"),
        "got:\n{}",
        text
    );
}

// ── Writing the build ────────────────────────────────────────────────────────

/// An empty directory under Cargo's per-target scratch space, named for the test using
/// it, for a build to write into.
fn fresh_build_dir(test: &str) -> std::path::PathBuf {
    let dir = Path::new(env!("CARGO_TARGET_TMPDIR")).join(test);
    if dir.exists() {
        std::fs::remove_dir_all(&dir).unwrap();
    }
    dir
}

/// Every file below `dir`, as sorted `/`-separated paths relative to it — empty when
/// `dir` does not exist.
fn files_under(dir: &Path) -> Vec<String> {
    fn walk(root: &Path, dir: &Path, out: &mut Vec<String>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries {
            let path = entry.unwrap().path();
            if path.is_dir() {
                walk(root, &path, out);
            } else {
                let relative = path.strip_prefix(root).unwrap();
                let parts: Vec<String> = relative
                    .components()
                    .map(|c| c.as_os_str().to_string_lossy().into_owned())
                    .collect();
                out.push(parts.join("/"));
            }
        }
    }

    let mut out = Vec::new();
    walk(dir, dir, &mut out);
    out.sort();
    out
}

/// `GEN-13`: a build that checks writes `out/js/` below its build directory — the runtime at
/// its root, then one directory per package of the build holding one file per module,
/// named after the module within its own package and never the namespace the dependent
/// writes (`AcmeWidgets.Size` is `acme-widgets/Size.mjs`), with a facade's companion
/// beside it under its own name. A private module is emitted like any other, since its
/// own package's modules import it. An import across the package boundary reaches the
/// sibling package's directory.
///
/// Mutation-checked three ways: dropping the push of the companion in `emit_build` loses
/// `Native.companion.mjs`; writing each module to its package-less `module_file` puts
/// every file at the root of `out/js/`; and dropping the package comparison in
/// `zelkova_js::module_specifier` has `App.mjs` import `./Size.mjs`. Each turns this red.
#[test]
fn a_build_writes_one_directory_per_package() {
    let build_dir = fresh_build_dir("a_build_writes_one_directory_per_package");

    let result = zelkova::compile_package_into(
        &fixture_package("package_namespaced_dependency"),
        &build_dir,
    );
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    assert_eq!(
        files_under(&build_dir),
        vec![
            "out/js/acme-widgets/Hidden.mjs",
            "out/js/acme-widgets/Native.companion.mjs",
            "out/js/acme-widgets/Native.mjs",
            "out/js/acme-widgets/Size.mjs",
            "out/js/package-namespaced-dependency/App.mjs",
            "out/js/zelkova.mjs",
        ]
    );

    let js = build_dir.join("out").join("js");
    assert_eq!(
        std::fs::read_to_string(js.join("zelkova.mjs")).unwrap(),
        zelkova_js::RUNTIME
    );
    assert_eq!(
        std::fs::read_to_string(js.join("acme-widgets/Native.companion.mjs")).unwrap(),
        std::fs::read_to_string(fixture_package("dep_widgets").join("src/Native.mjs")).unwrap()
    );
    let app = std::fs::read_to_string(js.join("package-namespaced-dependency/App.mjs")).unwrap();
    assert!(
        app.starts_with(
            "import { small as acme_widgets$Size$small } from \"../acme-widgets/Size.mjs\";\n"
        ),
        "got:\n{}",
        app
    );
}

/// `GEN-13`'s acceptance, the same tree `cargo run` writes: `std/core`'s ten modules,
/// the three `Js/*` companions beside their facades, and the runtime.
///
/// Mutation-checked by leaving out the runtime `emit_build` starts from: `out/js/zelkova.mjs`
/// goes missing and this turns red.
#[test]
fn the_stdlib_build_writes_every_module_and_companion() {
    let build_dir = fresh_build_dir("the_stdlib_build_writes_every_module_and_companion");

    let result = zelkova::compile_package_into(&std_package_root(), &build_dir);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    assert_eq!(
        files_under(&build_dir),
        vec![
            "out/js/zelkova-core/Basics.mjs",
            "out/js/zelkova-core/Bitwise.mjs",
            "out/js/zelkova-core/Js/Basics.companion.mjs",
            "out/js/zelkova-core/Js/Basics.mjs",
            "out/js/zelkova-core/Js/Bitwise.companion.mjs",
            "out/js/zelkova-core/Js/Bitwise.mjs",
            "out/js/zelkova-core/Js/Utils.companion.mjs",
            "out/js/zelkova-core/Js/Utils.mjs",
            "out/js/zelkova-core/Maybe.mjs",
            "out/js/zelkova-core/Result.mjs",
            "out/js/zelkova-core/String.mjs",
            "out/js/zelkova-core/Task.mjs",
            "out/js/zelkova-core/Tuple.mjs",
            "out/js/zelkova.mjs",
        ]
    );
}

/// `GEN-13`: a build with a failing module writes no file at all — not the runtime, not
/// a dependency's modules that checked cleanly, and not the failing package's own
/// modules that did check.
///
/// `package_type_error` depends on `zelkova-core` (`dep_core`, which is what gives
/// `Mismatch`'s annotation its `Int`) and on `acme-widgets` (`dep_widgets`), which
/// nothing here imports; both check without error, so their modules *do* reach
/// `checked` before the failure is known — unlike `package_type_error`'s own
/// `Mismatch`, which never gets there at all: `compile_in_build` returns `None` for a
/// package with any error, which drops every module of that package before `compile`'s
/// `checked` ever sees them. So the two guards this test pins are not interchangeable
/// with what keeps a failing package's own modules off disk — that is
/// `compile_in_build`'s `None`, not either guard in `compile` — and this fixture exists
/// specifically so two dependencies' modules are the ones the guards in `compile` are
/// the *only* thing keeping off disk.
///
/// Mutation-checked by emitting and writing whatever checked without looking at the
/// errors — both `if errors.is_empty()` guards around the codegen step in `compile`
/// replaced with `if true`. That writes the runtime, `zelkova-core`'s modules and
/// `acme-widgets`'s modules — `package_type_error`'s own `Mismatch.mjs` still does not
/// appear, because it was never in `checked` to begin with — and turns this red.
/// Removing only the outer guard leaves it green, correctly: the inner one still sees
/// the type error and writes nothing.
#[test]
fn a_build_with_a_failing_module_writes_nothing() {
    let build_dir = fresh_build_dir("a_build_with_a_failing_module_writes_nothing");

    let error = zelkova::compile_package_into(&fixture_package("package_type_error"), &build_dir)
        .expect_err("`Mismatch` does not type check");

    // The failure is the type error, not something the build step raised.
    let errors = many(&error);
    assert!(
        matches!(
            errors.as_slice(),
            [CompilationError::InFile(inner, _)] if matches!(**inner, CompilationError::Type(..))
        ),
        "got {:?}",
        errors
    );

    assert_eq!(files_under(&build_dir), Vec::<String>::new());
    assert!(!build_dir.exists());
}

/// `BUG-1`, one level up: a build whose check found an error fails, and the error the driver
/// hands back is the accumulator — `BuildError::Many` — holding the check's error as one
/// `Check` member, still wrapped in the `InFile` the check paired it with its file by.
///
/// `TOOL-7` moved the accumulator that decides the return value from the compiler into
/// the driver, so this pins the driver's half: `package_type_error`'s one type error must
/// come back as exactly that shape, not as a success and not as anything flatter.
///
/// Mutation-checked by making the driver's `compile` return `Ok(root_test_interfaces)`
/// whatever its accumulator holds: the build then comes back `Ok` and this turns red.
#[test]
fn a_build_whose_check_failed_returns_the_check_error_in_many() {
    let build_dir = fresh_build_dir("a_build_whose_check_failed_returns_the_check_error_in_many");

    let result = zelkova::compile_package_into(&fixture_package("package_type_error"), &build_dir);

    match &result {
        Err(BuildError::Many(errors)) => assert!(
            matches!(
                errors.as_slice(),
                [BuildError::Check(CompilationError::InFile(inner, _))]
                    if matches!(**inner, CompilationError::Type(..))
            ),
            "expected one Check around an InFile, got {:?}",
            errors
        ),
        other => panic!("expected Err(BuildError::Many(..)), got {:?}", other),
    }
}

/// `GEN-13`: a module that checks and cannot be emitted fails the build like any other
/// error, and nothing is written. The facade here has no companion beside it, which
/// [`zelkova_js::emit`] refuses.
///
/// Mutation-checked by dropping the second `if errors.is_empty()` in `compile`, so
/// files are written whether or not emission failed: the runtime is written and this
/// turns red.
///
/// The error is matched as `BuildError::InFile` around `BuildError::Emit`, the driver's
/// own variants since `TOOL-7`. Mutation-checked by making `emit_modules` drop the error
/// it pushes instead of pushing it: the build then comes back `Ok` and this turns red.
#[test]
fn a_build_that_cannot_be_emitted_writes_nothing() {
    let package = fresh_build_dir("a_build_that_cannot_be_emitted_writes_nothing_package");
    std::fs::create_dir_all(package.join("src")).unwrap();
    std::fs::write(
        package.join("zelkova.toml"),
        "name = \"no-companion\"\nversion = \"0.1.0\"\nprivate-modules = []\n\n[dependencies]\n\n[test-dependencies]\n",
    )
    .unwrap();
    std::fs::write(
        package.join("src/Answer.zel"),
        indoc::indoc! {r#"
            module Answer exposing (Answer(..))

            type Answer = Yes
        "#},
    )
    .unwrap();
    std::fs::write(
        package.join("src/Native.zel"),
        indoc::indoc! {r#"
            module foreign Native exposing (answer)

            import Answer exposing (Answer)

            unsafe answer : Answer
        "#},
    )
    .unwrap();
    let build_dir = package.join("build");

    let error = zelkova::compile_package_into(&package, &build_dir)
        .expect_err("a facade with no companion cannot be emitted");

    let BuildError::Many(errors) = &error else {
        panic!("expected Many, got {:?}", error);
    };
    assert!(
        matches!(
            errors.as_slice(),
            [BuildError::InFile(inner, _)]
                if matches!(
                    &**inner,
                    BuildError::Emit(emit_errors, _)
                        if matches!(emit_errors.as_slice(), [zelkova_js::Error::MissingCompanion { .. }])
                )
        ),
        "got {:?}",
        errors
    );
    assert_eq!(
        error.as_diagnostic().notes,
        vec![
            "[Native] the facade `Native` has no companion for the `javascript` target".to_string()
        ]
    );

    assert!(!build_dir.exists(), "got {:?}", files_under(&build_dir));
}

/// `GEN-18`: a test build whose `tests/` root holds a module that checks but cannot be
/// emitted writes neither tree — not `test/js/`, and not `out/js/` either, though every
/// module of `src/` emitted cleanly. The facade here sits under `tests/` with no
/// companion beside it, which [`zelkova_js::emit`] refuses.
///
/// Mutation-checked by moving the test tree's `emit_modules` call in `compile` back
/// after the write of `out/js/`: `out/js/` is written in full and this turns red.
///
/// The error is matched as `BuildError::InFile` around `BuildError::Emit`, the driver's
/// own variants since `TOOL-7`. Mutation-checked by making `emit_modules` drop the error
/// it pushes instead of pushing it: the build then comes back `Ok` and this turns red.
#[test]
fn a_test_build_whose_tests_cannot_be_emitted_writes_nothing() {
    let package =
        fresh_build_dir("a_test_build_whose_tests_cannot_be_emitted_writes_nothing_package");
    std::fs::create_dir_all(package.join("src")).unwrap();
    std::fs::create_dir_all(package.join("tests")).unwrap();
    std::fs::write(
        package.join("zelkova.toml"),
        "name = \"no-test-companion\"\nversion = \"0.1.0\"\nprivate-modules = []\n\n[dependencies]\n\n[test-dependencies]\n",
    )
    .unwrap();
    std::fs::write(
        package.join("src/Answer.zel"),
        indoc::indoc! {r#"
            module Answer exposing (Answer(..))

            type Answer = Yes
        "#},
    )
    .unwrap();
    std::fs::write(
        package.join("tests/Native.zel"),
        indoc::indoc! {r#"
            module foreign Native exposing (answer)

            import Answer exposing (Answer)

            unsafe answer : Answer
        "#},
    )
    .unwrap();
    let build_dir = package.join("build");

    let error = zelkova::compile_package_with_tests_into(&package, &build_dir)
        .expect_err("a facade under `tests/` with no companion cannot be emitted");

    let BuildError::Many(errors) = &error else {
        panic!("expected Many, got {:?}", error);
    };
    assert!(
        matches!(
            errors.as_slice(),
            [BuildError::InFile(inner, _)]
                if matches!(
                    &**inner,
                    BuildError::Emit(emit_errors, _)
                        if matches!(emit_errors.as_slice(), [zelkova_js::Error::MissingCompanion { .. }])
                )
        ),
        "got {:?}",
        errors
    );

    assert!(!build_dir.exists(), "got {:?}", files_under(&build_dir));
}

/// A test companion imports the companion it checks by their path in the source tree,
/// out of `tests/` and into `src/`
/// ([*Testing a companion*](../docs/spec/interop.md#testing-a-companion)). The test tree
/// holds both in one package directory under their `.companion.mjs` names, so that import
/// is written there as the path between the two — and only the test companion's text
/// changes: the companion under test is copied as it is.
///
/// Mutation-checked by leaving `companion_imports` empty in `compile`: the test companion
/// is copied byte for byte, keeps `../src/Native.mjs`, and this turns red.
#[test]
fn a_test_companion_imports_the_companion_it_checks_by_its_build_path() {
    let package = fresh_build_dir(
        "a_test_companion_imports_the_companion_it_checks_by_its_build_path_package",
    );
    std::fs::create_dir_all(package.join("src")).unwrap();
    std::fs::create_dir_all(package.join("tests")).unwrap();
    std::fs::write(
        package.join("zelkova.toml"),
        "name = \"checked-companion\"\nversion = \"0.1.0\"\nprivate-modules = []\n\n[dependencies]\n\n[test-dependencies]\n",
    )
    .unwrap();
    std::fs::write(
        package.join("src/Native.zel"),
        indoc::indoc! {r#"
            module foreign Native exposing (answer)

            unsafe answer : ()
        "#},
    )
    .unwrap();
    std::fs::write(
        package.join("src/Native.mjs"),
        "export const answer = undefined;\n",
    )
    .unwrap();
    std::fs::write(
        package.join("tests/NativeChecks.zel"),
        indoc::indoc! {r#"
            module foreign NativeChecks exposing (answerIsUnit)

            unsafe answerIsUnit : ()
        "#},
    )
    .unwrap();
    std::fs::write(
        package.join("tests/NativeChecks.mjs"),
        "import { answer } from '../src/Native.mjs';\nexport const answerIsUnit = answer;\n",
    )
    .unwrap();
    let build_dir = package.join("build");

    zelkova::compile_package_with_tests_into(&package, &build_dir).unwrap();

    let written = |path: &str| std::fs::read_to_string(build_dir.join(path)).unwrap();
    assert_eq!(
        written("test/js/checked-companion/NativeChecks.companion.mjs"),
        "import { answer } from './Native.companion.mjs';\nexport const answerIsUnit = answer;\n"
    );
    assert_eq!(
        written("test/js/checked-companion/Native.companion.mjs"),
        "export const answer = undefined;\n"
    );
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
/// above it: `format!("{:?}", e)` on the same error also contains "Int" and "Char".
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
        answer = 'a'
    "#};
    let parsed = parse_source(source);
    let interfaces = HashMap::from([basics_interface()]);

    let error = check_module(&test_package(), &interfaces, &parsed)
        .expect_err("`answer : Int` with a `Char` body must not type-check");

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
        message.contains("Char"),
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

    let errors = many(&error);
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
/// name. It can do better now: the caret is under `'a'`, and a *secondary* label
/// under `answer : Int` says where `Int` was expected from. Both ranges are computed
/// from the fixture text, and both matter: a primary label that had widened back out
/// to the declaration would still be "a label", and a missing secondary would leave
/// the reader to guess why `Int` was expected at all.
///
/// Mutation-checked four ways, each red on its own: making `canonical_expr_to_term`
/// build its terms with `NodeSpan::none()` (the primary falls back to the whole
/// declaration); dropping `annotation_span` from `Value::TypedValue` in favour of
/// `NodeSpan::none()` (the secondary disappears); pushing the annotation constraint
/// *after* `constraint::collect` in `infer_annotated` (the primary moves off `'a'`);
/// and having `Substitution::apply` return `c.origin.clone()` unchanged, so nothing
/// is ever explained (the secondary disappears).
#[test]
fn type_error_labels_the_expression_that_disagrees() {
    let root = fixture_package("package_type_error");
    assert_eq!(
        module_names(&root, SourceRoot::Src),
        vec!["src/Mismatch.zel"]
    );

    let source = std::fs::read_to_string(root.join("src").join("Mismatch.zel"))
        .expect("fixture is readable");
    let annotation = "answer : Int";
    let annotation_start = source
        .find(annotation)
        .expect("fixture declares `answer : Int`");
    let body = "'a'";
    let body_start = source
        .rfind(body)
        .expect("fixture's body is the literal `'a'`");

    let error =
        compile_package(&root).expect_err("`answer : Int` with a `Char` body must not compile");

    let errors = many(&error);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    // The phase is part of the contract: this must be the type error, not a
    // canonicalization failure that happened to land on the same line.
    match unwrap_in_file(errors[0]) {
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

    let errors = many(&error);
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
    assert_eq!(
        module_names(&root, SourceRoot::Src),
        vec!["src/Unknown.zel"]
    );

    let source =
        std::fs::read_to_string(root.join("src").join("Unknown.zel")).expect("fixture is readable");
    let identifier = "mystery";
    let start = source
        .find(identifier)
        .expect("fixture uses an undefined `mystery`");

    let error = compile_package(&root).expect_err("an undefined name must not compile");

    let errors = many(&error);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(errors[0]) {
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
/// conversion, `Pattern::from_parser`, and the other grammar site — `Pattern`'s
/// nullary-constructor alternative, which spans a bare constructor used as a function
/// argument. Taking it from a
/// binding pattern rather than a `case` branch keeps this error out of
/// `Error::Many`, so it pins the pattern span on its own; the grouping is
/// `grouped_canonical_error_keeps_every_label` below.
///
/// Mutation-checked two ways, each red on its own: making `Pattern`'s nullary
/// `QualTypeIdent` alternative emit `NodeSpan::none()`, and dropping the span from
/// `canonical::Error::VariantNotFound`'s `labels` arm.
#[test]
fn unknown_constructor_labels_the_pattern() {
    let root = fixture_package("package_unknown_constructor");
    assert_eq!(module_names(&root, SourceRoot::Src), vec!["src/Ctor.zel"]);

    let source =
        std::fs::read_to_string(root.join("src").join("Ctor.zel")).expect("fixture is readable");
    let constructor = "Purple";
    let start = source
        .find(constructor)
        .expect("fixture matches on an undeclared `Purple`");

    let error = compile_package(&root).expect_err("an undeclared constructor must not compile");

    let errors = many(&error);
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
/// The two case branches in the fixture each use an operator nothing declares, and
/// `Expression::from_parser` collects both through `collect_accumulate`, so what
/// reaches the reporter is a *single* `Error::Many` holding two `VariableNotFound`s.
/// `Many` has no position of its own, so if it did not flatten it would render as a
/// summary with no caret at all and both carets would vanish silently — the failure
/// mode is invisible, which is why this is asserted rather than assumed.
///
/// The fixture uses operators because an unresolved constructor is a hole rather than a
/// failure of its branch, and so is never grouped.
///
/// Mutation-checked by replacing the `Error::Many` arm of `canonical::Error::labels`
/// with `Vec::new()`: the diagnostic keeps its message and its notes and loses both
/// labels.
#[test]
fn grouped_canonical_error_keeps_every_label() {
    let root = fixture_package("package_two_unknown_operators");
    assert_eq!(
        module_names(&root, SourceRoot::Src),
        vec!["src/Grouped.zel"]
    );

    let source =
        std::fs::read_to_string(root.join("src").join("Grouped.zel")).expect("fixture is readable");
    let ranges: Vec<_> = ["<+>", "<*>"]
        .iter()
        .map(|operator| {
            let start = source.find(operator).unwrap_or_else(|| {
                panic!("fixture uses an undeclared `{}`", operator);
            });
            start..(start + operator.len())
        })
        .collect();

    let error = compile_package(&root).expect_err("two undeclared operators must not compile");

    let errors = many(&error);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    // One phase error — the group — carrying two members.
    match unwrap_in_file(errors[0]) {
        CompilationError::Canonical(canonical_errors, _) => {
            assert!(
                matches!(canonical_errors.as_slice(), [canonical::Error::Many(members)] if members.len() == 2),
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
        "the group must carry a caret for each operator it swallowed"
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
    assert_eq!(
        module_names(&root, SourceRoot::Src),
        vec!["src/CaseBody.zel"]
    );

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

    let errors = many(&error);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(errors[0]) {
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
        module_names(&root, SourceRoot::Src),
        vec!["src/A.zel", "src/B.zel", "src/Main.zel"]
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

    let errors = many(&error);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(errors[0]) {
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
    use zelkova_syntax::position::NodeSpan;

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
        module_name: zelkova_compiler::ModuleName::new(
            PackageName::new("zelkova-core").unwrap(),
            "Helper".into(),
        ),
        values,
        unions: HashMap::new(),
        opaque_unions: Default::default(),
        infixes: HashMap::new(),
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
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

    let error = check_module(&test_package(), &interfaces, &parse_source(source))
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
        module_names(&root, SourceRoot::Src),
        vec!["src/Ops.zel", "src/User.zel"]
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

    let errors = many(&error);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(errors[0]) {
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
    let errors = many(&error);

    // The bare phase error, with no `InFile` wrapper: nothing tells it which file
    // `Main` was read from.
    let bare = unwrap_in_file(errors[0]);
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
        module_names(&root, SourceRoot::Src),
        vec!["src/CycleA.zel", "src/CycleB.zel"]
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

    let errors = many(&error);
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
    assert_eq!(
        module_names(&root, SourceRoot::Src),
        vec!["src/Lib.zel", "src/Main.zel"]
    );

    let source =
        std::fs::read_to_string(root.join("src").join("Main.zel")).expect("fixture is readable");
    let identifier = "missing";
    let start = source
        .find(identifier)
        .expect("fixture imports an undeclared `missing`");

    let error = compile_package(&root).expect_err("importing an unexported name must not compile");

    let errors = many(&error);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(errors[0]) {
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
/// names too (`BUG-8`), and `crates/zelkova-compiler/tests/canonical.rs`'s
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
    assert_eq!(module_names(&root, SourceRoot::Src), vec!["src/Main.zel"]);

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

    let errors = many(&error);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);

    match unwrap_in_file(errors[0]) {
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
// used to vanish from the interface silently, and the importer's error was the
// only one a user saw. `SPEC-5` closes this at the source: `Widget` itself is
// rejected for the exposed, unannotated `label`. It still publishes the
// interface it has, so `Main` is checked against it and its own `Widget.label`
// is a missing name: two errors, the first of which says why.

/// `Widget` exposes `label` with no annotation, so it reports
/// `ExportedValueNotAnnotated` and publishes an `Interface` without `label`.
/// `Main` is checked against that interface, as `check_in_order` does, and
/// reports `VariableNotFound` for `Widget.label`.
///
/// Mutation-checked: reverting `do_exports`'s `Lower` arm to accept `label`
/// once `env.find_value` succeeds (the behaviour before the `values.get(name)`
/// annotation check was added) turns `Widget`'s check green, and this test
/// goes red on its first assertion.
#[test]
fn unannotated_export_is_rejected_at_the_declaration_and_the_importer_is_told_too() {
    let widget = indoc::indoc! {r#"
        module Widget exposing (label)
        label = 1
    "#};
    let main = indoc::indoc! {r#"
        module Main exposing (x)
        import Widget
        x : Int
        x = Widget.label
    "#};

    let pkg = test_package();
    let mut interfaces: HashMap<Name, Interface> = HashMap::from([basics_interface()]);

    // `Widget` is checked and its interface published whatever the check found,
    // which is what `check_in_order` does with an `Outcome::Module`.
    match check_module_recovering(&pkg, &interfaces, &parse_source(widget)) {
        Outcome::Module(widget_module, widget_errors) => {
            assert_eq!(widget_errors.len(), 1, "got {:?}", widget_errors);
            match &widget_errors[0] {
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

            interfaces.insert(
                widget_module.canonical.name.name().clone(),
                widget_module.to_interface(None),
            );
        }
        Outcome::Failed(error) => panic!("`Widget` should still publish: {:?}", error),
    }

    let main_error = check_module(&pkg, &interfaces, &parse_source(main))
        .expect_err("`Widget` publishes no `label` for Main to import");

    match &main_error {
        CompilationError::Canonical(errors, module) => {
            assert_eq!(module, &Name::from("Main"));
            assert!(
                errors
                    .iter()
                    .any(|e| matches!(e, canonical::Error::VariableNotFound(..))),
                "`Widget.label` is not in the interface `Widget` published: {:?}",
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
    let lib_module = check_module(&pkg, &interfaces, &parse_source(lib))
        .unwrap_or_else(|e| panic!("the exporting module should compile: {:?}", e));

    interfaces.insert(
        lib_module.canonical.name.name().clone(),
        lib_module.to_interface(None),
    );

    check_module(&pkg, &interfaces, &parse_source(main)).map(|_| ())
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
    use zelkova_compiler::PhaseError;

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
        module Main exposing ()
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
        module Main exposing ()
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
/// (`crates/zelkova-compiler/src/canonical/environment.rs`), which turns this red with
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
        module Main exposing ()
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
/// The loading failure now arrives inside `BuildError::Many`, because
/// loading a package's sources happens once per package *inside* the build's error
/// accumulator: a package that cannot be read pushes its failure onto that vector
/// and publishes nothing, rather than returning out of `compile_package` past the
/// packages whose diagnostics are already on it. See
/// [`a_package_that_cannot_be_read_does_not_hide_an_earlier_packages_errors`].
///
/// Mutation-checked by restoring the `filter_map(|r| r.ok())` discard in
/// `load_package_sources` (`crates/zelkova-compiler/src/source/mod.rs`): with the walk error
/// thrown away, `compile_package` returns `Ok(())` on this same fixture, and
/// `expect_err` below panics.
#[test]
fn compile_package_reports_a_missing_source_root() {
    let root = fixture_package("package_missing_src");
    // Resolution names a package by its canonical path, and `fixture_package` builds one
    // through `../..`.
    let src_root = root.canonicalize().unwrap().join("src");
    assert!(
        !src_root.exists(),
        "fixture must have no `src/` for this test to mean anything"
    );

    let error =
        compile_package(&root).expect_err("a package with no `src/` must not compile as success");

    let accumulated = many(&error);
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

    let BuildError::Check(CompilationError::Manifest(errors)) = &error else {
        panic!(
            "expected Err(BuildError::Check(CompilationError::Manifest(..))), got {:?}",
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
/// `LoadingFiles`, and it is the `let BuildError::Check(CompilationError::Manifest(..)) = … else`
/// below that catches it rather than `expect_err`.
#[test]
fn compile_package_reports_an_invalid_package_name() {
    let root = fixture_package("package_invalid_name");

    let error = compile_package(&root)
        .expect_err("a manifest with an illegal package name must not compile as success");

    let BuildError::Check(CompilationError::Manifest(errors)) = &error else {
        panic!(
            "expected Err(BuildError::Check(CompilationError::Manifest(..))), got {:?}",
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
/// error accumulation (`BuildError::Many`) rather than the unrendered path
/// the other manifest failures take. `package_private_module_not_found` declares
/// `private-modules = ["Ghost"]` and holds one real module, `Answer`.
///
/// Mutation-checked by making the `held_modules.contains(name)` filter in
/// `compile_package` always `true` (as if every name were held): the
/// `PrivateModuleNotFound` push never happens and `expect_err` panics.
#[test]
fn compile_package_reports_a_private_module_that_does_not_exist() {
    let root = fixture_package("package_private_module_not_found");
    assert_eq!(module_names(&root, SourceRoot::Src), vec!["src/Answer.zel"]);

    let error = compile_package(&root)
        .expect_err("a `private-modules` entry naming no real module must not compile as success");

    let errors = many(&error);
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

/// A module with a declaration that fails to parse is still among the modules the
/// package holds when its header parsed, so a `private-modules` entry naming it names a
/// module that exists — and the one broken file is reported once, truthfully, and not
/// a second time as the manifest's fault.
///
/// `package_private_module_parse_failure` declares `private-modules = ["Broken"]`
/// and holds exactly one module, `Broken`, whose `import` does not parse. The only
/// error is the parse error.
///
/// Mutation-checked by making `parse_root` drop a module with a failed declaration
/// without listing it in `headless`: a second `CompilationError::Manifest` joins the
/// parse error and the length assertion below fails.
#[test]
fn a_parse_failure_does_not_also_report_its_module_as_unheld() {
    let root = fixture_package("package_private_module_parse_failure");
    assert_eq!(module_names(&root, SourceRoot::Src), vec!["src/Broken.zel"]);

    let error = compile_package(&root).expect_err("`Broken.zel` does not parse");

    let errors = many(&error);
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

/// A module whose header does not parse contributes no module, so the list of modules
/// the package holds is short and the `private-modules` check is not run against it:
/// the header's syntax error is the only error.
///
/// Mutation-checked by removing the `headless.is_empty()` guard around the
/// `private-modules` check in `compile_in_build`: a `CompilationError::Manifest` joins
/// the syntax error and the assertion goes red.
#[test]
fn a_header_that_does_not_parse_does_not_also_report_its_module_as_unheld() {
    let root = fixture_package("package_private_module_parse_failure");
    let mut overlay = Overlay::new();
    overlay.insert(
        root.join("src").join("Broken.zel"),
        "module Broken exposing (\n".into(),
    );

    let check = check_package(&root, &overlay).expect("the manifest and the build resolve");

    assert!(
        matches!(check.errors.as_slice(), [CompilationError::Source(..)]),
        "expected the syntax error alone, got {:?}",
        check.errors
    );
}

/// A module with a declaration that does not parse, beside a header that does, is held,
/// so a `private-modules` entry naming a module the package really lacks is still
/// reported: the manifest's error stands beside the syntax error rather than waiting
/// behind it.
///
/// `package_private_module_unparsed_declaration` declares `private-modules = ["Ghost"]`
/// and holds `A`, whose `import` does not parse.
///
/// Mutation-checked by reverting the `private-modules` guard in `compile_in_build` to
/// `parsed.failures == 0`: the `PrivateModuleNotFound` is not pushed and the assertion
/// that finds it goes red.
#[test]
fn a_declaration_that_does_not_parse_does_not_hide_a_missing_private_module() {
    let root = fixture_package("package_private_module_unparsed_declaration");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert!(
        check
            .errors
            .iter()
            .any(|error| matches!(error, CompilationError::Source(..))),
        "expected `A`'s syntax error, got {:?}",
        check.errors
    );
    let missing: Vec<&Name> = check
        .errors
        .iter()
        .filter_map(|error| match error {
            CompilationError::Manifest(errors) => Some(errors),
            _ => None,
        })
        .flatten()
        .filter_map(|error| match error {
            manifest::ManifestError::PrivateModuleNotFound { name, .. } => Some(name),
            _ => None,
        })
        .collect();
    assert_eq!(
        missing,
        vec![&Name::from("Ghost")],
        "got {:?}",
        check.errors
    );
}

// ── Test 29: the default imports reach a module that wrote none ─────────────

/// Every checked module of the fixture package, in the order they were checked.
///
/// `compile_package` reports its modules to stderr and hands back only `Ok(())`,
/// so a test that has to look *inside* a checked module drives the walker with the
/// real `check_module_recovering` instead — the same seam
/// `check_in_order_keeps_passing_siblings_with_the_real_checker` uses.
///
/// `package` is the package the fixture's modules are checked as, whatever its manifest
/// says: a fixture standing in for `std/core` is checked as `zelkova-core`, since that is
/// the one package whose `Basics` declares the scalars.
fn check_fixture(name: &str, package: &PackageName) -> Vec<CheckedModule> {
    let root = fixture_package(name);
    let sources = load_package_sources(&root, SourceRoot::Src)
        .unwrap_or_else(|e| panic!("failed to load sources from {:?}: {:?}", root, e));
    let modules: Vec<parser::Module> = sources
        .iter()
        .map(|(_, file)| {
            parser::parse(file.file())
                .unwrap_or_else(|e| panic!("parse error in {:?}: {:?}", file.file().name(), e))
        })
        .collect();

    let module_files = HashMap::new();
    let walker = ModuleWalker::new(&modules, &module_files, package)
        .expect("no dependency cycle in the fixture");
    let mut interfaces: HashMap<Name, Interface> = HashMap::new();
    let (checked, _errors) = checked_and_errors(walker.check_in_order(
        package,
        &mut interfaces,
        &module_files,
        check_module_recovering,
    ));

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
    modules: &'a [CheckedModule],
    module: &str,
    value: &str,
) -> &'a canonical::Value {
    modules
        .iter()
        .find(|m| m.canonical.name.name() == &Name::from(module))
        .unwrap_or_else(|| panic!("`{}` should have checked, and did not", module))
        .canonical
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
            canonical::ExpressionKind::VarForeign(qual, _, _) => {
                out.push(qual.to_name().to_string())
            }
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
/// `crates/zelkova-compiler/tests/support/mod.rs` stands in for a real `Maybe.zel`.
///
/// Named apart from [`support::basics_interface`], which carries no values —
/// this one adds `+`/`add` on purpose, and a plain `use support::*` item-level
/// shadowing would otherwise hand every other call in this file the wrong one.
fn basics_interface_with_plus() -> (Name, Interface) {
    use zelkova_syntax::position::NodeSpan;

    let int_type = canonical::Type::Type(core_qual("Basics.Int"), vec![]);
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
                tpe: core_qual("Basics.Int"),
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
        module_name: zelkova_compiler::ModuleName::new(
            PackageName::new("zelkova-core").unwrap(),
            "Basics".into(),
        ),
        values,
        unions,
        opaque_unions: Default::default(),
        infixes,
        infix_functions: HashMap::new(),
        arities: HashMap::new(),
        classes: HashMap::new(),
        instances: Vec::new(),
        file: None,
        incomplete: false,
    };

    ("Basics".into(), interface)
}

/// `LANG-8`: a module that writes no `import` at all still resolves `+`, and
/// resolves it to `Basics`, in an ordinary package (one other than `zelkova-core`).
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

    let checked = check_module(&test_package(), &interfaces, &parse_source(source))
        .unwrap_or_else(|e| panic!("expected the implicit default to resolve `+`: {:?}", e));
    let x = checked
        .canonical
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
    // Its `Basics` stands in for `std/core`'s, `Int` included, so it is checked as
    // `zelkova-core`: the literals in `1 + 2` are the scalar `Int`, and a `Basics` of
    // any other package declares an ordinary one.
    let checked = check_fixture("package_default_imports", &PackageName::core());
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
        x : Int
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
        let module = check_module(&pkg, &interfaces, &parse_source(lib))
            .unwrap_or_else(|e| panic!("an exporting module should compile: {:?}", e));
        interfaces.insert(
            module.canonical.name.name().clone(),
            module.to_interface(None),
        );
    }

    check_module(&pkg, &interfaces, &parse_source(main)).map(|_| ())
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
            zelkova_compiler::typer::ErrorKind::UnificationFailed { .. }
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

/// A declaration whose annotation names both modules' `Size`, and whose body names
/// neither module's constructor: the two types are compared through the annotation
/// alone. [`an_imported_constructor_does_not_build_another_modules_type`] is the same
/// comparison reached through a constructor.
const CROSSES_TWO_SIZES: &str = indoc::indoc! {r#"
    module Main exposing (..)

    import A
    import B

    f : A.Size -> B.Size
    f s = s
"#};

// ── Test 34: what another module declares is type checked ────────────────────
//
// `BUG-36`. Each probe below reaches for something another module declares — a
// constructor in a pattern, a constructor in an expression, or a plain value — and
// holds a type error the typer has to find.

/// A module exposing a union with a parameter, a nullary union, a value and a
/// polymorphic function, for the importers below to reach for.
const IMPORTED: &str = indoc::indoc! {r#"
    module Lib exposing (Box(..), Flag(..), size, ident)

    type Box a = Box a

    type Flag = On | Off

    size : Int
    size = 1

    ident : a -> a
    ident x = x
"#};

/// [`IMPORTED`] checked into an `Interface` the way [`check_importer`] builds one, and
/// `main` checked against it with `Char` and `Maybe` in the map too, so that the
/// default imports bring the `Char` type and `Just` and `Nothing` in.
fn check_against_imported(main: &str) -> Result<CheckedModule, CompilationError> {
    let pkg = test_package();
    let mut interfaces: HashMap<Name, Interface> =
        HashMap::from([basics_interface(), char_interface(), maybe_interface()]);
    let lib = check_module(&pkg, &interfaces, &parse_source(IMPORTED))
        .unwrap_or_else(|e| panic!("the exporting module should compile: {:?}", e));

    interfaces.insert(lib.canonical.name.name().clone(), lib.to_interface(None));

    check_module(&pkg, &interfaces, &parse_source(main))
}

/// The type errors `main` is rejected with, which must be exactly one.
fn only_type_error(main: &str) -> (CompilationError, String) {
    let error = match check_against_imported(main) {
        Ok(_) => panic!("expected a type error, but the module checked clean"),
        Err(error) => error,
    };

    let CompilationError::Type(errors, module) = &error else {
        panic!("expected a Type error, got {:?}", error);
    };
    assert_eq!(module, &Name::from("Main"));
    assert_eq!(errors.len(), 1, "expected one type error, got {:?}", errors);
    assert!(
        matches!(
            errors[0].kind,
            zelkova_compiler::typer::ErrorKind::UnificationFailed { .. }
        ),
        "expected a unification failure, got {:?}",
        errors[0].kind
    );

    let message = errors[0].message();
    (error, message)
}

/// `main`'s one type error, which has to be a unification failure with its caret
/// under the only `'c'` in `main`.
///
/// The variant and the caret are both asserted, not merely the rejection: what these
/// tests pin is that the declaration reached the unifier and was blamed where the
/// source disagrees.
fn assert_rejected_at_the_char(main: &str) {
    let (error, _) = only_type_error(main);

    // The labels of the typer's own error rather than of a rendered diagnostic: a
    // `CompilationError` that no file was attached to renders without any.
    let CompilationError::Type(errors, _) = &error else {
        unreachable!("`only_type_error` answers with a Type error");
    };
    let labels = errors[0].labels();
    let primary = labels
        .iter()
        .find(|label| label.primary)
        .unwrap_or_else(|| panic!("expected a primary label, got {:?}", labels));

    let at = main.find("'c'").expect("the probe writes a `'c'`");
    assert_eq!(
        primary.span.to_range(),
        at..at + 3,
        "the caret must be under the `'c'`, got {:?}",
        labels
    );
}

/// A `case` over a `Maybe` — whose constructors every module receives through the
/// default imports — has its branches checked.
///
/// Mutation-checked by building `Translation::unions` from `module.types` alone:
/// `Just` then finds no union and the declaration is skipped, so this checks clean.
#[test]
fn a_case_over_a_default_imported_constructor_is_type_checked() {
    assert_rejected_at_the_char(indoc::indoc! {r#"
        module Main exposing (..)

        f : Maybe Int -> Int
        f m =
          case m of
            Just x ->
              'c'

            Nothing ->
              1
    "#});
}

/// Building an imported constructor checks what it is built from.
///
/// Mutation-checked three ways, each making the module check clean on its own:
/// building `Translation::constructors` from this module's unions alone (the
/// `VarConstructor` arm finds nothing and the declaration is skipped), registering
/// only this module's constructors in `type_check`'s second pass (`Lib.Box` is
/// unbound and the declaration comes back `UnboundName`), and naming a
/// `VarConstructor` by its written spelling in `Expression::from_parser` (the bare
/// `Box` becomes `Main.Box`, which nothing declares).
#[test]
fn building_an_imported_constructor_is_type_checked() {
    assert_rejected_at_the_char(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (Box(..))

        f : Box Int
        f = Box 'c'
    "#});
}

/// A constructor pattern written qualified finds the union its module declared.
///
/// Mutation-checked by building `Translation::unions` from `module.types` alone.
#[test]
fn a_case_over_a_qualified_imported_constructor_is_type_checked() {
    assert_rejected_at_the_char(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib

        f : Lib.Box Int -> Int
        f b =
          case b of
            Lib.Box x ->
              'c'
    "#});
}

/// The same pattern, written with the constructor exposed.
///
/// Mutation-checked by building `Translation::unions` from `module.types` alone.
#[test]
fn a_case_over_an_exposed_imported_constructor_is_type_checked() {
    assert_rejected_at_the_char(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (Box(..))

        f : Box Int -> Int
        f b =
          case b of
            Box x ->
              'c'
    "#});
}

/// A nullary imported constructor in a pattern.
///
/// Mutation-checked by building `Translation::unions` from `module.types` alone.
#[test]
fn a_case_over_a_nullary_imported_constructor_is_type_checked() {
    assert_rejected_at_the_char(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (Flag(..))

        f : Flag -> Int
        f flag =
          case flag of
            On ->
              'c'

            Off ->
              1
    "#});
}

/// An error that has nothing to do with the import is found in a declaration that
/// holds one: a skipped declaration hides every error in it.
///
/// Mutation-checked by building `Translation::unions` from `module.types` alone,
/// which makes the module check clean.
#[test]
fn an_unrelated_error_beside_an_imported_constructor_is_reported() {
    let (_, message) = only_type_error(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (Flag(..))

        f : Flag -> Int
        f flag =
          if 'c' then
            case flag of
              On ->
                1

              Off ->
                2
          else
            3
    "#});

    assert!(
        message == "cannot match `Bool` with `Char`"
            || message == "cannot match `Char` with `Bool`",
        "got {:?}",
        message
    );
}

/// A constructor of one module's `Size` is not a value of another module's `Size`.
///
/// Inherited from `BUG-35`, which made the two `Size`es two types but could not show
/// `B.S` meeting `A.Size`: the declaration never reached the unifier.
///
/// Mutation-checked by building `Translation::constructors` from this module's unions
/// alone, which makes `B.S` untranslatable and the module check clean.
#[test]
fn an_imported_constructor_does_not_build_another_modules_type() {
    let main = indoc::indoc! {r#"
        module Main exposing (..)

        import A
        import B

        x : A.Size
        x = B.S
    "#};

    let error = check_importer_of_two(SIZE_A, SIZE_B, main).expect_err("`B.S` is not an `A.Size`");

    let CompilationError::Type(errors, _) = &error else {
        panic!("expected a Type error, got {:?}", error);
    };
    assert_eq!(errors.len(), 1, "expected one type error, got {:?}", errors);

    let message = errors[0].message();
    assert!(
        message == "cannot match `A.Size` with `B.Size`"
            || message == "cannot match `B.Size` with `A.Size`",
        "both types must be named by their module, got {:?}",
        message
    );
}

/// A declaration that forwards an imported value is checked against that value's
/// declared type.
///
/// Mutation-checked by dropping the loop over `interfaces` from `type_check`'s first
/// pass: `Lib.size` is then unbound, the declaration comes back `UnboundName`, and
/// the module checks clean.
#[test]
fn forwarding_an_imported_value_is_type_checked() {
    let (_, message) = only_type_error(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (size)

        f : Char
        f = size
    "#});

    assert!(
        message == "cannot match `Int` with `Char`" || message == "cannot match `Char` with `Int`",
        "got {:?}",
        message
    );
}

/// An imported value written qualified, by its module's name or by an alias, is the
/// same value as the exposed one above and is checked the same way.
///
/// Mutation-checked by qualifying a `VarForeign` with the whole written spelling in
/// `Expression::from_parser` (`m.qualify_name(name)`): the reference then names
/// `Lib.Lib.size` or `Lib.L.size`, which nothing registers, and both check clean.
#[test]
fn forwarding_a_qualified_imported_value_is_type_checked() {
    for import in ["import Lib", "import Lib as L"] {
        let written = if import.ends_with(" L") {
            "L.size"
        } else {
            "Lib.size"
        };
        let main = format!(
            "module Main exposing (..)\n\n{}\n\nf : Char\nf = {}\n",
            import, written
        );

        let (_, message) = only_type_error(&main);

        assert!(
            message == "cannot match `Int` with `Char`"
                || message == "cannot match `Char` with `Int`",
            "`{}`: got {:?}",
            written,
            message
        );
    }
}

/// An imported constructor written under an alias is named by the module that
/// declared its union, and so is found.
///
/// Mutation-checked by naming a `VarConstructor` by its written spelling in
/// `Expression::from_parser` again (`name.to_qual()`, else this module's name): `L.Box`
/// then finds no constructor, and the declaration is skipped.
#[test]
fn building_an_aliased_imported_constructor_is_type_checked() {
    assert_rejected_at_the_char(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib as L

        f : L.Box Int
        f = L.Box 'c'
    "#});
}

/// The other half of the tests above: what they reject for a wrong type, they accept
/// for the right one, and each declaration becomes one a backend can read rather than
/// an entry in `unchecked`.
///
/// Without this, every rejection above would also pass on a typer that refused
/// anything imported outright.
#[test]
fn well_typed_uses_of_imported_names_are_checked_declarations() {
    let checked = check_against_imported(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (Box(..), Flag(..), size)

        boxed : Box Int
        boxed = Box size

        unboxed : Box Int -> Int
        unboxed b =
          case b of
            Box x ->
              x

        flagged : Flag -> Maybe Int
        flagged flag =
          case flag of
            On ->
              Just size

            Off ->
              Nothing
    "#})
    .unwrap_or_else(|e| panic!("expected the module to check, got {:?}", e));

    assert!(
        checked.ir.unchecked.is_empty(),
        "every declaration should have been checked, got {:?}",
        checked.ir.unchecked
    );
    assert_eq!(checked.ir.declarations.len(), 3);
}

/// `main` checks clean, with every one of its declarations checked rather than
/// skipped.
fn assert_checks_clean(main: &str) {
    let checked = check_against_imported(main)
        .unwrap_or_else(|e| panic!("expected the module to check, got {:?}", e));

    assert!(
        checked.ir.unchecked.is_empty(),
        "every declaration should have been checked, got {:?}",
        checked.ir.unchecked
    );
}

/// Each use of a polymorphic name another module declares is typed on its own:
/// one declaration can use it at two types.
///
/// `Just` and `Nothing` are the sharpest case, because they are two names for one
/// union: without instantiation both carry the same `a`, so `Nothing` would be
/// forced to `Maybe Int` beside `Just 1` even though neither name is used twice.
///
/// Mutation-checked by making `Types::by_name` hand a global's type back as it is
/// rather than instantiating it: each of the four probes is then rejected with a
/// unification failure.
#[test]
fn an_imported_polymorphic_name_is_instantiated_at_each_use() {
    // Two constructors of one union.
    assert_checks_clean(indoc::indoc! {r#"
        module Main exposing (..)

        f : (Maybe Int, Maybe Char)
        f = (Just 1, Nothing)
    "#});

    // One imported function, used at two types.
    assert_checks_clean(indoc::indoc! {r#"
        module Main exposing (..)

        f : Maybe Int -> Maybe Char -> (Int, Char)
        f a b = (Maybe.withDefault 0 a, Maybe.withDefault 'c' b)
    "#});

    // An imported value, used at two types.
    assert_checks_clean(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (ident)

        f : (Int, Char)
        f = (ident 1, ident 'c')
    "#});

    // One imported constructor, used at two types.
    assert_checks_clean(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (Box(..))

        f : (Box Int, Box Char)
        f = (Box 1, Box 'c')
    "#});
}

/// A union this module declares is instantiated the same way: its constructor can be
/// used at two types in one declaration.
///
/// Mutation-checked with [`an_imported_polymorphic_name_is_instantiated_at_each_use`].
#[test]
fn a_local_polymorphic_constructor_is_instantiated_at_each_use() {
    assert_checks_clean(indoc::indoc! {r#"
        module Main exposing (..)

        type B a = B a

        f : (B Int, B Char)
        f = (B 1, B 'c')
    "#});
}

/// Instantiation is fresh per use, not a licence to ignore types: inside one use,
/// the variable is still one variable, so `ident` applied to a `Char` is a `Char`.
///
/// Mutation-checked by giving each *occurrence* of a variable in a global's type its
/// own fresh variable in `Types::instantiate` (dropping the `fresh` lookup): `ident`
/// is then `a -> b` and this checks clean.
#[test]
fn one_use_of_an_imported_polymorphic_name_keeps_its_variables_linked() {
    assert_rejected_at_the_char(indoc::indoc! {r#"
        module Main exposing (..)

        import Lib exposing (ident)

        f : Int
        f = ident 'c'
    "#});
}

// ── Package boundaries: namespaces, unwrapping, and one name per module ──────
//
// `LANG-14`. Each of these drives `compile_package` over a fixture that depends on
// another fixture package through a `path` entry, which is the only source the
// compiler obtains today.

/// Every error of the check a failed `compile_package` accumulated, past the `Many` that
/// groups them and the `Check` each is wrapped in.
///
/// A resolution failure raised before the file database exists comes back on its own,
/// so both shapes have to be handled for a test to assert on what was reported.
fn accumulated(error: &BuildError) -> Vec<&CompilationError> {
    match error {
        BuildError::Many(_) => many(error),
        BuildError::Check(other) => vec![other],
        other => panic!("expected an error of the check, got {:?}", other),
    }
}

/// The resolution errors in a failed `compile_package`.
fn resolution_errors(error: &BuildError) -> Vec<&resolve::Error> {
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
        module_names(&root, SourceRoot::Src),
        vec!["src/App.zel".to_string()],
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
        module_names(&root, SourceRoot::Src),
        vec!["src/App.zel".to_string()],
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

/// An unwrapped dependency declaring its own `Bitwise` collides with `zelkova-core`'s,
/// which is always unwrapped: two modules answer to the spelling `Bitwise`, so the
/// package has no one declaration to resolve it to.
///
/// `Bitwise` rather than `Basics`, because a rival `Basics` — or any of the other
/// seven default imports' names — is rejected as `ReservedModuleName` before a build
/// ever reaches this collision check ([`a_non_core_packages_basics_is_reserved_even_wrapped`]).
/// `Bitwise` is what is left of `LANG-62`'s wider rule — every one of `zelkova-core`'s
/// module names is taken in every package — once the eight are carved out of it: this
/// pins that a build still reaches the ordinary `ModuleNameCollision` for a name
/// outside the eight.
///
/// Mutation-checked the same way as the test above, and additionally by dropping the
/// `CORE_PACKAGE` arm of `seen_unwrapped`: `zelkova-core` is then wrapped, its
/// `Bitwise` becomes `ZelkovaCore.Bitwise`, and no collision is reported at all.
#[test]
fn a_dependencys_bitwise_collides_with_cores() {
    let root = fixture_package("package_core_basics_collision");

    let error = compile_package(&root).expect_err("`Bitwise` is claimed twice");

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

    assert_eq!(name.as_str(), "Bitwise");

    let mut packages = [first.package.as_str(), second.package.as_str()];
    packages.sort_unstable();
    assert_eq!(packages, ["acme-bitwise", "zelkova-core"]);

    assert!(
        accumulated(&error)
            .into_iter()
            .all(|error| matches!(unwrap_in_file(error), CompilationError::Resolution(_))),
        "nothing in this package may be compiled, got {:?}",
        error
    );
}

// ── A package is part of a type's identity ──────────────────────────────────
//
// A module's name is unique within its package and not across a build, so a package
// may hold its own `Size` beside a wrapped dependency's `AcmeWidgets.Size`. The two
// `Size.Size` unions are two types, and a dependency's own `Basics` declares no scalar
// ([*What a package boundary cannot
// rename*](../docs/spec/packages.md#what-a-package-boundary-cannot-rename)).

/// Every type error in a failed build, each with the module it was found in.
fn type_errors(error: &BuildError) -> Vec<(&Name, &typer::Error)> {
    accumulated(error)
        .into_iter()
        .filter_map(|error| match unwrap_in_file(error) {
            CompilationError::Type(errors, module) => {
                Some(errors.iter().map(move |error| (module, error)))
            }
            _ => None,
        })
        .flatten()
        .collect()
}

/// A local `Size.Size` and a wrapped dependency's `AcmeWidgets.Size.Size` share a module
/// name and a type name, and are still two types: passing one off as the other is a type
/// error, and the message names each union the way this package spells it.
///
/// The message is asserted whole because `Size.Size` is a substring of
/// `AcmeWidgets.Size.Size`: written by its declaring module's name alone, the
/// dependency's union would read `Size.Size` too, and the sentence would say nothing.
///
/// Mutation-checked by leaving the package out of `QualName`'s equality (deriving
/// `PartialEq` and `Hash` by hand over `module` and `name` only): the two unions unify,
/// the build succeeds and `expect_err` panics. Separately, by writing a qualified union
/// by `name.to_name()` in `Type::write` instead of through `Spellings::spell`: the
/// message reads `Size.Size` twice and the assertion goes red.
#[test]
fn a_local_module_and_a_dependencys_of_one_name_declare_two_types() {
    let root = fixture_package("package_local_size_mismatch");

    let error = compile_package(&root).expect_err("`Size.Size` is not `AcmeWidgets.Size.Size`");

    let errors = type_errors(&error);
    let [(module, error)] = errors.as_slice() else {
        panic!("expected one type error, got {:?}", error);
    };

    assert_eq!(module.as_str(), "App");
    assert_eq!(error.declaration.as_str(), "f");
    assert!(
        matches!(error.kind, typer::ErrorKind::UnificationFailed { .. }),
        "expected a unification failure, got {:?}",
        error.kind
    );
    // Unification is symmetric, so which side each type lands on is not part of the
    // contract — see the sibling tests at `two_modules_same_named_types_do_not_unify`
    // and `an_imported_constructor_does_not_build_another_modules_type`. What matters
    // is that both spellings appear and neither is the bare, unwritable `Size.Size`
    // for the wrapped dependency.
    let message = error.message();
    assert!(
        message == "cannot match `AcmeWidgets.Size.Size` with `Size.Size`"
            || message == "cannot match `Size.Size` with `AcmeWidgets.Size.Size`",
        "both types must be named by the checked package's own spelling, got {:?}",
        message
    );
}

/// The same clash, but with no shared *module* name: a local module named `Widget`
/// (not `Size`) declares its own `type Size`, and the only thing it shares with the
/// wrapped dependency's `AcmeWidgets.Size.Size` is the bare type name `Size`.
///
/// This pins the broader reading of the ticket's rule, which the PR that implemented
/// it chose deliberately: [`AdtNames::collide`] switches to the qualified spelling
/// whenever the message would use one *word* for two declarations, not only when the
/// two declaring modules also share a name. Under the narrower reading — qualify only
/// when the two unions share a qualified name — this pair would not collide (`Widget.Size`
/// as text differs from `Size.Size`), and the message would print both sides through
/// the bare, unqualified `Display`: `` cannot match `Size` with `Size` ``, which is
/// exactly as ambiguous as the case this ticket exists to fix.
///
/// Mutation-checked by comparing `other.to_name() == name.to_name()` in
/// `AdtNames::collide` instead of the two `unqualified_name()`s: the pair here no
/// longer collides (`Widget.Size` and `Size.Size` differ as text), both sides fall
/// back to the unqualified `Display`, and the message reads `` cannot match `Size`
/// with `Size` ``, which fails both assertions below.
#[test]
fn a_bare_name_clash_with_no_shared_module_name_is_qualified_too() {
    let root = fixture_package("package_local_widget_size_mismatch");

    let error = compile_package(&root).expect_err("`Widget.Size` is not `AcmeWidgets.Size.Size`");

    let errors = type_errors(&error);
    let [(module, error)] = errors.as_slice() else {
        panic!("expected one type error, got {:?}", error);
    };

    assert_eq!(module.as_str(), "App");
    assert_eq!(error.declaration.as_str(), "f");
    assert!(
        matches!(error.kind, typer::ErrorKind::UnificationFailed { .. }),
        "expected a unification failure, got {:?}",
        error.kind
    );

    let message = error.message();
    assert!(
        message.contains("`AcmeWidgets.Size.Size`"),
        "expected the wrapped dependency's own spelling in the message, got {:?}",
        message
    );
    assert!(
        !message.contains("`Size.Size`"),
        "the wrapped dependency's union must not fall back to its bare, unwritable \
         module-local spelling, got {:?}",
        message
    );
}

/// In the same pair, the local module `Size` builds the dependency's union with the
/// dependency's constructor, and that checks: `AcmeWidgets.Size.Small` is a constructor
/// of `AcmeWidgets.Size.Size`, whatever this module's own union is called. So does
/// reading the dependency's `small`, although this module declares a `small` of its own.
///
/// A declaration the typer could not type is not an error until the build is emitted,
/// so the assertion is on the whole build rather than on the module's type check: an
/// untyped `theirs` fails emission as `Unchecked`. What the emitted `Size.mjs` makes of
/// `theirs` is `a_dependencys_constructor_is_hoisted_under_its_own_package`'s.
///
/// Mutation-checked by leaving the package out of `QualName`'s equality (deriving
/// `PartialEq` and `Hash` by hand over `module` and `name` only): this module's own
/// `Size.Size` then replaces the dependency's in `Translation::of`, `Small` is a
/// constructor of no union the typer can see, and the build fails with `Emit([Unchecked
/// { name: "theirs" }])`. Separately, by leaving the package out of `environment_key`:
/// this module's `small` then replaces the dependency's in the typer's environment,
/// `theirsSmall` is a type error and the build fails.
#[test]
fn a_dependencys_names_resolve_beside_a_local_module_of_the_same_name() {
    let build_dir =
        fresh_build_dir("a_dependencys_names_resolve_beside_a_local_module_of_the_same_name");

    let result = zelkova::compile_package_into(
        &fixture_package("package_local_size_beside_dependency"),
        &build_dir,
    );

    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// In the same pair, `App` imports both `Size`s — its own package's, and `acme-widgets`'
/// as `AcmeWidgets.Size` — and reads each one's `small`. The two are imported under two
/// local names, one per package, each from its own package's directory, and each
/// binding reads its own.
///
/// Asserted on the emitted text, since the collision this pins compiled `Ok` and wrote
/// two `import` lines binding one name, which only fails when the module loads.
///
/// Mutation-checked by leaving the package out of `zelkova_js::imported`: both imports
/// then bind `Size$small`, and the test goes red.
#[test]
fn two_packages_same_named_modules_import_under_distinct_names() {
    let build_dir = fresh_build_dir("two_packages_same_named_modules_import_under_distinct_names");

    let result = zelkova::compile_package_into(
        &fixture_package("package_local_size_beside_dependency"),
        &build_dir,
    );
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    let app = std::fs::read_to_string(build_dir.join("out/js/app/App.mjs")).unwrap();

    assert!(
        app.contains(
            "import { small as acme_widgets$Size$small } from \"../acme-widgets/Size.mjs\";\n"
        ),
        "got:\n{}",
        app
    );
    assert!(
        app.contains("import { small as app$Size$small } from \"./Size.mjs\";\n"),
        "got:\n{}",
        app
    );
    assert!(
        app.contains("const mine = app$Size$small;"),
        "got:\n{}",
        app
    );
    assert!(
        app.contains("const theirs = acme_widgets$Size$small;"),
        "got:\n{}",
        app
    );

    let mut imported: Vec<&str> = app
        .lines()
        .filter(|line| line.starts_with("import "))
        .filter_map(|line| line.split(" as ").nth(1))
        .filter_map(|rest| rest.split(' ').next())
        .collect();
    let count = imported.len();
    imported.sort_unstable();
    imported.dedup();
    assert_eq!(
        imported.len(),
        count,
        "a local name imported twice:\n{}",
        app
    );
}

/// In the same pair, the local module `Size` mentions `AcmeWidgets.Size.Small`, a
/// constructor of no arguments of another package's same-named module. It is not this
/// module's own, so `Size.mjs` hoists a constant for it, named by its package, beside
/// the one for its own `Mine`, and `theirs` refers to it.
///
/// Mutation-checked two ways: by comparing only the module's name, not its package,
/// when `Emitter::value` decides whether a constructor is this module's own — `Small`
/// is then taken for `Size`'s own, nothing hoists it and the `const` assertion goes
/// red; and by leaving the package out of `zelkova_js::hoisted`, which names it
/// `$Size$Small`.
#[test]
fn a_dependencys_constructor_is_hoisted_under_its_own_package() {
    let build_dir = fresh_build_dir("a_dependencys_constructor_is_hoisted_under_its_own_package");

    let result = zelkova::compile_package_into(
        &fixture_package("package_local_size_beside_dependency"),
        &build_dir,
    );
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    let size = std::fs::read_to_string(build_dir.join("out/js/app/Size.mjs")).unwrap();

    assert!(
        size.contains("const $app$Size$Mine = {$: \"Mine\"};"),
        "got:\n{}",
        size
    );
    assert!(
        size.contains("const $acme_widgets$Size$Small = {$: \"Small\"};"),
        "got:\n{}",
        size
    );
    assert!(
        size.contains("const theirs = $acme_widgets$Size$Small;"),
        "got:\n{}",
        size
    );
}

/// `acme-basics` declares its own module `Basics`. The eight default imports' names
/// are reserved for `zelkova-core` alone, so this is rejected at resolution before a
/// single module of `acme-basics` is compiled — whether the dependent wraps it or not
/// makes no difference, since the reservation is asked of the declaring package, not
/// of how anyone names it. `package-wrapped-rival-basics`, which depends on it, then
/// fails too, with `DependencyNotCompiled`, and no type error is ever reached.
///
/// Mutation-checked by dropping the `is_core` check ahead of `claim` in
/// `resolve::visible_modules`'s local-module loop: `acme-basics` then compiles
/// `Basics` as an ordinary union (its `type Int = Int` is not the scalar — a scalar
/// is declared in `zelkova-core` and nowhere else) and the build succeeds instead of
/// failing with the two resolution errors this pins. `Scalar::declares`'s package
/// check is still pinned on its own, by `scalars.rs`'s
/// `another_packages_basics_int_is_not_the_scalar` — which is now its only pin, since
/// a build can no longer reach a rival `Basics` to ask the question of.
#[test]
fn a_non_core_packages_basics_is_reserved_even_wrapped() {
    let root = fixture_package("package_wrapped_rival_basics");

    let error = compile_package(&root).expect_err("`Basics` is reserved for `zelkova-core`");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 2, "got {:?}", errors);

    let reserved = errors
        .iter()
        .find_map(|e| match e {
            resolve::Error::ReservedModuleName { package, module } => Some((package, module)),
            _ => None,
        })
        .expect("a ReservedModuleName error naming acme-basics's own Basics");
    assert_eq!(reserved.0.as_str(), "acme-basics");
    assert_eq!(reserved.1.module.as_str(), "Basics");

    assert!(
        errors.iter().any(|e| matches!(
            e,
            resolve::Error::DependencyNotCompiled { package, dependency }
                if package.as_str() == "package-wrapped-rival-basics"
                    && dependency.as_str() == "acme-basics"
        )),
        "expected the root to be reported as not compiled because of acme-basics, got {:?}",
        errors
    );

    assert!(
        type_errors(&error).is_empty(),
        "nothing of acme-basics or the root ever reaches the typer, got {:?}",
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
/// qualified name `crates/zelkova-compiler/src/scalars.rs` recognises — and `42` unifies with it.
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
        &fixture_package("dep_widgets"),
        SourceRoot::Src,
        Some(&widgets),
        &Overlay::new(),
        &mut sources,
    )
    .expect("the fixture loads");
    load_package_sources_into(
        &fixture_package("package_module_name_collision"),
        SourceRoot::Src,
        Some(&collision),
        &Overlay::new(),
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
        names.contains(&"acme-widgets:src/Size.zel".to_string()),
        "a file has to name the package it belongs to, got {:?}",
        names
    );
    assert!(
        names.contains(&"package-module-name-collision:src/Size.zel".to_string()),
        "a file has to name the package it belongs to, got {:?}",
        names
    );
}

// ── The second source root ───────────────────────────────────────────────────

/// Every diagnostic message a failed `compile_package` produced, phase errors
/// included.
fn diagnostic_messages(error: &BuildError) -> Vec<String> {
    accumulated(error)
        .into_iter()
        .map(|error| unwrap_in_file(error).as_diagnostic().message)
        .collect()
}

/// `LANG-15`: a module under `tests/` may import any module of its own package, the
/// ones `private-modules` names included. A package whose internals could only be
/// tested through its public surface would be pushed into exposing them.
///
/// The fixture's `Hidden` is private and `tests/HiddenTest.zel` annotates and calls
/// through it, so the name has to resolve as a module, as a type and as a value.
///
/// Mutation-checked by giving the tests pass an environment built from what the
/// package *publishes* rather than the live `interfaces` map `compile_in_build`
/// carries over from `src/` — the private module then reaches the test module no
/// longer and this goes red. That the tests root is compiled at all is pinned
/// separately, by
/// [`a_test_module_that_does_not_check_fails_only_when_tests_are_compiled`]: a
/// compiler that never compiles `tests/` passes this test.
#[test]
fn a_test_module_imports_a_private_module_of_its_own_package() {
    let root = fixture_package("package_test_root");

    assert_eq!(
        module_names(&root, SourceRoot::Src),
        vec!["src/Hidden.zel".to_string()],
        "the fixture must hold the private module this test is about"
    );
    assert_eq!(
        module_names(&root, SourceRoot::Tests),
        vec!["tests/HiddenTest.zel".to_string()],
        "the fixture must hold the test module this test is about"
    );

    let result = compile_package_with_tests(&root);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// Nothing may import a test module, and a `src/` module naming one fails as a module
/// that does not exist — the same answer a private module of another package gives,
/// and for the same reason: the name is absent from the map that `src/` is checked
/// against.
///
/// Mutation-checked by seeding the tests root's modules into the `src/` pass (walking
/// both roots in one `check_in_order` call): `AppTest` then resolves and the fixture
/// compiles.
#[test]
fn a_src_module_may_not_import_a_test_module() {
    let root = fixture_package("package_src_imports_test");

    assert_eq!(
        module_names(&root, SourceRoot::Tests),
        vec!["tests/AppTest.zel".to_string()],
        "the fixture must hold the test module `App` reaches for"
    );

    let error = compile_package_with_tests(&root)
        .expect_err("a module of `src/` must not reach a module of `tests/`");

    let messages = diagnostic_messages(&error);
    assert!(
        messages
            .iter()
            .any(|m| m.contains("cannot find a module named `AppTest`")),
        "expected the import to fail as an unknown module, got {:?}",
        messages
    );
}

/// The two roots share one set of module names: `src/Model.zel` and `tests/Model.zel`
/// are both `Model`, which is the same error as two modules of one root answering to
/// one name. Both files are named, because the package name alone cannot say which of
/// the two to change.
///
/// Mutation-checked by leaving the `tests/` modules out of the `local_modules` list
/// `compile_in_build` builds, which it and `compile_tests` both hand `visible_modules`:
/// each then sees the `src/` modules alone, the collision disappears and the fixture
/// compiles. *When* the collision is reported is pinned by
/// [`a_collision_between_the_roots_is_reported_before_src_is_checked`].
#[test]
fn one_module_name_under_both_roots_names_both_files() {
    let root = fixture_package("package_name_under_both_roots");

    let error =
        compile_package_with_tests(&root).expect_err("`Model` is declared under both source roots");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);
    assert!(
        matches!(errors[0], resolve::Error::ModuleNameCollision { .. }),
        "expected a module name collision, got {:?}",
        errors[0]
    );

    let notes = errors[0].notes();
    assert!(
        notes.iter().any(|n| n.contains("src/Model.zel"))
            && notes.iter().any(|n| n.contains("tests/Model.zel")),
        "both files must be named, got {:?}",
        notes
    );
}

/// The collision between the two roots is reported before any module of the package is
/// checked, so a `src/` that would also fail to check does not hide it: the fixture is
/// [`one_module_name_under_both_roots_names_both_files`]'s, plus a `src/Mismatch.zel`
/// that annotates `Apple` over a `Pear`, and the build reports the collision alone.
///
/// Mutation-checked by handing `compile_in_build`'s `visible_modules` call the `src/`
/// modules alone, while the `TestsEnvironment` still carries both roots: `src/` is then
/// checked, fails, and `compile_tests` is never reached, so the build reports the type
/// error and no collision at all. [`one_module_name_under_both_roots_names_both_files`]
/// stays green under that mutation, because its `src/` is clean and `compile_tests`
/// still finds the collision.
#[test]
fn a_collision_between_the_roots_is_reported_before_src_is_checked() {
    let root = fixture_package("package_name_under_both_roots_src_fails");

    let error =
        compile_package_with_tests(&root).expect_err("`Model` is declared under both source roots");

    let all = accumulated(&error);
    assert_eq!(all.len(), 1, "expected the collision alone, got {:?}", all);

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);
    assert!(
        matches!(errors[0], resolve::Error::ModuleNameCollision { .. }),
        "expected a module name collision, got {:?}",
        errors[0]
    );
}

/// A package name belongs to at most one of the two dependency maps. Writing it in
/// both is rejected when the manifest is read, before any source is loaded.
///
/// Mutation-checked by dropping the `DependencyListedTwice` loop in `manifest::load`:
/// the fixture then compiles, since both entries name the same directory.
#[test]
fn a_package_named_in_both_dependency_maps_is_rejected() {
    let root = fixture_package("package_dependency_listed_twice");

    let error = compile_package(&root)
        .expect_err("`acme-widgets` is written in both `dependencies` and `test-dependencies`");

    let BuildError::Check(CompilationError::Manifest(errors)) = &error else {
        panic!(
            "expected Err(BuildError::Check(CompilationError::Manifest(..))), got {:?}",
            error
        );
    };
    assert_eq!(errors.len(), 1, "got {:?}", errors);
    assert!(
        matches!(
            errors[0],
            manifest::ManifestError::DependencyListedTwice { .. }
        ),
        "expected DependencyListedTwice, got {:?}",
        errors[0]
    );
    assert!(
        errors[0].message().contains("acme-widgets"),
        "the package written twice must be named, got {:?}",
        errors[0].message()
    );
}

/// A package written in `test-dependencies` is available to `tests/`: the fixture's
/// test module annotates and calls through `AcmeExpect.Expect`, which is a module of
/// a package `dependencies` does not mention.
///
/// Mutation-checked by dropping the `test-dependencies` from the entries
/// `compile_tests` hands `direct_dependencies`: `AcmeExpect.Expect` then names nothing
/// and this goes red while its counterpart below stays green.
#[test]
fn a_test_dependency_reaches_the_tests_root() {
    let root = fixture_package("package_test_dependency");

    let result = compile_package_with_tests(&root);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// `GEN-18`: a test build writes a second, complete tree at `test/js/` — the root's
/// `tests/AppTest.zel` and the `test-dependency` it reaches through (`acme-expect`)
/// beside everything a plain build already writes — while `out/js/` itself stays exactly
/// what a plain build of the same package's `src/` would write: neither `AppTest.mjs`
/// nor `acme-expect/Expect.mjs` ever lands there, so a later plain `compile_package` run
/// never finds a test module a test build left behind.
///
/// Mutation-checked by dropping `test_tree_modules.extend(compiled.modules)` from the
/// test-only loop and the `test_tree_modules.extend(compile_tests(..))` around the call
/// to `compile_tests` in `compile`: `test/js/` then holds only the runtime and `App.mjs`,
/// missing both `Expect.mjs` and `AppTest.mjs`, and this goes red.
#[test]
fn a_test_build_writes_a_second_tree_beside_the_plain_one() {
    let build_dir = fresh_build_dir("a_test_build_writes_a_second_tree_beside_the_plain_one");

    let result = zelkova::compile_package_with_tests_into(
        &fixture_package("package_test_dependency"),
        &build_dir,
    );
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    assert_eq!(
        files_under(&build_dir),
        vec![
            "out/js/package-test-dependency/App.mjs".to_string(),
            "out/js/zelkova.mjs".to_string(),
            "test/js/acme-expect/Expect.mjs".to_string(),
            "test/js/package-test-dependency/App.mjs".to_string(),
            "test/js/package-test-dependency/AppTest.mjs".to_string(),
            "test/js/zelkova.mjs".to_string(),
        ]
    );
}

/// …and a plain build over the same fixture writes neither half of that second tree: it
/// never checks `tests/` or resolves `test-dependencies` into anything to compile, so
/// there is no `test/` directory at all and `acme-expect` — reachable only as a
/// `test-dependency` — is never compiled, let alone written.
///
/// Mutation-checked by having `compile`'s first loop not `continue` past a `test_only`
/// package: `out/js/acme-expect/Expect.mjs` then appears and this goes red.
#[test]
fn a_plain_build_writes_no_test_tree() {
    let build_dir = fresh_build_dir("a_plain_build_writes_no_test_tree");

    let result =
        zelkova::compile_package_into(&fixture_package("package_test_dependency"), &build_dir);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    assert_eq!(
        files_under(&build_dir),
        vec![
            "out/js/package-test-dependency/App.mjs".to_string(),
            "out/js/zelkova.mjs".to_string(),
        ]
    );
}

/// …and to nothing else. The same import written in `src/` reaches no module, because
/// a `test-dependency`'s modules are held out of the environment `src/` is checked
/// against.
///
/// Mutation-checked by handing `compile_in_build`'s `direct_dependencies` call the
/// `test-dependencies` as well as the `dependencies`: the fixture then compiles.
#[test]
fn a_test_dependency_does_not_reach_the_src_root() {
    let root = fixture_package("package_src_uses_test_dependency");

    let error = compile_package_with_tests(&root)
        .expect_err("a test-dependency is not importable from `src/`");

    let messages = diagnostic_messages(&error);
    assert!(
        messages
            .iter()
            .any(|m| m.contains("cannot find a module named `AcmeExpect.Expect`")),
        "expected the import to fail as an unknown module, got {:?}",
        messages
    );
}

/// `BUG-43`'s fixture (`package_test_cross_module_calls`) is otherwise reached only by
/// `cargo run -- test`, which `cargo test` never runs ([`DEC-18` decision
/// 6](../docs/decisions/dec-18.md)) — so a `cargo test` run never even compiled it, let
/// alone checked what it emits. This pins that it compiles under
/// `compile_package_with_tests_into`, against the real `zelkova-core` and `zelkova-test`
/// interfaces rather than the hand-built ones `crates/zelkova-js/tests/javascript.rs`'s `emitted_across`
/// uses, and that the emitted `PickTest.mjs` calls `Lib.pick` directly with both
/// arguments — the cross-module call BUG-43 fixed.
///
/// Mutation-checked by reverting `canonical::Module::to_interface` to record no arities
/// at all (an empty map in place of the one built from `emitted_arity`): the emitted call
/// goes back to `pick(true)(false)` and this assertion goes red.
#[test]
fn cross_module_arity_fixture_compiles_and_calls_directly() {
    let build_dir = fresh_build_dir("cross_module_arity_fixture_compiles_and_calls_directly");

    let result = zelkova::compile_package_with_tests_into(
        &fixture_package("package_test_cross_module_calls"),
        &build_dir,
    );
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    let pick_test = std::fs::read_to_string(
        build_dir.join("test/js/package-test-cross-module-calls/PickTest.mjs"),
    )
    .unwrap();
    assert!(
        pick_test.contains("package_test_cross_module_calls$Lib$pick(true, false)"),
        "expected a direct call supplying both arguments, got:\n{}",
        pick_test
    );
}

/// `LANG-63`: `compile_package_with_tests` hands back the root's checked `tests/`
/// `Interface`s, and `collection::collect` finds a test among them by the
/// exposed value's full `QualName` — package included — never by the spelling
/// `Test` alone. The fixture's `tests/AppTest.zel` exposes three values: `addsUp`,
/// a real `zelkova-test:Test.Test`; `helper`, an unrelated `Int`; and `decoyTest`,
/// of a type the fixture declares itself and also spells `Test`. Only `addsUp` is
/// collected.
///
/// Neutralise-checked twice. As the ticket's acceptance asks: comparing types through
/// `name.unqualified_name()` instead of the whole `QualName` in
/// `collection::is_test` makes `decoyTest` join `addsUp` in the collected set,
/// and this assertion goes red. Separately: dropping the `root_test_interfaces =
/// ..` assignment in `compile`'s test-tree branch leaves `AppTest` out of what
/// `compile_package_with_tests` hands back at all, and the `.expect` above panics —
/// which is what pins that the `Interface`s actually travel out of `compile`'s
/// private state rather than being computed independently by this test. Both
/// restored afterwards.
#[test]
fn a_test_is_found_by_its_qualname_not_its_spelling() {
    let root = fixture_package("package_test_collection");

    let interfaces = compile_package_with_tests(&root).expect("expected the fixture to compile");

    let collected = collection::collect(&interfaces);
    let app_test = collected
        .iter()
        .find(|module| module.module.name().as_str() == "AppTest")
        .expect("AppTest must be among the checked test modules");

    assert_eq!(
        app_test.tests,
        vec![Name::new("addsUp")],
        "only the exposed value of the real `Test` type must be collected, got {:?}",
        app_test.tests
    );
}

/// A package that holds no tests compiles as one. `package_checks` has no `tests/`
/// directory at all and `package_tests_without_modules` has one holding a `.mjs` and no
/// `.zel`, so between them both ways of holding no test modules are covered — and neither is
/// the missing-source-root failure a package with no `src/` is.
///
/// Mutation-checked by dropping the `SourceRoot::Tests` early return in
/// `load_package_sources_into`: `package_checks` then fails with `tests/` reported as
/// a path that does not exist.
#[test]
fn a_package_with_no_test_modules_compiles_with_its_tests() {
    let no_root = fixture_package("package_checks");
    assert!(
        !no_root.join("tests").exists(),
        "the fixture must have no `tests/` for this test to mean anything"
    );

    let result = compile_package_with_tests(&no_root);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    let no_modules = fixture_package("package_tests_without_modules");
    assert!(
        no_modules.join("tests").exists(),
        "the fixture must have a `tests/` for this half to mean anything"
    );
    assert_eq!(
        module_names(&no_modules, SourceRoot::Tests),
        Vec::<String>::new(),
        "the fixture's `tests/` holds a `.mjs` and no module"
    );

    let result = compile_package_with_tests(&no_modules);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// A module under `tests/` is checked like any other, and a failure in one is a
/// failure of the build that compiled it. A build that did not ask for the tests root
/// never reads the same file.
///
/// The two halves are one test because either alone can be satisfied by doing
/// nothing: a compiler that never compiles `tests/` passes the second, and one that
/// always does passes the first.
///
/// Mutation-checked once per half, because no single line carries both.
///
/// The first half — a failing test module fails the build that compiled it — goes red
/// when `compile` never calls `compile_tests`.
///
/// The second half — a build that did not ask for the tests root does not read it —
/// goes red when `compile_package` is changed to pass `TestRoot::Compiled`, the one
/// place that decides it: `compile`'s second stage runs only for the
/// `TestsEnvironment` `compile_in_build` hands back, and it hands one back only for
/// `TestRoot::Compiled`.
#[test]
fn a_test_module_that_does_not_check_fails_only_when_tests_are_compiled() {
    let root = fixture_package("package_broken_test");

    let error = compile_package_with_tests(&root)
        .expect_err("`AppTest` names a value `App` does not declare");

    let messages = diagnostic_messages(&error);
    assert!(
        messages.iter().any(|m| m.contains("[AppTest]")),
        "the failing test module must be named, got {:?}",
        messages
    );

    let result = compile_package(&root);
    assert!(
        result.is_ok(),
        "a build that did not ask for the tests root must not read it, got {:?}",
        result
    );
}

/// A file under `tests/` does not change what a module of `src/` means — and it is
/// held to the same reservation `src/` is: the eight default imports' names are
/// `zelkova-core`'s alone under either source root, so `tests/List.zel` fails
/// resolution with `ReservedModuleName` exactly when its build reads `tests/` at all.
///
/// A build that does not ask for the tests root never reads `tests/`, so it reports
/// nothing about `List` and `src/` still receives all eight — the fixture's `App.zel`
/// reaches `Flag` and `on` — a type and a *value* of its `Basics` — without writing an
/// import, which is what pins that `src/`'s own answer does not depend on `tests/` at
/// all, reservation included. A value, because a scalar type name is seeded for a
/// module of `zelkova-core` anyway (`LANG-58`), so a bare `Int` would resolve either
/// way.
///
/// Mutation-checked by dropping the `is_core` check ahead of `claim` in
/// `resolve::visible_modules`'s local-module loop: `compile_package_with_tests`
/// then succeeds instead of reporting `ReservedModuleName`, and the second assertion
/// panics.
#[test]
fn a_test_module_named_after_a_default_import_is_reserved_too() {
    let root = fixture_package("package_test_named_like_a_default");

    assert_eq!(
        module_names(&root, SourceRoot::Tests),
        vec!["tests/List.zel".to_string()],
        "the fixture must hold the test module named after a default import"
    );

    let result = compile_package(&root);
    assert!(
        result.is_ok(),
        "a build that does not read `tests/` must not report `List`, got {:?}",
        result
    );

    let error = compile_package_with_tests(&root)
        .expect_err("`tests/List.zel` is reserved for `zelkova-core` too");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);
    match &errors[0] {
        resolve::Error::ReservedModuleName { package, module } => {
            assert_eq!(package.as_str(), "package-test-named-like-a-default");
            assert_eq!(module.module.as_str(), "List");
        }
        other => panic!("expected ReservedModuleName, got {:?}", other),
    }
}

/// `SPEC-34`: an ordinary application holding its own `src/List.zel` fails resolution
/// with `ReservedModuleName`, whether or not `zelkova-core` is among its dependencies
/// — the two fixtures cover both, since the compiler does not supply core on its own
/// (`LANG-62`) and the reservation cannot rest on there being anything to collide
/// with. Before this rule, a package naming no dependency had nothing to collide with
/// and a package depending on core but naming a module core does not compile yet
/// (`List` is `.ignored` in real `std/core`) collided with nothing either — both were
/// silently taken for core-shaped instead of being told the name is not theirs to use.
///
/// Mutation-checked by dropping the `is_core` check ahead of `claim` in
/// `resolve::visible_modules`'s local-module loop: both fixtures then compile `List`
/// as an ordinary module instead of being rejected, and both `expect_err` calls panic.
#[test]
fn a_packages_own_list_is_reserved_with_and_without_core() {
    for (fixture, package) in [
        (
            "package_reserved_list_no_core",
            "package-reserved-list-no-core",
        ),
        (
            "package_reserved_list_with_core",
            "package-reserved-list-with-core",
        ),
    ] {
        let root = fixture_package(fixture);

        let error = compile_package(&root)
            .expect_err("a package's own `List` is reserved for `zelkova-core`");

        let errors = resolution_errors(&error);
        assert_eq!(errors.len(), 1, "{}: got {:?}", fixture, errors);
        match &errors[0] {
            resolve::Error::ReservedModuleName {
                package: reported,
                module,
            } => {
                assert_eq!(reported.as_str(), package, "{}", fixture);
                assert_eq!(module.module.as_str(), "List", "{}", fixture);
            }
            other => panic!("{}: expected ReservedModuleName, got {:?}", fixture, other),
        }
    }
}

/// A package reached only through `test-dependencies` is resolved into every build of
/// its dependent and compiled by the build that asked for the tests alone.
///
/// The version and cycle rules are settled over the union of the two maps, so the
/// package has to be *resolved* either way (`docs/spec/packages.md`'s
/// *`test-dependencies`*). Compiling it is a
/// different question, and the answer is the same one `src/` gets: a build that did not
/// ask for the tests cannot import a single module of it, so parsing and checking it
/// would only give that build a way to fail on a package it never reached for.
///
/// The fixture's test-dependency is `dep_broken`, which fails canonicalization, so the
/// two builds are told apart by their outcome rather than by a status line.
///
/// Mutation-checked by dropping the `test_only` skip in `compile`'s build loop:
/// `compile_package` then fails with `acme-broken`'s own canonicalization error.
#[test]
fn a_test_dependency_is_compiled_only_when_the_tests_are() {
    let root = fixture_package("package_broken_test_dependency");

    let result = compile_package(&root);
    assert!(
        result.is_ok(),
        "a build that did not ask for the tests must not compile a test-dependency, got {:?}",
        result
    );

    let error = compile_package_with_tests(&root)
        .expect_err("the test-dependency does not compile, and this build compiles it");

    let messages = diagnostic_messages(&error);
    assert!(
        messages
            .iter()
            .any(|m| m.contains("cannot find a type named `Nope.Nope`")),
        "the test-dependency's own error must be what fails this build, got {:?}",
        messages
    );
}

/// `SPEC-35`: a `test-dependency` may depend on the package it tests. `acme-lib` names
/// `acme-check` in its `test-dependencies`, `acme-check` names `acme-lib` in its
/// `dependencies`, and the edge back names `acme-lib`'s `src/` — so the build is
/// `acme-lib`'s `src/`, then `acme-check`, then `acme-lib`'s `tests/`, and not a cycle.
///
/// The test module imports a private module of its own package and `acme-check`, and
/// builds `acme-check`'s `Verdict` out of `Lib.token`: `acme-check` was checked against
/// the interfaces `acme-lib`'s `src/` published, and `tests/` against the ones it kept,
/// and the two have to agree that `Lib.Token` is one type.
///
/// Mutation-checked two ways, each red on its own. Restoring the unconditional
/// `Error::Cycle` branch in `Resolver::visit` (dropping the `at == 0 &&
/// through_test_dependency` return) fails the build with `Cycle([acme-lib,
/// acme-check])`. Compiling the test-only packages in `compile`'s first loop, in build
/// order, reaches `acme-check` before `acme-lib` has published anything, and fails it
/// with `DependencyNotCompiled`.
#[test]
fn a_test_dependency_may_depend_on_the_package_it_tests() {
    let root = fixture_package("package_acme_lib");
    assert_eq!(
        module_names(&root, SourceRoot::Tests),
        vec!["tests/LibTest.zel".to_string()],
        "the fixture must hold the test module this test is about"
    );

    let build_dir = root.join("build");
    if build_dir.exists() {
        std::fs::remove_dir_all(&build_dir).unwrap();
    }

    let result = compile_package_with_tests(&root);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);

    // `acme-check` and `LibTest` are checked for the tests and never written to `out/js/` —
    // only `test/js/` holds them, beside `acme-lib`'s own modules (`GEN-18`).
    assert_eq!(
        files_under(&build_dir),
        vec![
            "out/js/acme-lib/Internal.mjs".to_string(),
            "out/js/acme-lib/Lib.mjs".to_string(),
            "out/js/zelkova.mjs".to_string(),
            "test/js/acme-check/Check.mjs".to_string(),
            "test/js/acme-lib/Internal.mjs".to_string(),
            "test/js/acme-lib/Lib.mjs".to_string(),
            "test/js/acme-lib/LibTest.mjs".to_string(),
            "test/js/zelkova.mjs".to_string(),
        ]
    );
}

/// The negative of [`a_test_dependency_may_depend_on_the_package_it_tests`], so that its
/// `Ok` is known to come from a `tests/` root that was checked. The fixture pair is the
/// same arrangement, and the test module builds `acme-check`'s `Verdict` out of
/// `Internal.secret` rather than `Lib.token`: both modules resolve — the private
/// `Internal` from `acme-lib`'s own `src/`, `AcmeCheck.Check` from the test-only package
/// compiled against it — and the build fails with a type error in `LibTest`, the one
/// module under `tests/`, and nothing else.
///
/// Mutation-checked by making `compile` skip `compile_tests`: the build is then `Ok`.
/// Dropping `acme-lib`'s own modules from the `interfaces` `TestsEnvironment` carries
/// turns the error into a canonicalization error about `Internal`, and this goes red
/// on the type-error assertion.
#[test]
fn a_test_module_that_does_not_check_fails_when_its_test_dependency_depends_on_the_package() {
    let root = fixture_package("package_acme_lib_failing_test");
    assert_eq!(
        module_names(&root, SourceRoot::Tests),
        vec!["tests/LibTest.zel".to_string()],
        "the fixture must hold the test module this test is about"
    );

    let error = compile_package_with_tests(&root)
        .expect_err("`Holds` takes a `Lib.Token`, and `LibTest` hands it an `Internal.Secret`");

    let all = accumulated(&error);
    assert_eq!(all.len(), 1, "expected one error, got {:?}", all);
    match unwrap_in_file(all[0]) {
        CompilationError::Type(_, module) => assert_eq!(module.as_str(), "LibTest"),
        other => panic!("expected a type error in `LibTest`, got {:?}", other),
    }
}

/// The same pair built without the tests: `acme-check` is resolved, since the rules of
/// the build hold over both maps, but it is test-only and is never compiled.
///
/// Mutation-checked by having `test_only_packages`'s walk follow `test-dependencies` as
/// well as `dependencies`: `acme-check` is then no longer test-only and the
/// `test_only` assertion goes red.
#[test]
fn a_plain_build_leaves_a_test_dependency_on_its_dependent_uncompiled() {
    let root = fixture_package("package_acme_lib");

    let manifest = manifest::load(&root).expect("the fixture's manifest is valid");
    let root_name = manifest.name.clone();
    let build = resolve::resolve(&root, manifest).expect("the pair resolves");

    let names: Vec<&str> = build.iter().map(|p| p.name.as_str()).collect();
    assert_eq!(names, ["acme-check", "acme-lib"]);

    let test_only = resolve::test_only_packages(&build, &root_name);
    let expected: std::collections::HashSet<PackageName> =
        std::iter::once(PackageName::new("acme-check").unwrap()).collect();
    assert_eq!(test_only, expected);

    let build_dir =
        fresh_build_dir("a_plain_build_leaves_a_test_dependency_on_its_dependent_uncompiled");
    let result = zelkova::compile_package_into(&root, &build_dir);
    assert!(result.is_ok(), "expected Ok, got {:?}", result);
    assert_eq!(
        files_under(&build_dir),
        vec![
            "out/js/acme-lib/Internal.mjs".to_string(),
            "out/js/acme-lib/Lib.mjs".to_string(),
            "out/js/zelkova.mjs".to_string(),
        ]
    );
}

/// A cycle through the root's `dependencies` is still a cycle when a `test-dependency`
/// also reaches it, and when that `test-dependency`'s name sorts first. `acme-back`
/// depends on the root, and the root names it in `dependencies`; `acme-audit`, in
/// `test-dependencies`, depends on `acme-back` too.
///
/// Mutation-checked by visiting both maps' entries in one name-sorted list in
/// `Resolver::visit`, as it did before `SPEC-35`: `acme-audit` is then visited first,
/// its chain meets the root and stops without an error, `acme-back` is resolved, and
/// the plain entry finds it resolved — so the build compiles.
#[test]
fn a_dependency_cycle_through_the_root_is_reported_whichever_map_reaches_it_first() {
    let root = fixture_package("package_cycle_via_test_dependency");

    let error = compile_package(&root).expect_err("`acme-back` and the root depend on each other");

    let errors = resolution_errors(&error);
    assert_eq!(errors.len(), 1, "got {:?}", errors);

    let resolve::Error::Cycle(packages) = errors[0] else {
        panic!("expected a package cycle, got {:?}", errors[0]);
    };

    let mut names: Vec<&str> = packages.iter().map(|p| p.as_str()).collect();
    names.sort_unstable();
    assert_eq!(names, ["acme-back", "package-cycle-via-test-dependency"]);
}

// ── A program's `main` ───────────────────────────────────────────────────────

/// The one error a build of `root` fails with, whatever it is wrapped in.
fn only_error(root: &Path, error: BuildError) -> BuildError {
    let BuildError::Many(mut errors) = error else {
        panic!(
            "expected Err(BuildError::Many(..)) from {:?}, got {:?}",
            root, error
        );
    };
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);
    errors.remove(0)
}

/// The error of the check `error` wraps, for a test that expects the check to have failed
/// rather than emitting the build.
fn as_check(error: &BuildError) -> &CompilationError {
    match error {
        BuildError::Check(error) => error,
        other => panic!("expected an error of the check, got {:?}", other),
    }
}

/// The byte range `needle` occupies in `root`'s `src/App.zel`, found from the left.
fn range_in_app(root: &Path, needle: &str) -> std::ops::Range<usize> {
    let source =
        std::fs::read_to_string(root.join("src").join("App.zel")).expect("fixture is readable");
    let start = source
        .find(needle)
        .unwrap_or_else(|| panic!("`src/App.zel` holds {:?}", needle));
    start..(start + needle.len())
}

/// [Programs](../docs/spec/packages.md#programs)' own example: `main = Task.succeed ()`,
/// annotated `Task ()`, in the module the manifest names. It compiles.
///
/// Mutation-checked by making `program::is_task_of_unit` answer `false`: the build then
/// fails with a `MainNotTask` naming `Task ()`.
#[test]
fn a_main_of_type_task_unit_compiles() {
    let root = fixture_package("package_main_ok");
    assert_eq!(module_names(&root, SourceRoot::Src), vec!["src/App.zel"]);

    let result = compile_package(&root);

    assert!(result.is_ok(), "expected Ok, got {:?}", result);
}

/// `main = "Program"` names a module that exists only under `tests/`, which does not
/// count, so the manifest names no module the package holds — whether or not the build
/// compiled the tests. The error is the manifest's and has no span to point at.
///
/// Mutation-checked by deleting the `if let Some(main) = &package.manifest.main` call in
/// `compile_in_build`: both builds compile.
#[test]
fn a_main_naming_no_module_under_src_is_a_manifest_error() {
    let root = fixture_package("package_main_module_missing");
    assert_eq!(
        module_names(&root, SourceRoot::Tests),
        vec!["tests/Program.zel"]
    );

    let without_tests = compile_package(&root).map(|_| ());
    let with_tests = compile_package_with_tests(&root).map(|_| ());

    for result in [without_tests, with_tests] {
        let error = only_error(
            &root,
            result.expect_err("`main` names no module under `src/`"),
        );
        let CompilationError::Manifest(manifest_errors) = as_check(&error) else {
            panic!("expected a CompilationError::Manifest, got {:?}", error);
        };
        match manifest_errors.as_slice() {
            [manifest::ManifestError::MainModuleNotFound { name, .. }] => {
                assert_eq!(name, &Name::from("Program"));
            }
            other => panic!("expected one MainModuleNotFound, got {:?}", other),
        }
        assert!(
            error.as_diagnostic().labels.is_empty(),
            "`zelkova.toml` is not a file a label can point into"
        );
        assert!(
            error
                .as_diagnostic()
                .notes
                .iter()
                .any(|note| note.contains("has no location")),
            "the notes say why there is no caret: {:?}",
            error.as_diagnostic().notes
        );
    }
}

/// `App` declares `main : Task ()` and exposes only `helper`. The caret is under the
/// header's `exposing (helper)`, which is what has to change, and a secondary label
/// shows the `main` it left out.
///
/// Mutation-checked two ways: making `program::check` look `main` up in
/// `canonical.values` alone, ignoring what the header exposes, compiles the fixture; and
/// building `parser::Module::exposing_span` as `NodeSpan::none()` in the grammar drops
/// the primary label.
#[test]
fn a_main_module_exposing_no_main_is_an_error_at_its_exposing_list() {
    let root = fixture_package("package_main_not_exposed");

    let error = only_error(
        &root,
        compile_package(&root).expect_err("`App` does not expose `main`"),
    );
    match unwrap_in_file(as_check(&error)) {
        CompilationError::Program(program_errors, module) => {
            assert_eq!(module, &Name::from("App"));
            assert!(
                matches!(
                    program_errors.as_slice(),
                    [zelkova_compiler::program::Error::MainNotExposed { .. }]
                ),
                "got {:?}",
                program_errors
            );
        }
        other => panic!("expected a Program error, got {:?}", other),
    }

    let diagnostic = error.as_diagnostic();
    assert_eq!(diagnostic.labels.len(), 2, "got {:?}", diagnostic.labels);
    assert_eq!(diagnostic.labels[0].style, LabelStyle::Primary);
    assert_eq!(
        diagnostic.labels[0].range,
        range_in_app(&root, "exposing (helper)")
    );
    assert_eq!(diagnostic.labels[1].style, LabelStyle::Secondary);
    let declaration = range_in_app(&root, "main : Task ()");
    let body = range_in_app(&root, "Task.succeed ()");
    assert_eq!(diagnostic.labels[1].range, declaration.start..body.end);
}

/// `App` exposes `main : Int`. The diagnostic prints the type `main` has, and the caret
/// is under its annotation.
///
/// Mutation-checked by making `program::is_task_of_unit` answer `true`: the fixture
/// compiles.
#[test]
fn a_main_of_another_type_is_an_error_at_its_annotation() {
    let root = fixture_package("package_main_not_task");

    let error = only_error(
        &root,
        compile_package(&root).expect_err("`main : Int` is not a `Task ()`"),
    );
    match unwrap_in_file(as_check(&error)) {
        CompilationError::Program(program_errors, module) => {
            assert_eq!(module, &Name::from("App"));
            match program_errors.as_slice() {
                [zelkova_compiler::program::Error::MainNotTask {
                    found: Some(found), ..
                }] => assert_eq!(found.to_string(), "Int"),
                other => panic!("expected one MainNotTask, got {:?}", other),
            }
        }
        other => panic!("expected a Program error, got {:?}", other),
    }

    let diagnostic = error.as_diagnostic();
    assert!(
        diagnostic.message.contains("`Int`"),
        "the headline names the type `main` has: {}",
        diagnostic.message
    );
    assert_eq!(diagnostic.labels.len(), 1, "got {:?}", diagnostic.labels);
    assert_eq!(diagnostic.labels[0].style, LabelStyle::Primary);
    assert_eq!(
        diagnostic.labels[0].range,
        range_in_app(&root, "main : Int")
    );
}

/// `main : Task ()` where `Task` is a union `Effect` declares. `Task` is known by the
/// qualified name of `zelkova-core`'s declaration, so this one is another type that
/// happens to print the same, and the headline says which `Task` it is.
///
/// Mutation-checked by making `program::is_core_task` compare the unqualified name alone:
/// the fixture compiles.
#[test]
fn a_main_of_a_task_some_other_module_declares_is_an_error() {
    let root = fixture_package("package_main_own_task");

    let error = only_error(
        &root,
        compile_package(&root).expect_err("`Effect.Task` is not `Task.Task`"),
    );
    match unwrap_in_file(as_check(&error)) {
        CompilationError::Program(program_errors, _) => assert!(
            matches!(
                program_errors.as_slice(),
                [zelkova_compiler::program::Error::MainNotTask { .. }]
            ),
            "got {:?}",
            program_errors
        ),
        other => panic!("expected a Program error, got {:?}", other),
    }
    let message = error.as_diagnostic().message;
    assert!(message.contains("`Effect.Task`"), "got {}", message);
}

/// A dependency's `main` is checked like the root's: `package-main-not-task` is a
/// dependency here, not the package the compiler was pointed at, and its `main : Int`
/// still fails the build.
///
/// Mutation-checked by running the check only when `package.name` is the build's root:
/// the fixture compiles.
#[test]
fn a_dependencys_main_is_checked_too() {
    let root = fixture_package("package_depends_on_broken_program");

    let error = only_error(
        &root,
        compile_package(&root).expect_err("the dependency's `main` is an `Int`"),
    );
    assert!(
        matches!(
            unwrap_in_file(as_check(&error)),
            CompilationError::Program(_, module) if module == &Name::from("App")
        ),
        "got {:?}",
        error
    );
}

/// `main : Task ()` refers to an unannotated `helper`, so the typer leaves `main`
/// unchecked and inference solves nothing for it. The annotation is what is judged, it is
/// right, and the build fails with the emitter's own refusal instead of a claim that
/// `main` is not a `Task ()`.
///
/// Mutation-checked by deleting the `.or_else(..)` fallback in `program::check`: the
/// diagnostic becomes "the type of `main` could not be determined".
#[test]
fn an_unchecked_main_is_judged_by_its_annotation() {
    let root = fixture_package("package_main_unchecked");

    let error = only_error(
        &root,
        compile_package(&root).expect_err("the typer cannot check `main`, so it cannot be emitted"),
    );

    let message = error.as_diagnostic().message;
    assert!(
        message.contains("the type checker could not check it"),
        "the emitter's refusal, got {}",
        message
    );
    assert!(!message.contains("must have type"), "got {}", message);
}

/// The same shape with `main : Int`: the annotation is wrong, and that is what the
/// diagnostic says even though inference never reached `main`.
///
/// Mutation-checked with the same deletion: the headline loses `Int`.
#[test]
fn an_unchecked_main_with_the_wrong_annotation_is_still_rejected() {
    let root = fixture_package("package_main_unchecked_wrong");

    let error = only_error(
        &root,
        compile_package(&root).expect_err("`main : Int` is not a `Task ()`"),
    );

    let message = error.as_diagnostic().message;
    assert!(message.contains("`Int`"), "got {}", message);
    assert!(
        message.contains("must have type `Task ()`"),
        "got {}",
        message
    );
}

// ── Checking a package without building it ───────────────────────────────────

/// `TOOL-3`: `check_package` hands a type error back as data — in the file database it
/// returns, with its labels pointing into the right file — records its status lines
/// rather than printing them, and creates no `build/` directory.
///
/// `package_type_error` would write nothing through `compile_package` either, since it
/// does not check, so the `build/` assertion here is the ticket's floor and not what
/// tells the two halves apart: `check_package_writes_nothing_for_a_package_that_checks`
/// below is. Nothing here pins that nothing reaches stderr: the test process's stderr is
/// shared by every test running beside this one, so capturing it in-process would be
/// flaky. What pins it is the shape — `check` holds no writer, and its status lines come
/// back in `PackageCheck::status`, which the assertion on the `failure` line reads.
///
/// Mutation-checked three ways, each red on its own: making `check` call
/// `std::fs::create_dir_all(package_dir.join(BUILD_DIRECTORY))` (the `build/`
/// assertion); making `check_root` stop pushing its `failure` status (the status
/// assertion); and making `check_root` push its errors without their `InFile` wrapper
/// (the label assertion, since the labels then have no file to point into).
#[test]
fn check_package_hands_back_a_type_error_without_building() {
    let root = fixture_package("package_type_error");
    let build = root.join(BUILD_DIRECTORY);
    assert!(
        !build.exists(),
        "{:?} is left over from an earlier run; remove it",
        build
    );

    let source = std::fs::read_to_string(root.join("src").join("Mismatch.zel"))
        .expect("fixture is readable");
    let body = "'a'";
    let body_start = source
        .rfind(body)
        .expect("fixture's body is the literal `'a'`");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert!(
        !build.exists(),
        "checking a package must not create {:?}",
        build
    );

    assert_eq!(check.errors.len(), 1, "got {:?}", check.errors);
    assert!(
        matches!(
            unwrap_in_file(&check.errors[0]),
            CompilationError::Type(_, module) if module == &Name::from("Mismatch")
        ),
        "expected `Mismatch`'s type error, got {:?}",
        check.errors[0]
    );

    let diagnostic = check.errors[0].as_diagnostic();
    let primary = diagnostic
        .labels
        .iter()
        .find(|label| label.style == LabelStyle::Primary)
        .unwrap_or_else(|| panic!("expected a primary label, got {:?}", diagnostic.labels));
    let file = check
        .sources
        .get(primary.file_id)
        .expect("the label's file is in the returned database");
    assert_eq!(file.name(), "package-type-error:src/Mismatch.zel");
    assert_eq!(file.source(), &source);
    assert_eq!(primary.range, body_start..(body_start + body.len()));

    assert!(
        check
            .status
            .iter()
            .any(|status| !status.success && status.text.starts_with("checked modules")),
        "the failed check is recorded as a status line, got {:?}",
        check.status
    );
}

/// `TOOL-3`: `check_package` on a package that checks hands back its modules and writes
/// nothing, where `compile_package` on the very same directory writes `build/`.
///
/// The package is a copy of `package_checks` — which has no dependency, so it can be
/// copied anywhere — under Cargo's per-target scratch space, because other tests build the
/// fixture in place and would race this one's `build/` assertions.
///
/// Mutation-checked by making `check` call `output::write` on the runtime alone under
/// `package_dir.join(BUILD_DIRECTORY)`, the write `compile` does once a build checks:
/// the first `build/` assertion goes red. So does making `check` create `build/` outright.
#[test]
fn check_package_writes_nothing_for_a_package_that_checks() {
    let fixture = fixture_package("package_checks");
    let root = fresh_build_dir("check_package_writes_nothing");
    std::fs::create_dir_all(root.join("src")).unwrap();
    std::fs::copy(fixture.join("zelkova.toml"), root.join("zelkova.toml")).unwrap();
    std::fs::copy(
        fixture.join("src").join("Answer.zel"),
        root.join("src").join("Answer.zel"),
    )
    .unwrap();
    let build = root.join(BUILD_DIRECTORY);

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert!(check.errors.is_empty(), "got {:?}", check.errors);
    let names: Vec<&Name> = check
        .modules
        .iter()
        .map(|checked| checked.module.canonical.name.name())
        .collect();
    assert_eq!(names, vec![&Name::from("Answer")]);
    assert_eq!(check.modules[0].root_dir, root.join("src"));
    assert!(
        !build.exists(),
        "checking a package must not create {:?}",
        build
    );

    // The same directory does get a build from the CLI half, so the assertion above is
    // looking in the right place.
    compile_package(&root).expect("the package checks");
    assert!(build.join("out").join("js").is_dir());
}

/// `TOOL-3`: `check_package_with_tests` checks the root's `tests/` root as well and hands
/// the modules back in `test_modules` and `test_dependency_modules`, where `check_package`
/// on the same directory leaves both empty. Neither writes a `build/`.
///
/// The package is a copy of `package_test_run` with its two dependency paths made
/// absolute, under Cargo's per-target scratch space for the reason
/// `check_package_writes_nothing_for_a_package_that_checks` gives.
///
/// Mutation-checked by making `check_package_with_tests` pass `TestRoot::Skipped`: the
/// `test_modules` assertion goes red.
#[test]
fn check_package_with_tests_checks_the_tests_root_and_writes_nothing() {
    let fixture = fixture_package("package_test_run");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let root = fresh_build_dir("check_package_with_tests");
    for dir in ["src", "tests"] {
        std::fs::create_dir_all(root.join(dir)).unwrap();
    }
    for file in ["src/App.zel", "tests/AppTest.zel"] {
        std::fs::copy(fixture.join(file), root.join(file)).unwrap();
    }
    let manifest = std::fs::read_to_string(fixture.join("zelkova.toml"))
        .unwrap()
        .replace("../../../std", &repo.join("std").display().to_string());
    std::fs::write(root.join("zelkova.toml"), manifest).unwrap();
    let build = root.join(BUILD_DIRECTORY);

    let check = check_package_with_tests(&root, &Overlay::new())
        .expect("the manifest and the build resolve");

    assert!(check.errors.is_empty(), "got {:?}", check.errors);
    let test_names: Vec<&Name> = check
        .test_modules
        .iter()
        .map(|checked| checked.module.canonical.name.name())
        .collect();
    assert_eq!(test_names, vec![&Name::from("AppTest")]);
    assert_eq!(check.test_modules[0].root_dir, root.join("tests"));
    assert!(
        !check.test_dependency_modules.is_empty(),
        "`zelkova-test`'s modules are what `tests/` imports"
    );
    assert!(!build.exists(), "checking must not create {:?}", build);

    let src_only =
        check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");
    assert!(src_only.errors.is_empty(), "got {:?}", src_only.errors);
    assert!(src_only.test_modules.is_empty());
    assert!(src_only.test_dependency_modules.is_empty());
    assert_eq!(src_only.modules.len(), check.modules.len());
}

/// A copy of the `package_checks` fixture under Cargo's per-target scratch space, which
/// has no dependency and so can live anywhere. The `TOOL-2` tests overlay its `Answer`
/// module and must not touch `tests/fixtures/` in place, which other tests build
/// concurrently.
fn overlay_fixture(test: &str) -> std::path::PathBuf {
    let fixture = fixture_package("package_checks");
    let root = fresh_build_dir(test);
    std::fs::create_dir_all(root.join("src")).unwrap();
    std::fs::copy(fixture.join("zelkova.toml"), root.join("zelkova.toml")).unwrap();
    std::fs::copy(
        fixture.join("src").join("Answer.zel"),
        root.join("src").join("Answer.zel"),
    )
    .unwrap();
    root
}

/// What `Answer` holds in the `TOOL-2` overlay tests: the same module as on disk, with
/// a type error in it.
const OVERLAID_ANSWER: &str = "module Answer exposing (..)\n\n\ntype Label = Label\n\n\ntype Other = Other\n\n\nanswer : Label\nanswer = Other\n";

/// Asserts that `check` holds exactly the type error of `OVERLAID_ANSWER`, and that the
/// source its primary label's file resolves to is that text — the overlay's, not the
/// disk's.
fn assert_overlaid_answer_was_checked(check: &zelkova_compiler::PackageCheck) {
    assert_eq!(check.errors.len(), 1, "got {:?}", check.errors);
    assert!(
        matches!(
            unwrap_in_file(&check.errors[0]),
            CompilationError::Type(_, module) if module == &Name::from("Answer")
        ),
        "expected `Answer`'s type error, got {:?}",
        check.errors[0]
    );
    let diagnostic = check.errors[0].as_diagnostic();
    let primary = diagnostic
        .labels
        .iter()
        .find(|label| label.style == LabelStyle::Primary)
        .unwrap_or_else(|| panic!("expected a primary label, got {:?}", diagnostic.labels));
    let file = check
        .sources
        .get(primary.file_id)
        .expect("the label's file is in the returned database");
    assert_eq!(file.name(), "package-checks:src/Answer.zel");
    assert_eq!(file.source(), OVERLAID_ANSWER);
}

/// `TOOL-4`: a module with a syntax error in each of two declarations reports both, each
/// with a label in that file, and the file still counts once among those that failed to
/// parse. The declaration between them is annotated, because the module is checked
/// beside its syntax errors and `exposing (..)` would report an unannotated one.
///
/// Mutation-checked by making `parse_root` push only the first failure's error: the
/// error-count assertion goes red.
#[test]
fn check_package_reports_every_syntax_error_of_a_module() {
    const TWO_ERRORS: &str = "module Answer exposing (..)\n\nfirst = = 1\n\ntype Label = Label\n\nok : Label\nok = Label\n\nsecond = )\n";
    let root = overlay_fixture("every_syntax_error_of_a_module");
    let mut overlay = Overlay::new();
    overlay.insert(root.join("src").join("Answer.zel"), TWO_ERRORS.into());

    let check = check_package(&root, &overlay).expect("the manifest and the build resolve");

    assert_eq!(check.errors.len(), 2, "got {:?}", check.errors);
    let expected_starts = [
        TWO_ERRORS.find("= 1").unwrap(),
        TWO_ERRORS.rfind(")").unwrap(),
    ];
    for (error, expected_start) in check.errors.iter().zip(expected_starts) {
        assert!(
            matches!(unwrap_in_file(error), CompilationError::Source(..)),
            "expected a syntax error, got {:?}",
            error
        );
        let diagnostic = error.as_diagnostic();
        let primary = diagnostic
            .labels
            .iter()
            .find(|label| label.style == LabelStyle::Primary)
            .unwrap_or_else(|| panic!("expected a primary label, got {:?}", diagnostic.labels));
        let file = check
            .sources
            .get(primary.file_id)
            .expect("the label's file is in the returned database");
        assert_eq!(file.source(), TWO_ERRORS);
        assert_eq!(primary.range.start, expected_start);
    }

    assert!(
        check
            .status
            .iter()
            .any(|status| status.text == "parsed 0 modules, 1 failed to parse"),
        "the file counts once among those that failed, got {:?}",
        check.status
    );
}

/// `TOOL-2`: a module held in the overlay is checked in place of the file on disk, which
/// checks clean. The error comes back, and the source its label points into is the
/// overlay's text.
///
/// Mutation-checked by making `Overlay::get` always return `None`: the package checks
/// clean and the error-count assertion goes red.
#[test]
fn check_package_reads_a_module_from_the_overlay_instead_of_the_disk() {
    let root = overlay_fixture("overlay_replaces_disk");
    let clean = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");
    assert!(clean.errors.is_empty(), "got {:?}", clean.errors);

    let mut overlay = Overlay::new();
    overlay.insert(root.join("src").join("Answer.zel"), OVERLAID_ANSWER.into());
    // A buffer that is not a `.zel` source is ignored rather than loaded.
    overlay.insert(root.join("notes.txt"), "not a module".into());

    let check = check_package(&root, &overlay).expect("the manifest and the build resolve");

    assert_overlaid_answer_was_checked(&check);
}

/// `TOOL-2`: a module that only the overlay holds is walked in, so a module on disk that
/// imports it checks. Without the overlay the same package fails, which is what makes the
/// overlay the reason it checks.
///
/// Mutation-checked by removing the loop over `Overlay::zel_files_under` that follows the
/// walk in `load_package_sources_into`: the package no longer checks and the
/// `errors.is_empty()` assertion goes red.
#[test]
fn check_package_loads_a_module_that_only_the_overlay_holds() {
    let root = overlay_fixture("overlay_adds_module");
    std::fs::write(
        root.join("src").join("Answer.zel"),
        "module Answer exposing (..)\n\nimport Extra\n\n\nanswer : Extra.Token\nanswer = Extra.token\n",
    )
    .unwrap();

    let without =
        check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");
    assert!(
        !without.errors.is_empty(),
        "`Answer` imports a module that is on neither the disk nor the overlay"
    );

    let mut overlay = Overlay::new();
    overlay.insert(
        root.join("src").join("Extra.zel"),
        "module Extra exposing (..)\n\n\ntype Token = Token\n\n\ntoken : Token\ntoken = Token\n"
            .into(),
    );
    let check = check_package(&root, &overlay).expect("the manifest and the build resolve");

    assert!(check.errors.is_empty(), "got {:?}", check.errors);
    let mut names: Vec<String> = check
        .modules
        .iter()
        .map(|checked| checked.module.canonical.name.name().to_string())
        .collect();
    names.sort();
    assert_eq!(names, vec!["Answer", "Extra"]);
}

/// `TOOL-2`: the overlay finds a module by where its path points, not by how it is
/// spelled. The key goes through a sibling directory with a `..` segment, and the result
/// is `check_package_reads_a_module_from_the_overlay_instead_of_the_disk`'s.
///
/// Mutation-checked by making `overlay::normalise` return its argument: the key keeps its
/// `..` segment, the walked `Answer.zel` misses it, and the error-count assertion goes
/// red.
#[test]
fn check_package_matches_an_overlay_path_spelled_with_a_dot_dot_segment() {
    let root = overlay_fixture("overlay_normalises_paths");
    std::fs::create_dir_all(root.join("src").join("Sibling")).unwrap();

    let mut overlay = Overlay::new();
    overlay.insert(
        root.join("src")
            .join("Sibling")
            .join("..")
            .join("Answer.zel"),
        OVERLAID_ANSWER.into(),
    );

    let check = check_package(&root, &overlay).expect("the manifest and the build resolve");

    assert_overlaid_answer_was_checked(&check);
}

/// `TOOL-2`: a buffer under a `tests/` directory that is not on disk is still loaded. The
/// package has no `tests/`, which the walk skips, and the overlay's `tests/Foo.zel` comes
/// back in `test_modules` all the same.
///
/// Mutation-checked by restoring the early return `if !walk { return Ok(loaded); }` right
/// after the `walk` binding in `load_package_sources_into`: `test_modules` comes back
/// empty and the final assertion goes red.
#[test]
fn check_package_with_tests_loads_an_overlay_buffer_under_a_missing_tests_directory() {
    let root = overlay_fixture("overlay_adds_test_module");
    assert!(!root.join("tests").exists(), "the fixture has no `tests/`");

    let mut overlay = Overlay::new();
    overlay.insert(
        root.join("tests").join("Foo.zel"),
        "module Foo exposing (..)\n\n\ntype Token = Token\n".into(),
    );
    let check =
        check_package_with_tests(&root, &overlay).expect("the manifest and the build resolve");

    assert!(check.errors.is_empty(), "got {:?}", check.errors);
    let names: Vec<&Name> = check
        .test_modules
        .iter()
        .map(|checked| checked.module.canonical.name.name())
        .collect();
    assert_eq!(names, vec![&Name::from("Foo")]);
}

// ── A module with errors still has a shape ──────────────────────────────────

/// The names of the modules in `modules`, sorted.
fn sorted_module_names(modules: &[zelkova_compiler::CheckedSource]) -> Vec<String> {
    let mut names: Vec<String> = modules
        .iter()
        .map(|checked| checked.module.canonical.name.name().as_str().to_string())
        .collect();
    names.sort();
    names
}

/// A module with a type error in one declaration publishes its interface to the
/// modules that import it: `B` imports `A`'s well-typed `ok`, and the only error of
/// the check is `A`'s own, about `bad`.
///
/// Mutation-checked by making `check_in_order` insert the interface only for an
/// `Outcome::Module` whose error list is empty: `B` then reports `A` as a module that
/// does not exist, and the error count goes red.
#[test]
fn a_module_with_a_type_error_publishes_its_interface() {
    let root = fixture_package("package_import_type_error");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert_eq!(check.errors.len(), 1, "got {:?}", check.errors);
    assert!(
        matches!(&check.errors[0], CompilationError::InFile(..)),
        "expected the error to carry its file, got {:?}",
        check.errors[0]
    );
    assert!(
        matches!(
            unwrap_in_file(&check.errors[0]),
            CompilationError::Type(_, module) if module == &Name::from("A")
        ),
        "expected `A`'s type error, got {:?}",
        check.errors[0]
    );
}

/// A package that did not check hands back every module it built a tree for in
/// `failing`, and none in `modules`. `A`'s tree holds a typed `ok`, and lists the
/// rejected `bad` as unchecked with an error behind it.
///
/// Mutation-checked by making `type_check_recovering` return an empty `solved` when it
/// has errors: `ok` then has no declaration, and the `ok` assertion goes red.
#[test]
fn a_module_with_a_type_error_keeps_a_typed_tree() {
    let root = fixture_package("package_import_type_error");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert!(
        check.modules.is_empty(),
        "a package that did not check contributes no module, got {:?}",
        sorted_module_names(&check.modules)
    );
    assert_eq!(sorted_module_names(&check.failing), vec!["A", "B"]);

    let a = &check
        .failing
        .iter()
        .find(|checked| checked.module.canonical.name.name() == &Name::from("A"))
        .expect("`A` is among the failing modules")
        .module
        .ir;

    let ok = a
        .declarations
        .iter()
        .find(|declaration| declaration.name == Name::from("ok"))
        .unwrap_or_else(|| {
            panic!(
                "`ok` type checked and should have a declaration, got {:?}",
                a.declarations
                    .iter()
                    .map(|declaration| &declaration.name)
                    .collect::<Vec<_>>()
            )
        });
    assert_eq!(ok.tpe.to_string(), "T");

    let unchecked: Vec<(&Name, bool)> = a
        .unchecked
        .iter()
        .map(|unchecked| (&unchecked.name, unchecked.reported))
        .collect();
    assert_eq!(unchecked, vec![(&Name::from("bad"), true)]);
}

/// A module with a canonicalization error in one declaration publishes its interface to
/// the modules that import it: `B` imports `A`'s sound `ok`, and the only error of the
/// check is `A`'s own, about `bad`. `A`'s tree holds a typed `ok`, and lists the broken
/// `bad` as unchecked with an error behind it.
///
/// Mutation-checked by making `check_module_recovering` answer `Outcome::Failed` when
/// canonicalization reported anything: `A` publishes nothing, `B` reports it as missing,
/// and the error count goes red.
#[test]
fn a_module_with_a_canonicalization_error_publishes_its_interface() {
    let root = fixture_package("package_import_canonical_error");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert!(
        matches!(
            check.errors.as_slice(),
            [error] if matches!(
                unwrap_in_file(error),
                CompilationError::Canonical(_, module) if module == &Name::from("A")
            )
        ),
        "expected `A`'s canonicalization error alone, got {:?}",
        check.errors
    );
    assert!(
        check
            .errors
            .iter()
            .all(|error| unwrap_in_file(error).module() != Some(&Name::from("B"))),
        "got {:?}",
        check.errors
    );

    assert_eq!(sorted_module_names(&check.failing), vec!["A", "B"]);
    let a = &check
        .failing
        .iter()
        .find(|checked| checked.module.canonical.name.name() == &Name::from("A"))
        .expect("`A` is among the failing modules")
        .module
        .ir;

    let declarations: Vec<&Name> = a
        .declarations
        .iter()
        .map(|declaration| &declaration.name)
        .collect();
    assert_eq!(declarations, vec![&Name::from("ok")]);

    let unchecked: Vec<(&Name, bool)> = a
        .unchecked
        .iter()
        .map(|unchecked| (&unchecked.name, unchecked.reported))
        .collect();
    assert_eq!(unchecked, vec![(&Name::from("bad"), true)]);
}

/// A caller of a broken declaration is checked against the declaration's annotation:
/// `f`'s body uses an operator nothing declares, and `g = f 1` still has a typed tree,
/// of type `Int`. `f` itself is unchecked, with its canonicalization error behind it.
///
/// Mutation-checked twice, each going red. Leaving `module.broken` out of the typer's
/// `global` in `type_check_recovering` makes `f` a name the typer does not hold, and `g`
/// becomes an unchecked declaration. Leaving `module.broken` out of what `ir::build`
/// appends to `unchecked` empties the list.
#[test]
fn a_caller_of_a_broken_declaration_is_checked_against_its_annotation() {
    let source = indoc::indoc! {r#"
        module Test exposing (f, g)

        f : Int -> Int
        f x = x <+> 1

        g : Int
        g = f 1
    "#};
    let parsed = parse_source(source);
    let interfaces = HashMap::from([basics_interface()]);

    let (module, errors) = match check_module_recovering(&test_package(), &interfaces, &parsed) {
        dependencies::Outcome::Module(module, errors) => (module, errors),
        dependencies::Outcome::Failed(error) => {
            panic!("expected a module beside its errors, got {:?}", error)
        }
    };

    assert!(
        matches!(errors.as_slice(), [CompilationError::Canonical(..)]),
        "got {:?}",
        errors
    );

    let g = module
        .ir
        .declarations
        .iter()
        .find(|declaration| declaration.name == Name::from("g"))
        .unwrap_or_else(|| {
            panic!(
                "`g` should have a typed tree, got unchecked {:?}",
                module.ir.unchecked
            )
        });
    assert_eq!(g.tpe.to_string(), "Int");

    let unchecked: Vec<(&Name, bool)> = module
        .ir
        .unchecked
        .iter()
        .map(|unchecked| (&unchecked.name, unchecked.reported))
        .collect();
    assert_eq!(unchecked, vec![(&Name::from("f"), true)]);
}

/// A module with an import that does not resolve still has a canonical form and
/// publishes its interface, so a module importing it reports nothing about it: the one
/// error of the build is `A`'s own unresolved import.
///
/// `B` has no error of its own, but it was checked against an incomplete interface and
/// so is incomplete in turn (`canonical::Module::incomplete`): `check_root` does not list
/// it as checked, and the `checked modules` status line counts both modules as failed.
///
/// Mutation-checked by not setting the flag when an import fails, in `new_environment`:
/// `A`'s interface is then complete, `B` checks whole, and the status line goes red.
#[test]
fn a_module_with_an_unresolved_import_publishes_its_interface() {
    let root = fixture_package("package_import_unresolved_import");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert_eq!(check.errors.len(), 1, "got {:?}", check.errors);
    let a = unwrap_in_file(&check.errors[0]);
    match a {
        CompilationError::Canonical(errors, module) => {
            assert_eq!(module, &Name::from("A"));
            assert!(
                matches!(errors.as_slice(), [canonical::Error::EnvironmentErrors(..)]),
                "expected `A`'s unresolved import alone, got {:?}",
                errors
            );
        }
        other => panic!("expected `A` to fail canonicalization, got {:?}", other),
    }
    assert!(
        check
            .errors
            .iter()
            .all(|error| unwrap_in_file(error).module() != Some(&Name::from("B"))),
        "`B` has nothing to report, got {:?}",
        check.errors
    );

    assert_eq!(sorted_module_names(&check.failing), vec!["A", "B"]);
    assert_eq!(
        checked_modules_status(&check),
        "checked modules: [] (2 failed to check)"
    );
}

/// The `checked modules` line of a check's status.
fn checked_modules_status(check: &zelkova_compiler::PackageCheck) -> &str {
    check
        .status
        .iter()
        .find(|status| status.text.starts_with("checked modules"))
        .map(|status| status.text.as_str())
        .expect("a check reports its modules")
}

/// A `type` declaration that is rejected is one error, and the module importing it
/// reports nothing about the type: not its `T(..)` import entry, not `A.T` in an
/// annotation, not the constructor `MkT`. `B`'s `b` has no typed tree, with the error
/// behind it being `A`'s, and `B` is not among the modules that checked.
///
/// Mutation-checked twice. Leaving `Interface::incomplete` false in
/// `canonical::Module::to_interface` reports `B`'s import entry as `UnionNotFound`, so
/// the error count goes red. Dropping the `incomplete` condition from `check_root`'s
/// partition puts `B`, whose error list is empty, in the status line's list. (The
/// partition has no `broken` condition: a broken declaration implies an error or an
/// incomplete scope.)
#[test]
fn a_module_importing_a_broken_type_reports_nothing_about_it() {
    let root = fixture_package("package_import_broken_type");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    match check.errors.as_slice() {
        [error] => match unwrap_in_file(error) {
            CompilationError::Canonical(errors, module) => {
                assert_eq!(module, &Name::from("A"));
                assert!(
                    matches!(errors.as_slice(), [canonical::Error::InvalidVariant(..)]),
                    "expected `A`'s invalid variant alone, got {:?}",
                    errors
                );
            }
            other => panic!("expected a Canonical error for `A`, got {:?}", other),
        },
        other => panic!("expected one error, got {:?}", other),
    }

    assert_eq!(sorted_module_names(&check.failing), vec!["A", "B"]);
    let b = &check
        .failing
        .iter()
        .find(|checked| checked.module.canonical.name.name() == &Name::from("B"))
        .expect("`B` is among the failing modules")
        .module
        .ir;
    let unchecked: Vec<(&Name, bool)> = b
        .unchecked
        .iter()
        .map(|unchecked| (&unchecked.name, unchecked.reported))
        .collect();
    assert_eq!(unchecked, vec![(&Name::from("b"), true)]);

    assert_eq!(
        checked_modules_status(&check),
        "checked modules: [] (2 failed to check)"
    );
}

/// The `tests/` half of `check_root`'s partition: with `src/` clean and one module of
/// `tests/` holding a type error, `test_modules` holds only the module that checked,
/// `failing` holds only the one that did not, and `errors` is that module's one `Type`.
/// `modules` still holds `src/`'s, since the failure is in `tests/`.
///
/// Mutation-checked twice, each going red. Changing `root_check.failing.push(module)` in
/// `check_root` to `root_check.checked.push(module)` puts `Bad` in `test_modules` and
/// leaves `failing` empty. Deleting the `failing.extend(..)` in `compile_tests` leaves
/// `failing` empty while `test_modules` and `errors` stay right.
#[test]
fn a_test_module_with_a_type_error_is_failing_and_not_a_test_module() {
    let root = fixture_package("package_tests_one_failing");

    let check = check_package_with_tests(&root, &Overlay::new())
        .expect("the manifest and the build resolve");

    assert_eq!(sorted_module_names(&check.modules), vec!["App"]);
    assert_eq!(sorted_module_names(&check.test_modules), vec!["Good"]);
    assert_eq!(sorted_module_names(&check.failing), vec!["Bad"]);
    assert!(check.failing[0].root_dir.ends_with("tests"));
    assert_eq!(check.errors.len(), 1, "got {:?}", check.errors);
    assert!(
        matches!(
            unwrap_in_file(&check.errors[0]),
            CompilationError::Type(_, module) if module == &Name::from("Bad")
        ),
        "expected `Bad`'s type error, got {:?}",
        check.errors[0]
    );
}

/// A build holding a module with a type error writes nothing, though that module has a
/// typed tree: the driver reads only the modules that checked, and only once nothing
/// failed.
///
/// Mutation-checked by replacing the driver's two `if errors.is_empty()` guards around
/// emitting and writing in `compile` with `if true`: the runtime is written and the
/// build directory assertion goes red.
#[test]
fn a_build_with_a_module_that_has_a_typed_tree_and_an_error_writes_nothing() {
    let build_dir =
        fresh_build_dir("a_build_with_a_module_that_has_a_typed_tree_and_an_error_writes_nothing");

    let error =
        zelkova::compile_package_into(&fixture_package("package_import_type_error"), &build_dir)
            .expect_err("`A` does not type check");

    let errors = many(&error);
    assert!(
        matches!(
            errors.as_slice(),
            [CompilationError::InFile(inner, _)] if matches!(**inner, CompilationError::Type(..))
        ),
        "got {:?}",
        errors
    );
    assert!(!build_dir.exists());
}

// ── TOOL-11: a module with a syntax error stays in the build ────────────────

/// A module with a syntax error in one declaration stays in the build: `B` imports `A`'s
/// sound `ok` and reports nothing, and the only error of the check is the syntax error.
/// `A`'s tree holds a typed `ok` and lists `bad`, whose binding did not parse, as
/// unchecked with an error behind it; `B` checks whole.
///
/// Mutation-checked by restoring `failures.is_empty()` as the condition `parse_root`
/// keeps a module on: `A` is dropped, `B` reports it as a module that does not exist,
/// and the error assertion goes red. Dropping the test on a module's parse failures from
/// `check_root` reds the status-line assertions: `A` is then counted as checked.
#[test]
fn a_module_with_a_syntax_error_stays_in_the_build() {
    let root = fixture_package("package_import_syntax_error");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert!(
        matches!(check.errors.as_slice(), [CompilationError::Source(..)]),
        "expected `A`'s syntax error alone, got {:?}",
        check.errors
    );
    assert!(check.modules.is_empty());
    assert_eq!(sorted_module_names(&check.failing), vec!["A", "B"]);

    let ir_of = |name: &str| {
        &check
            .failing
            .iter()
            .find(|checked| checked.module.canonical.name.name() == &Name::from(name))
            .unwrap_or_else(|| panic!("`{}` is among the failing modules", name))
            .module
            .ir
    };

    let a = ir_of("A");
    let ok = a
        .declarations
        .iter()
        .find(|declaration| declaration.name == Name::from("ok"))
        .unwrap_or_else(|| panic!("`ok` should have a declaration, got {:?}", a.declarations));
    assert_eq!(ok.tpe.to_string(), "T");
    let unchecked: Vec<(&Name, bool)> = a
        .unchecked
        .iter()
        .map(|unchecked| (&unchecked.name, unchecked.reported))
        .collect();
    assert_eq!(unchecked, vec![(&Name::from("bad"), true)]);

    let b = ir_of("B");
    assert!(b.unchecked.is_empty(), "got {:?}", b.unchecked);

    // `A` came back with no error of its own, since the syntax error is the parser's, and
    // is still not among the modules that checked.
    let checked = check
        .status
        .iter()
        .find(|status| status.text.starts_with("checked modules"))
        .unwrap_or_else(|| {
            panic!(
                "expected a status line for the check, got {:?}",
                check.status
            )
        });
    assert!(!checked.success, "got {:?}", checked);
    assert!(
        !checked.text.contains("\"package-import-syntax-error:A\""),
        "got {:?}",
        checked
    );
    assert!(
        checked.text.ends_with("(1 failed to check)"),
        "got {:?}",
        checked
    );
}

/// A module whose header did not parse is still a module its importers can find: the
/// header's syntax error is the only error, and nothing is reported about `B`, which
/// imports a value and names a type of it.
///
/// Mutation-checked by not inserting the stand-in interface in `compile_in_build`: `B`
/// reports `A` as a module it cannot find, and the error assertion goes red.
#[test]
fn a_module_whose_header_did_not_parse_still_exists_for_its_importers() {
    let root = fixture_package("package_import_header_error");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert!(
        matches!(check.errors.as_slice(), [CompilationError::Source(..)]),
        "expected `A`'s syntax error alone, got {:?}",
        check.errors
    );
    assert!(
        check
            .errors
            .iter()
            .all(|error| unwrap_in_file(error).module() != Some(&Name::from("B"))),
        "got {:?}",
        check.errors
    );
    assert!(check.modules.is_empty());
}

/// A headless file named like a module a dependency already answers to does not replace
/// that module for the package's other modules: `B` still sees the dependency's `Basics`,
/// so both of its own errors are reported beside the header's syntax error, and `Int`
/// resolves.
///
/// `package_headless_dependency_name` depends on a stand-in `zelkova-core` and holds a
/// `src/Basics.zel` whose header does not parse, beside a `B` with two ill-typed values.
///
/// Mutation-checked by dropping `|| interfaces.contains_key(name)` from the stand-in
/// loop in `compile_in_build`: the stand-in replaces the dependency's `Basics`, `B`'s
/// errors are lost and the count goes red.
#[test]
fn a_headless_file_named_like_a_dependency_module_does_not_replace_it() {
    let root = fixture_package("package_headless_dependency_name");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    let in_b: Vec<&CompilationError> = check
        .errors
        .iter()
        .filter(|error| unwrap_in_file(error).module() == Some(&Name::from("B")))
        .collect();
    assert_eq!(
        in_b.len(),
        2,
        "expected `B`'s two own errors, got {:?}",
        check.errors
    );
    assert!(
        check
            .errors
            .iter()
            .any(|error| matches!(error, CompilationError::Source(..))),
        "expected the header's syntax error, got {:?}",
        check.errors
    );
}

/// A build holding a module with a syntax error writes nothing, though the module is
/// kept and checked beside the error.
///
/// Mutation-checked by replacing the driver's two `if errors.is_empty()` guards around
/// emitting and writing in `compile` with `if true`: the build directory is created and
/// its assertion goes red.
#[test]
fn a_build_with_a_module_that_has_a_syntax_error_writes_nothing() {
    let build_dir = fresh_build_dir("a_build_with_a_module_that_has_a_syntax_error_writes_nothing");

    let error =
        zelkova::compile_package_into(&fixture_package("package_import_syntax_error"), &build_dir)
            .expect_err("`A` does not parse whole");

    let errors = many(&error);
    assert!(
        matches!(errors.as_slice(), [CompilationError::Source(..)]),
        "got {:?}",
        errors
    );
    assert!(!build_dir.exists());
}

// ── TOOL-12: an unresolved name inside a sound body is a typed hole ─────────

/// The module `check_module_recovering` builds from `source`, checked against `Basics`,
/// beside the errors it found.
fn recovering(source: &str) -> (CheckedModule, Vec<CompilationError>) {
    let interfaces = HashMap::from([basics_interface()]);
    match check_module_recovering(&test_package(), &interfaces, &parse_source(source)) {
        dependencies::Outcome::Module(module, errors) => (module, errors),
        dependencies::Outcome::Failed(error) => {
            panic!("expected a module beside its errors, got {:?}", error)
        }
    }
}

/// The declaration `name` of `module`'s IR, or a panic listing the unchecked ones.
fn declaration<'m>(module: &'m CheckedModule, name: &str) -> &'m zelkova_compiler::ir::Declaration {
    module
        .ir
        .declarations
        .iter()
        .find(|declaration| declaration.name == Name::from(name))
        .unwrap_or_else(|| {
            panic!(
                "`{}` should have a typed tree, got unchecked {:?}",
                name, module.ir.unchecked
            )
        })
}

/// A declaration holding a hole has a typed tree, and the hole's type is the one its
/// position expects: `f` takes an `Int`, so the hole `nope` is an `Int`.
///
/// Mutation-checked by making `annotate` answer a hole with `ErrorKind::UnboundVariable`:
/// `g` is then rejected, an unchecked declaration, and the lookup of it panics.
#[test]
fn a_hole_is_typed_as_its_position_expects() {
    let source = indoc::indoc! {r#"
        module Test exposing (g)

        f : Int -> Int
        f x = x

        g : Int
        g = f nope
    "#};

    let (module, errors) = recovering(source);

    match errors.as_slice() {
        [CompilationError::Canonical(canonical_errors, _)] => assert!(
            matches!(
                canonical_errors.as_slice(),
                [canonical::Error::VariableNotFound(..)]
            ),
            "got {:?}",
            canonical_errors
        ),
        other => panic!("expected the hole's error alone, got {:?}", other),
    }

    let g = declaration(&module, "g");
    let body = &g.body.as_ref().expect("`g` has a body").expression;
    match &body.kind {
        zelkova_compiler::ir::TypedTermKind::Apply { arg, .. } => {
            assert!(
                matches!(arg.kind, zelkova_compiler::ir::TypedTermKind::Hole),
                "got {:?}",
                arg
            );
            assert_eq!(arg.tpe.to_string(), "Int");
        }
        other => panic!("expected `g` to apply `f`, got {:?}", other),
    }
}

/// A hole leaves the rest of its body checked: `g` reports both that `nope` does not
/// resolve and that `'c'` is not the `Int` `f` takes, for the one declaration.
///
/// Mutation-checked by returning the `VariableNotFound` from `Expression::from_parser`'s
/// `Variable` arm instead of pushing it: `g` is then broken, the typer never reads it, and
/// the type error goes missing.
#[test]
fn a_hole_does_not_hide_a_type_error_beside_it() {
    let source = indoc::indoc! {r#"
        module Test exposing (g)

        f : Int -> Int -> Int
        f a b = a

        g : Int
        g = f nope 'c'
    "#};

    let (_, errors) = recovering(source);

    assert!(
        matches!(
            errors.as_slice(),
            [
                CompilationError::Canonical(canonical_errors, _),
                CompilationError::Type(type_errors, _),
            ] if matches!(canonical_errors.as_slice(), [canonical::Error::VariableNotFound(..)])
                && type_errors.len() == 1
        ),
        "got {:?}",
        errors
    );
}

/// A declaration whose `case` matches on a constructor that does not resolve has a typed
/// tree: the pattern hole's argument is bound in the branch, at the type the branch body
/// needs.
///
/// Mutation-checked by refusing a pattern hole in `translate_pattern` (`return None`): `h`
/// is then untranslatable, an unchecked declaration, and the lookup of it panics. The
/// argument's type is checked by answering a pattern hole with the identity in
/// `apply_pattern` (`TermPatternKind::Hole { args }` for itself): `y` then keeps the
/// unsolved variable it was annotated with and the `Int` assertion goes red. The
/// `pattern_constraints` arm is pinned by
/// `a_pattern_hole_with_a_unit_argument_is_held_to_unit`.
#[test]
fn a_pattern_hole_leaves_its_declaration_a_typed_tree() {
    let source = indoc::indoc! {r#"
        module Test exposing (h)

        h : Int -> Int
        h x =
          case x of
            Nope y -> y
    "#};

    let (module, errors) = recovering(source);

    assert!(
        matches!(errors.as_slice(), [CompilationError::Canonical(..)]),
        "got {:?}",
        errors
    );
    let h = declaration(&module, "h");
    assert_eq!(h.tpe.to_string(), "Int -> Int");

    // The hole's argument is the type the branch body solved it to, not the variable it
    // was annotated with: `y` is returned from an `Int -> Int`.
    let body = h.body.as_ref().expect("`h` has a body");
    let TypedTermKind::Case { branches, .. } = &body.expression.kind else {
        panic!("expected a `case`, got {:?}", body.expression.kind);
    };
    let [(pattern, _)] = branches.as_slice() else {
        panic!("expected one branch, got {} of them", branches.len());
    };
    let TermPatternKind::Hole { args } = &pattern.kind else {
        panic!("expected a pattern hole, got {:?}", pattern.kind);
    };
    let [argument] = args.as_slice() else {
        panic!("expected one argument, got {:?}", args);
    };
    assert_eq!(argument.tpe.to_string(), "Int");
}

/// A pattern hole's `()` argument is held to `()`: the argument is constrained as a
/// resolved constructor's is, and so solves to the unit type.
///
/// Mutation-checked by changing the `Hole` arm of `pattern_constraints` to
/// `(None, vec![])`: the argument is then constrained by nothing and keeps its unsolved
/// variable, and the assertion goes red.
#[test]
fn a_pattern_hole_with_a_unit_argument_is_held_to_unit() {
    let source = indoc::indoc! {r#"
        module Test exposing (h)

        h : Int -> Int
        h x =
          case x of
            Nope () -> 1
    "#};

    let (module, errors) = recovering(source);

    assert!(
        matches!(errors.as_slice(), [CompilationError::Canonical(..)]),
        "got {:?}",
        errors
    );
    let body = declaration(&module, "h")
        .body
        .as_ref()
        .expect("`h` has a body");
    let TypedTermKind::Case { branches, .. } = &body.expression.kind else {
        panic!("expected a `case`, got {:?}", body.expression.kind);
    };
    let TermPatternKind::Hole { args } = &branches[0].0.kind else {
        panic!("expected a pattern hole, got {:?}", branches[0].0.kind);
    };
    assert_eq!(args[0].tpe.to_string(), "()");
}

/// A declaration that holds a hole but that the typer cannot type is `reported`: the
/// hole's error stands behind it, though the typer walked past it rather than rejecting
/// it. The first is `Solved::Untranslatable` (the float literal argument of the
/// unresolved constructor is refused), the second `Solved::UnboundName` (`u` has no annotation, so
/// the typer's environment does not hold it). A declaration the typer walks past with no
/// hole in it stays unreported.
///
/// Mutation-checked by setting `reported` to `false` for those two arms in `ir::build`:
/// both assertions on `reported: true` go red.
#[test]
fn a_hole_the_typer_cannot_type_is_a_reported_unchecked_declaration() {
    let source = indoc::indoc! {r#"
        module Test exposing (h, g)

        h : Int -> Int
        h x =
          case x of
            Nope 1.5 -> 1
            _ -> 2

        u x = x

        g : Int
        g = u nope
    "#};

    let (module, errors) = recovering(source);

    assert!(
        errors
            .iter()
            .all(|error| matches!(error, CompilationError::Canonical(..))),
        "got {:?}",
        errors
    );
    let reported = |name: &str| {
        module
            .ir
            .unchecked
            .iter()
            .find(|unchecked| unchecked.name == Name::from(name))
            .unwrap_or_else(|| {
                panic!(
                    "`{}` should be unchecked, got {:?}",
                    name, module.ir.unchecked
                )
            })
            .reported
    };
    assert!(
        reported("h"),
        "an untranslatable declaration holding a hole"
    );
    assert!(reported("g"), "an unbound-name declaration holding a hole");
}

/// A module whose declaration holds a hole is not among the modules that checked, though
/// nothing of it is broken: the hole's error stands in its error list, the build's one
/// error, and the build writes nothing.
///
/// Mutation-checked by not pushing the error in `Expression::from_parser`'s `Variable`
/// arm, leaving the hole: `A` then comes back with no error and checks, and the error
/// assertion goes red.
#[test]
fn a_module_holding_a_hole_does_not_check_and_writes_nothing() {
    let root = fixture_package("package_unresolved_name_hole");

    let check = check_package(&root, &Overlay::new()).expect("the manifest and the build resolve");

    assert!(
        matches!(
            check.errors.as_slice(),
            [error] if matches!(
                unwrap_in_file(error),
                CompilationError::Canonical(errors, module)
                    if module == &Name::from("A")
                        && matches!(errors.as_slice(), [canonical::Error::VariableNotFound(..)])
            )
        ),
        "expected `A`'s unresolved name alone, got {:?}",
        check.errors
    );
    assert!(check.modules.is_empty());
    assert_eq!(sorted_module_names(&check.failing), vec!["A"]);

    let a = &check.failing[0].module;
    assert!(
        a.canonical.broken.is_empty(),
        "got {:?}",
        a.canonical.broken
    );
    assert!(a.ir.unchecked.is_empty(), "got {:?}", a.ir.unchecked);

    let build_dir = fresh_build_dir("a_module_holding_a_hole_does_not_check_and_writes_nothing");
    zelkova::compile_package_into(&root, &build_dir).expect_err("`g` holds a hole");
    assert!(!build_dir.exists());
}

/// `LANG-48`: a label given twice in a record renders as a canonicalization error in
/// prose, its primary caret under the repeated label and a secondary one under the first,
/// both in the module's own file.
///
/// The fixture is `package_private_module_parse_failure` with its one module replaced
/// through the overlay, since `check_package` is what pairs an error with its file and
/// a label with no file is not rendered.
///
/// Mutation-checked by giving `Error::RepeatedLabel`'s primary label the first field's
/// span instead of the repeat's: the primary range assertion goes red.
#[test]
fn a_repeated_label_renders_under_the_repeat() {
    let source = "module Broken exposing ()\n\nr =\n  { taken = 1, taken = 2 }\n";
    let root = fixture_package("package_private_module_parse_failure");
    let mut overlay = Overlay::new();
    overlay.insert(root.join("src").join("Broken.zel"), source.into());

    let check = check_package(&root, &overlay).expect("the manifest and the build resolve");
    let [error] = check.errors.as_slice() else {
        panic!("expected one error, got {:?}", check.errors);
    };
    assert!(
        matches!(unwrap_in_file(error), CompilationError::Canonical(..)),
        "expected a canonicalization error, got {:?}",
        error
    );

    let diagnostic = error.as_diagnostic();
    assert_eq!(diagnostic.severity, Severity::Error);
    assert!(
        diagnostic
            .message
            .contains("`taken` labels two fields of one record"),
        "got {:?}",
        diagnostic.message
    );

    let first = source.find("taken").expect("the source gives `taken`");
    let repeat = source
        .rfind("taken")
        .expect("the source gives `taken` again");
    let ranges = |style: LabelStyle| -> Vec<std::ops::Range<usize>> {
        diagnostic
            .labels
            .iter()
            .filter(|label| label.style == style)
            .map(|label| label.range.clone())
            .collect()
    };
    assert_eq!(
        ranges(LabelStyle::Primary),
        vec![repeat..repeat + "taken".len()]
    );
    assert_eq!(
        ranges(LabelStyle::Secondary),
        vec![first..first + "taken".len()]
    );
}

/// `LANG-51`: the three errors a use of a record raises render in prose, each with its
/// one primary caret under the text the rule is about, in the module's own file — the
/// whole accessor or access whose record type nothing supplied, and the label an update
/// would add — and with the notes that say what to do about it.
///
/// The fixture is `package_private_module_parse_failure` with its one module replaced
/// through the overlay, as in [`a_repeated_label_renders_under_the_repeat`]. It has no
/// `std/core` in reach, so the update's record holds a type the module declares.
///
/// Mutation-checked by having `ErrorKind::record_use_label` answer `None` for all three:
/// the diagnostic then falls back to the declaration's span and the range assertion goes
/// red for each.
#[test]
fn a_records_use_renders_under_the_form() {
    let cases = [
        (
            "module Broken exposing ()\n\npick =\n  .name\n",
            "cannot type the accessor `.name`: nothing in this declaration says which record type it reads",
            ".name",
            vec![
                "in the declaration of `pick`",
                "a record's type is never worked out from the fields a declaration uses",
                "a type annotation on `pick` would supply it",
            ],
        ),
        (
            "module Broken exposing ()\n\nnameOf person =\n  person.name\n",
            "cannot read the field `name`: nothing in this declaration says which record type it is read from",
            "person.name",
            vec![
                "in the declaration of `nameOf`",
                "a record's type is never worked out from the fields a declaration uses",
                "a type annotation on `nameOf` would supply it",
            ],
        ),
        (
            "module Broken exposing ()\n\ntype Count\n  = Count\n\nadded : { taken : Count } -> { taken : Count }\nadded r =\n  { r | expected = Count }\n",
            "the record type `{ taken : Count }` has no field `expected`",
            "expected",
            vec![
                "in the declaration of `added`",
                "an update cannot add a field: each label it names must already be a field of the record it updates",
            ],
        ),
    ];

    for (source, message, caret, notes) in cases {
        let root = fixture_package("package_private_module_parse_failure");
        let mut overlay = Overlay::new();
        overlay.insert(root.join("src").join("Broken.zel"), source.into());

        let check = check_package(&root, &overlay).expect("the manifest and the build resolve");
        let [error] = check.errors.as_slice() else {
            panic!("expected one error, got {:?}", check.errors);
        };
        assert!(
            matches!(unwrap_in_file(error), CompilationError::Type(..)),
            "expected a type error, got {:?}",
            error
        );

        let diagnostic = error.as_diagnostic();
        assert_eq!(diagnostic.severity, Severity::Error);
        assert_eq!(diagnostic.message, format!("[Broken] {}", message));
        assert_eq!(diagnostic.notes, notes);

        let start = source
            .find(caret)
            .expect("the source holds the caret's text");
        let primary: Vec<_> = diagnostic
            .labels
            .iter()
            .filter(|label| label.style == LabelStyle::Primary)
            .map(|label| label.range.clone())
            .collect();
        assert_eq!(
            primary,
            vec![start..start + caret.len()],
            "for {:?}",
            source
        );
    }
}

/// `LANG-84`: the two errors a record pattern raises of its own render in prose, in the
/// module's own file — the label the matched record type lacks, under that label, and a
/// record pattern whose record type nothing supplied, under the whole pattern — each
/// with the notes that say what to do about it.
///
/// The fixture is the one [`a_records_use_renders_under_the_form`] overlays.
///
/// Mutation-checked by having `ErrorKind::record_use_label` answer `None` for all three
/// of a record use's errors: the diagnostic then falls back to the declaration's span and
/// the range assertion goes red for each. The missing label's note is mutation-checked
/// by deleting the `RecordUse::Pattern` arm of `MissingField` in `notes`: the notes
/// assertion goes red.
#[test]
fn a_record_pattern_renders_under_the_label_or_the_pattern() {
    let cases = [
        (
            "module Broken exposing ()\n\ntype Count\n  = Count\n\ntaken : { taken : Count } -> Count\ntaken { expected } =\n  expected\n",
            "the record type `{ taken : Count }` has no field `expected`",
            "expected }",
            "expected".len(),
            vec![
                "in the declaration of `taken`",
                "a record pattern names some of the fields of the record it matches, and each label it names must be one of them",
            ],
        ),
        (
            "module Broken exposing ()\n\nnameOf { name } =\n  name\n",
            "cannot type this record pattern: nothing in this declaration says which record type it matches",
            "{ name }",
            "{ name }".len(),
            vec![
                "in the declaration of `nameOf`",
                "a record's type is never worked out from the fields a declaration uses",
                "a type annotation on `nameOf` would supply it",
            ],
        ),
    ];

    for (source, message, caret, len, notes) in cases {
        let root = fixture_package("package_private_module_parse_failure");
        let mut overlay = Overlay::new();
        overlay.insert(root.join("src").join("Broken.zel"), source.into());

        let check = check_package(&root, &overlay).expect("the manifest and the build resolve");
        let [error] = check.errors.as_slice() else {
            panic!("expected one error, got {:?}", check.errors);
        };
        assert!(
            matches!(unwrap_in_file(error), CompilationError::Type(..)),
            "expected a type error, got {:?}",
            error
        );

        let diagnostic = error.as_diagnostic();
        assert_eq!(diagnostic.severity, Severity::Error);
        assert_eq!(diagnostic.message, format!("[Broken] {}", message));
        assert_eq!(diagnostic.notes, notes);

        let start = source
            .find(caret)
            .expect("the source holds the caret's text");
        let primary: Vec<_> = diagnostic
            .labels
            .iter()
            .filter(|label| label.style == LabelStyle::Primary)
            .map(|label| label.range.clone())
            .collect();
        assert_eq!(primary, vec![start..start + len], "for {:?}", source);
    }
}
