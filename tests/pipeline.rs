//! Layer 3: End-to-end pipeline tests.
//!
//! These tests exercise `check_module` — the full canonicalize → type_check →
//! exhaustiveness pipeline — on known inputs. Currently `type_check` and
//! `exhaustiveness::check` are stubs returning `Ok(())`, so these tests
//! primarily validate that the canonicalization phase succeeds (or fails) as
//! expected end-to-end.

use std::collections::HashMap;
use std::path::Path;

use codespan_reporting::files::SimpleFile;
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::{check_module, parser, Interface, PackageName};

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
    let module_a = check_module(&pkg, &interfaces, &parsed_a)
        .expect("Lib should compile");
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
    interfaces.insert(basics_module.name.name().clone(), basics_module.to_interface());

    // Maybe depends on Basics
    let maybe_path = src.join("Maybe.zel");
    if !maybe_path.exists() {
        eprintln!("Skipping: Maybe.zel not found");
        return;
    }
    let parsed_maybe = parse_file(&maybe_path);
    let maybe_module = check_module(&pkg, &interfaces, &parsed_maybe)
        .unwrap_or_else(|e| panic!("Maybe.zel failed: {:?}", e));
    interfaces.insert(maybe_module.name.name().clone(), maybe_module.to_interface());

    // Result depends on Basics and Maybe
    let result_path = src.join("Result.zel");
    if !result_path.exists() {
        eprintln!("Skipping: Result.zel not found");
        return;
    }
    let parsed_result = parse_file(&result_path);
    let result_module = check_module(&pkg, &interfaces, &parsed_result)
        .unwrap_or_else(|e| panic!("Result.zel failed: {:?}", e));
    interfaces.insert(result_module.name.name().clone(), result_module.to_interface());

    // At this point we've successfully compiled the core stdlib chain.
    // Verify Basics, Maybe, and Result are all in the interface map.
    assert!(interfaces.contains_key(&Name("Basics".to_string())));
    assert!(interfaces.contains_key(&Name("Maybe".to_string())));
    assert!(interfaces.contains_key(&Name("Result".to_string())));
}
