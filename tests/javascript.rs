//! Layer 5: the JavaScript text one checked module emits as.
//!
//! Each test compiles a small module through the whole pipeline — parse → canonicalize →
//! type_check → the IR — and asserts on the text [`javascript::emit`] produces for it.
//! Nothing here runs that text: whether it computes the right value is checked under
//! `node`, not by `cargo test`
//! ([`DEC-18` decision 6](../docs/decisions/dec-18.md#6--the-generated-code-is-checked-in-two-halves-and-cargo-test-does-not-run-node)).
//!
//! Fixtures are kept to the few lines each rule needs, since every assertion is on text
//! and a larger fixture is one more thing a later emitter changes.

use std::collections::HashMap;

use indoc::indoc;
use zelkova_lang::compiler::ir::{Body, Reference, ReferenceKind, TypedTerm, TypedTermKind};
use zelkova_lang::compiler::javascript::{self, Construct, Error};
use zelkova_lang::compiler::name::{Name, QualName};
use zelkova_lang::compiler::position::NodeSpan;
use zelkova_lang::compiler::{check_module, CheckedModule, Interface};

mod support;

use support::*;

/// `source` checked against `interfaces`, insisting that it checks.
fn checked_against(source: &str, interfaces: HashMap<Name, Interface>) -> CheckedModule {
    check_module(&test_package(), &interfaces, &parse_source(source), false)
        .unwrap_or_else(|error| panic!("expected the module to check, got {:?}", error))
}

fn checked(source: &str) -> CheckedModule {
    checked_against(
        source,
        HashMap::from([basics_interface(), char_interface(), maybe_interface()]),
    )
}

fn emit(module: &CheckedModule) -> String {
    javascript::emit(module)
        .unwrap_or_else(|errors| panic!("expected the module to emit, got {:?}", errors))
}

/// The text `source` emits as, insisting that it emits.
fn emitted(source: &str) -> String {
    emit(&checked(source))
}

/// The errors `source` fails to emit with, insisting that it fails.
fn refused(source: &str) -> Vec<Error> {
    match javascript::emit(&checked(source)) {
        Ok(text) => panic!("expected the module to be refused, got:\n{}", text),
        Err(errors) => errors,
    }
}

/// The byte offset of `needle` in `text`, or a panic showing the text.
fn position(text: &str, needle: &str) -> usize {
    text.find(needle)
        .unwrap_or_else(|| panic!("expected `{}` in:\n{}", needle, text))
}

/// The whole of a small module, which pins the order of its sections: imports, hoisted
/// constructors, functions, parameterless bindings, exports.
///
/// Mutation-checked by putting the parameterless bindings before the functions in
/// `emit`'s section list: the text no longer matches.
#[test]
fn a_module_is_imports_constructors_functions_bindings_and_exports() {
    let text = emitted(indoc! {r#"
        module Test exposing (Colour(..), pick, half)

        type Colour
          = Red
          | Green

        pick : Int -> Int -> Int
        pick a b =
          a

        half : Int -> Int
        half =
          pick 1

        colour : Colour
        colour =
          Green
    "#});

    assert_eq!(
        text,
        indoc! {r#"
            import { $curry } from "../zelkova.mjs";

            const $Test$Red = {$: "Red"};
            const $Test$Green = {$: "Green"};

            function pick(a, b) {
              return a;
            }

            const colour = $Test$Green;
            const half = $curry(pick, 2)(1n);

            export { half, pick };
        "#}
    );
}

/// A declaration of two parameters is a JavaScript function of two parameters, not a
/// function returning a function.
///
/// Mutation-checked by emitting only the first of `body.parameters`: the declaration then
/// reads `function pick(a)`.
#[test]
fn a_two_parameter_declaration_is_a_two_parameter_function() {
    let text = emitted(indoc! {r#"
        module Test exposing (pick)

        pick : Int -> Int -> Int
        pick a b =
          b
    "#});

    assert!(
        text.contains("function pick(a, b) {\n  return b;\n}"),
        "got:\n{}",
        text
    );
}

/// A call supplying both of a two-parameter function's arguments is a direct call, and
/// one supplying a single argument goes through the runtime's `$curry`, which the module
/// then imports.
///
/// Mutation-checked twice: ignoring the IR's saturation (treating every application as
/// a call of a value) turns the direct-call assertion red, and emitting a top-level
/// reference used as a value without `$curry` turns the partial one red.
#[test]
fn a_saturated_call_is_direct_and_a_partial_one_goes_through_the_runtime() {
    let text = emitted(indoc! {r#"
        module Test exposing (both, one)

        pick : Int -> Int -> Int
        pick a b =
          a

        both : Int
        both =
          pick 1 2

        one : Int -> Int
        one =
          pick 1
    "#});

    assert!(
        text.contains("const both = pick(1n, 2n);"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("const one = $curry(pick, 2)(1n);"),
        "got:\n{}",
        text
    );
    assert!(
        text.starts_with("import { $curry } from \"../zelkova.mjs\";\n"),
        "got:\n{}",
        text
    );
}

/// A module that calls nothing through the runtime does not import it.
///
/// Mutation-checked by inserting `$curry` into the helpers unconditionally: the text
/// then opens with the import.
#[test]
fn a_module_with_only_direct_calls_imports_nothing() {
    let text = emitted(indoc! {r#"
        module Test exposing (both)

        pick : Int -> Int -> Int
        pick a b =
          a

        both : Int
        both =
          pick 1 2
    "#});

    assert!(!text.contains("import"), "got:\n{}", text);
}

/// A function that is a value — a parameter here — is applied one argument per call, so
/// `f x y` applies `f x` before it evaluates `y`, the order the language evaluates it in.
///
/// Mutation-checked by passing every remaining argument to one call in `application`:
/// the body then reads `f(x, y)`.
#[test]
fn a_function_value_is_applied_one_argument_at_a_time() {
    let text = emitted(indoc! {r#"
        module Test exposing (apply)

        apply : (Int -> Int -> Int) -> Int -> Int -> Int
        apply f x y =
          f x y
    "#});

    assert!(text.contains("return f(x)(y);"), "got:\n{}", text);
}

/// An `Int` literal is a `BigInt` and a `Float` literal is a number.
///
/// Mutation-checked by dropping the `n` suffix from the `Int` arm of `expression`, and
/// separately by giving the `Float` arm one: each turns its assertion red.
#[test]
fn an_int_literal_ends_in_n_and_a_float_literal_does_not() {
    let text = emitted(indoc! {r#"
        module Test exposing (int, float)

        int : Int
        int =
          7

        float : Float
        float =
          2.5
    "#});

    assert!(text.contains("const int = 7n;"), "got:\n{}", text);
    assert!(text.contains("const float = 2.5;"), "got:\n{}", text);
}

/// `True` and `False` are JavaScript's `true` and `false`, and `Bool` hoists no constant.
///
/// A module can only mention them where their union is translatable, which today means in
/// the module declaring `Bool` itself (`BUG-36`), so the fixture is a `Basics` of its own.
///
/// Mutation-checked by removing the `scalars::BOOL` arm from `value`: the bindings then
/// read `$Basics$True` and `$Basics$False`. Removing the `Bool` filter from
/// `hoisted_constructors` separately turns the no-constant assertion red.
#[test]
fn true_is_javascripts_true() {
    let module = checked_against(
        indoc! {r#"
            module Basics exposing (Bool(..), yes, no)

            type Bool = True | False

            yes : Bool
            yes =
              True

            no : Bool
            no =
              False
        "#},
        HashMap::from([char_interface()]),
    );
    let text = emit(&module);

    assert!(text.contains("const yes = true;"), "got:\n{}", text);
    assert!(text.contains("const no = false;"), "got:\n{}", text);
    assert!(!text.contains("{$:"), "got:\n{}", text);
}

/// A constructor of no arguments is one module-level constant, and every mention of it
/// refers to that constant rather than building another object.
///
/// Mutation-checked by emitting a nullary constructor's mention as its tagged object,
/// `{$: "Red"}`: the object then appears three times.
#[test]
fn a_nullary_constructor_is_one_constant_every_mention_refers_to() {
    let text = emitted(indoc! {r#"
        module Test exposing (first, second)

        type Colour
          = Red
          | Green

        first : Colour
        first =
          Red

        second : Colour
        second =
          Red
    "#});

    assert_eq!(
        text.matches("{$: \"Red\"}").count(),
        1,
        "one object for `Red`, got:\n{}",
        text
    );
    assert!(
        text.contains("const $Test$Red = {$: \"Red\"};"),
        "got:\n{}",
        text
    );
    assert!(text.contains("const first = $Test$Red;"), "got:\n{}", text);
    assert!(text.contains("const second = $Test$Red;"), "got:\n{}", text);
}

/// A constructor of no arguments that another module declares is hoisted by the module
/// that mentions it, since the declaring module exports no constant for it.
///
/// No module the front end checks reaches this today: a declaration mentioning an
/// imported constructor is left unchecked (`BUG-36`). So the IR is the one a local
/// constructor produces, rewritten to name a union `Lib` declares.
///
/// Mutation-checked by not recording the constructor in `value`: `$Lib$Red` is then
/// mentioned and never declared.
#[test]
fn an_imported_nullary_constructor_is_hoisted_by_the_importer() {
    let mut module = checked(indoc! {r#"
        module Test exposing (first)

        type Colour
          = Red
          | Green

        first : Colour
        first =
          Red
    "#});

    module.ir.unions.clear();
    let lib_colour = QualName::parse("Lib.Colour").unwrap();
    for declaration in &mut module.ir.declarations {
        if let Some(Body {
            expression:
                TypedTerm {
                    kind:
                        TypedTermKind::Identifier(Reference {
                            kind: ReferenceKind::Constructor(ctor),
                            ..
                        }),
                    ..
                },
            ..
        }) = &mut declaration.body
        {
            ctor.union = lib_colour.clone();
        }
    }

    let text = emit(&module);

    assert!(
        text.contains("const $Lib$Red = {$: \"Red\"};"),
        "got:\n{}",
        text
    );
    assert!(text.contains("const first = $Lib$Red;"), "got:\n{}", text);
    assert!(!text.contains("$Test$"), "got:\n{}", text);
}

/// A constructor with arguments is a tagged object with its arguments in `a`, `b`, `c`,
/// in the order they are written, and is not hoisted.
///
/// Mutation-checked by numbering the fields from the last argument: `a` then holds `3n`.
#[test]
fn a_constructor_with_arguments_is_a_tagged_object_in_declaration_order() {
    let text = emitted(indoc! {r#"
        module Test exposing (paint)

        type Colour
          = Rgb Int Int Int

        paint : Colour
        paint =
          Rgb 1 2 3
    "#});

    assert!(
        text.contains("const paint = {$: \"Rgb\", a: 1n, b: 2n, c: 3n};"),
        "got:\n{}",
        text
    );
    assert!(!text.contains("$Test$Rgb"), "got:\n{}", text);
}

/// A tuple is an array.
///
/// Mutation-checked by emitting a tuple's elements in braces: the assertion goes red.
#[test]
fn a_tuple_is_an_array() {
    let text = emitted(indoc! {r#"
        module Test exposing (pair)

        pair : (Int, Float)
        pair =
          (1, 2.5)
    "#});

    assert!(text.contains("const pair = [1n, 2.5];"), "got:\n{}", text);
}

/// An `if` is a conditional expression.
///
/// Mutation-checked by swapping the branches in `expression`'s `If` arm.
#[test]
fn an_if_is_a_conditional_expression() {
    let text = emitted(indoc! {r#"
        module Test exposing (choose)

        choose : Bool -> Int
        choose b =
          if b then 1 else 2
    "#});

    assert!(text.contains("return b ? 1n : 2n;"), "got:\n{}", text);
}

/// An `if` in the condition of another is parenthesised: without the parentheses,
/// JavaScript reads `x ? y : x ? 1n : 2n` as a conditional in the *else* branch, a
/// different program.
///
/// Mutation-checked by returning `operand`'s text unparenthesised.
#[test]
fn an_if_in_condition_position_is_parenthesised() {
    let text = emitted(indoc! {r#"
        module Test exposing (choose)

        choose : Bool -> Bool -> Int
        choose x y =
          if (if x then y else x) then 1 else 2
    "#});

    assert!(
        text.contains("return (x ? y : x) ? 1n : 2n;"),
        "got:\n{}",
        text
    );
}

/// An `if` applied to an argument is parenthesised: without the parentheses, the call
/// would bind to the *else* branch alone.
///
/// Mutation-checked by returning `operand`'s text unparenthesised.
#[test]
fn an_if_in_callee_position_is_parenthesised() {
    let text = emitted(indoc! {r#"
        module Test exposing (choose)

        next : Int -> Int
        next n =
          n

        same : Int -> Int
        same n =
          n

        choose : Bool -> Int
        choose x =
          (if x then next else same) 1
    "#});

    assert!(
        text.contains("return (x ? next : same)(1n);"),
        "got:\n{}",
        text
    );
}

/// A parameterless binding that mentions another is emitted after it, whatever their
/// names' order: `alpha` sorts first and is initialised second.
///
/// Mutation-checked by emitting the bindings in the IR's name order instead of its
/// initialisation order: `alpha` then comes first.
#[test]
fn a_parameterless_binding_is_emitted_after_the_one_it_mentions() {
    let text = emitted(indoc! {r#"
        module Test exposing (alpha)

        alpha : Int
        alpha =
          beta

        beta : Int
        beta =
          1
    "#});

    assert!(
        position(&text, "const beta = 1n;") < position(&text, "const alpha = beta;"),
        "`beta` has to be initialised before `alpha` reads it, got:\n{}",
        text
    );
}

/// A declaration named `class`, a reserved word in JavaScript, is renamed wherever it is
/// declared and mentioned, and exported under its own name; `classy` is left alone.
///
/// Mutation-checked by making `mangle` return every name unchanged: `function class(a)`
/// then appears, which is not JavaScript.
#[test]
fn a_reserved_word_is_mangled_and_a_name_containing_one_is_not() {
    let text = emitted(indoc! {r#"
        module Test exposing (class, classy)

        class : Int -> Int
        class a =
          a

        classy : Int -> Int
        classy a =
          class a
    "#});

    assert!(text.contains("function $class(a) {"), "got:\n{}", text);
    assert!(
        text.contains("function classy(a) {\n  return $class(a);\n}"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("export { $class as class, classy };"),
        "got:\n{}",
        text
    );
}

/// A `case` is refused rather than emitted without it, naming the declaration.
///
/// Mutation-checked by making the `Case` arm of `expression` emit an empty string
/// without recording an error: the module is then emitted.
#[test]
fn a_case_is_refused() {
    let errors = refused(indoc! {r#"
        module Test exposing (flip)

        type Colour
          = Red
          | Green

        flip : Colour -> Colour
        flip c =
          case c of
            Red ->
              Green

            Green ->
              Red
    "#});

    assert_eq!(errors.len(), 1, "got {:?}", errors);
    match &errors[0] {
        Error::Unsupported {
            construct,
            declaration,
            ..
        } => {
            assert_eq!(*construct, Construct::Case);
            assert_eq!(*declaration, Name::new("flip"));
        }
        other => panic!("expected the `case` to be refused, got {:?}", other),
    }
}

/// A module holding a declaration the typer could not check is refused rather than
/// emitted without it.
///
/// Mutation-checked by starting `emit`'s errors empty instead of from `ir.unchecked`:
/// the module is then emitted with `helper` missing.
#[test]
fn a_declaration_with_no_ir_is_refused() {
    let errors = refused(indoc! {r#"
        module Test exposing (answer)

        import Maybe

        answer : Int
        answer =
          1

        helper : Int
        helper =
          Maybe.withDefault
    "#});

    // `NodeSpan`'s equality ignores the span, so this compares the variant and the name.
    assert_eq!(
        errors,
        vec![Error::Unchecked {
            name: Name::new("helper"),
            span: NodeSpan::none(),
        }]
    );
}

/// A facade is refused: what one emits as is `GEN-12`'s.
///
/// Mutation-checked by deleting the `ir.foreign` check: the facade then emits as an
/// empty module.
#[test]
fn a_facade_is_refused() {
    let errors = refused(indoc! {r#"
        module foreign Test exposing (combine)

        unsafe combine : Int -> Int -> Int
    "#});

    assert_eq!(
        errors,
        vec![Error::Facade {
            module: Name::new("Test")
        }]
    );
}

/// An exposed operator exports the function it stands for, even when the header does
/// not name the function itself.
///
/// Mutation-checked by dropping the `ExportType::Infix` half of `exports`' filter: `add`
/// then goes unexported.
#[test]
fn an_exposed_operator_exports_its_function() {
    let text = emitted(indoc! {r#"
        module Test exposing ((+++))

        infix left 6 (+++) = add

        add : Int -> Int -> Int
        add a b =
          a
    "#});

    assert!(text.ends_with("export { add };\n"), "got:\n{}", text);
}
