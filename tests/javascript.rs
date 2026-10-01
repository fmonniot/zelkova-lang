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
use zelkova_lang::compiler::canonical::Value;
use zelkova_lang::compiler::javascript::{self, Error, Unions, Unpredicated};
use zelkova_lang::compiler::name::Name;
use zelkova_lang::compiler::{check_module, CheckedModule, Interface, PackageName, PhaseError};
use zelkova_syntax::position::NodeSpan;

mod support;

use support::*;

/// `source` checked against `interfaces`, insisting that it checks.
fn checked_against(source: &str, interfaces: HashMap<Name, Interface>) -> CheckedModule {
    check_module(&test_package(), &interfaces, &parse_source(source))
        .unwrap_or_else(|error| panic!("expected the module to check, got {:?}", error))
}

fn checked(source: &str) -> CheckedModule {
    checked_against(
        source,
        HashMap::from([basics_interface(), char_interface(), maybe_interface()]),
    )
}

/// The unions a single module's boundary checks can read: its own. A fixture checked
/// against the support interfaces has no other module of its build to read one from.
fn unions_of(module: &CheckedModule) -> Unions {
    Unions::of([module])
}

fn emit(module: &CheckedModule) -> String {
    javascript::emit(module, true, &unions_of(module))
        .unwrap_or_else(|errors| panic!("expected the module to emit, got {:?}", errors))
}

/// The text `source` emits as, insisting that it emits.
fn emitted(source: &str) -> String {
    emit(&checked(source))
}

/// The errors `source` fails to emit with, insisting that it fails.
fn refused(source: &str) -> Vec<Error> {
    let module = checked(source);
    match javascript::emit(&module, true, &unions_of(&module)) {
        Ok(text) => panic!("expected the module to be refused, got:\n{}", text),
        Err(errors) => errors,
    }
}

/// The errors `source` fails to emit with when no companion is available for the
/// target being built, insisting that it fails.
fn refused_without_companion(source: &str) -> Vec<Error> {
    let module = checked(source);
    match javascript::emit(&module, false, &unions_of(&module)) {
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

            const $test_project$Test$Red = {$: "Red"};
            const $test_project$Test$Green = {$: "Green"};

            function pick(a, b) {
              return a;
            }

            const colour = $test_project$Test$Green;
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

/// A string literal is a JavaScript string literal holding the same characters: each
/// escape the source wrote was read by the tokenizer, and is written back out as the
/// escape JavaScript needs.
///
/// Mutation-checked by emitting `TypedTermKind::String` as `format!("\"{}\"", s)`,
/// unescaped: the quote and the line feed then land in the text raw.
#[test]
fn a_string_literal_is_an_escaped_javascript_string() {
    let text = emitted(indoc! {r#"
        module Test exposing ()

        greeting =
          "say \"hi\"\n\\ \u{E9}"
    "#});

    assert!(
        text.contains(r#"const greeting = "say \"hi\"\n\\ é";"#),
        "got:\n{}",
        text
    );
}

/// A declaration matching on a string literal is refused, not emitted: the typer does
/// not translate a string pattern, so it has no IR.
#[test]
fn a_string_pattern_is_refused() {
    let errors = refused(indoc! {r#"
        module Test exposing ()

        isHello s =
          case s of
            "hello" ->
              1

            _ ->
              0
    "#});

    assert_eq!(
        errors,
        vec![Error::Unchecked {
            name: Name::new("isHello"),
            span: NodeSpan::none(),
        }]
    );
}

/// `True` and `False` are JavaScript's `true` and `false`, and `Bool` hoists no constant.
///
/// The fixture is a `Basics` of its own, declaring `Bool` itself, so no interface has to
/// be built for it. It is checked as a module of `zelkova-core`, since a `Bool` declared by
/// any other package is an ordinary union.
///
/// Mutation-checked by removing the `scalars::BOOL` arm from `value`: the bindings then
/// read `$zelkova_core$Basics$True` and `$zelkova_core$Basics$False`. Removing the `Bool` filter from
/// `hoisted_constructors` separately turns the no-constant assertion red.
#[test]
fn true_is_javascripts_true() {
    let module = check_module(
        &PackageName::core(),
        &HashMap::from([char_interface()]),
        &parse_source(indoc! {r#"
            module Basics exposing (Bool(..), yes, no)

            type Bool = True | False

            yes : Bool
            yes =
              True

            no : Bool
            no =
              False
        "#}),
    )
    .unwrap_or_else(|errors| panic!("the module should check: {:?}", errors));
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
        text.contains("const $test_project$Test$Red = {$: \"Red\"};"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("const first = $test_project$Test$Red;"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("const second = $test_project$Test$Red;"),
        "got:\n{}",
        text
    );
}

/// A constructor of no arguments that another module declares is hoisted by the module
/// that mentions it, since the declaring module exports no constant for it.
///
/// `Lib` is checked first and `Test` against its interface, the way a build orders
/// them, so what is emitted is what the front end makes of an imported constructor —
/// written both exposed and qualified — and not a hand-built stand-in for it.
///
/// Mutation-checked two ways: by not recording the constructor in `value`, which
/// leaves `$test_project$Lib$Red` mentioned and never declared; and by naming an exposed
/// `VarConstructor` by the importing module in `Expression::from_parser`, which makes
/// `first` a `Test.Red` nothing declares, so the module is refused as unchecked.
#[test]
fn an_imported_nullary_constructor_is_hoisted_by_the_importer() {
    let mut interfaces = HashMap::from([basics_interface(), char_interface(), maybe_interface()]);
    let lib = checked_against(
        indoc! {r#"
            module Lib exposing (Colour(..))

            type Colour
              = Red
              | Green
        "#},
        interfaces.clone(),
    );
    interfaces.insert(lib.canonical.name.name().clone(), lib.to_interface(None));

    let module = checked_against(
        indoc! {r#"
            module Test exposing (first, second)

            import Lib exposing (Colour(..))

            first : Colour
            first =
              Red

            second : Lib.Colour
            second =
              Lib.Red
        "#},
        interfaces,
    );

    let text = emit(&module);

    assert!(
        text.contains("const $test_project$Lib$Red = {$: \"Red\"};"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("const first = $test_project$Lib$Red;"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("const second = $test_project$Lib$Red;"),
        "got:\n{}",
        text
    );
    assert!(!text.contains("$test_project$Test$"), "got:\n{}", text);
}

/// `Lib` checked on its own and `Test` against its interface, the way a build orders them,
/// so a call across the boundary is what the front end makes of it and not a hand-built
/// stand-in. Answers both modules' emitted text, `Lib`'s first.
fn emitted_across(lib: &str, test: &str) -> (String, String) {
    let mut interfaces = HashMap::from([basics_interface(), char_interface(), maybe_interface()]);
    let lib = checked_against(lib, interfaces.clone());
    interfaces.insert(lib.canonical.name.name().clone(), lib.to_interface(None));
    let test = checked_against(test, interfaces);

    (emit(&lib), emit(&test))
}

/// A module exports a declaration of two parameters as the plain two-parameter function it
/// emitted, and a module importing it reads its arity from the interface: a call supplying
/// both arguments is a direct call, and a partial application goes through `$curry`, as it
/// would inside `Lib`.
///
/// Mutation-checked two ways: by dropping the `ReferenceKind::Foreign` arm of
/// `Emitter::application`, which calls the import one argument at a time,
/// `test_project$Lib$pick(n)(n)`; and by recording no arity in
/// `canonical::Module::to_interface`, which reads the import as a parameterless binding's
/// and turns both calls red.
#[test]
fn an_exported_function_is_called_directly_by_an_importer() {
    let (lib, test) = emitted_across(
        indoc! {r#"
            module Lib exposing (pick)

            pick : Int -> Int -> Int
            pick a b =
              a
        "#},
        indoc! {r#"
            module Test exposing (both, one)

            import Lib

            both : Int -> Int
            both n =
              Lib.pick n n

            one : Int -> Int
            one =
              Lib.pick 1
        "#},
    );

    assert_eq!(
        lib,
        indoc! {r#"
            function pick(a, b) {
              return a;
            }

            export { pick };
        "#}
    );
    assert!(
        test.contains("function both(n) {\n  return test_project$Lib$pick(n, n);\n}"),
        "got:\n{}",
        test
    );
    assert!(
        test.contains("const one = $curry(test_project$Lib$pick, 2)(1n);"),
        "got:\n{}",
        test
    );
}

/// An imported operator is a call of the function its `infix` declaration names, with
/// that function's arity, even when `Lib`'s header exposes only the operator — the arity
/// then travels with the function's type in the interface's `infix_functions`.
///
/// Mutation-checked by recording arities for `values` alone in
/// `canonical::Module::to_interface`: `n +++ n` then reads `test_project$Lib$plus(n)(n)`.
#[test]
fn an_imported_operator_is_a_direct_call_of_its_function() {
    let (_, test) = emitted_across(
        indoc! {r#"
            module Lib exposing ((+++))

            infix left 6 (+++) = plus

            plus : Int -> Int -> Int
            plus a b =
              a
        "#},
        indoc! {r#"
            module Test exposing (double)

            import Lib exposing ((+++))

            double : Int -> Int
            double n =
              n +++ n
        "#},
    );

    assert!(
        test.contains("return test_project$Lib$plus(n, n);"),
        "got:\n{}",
        test
    );
}

/// A parameterless binding whose value is another module's function of two parameters —
/// `Basics`' `add = Js.Basics.addInt` is the shape — holds that function `$curry`'d, since a
/// caller of the binding sees arity 0 and calls it one argument at a time. An imported
/// function of one parameter already takes its argument one at a time and is not wrapped.
///
/// Mutation-checked by making `Emitter::value`'s `ReferenceKind::Foreign` arm return the
/// bare import whatever its arity: `add` then holds the raw two-parameter function.
#[test]
fn a_binding_to_an_imported_function_holds_it_curried() {
    let (_, test) = emitted_across(
        indoc! {r#"
            module foreign Lib exposing (addInt, negateInt)

            unsafe addInt : Int -> Int -> Int
            unsafe negateInt : Int -> Int
        "#},
        indoc! {r#"
            module Test exposing (double, negate)

            import Lib

            add : Int -> Int -> Int
            add =
              Lib.addInt

            negate : Int -> Int
            negate =
              Lib.negateInt

            double : Int -> Int
            double n =
              add n n
        "#},
    );

    assert!(
        test.contains("const add = $curry(test_project$Lib$addInt, 2);"),
        "got:\n{}",
        test
    );
    assert!(
        test.contains("const negate = test_project$Lib$negateInt;"),
        "got:\n{}",
        test
    );
    assert!(test.contains("return add(n)(n);"), "got:\n{}", test);
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
    assert!(!text.contains("$test_project$Test$Rgb"), "got:\n{}", text);
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

/// A parameterless binding that reaches another only through a function it calls is
/// emitted after it: `a` calls `f`, whose body reads `z`, so `const a = f(1n)` would read
/// `z` before it is initialised if it came first
/// ([*A binding with no parameters is evaluated
/// once*](../docs/spec/evaluation-semantics.md#a-binding-with-no-parameters-is-evaluated-once)).
/// The same module with `a` and `z` swapped is checked too, so the order cannot come from
/// breaking a tie by name: in one of the two the dependency sorts first, in the other last.
///
/// Mutation-checked by making `canonical::dependency_graph` add a node for the
/// parameterless declarations only, as it once did: the edge through `f` disappears, the
/// two bindings become independent, and the name tie-break puts `const a = f(1n);` first
/// in the first module.
#[test]
fn a_binding_reached_through_a_function_is_emitted_first() {
    let text = emitted(indoc! {r#"
        module Test exposing (a)

        a : Int
        a =
          f 1

        f : Int -> Int
        f x =
          z

        z : Int
        z =
          2
    "#});

    assert!(
        position(&text, "const z = 2n;") < position(&text, "const a = f(1n);"),
        "`f` reads `z`, so `z` has to be initialised before `a` calls `f`, got:\n{}",
        text
    );

    let swapped = emitted(indoc! {r#"
        module Test exposing (z)

        z : Int
        z =
          f 1

        f : Int -> Int
        f x =
          a

        a : Int
        a =
          2
    "#});

    assert!(
        position(&swapped, "const a = 2n;") < position(&swapped, "const z = f(1n);"),
        "`f` reads `a`, so `a` has to be initialised before `z` calls `f`, got:\n{}",
        swapped
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

/// A `case` over a three-constructor union is the scrutinee bound once, then a chain of
/// `if`/`else` nested one per constructor tested — in source order, since [Conditional
/// evaluation](../docs/spec/evaluation-semantics.md#conditional-evaluation) tries a
/// `case`'s branches in the order written — inside an immediately invoked function, so
/// the whole thing is still one expression. Coverage is not checked yet (`LANG-19`), so
/// even a `case` naming every constructor gets a `Fail` leaf after the last test, which
/// calls the runtime's `$abort` naming the declaration.
///
/// This pins the whole shape with one `assert!(text.contains(..))` against the full
/// nested block, plus a separate `starts_with` on the import — not an `assert_eq!` on
/// the whole module — since the nesting and the fall-through are the part every other
/// test below takes for granted.
///
/// This pins the text only. That the emitted `case` returns each branch's value when run
/// is checked by `std/core/tests/CaseTests.zel`, under `cargo run -- test std/core`.
/// What a value no branch matches does is pinned here alone: the abort cannot be
/// asserted from inside Zelkova.
///
/// Mutation-checked by having `Emitter::decision`'s `Test` arm drop the `else` and
/// concatenate `matched` and `default` one after the other: the `Green`/`Blue` arms
/// then both return, which is not valid JavaScript and the text no longer matches.
#[test]
fn a_case_on_a_three_constructor_union_is_nested_ifs_naming_each_tag() {
    let text = emitted(indoc! {r#"
        module Test exposing (label)

        type Colour
          = Red
          | Green
          | Blue

        label : Colour -> Int
        label c =
          case c of
            Red ->
              1

            Green ->
              2

            Blue ->
              3
    "#});

    assert!(
        text.contains(indoc! {r#"
            function label(c) {
              return (() => {
              const $scrutinee = c;
              if ($scrutinee.$ === "Red") {
                {
                  return 1n;
                }
              } else {
                if ($scrutinee.$ === "Green") {
                  {
                    return 2n;
                  }
                } else {
                  if ($scrutinee.$ === "Blue") {
                    {
                      return 3n;
                    }
                  } else {
                    return $abort("`label`'s case matched no branch");
                  }
                }
              }
            })();
            }
        "#}),
        "got:\n{}",
        text
    );
    assert!(
        text.starts_with("import { $abort } from \"../zelkova.mjs\";\n"),
        "got:\n{}",
        text
    );
}

/// The scrutinee is bound to `$scrutinee` once, even though the tree tests it more than
/// once (`Red`, then `Green`, then the fall-through to `Blue`): re-emitting it at every
/// test would call `next` once per test, evaluating it more than once — [Order of
/// evaluation](../docs/spec/evaluation-semantics.md#order-of-evaluation) forbids that.
///
/// Mutation-checked by having `case_expression` call `self.expression(scrutinee)`
/// inside `test_condition` instead of binding it to `$scrutinee` first: `next(c)` then
/// appears twice.
#[test]
fn a_cases_scrutinee_is_evaluated_once() {
    let text = emitted(indoc! {r#"
        module Test exposing (label)

        type Colour
          = Red
          | Green
          | Blue

        next : Colour -> Colour
        next value =
          value

        label : Colour -> Int
        label c =
          case next c of
            Red ->
              1

            Green ->
              2

            Blue ->
              3
    "#});

    assert_eq!(
        text.matches("next(c)").count(),
        1,
        "the scrutinee is a call, evaluated once, got:\n{}",
        text
    );
}

/// A wildcard branch matches unconditionally and binds nothing: the whole `case`
/// collapses to the scrutinee binding and a bare `return`, with no `if` at all.
///
/// That a wildcard gathers no binding is `ir::decision_tree`'s doing (`GEN-5`), not this
/// backend's — `Emitter::decision`'s `Leaf` arm only ever emits one `const` per binding
/// it is handed, so there is no mutation of *this* module that makes it emit a binding a
/// wildcard has none of. What this test actually pins on the emitter's own side is that
/// an unconditional match produces no `if` at all, only the leaf's own block.
#[test]
fn a_wildcard_branch_binds_nothing() {
    let text = emitted(indoc! {r#"
        module Test exposing (always_one)

        always_one : Int -> Int
        always_one n =
          case n of
            _ ->
              1
    "#});

    assert!(
        text.contains("const $scrutinee = n;\n  {\n    return 1n;\n  }"),
        "got:\n{}",
        text
    );
}

/// A variable branch also matches unconditionally, and binds the whole scrutinee under
/// its own (mangled) name.
///
/// Mutation-checked by having `occurrence_expr` answer `"$scrutinee.a"` for
/// `Occurrence::Root` instead of `root` itself: `x` is then bound to a field of the
/// scrutinee rather than the scrutinee.
#[test]
fn a_variable_branch_binds_the_whole_scrutinee() {
    let text = emitted(indoc! {r#"
        module Test exposing (identity)

        identity : Int -> Int
        identity n =
          case n of
            x ->
              x
    "#});

    assert!(
        text.contains("{\n    const x = $scrutinee;\n    return x;\n  }"),
        "got:\n{}",
        text
    );
}

/// A variable branch's binding may repeat a name the scrutinee expression itself reads —
/// [Variable patterns](../docs/spec/patterns.md#variable-patterns) allows it, since a
/// pattern's names are a fresh scope, not a reference to whatever a name already means.
/// `next(x)` here reads the parameter `x`, and the branch rebinds `x` to the whole
/// scrutinee; the two must not share a JavaScript scope, or the `const x` inside the
/// leaf's own block would put the `x` inside `next(x)` in its temporal dead zone and the
/// emitted function would throw `ReferenceError: Cannot access 'x' before
/// initialization` instead of returning.
///
/// Mutation-checked by having `Emitter::decision`'s `Leaf` arm emit its bindings and
/// `return` directly, the way it did before this test, instead of wrapping them in their
/// own block: the assertion below then looks for a block that is not there.
#[test]
fn a_shadowing_variable_branch_does_not_reach_into_the_scrutinees_scope() {
    let text = emitted(indoc! {r#"
        module Test exposing (identity)

        next : Int -> Int
        next value =
          value

        identity : Int -> Int
        identity x =
          case next x of
            x ->
              x
    "#});

    assert!(
        text.contains(
            "const $scrutinee = next(x);\n  {\n    const x = $scrutinee;\n    return x;\n  }"
        ),
        "got:\n{}",
        text
    );
}

/// An `Int` pattern is tested by equality against its `BigInt` literal.
///
/// Mutation-checked by having `test_condition`'s `Int` arm drop the `n` suffix: the
/// condition then reads `$scrutinee === 1`, a `Number` comparison an `Int` — a
/// `BigInt` — never equals.
#[test]
fn an_int_pattern_is_tested_by_equality() {
    let text = emitted(indoc! {r#"
        module Test exposing (label)

        label : Int -> Int
        label n =
          case n of
            1 ->
              10

            _ ->
              0
    "#});

    assert!(text.contains("if ($scrutinee === 1n) {"), "got:\n{}", text);
}

/// A `Char` pattern is tested by equality against its one-character string.
///
/// Mutation-checked by having `test_condition`'s `Char` arm write the character
/// unquoted: the emitted text is then not valid JavaScript.
#[test]
fn a_char_pattern_is_tested_by_equality() {
    let text = emitted(indoc! {r#"
        module Test exposing (code)

        code : Char -> Int
        code c =
          case c of
            'a' ->
              1

            _ ->
              0
    "#});

    assert!(
        text.contains("if ($scrutinee === \"a\") {"),
        "got:\n{}",
        text
    );
}

/// A `Bool` pattern — `true` or `false` — is tested by the value itself, never by a `$`
/// tag: `Bool` is a JavaScript boolean, not a tagged object, so a `case` on it reads
/// exactly like a `Basics.True`/`Basics.False` constructor pattern would.
///
/// Mutation-checked by having `test_condition`'s `Bool` arm build a tag check,
/// `.$ === "True"`, the way `Outcome::Constructor` does: the condition then reads a
/// field a JavaScript boolean does not have.
#[test]
fn a_bool_pattern_is_tested_by_its_value_not_a_tag() {
    let text = emitted(indoc! {r#"
        module Test exposing (choose)

        choose : Bool -> Int
        choose flag =
          case flag of
            true ->
              1

            false ->
              0
    "#});

    assert!(
        text.contains("if ($scrutinee === true) {"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("if ($scrutinee === false) {"),
        "got:\n{}",
        text
    );
    assert!(!text.contains("$scrutinee.$"), "got:\n{}", text);
}

/// The `True`/`False` constructor spelling emits exactly the same condition as the
/// `true`/`false` literal spelling does — `ir::translate_pattern` normalises both to the
/// same [`ir::Outcome::Literal`] before this backend ever sees the pattern
/// (`a_bool_constructor_is_tested_by_value_like_a_bool_literal` in `tests/ir.rs` pins
/// that), so this backend has no `Bool`-specific code path to tell the two spellings
/// apart at all. `True`/`False` is the more common spelling in real code, and nothing
/// above this test exercises it.
///
/// Mutation-checked the same way as the sibling test above: having `test_condition`'s
/// `Bool` arm build a tag check turns this one red too, since `True`/`False` reaches it
/// through the exact same `Outcome::Literal(Bool)`.
#[test]
fn a_bool_constructor_pattern_is_tested_by_its_value_not_a_tag() {
    let text = emitted(indoc! {r#"
        module Test exposing (choose)

        choose : Bool -> Int
        choose flag =
          case flag of
            True ->
              1

            False ->
              0
    "#});

    assert!(
        text.contains("if ($scrutinee === true) {"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("if ($scrutinee === false) {"),
        "got:\n{}",
        text
    );
    assert!(!text.contains("$scrutinee.$"), "got:\n{}", text);
}

/// A tuple pattern tests nothing — a value of a tuple type is always a tuple — and binds
/// each named element by its index into the array a tuple is. The wildcard element
/// contributes no binding.
///
/// Mutation-checked by having `occurrence_expr`'s `TupleElement` arm read
/// `.{index}` (a field, the way a constructor argument is read) instead of `[{index}]`:
/// the emitted text then reads a field a JavaScript array does not have.
#[test]
fn a_tuple_pattern_binds_its_elements_by_index() {
    let text = emitted(indoc! {r#"
        module Test exposing (second)

        second : (Int, Int) -> Int
        second pair =
          case pair of
            (_, b) ->
              b
    "#});

    assert!(
        text.contains("{\n    const b = $scrutinee[1];\n    return b;\n  }"),
        "got:\n{}",
        text
    );
}

/// A constructor pattern is tested by its `$` tag and binds its arguments by field —
/// `a`, `b`, … in declaration order, the same names [`tagged`] builds an object under.
/// `Box` has two constructors and the `case` names both, but coverage is not checked
/// yet (`LANG-19`), so the last one's `default` still reaches a `Fail` leaf, which
/// calls `$abort` naming the declaration rather than falling through to `undefined`.
///
/// Mutation-checked two ways: having `test_condition`'s `Constructor` arm read `.b`
/// instead of `.$`, and having `Emitter::decision`'s `Fail` arm return `undefined`
/// instead of calling `$abort` — the second is also acceptance's own requirement, since
/// nothing else in this suite runs the emitted text under `node` to observe it.
#[test]
fn a_constructor_pattern_is_tested_by_tag_and_binds_by_field() {
    let text = emitted(indoc! {r#"
        module Test exposing (Box, withDefault)

        type Box
          = Just Int
          | Nothing

        withDefault : Int -> Box -> Int
        withDefault fallback box =
          case box of
            Just n ->
              n

            Nothing ->
              fallback
    "#});

    assert!(
        text.contains(
            "if ($scrutinee.$ === \"Just\") {\n    {\n      const n = $scrutinee.a;\n      return n;\n    }"
        ),
        "got:\n{}",
        text
    );
    assert!(
        text.contains(
            "if ($scrutinee.$ === \"Nothing\") {\n      {\n        return fallback;\n      }"
        ),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("return $abort(\"`withDefault`'s case matched no branch\");"),
        "got:\n{}",
        text
    );
}

/// A parameter written as a pattern emits the same way a `case` does — the IR holds it
/// as a single-branch match on the parameter ([`ir::CaseForm::Parameter`]) — so `first`
/// takes its parameter under [`ir::pattern_parameter`]'s name, `$0`, and its body is the
/// same scrutinee-then-bindings shape a `case` expression's is.
///
/// Mutation-checked by keeping `Construct::ParameterPattern`'s old refusal instead of
/// routing `CaseForm::Parameter` through `case_expression` too: `first` is then refused
/// rather than emitted.
#[test]
fn a_parameter_written_as_a_pattern_emits_as_a_single_branch_match() {
    let text = emitted(indoc! {r#"
        module Test exposing (first)

        first : (Int, Int) -> Int
        first (x, _) =
          x
    "#});

    assert!(text.contains("function first($0) {"), "got:\n{}", text);
    assert!(
        text.contains(
            "const $scrutinee = $0;\n  {\n    const x = $scrutinee[0];\n    return x;\n  }"
        ),
        "got:\n{}",
        text
    );
}

/// A parameter written as a pattern can fail to match — `unwrap`'s parameter names only
/// `Just`, so a `Nothing` reaches the same `Fail` leaf a `case` missing a branch would —
/// and the `$abort` it calls describes itself as a parameter pattern, not a `case`, since
/// that is what the source actually wrote ([`ir::CaseForm::Parameter`]).
///
/// Mutation-checked by having `abort_description` ignore `form` and always build the
/// `case`-worded message: the assertion below then looks for text that is not there.
#[test]
fn a_failing_parameter_pattern_describes_itself_as_a_parameter_not_a_case() {
    let text = emitted(indoc! {r#"
        module Test exposing (unwrap)

        unwrap : Maybe Int -> Int
        unwrap (Just n) =
          n
    "#});

    assert!(
        text.contains("return $abort(\"`unwrap`'s parameter pattern matched no branch\");"),
        "got:\n{}",
        text
    );
    assert!(!text.contains("case matched no branch"), "got:\n{}", text);
}

/// A module holding a declaration the typer could not check is refused rather than
/// emitted without it. `helper` matches a tuple pattern nested inside another tuple
/// pattern, which the typer does not translate.
///
/// Mutation-checked by starting `emit`'s errors empty instead of from `ir.unchecked`:
/// the module is then emitted with `helper` missing.
#[test]
fn a_declaration_with_no_ir_is_refused() {
    let errors = refused(indoc! {r#"
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

    // `NodeSpan`'s equality ignores the span, so this compares the variant and the name.
    assert_eq!(
        errors,
        vec![Error::Unchecked {
            name: Name::new("helper"),
            span: NodeSpan::none(),
        }]
    );
}

/// An `unsafe` facade emits a module that imports its companion under an alias and
/// re-exports its names: a two-parameter export is a plain function whose body is a
/// direct, saturated call to the companion, and a zero-parameter one (a facade
/// constant) is a `const` bound to the companion's value. Each value the companion hands
/// back is run through the predicate of its declared type, and the runtime's `$abort` is
/// called, naming the export, when it fails.
///
/// Mutation-checked by reverting `Emitter::facade_declaration` to build the function's
/// call one argument at a time (`$companion$add(a)(b)`) instead of the plain parameter list:
/// this test's `assert_eq!` then fails on the function's body. And by returning
/// `$returned` in place of `checked` in both arms: the text then has no check at all.
#[test]
fn an_unsafe_facade_re_exports_its_companion() {
    let text = emitted(indoc! {r#"
        module foreign Test exposing (add, pi)

        unsafe add : Int -> Int -> Int
        unsafe pi : Float
    "#});

    assert_eq!(
        text,
        indoc! {r#"
            import { $abort } from "../zelkova.mjs";
            import { add as $companion$add, pi as $companion$pi } from "./Test.companion.mjs";

            function add(a, b) {
              const $returned = $companion$add(a, b);
              return typeof $returned === "bigint" && BigInt.asIntN(64, $returned) === $returned ? $returned : $abort("`Test.add`'s companion returned a value its declared type, `Int`, does not admit");
            }

            const pi = (($returned) => typeof $returned === "number" ? $returned : $abort("`Test.pi`'s companion returned a value its declared type, `Float`, does not admit"))($companion$pi);

            export { add, pi };
        "#}
    );
}

/// A facade whose result is `()` calls its companion and returns `undefined` whatever
/// it answered, so nothing is checked and nothing can fail; a facade constant of type
/// `()` is `undefined`, its export still imported so a companion missing it fails to link
/// ([The unit value crosses as
/// `undefined`](../docs/spec/interop.md#the-unit-value-crosses-as-undefined)).
///
/// Mutation-checked by deleting the `canonical::Type::Unit` branch at the top of
/// `Emitter::facade_declaration`: both declarations then check `$returned === undefined`
/// and import `$abort`.
#[test]
fn a_facade_result_of_unit_is_discarded_not_checked() {
    let text = emitted(indoc! {r#"
        module foreign Test exposing (log, nothing)

        unsafe log : Int -> ()
        unsafe nothing : ()
    "#});

    assert_eq!(
        text,
        indoc! {r#"
            import { log as $companion$log, nothing as $companion$nothing } from "./Test.companion.mjs";

            function log(a) {
              $companion$log(a);
              return undefined;
            }

            const nothing = undefined;

            export { log, nothing };
        "#}
    );
}

/// A tuple result is an array of the tuple's length whose elements each pass their own
/// predicate; a `Char` is a string of exactly one code point, a `Bool` a boolean, and a
/// `()` nested inside the tuple — unlike a whole result of `()` — must be `undefined`.
///
/// Mutation-checked by dropping the `length` test from `Predicates::test`'s tuple arm,
/// and separately by making its `Unit` arm answer `true`: the text no longer matches
/// either time.
#[test]
fn a_tuple_result_is_checked_element_by_element() {
    let text = emitted(indoc! {r#"
        module foreign Test exposing (split)

        unsafe split : Int -> (Char, Bool, ())
    "#});

    assert!(
        text.contains(
            "return Array.isArray($returned) && $returned.length === 3 \
             && typeof $returned[0] === \"string\" \
             && $returned[0].length === ($returned[0].codePointAt(0) > 0xFFFF ? 2 : 1) \
             && typeof $returned[1] === \"boolean\" \
             && $returned[2] === undefined ? $returned : $abort("
        ),
        "got:\n{}",
        text
    );
}

/// `lib` and a `facade` importing it, checked in that order, and the text the facade
/// emits with both modules' unions to read — as `compile_package` hands [`Unions`] every
/// module of the build.
fn facade_across(lib: &str, facade: &str) -> Result<String, Vec<Error>> {
    let mut interfaces = HashMap::from([basics_interface(), char_interface(), maybe_interface()]);
    let lib = checked_against(lib, interfaces.clone());
    interfaces.insert(lib.canonical.name.name().clone(), lib.to_interface(None));
    let facade = checked_against(facade, interfaces);

    javascript::emit(&facade, true, &Unions::of([&lib, &facade]))
}

/// A union result is decided by a function of the union's own, emitted into the facade:
/// it answers `false` for anything but an object, then reads `$` against every
/// constructor of the declaration — even when the declaring module exposes the type
/// without them, as `Shape` does here — checks each argument, read off `a`, `b`, …,
/// against the type its constructor declares, and answers `false` for a `$` naming no
/// constructor.
///
/// Mutation-checked by dropping the `default:` case `Predicates::union_function` pushes,
/// and separately by reading each argument off `field(index + 1)`: the text no longer
/// matches either time.
#[test]
fn a_union_result_is_checked_against_its_declaration() {
    let text = facade_across(
        indoc! {r#"
            module Shape exposing (Shape)

            type Shape
              = Circle Int
              | Rect Int Float
              | Empty
        "#},
        indoc! {r#"
            module foreign Test exposing (make)

            import Shape exposing (Shape)

            unsafe make : Int -> Shape
        "#},
    )
    .unwrap_or_else(|errors| panic!("expected the facade to emit, got {:?}", errors));

    assert!(
        text.contains(indoc! {r#"
            function $is$test_project$Shape$Shape($v0) {
              if (typeof $v0 !== "object" || $v0 === null) {
                return false;
              }
              switch ($v0.$) {
                case "Circle":
                  return typeof $v0.a === "bigint" && BigInt.asIntN(64, $v0.a) === $v0.a;
                case "Rect":
                  return typeof $v0.a === "bigint" && BigInt.asIntN(64, $v0.a) === $v0.a && typeof $v0.b === "number";
                case "Empty":
                  return true;
                default:
                  return false;
              }
            }

            function make(a) {
              const $returned = $companion$make(a);
              return $is$test_project$Shape$Shape($returned) ? $returned : $abort("#}),
        "got:\n{}",
        text
    );
}

/// A union with a type variable takes one predicate per variable, which the call site
/// builds from the type the union is applied to; a recursive union's function calls
/// itself, rather than being built again, and threads its own variable's predicate
/// through.
///
/// Mutation-checked by deleting the `contains_key` early return in `Predicates::union`:
/// building `Tree` then recurses until the stack overflows, and the test aborts.
#[test]
fn a_recursive_parameterised_union_calls_itself_with_its_variable() {
    let text = facade_across(
        indoc! {r#"
            module Tree exposing (Tree(..))

            type Tree a
              = Leaf
              | Node (Tree a) a (Tree a)
        "#},
        indoc! {r#"
            module foreign Test exposing (build)

            import Tree exposing (Tree)

            unsafe build : Int -> Tree Char
        "#},
    )
    .unwrap_or_else(|errors| panic!("expected the facade to emit, got {:?}", errors));

    assert!(
        text.contains("function $is$test_project$Tree$Tree($v0, $p0) {"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains(
            "return $is$test_project$Tree$Tree($v0.a, ($v1) => $p0($v1)) && $p0($v0.b) \
             && $is$test_project$Tree$Tree($v0.c, ($v1) => $p0($v1));"
        ),
        "got:\n{}",
        text
    );
    assert!(
        text.contains(
            "return $is$test_project$Tree$Tree($returned, ($v0) => typeof $v0 === \"string\" \
             && $v0.length === ($v0.codePointAt(0) > 0xFFFF ? 2 : 1)) ? $returned : $abort("
        ),
        "got:\n{}",
        text
    );
}

/// A union a facade may name but no predicate can decide — one of its constructors
/// declares a function-typed argument — is refused where its predicate would be built,
/// naming the constructor, and a second facade declaration naming the same union is
/// refused too rather than calling a predicate that was never finished.
///
/// Mutation-checked by deleting the `self.functions.remove(&name)` in
/// `Predicates::union`: `second` then emits a call to an empty function and only `first`
/// is refused.
#[test]
fn a_union_holding_a_function_has_no_predicate() {
    let errors = facade_across(
        indoc! {r#"
            module Handler exposing (Handler(..))

            type Handler
              = Handler (Int -> Int)
        "#},
        indoc! {r#"
            module foreign Test exposing (first, second)

            import Handler exposing (Handler)

            unsafe first : Int -> Handler
            unsafe second : Handler
        "#},
    )
    .err()
    .expect("expected the facade to be refused");

    let refusal = |name: &str| Error::NoPredicate {
        name: Name::new(name),
        span: NodeSpan::none(),
        found: Unpredicated::Function,
        constructor: Some(test_qual("Handler.Handler")),
    };
    assert_eq!(errors, vec![refusal("first"), refusal("second")]);
}

/// The text an effectful facade `source` emits as, with `Task`, `Failure` and `Result` in
/// the build.
fn emitted_effectful(source: &str) -> String {
    emit(&checked_against(
        source,
        HashMap::from([
            basics_interface(),
            char_interface(),
            maybe_interface(),
            task_interface(),
            result_interface(),
        ]),
    ))
}

/// A facade signature not marked `unsafe` declares an effect: its forwarding code builds a
/// `Task` whose run function calls the runtime's `$effect` with a function calling the
/// companion, the predicate of the payload, the export's name and the continuation. Nothing
/// calls the companion when the `Task` is built, and the predicate is the payload's `Int`,
/// not `Task (Result Failure Int)`'s.
///
/// Mutation-checked by matching `marked_unsafe: _` and skipping the payload lookup in
/// `Emitter::facade_declaration`: `add` is then taken for an `unsafe` facade, which calls the
/// companion directly and `Err`s on the `Task` no predicate decides.
#[test]
fn an_effectful_facade_builds_a_task_that_calls_effect() {
    let text = emitted_effectful(indoc! {r#"
        module foreign Test exposing (add)

        import Task exposing (Task, Failure)

        add : Int -> Int -> Task (Result Failure Int)
    "#});

    assert_eq!(
        text,
        indoc! {r#"
            import { $effect } from "../zelkova.mjs";
            import { add as $companion$add } from "./Test.companion.mjs";

            function add(a, b) {
              return {$: "Task", a: ($k) => $effect(() => $companion$add(a, b), ($returned) => typeof $returned === "bigint" && BigInt.asIntN(64, $returned) === $returned, "Test.add", $k)};
            }

            export { add };
        "#}
    );
}

/// A `()` payload has no predicate: `$effect` is handed `null`, and discards whatever the
/// companion returns.
///
/// Mutation-checked by building the predicate for a `()` payload too: the check then
/// reads `$returned === undefined` where `null` is expected.
#[test]
fn an_effectful_facade_with_a_unit_payload_passes_no_predicate() {
    let text = emitted_effectful(indoc! {r#"
        module foreign Test exposing (log)

        import Task exposing (Task, Failure)

        log : Int -> Task (Result Failure ())
    "#});

    assert!(
        text.contains("$effect(() => $companion$log(a), null, \"Test.log\", $k)"),
        "expected `null` in place of a predicate, got:\n{}",
        text
    );
}

/// A facade constant naming a `Task` is one module-level `Task`, and its companion export is
/// called, with no arguments, inside the run function — each time the `Task` runs.
///
/// Mutation-checked by emitting an arity-zero effectful facade with the `unsafe` constant's
/// shape (the companion export named, not called): the `$companion$now()` call goes missing.
#[test]
fn an_effectful_facade_constant_is_one_task_whose_companion_is_called_when_run() {
    let text = emitted_effectful(indoc! {r#"
        module foreign Test exposing (now)

        import Task exposing (Task, Failure)

        now : Task (Result Failure Int)
    "#});

    assert!(
        text.contains("const now = {$: \"Task\", a: ($k) => $effect(() => $companion$now(), "),
        "expected a module-level `Task` calling the companion with no arguments, got:\n{}",
        text
    );
}

/// An `unsafe` facade in the same module as an effectful one keeps its direct call: only the
/// unmarked signature gets the `Task`.
///
/// Mutation-checked by wrapping every facade declaration in `$effect`: `safe` then returns a
/// `Task`.
#[test]
fn an_unsafe_facade_beside_an_effectful_one_is_not_wrapped() {
    let text = emitted_effectful(indoc! {r#"
        module foreign Test exposing (safe, effectful)

        import Task exposing (Task, Failure)

        unsafe safe : Int -> Int
        effectful : Int -> Task (Result Failure Int)
    "#});

    assert!(
        text.contains("function safe(a) {\n  const $returned = $companion$safe(a);"),
        "expected `safe` to call its companion directly, got:\n{}",
        text
    );
    assert!(text.contains("function effectful(a) {\n  return {$: \"Task\""));
}

/// The errors a facade module fails to emit with once `edit` has changed its canonical module.
/// Canonicalization refuses each shape these tests need before the backend sees it, so the
/// module is checked as a valid `unsafe` facade and then edited by hand.
fn refused_after(edit: impl FnOnce(&mut CheckedModule)) -> Vec<Error> {
    let mut module = checked(indoc! {r#"
        module foreign Test exposing (add)

        unsafe add : Int -> Int -> Int
    "#});
    edit(&mut module);
    match javascript::emit(&module, true, &unions_of(&module)) {
        Ok(text) => panic!("expected the module to be refused, got:\n{}", text),
        Err(errors) => errors,
    }
}

/// A facade signature not marked `unsafe` whose result is not `Task (Result Failure a)` has no
/// payload to check, and is refused as `NotAnEffect`, naming the value and blaming the
/// signature's mark. `Int -> Int -> Int` unmarked is what canonicalization refuses first, so the
/// mark is cleared by hand.
///
/// Mutation-checked by dropping the `NotAnEffect` push from `effectful_result_payload`'s `None`
/// arm: no error is reported and this goes red.
#[test]
fn an_unmarked_facade_signature_that_is_not_a_task_is_refused_as_not_an_effect() {
    let errors = refused_after(|module| {
        if let Some(Value::TypedValue { marked_unsafe, .. }) =
            module.canonical.values.get_mut(&Name::new("add"))
        {
            *marked_unsafe = false;
        }
    });

    // `NodeSpan`'s equality ignores the span, so this compares the variant and the name.
    assert_eq!(
        errors,
        vec![Error::NotAnEffect {
            name: Name::new("add"),
            span: NodeSpan::none(),
        }]
    );
    assert_eq!(
        errors[0].message(),
        "`add` is not marked `unsafe`, and its result is not `Task (Result Failure a)`, so no wrapper can be built for it"
    );
}

/// A facade declaration the canonical module holds no signature for is refused as
/// `NoSignature`, whose message claims nothing about an `unsafe` mark or a `Task`, since there
/// is no signature to read either from.
///
/// Mutation-checked by pushing `NotAnEffect` at the `_` arm again: the variant assertion goes
/// red.
#[test]
fn a_facade_declaration_with_no_signature_is_refused_as_no_signature() {
    let errors = refused_after(|module| {
        module.canonical.values.remove(&Name::new("add"));
    });

    assert_eq!(
        errors,
        vec![Error::NoSignature {
            name: Name::new("add"),
            span: NodeSpan::none(),
        }]
    );
    assert_eq!(
        errors[0].message(),
        "the facade declaration `add` has no type signature, so its boundary cannot be built"
    );
}

/// A facade with no companion for the target being built is refused, naming the facade
/// and the target — the caller says so through `emit`'s `has_companion` parameter,
/// since `emit` has no path of its own to check a companion's presence with.
///
/// Mutation-checked by dropping the `!has_companion` half of the `emit` guard: this
/// then emits `add`'s forwarding code instead of refusing.
#[test]
fn a_facade_with_no_companion_is_refused() {
    let errors = refused_without_companion(indoc! {r#"
        module foreign Test exposing (add)

        unsafe add : Int -> Int -> Int
    "#});

    assert_eq!(
        errors,
        vec![Error::MissingCompanion {
            module: Name::new("Test"),
            target: "javascript",
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

/// `()` as an expression emits as `undefined`
/// ([The unit value crosses as
/// `undefined`](../docs/spec/interop.md#the-unit-value-crosses-as-undefined)).
///
/// Mutation-checked by reverting `Emitter::expression`'s `TypedTermKind::Unit` arm to
/// call `self.unsupported(Construct::Unit, term.span)`: the module is then refused
/// instead of emitted.
#[test]
fn a_unit_value_emits_as_undefined() {
    let text = emitted(indoc! {r#"
        module Test exposing (nothingUseful)

        nothingUseful : ()
        nothingUseful =
          ()
    "#});

    assert!(
        text.contains("const nothingUseful = undefined;"),
        "got:\n{}",
        text
    );
}

/// `()` as a pattern — a parameter, as `always` writes it, or a `case` branch, as
/// `viaCase` writes it — tests nothing and binds nothing: `decision_tree` already
/// lowers it to a [`Decision::Leaf`] with no test ahead of it, so both declarations
/// return their body unconditionally.
///
/// Mutation-checked by restoring the deleted `unit_pattern` loop at the top of
/// `Emitter::case_expression`: both declarations are then refused instead of emitted.
#[test]
fn a_unit_pattern_emits_with_no_test() {
    let text = emitted(indoc! {r#"
        module Test exposing (Flag(..), always, viaCase)

        type Flag
          = On
          | Off

        always : () -> Flag
        always () =
          On

        viaCase : () -> Flag
        viaCase u =
          case u of
            () ->
              Off
    "#});

    assert!(
        text.contains("function always($0) {\n  return (() => {\n  const $scrutinee = $0;\n  {\n    return $test_project$Test$On;\n  }\n})();\n}"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("function viaCase(u) {\n  return (() => {\n  const $scrutinee = u;\n  {\n    return $test_project$Test$Off;\n  }\n})();\n}"),
        "got:\n{}",
        text
    );
}

/// A `()` written as a tuple's element emits the same way as one at the top of a
/// pattern: no test, no binding for it, and the tuple's other element bound as usual.
///
/// Mutation-checked the same way as
/// [`a_unit_pattern_emits_with_no_test`]: restoring the deleted `unit_pattern` loop at
/// the top of `Emitter::case_expression` refuses `second` too, since that loop walks a
/// pattern to any depth and `()` here is nested inside the tuple rather than at the
/// top.
#[test]
fn a_nested_unit_pattern_emits_with_no_test() {
    let text = emitted(indoc! {r#"
        module Test exposing (second)

        second : (Int, ()) -> Int
        second (x, ()) =
          x
    "#});

    assert!(
        text.contains(
            "const $scrutinee = $0;\n  {\n    const x = $scrutinee[0];\n    return x;\n  }"
        ),
        "got:\n{}",
        text
    );
}

/// A binding named `undefined` is mangled to `$undefined`, so declaring one does not
/// change what a `()` elsewhere in the module reads as: without the mangling, `const
/// undefined = 1n;` would shadow the global for the rest of the module, and
/// `nothingUseful`'s `()` would read `1n` rather than the one value `()` has.
///
/// Mutation-checked by dropping `"undefined"` from `RESERVED`: the binding then emits
/// as `const undefined = 1n;`, which this test's `assert!` no longer finds.
#[test]
fn a_binding_named_undefined_is_mangled() {
    let text = emitted(indoc! {r#"
        module Test exposing (undefined, nothingUseful)

        undefined : Int
        undefined =
          1

        nothingUseful : ()
        nothingUseful =
          ()
    "#});

    assert!(text.contains("const $undefined = 1n;"), "got:\n{}", text);
    assert!(
        text.contains("const nothingUseful = undefined;"),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("export { nothingUseful, $undefined as undefined };"),
        "got:\n{}",
        text
    );
}

/// `std/core`'s `Task` module emits as it stands: `succeed` and `map` are exported and
/// nothing else is (`Done`'s helpers and the `Task` constructor stay inside), and each
/// helper that would hand off to a run function or a continuation returns a `Bounce`
/// of that call instead of making it.
///
/// Whether the emitted chain *runs* is not checked here; that needs the runtime's loop
/// (`$runTask`), which `runtime/js/tests/zelkovaChecks.mjs` checks.
///
/// Mutation-checked by making `Task.zel`'s `succeedRun` call `k a` directly instead of
/// returning `Bounce (callWith k a)`: the `succeedRun` assertion goes red.
#[test]
fn std_cores_task_module_emits_with_every_handoff_a_bounce() {
    let source = include_str!("../std/core/src/Task.zel");
    let core = PackageName::new("zelkova-core").unwrap();
    let module = check_module(&core, &HashMap::new(), &parse_source(source))
        .unwrap_or_else(|error| panic!("expected Task.zel to check, got {:?}", error));

    let text = emit(&module);

    assert!(
        text.contains("export { map, succeed };"),
        "expected exactly `map` and `succeed` exported, got:\n{}",
        text
    );
    for helper in ["succeedRun", "mapRun", "mapContinue"] {
        let start = text
            .find(&format!("function {}(", helper))
            .unwrap_or_else(|| panic!("`{}` should be emitted, got:\n{}", helper, text));
        let body = &text[start..];
        let body = &body[..body.find("\n}").unwrap_or(body.len())];
        assert!(
            body.contains("return {$: \"Bounce\", a: "),
            "`{}` should return a `Bounce`, got:\n{}",
            helper,
            body
        );
    }
}
