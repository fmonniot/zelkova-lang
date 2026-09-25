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
use zelkova_lang::compiler::javascript::{self, Error};
use zelkova_lang::compiler::name::Name;
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
    javascript::emit(module, true)
        .unwrap_or_else(|errors| panic!("expected the module to emit, got {:?}", errors))
}

/// The text `source` emits as, insisting that it emits.
fn emitted(source: &str) -> String {
    emit(&checked(source))
}

/// The errors `source` fails to emit with, insisting that it fails.
fn refused(source: &str) -> Vec<Error> {
    match javascript::emit(&checked(source), true) {
        Ok(text) => panic!("expected the module to be refused, got:\n{}", text),
        Err(errors) => errors,
    }
}

/// The errors `source` fails to emit with when no companion is available for the
/// target being built, insisting that it fails.
fn refused_without_companion(source: &str) -> Vec<Error> {
    match javascript::emit(&checked(source), false) {
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
/// The fixture is a `Basics` of its own, declaring `Bool` itself, so no interface has to
/// be built for it.
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
/// `Lib` is checked first and `Test` against its interface, the way a build orders
/// them, so what is emitted is what the front end makes of an imported constructor —
/// written both exposed and qualified — and not a hand-built stand-in for it.
///
/// Mutation-checked two ways: by not recording the constructor in `value`, which
/// leaves `$Lib$Red` mentioned and never declared; and by naming an exposed
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
        text.contains("const $Lib$Red = {$: \"Red\"};"),
        "got:\n{}",
        text
    );
    assert!(text.contains("const first = $Lib$Red;"), "got:\n{}", text);
    assert!(text.contains("const second = $Lib$Red;"), "got:\n{}", text);
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

/// A `case` over a three-constructor union is the scrutinee bound once, then a chain of
/// `if`/`else` nested one per constructor tested — in source order, since [Conditional
/// evaluation](../docs/spec/evaluation-semantics.md#conditional-evaluation) tries a
/// `case`'s branches in the order written — inside an immediately invoked function, so
/// the whole thing is still one expression. Coverage is not checked yet (`LANG-19`), so
/// even a `case` naming every constructor gets a `Fail` leaf after the last test, which
/// calls the runtime's `$abort` naming the declaration.
///
/// This pins the whole shape in one `assert_eq!` rather than a handful of `contains`,
/// since the nesting and the fall-through are the part every other test below takes for
/// granted.
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
                return 1n;
              } else {
                if ($scrutinee.$ === "Green") {
                  return 2n;
                } else {
                  if ($scrutinee.$ === "Blue") {
                    return 3n;
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
/// Mutation-checked by having `Emitter::decision`'s `Leaf` arm emit a `const` for every
/// binding the branch's pattern *could* have had rather than the ones `ir::decision_tree`
/// actually gathered: a wildcard then gets a spurious `const`.
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
        text.contains("const $scrutinee = n;\n  return 1n;"),
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
        text.contains("const x = $scrutinee;\n  return x;"),
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
/// exactly like a `Basics.True`/`Basics.False` constructor pattern would, and the
/// two spellings emit identically.
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
        text.contains("const b = $scrutinee[1];\n  return b;"),
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
            "if ($scrutinee.$ === \"Just\") {\n    const n = $scrutinee.a;\n    return n;"
        ),
        "got:\n{}",
        text
    );
    assert!(
        text.contains("if ($scrutinee.$ === \"Nothing\") {\n      return fallback;"),
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
        text.contains("const $scrutinee = $0;\n  const x = $scrutinee[0];\n  return x;"),
        "got:\n{}",
        text
    );
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
/// constant) is a `const` bound to the companion's value.
///
/// Mutation-checked by reverting `Emitter::facade_declaration` to build the function's
/// call one argument at a time (`Test$add(a)(b)`) instead of the plain parameter list:
/// this test's `assert_eq!` then fails on the function's body.
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
            import { add as Test$add, pi as Test$pi } from "./Test.mjs";

            function add(a, b) {
              return Test$add(a, b);
            }

            const pi = Test$pi;

            export { add, pi };
        "#}
    );
}

/// A facade signature not marked `unsafe` declares an effect, which this backend does
/// not wrap yet, so it is refused rather than emitted as if it were `unsafe`.
///
/// Mutation-checked by dropping the `!marked_unsafe` check in
/// `Emitter::facade_declaration`: `add` then emits the `unsafe` shape.
#[test]
fn an_effectful_facade_signature_is_refused() {
    let errors = refused(indoc! {r#"
        module foreign Test exposing (add)

        add : Int -> Int -> Int
    "#});

    // `NodeSpan`'s equality ignores the span, so this compares the variant and the name.
    assert_eq!(
        errors,
        vec![Error::Effectful {
            name: Name::new("add"),
            span: NodeSpan::none(),
        }]
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
