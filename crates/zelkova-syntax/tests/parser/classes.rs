//! `LANG-38`: `class` and `instance` declarations parse, with a `where` block of members.
//!
//! `class` and `instance` are reserved; `where` is reserved as a type variable and nowhere
//! else; `derived` is reserved nowhere, and is a keyword only where the token after it says
//! so. The layout half of the ticket, a block per member, is in `layout.rs`.
//!
//! `NodeSpan`'s `PartialEq` is blind, so the head lines are checked through the span's
//! bytes and the rest through the names and shapes.
use codespan_reporting::files::SimpleFile;
use zelkova_syntax::name::Name;
use zelkova_syntax::parser::{
    self, ClassDecl, ClassMember, Error, InstanceBody, InstanceDecl, Module, Type, TypeKind,
};

fn parse(source: &str) -> Result<Module, Error> {
    parser::parse(&SimpleFile::new("test".to_owned(), source.to_owned()))
}

fn parse_ok(source: &str) -> Module {
    parse(source).unwrap_or_else(|error| panic!("expected {:?} to parse, got {:?}", source, error))
}

/// A type written back as source, so that a test reads the shape off one string.
fn show(tpe: &Type) -> String {
    match &tpe.kind {
        TypeKind::Unqualified(name, args) => {
            let mut text = name.to_string();
            for arg in args {
                let inner = show(arg);
                if matches!(&arg.kind, TypeKind::Unqualified(_, a) if !a.is_empty())
                    || matches!(arg.kind, TypeKind::Arrow(..))
                {
                    text.push_str(&format!(" ({})", inner));
                } else {
                    text.push_str(&format!(" {}", inner));
                }
            }
            text
        }
        TypeKind::Arrow(a, b) => format!("{} -> {}", show(a), show(b)),
        TypeKind::Variable(name) => name.to_string(),
        other => panic!("`show` has no case for {:?}", other),
    }
}

fn constraints(context: &Option<parser::Context>) -> Vec<String> {
    context
        .as_ref()
        .map(|context| context.constraints.iter().map(show).collect())
        .unwrap_or_default()
}

fn text_of(source: &str, span: zelkova_syntax::position::NodeSpan) -> &str {
    let span = span.span().expect("the declaration has a span");
    &source[span.start.0 as usize..span.end.0 as usize]
}

const EXAMPLE: &str = indoc::indoc! {"
    module Example exposing (Comparable)

    class Eq a => Comparable a where
      compare : a -> a -> Order

      derived compare
        matched = EQ
        differed i j =
          compare i j

        combine x y =
          y

    instance Comparable Colour where
      compare a b =
        EQ

    instance Eq a => Eq (Box a) where
      derived
"};

fn the_class(module: &Module) -> &ClassDecl {
    assert_eq!(module.classes.len(), 1);
    &module.classes[0]
}

fn instance(module: &Module, at: usize) -> &InstanceDecl {
    &module.instances[at]
}

#[test]
fn a_class_holds_its_superclass_head_and_members() {
    let module = parse_ok(EXAMPLE);
    let class = the_class(&module);

    assert_eq!(constraints(&class.context), vec!["Eq a"]);
    assert_eq!(show(&class.head), "Comparable a");
    assert_eq!(
        text_of(EXAMPLE, class.span),
        "class Eq a => Comparable a where"
    );
    assert_eq!(class.members.len(), 2);

    let ClassMember::Signature(signature) = &class.members[0] else {
        panic!("the first member is a signature: {:?}", class.members[0]);
    };
    assert_eq!(signature.name, Name::new("compare"));
    assert_eq!(show(&signature.tpe), "a -> a -> Order");
    assert!(signature.context.is_none());
    assert!(!signature.marked_unsafe);
}

#[test]
fn a_derivation_holds_one_binding_per_block() {
    let module = parse_ok(EXAMPLE);
    let class = the_class(&module);

    let ClassMember::Derivation(derivation) = &class.members[1] else {
        panic!("the second member is a derivation: {:?}", class.members[1]);
    };
    assert_eq!(derivation.member, Name::new("compare"));
    assert_eq!(text_of(EXAMPLE, derivation.span), "derived compare");

    let names: Vec<_> = derivation.bindings.iter().map(|b| b.name.clone()).collect();
    assert_eq!(
        names,
        vec![
            Name::new("matched"),
            Name::new("differed"),
            Name::new("combine")
        ]
    );
    let parameters: Vec<_> = derivation
        .bindings
        .iter()
        .map(|b| b.pattern.patterns.len())
        .collect();
    assert_eq!(parameters, vec![0, 2, 2]);
}

#[test]
fn an_instance_holds_a_binding_per_member() {
    let module = parse_ok(EXAMPLE);
    assert_eq!(module.instances.len(), 2);
    let plain = instance(&module, 0);

    assert!(plain.context.is_none());
    assert_eq!(show(&plain.head), "Comparable Colour");
    assert_eq!(
        text_of(EXAMPLE, plain.span),
        "instance Comparable Colour where"
    );
    let InstanceBody::Bindings(bindings) = &plain.body else {
        panic!("a body of bindings: {:?}", plain.body);
    };
    assert_eq!(bindings.len(), 1);
    assert_eq!(bindings[0].name, Name::new("compare"));
    assert_eq!(bindings[0].pattern.patterns.len(), 2);
}

#[test]
fn an_instance_may_carry_a_context_and_the_word_derived() {
    let module = parse_ok(EXAMPLE);
    let derived = instance(&module, 1);

    assert_eq!(constraints(&derived.context), vec!["Eq a"]);
    assert_eq!(show(&derived.head), "Eq (Box a)");
    let InstanceBody::Derived(word) = &derived.body else {
        panic!("the word derived, got {:?}", derived.body);
    };
    assert_eq!(text_of(EXAMPLE, *word), "derived");
}

#[test]
fn a_class_member_keeps_its_context_and_its_unsafe_marker() {
    let source = indoc::indoc! {"
        module Example exposing ()

        class Comparable a where
          compare : Eq b => a -> b -> Order
          unsafe lt : a -> a -> Bool
    "};
    let module = parse_ok(source);
    let class = the_class(&module);

    let signatures: Vec<_> = class
        .members
        .iter()
        .map(|member| match member {
            ClassMember::Signature(signature) => signature,
            other => panic!("a signature: {:?}", other),
        })
        .collect();
    assert_eq!(constraints(&signatures[0].context), vec!["Eq b"]);
    assert!(!signatures[0].marked_unsafe);
    assert!(signatures[1].marked_unsafe);
}

#[test]
fn a_where_with_nothing_under_it_parses_as_an_empty_body() {
    let source = "module Example exposing ()\n\nclass C a where\n\ninstance C Int where\n";
    let module = parse_ok(source);

    assert!(the_class(&module).members.is_empty());
    assert!(matches!(
        &instance(&module, 0).body,
        InstanceBody::Bindings(bindings) if bindings.is_empty()
    ));
}

#[test]
fn a_declaration_after_a_class_still_parses() {
    let source = indoc::indoc! {"
        module Example exposing ()

        class C a where
          m : a -> Int
          n : a -> Int

        f = 1
    "};
    let module = parse_ok(source);

    assert_eq!(the_class(&module).members.len(), 2);
    assert_eq!(module.functions.len(), 1);
}

#[test]
fn a_class_and_an_instance_are_not_ordinary_names() {
    for (source, word) in [
        ("module Example exposing ()\n\nclass : Int\n", "class"),
        ("module Example exposing ()\n\ninstance = 1\n", "instance"),
    ] {
        assert!(
            matches!(parse(source), Err(Error::UnexpectedToken { .. })),
            "`{}` as a value name must not parse: {:?}",
            word,
            parse(source)
        );
    }
}

#[test]
fn where_is_not_a_type_variable() {
    let source = "module Example exposing ()\n\ntype Box where = Box where\n";

    assert!(
        matches!(parse(source), Err(Error::UnexpectedToken { .. })),
        "{:?}",
        parse(source)
    );
}

/// `where` is an ordinary name everywhere a value is named, and `derived` is one everywhere
/// at all. Each source is a declaration the compiler accepted before the words were
/// reserved.
#[test]
fn where_and_derived_are_names_outside_a_body() {
    for source in [
        "module Example exposing ()\n\nwhere : Int\nwhere = 1\n",
        "module Example exposing ()\n\nf where = where\n",
        "module Example exposing (where)\n\nwhere = 1\n",
        "module Example exposing ()\n\nderived = 5\n",
        "module Example exposing ()\n\nderived : Int\nderived = 5\n",
        "module Example exposing ()\n\ng derived = derived\n",
        "module Example exposing ()\n\ntype Box derived = Box derived\n",
    ] {
        parse_ok(source);
    }
}

#[test]
fn a_member_may_be_named_derived() {
    let source = indoc::indoc! {"
        module Example exposing ()

        class C a where
          derived : a -> Int
          other : a -> Int
    "};
    let module = parse_ok(source);
    let names: Vec<_> = the_class(&module)
        .members
        .iter()
        .map(|member| match member {
            ClassMember::Signature(signature) => signature.name.clone(),
            other => panic!("a signature: {:?}", other),
        })
        .collect();

    assert_eq!(names, vec![Name::new("derived"), Name::new("other")]);
}

#[test]
fn an_instance_body_cannot_mix_derived_with_a_binding() {
    for body in [
        "  derived\n  eq a b =\n    True\n",
        "  eq a b =\n    True\n  derived\n",
    ] {
        let source = format!(
            "module Example exposing ()\n\ninstance Eq A where\n{}",
            body
        );

        assert!(
            matches!(parse(&source), Err(Error::UnexpectedToken { .. })),
            "{:?}",
            parse(&source)
        );
    }
}

#[test]
fn a_binding_named_derived_is_not_the_request() {
    let source = "module Example exposing ()\n\ninstance C A where\n  derived a =\n    1\n";
    let module = parse_ok(source);

    let InstanceBody::Bindings(bindings) = &instance(&module, 0).body else {
        panic!("a body of bindings");
    };
    assert_eq!(bindings[0].name, Name::new("derived"));
}

/// A derivation with no binding parses, as an empty body does. Naming the missing
/// binding is canonicalization's.
///
/// Verified to fail by turning the derivation's `*` in `ClassMember` back into `+`.
#[test]
fn a_derivation_with_no_binding_parses_as_empty() {
    let source = indoc::indoc! {"
        module Example exposing ()

        class C a where
          derived compare
          other : a -> Int
    "};
    let module = parse_ok(source);

    let members = &module.classes[0].members;
    assert_eq!(members.len(), 2);
    let ClassMember::Derivation(derivation) = &members[0] else {
        panic!("a derivation, got {:?}", members[0]);
    };
    assert_eq!(derivation.member, Name::new("compare"));
    assert!(derivation.bindings.is_empty());
    assert!(matches!(&members[1], ClassMember::Signature(_)));
}
