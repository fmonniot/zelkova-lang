//! Keeps the editor grammar's keyword lists from drifting away from the language specification.
//!
//! `editors/vscode/syntaxes/zelkova.tmLanguage.json` has to spell the reserved words out, since
//! a TextMate grammar is a data file with no way to read the spec. This test is what makes that
//! second copy cheap to keep: it fails when the grammar's `reserved-words` rule is not exactly
//! the block under *Reserved words* in `docs/spec/lexical-structure.md`.
//!
//! What it does not check is the scopes the grammar assigns. Those are asserted by the fixtures
//! under `editors/vscode/tests/`, which `vscode-tmgrammar-test` runs in CI's `javascript` job.

use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

fn read(path: &str) -> String {
    let full = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(path);
    fs::read_to_string(&full).unwrap_or_else(|e| panic!("cannot read {}: {}", full.display(), e))
}

/// The words of the unlabelled code block under `## Reserved words`.
fn spec_reserved_words() -> BTreeSet<String> {
    let spec = read("docs/spec/lexical-structure.md");
    let after = spec
        .split("\n## Reserved words\n")
        .nth(1)
        .expect("lexical-structure.md has no `## Reserved words` section");
    let mut lines = after.lines().skip_while(|l| l.trim_end() != "```");
    lines.next();
    let words: BTreeSet<String> = lines
        .take_while(|l| l.trim_end() != "```")
        .flat_map(|l| l.split_whitespace())
        .map(str::to_string)
        .collect();
    assert!(
        !words.is_empty(),
        "found no words in the Reserved words block"
    );
    words
}

/// The alternation `(a|b|c)` in the `match` of the grammar's repository rule `rule`.
fn grammar_words(rule: &str) -> BTreeSet<String> {
    let json: serde_json::Value =
        serde_json::from_str(&read("editors/vscode/syntaxes/zelkova.tmLanguage.json"))
            .expect("the grammar is not valid JSON");
    let pattern = json["repository"][rule]["match"]
        .as_str()
        .unwrap_or_else(|| panic!("repository rule `{}` has no `match` string", rule));
    // The rule is `(?<!..)(alternation)(?!..)`; the alternation follows the lookbehind.
    let start = pattern
        .find("_])(")
        .unwrap_or_else(|| panic!("`{}`'s match is not shaped `(?<!..)(a|b)(?!..)`", rule))
        + "_])(".len();
    let len = pattern[start..]
        .find(')')
        .unwrap_or_else(|| panic!("`{}`'s alternation is not closed", rule));
    pattern[start..start + len]
        .split('|')
        .map(str::to_string)
        .collect()
}

#[test]
fn reserved_words_are_the_specs() {
    let spec = spec_reserved_words();
    let grammar = grammar_words("reserved-words");
    let missing: Vec<_> = spec.difference(&grammar).collect();
    let extra: Vec<_> = grammar.difference(&spec).collect();
    assert!(
        missing.is_empty() && extra.is_empty(),
        "the grammar's `reserved-words` rule has drifted from the spec's Reserved words block: \
         missing from the grammar {:?}, in the grammar but not reserved by the spec {:?}",
        missing,
        extra
    );
}

#[test]
fn true_and_false_are_not_keywords() {
    // `True` and `False` are constructors of `Basics.Bool`; the lowercase spellings are
    // ordinary identifiers.
    let grammar = grammar_words("reserved-words");
    for word in ["true", "false"] {
        assert!(
            !grammar.contains(word),
            "`{}` is highlighted as a keyword",
            word
        );
    }
}
