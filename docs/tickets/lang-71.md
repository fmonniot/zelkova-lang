# LANG-71 · A constraint context of four or more constraints does not parse

**Sizing:** small-to-medium. One production and a change of shape on the parser AST. What could
make it bigger: the shape change reaches every reader of `FunType::context`, and there are more
of those in the tests than in the source.

**Location:** `crates/zelkova-syntax/src/parser/grammar.lalrpop` — `ConstrainedType`, and
`AtomicType`, whose tuple productions stop at three elements;
`crates/zelkova-syntax/src/parser/mod.rs` — `FunType::context`, `FunType::assemble`,
`Function::context` and `Module::from_declarations`, which copies one onto the other;
`crates/zelkova-compiler/src/canonical/mod.rs` — `validate_context`, whose doc comment says a
list of two or three is the largest shape that reaches it, and the loop over `source.functions`
in `canonicalize_recovering` that calls it.

**Depends on:** none. **It is the first ticket of [the type-class
order](README.md#active-work-type-classes)**, ahead of [LANG-38](lang-38.md): a class head and
an instance head parse a context too, and they should be written against the shape this ticket
leaves, not the one it replaces.

**Decided ([`DEC-24` decision
1](../decisions/dec-24.md#1--a-context-holds-any-number-of-constraints), by the language
owner):** a context holds any number of constraints. The cap at three was inherited from tuple
types and was never a rule about contexts. Tuple types keep their limit (`CLAUDE.md`, *Tuples
are size 2 or 3 only*).

**Problem:** [`docs/spec/type-classes.md`](../spec/type-classes.md) says "several constraints
are parenthesised and comma-separated" and puts no cap on the count. The compiler caps it at
three:

```zel
f : (Eq a, Eq b, Eq c, Eq d) => a -> b -> c -> d -> Bool
```

is `unexpected token: Comma` at the fourth element, expecting `)`. The cause is the one
`ConstrainedType`'s comment gives. At the `(` of `(Eq a, Eq b) =>` an LALR(1) parser cannot tell
a list of constraints from a two-tuple type, so the context is parsed *as a type*, and
`AtomicType` has a tuple production for two elements and one for three and for no other arity.

**Approach:**

1. **A production of its own for four or more**, beside the two `ConstrainedType` has:

   ```
   "(" <a:Type> "," <b:Type> "," <c:Type> "," <d:Type> <rest:("," <Type>)*> ")" "=>" <t:Type>
   ```

   **Probed on 2026-10-02 and it builds**: LALRPOP accepts it with no conflict, the existing 71
   parser tests pass unchanged, and a five-constraint annotation parses. For two and three
   elements the parser still meets the tuple productions at `)`; from the fourth `,` on, only
   this production is viable. The other two productions stay exactly as they are — a context
   of one, two or three is still read as a type first, and `Int -> Int => a` still parses and
   is still rejected by canonicalization.

2. **The context becomes a list on the parser AST.** `FunType::context` and `Function::context`
   change from `Option<Type>` to `Option<Context>`, where

   ```rust
   pub struct Context {
       /// What was written between the parentheses, in order: each a constraint that
       /// canonicalization has yet to check is shaped like one.
       pub constraints: Vec<Type>,
       /// The whole of what stands in front of `=>`, parentheses included.
       pub span: NodeSpan,
   }
   ```

   The grammar builds it: for the four-or-more production directly, and for the existing
   `<ctx:Type> "=>"` production by flattening — a `TypeKind::Tuple` becomes its elements, and
   anything else is a list of one. `None` still means no `=>` was written. The elements stay
   `Type`s: nothing tells a constraint from a type until canonicalization looks, which is
   `ConstrainedType`'s own comment and does not change.

   This is not a `Vec` standing in for a tuple, which `Tuple<T>`'s doc comment forbids: a
   context is a list of any length, and `Tuple<T>` goes on being the only representation of a
   tuple type.

3. **`validate_context` reads the list.** Its `TypeKind::Tuple(tuple) => tuple.iter()` unpacking
   goes, since the grammar has already done it, and its `InvalidConstraintKind::Tuple` arm stays:
   a tuple *inside* the list — `((Eq a, Eq b), Eq c) =>` — is still a tuple written where a
   constraint belongs. `Error::FacadeConstrained` keeps pointing at the whole context, which is
   what `Context::span` is for. Rewrite the function's doc comment, which describes the cap.

4. `parser::Function::context`'s doc comment says it is "always `None` when `tpe` is". That
   invariant is kept by a comment and not by a type, and stays that way here: turning the pair
   into one `Annotation` value is a wider refactor of `Function` than this ticket is for.

**Acceptance:**

- A test in `crates/zelkova-syntax/tests/parser/types.rs` asserts a four-constraint and a
  five-constraint annotation reach `FunType::context` as four and five constraints, each with
  its own span. Seen red with the new production removed.
- The two existing context tests in that file (`type_annotation_single_constraint`,
  `type_annotation_constraint_list`) are rewritten against the new shape and assert one and two
  constraints.
- A test in `crates/zelkova-compiler/tests/canonical.rs` puts an `InvalidConstraint` caret under
  the fourth of four when it is malformed, asserted on `diagnostic.labels[..].range`.
- `((Eq a, Eq b), Eq c) => …` is `InvalidConstraint` with `InvalidConstraintKind::Tuple`.
- In [`docs/spec/type-classes.md`](../spec/type-classes.md), the four-constraint block under
  *Constraining an annotation* goes red and is retagged `expect=ok`, and its `**Known gap:**`
  paragraph is deleted. The sentence under *A constraint belongs to a signature, not to a type*
  saying a constrained annotation "is read as a type first" stays true for one to three
  constraints; leave it.
- `cargo test --workspace` is green. `cargo run -- compile std/core` still prints
  `parsed 10 modules`, lists all ten as checked and exits 0.
