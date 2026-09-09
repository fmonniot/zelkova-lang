# LANG-43 · A facade signature may name any type at all, including ones no runtime predicate can decide

**Sizing:** medium. The check itself is a walk over one `canonical::Type` and a new `Error`
variant; what makes it medium is that landing it stops `std/core` compiling until twelve
signatures are rewritten, and that rewrite is part of this ticket.

**Location:** `src/compiler/canonical/mod.rs` — the `if source.binding_javascript` branch of
`canonicalize`, at the `Type::from_parser_type(&env, tpe)?` call that resolves a facade's
annotation; the `Error` enum in the same file. Then `std/core/src/Js/Basics.zel` and
`std/core/src/Js/Utils.zel`, plus whatever in `std/core/src/Basics.zel` re-exports them.

**Problem:** [JS interop](../spec/js-interop.md#which-types-may-cross-the-boundary) settles which
types a `module javascript` facade signature may name. A type is admitted exactly when the
compiler can emit a predicate that decides, from a value alone, whether that value belongs to
it — which admits the primitives, tuples, records, lists and union types applied to admitted
types, and rejects the two forms that have no such predicate: a **type variable** and a
**function type**, the latter wherever it appears rather than only at the top level.

The compiler enforces none of it. The facade branch of `canonicalize` checks three things — no
infix declarations, no type declarations, and every value carrying an annotation and no
bindings — and then resolves the annotation with `Type::from_parser_type`, which accepts any type
that names something in scope. Every type a normal module may write, a facade may write.

Two blocks in that chapter are tagged `expect=ok` for exactly this reason, both in
[What a facade signature may not name](../spec/js-interop.md#what-a-facade-signature-may-not-name)
and covered by the one **Known gap:** paragraph that follows them and names this ticket:

- `equal : a -> a -> Bool`, a facade over a bare type variable.
- `count : (Int -> Bool) -> Int -> Int`, a facade taking a function.

Both go **red** when this lands, and that **Known gap:** paragraph is deleted in the same diff.

**Approach:**

1. Add a `canonical::Error` variant — `FacadeTypeNotAdmitted` or similar — carrying the offending
   annotation's span and the value's `Name`. The span is `function.annotation_span`, already in
   hand at the call site; a canonical `Type` carries no span of its own (`CLAUDE.md`'s *An error
   has to describe itself*, and `Type::from_parser_type`'s own doc comment), so the annotation is
   the finest caret available without first teaching that conversion to keep per-node spans. Its
   `message()` names the form that was rejected — a type variable, a function type — in the
   vocabulary of the source, and a `notes()` line points at the rule.
2. Walk the resolved `Type` in the facade branch, after `from_parser_type`. Strip the signature's
   top-level `Arrow`s — a facade *is* a function, so the arrows separating its parameters from
   its result are the one place an arrow is admitted — and reject `Type::Variable` and
   `Type::Arrow` anywhere in what is left. `Type::Type` and `Type::Tuple` recurse into their
   arguments. A facade constant has no top-level arrow to strip and is walked whole.
3. Push onto the branch's existing error accumulation rather than returning early: one bad
   signature must not hide the next, per `CLAUDE.md`'s *A pass that emitted an error must not
   report success*.
4. Rewrite the `std/core` facades this rejects, which is the reason `cargo run` goes red
   otherwise. Twelve signatures, every one of them declared over a bare `a`: `add`, `sub`, `mul`
   and `pow` in `Js/Basics.zel`, and all eight of `Js/Utils.zel` — `equal`, `notEqual`, `lt`,
   `le`, `gt`, `ge`, `compare` and `append`. Each becomes one monomorphic facade per type its
   JavaScript really handles — `addInt : Int -> Int -> Int` beside `addFloat : Float -> Float ->
   Float` — with `Basics.zel`, which re-exports several of them, calling the right one. `cargo
   run` must still print `parsed 8 modules` and list all eight when this is done.

**Tests:** `tests/compiler/canonical.rs` for the new error — a facade over `a -> a`, a facade over
`(Int -> Int) -> Int`, and a facade over an admitted tuple that is accepted. Neutralise the walk
and confirm each goes red before trusting it (`CLAUDE.md`'s *A green test proves nothing until
you have seen it fail*).

**Interactions:**

- **[`BUG-20`](bug-20.md)** is the worked consequence of the gap this closes, and this ticket does
  not close it. `BUG-20` is `Js.Utils`'s six comparison and append facades declaring a type their
  JavaScript cannot honour, and its acceptance is about `_Utils_cmp` and `append` failing loudly
  rather than returning nonsense. Step 4 above rewrites those signatures, which removes the
  particular over-promise `BUG-20` describes — but the runtime half of that ticket, the `.mjs`
  rejecting a value it cannot compare, is untouched and is what its acceptance names. Expect to
  read `BUG-20` while doing step 4; do not mark it done.
- **[`BUG-17`](bug-17.md)** caps what step 2 can check. A type application's arguments are
  discarded when its head resolves, so `Maybe (a -> a)` reaches the walk as a bare `Maybe` and
  its inadmissible argument is invisible. The walk is still correct for everything it can see;
  the union-argument half of the rule only becomes enforceable once `BUG-17` lands.
- **[`BUG-16`](bug-16.md)** means an unresolved type name is invented rather than reported, so a
  facade naming a type that does not exist passes this check as an admitted `Type::Type`. That is
  `BUG-16`'s to fix, not this ticket's, and neither blocks the other.
- **[`DEC-12`](../decisions/dec-12.md) decision 1** adds a rule about the *shape* of a facade's
  result, which is a different check from the admitted-types walk and belongs beside it. An
  [effectful facade](../spec/js-interop.md#an-effectful-facade) must declare
  `Task (Result Failure a)` and may declare no other `Task`, so step 2 gains a case on the result
  after the arrows are stripped: a result headed by `Task` is admitted only in that one shape, and
  `a` is what the walk then descends into, `Task` and the `Result` never crossing the boundary.
  This half cannot be tested until [`LANG-9`](lang-9.md) lands — a type argument must be a bare
  name today, so `Task (Result Failure String)` does not parse and no fixture can be written for
  it. Land the type-variable and function-type halves without waiting; sequence this one after
  `LANG-9`.
- **[`LANG-37`](lang-37.md)** adds constraint syntax. A facade may not carry a constraint either
  ([What a facade signature may not name](../spec/js-interop.md#what-a-facade-signature-may-not-name));
  that is a separate rejection on a separate
  node and belongs with the ticket that makes the syntax parse.

**Found:** filed by `SPEC-18`, which wrote the chapter section that decided the rule. Not
implemented there, because a spec change and a semantics change do not share a diff
([conventions](../spec/conventions.md#a-spec-change-and-a-semantics-change-do-not-share-a-diff)).

**Acceptance:** a `module javascript` facade whose signature names a type variable or a function
type is rejected with a diagnostic whose caret sits under that annotation. The two `expect=ok`
blocks named above are retagged and their **Known gap:** paragraph deleted, and `cargo test
--test spec` is green. `cargo run` prints `parsed 8 modules` and lists all eight.
