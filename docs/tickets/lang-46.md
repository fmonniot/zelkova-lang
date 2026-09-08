# LANG-46 · `std/core` declares `List`, opaquely, with `(::)` over it

**Sizing:** medium. One new `.zel` module and one `infix` declaration, but it is the module the
bracket syntax is read against, so nothing else about lists can land before it.

**Location:** `std/core/src/` — a new `List.zel`. `List.ignored` sits beside it today, carrying
Elm's whole list API and an `import Elm.Kernel.List`.

**Depends on:** [`LANG-9`](lang-9.md), hard. A type argument must be a bare name, so
`Cons a (List a)` — the recursive variant this ticket exists to declare — is a syntax error
until that lands. There is no way to write the type without it.

**Decided (`SPEC-22`, by the language owner; [`DEC-7`](../decisions/dec-7.md) decisions 2 and 3):**
a list is an ordinary two-variant union type declared in `std/core`, exposed **opaquely**, and
`::` is an ordinary operator bound by an `infix` declaration rather than a reserved spelling.
[Lists](../spec/lists.md#what-a-list-is) is the rule.

**Problem:** the chapter reads `[`, `]` and `::` against a type and two constructors that no
module declares. `std/core/src/List.ignored` is Elm's module verbatim — it declares no `List`
type at all, because Elm's is kernel-provided, and `CLAUDE.md`'s *Zelkova has no `Elm.Kernel.*`*
rules that out. So the names the language's list syntax means do not exist anywhere in the tree.

**Approach:** write `std/core/src/List.zel` with the type, the cons function and the operator:

```zel
module List exposing (List, cons, (::))

type List a
  = Nil
  | Cons a (List a)

infix right 5 (::) = cons

cons : a -> List a -> List a
cons x xs =
  Cons x xs
```

`exposing (List)` and not `List(..)` is the whole of the opacity rule — a program that could
name `Cons` could observe the representation, which
[Lists](../spec/lists.md#what-a-list-is) says nothing does.

Porting the rest of `List.ignored`'s API is **not** this ticket. That file stays `.ignored`
until its functions can compile; this one adds only what the language's own syntax needs.

**Acceptance:** `std/core/src/List.zel` exists and compiles. `cargo run` prints
`parsed 9 modules` and lists all nine as checked, and `CLAUDE.md`'s baseline is updated in the
same commit — the count is the smoke test and a stale one is worse than none.
`tests/pipeline.rs::stdlib_package_compiles` covers it.

**No spec block goes red when this lands**, and that is not an oversight: a block in
`docs/spec/` canonicalizes against no interfaces at all, so no example can name `std/core`'s
`List`. The pipeline test is what holds this one to account.

**Found:** while writing [`docs/spec/lists.md`](../spec/lists.md) (`SPEC-22`).
