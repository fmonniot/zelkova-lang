# SPEC-15 · Nothing says what an effect is, so `main`'s type and what a test is are both undesigned

**Sizing:** large. One design decision — what a value describing an effect *is* — and then prose
in two chapters and one appendix section that currently cannot be finished. No compiler change:
the mechanism this specifies has no implementation and will not get one here.

**Location:** [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md)'s *Open
questions*, first entry; [`docs/spec/packages.md`](../spec/packages.md)'s *Open questions*, both
entries; [`docs/spec/toolchain.md`](../spec/toolchain.md)'s *Running a package's tests*, whose
**Provisional:** paragraph says in as many words that it cannot be finished until this is
answered.

**Problem:** three open questions in two chapters are one question asked three times, and each
of the three says so.

[Evaluation semantics](../spec/evaluation-semantics.md#open-questions) asks how a program
reaches the outside world:

> Everything above describes a pure computation, and a program that only computes is not much
> of a program. The mechanism — what a value that describes an effect is, how one is run, how
> results come back — is undesigned, and it is the same design that [what type `main` must
> have] waits on.

[Packages](../spec/packages.md#open-questions) asks what type `main` must have, and then asks
what a test is — "It waits on the same design `main`'s type waits on, since both are a value
the outside world picks up and acts on."

Left as three prose entries, the design nobody has done is invisible in the ticket index, and
it is not a small one: it is the difference between a language that computes and a language
that runs. It also blocks work that is otherwise ready.
[`LANG-15`](lang-15.md) — the `tests/` root — explicitly carves the runner out of its own scope
and points here, and [`docs/spec/toolchain.md`](../spec/toolchain.md)'s *Running a package's
tests* is a **Provisional:** paragraph that says what it can and admits the rest waits.

**Filed as one ticket rather than three** because the two dependent questions are not variations
on the first: `main`'s type is a consequence of what an effect value is, and a test's shape is a
consequence of `main`'s. Three tickets would each block on the other two.

**The question to settle:** what a value that describes an effect is, and how one is run. The
shape of the answer decides the other two, so it comes first. Some of what the answer has to
cover:

- Whether an effect is a value of an ordinary type declared in `std/core` — Elm's `Cmd`/`Sub`
  shape, where a program hands the runtime a description and the runtime performs it — or
  something the language knows about.
- Whether an effect type is parameterised over the value it produces, which is what makes
  sequencing one after another expressible, and what that costs given
  [Type classes](../spec/type-classes.md)' deliberate lack of higher-kinded variables. A monadic
  interface is not available under that restriction, and this is the first place the language
  needs one; whether the answer is a different shape or a different restriction is the crux.
- How results come back, given that Zelkova has no way to write a callback into the runtime
  other than a function value.
- Whether the boundary is [JS interop](../spec/js-interop.md)'s — an effect is ultimately a
  JavaScript call, and the facade mechanism already exists — or a second boundary beside it.
  [SPEC-18](spec-18.md) is nearby: whatever crosses into an effect has to be something the
  boundary permits.

Only once that is settled can *What type `main` must have* say anything, and only then can
*What a test is* choose between an exposed value of a particular type, a naming convention, and
a declaration form the language does not have.

**Approach:** follow `write-spec-chapter`, at the scale of several sections rather than a
chapter. Settle the design with the language owner first — it is the whole ticket. Then:

1. [Evaluation semantics](../spec/evaluation-semantics.md) gains a section saying what an effect
   value is and what running one means, with blocks tagged `expect=unimplemented`, and its first
   open question is deleted.
2. [Packages](../spec/packages.md)' *`main`* section says what type the named module's `main`
   must have, and its first open question is deleted.
3. [Packages](../spec/packages.md)' *Tests* section says what makes a declaration under `tests/`
   something a runner runs, and its second open question is deleted.
4. [The toolchain](../spec/toolchain.md)'s *Running a package's tests* loses the **Provisional:**
   hedge about what the runner finds, keeping the three claims it already makes that do not
   depend on the answer.

**What this is not.** Do not answer it by making `main` a value of an arbitrary type that the
compiler prints. That defers the question rather than settling it, and it makes the first real
effect a breaking change to every program. Equally, do not specify a runner's command-line
interface or its output here — that is
[toolchain](../spec/toolchain.md#running-a-packages-tests)'s, and it is an appendix for the
reason appendices exist.

**Acceptance:** `docs/spec/` says what a value describing an effect is, what type `main` must
have, and what makes a declaration under `tests/` a test. None of those three open questions
remains in any chapter's *Open questions* section, and
[`docs/spec/toolchain.md`](../spec/toolchain.md)'s *Running a package's tests* no longer says it
cannot be finished. `cargo test --test spec` green, with every new block tagged
`expect=unimplemented` and proven to fail.

**No block holds this to account** beyond the new blocks' `expect=unimplemented` tags, which say
only that the syntax does not parse. Whether an effect *runs* correctly is a claim about a
runtime nothing in the harness reaches ([`TEST-2`](test-2.md)), and about a code generator that
does not exist ([`GEN-1`](gen-1.md)).

**Sequencing:** blocks the runner half of [`LANG-15`](lang-15.md), and blocks
[`LANG-13`](lang-13.md) only in the sense that `main`'s manifest field cannot be validated
against a type nobody has chosen — reading the field does not wait on this. Not a prerequisite
for [`GEN-1`](gen-1.md), but a code generator that lands first will have to be revisited by it.

**Found:** while auditing `docs/spec/` for open questions with no ticket attached, on 2026-09-04.
