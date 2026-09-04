# SPEC-16 · The spec makes one promise about space and does not say whether it makes others

**Sizing:** small-to-medium. Prose in one chapter, and a design question whose answer is cheap to
write and expensive to get wrong, because it constrains a code generator that does not exist yet.

**Location:** [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md) —
*Recursion and tail calls*, which makes the one promise, and *Open questions*, third entry.

**Problem:** [Evaluation semantics](../spec/evaluation-semantics.md)' *Recursion and tail calls*
promises that a self tail call runs in constant stack, and defines tail position precisely enough
to hold a code generator to. That is the only claim the language makes about memory, and the
chapter's own open question is whether it is the only one:

> The tail-call rule is the one promise here about memory. Whether a program can rely on anything
> more — that a value is not copied, that a partially applied function is not rebuilt per call —
> is unanswered, and each answer constrains a code generator that does not exist.

A language that says nothing about space is not thereby neutral. A reader who writes a loop over
a large structure needs to know whether the obvious spelling allocates once or once per
iteration, and with nothing written down they will find out empirically against whatever
[`GEN-1`](gen-1.md) happens to emit — at which point the first generated backend's incidental
behaviour becomes the de facto specification, and the second one is a breaking change.

The question is cheapest to answer now, before any code generator exists to be constrained by
it, which is exactly why it is worth a ticket rather than another year in a prose list.

**The question to settle:** what else, if anything, a program may rely on about space. Three
positions, and they are not points on one scale:

- **Nothing beyond the tail-call rule.** The chapter says explicitly that no other space property
  is promised, and a program that depends on one depends on an implementation. Cheapest to write,
  maximally free for a code generator, and it makes performance-sensitive code unwritable in
  the sense that it cannot be written *portably* — only against a backend.
- **Sharing is preserved.** A value is not copied when it is passed, returned or stored, so
  binding a large structure to a second name costs nothing. This is what a reader of an
  immutable-value language already assumes, and it is what makes persistent data structures
  worth having. It rules out a backend that copies on assignment, which no reasonable one does.
- **The above, plus a promise about closures** — that a partially applied function is not
  rebuilt per call, so `f x` in a loop where `f` is applied to a constant does not allocate a
  closure each time. Strongest, most useful, and the hardest to honour: it is a claim about an
  optimisation rather than about a representation, and it constrains how currying is compiled.

This ticket does not pick. The second is the likeliest floor and the third is the one worth
arguing about, but *whether the language promises an optimisation at all* is a decision about
what kind of specification this is, and it is the language owner's.

Whatever wins, the chapter also has to say what a promise about space *means* given that
[Evaluation semantics](../spec/evaluation-semantics.md) specifies no cost model — a promise that
something is "not copied" needs a notion of what a value is at runtime, and no chapter has one.

**Approach:** follow `write-spec-chapter`, at the scale of a section. Settle the question above
with the language owner first. Then:

1. *Recursion and tail calls* — or a new sibling section, if the answer is more than a sentence —
   states what a program may rely on about space, in the same register as the tail-call rule it
   sits beside: a property a reader can act on, not an aspiration.
2. The chapter says what is deliberately *not* promised, so that a reader who wants a stronger
   guarantee knows they are outside the language rather than in an unwritten part of it. This
   half matters as much as the first and is the half that a "nothing beyond the tail-call rule"
   answer consists entirely of.
3. The third open question in *Open questions* is deleted.

**What this is not.** Not a performance chapter, and not a cost model for every construct. The
question is which properties are *promises* — things a program may depend on and a backend must
honour — and the answer may be a short one. Do not specify how a code generator achieves
whatever is promised; that is [`GEN-1`](gen-1.md)'s, and writing it here would make the chapter
a description of one implementation.

**Acceptance:** [`docs/spec/evaluation-semantics.md`](../spec/evaluation-semantics.md) states
what a program may rely on about space beyond the tail-call rule, and states what it may not.
The third entry in that chapter's *Open questions* is gone. `cargo test --test spec` stays green;
no block need change, since no space property is observable through the harness.

**No block holds this to account**, and none can — the spec harness compiles and checks modules,
and observes nothing about a running program's memory. Whether a backend honours whatever this
settles is [`GEN-1`](gen-1.md)'s problem and needs a runtime the harness does not reach
([`TEST-2`](test-2.md)). This is a read-and-decide ticket.

**Sequencing:** cheaper before [`GEN-1`](gen-1.md) than after, and that is most of the argument
for doing it. A code generator written first will make these choices incidentally, and reversing
one afterwards costs a rewrite of the phase rather than a paragraph.

**Found:** while auditing `docs/spec/` for open questions with no ticket attached, on 2026-09-04.
