# SPEC-26 · Design rationale has nowhere to live, so it is kept in three unrelated places or lost

**Sizing:** small-to-medium. The mechanism is a directory and a convention, which is small. What
makes it bigger is deciding where the boundary with `docs/spec/` and `docs/tickets/` falls, and
that boundary is the whole point of the ticket.

**Location:** a new track, plus the three places currently standing in for it —
[`docs/spec/README.md`](../spec/README.md)'s *Appendices* section,
[`docs/spec/deriving-elsewhere.md`](../spec/deriving-elsewhere.md), and the module doc comment at
the head of `tests/spec.rs`. Also [`docs/tickets/README.md`](README.md)'s closing convention,
which names the destinations a dying ticket's narrative may be promoted to.

**Problem:** this repository has three kinds of long-lived document — `docs/spec/` (what the
language *is*, normative and checked), `docs/tickets/` (what is *to be done*, deleted on
completion), and `CLAUDE.md` (rules that constrain a diff). None of them is *why a rule is what
it is*, and that fourth kind exists whether or not there is a place for it. Three instances, all
present in the tree today:

**One — the appendix says so itself.** [`docs/spec/README.md`](../spec/README.md)'s *Appendices*
section, describing [Deriving in other languages](../spec/deriving-elsewhere.md), ends:

> It is design rationale rather than specification, which is why it is not normative and why no
> claim in it constrains a program. A dedicated record for decisions of that kind would be the
> better home; until one exists this is where they go.

A document that explains its own filing is a document in the wrong place.
[`docs/spec/conventions.md`](../spec/conventions.md)'s *Chapter or appendix* test does not
classify it either — it splits on "a rule that decides what a program means" versus "a rule about
how bytes arrive, where they are kept, or what a command prints", and rationale is neither.

**Two — a numbered decision list is cited five times and does not exist.** `SPEC-12`'s design
session produced numbered decisions, and four live files cite them by number:

```
docs/tickets/class-1.md:95  **A `module javascript` facade may not carry a constraint** (`SPEC-12` decision 6).
docs/tickets/class-4.md:47  decision 5 — no higher-kinded variables — is what makes this simple
docs/tickets/class-6.md:32  **The shape decision 6 forces.** A facade may not be constrained
docs/tickets/gen-1.md:56    ([`docs/spec/type-classes.md`](../spec/type-classes.md), decision 7)
docs/tickets/README.md:184  **What is not a ticket:** dictionary erasure. `SPEC-12` decision 7 settles …
```

`docs/tickets/spec-12.md` was deleted when the ticket closed, per this directory's own
convention. `grep -n "decision [0-9]" docs/spec/type-classes.md` returns nothing: the chapter has
never carried numbered decisions. So every one of those five citations resolves nowhere, and
`gen-1.md`'s is the worst of them because it names a file that *does* exist and attributes to it
a numbering it has never had. A reader chasing "decision 6" has `git show <sha>^:docs/tickets/spec-12.md`
as their only route, and no reason to guess it.

This is not an argument against the closing convention, which is right for the reason it gives —
a closed ticket's implementation narrative describes a tree that no longer exists. It is an
argument that the convention names only two promotion destinations, "into the code as a doc
comment where it explains behaviour, or into `CLAUDE.md`'s *Standing invariants* where it is a
rule", and a decision *list* is neither. It was promoted nowhere and the citations outlived it.

**Three — rationale about documentation policy is in a Rust test binary.** `SPEC-23` weighed
whether the spec harness should check `../tickets/*.md` citations at all, and the reasoning is a
fifty-line module doc comment at the head of `tests/spec.rs` ("Two scope decisions, both of which
the ticket that asked for this (`SPEC-23`) left open on purpose…"). That is a good piece of
writing in an odd place: it is about a convention for markdown files, and it is readable only by
someone who has opened a Rust file to find out why a test exists.

**What the harness does to a rationale document kept where it is.** `tests/spec.rs`'s
`load_chapters` reads every `*.md` directly under `docs/spec/`, with no chapter/appendix
distinction, so `deriving-elsewhere.md` is held to the full set: every ```` ```zel ```` block
carries an `expect=` tag, every relative link and anchor resolves, and the liveness assertion in
`spec_cross_references_resolve` requires it to write at least one header *and* at least one link
or the run fails. Whether that is a tax or a feature is one of the open questions below — it
keeps a rationale document's cross-references honest, and it also means a document that is
explicitly *not* normative is checked as though it were.

**The design to settle.** This ticket does not pick any of it:

- **Where.** A `docs/decisions/` sibling; a subdirectory of `docs/spec/`; or a `DEC-` prefix
  inside `docs/tickets/` with a status that never closes. The last is tempting because the
  tooling exists, and wrong for the reason the closing convention gives: everything in
  `docs/tickets/` is work, and work is finished and deleted.
- **What an entry is.** ADR-style (context, decision, consequences, status) or the freer shape
  `deriving-elsewhere.md` already has. Whether entries are numbered, and whether a number is
  stable and never reused the way a ticket ID is — `SPEC-12 decision 7` is the argument for
  stable numbering and for citing decisions by an ID that outlives its ticket.
- **Whether anything checks it.** If it lives under `docs/spec/`, `tests/spec.rs` checks it
  already and the question is whether to exempt it. If it lives elsewhere, the question is
  whether the cross-reference and anchor checks should be extended to cover it, since the
  failure mode `SPEC-23` fixed — a citation nothing keeps alive — is exactly the failure mode
  instance two above is an example of.
- **The boundary with `docs/spec/`.** A chapter states a rule and deliberately does not argue
  it. How much argument a chapter may keep, and at what point it moves, wants a sentence in
  [`docs/spec/conventions.md`](../spec/conventions.md) beside the existing *Chapter or appendix*
  test.
- **Whether the closing convention gains a third destination.** If it does, a closing ticket
  with a decision list in it has somewhere to promote to, and instance two stops recurring.

**Approach:**

1. Settle the above with the repository owner. The `docs/decisions/` directory with stable
   numbering is the obvious default; it is not the only one and this ticket does not choose it.
2. Create the track with its own README, in the shape `docs/tickets/README.md` and
   `docs/spec/README.md` both use: what belongs here, what does not, and the conventions.
3. Move [`docs/spec/deriving-elsewhere.md`](../spec/deriving-elsewhere.md) into it as the first
   entry, and update the two links into it — `docs/spec/README.md`'s appendix row and prose, and
   [`docs/tickets/spec-25.md`](spec-25.md)'s *Found* paragraph. `docs/spec/README.md`'s
   *Appendices* section returns to one appendix and the "until one exists" paragraph goes.
4. Recover `SPEC-12`'s decision list (`git show <sha>^:docs/tickets/spec-12.md`, found via
   `git log --oneline --diff-filter=D -- docs/tickets/spec-12.md`) and file it as the second
   entry, so the five citations above resolve. Then fix `gen-1.md`'s, which additionally names
   the wrong file.
5. Decide whether `tests/spec.rs`'s header rationale moves or stays. It is *about* the harness
   and reasonable where it is; the ticket should say which, not leave it.
6. Add the boundary sentence to [`docs/spec/conventions.md`](../spec/conventions.md), and the
   third promotion destination to [`docs/tickets/README.md`](README.md)'s closing convention if
   step 1 decided on one.

**What this is not.** Not a general documentation reorganisation, and not a reason to move
`CLAUDE.md`'s *Standing invariants* — those are rules that constrain a diff, which is a fourth
thing again and is correctly where it is. Not a licence to write a decision record for every
choice: the test is whether a later reader would otherwise re-open the question, which is true
of `SPEC-12`'s seven and of the deriving survey, and false of most commits.

**Acceptance:** a design-decision track exists with a README stating what belongs in it, and
holds at least the deriving survey and `SPEC-12`'s decision list. `grep -rn "decision [0-9]"
docs/` resolves — every numbered citation names a document that exists and carries that number.
`docs/spec/README.md` no longer says a better home would exist, and its *Appendices* section
describes only appendices. `cargo test --test spec` green: if the track lives under
`docs/spec/`, its entries satisfy the harness; if it does not, no chapter links into it in a way
that breaks `spec_cross_references_resolve`.

**Found:** while surveying how Haskell, Clean, Scala 3, Rust, OCaml, PureScript, Lean, C++ and
Elm derive instances, in order to judge the mechanism
[Type classes](../spec/type-classes.md#a-class-says-how-it-is-derived) had just settled, on
2026-09-06. The survey was worth keeping and had nowhere to go; the appendix it went into was
agreed at the time to be a stopgap, and this is the ticket for the thing it is standing in for.
Instance two was found while grounding this ticket rather than being the reason for it.
