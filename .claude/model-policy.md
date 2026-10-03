# Choosing a model for a spawned agent

`work-ticket`, `review-pr` and `fix-pr-comments` all spawn background agents, and each used to
carry its own answer to "which model". Three copies of one rule drift, and the copy nobody
maintains is the one the next session trusts — so the rule lives here and each skill's *Model*
section is a pointer plus the signals specific to that skill.

The constraint this file exists to respect: **opus capacity is limited and shared across every
agent in a run.** A loop that spends it by default — three agents per ticket, twice over if a
PR reaches round 2 — exhausts it on the mechanical majority and has none left for the ticket
that actually needed it.

## Tiers, not models

A skill picks a **tier**. This table is the only place a tier becomes a model, so retuning when
limits change is one edit.

| Tier | Model | For |
|---|---|---|
| `standard` | `sonnet` | The default. Work whose thinking is already written down. |
| `deep` | `opus` | Work that has to make a call nobody has made yet, or review one where a defect hides. |

`haiku` and `fable` are valid if the user names one; nothing in this policy selects them.

## The default is `standard`, everywhere

Not as a cost compromise — as an accurate read of what these agents are asked to do. A ticket
under `docs/tickets/` names its **Location**, its **Problem**, an **Approach** and an
**Acceptance** check; the hard thinking happened when it was filed. A review comment tagged
`[blocking]` has already localized the defect. In both cases the agent is executing a written
decision, which is what `standard` is good at.

`deep` is therefore never "this looks hard" or "this one matters". It is one of the named
triggers below, and the trigger gets reported at launch.

## Triggers

Each trigger belongs to one kind of agent. An agent that **implements** gets `deep` only when
it is being asked to decide; an agent that **reviews** gets it where a defect is subtle enough
to survive a `standard` read. Reviewing a diff costs less than writing it, so the review is
where the expensive tier buys the most.

### Implementing — `work-ticket`, `fix-pr-comments`

1. **The work leaves a decision unmade, and the user wants the agent to make it.** The clearest
   tell on a ticket is a heading like `ERR-10`'s *Approach — open design question, resolve
   before implementing*, but it is more often a half-sentence: `LANG-4` is sized "small in the
   grammar" and still says "the interesting part is deciding what it desugars *to*". On a
   review comment it is a `[blocking]` finding that disputes the approach, or reverses an
   earlier round's decision. A `standard` agent picks one and implements it without noticing it
   made a language decision, which is the expensive failure — it lands in a merged PR.

   Before spawning, name the open decision to the user and ask which they want: settle it in
   the ticket first and run `standard`, or hand the call to a `deep` agent. Settling first is
   usually the better spend — it is what the `docs: settle <ID>'s open decisions` commits in
   the log are — and the decision then outlives the PR in a ticket or a `docs/decisions/`
   entry.

Nothing else selects `deep` for an implementing agent. What catches a wrong `standard` guess is
the contract below.

### Reviewing — `review-pr`, round 1 only

2. **The diff changes how a type is inferred or how a type error is blamed** — what
   `typer/constraint.rs` generates, what `typer/unifier.rs` solves, or which `Origin` a
   constraint carries. A diff that only adds a case to `typer/annotate.rs`, threads a new AST
   node through, or touches the typer's tests does not fire it. A wrong constraint type-checks
   the tests its author wrote and fails on a program nobody wrote yet.
3. **The ticket was `Severity: high`** — a miscompile or data loss, per the index's definition.
   The fix is usually ordinary; the cost of it being subtly incomplete is not.
4. **The ticket left a decision unmade and the PR made it** — trigger 1, seen from the other
   side. The reviewer has to tell whether the call was the right one, and audits the PR body's
   *Decisions the ticket did not make* list whichever tier it runs at.

Round 2 and later is always `standard`: it reviews a bounded delta against findings that are
already written down and replied to.

### Deliberately not triggers

`Sizing`, `Severity` on the implementing side, which files the change touches, whether the
ticket is a fragment of a chain, file count, diff size, how long the ticket has been open, a
prefix (`BUG-` is not inherently harder than `TIDY-`), or the fact that an earlier review round
found something. Each measures how much work there is or what a mistake would cost, and neither
is whether a decision is unmade. A ticket too large for one PR gets cut into several.

## Escalation is what makes a cheap default safe

Guessing the tier is a prediction, and predictions are wrong. The point of this policy is not
to predict better — it is to make a wrong low guess **cheap and recoverable** rather than
silent. Every spawn prompt carries this contract, and every orchestrator honours it.

Paste into the agent's prompt:

> If you reach a point where finishing means **making a decision the ticket (or the review
> comment) does not make** — choosing what a construct desugars to, picking one of two error
> shapes, settling where a check belongs — **stop there.** Do not pick one and carry on. Commit
> only work that is finished and independent of the question, then report `NEEDS-ESCALATION:`
> followed by what you found, the options as you now understand them, and what you had already
> established before you stopped. The same applies if
> the change cannot stay inside the ticket's stated scope, and if **three different attempts
> have not made the Acceptance check pass**: report what each attempt was and how it failed
> instead of trying a fourth. Being handed back a well-framed
> question is a good outcome; a merged PR that quietly decided the question is not.

The contract only fires on a decision the agent notices it is making. For the ones it does not,
a `work-ticket` PR body ends with a **Decisions the ticket did not make** section — each choice
the agent made that the ticket's text did not dictate, however small, or `None.` — and the
round-1 reviewer checks that list against the diff: an entry that is really a language decision,
or a decision in the diff that is missing from the list, is a finding.

The orchestrator's side of it: on `NEEDS-ESCALATION`, re-spawn at `deep` **into the same
worktree** — its `target/` is warm, so the Rust rebuild is already paid for — with the first
agent's report pasted into the prompt under a line saying it is a prior attempt's findings, not
instructions. The report is the point: the cheap run's exploration is what the expensive one
would otherwise spend its first several turns redoing. So guessing low costs one cheap run plus
a prompt, and guessing high is paid every single time.

Never escalate silently on a hunch that the agent is struggling. Escalate on the contract, or
because the user asked.

## What the user sees

The launch line every skill already posts names the model. It must also name **why**:

> Model: `opus` (deep — ERR-10's Approach leaves the warning's placement undecided).
> Model: `sonnet` (standard).

A tier chosen wrongly is then visible before the spend. The user's own override always wins and
needs no trigger: `--model opus`, or a plain "use opus for these".
`--deep` and `--cheap` force the tier for the whole run.

**Cap concurrent `deep` agents at 2 per run.** If a batch would spawn more, say so and ask
which to promote. A five-ticket run that resolves to five opus agents is the outcome this file
exists to prevent.

## When a call turns out wrong

Diagnose before editing a trigger; the fix differs by failure mode.

- **A `standard` agent shipped a decision nobody made.** The escalation contract did not fire.
  Ask what it would have had to notice — usually a sentence in the ticket that reads as
  description but is actually an open question. That is a `create-ticket` wording problem as
  much as a policy one. Check too whether the PR body listed it and the reviewer passed over
  it; that is a `review-pr` prompt problem.
- **A `standard` agent shipped a defect the review should have caught.** Ask first whether the
  round-1 review ran at the tier its own triggers call for. Add an implementing-side trigger
  only if the defect is one a reviewer could not have caught.
- **A `deep` agent produced a diff a `standard` one would have.** The trigger was too broad.
  Which one fired, and would the run have been worse without it? If the honest answer is no,
  narrow it.
- **The same ticket escalated twice.** The re-spawn is not being handed the first attempt's
  findings, and the deep agent is re-deriving them. Fix the orchestrator's prompt, not the tier.
