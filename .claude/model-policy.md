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
| `deep` | `opus` | Work that has to make a call nobody has made yet. |

`haiku` and `fable` are valid if the user names one; nothing in this policy selects them.

## The default is `standard`, everywhere

Not as a cost compromise — as an accurate read of what these agents are asked to do. A ticket
under `docs/tickets/` names its **Location**, its **Problem**, an **Approach** and an
**Acceptance** check; the hard thinking happened when it was filed. A review comment tagged
`[blocking]` has already localized the defect. In both cases the agent is executing a written
decision, which is what `standard` is good at.

`deep` is therefore never "this looks hard" or "this one matters". It is one of the named
triggers below, and the trigger gets reported at launch.

## Triggers — any one of these fires `deep`

1. **The ticket leaves a decision unmade.** The clearest tell is a heading like `ERR-10`'s
   *Approach — open design question, resolve before implementing*, but it is more often a
   half-sentence: `LANG-4` is sized "small in the grammar" and still says "the interesting part
   is deciding what it desugars *to*". A `standard` agent picks one and implements it without
   noticing it made a language decision, which is the expensive failure — it lands in a merged PR.
2. **`Sizing: large`.** `GEN-1` (a phase that does not exist) and `LANG-14` (the largest piece
   of the packages design) are the live examples.
3. **`Severity: high`** on a bug — a miscompile or data loss, per the index's definition.
4. **The change is cross-cutting by construction.** Three areas, each named in `CLAUDE.md`'s
   *Standing invariants* because getting them wrong already produced a bad diff:
   `grammar.lalrpop` + the `parser` AST + the `canonical` conversions moving together; anything
   inside `src/compiler/typer/` (`constraint.rs`, `unifier.rs`); and the error accumulation in
   `compile_package`.
5. **The ticket is a fragment that does nothing on its own** — `LANG-37` through `LANG-41`, the
   type-classes chain, where the mechanism only works once the chain lands and a locally
   sensible choice can be wrong three tickets later.

Deliberately **not** triggers: file count, diff size, how long the ticket has been open, a
prefix (`BUG-` is not inherently harder than `TIDY-`), or the fact that an earlier review round
found something.

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
> the change cannot stay inside the ticket's stated scope. Being handed back a well-framed
> question is a good outcome; a merged PR that quietly decided the question is not.

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
  much as a policy one.
- **A `deep` agent produced a diff a `standard` one would have.** The trigger was too broad.
  Which one fired, and would the run have been worse without it? If the honest answer is no,
  narrow it.
- **The same ticket escalated twice.** The re-spawn is not being handed the first attempt's
  findings, and the deep agent is re-deriving them. Fix the orchestrator's prompt, not the tier.
