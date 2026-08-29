---
name: review-pr
description: Spawn one agent per PR to do an adversarial review and post findings as GitHub comments, preferring inline. Use when the user says "review PR #N", "review AST-1 -> PR #M", or asks for an adversarial/independent review of one or more open PRs.
argument-hint: <PR-number | TICKET-ID -> PR-M> [...] [--model sonnet|opus|fable|haiku]
---

# Review PR

Given one or more `<TICKET-ID> -> PR-M` pairs (or bare PR numbers), spawn one agent per PR to
do an adversarial review and post the findings as GitHub comments — inline where possible.

## Input

$ARGUMENTS

Pairs like `AST-1 -> PR 120` or `BUG-2 -> PR 121`. A ticket ID is any ID in
`docs/tickets/README.md` — bugs and tasks share one namespace. Bare PR numbers are fine when
there is no associated ticket. Normalize to a list of `(ticket_id_or_none, pr_number)`.

### Model

Default model is **opus** — adversarial review benefits from the stronger model. Honor a model
the user names, globally or per PR. Valid: `sonnet`, `opus`, `fable`, `haiku`.

## Step 0 — Locate each PR's worktree, and pull the ticket

`work-ticket` leaves a persistent worktree at `.claude/worktrees/<id-lower>` for every ticket
it works on. Reuse it rather than creating a fresh checkout — a Rust rebuild from cold is the
slowest thing in this loop.

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
WT="$REPO_ROOT/.claude/worktrees/<id-lower>"
```

If a pair has no ticket ID, or `$WT` doesn't exist, find or create one from the PR's branch:

```bash
BRANCH=$(gh pr view <PR> --repo fmonniot/zelkova-lang --json headRefName -q .headRefName)
EXISTING=$(git worktree list --porcelain | awk -v b="refs/heads/$BRANCH" '/^worktree /{p=$2} /^branch /{if ($2==b) print p}')
```

Reuse `$EXISTING` if non-empty. Otherwise:

```bash
git fetch origin "$BRANCH"
git worktree add "$REPO_ROOT/.claude/worktrees/pr-<PR>" "$BRANCH"
```

Either way, refresh it to the PR's current head before handing it over:

```bash
git -C "$WT" fetch origin "$BRANCH"
git -C "$WT" checkout "$BRANCH"
git -C "$WT" merge --ff-only "origin/$BRANCH"
```

Then, for a pair with a ticket ID, pull the ticket — **from `origin/main`, not from the
branch**. A correct PR deletes `docs/tickets/<id-lower>.md` as its close-out, so on the branch
the file is already gone:

```bash
git show "origin/main:docs/tickets/<id-lower>.md"
```

Paste it into the prompt below. This is the reviewer's yardstick: a PR that compiles and reads
well can still not do what the ticket asked, and the **Acceptance** clause is the only place
that is written down.

## Step 0.5 — Pull the prior rounds, and decide which round this is

**Do not skip this.** A reviewer agent starts cold and cannot tell "nobody has looked at this"
from "this was examined last round and settled". Without the history, round 2 reviews the
round-1 patch instead of the ticket, and reverses decisions it does not know were made.

```bash
gh api repos/fmonniot/zelkova-lang/pulls/<PR>/comments --paginate \
  --jq '.[] | {id, in_reply_to_id, path, line, user: .user.login, created_at, body}'
gh api repos/fmonniot/zelkova-lang/pulls/<PR>/reviews --paginate \
  --jq '.[] | select(.body != "") | {submitted_at, body}'
```

- **Empty → round 1.** Use the shared prompt body as-is.
- **Non-empty → round N ≥ 2.** Prepend the round-N block, and compute the delta the fix rounds
  produced — which is all round N is allowed to review:

```bash
LAST_REVIEW_SHA=$(gh api repos/fmonniot/zelkova-lang/pulls/<PR>/reviews --paginate \
  --jq '[.[] | select(.body != "")] | last | .commit_id')
git -C "$WT" log --oneline "$LAST_REVIEW_SHA..HEAD"
git -C "$WT" diff "$LAST_REVIEW_SHA..HEAD"
```

Paste the prior findings **and their replies**. The replies are the important half: they record
what was decided and why, and a decision recorded there is settled.

## Step 1 — Spawn one agent per PR, all in parallel

Single message, multiple `Agent` calls. For each:

- `subagent_type: general-purpose`
- `model:` resolved above (opus unless overridden)
- `run_in_background: true`
- **Do not set `isolation: "worktree"`** — reuse the worktree from Step 0 via `cd`/`git -C`.
- **Prompt:** the shared body below; for round ≥ 2, the round-N block prepended to it.

### Prompt template — shared body

> You are reviewing an existing PR in the zelkova-lang repo. Its branch is already checked out
> for you at `<WT>` — operate only inside this directory via `cd <WT>` or `git -C <WT>`. Never
> touch `<REPO_ROOT>` itself (the user's own working copy) and never check out a different
> branch there. Leave the worktree in place when you're done.
>
> Do an adversarial review of https://github.com/fmonniot/zelkova-lang/pull/<PR>. Write your
> findings as comments on the PR, favoring inline comments where possible.
>
> This PR claims to implement `<ID>`. Here is that ticket — check the diff against it, not just
> against itself. Its **Acceptance** clause is the yardstick: a change that is clean but does
> not satisfy it is a finding, and so is one that quietly widens scope beyond what was asked.
>
> ```markdown
> <TICKET-TEXT>
> ```
>
> Ground the review in real repo context from your worktree — read `CLAUDE.md`, especially its
> **Standing invariants** section, plus the surrounding modules and existing tests. Don't review
> the diff in isolation. Use `gh pr diff <PR> --repo fmonniot/zelkova-lang` for the exact diff
> and `gh pr view <PR> --repo fmonniot/zelkova-lang --json headRefName,headRefOid` for the head
> SHA needed for inline comments.
>
> **What this project is.** Zelkova is a compiler for an Elm-like language, written in Rust as a
> learning project. It has no users, no network surface, no untrusted input — its input is
> `.zel` source files the author wrote. A security finding is almost never the right frame here;
> a *correctness* finding usually is. Calibrate accordingly: "a malicious `.zel` file could…"
> is not a useful review comment, but "this silently drops a construct during canonicalization"
> very much is.
>
> Focus on, roughly in order of value:
>
> - **Does the diff satisfy the ticket's Acceptance, and did it stay in scope?**
> - **New `panic!`, `unwrap()`, `expect()` or `todo!()` on a non-test path.** This is the
>   repo's first standing invariant and an entire past ticket (`ERR-1`) was spent removing
>   them. `unwrap()` under `#[cfg(test)]` is fine.
> - **Errors that cannot be reported.** Phase errors are rendered by
>   `CompilationError::as_diagnostic`. A new error variant carrying no location information
>   cannot become a real diagnostic — flag it, and note that `ERR-2` is the ticket for the
>   general case so the fix here need only not make it worse.
> - **A `grammar.lalrpop` change without its `parser` AST and `canonical` conversion
>   counterparts.** These three must move together; split across commits, the tree either does
>   not build or silently drops a construct.
> - **Tests that pin nothing.** Ask, per new test: if I reverted the one line that constitutes
>   the fix, would this test go red? An assertion of `is_err()` where the point was *which*
>   error is raised, an assertion that holds trivially because of ordering elsewhere, one the
>   type system already guarantees. This is the highest-yield question in the whole review.
> - **Regressions in `cargo run`.** `CLAUDE.md` records the expected baseline output. A diff
>   that changes it without the ticket asking is a regression whatever the tests say.
> - **Comments that describe something other than what the code does.** Only raise wording when
>   the comment states something *false*; imprecision is a `[note]` at most.
>
> Skip pure style and formatting. But note that `.github/workflows/rust.yml` marks the fmt and
> clippy jobs `continue-on-error: true`, so **CI does not gate on them** — a clippy warning or a
> formatting diff in this PR is a legitimate finding, not something CI already caught. Check
> with `cargo clippy --all-features` and `cargo fmt --all --check` in your worktree.
>
> Also check the ticket close-out, which is part of the diff: the PR should **delete**
> `docs/tickets/<ID-LOWER>.md` and rewrite that ticket's row in `docs/tickets/README.md` as a
> tombstone — link dropped, `status` set to `closed <YYYY-MM-DD>`, and **no SHA and no PR number
> in the row** (the closing commit runs before either exists). A PR that leaves the ticket file
> in place, or that drops the row instead of tombstoning it, is a finding: the row is the only
> thing telling a future reader that path was ever there to `git log`.
>
> **Severity tag — every inline comment starts with exactly one of these, as its first
> characters:**
>
> - `[blocking]` — merging this ships a defect: a miscompile, a broken invariant, or an unmet
>   clause of the ticket's **Acceptance**. **At most 3 per review** — if you have more, rank
>   them and tag the rest `[should-fix]`, which is still implemented, so nothing is lost by
>   ranking honestly. The cap forces a ranking; it does not suppress findings.
> - `[should-fix]` — a real defect or a test that pins nothing, but shipping it is survivable.
>   **No cap.**
> - `[note]` — an observation: imprecise prose, a stale cross-reference, a design opinion, a
>   "worth considering". These will be replied to, **not implemented**. Write them knowing
>   that. **No cap.**
>
> **There is no cap on total findings.** If the diff has nine problems, report nine. Do not pad
> either: two findings is a good review and zero is a valid outcome. Prefer merging several
> small observations about one file into a single comment over splitting them.
>
> For each finding, post an inline comment:
> ```bash
> gh api repos/fmonniot/zelkova-lang/pulls/<PR>/comments \
>   -X POST -f commit_id="<head-sha>" -f path="<file>" -F line=<n> -f side="RIGHT" \
>   -f body="[blocking] <finding>"
> ```
> Use `position` instead of `line` if the line falls in a hunk header or a deletion. Findings
> that don't map to a specific line go in the top-level summary instead.
>
> Finish with a top-level review. Open the summary with one of two verdicts on its own line:
>
> - `**APPROVED**` — no `[blocking]` findings. Say what you checked and what convinced you.
> - `**CHANGES REQUESTED**` — one or more `[blocking]` findings, listed in order.
>
> ```bash
> gh pr review <PR> --repo fmonniot/zelkova-lang --comment --body "<summary>"
> ```
>
> Report back your verdict, how many inline comments you posted, and your top finding.

### Prompt template — additional block for round N ≥ 2

Prepend when Step 0.5 found prior rounds. `<PRIOR-FINDINGS-AND-REPLIES>` is the dump from Step
0.5 — findings *with* their replies — and `<LAST_REVIEW_SHA>` the SHA that review was written
against.

> **This is review round `<N>`. A previous round already reviewed this PR, and its findings were
> fixed.** Here is everything that was raised, and how each was answered:
>
> ```
> <PRIOR-FINDINGS-AND-REPLIES>
> ```
>
> Three rules follow, and they override the general instructions below where they conflict:
>
> 1. **A question answered above is settled.** Do not re-raise it, and do not raise its mirror
>    image. If you conclude a previous round's fix was actually wrong, that is legitimate — but
>    post it as `[blocking] Reversing round <N-1>'s decision on <X>: <the new evidence>`, naming
>    the commit you are undoing. Never present it as a fresh discovery.
> 2. **Review only what changed since `<LAST_REVIEW_SHA>`**, plus a verification pass on the
>    previous round's `[blocking]` items — did the fix actually close them? The original ticket
>    work was already reviewed; re-reviewing it is how a round finds eight new things about code
>    nobody touched.
> 3. **The fixes themselves are the main thing you are looking at, and that is a trap.** Most
>    round-2 findings land on code the round-1 fixes just wrote. Some of that is legitimate — a
>    rushed fix can introduce a defect. Most is churn. Before posting, ask: *would I have
>    flagged this if it had been in the original PR?* If not, it is a `[note]` or nothing.
>
> If the previous round's blocking items are closed and the fix commits introduced nothing
> blocking, the correct output is `**APPROVED**` with zero inline comments. That is a normal and
> expected result for round 2 — say so and stop.

## Step 2 — Report the launch immediately

One line per agent, right after spawning, naming the model:

> I've launched an agent in the background to review PR #<PR> (`<ID>`, one-line description).
> Model: `<model>`. It's working in `<WT>` on branch `<branch>`. I'll verify its work and report
> back once it completes.

## Step 3 — Verify

Don't take "posted N comments" at face value:

```bash
gh api repos/fmonniot/zelkova-lang/pulls/<PR>/comments --paginate \
  --jq '[.[] | select(.in_reply_to_id == null)] | length'
gh pr view <PR> --repo fmonniot/zelkova-lang --json reviews -q '.reviews[-1].body'

# Severity histogram for the round just posted
gh api repos/fmonniot/zelkova-lang/pulls/<PR>/comments --paginate \
  --jq '[.[] | select(.in_reply_to_id == null) | .body[0:12]] | group_by(.) | map({tag: .[0], n: length})'
```

Two things to check, and to fix by hand rather than re-spawning:

- **Untagged comments.** A finding with no `[blocking]`/`[should-fix]`/`[note]` prefix is
  invisible to `fix-pr-comments`' triage and gets treated as `[should-fix]`. If more than one
  or two are untagged, say so in the report.
- **More than 3 `[blocking]`.** Ranking was skipped; flag it. A review that tags most of its
  findings blocking has defeated the mechanism.

## Step 4 — Report

| Ticket | PR | Round | Model | Verdict | blocking / should-fix / note | Top finding |
|---|---|---|---|---|---|---|
| AST-1 | #NNN | 2 | opus | APPROVED | 0 / 1 / 2 | one-line |

If the verdict is `APPROVED` with zero blocking findings, say so plainly and recommend merging
rather than another `fix-pr-comments` pass. **The loop is supposed to terminate**; an approved
round is the terminating condition, not an invitation to run another one.

## Notes

- **Never `git checkout` / `gh pr checkout` in the main repo.** All git activity for a PR
  happens in its dedicated worktree.
- **Line numbers are for the new-file side (`+`)** in the diff — get this wrong and the GitHub
  API 422s; fall back to a top-level comment rather than dropping the finding.
- **Leave the worktree in place** — `fix-pr-comments` reuses it next.
- **Round 3 is a smell.** If a PR reaches a third round, the finding volume is being generated
  by the loop rather than by the code. Stop and hand it to the user with a summary of what is
  genuinely still open.
- **Severity is the brake, not volume.** What stops the loop is that `[note]` gets a reply
  instead of a commit, so a long review no longer mechanically produces a long diff for the next
  round to review.
- **A finding worth more than this PR belongs in a ticket.** If the review turns up a real
  problem outside the diff, say so in the summary as a `[note]` and recommend `create-ticket` —
  do not push the PR into fixing it.
