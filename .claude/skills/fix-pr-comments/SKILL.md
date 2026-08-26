---
name: fix-pr-comments
description: Triage the review comments on a GitHub PR (inline, review-level, and issue-level) by severity, implement the blocking and should-fix ones, reply to the rest, resolve merge conflicts with main, run cargo fmt/clippy, push, and resolve the threads. Use when the user says "fix the comments on PR #N", "address the review", or asks to act on reviewer feedback for one or more PRs.
argument-hint: <TICKET-ID -> PR-M> [...] [--model sonnet|opus|fable|haiku]
---

# Fix PR Comments

Given one or more `<TICKET-ID> -> PR-M` pairs, implement every actionable review comment on each
PR, commit the fixes, resolve any merge conflicts with `main`, run `cargo fmt`/clippy, push, and
resolve the comment threads on GitHub.

## Input

$ARGUMENTS

Pairs like `AST-1 -> PR 120` or `BUG-2 -> PR 121`. A ticket ID is any ID in
`docs/tickets/INDEX.md` — bugs and tasks share one namespace.

### Model

If the user names a model — globally or per PR — use it and skip the complexity assessment for
that PR. Valid: `sonnet`, `opus`, `fable`, `haiku`.

Otherwise fall through to the heuristic at the end of Step 0. **Triage in Step 0 runs either
way** — the model choice is separate from deciding which comments get implemented.

## Step 0 — Triage by severity, then pick a model per PR

Pull the review feedback and skim it before spawning anything:

```bash
gh api repos/fmonniot/zelkova-lang/pulls/<PR>/comments --paginate --jq '.[] | {path, line, body}'
gh api repos/fmonniot/zelkova-lang/pulls/<PR>/reviews --paginate --jq '.[] | select(.body != "") | {state, body}'
gh api repos/fmonniot/zelkova-lang/issues/<PR>/comments --paginate --jq '.[] | {body}'
```

**Severity is the input that decides what gets built.** `review-pr` tags every inline finding
`[blocking]`, `[should-fix]` or `[note]`. Sort them:

- `[blocking]` and `[should-fix]` → **implement**.
- `[note]` → **reply, do not implement.** These are observations, imprecise prose and design
  opinions. Implementing them is how a 1-commit ticket becomes a 20-commit PR, and each
  rewritten doc comment becomes the next round's surface to find fault with.
- **Untagged** (an older review, or a reviewer that skipped the convention) → judge it against
  the same bar: does merging as-is ship a defect, or leave a clause of the ticket's Acceptance
  unmet? If yes, treat as `[should-fix]`. If it is about wording, a cross-reference, or "worth
  considering", treat as `[note]`.

If the review summary opens with `**APPROVED**` and carries no `[blocking]` findings, **do not
spawn anything** — tell the user the PR is ready to merge and stop. Running a fix pass over an
approved review is how the loop fails to terminate.

Report the triage before spawning, so a `[note]` you are about to skip can be promoted by hand:

> PR #NNN: 1 blocking, 2 should-fix, 3 notes. Implementing 3, replying to 3.

### Then pick the model

Skip for any PR whose model the user named. Otherwise default to `opus`. Downgrade to `sonnet`
only when every comment **to be implemented** is mechanical — typos, renames, formatting, moving
a line, a one-word doc fix. Anything touching the AST, the grammar, error types, control flow or
test coverage keeps that PR on opus.

## Step 1 — Reuse the PR's worktree

Same as `review-pr` Step 0: prefer `.claude/worktrees/<id-lower>`; otherwise locate or create
one from the PR's branch. Refresh to the current remote head before spawning:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
WT="$REPO_ROOT/.claude/worktrees/<id-lower>"
BRANCH=$(gh pr view <PR> --repo fmonniot/zelkova-lang --json headRefName -q .headRefName)
git -C "$WT" fetch origin "$BRANCH"
git -C "$WT" checkout "$BRANCH"
git -C "$WT" merge --ff-only "origin/$BRANCH"
```

## Step 2 — Spawn one agent per PR, all in parallel

Single message, multiple `Agent` calls, using the model picked in Step 0 for each. **Do not set
`isolation: "worktree"`** — reuse the worktree from Step 1 via `cd`/`git -C`.

### Prompt template

> You are addressing reviewer feedback on an existing PR in zelkova-lang. Its branch is already
> checked out for you at `<WT>` — operate only inside this directory via `cd <WT>` or
> `git -C <WT>`. Never touch `<REPO_ROOT>` itself (the user's own working copy). Leave the
> worktree in place when done.
>
> A review has been posted on https://github.com/fmonniot/zelkova-lang/pull/<PR>. Address the
> actionable items — see the triage rule below, which decides what "actionable" means. Commit
> the changes, resolve any merge conflict with `origin/main`, run `cargo fmt` and clippy, push,
> and resolve the threads.
>
> Detail:
>
> 1. Fetch all three comment sources before writing any code, so you see the full picture and
>    can order fixes sensibly (root-cause fixes before the tests that pin them; removals before
>    renames):
>    ```bash
>    gh api repos/fmonniot/zelkova-lang/pulls/<PR>/comments --paginate --jq '.[] | {id, in_reply_to_id, path, line, body, diff_hunk}'
>    gh api repos/fmonniot/zelkova-lang/pulls/<PR>/reviews --paginate --jq '.[] | select(.body != "") | {id, state, body}'
>    gh api repos/fmonniot/zelkova-lang/issues/<PR>/comments --paginate --jq '.[] | {id, body}'
>    ```
>    Skip pure acknowledgements ("LGTM", "+1").
>
>    **Triage before you write anything.** Each inline finding is tagged with its severity as
>    the first characters of its body:
>    - `[blocking]`, `[should-fix]` → implement it.
>    - `[note]` → **do not implement.** Reply saying you read it and why it is not being
>      actioned (or that you agree and it is worth a follow-up ticket). A `[note]` is an
>      observation, not an instruction. You are not being graded on how many you close.
>    - untagged → same bar: a defect or an unmet ticket-Acceptance clause is `[should-fix]`;
>      wording, cross-references and "worth considering" are `[note]`.
>
>    **Comments that reverse an earlier decision on this PR are not implemented.** Fetch the
>    replies too (`in_reply_to_id != null`) — they record what previous rounds decided. If a
>    finding asks you to undo something an earlier round explicitly asked for and a reply
>    confirms was done that way on purpose, **do not implement it**. Reply with the earlier
>    commit SHA, quote the earlier instruction, say the two rounds disagree, and leave that
>    thread unresolved so the user arbitrates. Pushing back is the correct behaviour here, not a
>    failure to comply.
> 2. Implement the fixes. **Group them into a small number of coherent commits** — one per theme
>    or per subsystem, not one per comment. A 6-comment review should produce roughly 2–4
>    commits; one-commit-per-comment is what makes these PRs 16 commits long, and every one of
>    those commits is surface for the next review round. Run `cargo test` before each commit.
>
>    Read `CLAUDE.md`'s **Standing invariants** before touching anything: no `panic!`/`unwrap()`
>    /`expect()`/`todo!()` on non-test paths, and a `grammar.lalrpop` change lands together with
>    its `parser` AST and `canonical` conversion counterparts. A fix that satisfies a reviewer by
>    breaking one of those is not a fix.
>
>    **Any test you add or change gets mutation-checked**: revert the line that constitutes the
>    fix, confirm that test goes red, restore. The same rule the original work was held to.
> 3. Merge `main` in and resolve conflicts:
>    ```bash
>    git fetch origin main
>    git merge origin/main
>    ```
>    `docs/tickets/INDEX.md` is the one file every ticket's PR touches — each closes out by
>    rewriting its own row as a tombstone — so it is the likely conflict. **Resolve it by keeping
>    both sides' rows.** Never drop another ticket's row to make the merge go away: that row is
>    the only record its ticket file ever existed, and deleting it breaks the
>    `git log --diff-filter=D` recovery path documented in the INDEX header. Ticket *bodies*
>    can't conflict — they're one file each.
> 4. Run and fix, committing separately if anything changes:
>    ```bash
>    cargo build && cargo test
>    cargo fmt --all
>    cargo clippy --all-features
>    cargo run                    # must match the baseline in CLAUDE.md
>    ```
>    `.github/workflows/rust.yml` marks the fmt and clippy jobs `continue-on-error: true`, so
>    **CI will not fail on either** — a green CI run proves nothing about them. Leave no warnings
>    regardless.
> 5. Push: `git push origin <branch>`.
> 6. Reply to **every** comment, implemented or not:
>    - **Implemented** — the short commit SHA that addressed it and a one-line description; if
>      you diverged from the suggestion, say why.
>    - **`[note]`, not implemented** — one line saying you read it and why it isn't being
>      actioned ("agreed but out of scope for this PR", "the comment is accurate as written, see
>      X", "worth a follow-up ticket — not filing one unprompted"). A short honest decline is a
>      better outcome than a commit nobody asked for.
>    - **Disputed** (reverses an earlier round's decision) — quote the earlier instruction and
>      its SHA, state that the two rounds disagree, say you are leaving it for the user.
>
>    Endpoints:
>    - Inline: `gh api repos/fmonniot/zelkova-lang/pulls/<PR>/comments -X POST -f body="<reply>" -f in_reply_to=<comment-id>`
>      (the `/replies` endpoint 404s — use `in_reply_to` on the comments endpoint).
>    - Review-level / issue-level: `gh api repos/fmonniot/zelkova-lang/issues/<PR>/comments -X POST -f body="<reply>"`
>      (no `in_reply_to` concept — the reply is a new top-level comment).
> 7. Resolve the inline threads you implemented or declined. **Leave disputed threads open** — an
>    unresolved thread is the signal the user needs to arbitrate, and resolving it hides the
>    disagreement. GraphQL only, no REST endpoint:
>    ```bash
>    gh api graphql -f query='
>      query($pr:Int!) {
>        repository(owner:"fmonniot", name:"zelkova-lang") {
>          pullRequest(number:$pr) {
>            reviewThreads(first:100) { nodes { id isResolved comments(first:1) { nodes { databaseId } } } }
>          }
>        }
>      }' -F pr=<PR>
>    ```
>    Match each thread to its comment by `databaseId`, then for each:
>    ```bash
>    gh api graphql -f query='mutation($id:ID!) { resolveReviewThread(input:{threadId:$id}) { thread { isResolved } } }' -f id="<thread-node-id>"
>    ```
> 8. Check CI: `gh pr checks <PR>`. Fix any failures in their own commit, then push again.
>
> Report back: commits made, whether the merge had conflicts, final `gh pr checks` state, how
> many threads you resolved, and — listed explicitly — which comments you declined and which you
> disputed.

## Step 3 — Report the launch immediately

One line per agent, right after spawning, naming the model:

> I've launched an agent in the background to address review comments on PR #<PR> (`<ID>`,
> one-line description). Model: `<model>`. It's working in `<WT>` on branch `<branch>`. I'll
> verify its work and report back once it completes.

## Step 4 — Verify

```bash
git -C "$WT" log --oneline "origin/main..HEAD"
git ls-remote origin "<branch>"          # confirm the push landed
gh pr checks <PR> --repo fmonniot/zelkova-lang
gh api graphql -f query='query($pr:Int!){repository(owner:"fmonniot",name:"zelkova-lang"){pullRequest(number:$pr){reviewThreads(first:100){nodes{isResolved}}}}}' -F pr=<PR> --jq '[.data.repository.pullRequest.reviewThreads.nodes[] | select(.isResolved==false)] | length'
```

The last command should print `0`, **or exactly the number of threads the agent reported as
disputed**. Anything else means threads were dropped silently; go resolve them rather than
trusting the report. A disputed thread left open is correct — surface it to the user below
rather than closing it.

Because CI does not gate fmt or clippy, a passing `gh pr checks` is not evidence they are clean.
Confirm in the worktree: `git -C "$WT" ...` then `cargo fmt --all --check` and
`cargo clippy --all-features`.

## Step 5 — Report

| Ticket | PR | Model | Commits | Implemented / declined / disputed | Conflicts resolved? | CI | Threads resolved |
|---|---|---|---|---|---|---|---|
| AST-1 | #NNN | opus | 3 | 3 / 3 / 1 | yes | passing | 6/7 |

Spell out any disputed item in prose under the table — which finding, which earlier decision it
contradicts, and the two SHAs. That is the one thing here the user actually has to decide.

## Notes

- **Never `git checkout` / `gh pr checkout` in the main repo** — everything happens in the PR's
  dedicated worktree.
- **Group fixes into a few coherent commits**, not one per comment. Explain any divergence from
  a reviewer's exact suggestion in the reply rather than silently doing something else.
- **Not every comment is an instruction.** `[note]` findings get a reply and no commit. Closing
  every thread with code is how a 6-comment review becomes a 16-commit diff, which is then the
  surface the next review round examines.
- **A declined `[note]` worth acting on later is a `create-ticket` candidate.** Say so in the
  reply; don't file one unprompted, and don't grow this PR to cover it.
- **Leave the worktree in place** after pushing — remove it only once the PR is merged or
  closed, and even then only if asked (`git worktree remove <path>`).
