---
name: work-ticket
description: Spawn one agent per ticket, each in its own git worktree, to read the ticket from docs/tickets/<id>.md, implement it, and open a PR. Use when the user says "work on AST-1", "fix BUG-2 and TIDY-3", or asks to start work on one or more tickets from docs/tickets/ in parallel.
argument-hint: <TICKET-ID> [TICKET-ID ...] [--model sonnet|opus|fable|haiku]
---

# Work Ticket

Given one or more ticket IDs — each one a file under `docs/tickets/` (`BUG-2`, `AST-1`,
`TIDY-3`, …) — spawn one agent per ticket, each in its own persistent git worktree, to read the
ticket, implement it, and open a PR against `main`.

## Input

$ARGUMENTS

One or more ticket IDs, e.g. `AST-1 TIDY-3`, `BUG-2`. A bare number is invalid — the user
should give the full prefix.

### Model

Default model is **sonnet**. If the user names a model — globally ("use opus for these") or per
ticket ("BUG-2 with opus, TIDY-3 with sonnet") — honor it; a global choice applies to every
ticket in the run, a per-ticket choice only to that one. Valid models: `sonnet`, `opus`,
`fable`, `haiku`. Solving a written ticket is usually mechanical enough not to need more than
sonnet; `ERR-2` and `AST-2`, which ask for a design decision, are the kind that do.

## Step 0 — Read each ticket and check it's actually workable

One ticket is one file: `docs/tickets/<id-lower>.md` (`AST-1` → `docs/tickets/ast-1.md`). The
primary working directory may have local edits or be on a branch other than `main`, so read
from `origin/main` rather than the working tree.

```bash
git fetch origin main

ID="AST-1"
ID_LOWER="ast-1"

# The index — cross-cutting notes and the conventions live here, not on the ticket.
git show "origin/main:docs/tickets/README.md"

# The ticket itself.
git show "origin/main:docs/tickets/$ID_LOWER.md"
```

If the file doesn't exist, check the INDEX table before guessing: a row with a close date is a
tombstone, meaning the ticket is already closed and its file was deleted. Stop and tell the
user rather than reconstructing it.

Keep the ticket text — Step 2 pastes it into the agent's prompt and you need it to sanity-check
the work later. While reading, check whether the ticket is safe to start:

- A **"do not fix" / blocked / superseded** note, on the ticket or in `README.md`.
- A **`Depends on:`** field naming a prerequisite ticket that is still open. `AST-2` depends on
  `AST-1` this way. Read the dependency's own wording — `AST-2`'s says the dependency is soft
  and describes how to absorb it, which is workable; a hard one is not.
- Two requested tickets that would collide. `BUG-1` and `BUG-2` both restructure
  `compile_package`'s error handling, and `ERR-2` says outright that doing it before `BUG-1`
  means writing the same plumbing twice. Running those concurrently produces two PRs that
  cannot both merge. Say so and let the user sequence them.

If a requested ticket is blocked, already done, or retired, don't spawn an agent for it — tell
the user why and drop it from the run.

## Step 1 — Create or reuse a worktree per ticket

For each ticket, lowercase the ID (`AST-1` → `ast-1`) — that's the fixed, well-known worktree
location, reused across `work-ticket`, `review-pr` and `fix-pr-comments` so later passes skip a
full rebuild. `.gitignore` ignores `.claude/*` and re-includes `.claude/skills`, so worktrees
never show up in `git status` on the main repo — but the skills themselves are tracked, and
edits to this file are ordinary committable changes.

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
ID_LOWER="ast-1"
WT="$REPO_ROOT/.claude/worktrees/$ID_LOWER"
if [ -d "$WT" ]; then
  # Reuse: a previous partial run may already have commits. Do NOT reset it.
  git -C "$WT" fetch origin main
else
  git worktree add "$WT" -b "$ID_LOWER" origin/main
fi
```

Branch off `origin/main`, not local `main` — clean base, and it avoids "branch already checked
out" errors. The placeholder branch name (`$ID_LOWER`) gets renamed by the agent to the real
convention before it opens the PR; see Step 2.

Rust builds are not cheap and each worktree has its own `target/`. That is the cost of
parallelism here — worth it for two or three tickets, not for ten.

## Step 2 — Spawn one agent per ticket, all in parallel

Spawn all agents in a **single message** (multiple `Agent` tool calls) so they run
concurrently. For each:

- `subagent_type: general-purpose`
- `model:` the model resolved for this ticket in Input → Model
- `run_in_background: true`
- **Do not set `isolation: "worktree"`** — the worktree already exists from Step 1; the agent
  must use it via `cd`/`git -C`, not fork its own.

### Branch naming

- **`BUG-*`**: `fix/bug-N-<slug>` — matches the repo's existing `fix/canonical-error-handling`.
- **Everything else**: `task/<id-lower>-<slug>`.

Slug is 2–4 kebab-case words from the ticket title. Commit subjects lead with `<ID>: <summary>`.

### Prompt template

The agent starts cold — no memory of this conversation. Fill in `<WT>`, `<REPO_ROOT>`, `<ID>`,
`<ID-LOWER>`, `<TICKET-TEXT>` (from Step 0), and `<branch-prefix>`/`<slug>`.

**Paste the ticket in anyway.** You have it from Step 0, and handing it over saves the agent a
round trip on its first turn.

> You are working in the zelkova-lang repo, already checked out for you at `<WT>` (a dedicated
> git worktree, currently on branch `<id-lower>`). Operate only inside this directory via
> `cd <WT>` or `git -C <WT>` — never touch `<REPO_ROOT>` itself, which is the user's own
> working copy and may have unrelated changes in progress. Leave this worktree in place when
> you're done; don't remove it.
>
> Solve `<ID>`. Open a PR with your changes.
>
> The ticket is `<ID>`, and it lives at `docs/tickets/<ID-LOWER>.md`. Here it is in full:
>
> ```markdown
> <TICKET-TEXT>
> ```
>
> Process:
>
> 1. Read `CLAUDE.md` — project context, commands, and the **Standing invariants** section,
>    which is the set of rules that outlive individual tickets. Every one of them is there
>    because breaking it produced a bad diff.
> 2. Follow any cross-reference the ticket actually makes. Another ticket is just
>    `docs/tickets/<other-id-lower>.md`; `docs/tickets/README.md` lists them all.
> 3. Explore the relevant files and confirm the current state before editing. The ticket was
>    written at some point in the past and the tree has moved since; if what you find
>    contradicts the ticket, say so in the PR body rather than quietly working around it.
> 4. Implement it, scoped to `<ID>`. If you find a second, unrelated problem on the way, do not
>    fix it — note it in your final report so it can be filed as its own ticket. Widening the
>    diff is the failure mode this ticket system exists to prevent.
>
>    Two invariants worth restating because they are the ones most often broken here:
>    **no `panic!`, `unwrap()`, `expect()` or `todo!()` on a non-test path** — return a phase
>    `Error` instead; and **a change to `grammar.lalrpop` lands in the same commit as the
>    matching `parser` AST and `canonical` conversion changes**, never split across commits.
> 5. Rename the branch: `git branch -m <branch-prefix>/<id-lower>-<slug>`.
> 6. Run the checks before committing:
>    ```sh
>    cargo build && cargo test
>    cargo fmt --all
>    cargo clippy --all-features
>    cargo run                      # must still parse 7 modules; see CLAUDE.md for the baseline
>    ```
>    Fix every clippy warning. Note that `.github/workflows/rust.yml` marks the fmt and clippy
>    jobs `continue-on-error: true`, so **CI will not catch these for you** — a green CI run on
>    a PR with clippy warnings is not evidence of anything.
>
>    **Then mutation-check every test you added.** A green test proves nothing until you have
>    seen it fail. For each new test: neutralise the behavioural change it is meant to pin —
>    revert the one line that constitutes the fix, comment out the new branch, delete the new
>    guard — re-run *that* test, confirm it goes **red**, then restore. If it stays green, the
>    test does not pin the fix; rewrite it until it does, and note in its doc comment what you
>    neutralised to verify it.
>
>    This is cheap to check and expensive to miss. The recurring shapes: an assertion that
>    holds trivially because of ordering elsewhere; one that cannot tell "the code under test
>    rejected it" from "it was never reached"; and one the type system already guarantees.
>    A test asserting only `is_err()` where the point of the change was *which* error is raised
>    is the local version of this — assert on the variant.
> 7. Close out `<ID>`: delete `docs/tickets/<ID-LOWER>.md`, and in `docs/tickets/README.md`
>    rewrite that ticket's row as a tombstone — drop the link, set `status` to
>    `closed <YYYY-MM-DD>`:
>
>    ```
>    | TIDY-3 | task | — | closed 2026-09-01 | Fix the `associativy` typo |
>    ```
>
>    **No SHA and no PR number in that row** — this commit is written before either exists. The
>    file path is the query key: `git log --diff-filter=D -- docs/tickets/<ID-LOWER>.md`
>    recovers it. Before deleting, promote anything worth keeping longer than the fix: into the
>    code as a doc comment where it explains behaviour, or into `CLAUDE.md`'s *Standing
>    invariants* where it is a rule. Then grep the repo for `<ID>` and repoint anything that
>    linked to the ticket file at `README.md` — including comments in your own diff pointing at
>    "future work `<ID>`" that your diff has just done.
> 8. Commit with a message explaining *why*, not just *what*. Subject line: `<ID>: <summary>`.
> 9. Push: `git push -u origin <branch-prefix>/<id-lower>-<slug>`.
> 10. Open the PR: `gh pr create --repo fmonniot/zelkova-lang --base main --head
>     <branch-prefix>/<id-lower>-<slug>` with a title referencing `<ID>` and a body covering
>     root cause / approach, the fix, and how it was verified.
>
> Report back the branch name, the PR URL, and anything you deliberately left unfixed.

## Step 3 — Report the launch immediately

Right after spawning — before verifying anything, without waiting for completion — post one
line per agent. Always name the model: surfacing it only in the final report means it is
discovered after the agent may already have been interrupted.

> I've launched an agent in the background to work on `<ID>` (one-line description from the
> ticket). Model: `<model>`. It's working in its own worktree at `.claude/worktrees/<id-lower>`
> on branch `<id-lower>` (to be renamed to `<branch-prefix>/<id-lower>-<slug>`). I'll verify
> its work and report back once it completes.

## Step 4 — Verify (never trust an agent's self-report)

Agents hallucinate commit SHAs, branch names and PR URLs, especially under context pressure.
When each agent completes, check real state:

```bash
git -C "$WT" branch --show-current                       # 1. actual branch
git -C "$WT" log --oneline origin/main..HEAD             # 2. commits beyond main?
git ls-remote origin "<branch>"                          # 3. pushed?
gh pr list --repo fmonniot/zelkova-lang --head "<branch>" --json number,url -q '.[] | "\(.number) \(.url)"'
```

| Commits? | Pushed? | PR? | State | Action |
|---|---|---|---|---|
| Yes | Yes | Yes | Done | Sanity-check the PR title/body |
| Yes | Yes | No | Partial | Open the PR yourself |
| Yes | No | No | Partial | Push, then open the PR |
| No | No | No | Not started | Re-spawn with the same prompt into the same empty worktree |

Also confirm the close-out actually happened — `git -C "$WT" show --stat HEAD` should show
`docs/tickets/<id-lower>.md` deleted and `README.md` modified. Agents forget step 7 more often
than they forget step 6.

## Step 5 — Report

| Ticket | Model | Branch | PR | Summary |
|---|---|---|---|---|
| AST-1 | sonnet | task/ast-1-remove-box-vec | #NNN | one-line summary |

Then `cd "$REPO_ROOT"`.

Surface anything an agent reported as deliberately left unfixed — that is a `create-ticket`
candidate and it is the thing most easily lost between here and the merge.

## Notes

- **Batch all spawns into one message** — sequential spawns serialize the work.
- **Never touch the main repo's working directory.** Every git operation for a ticket happens
  in its own `.claude/worktrees/<id-lower>`; even the ticket is read via `git show origin/main:…`.
- **Leave worktrees in place.** `review-pr` and `fix-pr-comments` reuse them.
- **One ID namespace, one closing convention.** Bugs and tasks are both just files under
  `docs/tickets/`; both close the same way. No per-type special case.
- **`cargo run` is the smoke test.** `CLAUDE.md` records the expected baseline. A diff that
  changes that output without the ticket asking for it is a regression, whatever the tests say.
- **A doc comment describes what the code at that site does** — not what the ticket intended.
  This repo comments heavily, which makes an overstated comment a real defect: it is what the
  next reader trusts. `TIDY-4` exists because two test headers describe a type checker that
  stopped being a stub months ago. Prefer saying less over saying more than you verified.
