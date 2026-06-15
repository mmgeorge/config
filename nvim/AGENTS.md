# Neovim Plugin Development

The Neovim/Lua development guidance and the hard-won DiffReview / Trouble /
Snacks plugin patterns now live in a skill at the repo root:

- **Skill:** [`.agents/skills/neovim-lua-dev/SKILL.md`](../.agents/skills/neovim-lua-dev/SKILL.md)
  — Lua + Neovim plugin idioms, and driving a live Neovim over RPC.
- **Reference:** [`.agents/skills/neovim-lua-dev/references/plugin-patterns.md`](../.agents/skills/neovim-lua-dev/references/plugin-patterns.md)
  — Trouble v3 sources, the Snacks diff renderer, window/fold/highlight
  pitfalls, git-from-inside-nvim (the fake-editor commit bridge), and a numbered
  catalog of bugs with fixes.

Read the relevant reference section before touching that subsystem.

When editing Neovim Lua plugin code, use LuaLS/EmmyLua annotations for public
APIs, table-shaped state, callback seams, and tests. Prefer `---@class`,
`---@field`, `---@alias`, `---@param`, `---@return`, and `---@type` so LSP
diagnostics, completion, and jump-to-definition stay useful.

Every Neovim request path must surface request failures with notifications:
external CLI calls, Git/GitHub/API requests, async metadata loads, completion
sources, and background processes must report nonzero exits, invalid JSON,
missing required context, and stale-operation errors through `vim.notify()` or a
module notification wrapper. Do not silently convert request failures into empty
lists, no-op refreshes, or stuck loading states; keep "zero results" distinct
from "request failed" and test the failure notification path.

More generally, make Neovim error handling loud and obvious. When plugin code
catches or recovers from an error, favor a user-visible notification over a
silent return, hidden log-only message, or speculative fallback unless the error
is truly expected and harmless.

## GitHub Data And Issue Cache

All Neovim GitHub state that should survive restarts belongs under the
`gitstatus` namespace in Neovim's data dir: `github.repo_cache.base_dir()`
defaults to `vim.fs.joinpath(vim.fn.stdpath("data"), "gitstatus")`. Do not
hand-roll sibling cache paths. Route repo-scoped files through
`github.repo_cache.repo_dir(repo)`, cwd mappings through the repo-cache helpers,
and tests through `github.repo_cache.set_data_dir_for_test(path)`.
`:GithubDeleteRepoCache` is the cleanup boundary for the current repo and must
delete any repo-local GitHub data placed under `repo_dir(repo)`.

The GitHub issue autocomplete cache is owned by `github.issue_index`. Its
durable database is `repo_dir(repo)/issues/issues.redb`, and the hot completion
path reads `repo_dir(repo)/issues/open-snapshot.json`. The database is redb via
the Rust sidecar in `nvim/rust/github-issue-index` because it is pure Rust,
portable on Windows, ACID, and avoids requiring a system SQLite DLL. Keep the
sidecar storage-focused: it owns redb tables/indexes and snapshot generation;
Lua owns Neovim async control flow, GitHub requests, progress UI, retry policy,
notifications, and buffer hooks.

`#` issue completion must never call GitHub per keystroke. Completion filters
the latest local snapshot synchronously and returns nothing if no snapshot is
available yet. Background sync is triggered when GitStatus, PR status/review,
issue, notifications, or other GitHub buffers enable repo-aware completion or
load repo metadata. The normal historical sync downloads open issues only.
After the open history is complete, automatic refreshes are stale after 10
minutes and fetch updated open/closed issues until the stored high-water
timestamp is reached, so cached open issues can be updated if they close.
Manual `:GithubIssueSync` syncs open issues; `:GithubIssueSync all` syncs all
issue states.

Sync writes one page at a time: fetch a GraphQL page, atomically upsert it into
redb, then regenerate the open snapshot. This makes interruption safe because a
partial run leaves a valid database and a valid last snapshot. Rate-limit
responses or zero remaining budget should pause and retry instead of spinning.
First-load and manual syncs should show progress; all sync, sidecar, JSON, and
GitHub request failures must notify with the useful underlying message.
