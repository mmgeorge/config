---
globs:
  - 'nvim/**/*'
---

# Neovim Plugin Development

Use the **neovim-lua-dev** skill for Lua and Neovim plugin work. It covers Neovim plugin idioms, driving a live Neovim over RPC, Trouble v3 sources, the Snacks diff renderer, window/fold/highlight pitfalls, git-from-inside-nvim through the fake-editor commit bridge, and a numbered catalog of bugs with fixes.

Read the relevant skill reference section before modifying that subsystem.

When editing Neovim Lua plugin code, use LuaLS/EmmyLua annotations for public APIs, table-shaped state, callback boundaries, and tests. Prefer `---@class`, `---@field`, `---@alias`, `---@param`, `---@return`, and `---@type` so LSP diagnostics, completion, and jump-to-definition stay useful.

Diagnostics come from `lua-language-server`. Check the whole plugin from the CLI with `lua-language-server --check nvim/lua/forge --logpath <scratch-outside-repo>` using the scoop build because the Mason build crashes `--check` on a locale bug. Config lives at `nvim/.luarc.json` with LuaJIT, the `vim` global, and `checkThirdParty = false`. Lazydev supplies the real `vim` runtime types in-editor.

Most `undefined-field` and `inject-field` volume comes from the dynamic `dr()` boundary pattern. `init` injects hundreds of `M._x` members through `for pairs` loops that lua-ls cannot see statically. After a `git mv`-heavy refactor, run `:LspRestart` to clear stale-index duplicate warnings because lua-ls merges the old and new paths. See `.rulesync/rules/forge.md` under Linting for triage and the actionable codes worth fixing.

Every Neovim request path must surface request failures with notifications. External CLI calls, Git/GitHub/API requests, async metadata loads, completion sources, and background processes must report nonzero exits, invalid JSON, missing required context, and stale-operation errors through `vim.notify()` or a module notification wrapper.

Do not silently convert request failures into empty lists, no-op refreshes, or stuck loading states. Keep "zero results" distinct from "request failed" and test the failure notification path.

Report caught errors via `vim.notify` or module notification helpers instead of returning silently or writing only to logs. Omit notifications only when error recovery is a documented normal path (such as falling back to unstaged git status when no staged diff exists).

## Personal Infrastructure Compatibility

Treat this Neovim configuration and its local sidecars as personal infrastructure that may break stored internal state between revisions. Do not add data migrations, compatibility shims, legacy decoders, or fallback code for old private formats unless explicitly requested.

Version durable session payloads with one exact current format. Hide sessions written by any other version instead of upgrading, deleting, or partially decoding them. Preserve independent user preferences only when their current schema still decodes directly.

Prefer deleting obsolete compatibility code when a format changes. Tests should prove that current data reopens and outdated data stays invisible, not that historical formats migrate successfully.

Harness permissions live in `stdpath("config")/forge/permissions.json` and use the validated Rulesync-shaped JSON format owned by the Rust sidecar. Edit that one document through `:ForgePermissions`. Keep Read, Write, Full, and YOLO as fixed execution modes, project each mode through the backend's native sandbox boundary, and never let JSON permission rules widen the selected mode. Do not reintroduce named trust profiles, Lua boolean permission tables, provider-specific approval persistence, or direct writes that bypass policy validation.

Keep the global Rulesync `codexcli` target free of the `permissions` feature while Harness owns Codex approvals. A generated Codex exec policy can reject an operation before Harness receives its approval request, splitting policy ownership across two evaluators.

## GitHub Data And Issue Cache

All Neovim GitHub state that should survive restarts belongs under the `forge/github` namespace in Neovim's data dir. `github.repo_cache.base_dir()` defaults to `vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "github")`. Repository and cwd paths include the hostname selected by `github.repo_cache.hostname()`.

Do not construct ad hoc sibling cache paths. Route repo-scoped files through `github.repo_cache.repo_dir(repo)`, cwd mappings through the repo-cache helpers, and tests through `github.repo_cache.set_data_dir_for_test(path)`. `:ForgeGithubDeleteRepoCache` forms the cleanup boundary for the current repo and must delete any repo-local GitHub data placed under `repo_dir(repo)`.

The GitHub issue autocomplete cache belongs to `github.issue_index`. Its durable database lives at `repo_dir(repo)/issues/issues.redb`. Asynchronous preload reads `repo_dir(repo)/issues/open-snapshot.json`, and synchronous completion filters only the adopted in-memory records.

The database uses redb through `forge-github::IssueStore` in `nvim/rust/forge`. Lua sends typed `github.issues` storage requests and `github.sync` refresh requests through the shared Forge client. GithubService owns pagination, freshness, retries, repository leases, and native gh reads. Runtime shutdown joins storage, sync, and native process ownership. Do not restore a standalone issue storage executable, startup builder, Lua sync loop, or Lua filesystem sync lock.

Lua owns sync triggers, progress presentation, notifications, and snapshot preload after the final host response. Sync must reject a resolved hostname that differs from the current cache namespace. Repository deletion also uses `github.issues` and invalidates loaded snapshots and detail memory only after successful completion.

Every production snapshot preload first sends `github.issues` with `reconcile_snapshot`. Rust compares the published repository, filter, row count, and revision with committed database state while retaining the database handle. Page commits advance the revision atomically, and recovery republishes missing or mismatched snapshots without fetching GitHub. Lua adopts only the revision returned by reconciliation, retries concurrent replacement within the existing three-attempt bound, and preserves prior records on failure. A database without a committed page is not a successful empty publication. Keep reconciliation inside bounded preload admission, including watcher-triggered reloads.

Issue detail fetching uses `github.detail`. GithubService retains the remote request and repository lease through cache persistence before returning a normalized record. Sync, detail, and repository metadata jobs share a two-job service bound, and all native gh reads share a four-request pool. Lua can display a cached detail while refreshing it but must not run its own remote issue-detail request or duplicate the host's persistence step. A failed remote fetch remains distinct from an empty or cached result. Unrelated standalone GitHub commands remain separate consumers until their service cutovers.

Completed `github.detail`, `github.metadata`, and `github.issues` responses above 512 KiB use request-correlated `result.part` and `result.complete` events. Each encoded response is bounded to 16 MiB with two active encoding permits. Senders wait for output capacity. Lua validates sequence, byte totals, and response identity before completing the request. Partial results must never reach UI callbacks or Harness subscribers. Transfer admission and size errors retain `operation_completed: true` because service persistence has already completed.

Repository contributor and collaborator metadata uses `github.metadata`. Rust merges and validates both sources, retains partial failure diagnostics, applies the ten-minute freshness policy, and atomically publishes the current-format `metadata.json` before responding. A separate metadata-refresh lease excludes competing refreshes while the shared repository lease blocks cache deletion. Lua must not restore contributor subprocesses or metadata file writes. Cached reads and cwd mapping remain Lua consumers. Legacy metadata without a hostname inherits the validated cache-path identity without moving its file.

PR lifecycle changes use `github.pull_request` through the Lua transition adapter. Title/body editing uses `forge.review` and the `review.*` document routes, with Rust-owned field revisions and saved baselines. Both use the same GithubService remote queue, ordered by normalized hostname, repository, and issue/PR number, with 64 admitted operations, 64 retained resources, and 16 MiB of retained request input. Unknown outcomes retain resource admission until a read-only reconciliation succeeds. Edit reconciliation compares the exact submitted fields, and an empty body clears it while omitted fields remain unchanged. The service owns queued and running jobs independently of Lua request receivers. Native reads and writes share four process slots and a separate 16 MiB input budget. Queue closure rejects waiting work and retains the active native write through collection. Other Lua GitHub writers remain outside this ordering boundary until their cutovers.

`#` issue completion must never call GitHub per keystroke. Completion filters the latest local snapshot synchronously and returns nothing if no snapshot exists yet.

Background sync triggers when ForgeStatus, PR status/review, issue, notifications, or other GitHub buffers enable repo-aware completion or load repo metadata. The normal historical sync downloads open issues only.

After the open history completes, automatic refreshes become stale after 10 minutes and fetch updated open/closed issues until the stored high-water timestamp is reached, so cached open issues can update if they close. Manual `:ForgeGithubIssueSync` syncs open issues. `:ForgeGithubIssueSync all` syncs all issue states.

Atomic page-level upserts ensure database consistency if execution is interrupted. Rate-limit responses or zero remaining budget should pause and retry instead of spinning.

First-load and manual syncs should show progress. All sync, sidecar, JSON, and GitHub request failures must notify with the underlying stderr or API error text.
