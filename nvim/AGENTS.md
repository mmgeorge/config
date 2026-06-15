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
