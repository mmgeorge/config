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
