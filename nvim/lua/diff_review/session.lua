--- Owns the mutable state of an active review session: the active status state, the
--- per-buffer status registry, the borrowed preview window, and the preview-suspend
--- flag the commit flow toggles.
---
--- Holds the state that views, render, git, and actions all read and write across a
--- session, so they coordinate through one explicit owner instead of through the init
--- facade. Each status-like buffer registers in `states`, and `status` always points at
--- the buffer the autocmd state machine (views/status/state.lua) last made current.
---@class DiffReviewSession
local M = {}

--- The active status state — the buffer the current render, navigation, and action code
--- operate on. Swapped on BufEnter by the status state machine.
---@type table?
M.status = nil

--- The primary `:GitStatus` buffer's state, kept distinct from `status` so transient
--- PR/review/diff buffers never displace the main view.
---@type table?
M.main_status = nil

--- Per-buffer status state registry, keyed by buffer handle.
---@type table<integer, table>
M.states = {}

--- The window the status views borrow for diff previews, restored exactly on teardown.
---@type integer?
M.main_win = nil

--- When true, suppresses preview rendering while the commit flow borrows the window.
---@type boolean?
M.suspend_preview = nil

-- Per-session diff caches shared across the git layer, the render engine, and the
-- views. The git layer produces file_diffs/file_hunk_staged/untracked on each
-- refresh; the render engine and diff buffer read them and the per-buffer render
-- artifacts below.

--- Per-file diff text (filename -> unified diff or false), produced by the git layer
--- and consumed by the diff buffer. Reset on each status refresh.
---@type table<string, string|false>
M.file_diffs = {}

--- Per-file staged-hunk flags (filename -> boolean[]), produced alongside file_diffs.
---@type table<string, boolean[]>
M.file_hunk_staged = {}

--- Untracked file map (absolute path -> repo-relative path).
---@type table<string, string>
M.untracked = {}

--- Last-rendered diff text per buffer, used to skip redundant re-renders.
---@type table<integer, string>
M.buf_last_rendered = {}

--- Real (pre-gutter-padding) content length per buffer row, for cursor alignment.
---@type table<integer, table<integer, integer>>
M.diff_line_content_lengths = {}

--- Rows that rendered as an empty diff, per buffer.
---@type table<integer, table>
M.empty_diff_rows = {}

return M
