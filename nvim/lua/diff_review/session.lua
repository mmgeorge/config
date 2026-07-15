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

---@class DiffReviewHarnessPresentationState
---@field client DiffReviewHarnessClient?
---@field ready boolean
---@field busy boolean
---@field cancel_requested boolean
---@field session table?
---@field capability table
---@field interaction table[]
---@field interaction_by_id table<string, table>
---@field pending_interaction table?
---@field render_rows table<integer, table>
---@field fold_installed table<string, boolean>
---@field activity_expanded table<string, boolean>
---@field rendered_lines string[]
---@field render_namespace integer?
---@field render_initialized boolean
---@field render_fold_signature? string
---@field queue string[]
---@field pending_steer table[]
---@field transcript_buf integer?
---@field transcript_win integer?
---@field composer_buf integer?
---@field composer_win integer?
---@field prompt_line integer[]
---@field activity_range table[]
---@field active_plan table?
---@field active_elicitation table?
---@field approval DiffReviewApprovalRequest[]
---@field approval_open boolean
---@field artifact table[]
---@field timeline table[]
---@field plan_annotations table[]
---@field no_checkpoint boolean
---@field interactions table?
---@field plan_review table?
---@field command_set DiffReviewViewCommandSet?
---@field goal table?
---@field pending_config table?
---@field pending_mode string?
---@field plan_question_open boolean?
---@field presented_question_set_id string?
---@field prompt_history string[]
---@field prompt_history_index integer
---@field prompt_history_draft string?
---@field agent table
---@field selected_agent_run_id string?
---@field agent_live table<string, table>

--- Harness process, view, queue, interaction-tree, and capability state. The broker owns
--- durable state while this table owns only the current Neovim presentation.
---@type DiffReviewHarnessPresentationState
M.harness = {
  client = nil,
  ready = false,
  busy = false,
  cancel_requested = false,
  session = nil,
  capability = {},
  interaction = {},
  interaction_by_id = {},
  pending_interaction = nil,
  render_rows = {},
  fold_installed = {},
  activity_expanded = {},
  rendered_lines = {},
  render_namespace = nil,
  render_initialized = false,
  render_fold_signature = nil,
  queue = {},
  pending_steer = {},
  transcript_buf = nil,
  transcript_win = nil,
  composer_buf = nil,
  composer_win = nil,
  prompt_line = {},
  activity_range = {},
  active_plan = nil,
  active_elicitation = nil,
  approval = {},
  approval_open = false,
  artifact = {},
  timeline = {},
  plan_annotations = {},
  no_checkpoint = false,
  interactions = nil,
  plan_review = nil,
  plan_question_open = false,
  presented_question_set_id = nil,
  prompt_history = {},
  prompt_history_index = 0,
  prompt_history_draft = nil,
  agent = { definition = {}, run = {}, turn = {} },
  selected_agent_run_id = nil,
  agent_live = {},
}

return M
