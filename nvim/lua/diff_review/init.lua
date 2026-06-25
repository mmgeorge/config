--- DiffReview standalone git review UI.
--- Usage: :DiffReview

---@class DiffReviewModule
---@field config DiffReviewConfig?
---@field _git_backend DiffReviewGitBackend?
---@field _status table?
---@field _main_status table?
---@field _status_states table<integer, table>?
---@field _untracked table<string, string>?
---@field _file_diffs table<string, string>?
---@field _file_hunk_staged table<string, boolean[]>?
---@field _diff_bufs table<string, integer>?
---@field _buf_hunks table<integer, DiffReviewHunk[]>?
---@field _buf_filename table<integer, string>?
---@field _buf_saved_cursor table<integer, integer[]>?
---@field _buf_last_rendered table<integer, string>?
---@field _ts_context_cache table<string, DiffReviewHunkTreeSitterContext|string|false|DiffReviewTreeSitterContextPending>?
---@field _ts_syntax_cache table<string, DiffReviewTreeSitterSyntax|false|DiffReviewTreeSitterSyntaxPending>?
---@field _ts_diff_syntax_cache table<string, DiffReviewTreeSitterSyntax|false|DiffReviewTreeSitterSyntaxPending>?
---@field _ts_source_bufs table<string, integer>?
---@field _diff_uses_file_syntax fun(hunk_staged?: boolean[], opts?: table): boolean
---@field _prewarm_diff_syntax fun(filename: string, diff_text: string, hunk_staged?: boolean[], callback_key: string, on_update?: fun(syntax?: DiffReviewTreeSitterSyntax), opts?: table)
---@field _status_prewarm_hunk_budget fun(hunk_count: integer, options?: DiffReviewConfig): integer
---@field _diff_mutation_queue_model table
---@field _main_win integer?
---@field suspend_preview boolean?

---@type DiffReviewModule
local M = {}

-- Register the plugin's bundled treesitter queries on the runtimepath (see query_runtime).
require("diff_review.query_runtime")

local status_diff_hunks_for_file
local status_open_pr
M._comment_icon = "󰅺"
M._reply_icon = "↳"
M._pending_review_icon = "◷"
M._codeowner_review_icon = "⚠"
M._milestone_icon = "◆"

---@param cwd string
---@param extra_args? string[]
---@return string[]
function M._git_diff_command(cwd, extra_args)
  local command = {
    "git", "-C", cwd,
    "-c", "core.quotepath=false",
    "diff", "--no-color", "--no-ext-diff", "--unified=0",
  }
  for _, arg in ipairs(extra_args or {}) do
    command[#command + 1] = arg
  end
  return command
end

---@param cwd string
---@param commit_oid string
---@return string[]
function M._git_show_diff_command(cwd, commit_oid)
  return {
    "git", "-C", cwd,
    "show", "--format=", "--no-color", "--no-ext-diff", "--unified=0", commit_oid,
  }
end
M._pr_overview = require("diff_review.views.pr.pr_overview")
M._datetime = require("diff_review.integrations.datetime")
M._conventional_commit = require("diff_review.integrations.conventional_commit")
M._status_conventional_commit_type_end = M._conventional_commit.type_end
M._status_conventional_commit_subject_segments = M._conventional_commit.subject_segments
M._comment_rows = require("github.comment_rows")

M._hunk_header_ns = vim.api.nvim_create_namespace("diff_review_headers")
M._active_hunk_header_ns = vim.api.nvim_create_namespace("diff_review_active_hunk")
M._status_ns = vim.api.nvim_create_namespace("diff_review_status")
M._status_decorate_ns = vim.api.nvim_create_namespace("diff_review_status_decorate")
M._hunk_header_priority = 20
M._active_hunk_header_priority = 200

local config = require("diff_review.infra.config")
local ai_commit = require("diff_review.integrations.ai_commit")
local gh = require("diff_review.integrations.gh")
local highlights = require("diff_review.infra.highlights")
local notifications = require("diff_review.infra.notifications")
local syntax_engine = require("diff_review.render.syntax_engine")
local diff_render = require("diff_review.render.diff_render")
local status_render = require("diff_review.views.status.status_render")
local keymaps = require("diff_review.shared.keymaps")
local hunk_model = require("diff_review.render.hunk_model")
-- Re-expose the pure hunk model under its original M._hunk_* names so init render
-- helpers and tests reach it unchanged.
for _hunk_name, _hunk_fn in pairs(hunk_model) do M["_hunk_" .. _hunk_name] = _hunk_fn end
M._diff_perf = require("diff_review.infra.perf")
M._diff_source_model = require("diff_review.render.source")
M._diff_annotation_model = require("diff_review.render.annotations")
M._diff_layout_model = require("diff_review.render.layout")
M._diff_mutation_queue_model = require("diff_review.render.mutation_queue")
M._diff_hunk_index_model = require("diff_review.render.hunk_index")
M._diff_row_tree_model = require("diff_review.render.row_tree")
M._diff_source_loader_model = require("diff_review.render.source_loader")
M._diff_syntax_context_model = require("diff_review.render.syntax_context")
M._diff_decoration_model = require("diff_review.render.decoration")
M._diff_view_controller_model = require("diff_review.shared.view_controller")
M._diff_view_command_set_model = require("diff_review.shared.view_command_set")
M._intraline_diff = require("diff_review.render.intraline_diff")
M._inventory = require("diff_review.infra.inventory")
M._window_options = require("diff_review.views.status.window_options")
-- Re-expose window option helpers under their original names so existing callers stay unchanged.
M._status_window_option_with_pair = M._window_options.option_with_pair
M._hide_line_numbers = M._window_options.hide_line_numbers
M._apply_status_window_options = M._window_options.apply
M._restore_line_numbers = M._window_options.restore
M._status_keys = require("diff_review.views.status.status_keys")
M._diff_buffer = require("diff_review.views.diff_buffer")
-- Re-expose every diff buffer view function under its original name (open_diff_buffer,
-- refresh_open_diff_buffer, the _diff_gutter_*/cursor helpers) so existing callers stay unchanged.
for _diff_buffer_name, _diff_buffer_fn in pairs(M._diff_buffer) do M[_diff_buffer_name] = _diff_buffer_fn end
M._status_issues = require("diff_review.views.status.status_issues")
M._status_head = require("diff_review.views.status.status_head")
-- Re-expose every head-line builder under its original name so existing callers stay unchanged.
for _status_head_name, _status_head_fn in pairs(M._status_head) do M[_status_head_name] = _status_head_fn end
-- The gitstatus debug/diagnostics table; callers reach it as M._gitstatus_debug.event/dump/perf_span.
M._gitstatus_debug = require("diff_review.views.status.status_debug")
M._size_gate = require("diff_review.views.status.size_gate")
-- Re-expose every size-gate estimator under its original name so existing callers stay unchanged.
for _size_gate_name, _size_gate_fn in pairs(M._size_gate) do M[_size_gate_name] = _size_gate_fn end
M._fold_state = require("diff_review.views.status.fold_state")
-- Re-expose fold accessors/appliers under their original names so existing callers stay unchanged.
for _fold_state_name, _fold_state_fn in pairs(M._fold_state) do M[_fold_state_name] = _fold_state_fn end
M._section_builder = require("diff_review.views.status.section_builder")
M._pr_overview.review_context_radius = 2
M._section_map = require("diff_review.views.status.section_map")
-- Re-expose section assembly + the section-map structure ops under their original names.
for _section_map_name, _section_map_fn in pairs(M._section_map) do M[_section_map_name] = _section_map_fn end
M._diff_source_state = require("diff_review.views.status.diff_source_state")
-- Re-expose per-file diff-source state functions so status_render's dr() calls resolve unchanged.
for _diff_source_state_name, _diff_source_state_fn in pairs(M._diff_source_state) do M[_diff_source_state_name] = _diff_source_state_fn end
M._entry_nav = require("diff_review.views.status.entry_nav")
-- Re-expose cursor/entry navigation helpers under their original names so existing callers stay unchanged.
for _entry_nav_name, _entry_nav_fn in pairs(M._entry_nav) do M[_entry_nav_name] = _entry_nav_fn end
M._git_data = require("diff_review.git.git_data")
-- Re-expose git execution + diff parsing + async syntax compute under their original names
-- (stage_patch/compute_diff_syntax_async/M._parse_diff/etc) so existing callers and test seams stay unchanged.
for _git_data_name, _git_data_fn in pairs(M._git_data) do M[_git_data_name] = _git_data_fn end
M._commit_view = require("diff_review.views.status.commit_view")
-- Re-expose the commit-message/dialog/action handlers under their original names so keymaps and
-- other modules' dr() dispatch resolves unchanged.
for _commit_view_name, _commit_view_fn in pairs(M._commit_view) do M[_commit_view_name] = _commit_view_fn end

local function setup_bg_highlights()
  highlights.setup()
end

setup_bg_highlights()


---@param message string
---@param title? string
local function notify_error(message, title)
  notifications.error(message, title)
end

---@return boolean
local function debug_notifications_enabled()
  local options = M.config or config.options or config.defaults
  return options.debug_notifications == true
end

---@param message string
---@param level? integer
---@param opts? table
local function notify_debug(message, level, opts)
  if not debug_notifications_enabled() then return end
  vim.notify(message, level or vim.log.levels.INFO, opts)
end

local git_backend = require("diff_review.git.git_backend")
M.set_git_backend = git_backend.set_backend
M.reset_git_backend = git_backend.reset_backend
M._system_output = git_backend.system_output

local paths = require("diff_review.infra.paths")
local repo_file_path = paths.repo_file_path
local repo_relative = paths.repo_relative
M._repo_relative_for_test = paths.repo_relative_for_test

-- Render accumulators live in status_buffer (state-passing). The locals below stay as
-- thin orchestration wrappers that thread the active M._status into each module call,
-- so existing call sites render into the active buffer's state unchanged.
local status_buffer = require("diff_review.views.status.status_buffer")
M._status_buffer = status_buffer


---@param title string
---@param failures DiffReviewGitFailure[]
function M.notify_git_failures(title, failures)
  notifications.git_failures(title, failures)
end

---@param files string[]
---@param args_for_file fun(relpath: string): string[]
---@param title string
---@param cb fun(result: { ok: boolean, successes: string[], failures: DiffReviewGitFailure[] })

--- Return cached Tree-sitter context and kick off an async request if needed.
---@param filename string
---@param line number
---@param callback_key string
---@param on_update? fun(context?: DiffReviewHunkTreeSitterContext|string)
---@return DiffReviewHunkTreeSitterContext|string?

---@param filename string
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending

---@param context DiffReviewHunkTreeSitterContext|string?
---@return string?

---@param context DiffReviewHunkTreeSitterContext|string?
---@return string?

---@param left DiffReviewHunkTreeSitterContext|string?
---@param right DiffReviewHunkTreeSitterContext|string?
---@return boolean



---@param left DiffReviewHunkTreeSitterContext|string?
---@param right DiffReviewHunkTreeSitterContext|string?
---@return boolean

---@param text string?
---@return string

---@param diff_lines string|string[]?
---@return table<string, boolean>



---@class DiffReviewGutterSpec
---@field width integer
---@field old_width integer
---@field new_width integer

---@return DiffReviewGutterSpec

---@param gutter DiffReviewGutterSpec
---@param old_line? integer
---@param new_line? integer
---@param sign string?
---@param sign_hl? string
---@param line_hl? string
---@param changed_line_hl? string
---@return table[]

---@param row table
---@param gutter DiffReviewGutterSpec
---@param old_line? integer
---@param new_line? integer
---@param sign string?
---@param sign_hl? string
---@param line_hl? string
---@param changed_line_hl? string

---@param text string
---@param segments? DiffReviewHighlightSegment[]
---@param line_number? integer
---@param gutter? DiffReviewGutterSpec
---@param file? string
---@param old_line? integer
---@param new_line? integer
---@return table

---@param reference_text string?
---@param gutter? DiffReviewGutterSpec
---@return table

---@param header_parts table[]
---@return table

-- Cache for treesitter context per file (cleared on refresh)
M._ts_context_cache = {}
M._ts_source_bufs = {}

-- Window option save/restore now lives in diff_review.window_options (M._window_options),
-- re-exposed as M._hide_line_numbers / M._apply_status_window_options / M._restore_line_numbers.

--- Register a view controller per status-like view kind so command vocabulary and
--- render hooks resolve through a narrow per-view boundary instead of inline branches.
function M._status_register_view_controllers()
  local controllers = M._diff_view_controller_model
  local command_sets = M._diff_view_command_set_model
  controllers.reset()
  local keymaps = (M.config or config.options or config.defaults).keymaps or {}
  local after_render_by_kind = {
    status = M._status_after_render_status,
    pr = M._status_after_render_pr,
    review = M._status_after_render_review,
  }
  for _, view_kind in ipairs({ "status", "pr", "review", "diff" }) do
    local command_keys = (view_kind == "review" and keymaps.review) or keymaps.status or {}
    local set = command_sets.new()
    for command_id in pairs(command_keys) do
      command_sets.register(set, command_id, function() end)
    end
    local after_render = after_render_by_kind[view_kind]
    controllers.register(controllers.new({
      view_kind = view_kind,
      command_set = function() return set end,
      after_render = after_render and function(state) after_render(state.buf) end or nil,
    }))
  end
end

---@param opts? DiffReviewConfig
function M.setup(opts)
  M.config = config.setup(opts)
  M._diff_perf.configure_from_diff_review_options(M.config)
  setup_bg_highlights()
  M._status_register_view_controllers()
end

--- Close and wipe all diff buffers
function M._cleanup_diff_buffers()
  M._restore_line_numbers(M._main_win)
  M._window_options.reset()
  M._diff_bufs = M._diff_bufs or {}
  for key, buf in pairs(M._diff_bufs) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end
  M._diff_bufs = {}
  M._buf_hunks = {}
  M._buf_filename = {}
  M._buf_saved_cursor = {}
  M._empty_diff_rows = {}
  M._diff_line_content_lengths = {}
  M._main_win = nil
end

---@param cwd string
---@param cb fun(items: table[])
---@param _ctx table?

---@param cb fun(items: table[])
---@param _ctx table?
function M.get(cb, _ctx)
  git_backend.git_root_async(function(cwd)
    if not cwd then
      cb({})
      return
    end
    M._collect_items_from_git(cwd, cb, _ctx)
  end)
end

-- Cache of diff preview items keyed by filename
M._diff_items = {}

-- Namespace for diff preview/status rendering
M._ns = vim.api.nvim_create_namespace("diff_review_preview")
M._diff_gutter_visual_ns = vim.api.nvim_create_namespace("diff_review_visual_gutter")
M._empty_diff_rows = {}
M._diff_line_content_lengths = {}














---@class DiffReviewHunkChangeRegion
---@field first_item integer
---@field last_item integer
---@field context_line integer?
---@field context DiffReviewHunkTreeSitterContext|string?
---@field context_key string?
---@field changed_line integer?
---@field after_line integer?
---@field added integer
---@field removed integer




---@param hunk DiffReviewHunk?
---@return integer?

---@param parsed_line DiffReviewParsedHunkLine
---@param context DiffReviewHunkTreeSitterContext|string?
---@return boolean


---@alias DiffReviewDiffSyntaxSide "old"|"new"

---@param diff_text string
---@param side DiffReviewDiffSyntaxSide
---@return string[]

---@param filename string
---@param diff_text string
---@param side DiffReviewDiffSyntaxSide
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending

---@param filename string
---@param diff_text string
---@return string[]?

---@param filename string
---@param diff_text string
---@return boolean

---@param filename string
---@param diff_text string
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@return DiffReviewTreeSitterSyntax? syntax
---@return boolean pending

---@param hunk_staged? boolean[]
---@param opts? table
---@return boolean

---@param filename string
---@param diff_text string
---@param hunk_staged? boolean[]
---@param callback_key string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@param opts? table

---@param file DiffReviewStatusFile?
---@return string?

---@param hunk_count integer
---@param options? DiffReviewConfig
---@return integer

---@param file DiffReviewStatusFile?
---@param callback_key_prefix string
---@param on_update? fun(syntax?: DiffReviewTreeSitterSyntax)
---@param opts? { syntax_source?: "file"|"diff" }



---@param parsed_line DiffReviewParsedHunkLine
---@param gutter DiffReviewGutterSpec
---@param file string
---@param syntax? DiffReviewTreeSitterSyntax
---@param syntax_row? integer
---@return table



---@class DiffReviewHunkContextPaddingLine
---@field line_number integer
---@field old_line? integer
---@field new_line? integer
---@field text string







---@class DiffReviewHunkRenderPlan
---@field block DiffReviewParsedBlock
---@field hunk DiffReviewParsedHunk
---@field hunk_index integer
---@field region DiffReviewHunkChangeRegion
---@field region_index integer
---@field region_count integer
---@field render_items table[]
---@field include_render_line fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@field gutter DiffReviewGutterSpec
---@field source_lines string[]?
---@field occupied_lines table<integer, boolean>
---@field syntax_by_item table<integer, { syntax?: DiffReviewTreeSitterSyntax, row?: integer }>
---@field visible_source_lines table<string, boolean>
---@field before_padding_lines DiffReviewHunkContextPaddingLine[]
---@field after_padding_lines DiffReviewHunkContextPaddingLine[]
---@field changed_lines table<integer, boolean>
---@field display_start integer?
---@field display_end integer?

---@class DiffReviewHunkDisplayGroup
---@field plans DiffReviewHunkRenderPlan[]
---@field gutter DiffReviewGutterSpec
---@field added integer
---@field removed integer
---@field display_start integer?
---@field display_end integer?
---@field changed_lines table<integer, boolean>




---@param left DiffReviewGutterSpec?
---@param right DiffReviewGutterSpec?
---@return DiffReviewGutterSpec

---@param group DiffReviewHunkDisplayGroup
---@param plan DiffReviewHunkRenderPlan


---@param plans DiffReviewHunkRenderPlan[]
---@return DiffReviewHunkDisplayGroup[]











---@param diff_text string
---@param hunk_staged? boolean[]
---@param filename? string
---@param context_callback_key? fun(hunk_line: number): string
---@param on_context_update? fun()
---@param opts? { context_line: integer?, boundary_context: boolean?, suppress_start_boundary: boolean?, suppress_end_boundary: boolean?, suppress_start_boundary_keys?: table<string, boolean>, suppress_end_boundary_keys?: table<string, boolean>, syntax_source?: "file"|"diff", syntax_diff_text?: string, fallback_syntax_diff_text?: string, old_syntax_row_offset?: integer, new_syntax_row_offset?: integer, compact_replacements?: boolean, require_file_match_for_context?: boolean, debug_context?: table }

---@param buf integer
---@param ns integer
---@param rows table[]

--- Render a formatted diff into a buffer using DiffReview's local formatter.
---@param buf number
---@param diff_text string
---@param hunk_staged? boolean[] staged status per hunk (in order)
---@param filename? string

-- Per-buffer hunk metadata: maps buffer line ranges to raw diff patches
M._buf_hunks = {}
-- Per-buffer filename mapping
M._buf_filename = {}
-- Per-buffer saved cursor position (for restoring after jumping to file)
M._buf_saved_cursor = {}
-- Track what was last rendered per buffer to avoid re-rendering same data (diff_buffer view state)
M._buf_last_rendered = {}


local function confirm(lines, on_yes, on_no)
  local body = vim.list_extend({}, lines)
  body[#body + 1] = ""
  body[#body + 1] = "  [y] yes    [n] no"
  local width = 32
  for _, line in ipairs(body) do
    width = math.max(width, #line + 4)
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, body)
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = "wipe"
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = #body,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - #body) / 2),
    style = "minimal",
    border = "rounded",
    title = " Confirm ",
    title_pos = "center",
  })
  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end
  local function cancel()
    close()
    if on_no then on_no() end
  end
  vim.keymap.set("n", "y", function()
    close()
    on_yes()
  end, { buffer = buf, nowait = true, silent = true })
  for _, key in ipairs({ "n", "q", "<Esc>" }) do
    vim.keymap.set("n", key, cancel, { buffer = buf, nowait = true, silent = true })
  end
end

---@class DiffReviewSectionConfig
---@field name string
---@field title string
---@field default_folded boolean

---@type DiffReviewSectionConfig[]
local status_section_order = {
  { name = "unstaged", title = "Unstaged changes", default_folded = false },
  { name = "staged", title = "Staged changes", default_folded = false },
}
local status_file_indent = 0
local status_hunk_indent = 0
local status_reconcile_delay_ms = 120

---@class DiffReviewStatusCommandSpec
---@field id string
---@field label string
---@field desc string
---@field modes string|string[]
---@field keymap? "status"|"review"
---@field visual? boolean
---@field pinned boolean
---@field views? table<DiffReviewStatusViewKind, boolean>

---@type DiffReviewStatusCommandSpec[]
local status_command_specs = require("diff_review.shared.command_specs").specs

M._status_hint_command_ids_by_view = require("diff_review.shared.command_specs").hint_command_ids_by_view

local status_command_specs_by_id = require("diff_review.shared.command_specs").by_id

---@type table<string, DiffReviewSectionConfig>
local status_section_by_name = {}
for _, section in ipairs(status_section_order) do
  status_section_by_name[section.name] = section
end


---@param buf integer
---@param state table
local function attach_status_state(buf, state)
  M._ensure_status_resize_autocmd()
  M._status_states = M._status_states or {}
  M._status_states[buf] = state
  vim.api.nvim_create_autocmd("BufEnter", {
    buffer = buf,
    callback = function()
      M._status_perf_span("status.autocmd_buf_enter", buf, nil, function()
        local current = M._status_states and M._status_states[buf] or nil
        if current then M._status = current end
        M._apply_status_window_options(vim.api.nvim_get_current_win(), current)
        M._status_apply_native_folds(buf)
        M._status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
      end)
    end,
  })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    buffer = buf,
    callback = function()
      M._status_perf_span("status.autocmd_buf_win_enter", buf, nil, function()
        local current = M._status_states and M._status_states[buf] or nil
        if current then M._status = current end
        M._apply_status_window_options(vim.api.nvim_get_current_win(), current)
        M._status_apply_native_folds(buf)
        if current and current.view_kind == "review" and M._review and M._review.refresh_inline_comment_rules then
          M._review.refresh_inline_comment_rules(buf, vim.api.nvim_get_current_win())
        end
        M._status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
      end)
    end,
  })
  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buf,
    callback = function()
      M._status_perf_span("status.autocmd_cursor_moved", buf, nil, function()
        M._normalize_status_cursor(buf)
        if M._status_issues then M._status_issues.sync_modifiable(buf) end
      end)
    end,
  })
  vim.api.nvim_create_autocmd("ModeChanged", {
    buffer = buf,
    callback = function()
      M._status_perf_span("status.autocmd_mode_changed", buf, nil, function()
        M._normalize_status_cursor(buf)
        if M._status_issues then M._status_issues.sync_modifiable(buf) end
      end)
    end,
  })
  if state.view_kind == "status" and M._status_issues then
    M._status_issues.attach(buf)
  end
  vim.api.nvim_create_autocmd("BufLeave", {
    buffer = buf,
    callback = function()
      M._restore_line_numbers(vim.api.nvim_get_current_win())
      M._status_clear_hint_bar(vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("BufWinLeave", {
    buffer = buf,
    callback = function()
      M._status_clear_hint_bar(vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buf,
    callback = function()
      if M._status_states then M._status_states[buf] = nil end
      if M._main_status == state then M._main_status = nil end
      if M._status == state then M._status = M._main_status end
      if M._diff_line_content_lengths then M._diff_line_content_lengths[buf] = nil end
    end,
  })
end


-- Status entry key builders now live in diff_review.status_keys (M._status_keys), re-exposed below.

---@param sections DiffReviewStatusSection[]?
function M._status_restore_initial_folds(sections)
  M._status = M._status or {}
  M._status.folds = {}
end

---@param sections DiffReviewStatusSection[]?
---@return string?
function M._status_first_grouping_id(sections)
  local section = sections and sections[1] or nil
  return section and M._status_keys.section_key(section.name) or nil
end

local function status_add_highlight(line, start_col, end_col, hl_group, priority)
  return status_buffer.add_highlight(M._status, line, start_col, end_col, hl_group, priority)
end

local function status_add_extmark(line, col, opts)
  return status_buffer.add_extmark(M._status, line, col, opts)
end

local function status_add_line(text, entry, line_hl_group)
  return status_buffer.add_line(M._status, text, entry, line_hl_group)
end

M._status_segment_line_parts = status_buffer.segment_line_parts

local function status_add_segment_line(segments, entry)
  return status_buffer.add_segment_line(M._status, segments, entry)
end

---@param buf integer
---@param entry_id string
---@param head_line DiffReviewStatusHeadLine
---@return boolean
function M._status_patch_head_line(buf, entry_id, head_line)
  local status = M._status_states and M._status_states[buf] or M._status
  if not (
    status
    and status.entries
    and head_line
    and head_line.segments
    and vim.api.nvim_buf_is_valid(buf)
  ) then return false end

  local target_line = nil
  for line, entry in pairs(status.entries) do
    if entry and entry.id == entry_id then
      target_line = line
      break
    end
  end
  if not target_line then return false end

  local text, segment_highlights = M._status_segment_line_parts(head_line.segments)
  local was_rendering = vim.b[buf].diff_review_status_rendering
  vim.b[buf].diff_review_status_rendering = true
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, target_line - 1, target_line, false, { text })
  vim.bo[buf].modifiable = false
  vim.b[buf].diff_review_status_rendering = was_rendering
  status.entries[target_line] = head_line.entry
  if status.lines then status.lines[target_line] = text end

  vim.api.nvim_buf_clear_namespace(buf, M._status_ns, target_line - 1, target_line)
  for _, highlight in ipairs(segment_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._status_ns, target_line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end
  return true
end


---@return DiffReviewStatusKeymapConfig

---@param spec DiffReviewStatusCommandSpec
---@return boolean

---@param spec DiffReviewStatusCommandSpec
---@param view_kind DiffReviewStatusViewKind
---@return boolean

---@param command_id string
---@return string[]

-- status_primary_key / status_key_text now live in diff_review.status_keys.

---@param state? table
---@return table[]

---@param state table
---@return string

---@param segments table[]
---@param title string
---@return string

---@param buf integer
---@param win? integer

---@param win integer

local function status_add_fancy_row(row, entry, indent)
  return status_buffer.add_fancy_row(M._status, row, entry, indent)
end


---@param file DiffReviewStatusFile
---@return DiffReviewHunk[]
status_diff_hunks_for_file = function(file)
  return M._status_perf_span("status.diff_hunks_for_file", M._status and M._status.buf or nil, {
    file = file and file.filename or nil,
    relpath = file and file.relpath or nil,
    existing_hunk_count = file and file.hunks and #file.hunks or nil,
    untracked = file and file.untracked or nil,
  }, function()
    if #file.hunks > 0 then return file.hunks end
    if not file.untracked then return {} end

    local relpath = M._untracked and M._untracked[file.filename]
    local diff_text = relpath and M._build_untracked_diff(file.filename, relpath) or nil
    if not diff_text then return {} end

    local hunks = {}
    for _, parsed_hunk in ipairs(M._parse_diff(diff_text, false)) do
      hunks[#hunks + 1] = {
        filename = file.filename,
        section_name = file.section_name,
        pos = parsed_hunk.pos,
        diff = parsed_hunk.diff,
        staged = false,
        context_text = parsed_hunk.context or "",
        added = parsed_hunk.added or 0,
        removed = parsed_hunk.removed or 0,
      }
    end
    return hunks
  end)
end



local status_cursor_target

---@param entry_kind? string
---@return "file"|"diff"

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param previous_hunk? DiffReviewHunk
---@param next_hunk? DiffReviewHunk
---@param entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"
---@param hunk_key_override? string







---@param file DiffReviewStatusFile
---@param entry_kind? "file"|"commit_file"|"pr_file"|"pr_review_file"
---@param hunk_entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"
---@param file_key_override? string
---@param hunk_key_builder? fun(hunk: DiffReviewHunk): string
---@param opts? { force_open?: boolean, default_open?: boolean }







---@param commit DiffReviewStatusCommit
---@return string?
local function status_commit_relative_date(commit)
  local value = commit.committed_at or commit.authored_at
  local relative = M._datetime.relative(value, { yesterday = false })
  if relative == "" then return nil end
  return relative
end


---@param commit DiffReviewStatusCommit
---@param date_width integer

---@param section DiffReviewStatusSection

---@param commit DiffReviewStatusCommit
local function status_load_commit_files(commit)
  local status = M._status
  local cwd = status and status.cwd
  local buf = status and status.buf
  if not (cwd and buf and vim.api.nvim_buf_is_valid(buf)) then return end

  local source_state = M._status_ensure_commit_source_state(commit)
  if not source_state then return end
  local source_id = source_state.handle.id
  status.commit_file_cache = status.commit_file_cache or {}
  local cached = status.commit_file_cache[commit.oid]
  if cached and (cached.files_loaded or cached.files_loading) then
    commit.files = cached.files
    commit.files_loaded = cached.files_loaded
    commit.files_loading = cached.files_loading
    commit.files_error = cached.files_error
    return
  end

  local request_id = (status.commit_file_request_id or 0) + 1
  status.commit_file_request_id = request_id
  commit.files_loading = true
  commit.files_error = nil
  status.commit_file_cache[commit.oid] = {
    files = nil,
    files_loaded = false,
    files_loading = true,
    files_error = nil,
  }

  M._diff_source_model.ensure_loaded(source_state, function(ok, err)
    local latest_status = M._status
    if not (latest_status and latest_status.buf and vim.api.nvim_buf_is_valid(latest_status.buf)) then return end
    if latest_status.cwd ~= cwd then return end

    latest_status.commit_file_cache = latest_status.commit_file_cache or {}
    local next_cache = {
      files = nil,
      files_loaded = ok == true,
      files_loading = false,
      files_error = nil,
    }
    local latest_source = latest_status.diff_source_registry
      and latest_status.diff_source_registry.source_by_id
      and latest_status.diff_source_registry.source_by_id[source_id]
      or source_state
    if not ok then
      next_cache.files_error = "Unable to load commit diff" .. (err and err ~= "" and (": " .. tostring(err)) or "")
    else
      next_cache.files = latest_source.metadata and latest_source.metadata.files or {}
    end
    latest_status.commit_file_cache[commit.oid] = next_cache

    for _, section in ipairs(latest_status.sections or {}) do
      for _, current_commit in ipairs(section.commits or {}) do
        if current_commit.oid == commit.oid then
          current_commit.files = next_cache.files
          current_commit.files_loaded = next_cache.files_loaded
          current_commit.files_loading = false
          current_commit.files_error = next_cache.files_error
        end
      end
    end

    M.render_status(latest_status.buf, nil, nil, { reuse_sections = true })
  end)
end

---@param state? table
---@return DiffReviewStatusEntry?

---@param buf integer?
---@return string? target_id
---@return integer? fallback_line
status_cursor_target = function(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return nil, nil end
  local ok_current_buf, current_buf = pcall(vim.api.nvim_get_current_buf)
  if not ok_current_buf or current_buf ~= buf then return nil, nil end
  local ok_cursor, cursor = pcall(vim.api.nvim_win_get_cursor, 0)
  if not ok_cursor then return nil, nil end

  local line = cursor[1]
  local status = M._status
  local entry = status and status.entries and status.entries[line] or nil
  return entry and entry.id or nil, line
end

--- Resolve the gitstatus debug log path; kept in init as a test override seam
--- (tests swap M._gitstatus_debug_log_path to redirect the log file).
---@return string
function M._gitstatus_debug_log_path()
  if M._gitstatus_debug_log_file then return M._gitstatus_debug_log_file end
  local dir = (vim.fn.stdpath("state") or ".") .. "/diff_review"
  pcall(vim.fn.mkdir, dir, "p")
  M._gitstatus_debug_log_file = dir .. "/gitstatus-debug.log"
  return M._gitstatus_debug_log_file
end

---@param buf integer?
---@param extra? table
---@return table
function M._status_perf_payload(buf, extra)
  local state = buf and M._status_states and M._status_states[buf] or nil
  if not state and M._status and (not buf or M._status.buf == buf) then state = M._status end
  local payload = vim.deepcopy(extra or {})
  payload.buf = buf
  payload.view_kind = state and state.view_kind or nil
  if buf and vim.api.nvim_buf_is_valid(buf) then
    payload.line_count = vim.api.nvim_buf_line_count(buf)
  end
  if buf and vim.api.nvim_get_current_buf() == buf then
    local cursor = vim.api.nvim_win_get_cursor(0)
    payload.cursor_row = cursor[1]
    payload.cursor_col = cursor[2]
  end
  local viewport = state and state.diff_viewport or nil
  if viewport and viewport.enabled then
    payload.viewport_top = viewport.top
    payload.viewport_total = viewport.total
    payload.viewport_logical_total = viewport.logical_total
    payload.viewport_render_count = viewport.render_count
  end
  return payload
end

---@param event string
---@param buf integer?
---@param extra table?
function M._status_perf_event(event, buf, extra)
  local debug = M._gitstatus_debug
  local payload = M._status_perf_payload(buf, extra)
  if M._diff_perf and M._diff_perf.enabled() then M._diff_perf.event(event, payload) end
  if not (debug and debug.perf_event) then return end
  debug.perf_event(event, payload)
end

---@param event string
---@param buf integer?
---@param extra table?
---@param callback fun(): any
---@return any
function M._status_perf_span(event, buf, extra, callback)
  local debug = M._gitstatus_debug
  local payload = M._status_perf_payload(buf, extra)
  if M._diff_perf and M._diff_perf.enabled() then
    return M._diff_perf.span(event, payload, function()
      if not (debug and debug.perf_span and debug.perf_enabled and debug.perf_enabled()) then return callback() end
      return debug.perf_span(event, payload, callback)
    end)
  end
  if not (debug and debug.perf_span and debug.perf_enabled and debug.perf_enabled()) then return callback() end
  return debug.perf_span(event, payload, callback)
end


---@param buf integer
function M._status_stop_markdown_highlighter(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if vim.bo[buf].filetype ~= "GitStatus" then return end
  if vim.treesitter and type(vim.treesitter.stop) == "function" then
    pcall(vim.treesitter.stop, buf, "markdown")
  end
end


---@param buf integer

---@param buf integer

--- Run the PR overview view's after-render work (its view controller's after_render hook).
---@param buf integer
function M._status_after_render_pr(buf)
  M._status_perf_span("status.after_render.pr_edit_on_render", buf, nil, function() M._pr_edit.on_render(buf) end)
  M._status_perf_span("status.after_render.review_on_render", buf, nil, function() M._review.on_render(buf) end)
  M._status_perf_span("status.after_render.markdown", buf, nil, function() M._pr_edit.render_markdown_regions(buf) end)
  M._status_perf_span("status.after_render.native_folds", buf, nil, function() M._status_apply_native_folds(buf) end)
  M._status_perf_span("status.after_render.pr_sync_modifiable", buf, nil, function() M._pr_edit.sync_modifiable(buf) end)
  M._status_perf_span("status.after_render.schedule_native_folds", buf, nil, function() M._status_schedule_native_folds(buf) end)
end

--- Run the PR review view's after-render work.
---@param buf integer
function M._status_after_render_review(buf)
  M._status_perf_span("status.after_render.review_on_render", buf, nil, function() M._review.on_render(buf) end)
  M._status_perf_span("status.after_render.markdown", buf, nil, function() M._pr_edit.render_markdown_regions(buf) end)
  M._status_perf_span("status.after_render.native_folds", buf, nil, function() M._status_apply_native_folds(buf) end)
  M._status_perf_span("status.after_render.review_sync_modifiable", buf, nil, function() M._review.sync_modifiable(buf) end)
  M._status_perf_span("status.after_render.schedule_native_folds", buf, nil, function() M._status_schedule_native_folds(buf) end)
end

--- Run the GitStatus view's after-render work.
---@param buf integer
function M._status_after_render_status(buf)
  M._status_perf_span("status.after_render.stop_markdown", buf, nil, function() M._status_stop_markdown_highlighter(buf) end)
  M._status_perf_span("status.after_render.native_folds", buf, nil, function() M._status_apply_native_folds(buf) end)
  M._status_perf_span("status.after_render.issues_sync_modifiable", buf, nil, function() M._status_issues.sync_modifiable(buf) end)
end

--- Run the after-render work for branch diff and any other status-like view.
---@param buf integer
function M._status_after_render_default(buf)
  M._status_perf_span("status.after_render.native_folds", buf, nil, function() M._status_apply_native_folds(buf) end)
end

---@param buf integer
---@param walkthrough table?


---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean }
---@param head_lines table[]
---@param sections DiffReviewStatusSection[]

---@param cwd string
---@param buf integer
---@param force? boolean
local function status_start_pr_lookup(cwd, buf, request_id)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  local status = M._status_states and M._status_states[buf] or M._status
  if not (status and status.pr_request_id == request_id and status.pr_root == cwd and status.pr) then return end
  if status.pr.lookup_started then return end
  status.pr.lookup_started = true

  local function complete_pr_lookup(result)
    local latest_status = M._status_states and M._status_states[buf] or M._status
    if not (latest_status and latest_status.pr_request_id == request_id and latest_status.pr_root == cwd) then return end
    M._status = latest_status
    local pending_open = latest_status.pr and latest_status.pr.open_when_ready
    local pending_open_win = latest_status.pr and latest_status.pr.open_when_ready_win

    if result.ok and result.pr then
      latest_status.pr = { state = "ready", pr = result.pr, lookup_started = true }
    elseif result.ok and result.unavailable then
      latest_status.pr = { state = "unavailable", message = result.message, lookup_started = true }
    elseif result.ok then
      latest_status.pr = { state = "none", lookup_started = true }
    else
      local message = result.message or "Unable to fetch GitHub pull request"
      latest_status.pr = { state = "error", message = message, lookup_started = true }
      notify_error("GitHub PR lookup failed: " .. message)
    end

    if latest_status.head_values then
      latest_status.head_lines = M._status_build_head_lines(
        latest_status.head_values,
        latest_status.pr,
        latest_status.about,
        latest_status.issues
      )
    end
    if vim.api.nvim_buf_is_valid(buf) and latest_status.head_lines and latest_status.sections then
      if not M._status_patch_head_line(buf, "pr", M._status_pr_head_line(latest_status.pr)) then
        M.render_status(buf, nil, nil, { reuse_sections = true })
      end
    end
    if pending_open then
      vim.schedule(function()
        if not (status_open_pr and vim.api.nvim_buf_is_valid(buf)) then return end
        local current_status = M._status_states and M._status_states[buf] or M._status
        if not (current_status and current_status.pr_request_id == request_id and current_status.pr_root == cwd) then return end
        M._status = current_status

        local function open_pending_pr()
          status_open_pr(nil)
        end

        if pending_open_win and vim.api.nvim_win_is_valid(pending_open_win) then
          local ok, win_buf = pcall(vim.api.nvim_win_get_buf, pending_open_win)
          if ok and win_buf == buf then
            vim.api.nvim_win_call(pending_open_win, open_pending_pr)
            return
          end
        end
        if vim.api.nvim_get_current_buf() == buf then open_pending_pr() end
      end)
    end
  end

  local options = M.config or config.options or config.defaults
  if options.pr_lookup_mode == "mock-delay" then
    vim.defer_fn(function()
      complete_pr_lookup({ ok = true })
    end, math.max(1, tonumber(options.pr_mock_delay_ms) or 5000))
    return
  end

  gh.current_pr_async(cwd, complete_pr_lookup)
end

---@param cwd string
---@param buf integer
---@param force? boolean
---@return integer? request_id
local function status_ensure_pr_state(cwd, buf, force)
  M._status = M._status or {}
  local status = M._status
  if not force and status.pr_root == cwd and status.pr then return nil end

  status.pr_root = cwd
  status.pr = { state = "fetching" }
  status.pr_request_id = (status.pr_request_id or 0) + 1
  return status.pr_request_id
end

---@param sections DiffReviewStatusSection[]?
---@return boolean
local function status_has_changes(sections)
  for _, section in ipairs(sections or {}) do
    if #(section.files or {}) > 0 then return true end
  end
  return false
end

---@param cwd string
---@param buf integer
---@param has_changes boolean
---@param force? boolean
---@param allow_generation? boolean
local function status_ensure_about_state(cwd, buf, has_changes, force, allow_generation)
  M._status = M._status or {}
  local status = M._status
  if not has_changes then
    status.about_root = cwd
    status.about = { state = "none", waiters = {} }
    return
  end

  if not allow_generation and (M.config or config.options or config.defaults).about_auto_generate == false then
    status.about_root = cwd
    status.about = { state = "none", waiters = {} }
    status.about_pending = nil
    return
  end

  local current = ai_commit.state()
  if not force and not allow_generation and status.about_root == cwd and status.about_pending and status.about then return end
  if not force and status.about_root == cwd and status.about and current == status.about then return end

  status.about_root = cwd
  status.about = current and current.cwd == cwd and current or { state = "generating", cwd = cwd, waiters = {} }
  status.about_request_id = (status.about_request_id or 0) + 1
  local request_id = status.about_request_id
  if allow_generation then
    M._status_patch_about_line(buf)
  end

  local function start_generation()
    local latest_status = M._status_states and M._status_states[buf] or M._status
    if not (latest_status and latest_status.about_request_id == request_id and latest_status.about_root == cwd) then return end
    latest_status.about_pending = nil
    ai_commit.ensure(cwd, { force = force }, function(result)
      latest_status = M._status_states and M._status_states[buf] or M._status
      if not (latest_status and latest_status.about_request_id == request_id and latest_status.about_root == cwd) then return end
      M._status = latest_status
      latest_status.about = result
      latest_status.about_pending = nil
      if latest_status.head_values then
        latest_status.head_lines = M._status_build_head_lines(
          latest_status.head_values,
          latest_status.pr,
          latest_status.about,
          latest_status.issues
        )
      end
      if vim.api.nvim_buf_is_valid(buf) and latest_status.head_lines and latest_status.sections then
        if not M._status_patch_about_line(buf) then
          M.render_status(buf, nil, nil, { reuse_sections = true })
        end
      end
    end)
  end

  local delay = allow_generation and 0 or ((M.config or config.options or config.defaults).about_auto_generate_delay_ms or 0)
  if delay > 0 then
    status.about_pending = true
    vim.defer_fn(start_generation, delay)
    return
  end

  start_generation()
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean, refresh_pr?: boolean, refresh_about?: boolean, restore_initial_folds?: boolean }
function M.render_status(buf, target_id, fallback_line, opts)
  opts = opts or {}
  setup_bg_highlights()
  if M._status_states and M._status_states[buf] then
    M._status = M._status_states[buf]
  end
  M._status = M._status or {}
  M._status.buf = buf
  M._status.reconcile_generation = (M._status.reconcile_generation or 0) + 1
  local render_state = M._status
  local preserve_current_cursor = target_id == nil and fallback_line == nil

  if opts.reuse_sections and M._status.head_lines and M._status.sections then
    if preserve_current_cursor then
      target_id, fallback_line = status_cursor_target(buf)
    end
    status_render.status_render_loaded(buf, target_id, fallback_line, opts, M._status.head_lines, M._status.sections)
    return
  end

  M._status.request_id = (M._status.request_id or 0) + 1
  local request_id = M._status.request_id
  local has_existing_view = M._status.head_lines ~= nil or M._status.sections ~= nil
  if not has_existing_view then
    M._status_set_plain_lines(buf, { "Loading DiffReview..." })
  end

  git_backend.git_root_async(function(cwd, root_err)
    local latest_status = M._status_states and M._status_states[buf] or render_state
    if not (latest_status and latest_status.request_id == request_id) then return end
    M._status = latest_status
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      if not has_existing_view then
        M._status_set_plain_lines(buf, { "Not a git repository" })
      end
      return
    end

    latest_status.cwd = cwd
    M._status_issues.ensure_state(latest_status, cwd)
    local pr_request_id = status_ensure_pr_state(cwd, buf, opts.refresh_pr)

    M._status_load_async(cwd, function(result)
      local current_status = M._status_states and M._status_states[buf] or render_state
      if not (current_status and current_status.request_id == request_id) then return end
      M._status = current_status
      if not vim.api.nvim_buf_is_valid(buf) then return end
      if M._status_operations_pending() then
        M._status_request_reconcile(buf, target_id)
        return
      end
      if opts.restore_initial_folds then
        M._status_restore_initial_folds(result.sections)
        target_id = M._status_first_grouping_id(result.sections)
        fallback_line = nil
      end
      status_ensure_about_state(cwd, buf, status_has_changes(result.sections), opts.refresh_about)
      M._status_issues.ensure_state(current_status, cwd)
      result.head_lines = M._status_build_head_lines(
        result.head_values or {},
        current_status.pr,
        current_status.about,
        current_status.issues
      )
      current_status.head_lines = result.head_lines
      current_status.head_values = result.head_values
      current_status.sections = result.sections
      current_status.fancy_rows = {}
      if preserve_current_cursor and not opts.restore_initial_folds then
        target_id, fallback_line = status_cursor_target(buf)
      end
      status_render.status_render_loaded(buf, target_id, fallback_line, opts, result.head_lines, result.sections)
      vim.schedule(function()
        local metadata_status = M._status_states and M._status_states[buf] or render_state
        if not (
          metadata_status
          and metadata_status.request_id == request_id
          and metadata_status.cwd == cwd
          and vim.api.nvim_buf_is_valid(buf)
        ) then return end
        M.github_load_repo_metadata(cwd, require("github.repo_cache").repo_for_cwd(cwd))
      end)
      if pr_request_id then
        vim.defer_fn(function()
          status_start_pr_lookup(cwd, buf, pr_request_id)
        end, 50)
      end
    end)
  end)
end

---@param buf integer
---@param target_id? string
---@param fallback_line? integer
---@param opts? { reuse_sections?: boolean, refresh_pr?: boolean, refresh_about?: boolean, restore_initial_folds?: boolean }
local function render_status_or_notify(buf, target_id, fallback_line, opts)
  local ok, err = xpcall(function()
    M.render_status(buf, target_id, fallback_line, opts)
  end, debug.traceback)
  if not ok then
    notify_error("DiffReview render failed: " .. tostring(err))
  end
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
---@param diff_text? string
local function render_pr_status(pr, cwd, buf, diff_text)
  local status = M._status
  if not (status and status.buf == buf) then return end
  if M._pr_edit.blocks_render(buf) then return end
  diff_text = diff_text or status.pr_diff_text
  M._gitstatus_debug.event("render_pr_status.start", {
    buf = buf,
    cwd = cwd,
    pr_number = pr and pr.number or nil,
    repo = pr and pr.repo or nil,
    diff_len = #(diff_text or ""),
    comment_count = status.pr_comments and #status.pr_comments or nil,
    standalone_comment_count = status.pr_standalone_comments and #status.pr_standalone_comments or nil,
    regular_comment_count = status.pr_regular_comments and #status.pr_regular_comments or nil,
  })
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  status.pr_regular_comments = status.pr_regular_comments or {}
  M._pr_overview.refresh_editable_comments(status)
  status.head_lines = M._status_pr_detail_head_lines(pr, status)
  status.sections = M._status_pr_sections(cwd, pr, diff_text, status.pr_comments, status.pr_standalone_comments, status.pr_regular_comments)
  local section_file_count = 0
  for _, section in ipairs(status.sections or {}) do
    section_file_count = section_file_count + #(section.files or {})
  end
  M._gitstatus_debug.event("render_pr_status.sections", {
    buf = buf,
    pr_number = pr and pr.number or nil,
    section_count = status.sections and #status.sections or nil,
    file_count = section_file_count,
  })
  status.fancy_rows = {}
  status.pr_code_comments_by_anchor = M._section_builder.comment_anchor_index_from_sections(status.sections, { field = "pr_comments" })
  status.review_after_row = function(diff_line, indent)
    M._section_builder.emit_anchored_comments(status, diff_line, indent, { index_field = "pr_code_comments_by_anchor" })
  end
  status_render.status_render_loaded(buf, nil, nil, { reuse_sections = true }, status.head_lines, status.sections)
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
local function load_pr_diff(pr, cwd, buf)
  local status = M._status
  if not (status and status.buf == buf) then return end
  status.pr_diff_request_id = (status.pr_diff_request_id or 0) + 1
  local request_id = status.pr_diff_request_id
  M._gitstatus_debug.event("load_pr_diff.start", {
    buf = buf,
    cwd = cwd,
    request_id = request_id,
    pr_number = pr and pr.number or nil,
    repo = pr and pr.repo or nil,
  })
  gh.pr_diff_async(cwd, pr.number, pr.repo, function(result)
    local latest_status = M._status_states and M._status_states[buf] or nil
    if not (
      latest_status
      and latest_status.pr_diff_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    M._status = latest_status
    M._gitstatus_debug.event("load_pr_diff.done", {
      buf = buf,
      cwd = cwd,
      request_id = request_id,
      pr_number = pr and pr.number or nil,
      repo = pr and pr.repo or nil,
      code = result and result.code or nil,
      stdout_len = result and result.stdout and #result.stdout or nil,
      stdout_preview = result and result.stdout and result.stdout:sub(1, 800) or nil,
      stderr = result and result.stderr ~= "" and result.stderr or nil,
      output = result and result.code ~= 0 and result.output or nil,
    })
    if result.code ~= 0 then
      notify_error("GitHub PR diff failed: " .. (result.output ~= "" and result.output or ("gh exited " .. result.code)), "DiffReview")
      return
    end
    latest_status.pr_diff_text = result.stdout or ""
    render_pr_status(pr, cwd, buf, latest_status.pr_diff_text)
  end)
end



-- PR review mode (":or" in the PR/status view). A review buffer
-- (view_kind = "review") shows the PR title, an editable review summary, and
-- the changed files split into Unviewed/Viewed sections. `S`/`U` move a file
-- between them, `C` posts an inline comment to GitHub at the cursor/selection,
-- `y`/`n` jump between comments, and the submit key posts the review verdict.
-- Everything hangs off the module table because init.lua is at Lua's
-- 200-local limit.
M._review = require("diff_review.views.pr.review")
M._review.comment_icon = M._comment_icon
M._review.reply_icon = M._reply_icon





















































































































































-- Per-repository config, read from `<repo root>/.diffreview.json`. Currently
-- only `branch_prefix` (used by the `bc` branch-create action). On the module
-- table because init.lua is at Lua's 200-local limit.
M._repo_config = require("diff_review.git.repo_config")


--- Centered single-line input popup (not vim.ui.input / snacks). `<CR>`
--- submits, `<Esc>`/`q` cancels (calling back with nil).
---@param prefix string prefilled text
---@param on_submit fun(name: string?)
function M._prompt_branch_name(prefix, on_submit)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { prefix })
  local width = math.min(60, math.max(30, math.floor(vim.o.columns * 0.4)))
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    row = math.floor((vim.o.lines - 3) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    width = width,
    height = 1,
    style = "minimal",
    border = "rounded",
    title = " New branch ",
    title_pos = "center",
  })
  local done = false
  local function finish(value)
    if done then return end
    done = true
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
    on_submit(value)
  end
  vim.keymap.set({ "i", "n" }, "<CR>", function()
    vim.cmd("stopinsert")
    finish(vim.api.nvim_buf_get_lines(buf, 0, -1, false)[1] or "")
  end, { buffer = buf })
  vim.keymap.set("i", "<C-c>", function() vim.cmd("stopinsert"); finish(nil) end, { buffer = buf })
  vim.keymap.set("n", "<Esc>", function() finish(nil) end, { buffer = buf, nowait = true })
  vim.keymap.set("n", "q", function() finish(nil) end, { buffer = buf, nowait = true })
  vim.api.nvim_create_autocmd("BufLeave", { buffer = buf, once = true, callback = function() finish(nil) end })
  vim.cmd("startinsert!")
end

--- `bc`: prompt for a name (prefilled with the repo's branch prefix) and
--- create + switch to the new branch, then refresh the status view.
---@param buf integer
function M._create_branch(buf)
  local status = M._status_states and M._status_states[buf] or M._status
  local cwd = status and status.cwd
  if not cwd then
    notify_error("Not a git repository", "DiffReview")
    return
  end
  local prefix = M._repo_config.branch_prefix(cwd)
  M._prompt_branch_name(prefix, function(name)
    if not name then return end
    name = vim.trim(name)
    if name == "" or name == prefix then return end
    git_backend.run_git_at_root_async(cwd, { "switch", "-c", name }, nil, function(result)
      if not result.ok then
        notify_error(
          "Create branch failed: " .. (result.output ~= "" and result.output or ("git exited " .. tostring(result.code))),
          "DiffReview"
        )
        return
      end
      vim.notify("Created branch " .. name, vim.log.levels.INFO, { title = "DiffReview" })
      if vim.api.nvim_buf_is_valid(buf) then
        M._status = status
        status.pr = nil
        status.about = nil
        render_status_or_notify(buf, nil, nil, { restore_initial_folds = true, refresh_pr = true, refresh_about = true })
      end
    end)
  end)
end

-- Branch-diff view (":GitDiff <branch>") helpers. Grouped on the module
-- table rather than as locals: this chunk is at Lua's hard limit of 200
-- local variables, so new file-scope `local function`s break the module.
-- Seam: expose the render orchestrator for the extracted branch_diff module.
M._status_render_loaded = status_render.status_render_loaded
M._branch_diff = require("diff_review.views.branch_diff")

---@param target_id? string


-- PR title/description editing in the PR view: insert mode is gated to the
-- title line and description block, ":w" syncs via `gh pr edit`, and a "*"
-- marks unsynced fields. Functions hang off the module table because
-- init.lua is at Lua's 200-local limit.
-- Render/confirm seams used by the extracted pr_edit module (all in scope here).
M._confirm = confirm
M._render_pr_status = render_pr_status
-- Seams shared with the extracted pr_overview module.
-- Fold state seam reached by the extracted status_head module through dr().
M._status_provider_file_key = M._status_keys.provider_file_key
M._status_provider_hunk_key = M._status_keys.provider_hunk_key
M._status_render_file = status_render.status_render_file
-- Seams shared with the extracted review module.
M._status_command_visible = keymaps.status_command_visible
M._status_keys_for = keymaps.status_keys_for
M._notify_debug = notify_debug
M._status_command_specs_by_id = status_command_specs_by_id
M._status_diff_hunks_for_file = status_diff_hunks_for_file
M._status_cursor_target = status_cursor_target
-- Seams for the extracted syntax_engine module.
M._file_source_lines = syntax_engine.file_source_lines
M._same_hunk_ancestor_scope = syntax_engine.same_hunk_ancestor_scope
M._status_hunk_context_line = syntax_engine.status_hunk_context_line
M._old_file_syntax_source_lines = syntax_engine.old_file_syntax_source_lines
M._diff_new_side_matches_file = syntax_engine.diff_new_side_matches_file
M._cached_old_file_syntax = syntax_engine.cached_old_file_syntax
M._diff_uses_file_syntax = syntax_engine.diff_uses_file_syntax
M._prewarm_diff_syntax = syntax_engine.prewarm_diff_syntax
M._status_file_syntax_diff_text = syntax_engine.status_file_syntax_diff_text
M._status_prewarm_hunk_budget = syntax_engine.status_prewarm_hunk_budget
M._status_syntax_source_for_entry_kind = syntax_engine.status_syntax_source_for_entry_kind
-- Seams the git_data module reaches through dr() for notifications and stat counting.
M._notify_error = notify_error
-- Seam the commit_view module reaches through dr() to load a commit's files.
M._status_load_commit_files = status_load_commit_files
-- Seams the diff_buffer module reaches through dr() for its diff sourcing.
-- Seams the section_map module reaches through dr() for section ordering and git collection.
M._status_section_order = status_section_order
M._status_section_by_name = status_section_by_name
M._status_render_current_model = status_render.status_render_current_model
-- Seams for the extracted diff_render module.
M._merge_hunk_gutter_specs = diff_render.merge_hunk_gutter_specs
M._add_plan_to_hunk_display_group = diff_render.add_plan_to_hunk_display_group
M._merge_hunk_render_plans = diff_render.merge_hunk_render_plans
-- Seams for the extracted status_render module.
M._status_decorate_visible = status_render.status_decorate_visible
M._status_emit_row_spans = status_render.status_emit_row_spans
M._status_decorate_rows = status_render.status_decorate_rows
M._status_register_decoration_provider = status_render.status_register_decoration_provider
M._status_write_rendered_buffer = status_render.status_write_rendered_buffer
M._status_apply_rendered_extmarks = status_render.status_apply_rendered_extmarks
M._status_after_buffer_render = status_render.status_after_buffer_render
M._status_commit_relative_date = status_commit_relative_date
M._status_hunk_key = M._status_keys.hunk_key
M._setup_bg_highlights = setup_bg_highlights
M._attach_status_state = attach_status_state
M._status_commit_key = M._status_keys.commit_key
M._status_commit_hunk_key = M._status_keys.commit_hunk_key
M._status_commit_file_key = M._status_keys.commit_file_key
M._status_file_key = M._status_keys.file_key
M._status_section_key = M._status_keys.section_key
-- Seams shared with the extracted hunk_model module.
M._hunk_add_gutter = diff_render.hunk_add_gutter
M._treesitter_line_segments = syntax_engine.treesitter_line_segments
M._hunk_context_scope_key = syntax_engine.hunk_context_scope_key
M._same_hunk_context_scope = syntax_engine.same_hunk_context_scope
M._hunk_line_visible_in_context_scope = syntax_engine.hunk_line_visible_in_context_scope
M._pr_edit = require("diff_review.views.pr.pr_edit")
M._status_actions = require("diff_review.views.status.actions")
for _action_name, _action_fn in pairs(M._status_actions) do M[_action_name] = _action_fn end
M._commands = require("diff_review.views.commands")
for _command_name, _command_fn in pairs(M._commands) do M[_command_name] = _command_fn end


---@param diff_text string
---@param old_target_line integer
---@return integer?
local function closest_current_line_for_deleted_diff_line(diff_text, old_target_line)
  local old_line
  local new_line
  local in_hunk = false
  for diff_line in diff_text:gmatch("[^\n]+") do
    if diff_line:match("^@@") then
      old_line = tonumber(diff_line:match("%-(%d+)"))
      new_line = tonumber(diff_line:match("%+(%d+)"))
      in_hunk = old_line ~= nil and new_line ~= nil
    elseif in_hunk then
      local prefix = diff_line:sub(1, 1)
      if prefix == "-" then
        if old_line == old_target_line then
          return new_line
        end
        old_line = old_line + 1
      elseif prefix == "+" then
        new_line = new_line + 1
      else
        if old_line == old_target_line then
          return new_line
        end
        old_line = old_line + 1
        new_line = new_line + 1
      end
    end
  end
end

M._file_revision = require("diff_review.views.file_revision")



---@param entry DiffReviewStatusEntry?
status_open_pr = function(entry)
  local pr = entry and entry.pr
  if not pr and M._status and M._status.pr and M._status.pr.state == "ready" then
    pr = M._status.pr.pr
  end
  if not pr then
    local pr_state = M._status and M._status.pr
    if pr_state and pr_state.state == "fetching" then
      pr_state.open_when_ready = true
      pr_state.open_when_ready_win = vim.api.nvim_get_current_win()
      return
    end
    if pr_state and pr_state.state == "error" then
      notify_error("GitHub PR lookup failed: " .. (pr_state.message or "Unable to fetch GitHub pull request"), "DiffReview")
      return
    end
    if pr_state and pr_state.state == "unavailable" then
      vim.notify(pr_state.message or "GitHub PR lookup is unavailable", vim.log.levels.INFO, { title = "DiffReview" })
      return
    end
    M._status_confirm_create_pr(function()
      require("github.open_pr").open()
    end)
    return
  end
  M.open_pr(pr, { cwd = M._status and M._status.cwd or nil })
end



-- Seams for the extracted keymaps module.
M._status_open_pr = status_open_pr
M._render_status_or_notify = render_status_or_notify
-- Seams the commit_view module reaches through dr() for about-state and deleted-line jump resolution.
M._status_has_changes = status_has_changes
M._status_ensure_about_state = status_ensure_about_state
M._closest_current_line_for_deleted_diff_line = closest_current_line_for_deleted_diff_line
M._status_primary_key = M._status_keys.primary_key
M._load_pr_diff = load_pr_diff
M._status_command_visible_for_view = keymaps.status_command_visible_for_view
M._status_hint_segments = keymaps.status_hint_segments
M._status_hint_title = keymaps.status_hint_title
M._status_hint_winbar = keymaps.status_hint_winbar
M._status_apply_hint_bar = keymaps.status_apply_hint_bar
M._status_clear_hint_bar = keymaps.status_clear_hint_bar

return M
