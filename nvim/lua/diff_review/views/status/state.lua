--- Owns the status-view state lifecycle: view-controller registration, diff-buffer
--- teardown, the per-buffer autocmd state machine that swaps the active status table,
--- initial fold seeding, and the perf payload/event/span instrumentation wrappers.
---
--- Writes the live status state back through session.lua, so callers and
--- autocmd callbacks resolve the same orchestrator tables this module mutates.

local config = require("diff_review.infra.config")

local view_controller = require("diff_review.shared.view_controller")
local view_command_set = require("diff_review.shared.view_command_set")
-- render_orchestrator edge kept lazy to avoid a load-time cycle.
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
local review = require("diff_review.views.pr.review")
local status_issues = require("diff_review.views.status.status_issues")
local status_keys = require("diff_review.views.status.status_keys")
local window_options = require("diff_review.views.status.window_options")
local fold_state = require("diff_review.views.status.fold_state")
local keymaps = require("diff_review.shared.keymaps")
local session = require("diff_review.session")
local diff_buffer = require("diff_review.views.diff_buffer")
local trace = require("diff_review.infra.perf_trace")

local M = {}

-- Perf tracing (payload/event/span) now lives in infra/perf_trace.lua (trace.*).

--- Register a view controller per status-like view kind so command vocabulary and
--- render hooks resolve through a narrow per-view boundary instead of inline branches.
local function register_view_controllers()
  local controllers = view_controller
  local command_sets = view_command_set
  controllers.reset()
  local keymaps = (config.options or config.options or config.defaults).keymaps or {}
  local after_render_by_kind = {
    status = render_orchestrator().after_render_status,
    pr = render_orchestrator().after_render_pr,
    review = render_orchestrator().after_render_review,
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

--- Close and wipe all diff buffers
local function cleanup_diff_buffers()
  window_options.restore(session.main_win)
  window_options.reset()
  diff_buffer.cleanup_diff_buffers()
  session.empty_diff_rows = {}
  session.diff_line_content_lengths = {}
  session.main_win = nil
end

---@param buf integer
---@param state table
local function attach_status_state(buf, state)
  fold_state._ensure_status_resize_autocmd()
  session.states = session.states or {}
  session.states[buf] = state
  vim.api.nvim_create_autocmd("BufEnter", {
    buffer = buf,
    callback = function()
      trace.span("status.autocmd_buf_enter", buf, nil, function()
        local current = session.states and session.states[buf] or nil
        if current then session.status = current end
        window_options.apply(vim.api.nvim_get_current_win(), current)
        fold_state._status_apply_native_folds(buf)
        keymaps.status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
      end)
    end,
  })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    buffer = buf,
    callback = function()
      trace.span("status.autocmd_buf_win_enter", buf, nil, function()
        local current = session.states and session.states[buf] or nil
        if current then session.status = current end
        window_options.apply(vim.api.nvim_get_current_win(), current)
        fold_state._status_apply_native_folds(buf)
        if current and current.view_kind == "review" and review and review.refresh_inline_comment_rules then
          review.refresh_inline_comment_rules(buf, vim.api.nvim_get_current_win())
        end
        keymaps.status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
      end)
    end,
  })
  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buf,
    callback = function()
      trace.span("status.autocmd_cursor_moved", buf, nil, function()
        diff_buffer._normalize_status_cursor(buf)
        if status_issues then status_issues.sync_modifiable(buf) end
      end)
    end,
  })
  vim.api.nvim_create_autocmd("ModeChanged", {
    buffer = buf,
    callback = function()
      trace.span("status.autocmd_mode_changed", buf, nil, function()
        diff_buffer._normalize_status_cursor(buf)
        if status_issues then status_issues.sync_modifiable(buf) end
      end)
    end,
  })
  if state.view_kind == "status" and status_issues then
    status_issues.attach(buf)
  end
  vim.api.nvim_create_autocmd("BufLeave", {
    buffer = buf,
    callback = function()
      window_options.restore(vim.api.nvim_get_current_win())
      keymaps.status_clear_hint_bar(vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("BufWinLeave", {
    buffer = buf,
    callback = function()
      keymaps.status_clear_hint_bar(vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buf,
    callback = function()
      if session.states then session.states[buf] = nil end
      if session.main_status == state then session.main_status = nil end
      if session.status == state then session.status = session.main_status end
      if session.diff_line_content_lengths then session.diff_line_content_lengths[buf] = nil end
    end,
  })
end

---@param sections DiffReviewStatusSection[]?
local function restore_initial_folds(sections)
  session.status = session.status or {}
  session.status.folds = {}
end

---@param sections DiffReviewStatusSection[]?
---@return string?
local function first_grouping_id(sections)
  local section = sections and sections[1] or nil
  return section and status_keys.section_key(section.name) or nil
end

M.register_view_controllers = register_view_controllers
M.cleanup_diff_buffers = cleanup_diff_buffers
M.attach = attach_status_state
M.restore_initial_folds = restore_initial_folds
M.first_grouping_id = first_grouping_id

return M
