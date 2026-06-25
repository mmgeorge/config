--- Owns the status-view state lifecycle: view-controller registration, diff-buffer
--- teardown, the per-buffer autocmd state machine that swaps the active status table,
--- initial fold seeding, and the perf payload/event/span instrumentation wrappers.
---
--- Writes the live status state back through the init module via dr(), so callers and
--- autocmd callbacks resolve the same orchestrator tables this module mutates.

local config = require("diff_review.infra.config")

--- Resolve the init module lazily so the lifecycle can read and write orchestrator
--- state without a load-time circular require.
local function dr()
  return require("diff_review")
end
local session = require("diff_review.session")
local diff_buffer = require("diff_review.views.diff_buffer")

local M = {}

--- Build the perf payload for a status buffer from its live state, enriching with
--- cursor, line count, and viewport fields when the buffer is current.
---@param buf integer?
---@param extra? table
---@return table
local function perf_payload(buf, extra)
  local state = buf and session.states and session.states[buf] or nil
  if not state and session.status and (not buf or session.status.buf == buf) then state = session.status end
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
local function perf_event(event, buf, extra)
  local debug = dr()._gitstatus_debug
  local payload = perf_payload(buf, extra)
  if dr()._diff_perf and dr()._diff_perf.enabled() then dr()._diff_perf.event(event, payload) end
  if not (debug and debug.perf_event) then return end
  debug.perf_event(event, payload)
end

---@param event string
---@param buf integer?
---@param extra table?
---@param callback fun(): ...
---@return any
local function perf_span(event, buf, extra, callback)
  local debug = dr()._gitstatus_debug
  local payload = perf_payload(buf, extra)
  if dr()._diff_perf and dr()._diff_perf.enabled() then
    return dr()._diff_perf.span(event, payload, function()
      if not (debug and debug.perf_span and debug.perf_enabled and debug.perf_enabled()) then return callback() end
      return debug.perf_span(event, payload, callback)
    end)
  end
  if not (debug and debug.perf_span and debug.perf_enabled and debug.perf_enabled()) then return callback() end
  return debug.perf_span(event, payload, callback)
end

--- Register a view controller per status-like view kind so command vocabulary and
--- render hooks resolve through a narrow per-view boundary instead of inline branches.
local function register_view_controllers()
  local controllers = dr()._diff_view_controller_model
  local command_sets = dr()._diff_view_command_set_model
  controllers.reset()
  local keymaps = (dr().config or config.options or config.defaults).keymaps or {}
  local after_render_by_kind = {
    status = dr()._status_after_render_status,
    pr = dr()._status_after_render_pr,
    review = dr()._status_after_render_review,
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
  dr()._restore_line_numbers(session.main_win)
  dr()._window_options.reset()
  diff_buffer.cleanup_diff_buffers()
  session.empty_diff_rows = {}
  session.diff_line_content_lengths = {}
  session.main_win = nil
end

---@param buf integer
---@param state table
local function attach_status_state(buf, state)
  dr()._ensure_status_resize_autocmd()
  session.states = session.states or {}
  session.states[buf] = state
  vim.api.nvim_create_autocmd("BufEnter", {
    buffer = buf,
    callback = function()
      perf_span("status.autocmd_buf_enter", buf, nil, function()
        local current = session.states and session.states[buf] or nil
        if current then session.status = current end
        dr()._apply_status_window_options(vim.api.nvim_get_current_win(), current)
        dr()._status_apply_native_folds(buf)
        dr()._status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
      end)
    end,
  })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    buffer = buf,
    callback = function()
      perf_span("status.autocmd_buf_win_enter", buf, nil, function()
        local current = session.states and session.states[buf] or nil
        if current then session.status = current end
        dr()._apply_status_window_options(vim.api.nvim_get_current_win(), current)
        dr()._status_apply_native_folds(buf)
        if current and current.view_kind == "review" and dr()._review and dr()._review.refresh_inline_comment_rules then
          dr()._review.refresh_inline_comment_rules(buf, vim.api.nvim_get_current_win())
        end
        dr()._status_apply_hint_bar(buf, vim.api.nvim_get_current_win())
      end)
    end,
  })
  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buf,
    callback = function()
      perf_span("status.autocmd_cursor_moved", buf, nil, function()
        dr()._normalize_status_cursor(buf)
        if dr()._status_issues then dr()._status_issues.sync_modifiable(buf) end
      end)
    end,
  })
  vim.api.nvim_create_autocmd("ModeChanged", {
    buffer = buf,
    callback = function()
      perf_span("status.autocmd_mode_changed", buf, nil, function()
        dr()._normalize_status_cursor(buf)
        if dr()._status_issues then dr()._status_issues.sync_modifiable(buf) end
      end)
    end,
  })
  if state.view_kind == "status" and dr()._status_issues then
    dr()._status_issues.attach(buf)
  end
  vim.api.nvim_create_autocmd("BufLeave", {
    buffer = buf,
    callback = function()
      dr()._restore_line_numbers(vim.api.nvim_get_current_win())
      dr()._status_clear_hint_bar(vim.api.nvim_get_current_win())
    end,
  })
  vim.api.nvim_create_autocmd("BufWinLeave", {
    buffer = buf,
    callback = function()
      dr()._status_clear_hint_bar(vim.api.nvim_get_current_win())
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
  return section and dr()._status_keys.section_key(section.name) or nil
end

M.register_view_controllers = register_view_controllers
M.cleanup_diff_buffers = cleanup_diff_buffers
M.attach = attach_status_state
M.restore_initial_folds = restore_initial_folds
M.first_grouping_id = first_grouping_id
M.perf_payload = perf_payload
M.perf_event = perf_event
M.perf_span = perf_span

return M
