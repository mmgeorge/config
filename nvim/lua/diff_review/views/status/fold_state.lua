--- Owns fold state for the status views: the per-key fold map (over status_buffer), the registered
--- fold ranges and native-fold application, the foldtext callback, and the window-resize refresh
--- that re-applies folds and view-specific modifiable/comment-rule sync.
---
--- Reads live status state and the view modules (pr_edit/review/status_issues), window options, and
--- perf span via session.lua and direct requires.

local status_buffer = require("diff_review.views.status.status_buffer")
local comment_box_rows = require("diff_review.views.status.comment_box_rows")
local fold_presentation = require("diff_review.render.fold_presentation")

local pr_edit = require("diff_review.views.pr.pr_edit")
-- review edge kept lazy to avoid a load-time cycle.
local function review() return require("diff_review.views.pr.review") end
-- render_orchestrator edge kept lazy to avoid a load-time cycle.
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
local status_issues = require("diff_review.views.status.status_issues")
local window_options = require("diff_review.views.status.window_options")
-- keymaps edge kept lazy to avoid a load-time cycle.
local function keymaps() return require("diff_review.shared.keymaps") end
local trace = require("diff_review.infra.perf_trace")
-- Status WinResized/VimResized autocmd id, registered once (module-private state).
local resize_autocmd
local session = require("diff_review.session")

local M = {}

--- Checks whether an entry is recorded as folded in session state or fallback default.
---@param key string Entry key string.
---@param default? boolean Default fold state boolean.
---@param state? table Optional status session state table.
---@return boolean folded True if entry is folded.
local function status_folded(key, default, state)
  return status_buffer.folded(state or session.status or {}, key, default)
end

--- Updates the persistent fold state for an entry key.
---@param key string Entry key string.
---@param folded boolean Next fold state boolean.
---@param state? table Optional status session state table.
---@return boolean folded New fold state boolean.
local function set_status_folded(key, folded, state)
  if not state then
    session.status = session.status or {}
    state = session.status
  end
  return status_buffer.set_folded(state, key, folded)
end

--- Resolves the formatted fold text string for a folded header line.
---@param value any Raw fold text or generator value.
---@return any resolved Evaluated fold text string or structure.
function M._status_fold_text(value)
  return fold_presentation.resolve(value)
end

--- Registers a buffer line range as a collapsible native fold region.
---@param fold_id string? Target entry identifier string.
---@param start_line integer? One-based starting buffer line index.
---@param end_line integer? One-based ending buffer line index.
---@param default_folded boolean? Default collapsed state boolean.
---@param fold_text any Custom fold text string or format descriptor.
function M._status_register_fold_range(fold_id, start_line, end_line, default_folded, fold_text)
  if not (fold_id and start_line and end_line and end_line > start_line) then return end
  local status = session.status
  if not status then return end
  status.fold_ranges_by_id = status.fold_ranges_by_id or {}
  status.fold_range_order = status.fold_range_order or {}
  status.fold_text_by_start_line = status.fold_text_by_start_line or {}
  local range = {
    id = fold_id,
    start_line = start_line,
    end_line = end_line,
    default_folded = default_folded == true,
    fold_text = fold_text,
  }
  local existing = status.fold_ranges_by_id[fold_id]
  if not existing then
    status.fold_ranges_by_id[fold_id] = { range }
  elseif existing.start_line then
    status.fold_ranges_by_id[fold_id] = { existing, range }
  else
    existing[#existing + 1] = range
  end
  status.fold_range_order[#status.fold_range_order + 1] = range
  status.fold_text_by_start_line[start_line] = fold_text
end

--- Retrieves all registered fold ranges associated with an entry identifier.
---@param state table? Status session state table.
---@param fold_id string? Target entry identifier string.
---@return table[] ranges Array of fold range descriptor tables.
function M._status_fold_ranges_for_id(state, fold_id)
  local value = state and state.fold_ranges_by_id and fold_id and state.fold_ranges_by_id[fold_id] or nil
  if not value then return {} end
  if value.start_line then return { value } end
  return value
end

--- Computes the line span distance for a fold range.
---@param range table Fold range descriptor table.
---@return integer span Number of lines enclosed by the range.
function M._status_fold_range_span(range)
  return (tonumber(range.end_line) or 0) - (tonumber(range.start_line) or 0)
end

--- Adjusts saved window view cursor position to fold header lines when closed.
---@param view table Saved window view dictionary (`winsaveview()`).
---@param ranges table[] Array of fold range descriptor tables.
---@param state table? Optional status session state table.
---@return table view Adjusted window view dictionary.
function M._status_view_for_fold_restore(view, ranges, state)
  local line = view and tonumber(view.lnum) or nil
  if not line then return view end
  local adjusted = nil
  for _, range in ipairs(ranges or {}) do
    local start_line = range and range.start_line or nil
    local end_line = range and range.end_line or nil
    if start_line and end_line and line > start_line and line <= end_line and status_folded(range.id, range.default_folded, state) then
      adjusted = adjusted or vim.deepcopy(view)
      adjusted.lnum = start_line
      adjusted.col = 0
      adjusted.curswant = 0
    end
  end
  return adjusted or view
end

--- Updates native Vim fold states for an entry's registered line ranges.
---@param buf integer Status buffer handle.
---@param fold_id string Target entry identifier string.
---@param _folded boolean New fold state boolean.
---@return boolean applied True if native fold ranges were found and applied.
function M._status_set_native_fold_state(buf, fold_id, _folded)
  local state = session.states and session.states[buf] or session.status
  local ranges = M._status_fold_ranges_for_id(state, fold_id)
  if #ranges == 0 then return false end
  local has_native_range = false
  for _, range in ipairs(ranges) do
    if M._status_fold_range_span(range) > 0 then
      has_native_range = true
      break
    end
  end
  if not has_native_range then return false end
  M._status_apply_native_folds(buf)
  return true
end

--- Checks if an entry's child content has been expanded and populated.
---@param state table? Status session state table.
---@param fold_id string Target entry identifier string.
---@return boolean materialized True if entry content has been generated.
function M._status_entry_materialized(state, fold_id)
  return state ~= nil and state.materialized_entry_by_id ~= nil and state.materialized_entry_by_id[fold_id] == true
end

--- Records that an entry's child content has been generated.
---@param state table? Status session state table.
---@param fold_id string Target entry identifier string.
function M._status_set_entry_materialized(state, fold_id)
  if not state then return end
  state.materialized_entry_by_id = state.materialized_entry_by_id or {}
  state.materialized_entry_by_id[fold_id] = true
end

--- Checks whether an entry has at least one valid native fold range.
---@param state table? Status session state table.
---@param fold_id string Target entry identifier string.
---@return boolean has_range True if at least one multi-line fold range exists.
function M._status_has_native_fold_range(state, fold_id)
  for _, range in ipairs(M._status_fold_ranges_for_id(state, fold_id)) do
    if M._status_fold_range_span(range) > 0 then return true end
  end
  return false
end

--- Checks live Neovim native fold state for a range using `foldclosed`.
---@param buf integer? Status buffer handle.
---@param ranges table[] Array of fold range descriptor tables.
---@param win integer? Window handle displaying the buffer.
---@return boolean? folded True if the first range is closed, or nil if invalid.
function M._status_native_folded(buf, ranges, win)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and ranges and ranges[1]) then return nil end
  if not (win and vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == buf) then
    win = vim.fn.bufwinid(buf)
  end
  if win == -1 or not vim.api.nvim_win_is_valid(win) then return nil end
  local range = ranges[1]
  if not (range.start_line and range.end_line and range.end_line > range.start_line) then return nil end
  return vim.api.nvim_win_call(win, function()
    return vim.fn.foldclosed(range.start_line) ~= -1
  end)
end

--- Applies registered native folds, foldtext formatting, and window options to the buffer.
---@param buf integer Status buffer handle.
function M._status_apply_native_folds(buf)
  local state = session.states and session.states[buf] or session.status
  if not (state and state.fold_range_order and vim.api.nvim_buf_is_valid(buf)) then return end
  fold_presentation.replace(buf, state.fold_text_by_start_line)
  return trace.span("status.apply_native_folds", buf, {
    fold_ranges = #(state.fold_range_order or {}),
  }, function()
    local max_line = vim.api.nvim_buf_line_count(buf)
    local ranges = {}
    for _, range in ipairs(state.fold_range_order) do
      ranges[#ranges + 1] = range
    end
    table.sort(ranges, function(left, right)
      local left_span = M._status_fold_range_span(left)
      local right_span = M._status_fold_range_span(right)
      if left_span == right_span then return (left.start_line or 0) > (right.start_line or 0) end
      return left_span < right_span
    end)
    for _, win in ipairs(vim.fn.win_findbuf(buf)) do
      if vim.api.nvim_win_is_valid(win) then
        window_options.apply(win, state)
        vim.api.nvim_win_call(win, function()
          local view = vim.fn.winsaveview()
          pcall(vim.cmd, "normal! zE")
          local folded_ranges = {}
          for range_index = 1, #ranges do
            local range = ranges[range_index]
            if range.start_line >= 1
              and range.end_line <= max_line
              and range.end_line > range.start_line
              and status_folded(range.id, range.default_folded, state) then
              pcall(vim.cmd, ("%d,%dfold"):format(range.start_line, range.end_line))
              folded_ranges[#folded_ranges + 1] = range
            end
          end
          for _, range in ipairs(folded_ranges) do
            pcall(vim.cmd, ("%dfoldclose"):format(range.start_line))
          end
          vim.fn.winrestview(M._status_view_for_fold_restore(view, ranges, state))
        end)
      end
    end
  end)
end

--- Synchronizes buffer modifiable and issue state across PR, review, and status views.
---@param buf integer Status buffer handle.
function M._status_sync_after_native_folds(buf)
  local state = session.states and session.states[buf] or session.status
  if not state then return end
  return trace.span("status.sync_after_native_folds", buf, nil, function()
    if state.view_kind == "pr" then
      trace.span("status.native_folds.pr_sync_modifiable", buf, nil, function()
        pr_edit.sync_modifiable(buf)
      end)
    elseif state.view_kind == "review" then
      trace.span("status.native_folds.review_sync_modifiable", buf, nil, function()
        review().sync_modifiable(buf)
      end)
    elseif state.view_kind == "status" then
      trace.span("status.native_folds.issues_sync_modifiable", buf, nil, function()
        status_issues.sync_modifiable(buf)
      end)
    end
  end)
end

--- Debounces and schedules asynchronous native fold application.
---@param buf integer Status buffer handle.
function M._status_schedule_native_folds(buf)
  local state = session.states and session.states[buf] or session.status
  if not (state and vim.api.nvim_buf_is_valid(buf)) then return end
  state.native_fold_generation = (state.native_fold_generation or 0) + 1
  local generation = state.native_fold_generation
  vim.defer_fn(function()
    local latest = session.states and session.states[buf] or session.status
    if not (latest and latest.native_fold_generation == generation and vim.api.nvim_buf_is_valid(buf)) then return end
    trace.span("status.native_folds_deferred", buf, { generation = generation }, function()
      session.status = latest
      M._status_apply_native_folds(buf)
      M._status_sync_after_native_folds(buf)
    end)
  end, 20)
end

--- Re-applies layout widths, comment rules, and native folds across status windows on resize.
function M._refresh_status_windows_after_resize()
  if not session.states then return end
  local refreshed_buffers = {}
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_is_valid(win) then
      local buf = vim.api.nvim_win_get_buf(win)
      local state = session.states[buf]
      if state then
        window_options.apply(win, state)
        keymaps().status_apply_hint_bar(buf, win)
        if not refreshed_buffers[buf] then
          refreshed_buffers[buf] = true
          local comment_layout_changed = comment_box_rows.refresh_buffer(state)
          if comment_layout_changed then
            if state.view_kind == "pr" or state.view_kind == "review" then
              review().render_preserving_inline_cursor(buf)
            else
              render_orchestrator().render_status_or_notify(buf, nil, nil, { reuse_sections = true })
            end
          else
            M._status_apply_native_folds(buf)
          end
          local walkthrough = package.loaded["diff_review.views.walkthrough"]
          if walkthrough and walkthrough.on_status_rendered then walkthrough.on_status_rendered(buf) end
        end
        if state.view_kind == "review" and review and review().refresh_inline_comment_rules then
          local width = review().comment_rule_width(win, buf)
          if state.review_comment_rule_width ~= width then
            state.review_comment_rule_width = width
            review().refresh_inline_comment_rules(buf, win)
          end
        end
      end
    end
  end
end

--- Ensures the global window resize autocommands are registered.
function M._ensure_status_resize_autocmd()
  if resize_autocmd then return end
  local group = vim.api.nvim_create_augroup("DiffReviewStatusResize", { clear = true })
  resize_autocmd = vim.api.nvim_create_autocmd({ "WinResized", "VimResized" }, {
    group = group,
    callback = function()
      M._refresh_status_windows_after_resize()
    end,
  })
end

-- Expose the bare-local fold accessors under their original seam names.
M._status_folded = status_folded
M._set_status_folded = set_status_folded

return M
