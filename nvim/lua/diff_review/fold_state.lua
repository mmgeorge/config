--- Owns fold state for the status views: the per-key fold map (over status_buffer), the registered
--- fold ranges and native-fold application, the foldtext callback, and the window-resize refresh
--- that re-applies folds and view-specific modifiable/comment-rule sync.
---
--- Reads live status state and the view modules (pr_edit/review/status_issues), window options, and
--- perf span through the init module via dr().

local status_buffer = require("diff_review.status_buffer")

--- Resolve the init module lazily so fold application can reach orchestrator state and the view
--- sync/window-option seams without a load-time circular require.
local function dr()
  return require("diff_review")
end

local M = {}

local function status_folded(key, default, state)
  return status_buffer.folded(state or dr()._status or {}, key, default)
end

local function set_status_folded(key, folded, state)
  if not state then
    dr()._status = dr()._status or {}
    state = dr()._status
  end
  return status_buffer.set_folded(state, key, folded)
end

---@param value any
---@return string
function M._status_fold_text(value)
  if type(value) == "function" then
    local ok, text = pcall(value)
    if ok then return tostring(text or "") end
  elseif value ~= nil then
    return tostring(value)
  end
  local fold_start = tonumber(vim.v.foldstart) or vim.fn.line(".")
  return vim.fn.getline(fold_start)
end

function _G.diff_review_status_foldtext()
  local buf = vim.api.nvim_get_current_buf()
  local state = dr()._status_states and dr()._status_states[buf] or dr()._status
  local fold_start = tonumber(vim.v.foldstart) or vim.fn.line(".")
  local value = state and state.fold_text_by_start_line and state.fold_text_by_start_line[fold_start] or nil
  return dr()._status_fold_text(value)
end

---@param fold_id string?
---@param start_line integer?
---@param end_line integer?
---@param default_folded boolean?
---@param fold_text any
function M._status_register_fold_range(fold_id, start_line, end_line, default_folded, fold_text)
  if not (fold_id and start_line and end_line and end_line > start_line) then return end
  local status = dr()._status
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

---@param state table?
---@param fold_id string?
---@return table[]
function M._status_fold_ranges_for_id(state, fold_id)
  local value = state and state.fold_ranges_by_id and fold_id and state.fold_ranges_by_id[fold_id] or nil
  if not value then return {} end
  if value.start_line then return { value } end
  return value
end

---@param range table
---@return integer
function M._status_fold_range_span(range)
  return (tonumber(range.end_line) or 0) - (tonumber(range.start_line) or 0)
end

---@param view table
---@param ranges table[]
---@param state table?
---@return table
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

---@param buf integer
---@param fold_id string
---@param _folded boolean
---@return boolean
function M._status_set_native_fold_state(buf, fold_id, _folded)
  local state = dr()._status_states and dr()._status_states[buf] or dr()._status
  if type(fold_id) == "string"
    and (fold_id:find("^hunk:")
      or fold_id:find("^commit%-hunk:")
      or fold_id:find("^provider%-hunk:")) then
    return false
  end
  local ranges = dr()._status_fold_ranges_for_id(state, fold_id)
  if #ranges == 0 then return false end
  local has_native_range = false
  for _, range in ipairs(ranges) do
    if dr()._status_fold_range_span(range) > 0 then
      has_native_range = true
      break
    end
  end
  if not has_native_range then return false end
  dr()._status_apply_native_folds(buf)
  return true
end

---@param buf integer?
---@param ranges table[]
---@param win integer?
---@return boolean?
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

---@param buf integer
function M._status_apply_native_folds(buf)
  local state = dr()._status_states and dr()._status_states[buf] or dr()._status
  if not (state and state.fold_range_order and vim.api.nvim_buf_is_valid(buf)) then return end
  return dr()._status_perf_span("status.apply_native_folds", buf, {
    fold_ranges = #(state.fold_range_order or {}),
  }, function()
    local max_line = vim.api.nvim_buf_line_count(buf)
    local ranges = {}
    for _, range in ipairs(state.fold_range_order) do
      ranges[#ranges + 1] = range
    end
    table.sort(ranges, function(left, right)
      local left_span = dr()._status_fold_range_span(left)
      local right_span = dr()._status_fold_range_span(right)
      if left_span == right_span then return (left.start_line or 0) > (right.start_line or 0) end
      return left_span < right_span
    end)
    for _, win in ipairs(vim.fn.win_findbuf(buf)) do
      if vim.api.nvim_win_is_valid(win) then
        dr()._apply_status_window_options(win, state)
        vim.api.nvim_win_call(win, function()
          local view = vim.fn.winsaveview()
          pcall(vim.cmd, "normal! zE")
          for range_index = #ranges, 1, -1 do
            local range = ranges[range_index]
            if range.start_line >= 1
              and range.end_line <= max_line
              and range.end_line > range.start_line
              and status_folded(range.id, range.default_folded, state) then
              pcall(vim.cmd, ("%d,%dfold"):format(range.start_line, range.end_line))
            end
          end
          vim.fn.winrestview(dr()._status_view_for_fold_restore(view, ranges, state))
        end)
      end
    end
  end)
end

---@param buf integer
function M._status_sync_after_native_folds(buf)
  local state = dr()._status_states and dr()._status_states[buf] or dr()._status
  if not state then return end
  return dr()._status_perf_span("status.sync_after_native_folds", buf, nil, function()
    if state.view_kind == "pr" then
      dr()._status_perf_span("status.native_folds.pr_sync_modifiable", buf, nil, function()
        dr()._pr_edit.sync_modifiable(buf)
      end)
    elseif state.view_kind == "review" then
      dr()._status_perf_span("status.native_folds.review_sync_modifiable", buf, nil, function()
        dr()._review.sync_modifiable(buf)
      end)
    elseif state.view_kind == "status" then
      dr()._status_perf_span("status.native_folds.issues_sync_modifiable", buf, nil, function()
        dr()._status_issues.sync_modifiable(buf)
      end)
    end
  end)
end

---@param buf integer
function M._status_schedule_native_folds(buf)
  local state = dr()._status_states and dr()._status_states[buf] or dr()._status
  if not (state and vim.api.nvim_buf_is_valid(buf)) then return end
  state.native_fold_generation = (state.native_fold_generation or 0) + 1
  local generation = state.native_fold_generation
  vim.defer_fn(function()
    local latest = dr()._status_states and dr()._status_states[buf] or dr()._status
    if not (latest and latest.native_fold_generation == generation and vim.api.nvim_buf_is_valid(buf)) then return end
    dr()._status_perf_span("status.native_folds_deferred", buf, { generation = generation }, function()
      dr()._status = latest
      dr()._status_apply_native_folds(buf)
      dr()._status_sync_after_native_folds(buf)
    end)
  end, 20)
end

function M._refresh_status_windows_after_resize()
  if not dr()._status_states then return end
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_is_valid(win) then
      local buf = vim.api.nvim_win_get_buf(win)
      local state = dr()._status_states[buf]
      if state then
        dr()._apply_status_window_options(win, state)
        dr()._status_apply_hint_bar(buf, win)
        if state.view_kind == "review" and dr()._review and dr()._review.refresh_inline_comment_rules then
          local width = dr()._review.comment_rule_width(win, buf)
          if state.review_comment_rule_width ~= width then
            state.review_comment_rule_width = width
            dr()._review.refresh_inline_comment_rules(buf, win)
          end
        end
      end
    end
  end
end

function M._ensure_status_resize_autocmd()
  if dr()._status_resize_autocmd then return end
  local group = vim.api.nvim_create_augroup("DiffReviewStatusResize", { clear = true })
  dr()._status_resize_autocmd = vim.api.nvim_create_autocmd({ "WinResized", "VimResized" }, {
    group = group,
    callback = function()
      dr()._refresh_status_windows_after_resize()
    end,
  })
end

-- Expose the bare-local fold accessors under their original seam names.
M._status_folded = status_folded
M._set_status_folded = set_status_folded

return M
