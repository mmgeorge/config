local M = {}

local fold_presentation = require("diff_review.render.fold_presentation")

---@class DiffReviewPlanFoldRange
---@field id string
---@field start_line integer
---@field end_line integer
---@field default_folded boolean

---@class DiffReviewPlanFoldController
---@field range_by_id table<string, DiffReviewPlanFoldRange>
---@field folded_by_id table<string, boolean>
---@field fold_target_by_line table<integer, string>
local FoldController = {}
FoldController.__index = FoldController

---@param win integer?
---@return boolean
local function valid_window(win)
  return win ~= nil and win > 0 and vim.api.nvim_win_is_valid(win)
end

---@param line_meta_list table[]
---@return table<string, DiffReviewPlanFoldRange>
local function fold_range_by_id(line_meta_list)
  local range_by_id = {}
  for line_number, metadata in ipairs(line_meta_list or {}) do
    if metadata.fold_id then
      range_by_id[metadata.fold_id] = {
        id = metadata.fold_id,
        start_line = line_number,
        end_line = line_number,
        default_folded = metadata.default_folded == true,
      }
    end
    for _, ancestor_id in ipairs(metadata.ancestor_ids or {}) do
      local range = range_by_id[ancestor_id]
      if range then range.end_line = line_number end
    end
  end
  return range_by_id
end

---@param buf integer
---@param win integer?
function FoldController:capture(buf, win)
  if not valid_window(win) or vim.api.nvim_win_get_buf(win) ~= buf then return end
  vim.api.nvim_win_call(win, function()
    for fold_id, range in pairs(self.range_by_id) do
      self.folded_by_id[fold_id] = vim.fn.foldclosed(range.start_line) >= 0
    end
  end)
end

---@param buf integer
---@param win integer?
---@param projection DiffReviewPlanCommentProjection
function FoldController:apply(buf, win, projection)
  if not valid_window(win) or vim.api.nvim_win_get_buf(win) ~= buf then return end
  local range_by_id = fold_range_by_id(projection.line_meta_list)
  local range_list = {}
  local fold_text_by_start_line = {}
  for _, range in pairs(range_by_id) do
    if range.end_line > range.start_line then
      range_list[#range_list + 1] = range
      fold_text_by_start_line[range.start_line] = projection.line_list[range.start_line]
    end
  end
  fold_presentation.replace(buf, fold_text_by_start_line)
  table.sort(range_list, function(left, right)
    local left_span = left.end_line - left.start_line
    local right_span = right.end_line - right.start_line
    if left_span == right_span then return left.start_line > right.start_line end
    return left_span < right_span
  end)

  vim.api.nvim_win_call(win, function()
    local view = vim.fn.winsaveview()
    vim.wo[win].foldmethod = "manual"
    vim.wo[win].foldenable = true
    fold_presentation.apply_window(win)
    vim.cmd("silent! normal! zE")
    for _, range in ipairs(range_list) do
      vim.cmd(("%d,%dfold"):format(range.start_line, range.end_line))
    end
    for index = #range_list, 1, -1 do
      local range = range_list[index]
      local folded = self.folded_by_id[range.id]
      if folded == nil then folded = range.default_folded end
      if not folded then
        vim.api.nvim_win_set_cursor(win, { range.start_line, 0 })
        vim.cmd("silent! foldopen")
      end
    end
    vim.fn.winrestview(view)
  end)
  self.range_by_id = range_by_id
  self.fold_target_by_line = {}
  for line_number, metadata in ipairs(projection.line_meta_list or {}) do
    local fold_target_id = metadata.fold_id or metadata.fold_target_id
    if fold_target_id then self.fold_target_by_line[line_number] = fold_target_id end
  end
end

---Release fold presentation state owned by one Plan Review buffer.
---@param buf integer
function FoldController:detach(buf)
  fold_presentation.clear(buf)
end

---@param buf integer
---@param win integer?
function FoldController:toggle(buf, win)
  if not valid_window(win) or vim.api.nvim_win_get_buf(win) ~= buf then return end
  local cursor_line = vim.api.nvim_win_get_cursor(win)[1]
  local fold_id = self.fold_target_by_line[cursor_line]
  local range = fold_id and self.range_by_id[fold_id] or nil
  if not range then return end
  vim.api.nvim_win_call(win, function()
    local view = vim.fn.winsaveview()
    vim.api.nvim_win_set_cursor(win, { range.start_line, 0 })
    local folded_before_toggle = vim.fn.foldclosed(range.start_line) == range.start_line
    vim.cmd(folded_before_toggle and "silent! normal! zO" or "silent! normal! zc")
    local folded = vim.fn.foldclosed(range.start_line) >= 0
    self.folded_by_id[fold_id] = folded
    if not folded or view.lnum <= range.start_line then
      vim.fn.winrestview(view)
    end
  end)
end

---@return DiffReviewPlanFoldController
function M.new()
  return setmetatable({
    range_by_id = {},
    folded_by_id = {},
    fold_target_by_line = {},
  }, FoldController)
end

return M
