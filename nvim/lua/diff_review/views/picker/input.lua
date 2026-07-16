local PickerInput = {}

local popup_window = require("diff_review.infra.popup_window")
local input_gutter = require("diff_review.shared.input_gutter")

---@param frame table
---@return integer
local function input_height(frame)
  return math.max(3, math.min(6, frame.input_height or 3))
end

---@param parent_win integer
---@param frame table
---@param text string
---@param origin DiffReviewPopupOrigin
---@return integer, integer
function PickerInput.open(parent_win, frame, text, origin)
  local parent = vim.api.nvim_win_get_config(parent_win)
  local buf, win = popup_window.open({
    relative = "editor",
    row = (tonumber(parent.row) or 0) + (frame.input_start or 1),
    col = (tonumber(parent.col) or 0) + 2,
    width = math.max(20, parent.width - 4),
    height = input_height(frame),
    title = "",
    border = "none",
    filetype = "DiffReviewPickerInput",
    wrap = true,
    linebreak = true,
    zindex = 91,
    origin = origin,
  })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text or "", "\n", { plain = true }))
  input_gutter.apply(win)
  return buf, win
end

---@param win integer
---@param parent_win integer
---@param frame table
function PickerInput.resize(win, parent_win, frame)
  if not (vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_is_valid(parent_win)) then return end
  local parent = vim.api.nvim_win_get_config(parent_win)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.row = (tonumber(parent.row) or 0) + (frame.input_start or 1)
  window_config.col = (tonumber(parent.col) or 0) + 2
  window_config.width = math.max(20, parent.width - 4)
  window_config.height = input_height(frame)
  vim.api.nvim_win_set_config(win, window_config)
end

---@param buf integer
---@return string
function PickerInput.text(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return "" end
  return vim.trim(table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n"))
end

return PickerInput
