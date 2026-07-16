local PickerSearchInput = {}

local popup_window = require("diff_review.infra.popup_window")

local namespace = vim.api.nvim_create_namespace("DiffReviewPickerSearchInput")

---@param buf integer
local function render_prompt(buf)
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  vim.api.nvim_buf_set_extmark(buf, namespace, 0, 0, {
    virt_text = { { "> ", "DiffReviewPickerKey" } },
    virt_text_pos = "inline",
    right_gravity = false,
  })
end

---@param parent_win integer
---@param frame table
---@param text string
---@param origin DiffReviewPopupOrigin
---@param on_change fun(text: string)
---@return integer, integer
function PickerSearchInput.open(parent_win, frame, text, origin, on_change)
  local parent = vim.api.nvim_win_get_config(parent_win)
  local buf, win = popup_window.open({
    relative = "editor",
    row = (tonumber(parent.row) or 0) + (frame.search_start or 1),
    col = (tonumber(parent.col) or 0) + 2,
    width = math.max(20, parent.width - 4),
    height = 1,
    title = "",
    border = "none",
    filetype = "DiffReviewPickerSearch",
    wrap = false,
    zindex = 91,
    origin = origin,
  })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { text })
  render_prompt(buf)
  vim.api.nvim_win_set_cursor(win, { 1, #text })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    buffer = buf,
    callback = function()
      if not vim.api.nvim_buf_is_valid(buf) then return end
      render_prompt(buf)
      on_change(vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1] or "")
    end,
    desc = "Filter shared picker options from the search editor",
  })
  return buf, win
end

---@param win integer
---@param parent_win integer
---@param frame table
function PickerSearchInput.resize(win, parent_win, frame)
  if not (vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_is_valid(parent_win)) then return end
  local parent = vim.api.nvim_win_get_config(parent_win)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.row = (tonumber(parent.row) or 0) + (frame.search_start or 1)
  window_config.col = (tonumber(parent.col) or 0) + 2
  window_config.width = math.max(20, parent.width - 4)
  window_config.height = 1
  vim.api.nvim_win_set_config(win, window_config)
end

---@param buf integer
---@return string
function PickerSearchInput.text(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return "" end
  return vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1] or ""
end

return PickerSearchInput
