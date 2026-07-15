local M = {}

local popup_window = require("diff_review.infra.popup_window")

local padding_namespace = vim.api.nvim_create_namespace("DiffReviewHarnessQuestionInputPadding")

---@param buf integer
local function apply_padding(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  vim.api.nvim_buf_clear_namespace(buf, padding_namespace, 0, -1)
  for row = 0, vim.api.nvim_buf_line_count(buf) - 1 do
    vim.api.nvim_buf_set_extmark(buf, padding_namespace, row, 0, {
      right_gravity = false,
      virt_text = { { " ", "Normal" } },
      virt_text_pos = "inline",
    })
  end
end

---@param transcript_win integer
---@param question_win integer
---@param title string
---@param text string
---@return integer, integer
function M.open(transcript_win, question_win, title, text)
  local question_config = vim.api.nvim_win_get_config(question_win)
  local transcript_height = vim.api.nvim_win_get_height(transcript_win)
  local question_row = tonumber(question_config.row) or 0
  local question_height = vim.api.nvim_win_get_height(question_win)
  local available_height = transcript_height - question_row - question_height - 2
  local height = math.max(1, math.min(5, available_height))
  local buf, win = popup_window.open({
    relative = "win",
    win = transcript_win,
    row = question_row + question_height + 2,
    col = tonumber(question_config.col) or 0,
    width = vim.api.nvim_win_get_width(question_win),
    height = height,
    title = title,
    filetype = "DiffReviewHarnessQuestionInput",
    wrap = true,
    linebreak = true,
    zindex = 81,
  })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
  apply_padding(buf)
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    buffer = buf,
    callback = function() apply_padding(buf) end,
    desc = "Maintain Harness question input padding",
  })
  return buf, win
end

---@param win integer
---@param question_win integer
---@param title string
function M.retarget(win, question_win, title)
  if not (vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_is_valid(question_win)) then return end
  local question_config = vim.api.nvim_win_get_config(question_win)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.row = (tonumber(question_config.row) or 0) + vim.api.nvim_win_get_height(question_win) + 2
  window_config.col = tonumber(question_config.col) or 0
  window_config.width = vim.api.nvim_win_get_width(question_win)
  window_config.title = " " .. title .. " "
  vim.api.nvim_win_set_config(win, window_config)
end

---@param buf integer
---@return string
function M.text(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return "" end
  return vim.trim(table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n"))
end

---@param buf integer
---@param text string
function M.set_text(buf, text)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
  apply_padding(buf)
end

---@param win integer?
function M.close(win)
  popup_window.close(win)
end

return M
