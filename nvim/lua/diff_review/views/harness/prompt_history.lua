local M = {}

local client = require("diff_review.harness.client")
local notifications = require("diff_review.infra.notifications")
local session = require("diff_review.session")

local LIMIT = 100

---@return DiffReviewHarnessPresentationState
local function state() return session.harness end

---@param buf integer?
---@return string
local function buffer_text(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return "" end
  return table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
end

---@param buf integer?
---@param text string
local function replace_buffer(buf, text)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
  local win = state().composer_win
  if win and vim.api.nvim_win_is_valid(win) then
    local line_list = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    vim.api.nvim_win_set_cursor(win, { #line_list, #line_list[#line_list] })
  end
end

---@param key string
local function replay_native(key)
  vim.api.nvim_feedkeys(vim.keycode(key), "n", false)
end

---@param prompt_list string[]?
---@return nil
function M.replace(prompt_list)
  local harness = state()
  harness.prompt_history = {}
  for index = 1, math.min(#(prompt_list or {}), LIMIT) do
    harness.prompt_history[index] = prompt_list[index]
  end
  M.reset_navigation()
end

---@return nil
function M.reset_navigation()
  local harness = state()
  harness.prompt_history_index = 0
  harness.prompt_history_draft = nil
end

---@param text string
---@return nil
function M.record(text)
  local harness = state()
  table.insert(harness.prompt_history, 1, text)
  while #harness.prompt_history > LIMIT do table.remove(harness.prompt_history) end
  M.reset_navigation()
  client.request("history.record", { text = text }, function(result, request_error)
    if request_error then
      notifications.error(request_error, "Harness prompt history")
      return
    end
    harness.prompt_history = vim.deepcopy(result or harness.prompt_history)
  end)
end

---@return nil
function M.previous()
  local harness = state()
  if harness.prompt_history_index == 0 then
    local current = buffer_text(harness.composer_buf)
    if current ~= "" then
      replay_native("<Up>")
      return
    end
    harness.prompt_history_draft = current
  end
  if harness.prompt_history_index >= #harness.prompt_history then return end
  harness.prompt_history_index = harness.prompt_history_index + 1
  replace_buffer(harness.composer_buf, harness.prompt_history[harness.prompt_history_index])
end

---@return nil
function M.next()
  local harness = state()
  if harness.prompt_history_index == 0 then
    replay_native("<Down>")
    return
  end
  harness.prompt_history_index = harness.prompt_history_index - 1
  local text = harness.prompt_history_index == 0
      and (harness.prompt_history_draft or "")
      or harness.prompt_history[harness.prompt_history_index]
  replace_buffer(harness.composer_buf, text)
end

---@param buf integer
---@return nil
function M.attach(buf)
  local group = vim.api.nvim_create_augroup("DiffReviewHarnessPromptHistory", { clear = true })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    group = group,
    buffer = buf,
    callback = function()
      local harness = state()
      local selected = harness.prompt_history[harness.prompt_history_index]
      if harness.prompt_history_index > 0 and buffer_text(buf) ~= selected then M.reset_navigation() end
    end,
  })
end

return M
