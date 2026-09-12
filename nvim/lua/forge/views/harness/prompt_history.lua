local M = {}

local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")

local LIMIT = 100

---@return ForgeHarnessPresentationState
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
  local last_row = vim.api.nvim_buf_line_count(buf) - 1
  local last = vim.api.nvim_buf_get_lines(buf, last_row, last_row + 1, false)[1]
  vim.api.nvim_buf_set_text(buf, 0, 0, last_row, #last, vim.split(text, "\n", { plain = true }))
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
  local normalized_prompt_list = prompt_list or {}
  harness.prompt_history = {}
  for index = 1, math.min(#normalized_prompt_list, LIMIT) do
    harness.prompt_history[index] = normalized_prompt_list[index]
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
  local group = vim.api.nvim_create_augroup("ForgeHarnessPromptHistory", { clear = true })
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
