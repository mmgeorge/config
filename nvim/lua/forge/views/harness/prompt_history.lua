local M = {}

local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")
local attachment = require("forge.views.harness.attachment")

---@return ForgeHarnessPresentationState
local function state() return session.harness end

---@param buf integer?
---@return string
local function buffer_text(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return "" end
  return table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
end

---@param buf integer?
---@param draft string|ForgePromptDraft
local function replace_buffer(buf, draft)
  if type(draft) == "string" then draft = { text = draft, attachments = {} } end
  local text = draft.text
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local last_row = vim.api.nvim_buf_line_count(buf) - 1
  local last = vim.api.nvim_buf_get_lines(buf, last_row, last_row + 1, false)[1]
  vim.api.nvim_buf_set_text(buf, 0, 0, last_row, #last, vim.split(text, "\n", { plain = true }))
  attachment.restore(buf, draft.attachments)
  local win = state().composer_win
  if win and vim.api.nvim_win_is_valid(win) then
    local line_list = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    vim.api.nvim_win_set_cursor(win, { #line_list, #line_list[#line_list] })
  end
end

---@param key string
local function replay_native(key)
  vim.api.nvim_feedkeys(vim.keycode(key), "ni", false)
end

---@param harness ForgeHarnessPresentationState
---@param prompt_list (string|ForgePromptDraft)[]
local function adopt_history(harness, prompt_list)
  local selected = harness.prompt_history[harness.prompt_history_index]
  local occurrence = 0
  if selected then
    for index = harness.prompt_history_index, #harness.prompt_history do
      if vim.deep_equal(selected, harness.prompt_history[index]) then occurrence = occurrence + 1 end
    end
  end
  harness.prompt_history = vim.deepcopy(prompt_list)
  if not selected then return end
  for index = #harness.prompt_history, 1, -1 do
    if vim.deep_equal(selected, harness.prompt_history[index]) then
      occurrence = occurrence - 1
      if occurrence == 0 then
        harness.prompt_history_index = index
        return
      end
    end
  end
  harness.prompt_history_index = 0
  harness.prompt_history_draft = nil
end

---@param prompt_list (string|ForgePromptDraft)[]?
---@return nil
function M.replace(prompt_list)
  adopt_history(state(), prompt_list or {})
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
  local draft = attachment.capture(harness.composer_buf)
  if draft.text ~= text then draft = { text = text, attachments = {} } end
  table.insert(harness.prompt_history, 1, draft)
  M.reset_navigation()
  client.request("history.record", draft, function(result, request_error)
    if request_error then
      notifications.error(request_error, "Harness prompt history")
      return
    end
    if result then adopt_history(harness, result) end
  end)
end

---@return nil
function M.previous()
  local harness = state()
  if harness.prompt_history_index == 0 then
    local win = harness.composer_win
    if win and vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_cursor(win)[1] > 1 then
      replay_native("<Up>")
      return
    end
    if #harness.prompt_history == 0 then return end
    harness.prompt_history_draft = attachment.capture(harness.composer_buf)
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
      and (harness.prompt_history_draft or { text = "", attachments = {} })
      or harness.prompt_history[harness.prompt_history_index]
  replace_buffer(harness.composer_buf, text)
end

---@param buf integer
---@return nil
function M.attach(buf)
  local harness = state()
  local group = vim.api.nvim_create_augroup("ForgeHarnessPromptHistory" .. buf, { clear = true })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    group = group,
    buffer = buf,
    callback = function()
      local selected = harness.prompt_history[harness.prompt_history_index]
      if type(selected) == "table" then selected = selected.text end
      if harness.prompt_history_index > 0 and buffer_text(buf) ~= selected then
        harness.prompt_history_index = 0
        harness.prompt_history_draft = nil
      end
    end,
  })
end

return M
