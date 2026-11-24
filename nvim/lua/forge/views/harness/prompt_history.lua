local M = {}

local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")
local keymaps = require("forge.shared.keymaps")
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
local function reset_navigation(harness)
  harness.prompt_history_index = 0
  harness.prompt_history_draft = nil
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
  reset_navigation(harness)
end

---@param prompt_list (string|ForgePromptDraft)[]?
---@return nil
function M.replace(prompt_list)
  adopt_history(state(), prompt_list or {})
end

---Record a prompt as the newest history entry.
---
---A string takes its attachments from the composer when the composer still shows that text. A
---draft, such as one saved outside the composer, is recorded as given.
---@param entry string|ForgePromptDraft
---@return nil
function M.record(entry)
  local harness = state()
  local draft
  if type(entry) == "table" then draft = { text = entry.text, attachments = entry.attachments or {} }
  else
    draft = attachment.capture(harness.composer_buf)
    if draft.text ~= entry then draft = { text = entry, attachments = {} } end
  end
  table.insert(harness.prompt_history, 1, draft)
  reset_navigation(harness)
  client.request({ session_id = harness.session.id, method = "session.prompt.history.record", params = draft }, function(result, request_error)
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
    local buf = harness.composer_buf
    if buf and vim.api.nvim_buf_is_valid(buf)
        and (vim.api.nvim_buf_line_count(buf) > 1 or vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1] ~= "") then
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
  local namespace = vim.api.nvim_create_namespace("ForgeHarnessPromptHistory" .. buf)
  local navigation_keys = {}
  for _, command in ipairs({ "history_previous", "history_next" }) do
    for _, key in ipairs(keymaps.view_keys_for("harness", command)) do
      navigation_keys[vim.keycode(key)] = true
    end
  end
  vim.on_key(function(_, typed)
    if harness.prompt_history_index > 0 and typed ~= "" and not navigation_keys[typed]
        and vim.api.nvim_get_current_buf() == buf then
      reset_navigation(harness)
    end
  end, namespace)
  vim.api.nvim_create_autocmd({ "BufLeave", "WinLeave" }, {
    group = group,
    buffer = buf,
    callback = function() reset_navigation(harness) end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    group = group,
    buffer = buf,
    once = true,
    callback = function() vim.on_key(nil, namespace) end,
  })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "TextChangedP" }, {
    group = group,
    buffer = buf,
    callback = function()
      local selected = harness.prompt_history[harness.prompt_history_index]
      if type(selected) == "table" then selected = selected.text end
      if harness.prompt_history_index > 0 and buffer_text(buf) ~= selected then
        reset_navigation(harness)
      end
    end,
  })
end

return M
