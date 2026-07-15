---@module 'blink.cmp'
local CommandSource = {}
local session = require("diff_review.session")

local command_list = {
  { label = "/agent", detail = "Select or spawn a child agent", capability = "agent" },
  { label = "/plan", detail = "Create a reviewed plan" },
  { label = "/plan cancel", detail = "Cancel the active plan" },
  { label = "/questions", detail = "Reopen pending planning questions" },
  { label = "/goal", detail = "Set a persistent goal" },
  { label = "/goal pause", detail = "Pause automatic goal continuation" },
  { label = "/goal resume", detail = "Resume automatic goal continuation" },
  { label = "/goal clear", detail = "Clear the active goal" },
  { label = "/read", detail = "Allow reads and network, deny local writes" },
  { label = "/write", detail = "Allow workspace writes" },
  { label = "/full", detail = "Allow machine-wide writes with approvals" },
  { label = "/yolo", detail = "Allow machine-wide writes without approvals" },
  { label = "/clear", detail = "Start a new session" },
  { label = "/compact", detail = "Compact provider-owned context", capability = "native_compact" },
  { label = "/rename", detail = "Rename the current session" },
  { label = "/effort", detail = "Select reasoning effort" },
  { label = "/model", detail = "Select the backend model" },
  { label = "/fast", detail = "Toggle Codex fast mode" },
  { label = "/fast on", detail = "Enable Codex fast mode" },
  { label = "/fast off", detail = "Disable Codex fast mode" },
}

function CommandSource.new()
  return setmetatable({}, { __index = CommandSource })
end

function CommandSource:get_trigger_characters()
  return { "/", " " }
end

function CommandSource:enabled()
  local line = vim.api.nvim_get_current_line()
  local cursor_column = vim.api.nvim_win_get_cursor(0)[2]
  return line:sub(1, cursor_column):match("^%s*/.*$") ~= nil
end

function CommandSource:get_completions(_, callback)
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local cursor_column = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local prefix = line:sub(1, cursor_column)
  local command_start = prefix:find("/")
  local items = {}
  local definition_start = prefix:match("^%s*/agent%s+()([^%s]*)$")
  if definition_start then
    local capability = session.harness.capability or {}
    if capability.agent and capability.agent.catalog then
      for _, definition in ipairs((session.harness.agent or {}).definition or {}) do
        items[#items + 1] = {
          label = definition.name,
          filterText = definition.name,
          detail = definition.description,
          kind = vim.lsp.protocol.CompletionItemKind.Value,
          textEdit = {
            newText = definition.name,
            range = {
              start = { line = row, character = definition_start - 1 },
              ["end"] = { line = row, character = cursor_column },
            },
          },
        }
      end
    end
    callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
    return
  end
  if prefix:match("^%s*/agent%s+") then
    callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
    return
  end
  if command_start then
    for _, command in ipairs(command_list) do
      local capability = session.harness.capability or {}
      if command.capability == "agent" and not (capability.agent and capability.agent.catalog) then goto continue end
      if command.capability and command.capability ~= "agent" and capability[command.capability] ~= true then
        goto continue
      end
      items[#items + 1] = {
        label = command.label,
        filterText = command.label,
        detail = command.detail,
        kind = vim.lsp.protocol.CompletionItemKind.Keyword,
        textEdit = {
          newText = command.label,
          range = {
            start = { line = row, character = command_start - 1 },
            ["end"] = { line = row, character = cursor_column },
          },
        },
      }
      ::continue::
    end
  end
  callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
end

return CommandSource
