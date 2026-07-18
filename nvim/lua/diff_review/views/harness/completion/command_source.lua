---@module 'blink.cmp'
---@class DiffReviewHarnessCommandSource
---@field model_backend string?
---@field model_list table[]?
---@field model_callback_list (fun(model_list: table[]))[]
local CommandSource = {}
local agent_catalog = require("diff_review.views.harness.agent_catalog")
local agent_summary = require("diff_review.render.harness.agent_summary")
local client = require("diff_review.harness.client")
local notifications = require("diff_review.infra.notifications")
local session = require("diff_review.session")

local effort_list = { "minimal", "low", "medium", "high", "xhigh" }

local command_list = {
  { label = "/agent", detail = "Switch between Main and child-agent timelines", capability = "agent_observe" },
  { label = "/spawn", detail = "Spawn a child agent", capability = "agent_catalog" },
  { label = "/plan", detail = "Create a reviewed plan" },
  { label = "/plan cancel", detail = "Cancel the active plan" },
  { label = "/sessions", detail = "Search, preview, resume, or delete a Harness session" },
  { label = "/undo", detail = "Roll back to an interaction and restore its prompt" },
  { label = "/backend", detail = "Switch the Harness CLI backend" },
  { label = "/questions", detail = "Reopen pending planning questions" },
  { label = "/goal", detail = "Set a persistent goal" },
  { label = "/goal pause", detail = "Pause automatic goal continuation" },
  { label = "/goal resume", detail = "Resume automatic goal continuation" },
  { label = "/goal clear", detail = "Clear the active goal" },
  { label = "/read", detail = "Allow reads and network, deny local writes" },
  { label = "/write", detail = "Allow workspace writes" },
  { label = "/full", detail = "Allow machine-wide writes with approvals" },
  { label = "/yolo", detail = "Allow machine-wide writes without approvals" },
  { label = "/mode", detail = "Select the Harness execution mode" },
  { label = "/clear", detail = "Start a new session" },
  { label = "/compact", detail = "Compact provider-owned context", capability = "native_compact" },
  { label = "/fork", detail = "Fork the current session with provider-owned context", capability = "native_fork" },
  { label = "/rename", detail = "Rename the current session" },
  { label = "/effort", detail = "Select reasoning effort", capability = "effort_selection" },
  { label = "/model", detail = "Select the backend model", capability = "model_selection" },
  { label = "/fast", detail = "Toggle backend fast mode", capability = "fast_mode" },
  { label = "/fast on", detail = "Enable backend fast mode", capability = "fast_mode" },
  { label = "/fast off", detail = "Disable backend fast mode", capability = "fast_mode" },
}

---@return DiffReviewHarnessCommandSource
function CommandSource.new()
  return setmetatable({
    model_backend = nil,
    model_list = nil,
    model_callback_list = {},
  }, { __index = CommandSource })
end

function CommandSource:get_trigger_characters()
  return { "/", " " }
end

function CommandSource:enabled()
  local line = vim.api.nvim_get_current_line()
  local cursor_column = vim.api.nvim_win_get_cursor(0)[2]
  return line:sub(1, cursor_column):match("^%s*/.*$") ~= nil
end

---@param label string
---@param detail string
---@param row integer
---@param start_character integer
---@param end_character integer
---@return table
local function value_completion(label, detail, row, start_character, end_character)
  return {
    label = label,
    filterText = label,
    detail = detail,
    kind = vim.lsp.protocol.CompletionItemKind.Value,
    textEdit = {
      newText = label,
      range = {
        start = { line = row, character = start_character },
        ["end"] = { line = row, character = end_character },
      },
    },
  }
end

---@param row integer
---@param start_character integer
---@param end_character integer
---@param allowed_effort_list? string[]
---@return table[]
local function effort_completion_list(row, start_character, end_character, allowed_effort_list)
  local result = {}
  for _, effort in ipairs(allowed_effort_list or effort_list) do
    result[#result + 1] = value_completion(
      effort,
      "Use " .. effort .. " reasoning effort",
      row,
      start_character,
      end_character
    )
  end
  return result
end

---@param callback fun(model_list: table[])
function CommandSource:request_model_list(callback)
  local backend = session.harness.session and session.harness.session.backend or "active"
  if session.harness.model_backend == backend and type(session.harness.model_list) == "table" then
    callback(session.harness.model_list)
    return
  end
  if self.model_backend ~= backend then
    self.model_backend = backend
    self.model_list = nil
    self.model_callback_list = {}
  end
  if self.model_list then
    callback(self.model_list)
    return
  end
  self.model_callback_list[#self.model_callback_list + 1] = callback
  if #self.model_callback_list > 1 then return end
  client.request("backend.models", {}, function(model_list, request_error)
    local callback_list = self.model_callback_list
    self.model_callback_list = {}
    if request_error then
      notifications.error("Failed to complete Harness model: " .. request_error, "Harness")
      for _, pending_callback in ipairs(callback_list) do pending_callback({}) end
      return
    end
    self.model_list = type(model_list) == "table" and model_list or {}
    session.harness.model_backend = backend
    session.harness.model_list = vim.deepcopy(self.model_list)
    for _, pending_callback in ipairs(callback_list) do pending_callback(self.model_list) end
  end)
end

function CommandSource:get_completions(_, callback)
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local cursor_column = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local prefix = line:sub(1, cursor_column)
  local command_start = prefix:find("/")
  local items = {}
  local definition_start = prefix:match("^%s*/spawn%s+()([^%s]*)$")
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
  if prefix:match("^%s*/spawn%s+") then
    callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
    return
  end
  local mode_start = prefix:match("^%s*/mode%s+()([^%s]*)$")
  if mode_start then
    local mode_list = {
      { name = "read", detail = "Allow reads and network, deny local writes" },
      { name = "write", detail = "Allow workspace writes" },
      { name = "full", detail = "Allow machine-wide writes with approvals" },
      { name = "yolo", detail = "Allow machine-wide writes without approvals" },
    }
    for _, mode in ipairs(mode_list) do
      items[#items + 1] = {
        label = mode.name,
        filterText = mode.name,
        detail = mode.detail,
        kind = vim.lsp.protocol.CompletionItemKind.Value,
        textEdit = {
          newText = mode.name,
          range = {
            start = { line = row, character = mode_start - 1 },
            ["end"] = { line = row, character = cursor_column },
          },
        },
      }
    end
    callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
    return
  end
  if prefix:match("^%s*/mode%s+") then
    callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
    return
  end
  local model_name, model_effort_start = prefix:match("^%s*/model%s+(%S+)%s+()([^%s]*)$")
  if model_name and model_effort_start then
    self:request_model_list(function(model_list)
      local allowed_effort_list = effort_list
      for _, model in ipairs(model_list or {}) do
        if model.id == model_name and #(model.reasoning or {}) > 0 then
          allowed_effort_list = model.reasoning
          break
        end
      end
      callback({
        items = effort_completion_list(row, model_effort_start - 1, cursor_column, allowed_effort_list),
        is_incomplete_backward = false,
        is_incomplete_forward = false,
      })
    end)
    return
  end
  local model_start = prefix:match("^%s*/model%s+()([^%s]*)$")
  if model_start then
    self:request_model_list(function(model_list)
      local model_completion_list = {}
      for _, model in ipairs(model_list or {}) do
        model_completion_list[#model_completion_list + 1] = value_completion(
          model.id,
          model.description or model.label or "Backend model",
          row,
          model_start - 1,
          cursor_column
        )
      end
      callback({ items = model_completion_list, is_incomplete_backward = false, is_incomplete_forward = false })
    end)
    return
  end
  if prefix:match("^%s*/model%s+") then
    callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
    return
  end
  local effort_start = prefix:match("^%s*/effort%s+()([^%s]*)$")
  if effort_start then
    callback({
      items = effort_completion_list(row, effort_start - 1, cursor_column),
      is_incomplete_backward = false,
      is_incomplete_forward = false,
    })
    return
  end
  if prefix:match("^%s*/effort%s+") then
    callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
    return
  end
  local selector_start = prefix:match("^%s*/agent%s+()([^%s]*)$")
  if selector_start then
    local selector_list = { { selector = "main", detail = "Parent conversation" } }
    for index, run in ipairs(agent_catalog.run_list(session.harness.agent, true)) do
      if index > 26 then break end
      selector_list[#selector_list + 1] = {
        selector = string.char(string.byte("a") + index - 1),
        detail = agent_summary.label(run) .. " · running child " .. index,
      }
    end
    for _, selector in ipairs(selector_list) do
      items[#items + 1] = {
        label = selector.selector,
        filterText = selector.selector,
        detail = selector.detail,
        kind = vim.lsp.protocol.CompletionItemKind.Value,
        textEdit = {
          newText = selector.selector,
          range = {
            start = { line = row, character = selector_start - 1 },
            ["end"] = { line = row, character = cursor_column },
          },
        },
      }
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
      if command.capability == "agent_observe"
        and not (capability.agent and capability.agent.observe)
        and #(((session.harness.agent or {}).run) or {}) == 0
      then
        goto continue
      end
      if command.capability == "agent_catalog" and not (capability.agent and capability.agent.catalog) then
        goto continue
      end
      if command.capability and command.capability ~= "agent_observe" and command.capability ~= "agent_catalog"
        and capability[command.capability] ~= true
      then
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
