local Checks = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local field = require("forge.views.picker.field")
local notifications = require("forge.infra.notifications")
local owner = "forge-harness-checks"

---@class ForgeCheckDefinition
---@field id string
---@field name string
---@field category string
---@field enabled boolean
---@field auto boolean
---@field area string
---@field trigger {kind: string, include?: string[], exclude?: string[]}
---@field scope string
---@field directory string
---@field all {command: string, args?: string[]}
---@field files? {command: string, args?: string[]}|userdata
---@field cases? {command: string, args?: string[]}|userdata
---@field depends_on string[]

---@class ForgeCheckCatalog
---@field configured boolean
---@field revision string
---@field error? string|userdata
---@field snapshot? {config: {definitions: ForgeCheckDefinition[]}}|userdata
---@field overrides {enabled: table<string, boolean>, scopes: table<string, string>, auto?: table<string, boolean>}
---@field history table[]
---@field active_run? string|userdata
---@field applicability? {id: string, selected: boolean, reason: string, matched_files: string[]}[]
---@field changed_files? string[]|userdata
---@field preview? {id: string, command: string, reason: string, selected: boolean, pending: boolean}[]
---@field coverage? {id: string, command: string, pending: boolean, evidence?: {run_id: string, state: string}}[]
---@field automatic? {pending: boolean, blocked?: string}

---@param value any
---@return boolean
local function present(value) return type(value) == "table" or type(value) == "string" end

---@param scope string
---@return string
local function scope_label(scope)
  return ({ all = "All", changed_files = "Changed files", changed_cases = "Added/modified cases", new_cases = "New cases" })[scope] or scope
end

---@param operation {command: string, args?: string[]}
---@return string
local function operation_label(operation)
  local parts = { operation.command }
  for _, argument in ipairs(type(operation.args) == "table" and operation.args or {}) do
    parts[#parts + 1] = vim.json.encode(argument)
  end
  return table.concat(parts, " ")
end

---@param state ForgeHarnessPresentationState
---@param host table
function Checks.open(state, host)
  if not state.session then return end
  local session_id = state.session.id
  ---@type ForgeCheckCatalog
  local catalog = { configured = false, revision = "", overrides = { enabled = vim.empty_dict(), scopes = vim.empty_dict() }, history = {} }
  local selected_id
  local active_field = "scope"
  local expanded = {}
  local busy = false
  local closed = false
  local load_error
  local timer
  local build_spec
  local refresh
  local poll

  local function stop_poll()
    if timer then timer:stop() timer:close() timer = nil end
  end

  poll = function()
    if closed or timer then return end
    timer = vim.defer_fn(function()
      timer = nil
      if closed or not picker.is_open(owner) then return end
      if busy then poll() else refresh() end
    end, 1000)
  end

  local function request(method, params, callback, on_failure)
    if busy then return end
    busy = true
    client.request_for(session_id, method, params, function(result, failure)
      busy = false
      if failure then
        notifications.error(failure, "Harness checks")
        if not closed and on_failure then on_failure(failure) end
        return
      end
      if closed or not state.session or state.session.id ~= session_id then return end
      if callback then callback(result) else refresh() end
    end)
  end

  local function definitions()
    local source = catalog and present(catalog.snapshot) and catalog.snapshot.config.definitions or {}
    local result, completed = {}, {}
    while #result < #source do
      local ready
      for _, check in ipairs(source) do
        if not completed[check.id] and vim.iter(check.depends_on or {}):all(function(id) return completed[id] end) then ready = check break end
      end
      if not ready then break end
      completed[ready.id] = true
      result[#result + 1] = ready
    end
    return result
  end

  local function effective(check)
    local overrides = catalog.overrides or { enabled = {}, scopes = {} }
    local enabled = overrides.enabled[check.id]
    if enabled == nil then enabled = check.enabled end
    local automatic = (overrides.auto or {})[check.id]
    if automatic == nil then automatic = check.auto ~= false end
    return enabled, overrides.scopes[check.id] or check.scope or "all", automatic
  end

  local function selected()
    for _, check in ipairs(definitions()) do if check.id == selected_id then return check end end
  end

  local function applicability(check)
    for _, item in ipairs(catalog.applicability or {}) do
      if item.id == check.id then return (item.selected and "Selected: " or "Not selected: ") .. item.reason end
    end
    return "Not evaluated"
  end

  local function latest(check)
    for index = #(catalog.history or {}), 1, -1 do
      local matches = {}
      local priority = { configuration_error = 8, failed = 7, blocked = 6, interrupted = 5, running = 4, pending = 3, skipped = 2, passed = 1 }
      local result
      for _, command in ipairs(catalog.history[index].commands or {}) do
        if present(command.context) and command.context.definition_id == check.id then
          matches[#matches + 1] = command
          if not result or (priority[command.state] or 0) > (priority[result.state] or 0) then result = command end
        end
      end
      if result then
        result = vim.deepcopy(result)
        result.expanded = matches
        return result
      end
    end
    return nil
  end

  local function details(check)
    local result = { "Directory: " .. (check.directory or "."), "All: " .. operation_label(check.all) }
    result[#result + 1] = "Prerequisites: " .. (#(check.depends_on or {}) > 0 and table.concat(check.depends_on, ", ") or "None")
    result[#result + 1] = "Trigger: " .. vim.json.encode(check.trigger)
    result[#result + 1] = "Area: " .. check.area
    result[#result + 1] = applicability(check)
    if present(check.files) then result[#result + 1] = "Files: " .. operation_label(check.files) end
    if present(check.cases) then result[#result + 1] = "Cases: " .. operation_label(check.cases) end
    for _, preview in ipairs(catalog.preview or {}) do
      if preview.id == check.id then
        result[#result + 1] = (preview.pending and "Pending: " or preview.selected and "Will run: " or "Not selected: ") .. preview.command
        result[#result + 1] = preview.reason
      end
    end
    for _, coverage in ipairs(catalog.coverage or {}) do
      if coverage.id == check.id then
        local evidence = coverage.evidence
        result[#result + 1] = type(evidence) == "table"
          and ("Current evidence: " .. evidence.state .. " · run " .. evidence.run_id)
          or "Coverage: pending execution"
      end
    end
    local command = latest(check)
    if command then
      if type(command.error) == "string" then result[#result + 1] = command.error end
      if present(command.context) then
        result[#result + 1] = "Requested: " .. vim.json.encode(command.context.selection)
        if present(command.context.resolved) then result[#result + 1] = "Resolved: " .. vim.json.encode(command.context.resolved) end
      end
      result[#result + 1] = "Output: " .. command.output
      for _, invocation in ipairs(command.expanded or {}) do
        result[#result + 1] = invocation.state .. ": " .. invocation.command
        result[#result + 1] = "Output: " .. invocation.output
      end
    end
    return result
  end

  local function change(field_name, direction)
    local check = selected()
    if not check then return end
    local enabled, scope, automatic = effective(check)
    local overrides = vim.deepcopy(catalog.overrides)
    if field_name == "enabled" then overrides.enabled[check.id] = not enabled
    elseif field_name == "auto" then
      overrides.auto = overrides.auto or {}
      overrides.auto[check.id] = not automatic
    else
      local scopes = { "all" }
      if present(check.files) then scopes[#scopes + 1] = "changed_files" end
      if present(check.cases) then scopes[#scopes + 1] = "changed_cases" scopes[#scopes + 1] = "new_cases" end
      local index = vim.fn.index(scopes, scope)
      overrides.scopes[check.id] = scopes[(index + direction) % #scopes + 1]
    end
    request("checks.override", overrides, function(result) catalog = result picker.update(build_spec()) end)
  end

  build_spec = function()
    local rows = {}
    if type(catalog.active_run) == "string" then rows[#rows + 1] = { id = "cancel", label = "Cancel active checks", wrap_label = true } end
    if load_error or (catalog.configured and type(catalog.error) == "string") then
      rows[#rows + 1] = { id = "error", label = "Fix checks: " .. (load_error or catalog.error), wrap_label = true }
    end
    for _, check in ipairs(definitions()) do
      local enabled, scope, automatic = effective(check)
      local focus = selected_id == check.id and active_field
      rows[#rows + 1] = { id = check.id, label = check.name, value = check,
        columns = { (enabled and "● " or "○ ") .. check.name, check.category,
          field.render(scope_label(scope), focus == "scope"),
          field.render(automatic and "Yes" or "No", focus == "auto"), check.area },
        child_line_list = expanded[check.id] and details(check) or nil }
    end
    local history = {}
    for index = #(catalog.history or {}), 1, -1 do
      local record = catalog.history[index]
      local lines = {}
      for _, event in ipairs(record.lifecycle or {}) do lines[#lines + 1] = event.text end
      for _, command in ipairs(record.commands or {}) do lines[#lines + 1] = command.command .. " · " .. command.state .. " · " .. command.output end
      history[#history + 1] = { id = record.id, label = record.context and present(record.context) and record.context.trigger or record.id,
        detail = record.state, child_line_list = expanded[record.id] and lines or nil, value = record }
    end
    return { owner = owner, host = host,
      page_list = {
        { id = "checks", title = "Project checks · " .. (load_error and "Unavailable" or catalog.configured and (present(catalog.snapshot) and "Configured" or "Configuration error") or "Unconfigured"),
          column_headers = { "Check", "Category", "Scope", "Auto", "Area" }, option_list = rows,
          empty_text = "No checks configured. Use /checks setup.",
          footer = "↑↓ select  Space enable/disable  Tab field  ←→ value  [] pages  ↵ run  i details  q close",
          action_list = {
            { id = "toggle", key = "<Space>", callback = function() change("enabled", 1) end },
            { id = "field", key = "<Tab>", callback = function() active_field = active_field == "scope" and "auto" or "scope" picker.update(build_spec()) end },
            { id = "previous", command = "previous_value", callback = function() change(active_field, -1) end },
            { id = "next", command = "next_value", callback = function() change(active_field, 1) end },
          } },
        { id = "history", title = "Check history", option_list = history, empty_text = "No check runs yet.", footer = "[ ] pages  ↑↓ select  i details  q close" },
      },
      on_change = function(context)
        local next_id = context.option and context.option.value and context.option.value.category and context.option.id or nil
        if selected_id ~= next_id then
          selected_id = next_id
          vim.schedule(function() if picker.is_open(owner) then picker.update(build_spec()) end end)
        end
      end,
      on_cancel = function() closed = true stop_poll() end,
      on_confirm = function(result)
        local option = result.option
        if option.id == "cancel" then request("checks.cancel", { run_id = catalog.active_run }) return false end
        if option.value and option.value.category then request("checks.run", { ids = { option.id } }) return false end
        return false
      end,
      action_list = {
        { id = "details", key = "i", callback = function(context) if context.option then expanded[context.option.id] = not expanded[context.option.id] picker.update(build_spec()) end end },
        { id = "refresh", key = "R", callback = function() refresh() end },
      },
    }
  end

  refresh = function()
    request("checks.list", {}, function(result)
      load_error = nil
      catalog = result
      if picker.is_open(owner) then picker.update(build_spec()) else picker.open(build_spec()) end
      if type(catalog.active_run) == "string" then poll() end
    end, function(failure)
      load_error = failure
      if picker.is_open(owner) then picker.update(build_spec()) else picker.open(build_spec()) end
    end)
  end
  refresh()
end

return Checks
