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
---@field trigger {auto?: boolean, paths?: string[], exclude?: string[]}
---@field directory string
---@field action {command: string|string[], inputs?: table<string, {type: string, description: string, required?: boolean, default?: any, source?: string}>, input?: boolean, directory?: string, after?: string[]}
---@field depends_on string[]

---@class ForgeCheckCatalog
---@field configured boolean
---@field revision string
---@field error? string|userdata
---@field snapshot? {config: table<string, {trigger: table, action: table}>}|userdata
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

---@param state ForgeHarnessPresentationState
---@param host table
function Checks.open(state, host)
  if not state.session then return end
  local session_id = state.session.id
  ---@type ForgeCheckCatalog
  local catalog = { configured = false, revision = "", overrides = { enabled = vim.empty_dict(), scopes = vim.empty_dict() }, history = {} }
  local selected_id
  ---@type ForgeCheckDefinition[]
  local definition_list = {}
  local active_field = "auto"
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

  ---@param result ForgeCheckCatalog
  ---@return ForgeCheckDefinition[]? definitions
  ---@return string? failure
  local function decode_definitions(result)
    if type(result) ~= "table" then return nil, "Invalid checks response: expected a catalog" end
    if not result.configured or type(result.error) == "string" then return {} end
    local configured = type(result.snapshot) == "table" and result.snapshot.config or nil
    if type(configured) ~= "table" then return nil, "Invalid checks response: missing configuration" end
    if type(configured.version) == "number" then
      return nil, "Checks schema mismatch: expected named trigger/action entries. Rebuild and restart Forge, then update .forge.json."
    end
    local source = {}
    for id, specification in pairs(configured) do
      if type(id) ~= "string" or type(specification) ~= "table" or type(specification.action) ~= "table" then
        return nil, "Invalid check " .. tostring(id) .. ": expected an action object"
      end
      local action = specification.action
      local trigger = specification.trigger or {}
      if type(trigger) ~= "table" then return nil, "Invalid check " .. id .. ": expected a trigger object" end
      if type(action.command) ~= "string" and type(action.command) ~= "table" then
        return nil, "Invalid check " .. id .. ": expected an action command"
      end
      if type(action.command) == "table" then
        if #action.command == 0 then return nil, "Invalid check " .. id .. ": empty action command" end
        for _, argument in ipairs(action.command) do
          if type(argument) ~= "string" then return nil, "Invalid check " .. id .. ": expected string command arguments" end
        end
      end
      for _, property in ipairs({ "label", "category", "directory" }) do
        if action[property] ~= nil and type(action[property]) ~= "string" then
          return nil, "Invalid check " .. id .. ": expected string " .. property
        end
      end
      if action.inputs ~= nil and type(action.inputs) ~= "table" then
        return nil, "Invalid check " .. id .. ": expected declared inputs"
      end
      if action.after ~= nil and type(action.after) ~= "table" then
        return nil, "Invalid check " .. id .. ": expected prerequisite names"
      end
      for _, dependency in ipairs(action.after or {}) do
        if type(dependency) ~= "string" then return nil, "Invalid check " .. id .. ": expected prerequisite names" end
      end
      source[#source + 1] = {
        id = id, name = action.label or id, category = action.category or "check",
        directory = action.directory or ".", area = action.directory or ".",
        auto = trigger.auto == true, enabled = true,
        trigger = trigger, action = action, depends_on = action.after or {},
      }
    end
    local category_priority = { format = 0, lint = 1, check = 1, test = 2, build = 3 }
    table.sort(source, function(left, right)
      local left_priority = category_priority[left.category] or 1
      local right_priority = category_priority[right.category] or 1
      if left_priority ~= right_priority then return left_priority < right_priority end
      return left.id < right.id
    end)
    local result, completed = {}, {}
    while #result < #source do
      local ready
      for _, check in ipairs(source) do
        if not completed[check.id] and vim.iter(check.depends_on or {}):all(function(id) return completed[id] end) then ready = check break end
      end
      if not ready then return nil, "Invalid checks response: missing or cyclic prerequisites" end
      completed[ready.id] = true
      result[#result + 1] = ready
    end
    return result
  end

  ---@param result ForgeCheckCatalog
  local function adopt(result)
    local definitions, failure = decode_definitions(result)
    if failure then
      if load_error ~= failure then notifications.error(failure, "Harness checks") end
      load_error = failure
      return
    end
    load_error = nil
    catalog, definition_list = result, definitions
  end

  local function effective(check)
    local overrides = catalog.overrides or { enabled = {}, scopes = {} }
    local enabled = overrides.enabled[check.id]
    if enabled == nil then enabled = check.enabled end
    local automatic = (overrides.auto or {})[check.id]
    if automatic == nil then automatic = check.auto == true end
    return enabled, automatic
  end

  local function selected()
    for _, check in ipairs(definition_list) do if check.id == selected_id then return check end end
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
    local command = check.action.command
    local result = { "Directory: " .. check.directory,
      "Action: " .. (type(command) == "table" and table.concat(vim.tbl_map(vim.json.encode, command), " ") or command) }
    result[#result + 1] = "Prerequisites: " .. (#(check.depends_on or {}) > 0 and table.concat(check.depends_on, ", ") or "None")
    result[#result + 1] = "Trigger: " .. vim.json.encode(check.trigger)
    result[#result + 1] = "Area: " .. check.area
    result[#result + 1] = applicability(check)
    if check.action.inputs then result[#result + 1] = "Inputs: " .. vim.json.encode(check.action.inputs) end
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
    if load_error then return end
    local check = selected()
    if not check then return end
    local enabled, automatic = effective(check)
    local overrides = vim.deepcopy(catalog.overrides)
    if field_name == "enabled" then overrides.enabled[check.id] = not enabled
    elseif field_name == "auto" then
      overrides.auto = overrides.auto or {}
      overrides.auto[check.id] = not automatic
    end
    request("checks.override", overrides, function(result) adopt(result) picker.update(build_spec()) end)
  end

  build_spec = function()
    local rows = {}
    if type(catalog.active_run) == "string" then rows[#rows + 1] = { id = "cancel", label = "Cancel active checks", wrap_label = true } end
    if load_error or (catalog.configured and type(catalog.error) == "string") then
      rows[#rows + 1] = { id = "error", label = "Fix checks: " .. (load_error or catalog.error), wrap_label = true }
    end
    for _, check in ipairs(definition_list) do
      local enabled, automatic = effective(check)
      local focus = selected_id == check.id and active_field
      rows[#rows + 1] = { id = check.id, label = check.name, value = check,
        columns = { (enabled and "● " or "○ ") .. check.name, check.category,
          field.render(automatic and "Yes" or "No", focus == "auto"), check.area },
        child_line_list = expanded[check.id] and details(check) or nil }
    end
    local history = {}
    for index = #(catalog.history or {}), 1, -1 do
      local record = catalog.history[index]
      local lines = {}
      for _, event in ipairs(record.lifecycle or {}) do
        if event.kind ~= "started" then lines[#lines + 1] = event.text end
      end
      for _, command in ipairs(record.commands or {}) do lines[#lines + 1] = command.command .. " · " .. command.state .. " · " .. command.output end
      history[#history + 1] = { id = record.id, label = "Checks",
        detail = record.state, child_line_list = expanded[record.id] and lines or nil, value = record }
    end
    return { owner = owner, host = host,
      page_list = {
        { id = "checks", title = "Project checks · " .. (load_error and "Unavailable" or catalog.configured and (present(catalog.snapshot) and "Configured" or "Configuration error") or "Unconfigured"),
          column_headers = { "Check", "Category", "Auto", "Directory" }, option_list = rows,
          empty_text = "No checks configured. Enable /checks edit on, then request configuration in chat.",
          footer = "↑↓ select  Space enable/disable  Tab field  ←→ value  [] pages  i details  q close",
          action_list = {
            { id = "toggle", key = "<Space>", callback = function() change("enabled", 1) end },
            { id = "field", key = "<Tab>", callback = function() active_field = "auto" picker.update(build_spec()) end },
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
      adopt(result)
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
