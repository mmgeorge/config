local Checks = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local field = require("forge.views.picker.field")
local notifications = require("forge.infra.notifications")
local owner = "forge-harness-checks"
local category_priority = { typecheck = 0, test = 1, format = 2, lint = 3, build = 4, custom = 5 }

---@class ForgeCheckDefinition
---@field id string
---@field name string
---@field category string
---@field enabled boolean
---@field auto boolean
---@field area string
---@field trigger {auto?: boolean, paths?: string[], exclude?: string[]}
---@field directory string
---@field action {command?: string|string[], adapter?: table, inputs?: table<string, {type: string, description: string, required?: boolean, default?: any, source?: string}>, prepare?: boolean, directory?: string, after?: string[]}
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
  local unsubscribe
  local refresh_pending = false
  local build_spec
  local refresh

  local function request(method, params, callback, on_failure)
    if closed then return end
    if busy then refresh_pending = true return end
    busy = true
    client.request({ session_id = session_id, method = method, params = params }, function(result, failure)
      busy = false
      if closed or not state.session or state.session.id ~= session_id then return end
      if failure then
        notifications.error(failure, "Harness checks")
        if on_failure then on_failure(failure) end
      elseif callback then callback(result) else refresh() end
      if refresh_pending and not closed then
        refresh_pending = false
        refresh()
      end
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
      local has_command, has_adapter = action.command ~= nil, action.adapter ~= nil
      if has_command == has_adapter then
        return nil, "Invalid check " .. id .. ": expected exactly one command or adapter"
      end
      if has_adapter and (type(action.adapter) ~= "table" or vim.tbl_count(action.adapter) ~= 1) then
        return nil, "Invalid check " .. id .. ": expected one adapter configuration"
      end
      if has_command and type(action.command) ~= "string" and type(action.command) ~= "table" then
        return nil, "Invalid check " .. id .. ": expected a command string or arguments"
      end
      if has_command and type(action.command) == "table" then
        if #action.command == 0 then return nil, "Invalid check " .. id .. ": empty action command" end
        for _, argument in ipairs(action.command) do
          if type(argument) ~= "string" then return nil, "Invalid check " .. id .. ": expected string command arguments" end
        end
      end
      if category_priority[action.category] == nil then
        return nil, "Invalid check " .. id .. ": expected typecheck, test, format, lint, build, or custom category"
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
        id = id, name = action.label or id, category = action.category,
        directory = action.directory or ".", area = action.directory or ".",
        auto = trigger.auto == true, enabled = true,
        trigger = trigger, action = action, depends_on = action.after or {},
      }
    end
    table.sort(source, function(left, right)
      local left_priority = category_priority[left.category]
      local right_priority = category_priority[right.category]
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
    catalog, definition_list = result, assert(definitions)
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

  local function details(check)
    ---@type ForgePickerDetailField[]
    local result = {}
    local function add(name, value)
      if value == nil or value == vim.NIL then return end
      if type(value) ~= "table" then
        result[#result + 1] = { label = name, value = tostring(value) }
      elseif vim.islist(value) then
        for index, entry in ipairs(value) do add(index == 1 and name or "", entry) end
      else
        local names = vim.tbl_keys(value)
        table.sort(names)
        if #names == 0 then return end
        result[#result + 1] = { label = name, value = "" }
        local indent = (name:match("^%s*") or "") .. "  "
        for _, property in ipairs(names) do add(indent .. property, value[property]) end
      end
    end
    local function add_command(name, command)
      if type(command) == "table" then
        command = table.concat(vim.tbl_map(function(argument)
          if argument == "" or argument:find('[%s"\\]') then return vim.json.encode(argument) end
          return argument
        end, command), " ")
      end
      add(name, command)
    end
    if check.action.adapter then
      local adapter = next(check.action.adapter)
      add("adapter", adapter)
      local options = check.action.adapter[adapter]
      local names = vim.tbl_keys(options)
      table.sort(names)
      for _, name in ipairs(names) do
        local value = options[name]
        local default = adapter == "rust_nextest"
          and (name == "nextest_profile" and value == "default" or name == "all_features" and value == false)
        if not default then
          if name == "command" then add_command("  " .. name, value) else add("  " .. name, value) end
        end
      end
    else
      add_command("command", check.action.command)
    end
    add("after", check.action.after)
    add("paths", check.trigger.paths)
    add("exclude", check.trigger.exclude)
    add("prepare", check.action.prepare)
    add("inputs", check.action.inputs)
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
    request("session.check.override", overrides, function() refresh() end)
  end

  build_spec = function()
    local rows = {}
    if type(catalog.active_run) == "string" then rows[#rows + 1] = { id = "cancel", label = "Cancel current work (including checks)", wrap_label = true } end
    if load_error or (catalog.configured and type(catalog.error) == "string") then
      rows[#rows + 1] = { id = "error", label = "Fix checks: " .. (load_error or catalog.error), wrap_label = true }
    end
    for _, check in ipairs(definition_list) do
      local enabled, automatic = effective(check)
      local focus = selected_id == check.id and active_field
      rows[#rows + 1] = { id = check.id, label = check.name, value = check,
        columns = { (enabled and "● " or "○ ") .. check.name, check.category,
          field.render(automatic and "Yes" or "No", focus == "auto"), check.area },
        child_field_list = expanded[check.id] and details(check) or nil }
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
      on_dispose = function() closed = true if unsubscribe then unsubscribe() end end,
      on_confirm = function(result)
        local option = result.option
        if option.id == "cancel" then request("session.turn.cancel", {}) return false end
        return false
      end,
      action_list = {
        { id = "details", key = "i", callback = function(context) if context.option then expanded[context.option.id] = not expanded[context.option.id] picker.update(build_spec()) end end },
        { id = "refresh", key = "R", callback = function() refresh() end },
      },
    }
  end

  refresh = function()
    request("session.check.list", {}, function(result)
      adopt(result)
      if picker.is_open(owner) then picker.update(build_spec()) else picker.open(build_spec()) end
    end, function(failure)
      load_error = failure
      if picker.is_open(owner) then picker.update(build_spec()) else picker.open(build_spec()) end
    end)
  end
  unsubscribe = client.session_events_subscribe(function(event, payload, event_session_id)
    if closed or event_session_id ~= session_id then return end
    if event == "checks_changed" or event == "state_invalidated" or event == "session_configured"
      or event == "plan_checks_updated"
      or event == "provider_event" and payload.kind == "checks_configuration_error" then
      refresh()
    end
  end)
  refresh()
end

return Checks
