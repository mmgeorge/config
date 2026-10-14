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
---@field area string
---@field trigger {kind: string, include?: string[], exclude?: string[]}
---@field scope string
---@field directory string
---@field all {command: string}
---@field files? {command: string}|userdata
---@field cases? {command: string}|userdata
---@field depends_on string[]

---@class ForgeCheckCatalog
---@field configured boolean
---@field revision string
---@field error? string|userdata
---@field snapshot? {config: {definitions: ForgeCheckDefinition[]}}|userdata
---@field overrides {enabled: table<string, boolean>, scopes: table<string, string>}
---@field history table[]
---@field active_run? string|userdata
---@field applicability? {id: string, selected: boolean, reason: string, matched_files: string[]}[]
---@field changed_files? string[]|userdata

---@param value any
---@return boolean
local function present(value) return type(value) == "table" or type(value) == "string" end

---@param scope string
---@return string
local function scope_label(scope)
  return ({ all = "All", changed_files = "Changed files", changed_cases = "Added/modified cases" })[scope] or scope
end

---@param state ForgeHarnessPresentationState
---@param host table
---@param setup fun(check_id: string?)
function Checks.open(state, host, setup)
  if not state.session then return end
  local session_id = state.session.id
  ---@type ForgeCheckCatalog
  local catalog = { configured = false, revision = "", overrides = { enabled = vim.empty_dict(), scopes = vim.empty_dict() }, history = {} }
  local selected_id
  local active_field = "enabled"
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
    return enabled, overrides.scopes[check.id] or check.scope or "all"
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
      for _, command in ipairs(catalog.history[index].commands or {}) do
        if present(command.context) and command.context.definition_id == check.id then
          local completed = command.completed_at_ms
          if type(completed) ~= "number" and command.state == "running" then
            local seconds, microseconds = vim.uv.gettimeofday()
            completed = seconds * 1000 + microseconds / 1000
          end
          local duration = type(command.started_at_ms) == "number" and type(completed) == "number"
              and string.format("%.1fs", (completed - command.started_at_ms) / 1000) or ""
          return command, duration
        end
      end
    end
    return nil, ""
  end

  local function details(check)
    local result = { "Directory: " .. (check.directory or "."), "All: " .. check.all.command }
    result[#result + 1] = "Prerequisites: " .. (#(check.depends_on or {}) > 0 and table.concat(check.depends_on, ", ") or "None")
    result[#result + 1] = "Trigger: " .. vim.json.encode(check.trigger)
    result[#result + 1] = "Area: " .. check.area
    result[#result + 1] = applicability(check)
    if present(check.files) then result[#result + 1] = "Files: " .. check.files.command end
    if present(check.cases) then result[#result + 1] = "Cases: " .. check.cases.command end
    local command = latest(check)
    if command then
      if type(command.error) == "string" then result[#result + 1] = command.error end
      if present(command.context) then
        result[#result + 1] = "Requested: " .. vim.json.encode(command.context.selection)
        if present(command.context.resolved) then result[#result + 1] = "Resolved: " .. vim.json.encode(command.context.resolved) end
      end
      result[#result + 1] = "Output: " .. command.output
    end
    return result
  end

  local function change(direction)
    local check = selected()
    if not check then return end
    local enabled, scope = effective(check)
    local overrides = vim.deepcopy(catalog.overrides)
    if active_field == "enabled" then overrides.enabled[check.id] = not enabled
    else
      local scopes = { "all" }
      if present(check.files) then scopes[#scopes + 1] = "changed_files" end
      if present(check.cases) then scopes[#scopes + 1] = "changed_cases" end
      local index = vim.fn.index(scopes, scope)
      overrides.scopes[check.id] = scopes[(index + direction) % #scopes + 1]
    end
    request("checks.override", overrides, function(result) catalog = result picker.update(build_spec()) end)
  end

  local function run(check, selection, all)
    local params = { ids = check and { check.id } or {}, selections = vim.empty_dict() }
    if all then params.all = true end
    if selection then params.selections[check.id] = selection end
    request("checks.run", params)
  end

  local function select_scope(kind)
    local check = selected()
    if not check then return end
    if not present(check[kind]) then notifications.error("This check does not support " .. kind .. ". Fix its definition with setup.", "Harness checks") return end
    closed = true
    picker.open({ owner = owner .. "-selection", host = host,
      page_list = {{ id = "selection", title = "Run " .. check.name, allow_input = true,
        option_list = {{ id = "selection", label = kind == "files" and "Files: one project-relative path per line" or "Cases: file:name, one per line", input_kind = "other" }} }},
      on_confirm = function(result)
        local selection = { kind = kind, [kind] = {} }
        for line in (result.text or ""):gmatch("[^\r\n]+") do
          if kind == "files" then selection.files[#selection.files + 1] = vim.trim(line)
          else
            local path, name = line:match("^([^:]+):(.+)$")
            if not path then notifications.error("Use project-relative file:name for each case", "Harness checks") return false end
            selection.cases[#selection.cases + 1] = { file = vim.trim(path), name = vim.trim(name) }
          end
        end
        client.request_for(session_id, "checks.run", { ids = { check.id }, selections = { [check.id] = selection } }, function(_, failure)
          if failure then notifications.error(failure, "Harness checks") end
          Checks.open(state, host, setup)
        end)
      end,
    })
  end

  build_spec = function()
    local unavailable = load_error or (not present(catalog.snapshot) and (catalog.error or "Configure checks first"))
    local rows = {
      { id = "setup", label = "Setup / edit with agent", wrap_label = true },
      { id = "run", label = "Run applicable checks", wrap_label = true, disabled_reason = unavailable },
      { id = "run-all", label = "Run all enabled checks (ignore triggers)", wrap_label = true, disabled_reason = unavailable },
      { id = "reset", label = "Reset task overrides", wrap_label = true, disabled_reason = unavailable },
      { id = "save", label = "Save as project defaults", wrap_label = true, disabled_reason = unavailable },
    }
    if type(catalog.active_run) == "string" then rows[#rows + 1] = { id = "cancel", label = "Cancel active checks", wrap_label = true } end
    if load_error or (catalog.configured and type(catalog.error) == "string") then
      rows[#rows + 1] = { id = "error", label = "Fix checks: " .. (load_error or catalog.error), wrap_label = true }
    end
    for index, check in ipairs(definitions()) do
      local enabled, scope = effective(check)
      local command, duration = latest(check)
      local result = command and ({ pending = "Pending", running = "Running", passed = "Passed", failed = "Failed", blocked = "Blocked", interrupted = "Interrupted", skipped = "Not run", configuration_error = "Configuration error" })[command.state] or "Not run"
      local focus = selected_id == check.id and active_field
      rows[#rows + 1] = { id = check.id, label = check.name, value = check,
        columns = { string.format("%d. %s", index, check.name), check.category,
          field.render(enabled and "Enabled" or "Disabled", focus == "enabled"), field.render(scope_label(scope), focus == "scope"),
          result, duration, check.area },
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
          subtitle = selected() and (selected().area .. " · " .. selected().name .. "\n" .. applicability(selected()))
            or (load_error and "Check configuration could not be loaded. Fix the error below, then refresh."
              or catalog.configured and "Automatic checks follow task changes. Select a check to inspect its applicability."
              or "No checks configured. Use Setup / edit with agent to choose coverage."),
          column_headers = { "Check", "Category", "State", "Scope", "Result", "Duration", "Area" }, option_list = rows,
          footer = "↑↓ select ←→ value Tab field [] pages ↵ run i info e edit f/T scope q close",
          action_list = {
            { id = "field", key = "<Tab>", callback = function() active_field = active_field == "enabled" and "scope" or "enabled" picker.update(build_spec()) end },
            { id = "previous", command = "previous_value", callback = function() change(-1) end },
            { id = "next", command = "next_value", callback = function() change(1) end },
            { id = "edit", key = "e", callback = function() local check = selected() setup(check and check.id) end },
            { id = "files", key = "f", callback = function() select_scope("files") end },
            { id = "cases", key = "T", callback = function() select_scope("cases") end },
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
        if option.id == "setup" then setup() return end
        if option.id == "run" then run() return false end
        if option.id == "run-all" then run(nil, nil, true) return false end
        if option.id == "reset" then request("checks.override", { enabled = vim.empty_dict(), scopes = vim.empty_dict() }) return false end
        if option.id == "save" then request("checks.save_defaults", {}) return false end
        if option.id == "cancel" then request("checks.cancel", { run_id = catalog.active_run }) return false end
        if option.value and option.value.category then run(option.value) return false end
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
