local Profiles = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local picker_field = require("forge.views.picker.field")
local form = require("forge.shared.form")
local notifications = require("forge.infra.notifications")

local field_list = {
  { "name", "Name" }, { "description", "Purpose" },
  { "provider", "Provider" }, { "model", "Model" }, { "reasoning_effort", "Thinking" },
  { "service_tier", "Speed" }, { "permission_mode", "Permissions" },
  { "enabled", "Enabled" }, { "default", "Default profile" }, { "instructions", "Instructions" },
}

---@param name string
---@param document table
---@return table<string, string>
local function values_for(name, document)
  local profile = document.profiles[name] or { description = "", instructions = "" }
  local values = { name = name, enabled = profile.enabled == false and "Off" or "On",
    default = document.default_profile == name and "On" or "Off" }
  for _, field in ipairs(field_list) do
    if values[field[1]] == nil then values[field[1]] = profile[field[1]] or "inherit" end
  end
  return values
end

---Open global profile selection or a named full-page profile form.
---@param state ForgeHarnessPresentationState
---@param host table
---@param selected_name? string
function Profiles.open(state, host, selected_name)
  local session_id = state.session.id
  local snapshot, show_list, show_profile
  local active, saving = true, false
  local function current() return active and state.session and state.session.id == session_id end
  local function model_values(field, values, done)
    local provider = values.provider == "inherit" and state.session.backend or values.provider
    client.request_for(session_id, "agent.models", { provider = provider }, function(models, failure)
      if failure then done(nil, failure) return end
      local choices = { "inherit", "default" }
      if field == "model" then
        for _, model in ipairs(models) do
          if not vim.tbl_contains(choices, model.id) then choices[#choices + 1] = model.id end
        end
      else
        local selected = values.model == "inherit" and (state.session.resolved_model or state.session.model) or values.model
        for _, model in ipairs(models) do
          if model.id == selected or (selected == "default" and model.is_default) then
            for _, effort in ipairs(model.reasoning or {}) do
              if not vim.tbl_contains(choices, effort) then choices[#choices + 1] = effort end
            end
            break
          end
        end
      end
      done(choices)
    end)
  end
  show_profile = function(name)
    if saving then return end
    picker.close(false)
    local original_name = name
    local values = values_for(name, snapshot.document)
    local fields = {}
    local choices = {
      provider = { "inherit", "codex", "copilot", "claude", "antigravity" },
      service_tier = { "inherit", "default", "fast", "ultrafast" },
      permission_mode = { "inherit", "read", "write", "yolo" },
      enabled = { "On", "Off" }, default = { "Off", "On" },
      model = function(selected, done) model_values("model", selected, done) end,
      reasoning_effort = function(selected, done) model_values("reasoning_effort", selected, done) end,
    }
    for _, field in ipairs(field_list) do
      fields[#fields + 1] = { id = field[1], label = field[2], value = values[field[1]],
        multiline = field[1] == "instructions", choices = choices[field[1]] }
    end
    form.open({ title = "Subagent profile", subtitle = snapshot.path, fields = fields,
      save = function(edited, done)
        if not current() then done("Harness session changed. Reopen the profile editor") return end
        if edited.name ~= original_name and snapshot.document.profiles[edited.name] then
          done("Profile already exists: " .. edited.name) return
        end
        local document = vim.deepcopy(snapshot.document)
        local profile = {}
        for _, field in ipairs(field_list) do
          if field[1] ~= "name" and field[1] ~= "default" then profile[field[1]] = edited[field[1]] end
        end
        profile.enabled = edited.enabled == "On"
        if original_name ~= "" then document.profiles[original_name] = nil end
        document.profiles[edited.name] = profile
        if edited.default == "On" then document.default_profile = edited.name
        elseif document.default_profile == original_name then
          done("Select another default profile before clearing this default") return
        end
        client.request_for(session_id, "agent.profiles.save", {
          expected_revision = snapshot.revision, document = document,
        }, function(result, failure)
          if not current() then done("Harness session changed while saving the profile") return end
          if failure then done(failure) return end
          snapshot, original_name = result, edited.name
          done()
        end)
      end,
      reload = function(done)
        if not current() then done(nil, "Harness session changed. Reopen the profile editor") return end
        client.request_for(session_id, "agent.profiles.get", {}, function(result, failure)
          if not current() then done(nil, "Harness session changed while reloading the profile") return end
          if failure or result.error then done(nil, failure or result.error) return end
          snapshot = result
          if original_name ~= "" and not result.document.profiles[original_name] then
            done(nil, "Profile no longer exists: " .. original_name) return
          end
          done(values_for(original_name, result.document))
        end)
      end,
      close = function() if current() then show_list() end end,
    })
  end
  local function save_list(document)
    saving = true
    client.request_for(session_id, "agent.profiles.save", {
      expected_revision = snapshot.revision, document = document,
    }, function(result, failure)
      saving = false
      if not current() then return end
      if failure then notifications.error(failure, "Subagent profiles") return end
      snapshot = result
      show_list()
    end)
  end
  local function change_value(context, direction)
    if saving then return end
    if context.option and context.option.id == "activity-limit" then
      local document = vim.deepcopy(snapshot.document)
      document.activity_limit = math.max(1, math.min(20, (document.activity_limit or 5) + direction))
      if document.activity_limit ~= (snapshot.document.activity_limit or 5) then save_list(document) end
      return
    end
    local name = context.option and context.option.value
    if not name then return end
    local document = vim.deepcopy(snapshot.document)
    document.profiles[name].enabled = not document.profiles[name].enabled
    save_list(document)
  end
  show_list = function()
    local entries = {}
    for name, profile in pairs(snapshot.document.profiles) do
      entries[#entries + 1] = { id = name, label = name, value = name,
        columns = { name, picker_field.render(profile.enabled and "On" or "Off", true), profile.provider, profile.model,
          snapshot.document.default_profile == name and "Default" or profile.description } }
    end
    table.sort(entries, function(left, right) return left.label < right.label end)
    entries[#entries + 1] = { id = "activity-limit", label = "Activity entries",
      columns = { "Activity entries", picker_field.render(tostring(snapshot.document.activity_limit or 5), true),
        "", "", "Recent entries returned to Main (1–20)" } }
    entries[#entries + 1] = { id = "config:" .. snapshot.path, label = "Open config file",
      columns = { "Open config file", "", "", "", snapshot.path } }
    local spec = { owner = "subagent-profiles", host = host,
      page_list = { { id = "subagent-profiles", title = "Subagent profiles", subtitle = snapshot.error or snapshot.path,
        column_headers = { "Profile", "Enabled", "Provider", "Model", "Purpose" }, option_list = entries,
        footer = "←→ change value  Enter edit  C-a add  C-x delete  C-r reload  q close" } },
      on_confirm = function(result)
        if saving then return false end
        if result.option.id == "activity-limit" then return false
        elseif result.option.value then show_profile(result.option.value)
        else
          picker.close()
          local opened, failure = pcall(vim.cmd, "tabedit " .. vim.fn.fnameescape(snapshot.path))
          if not opened then notifications.error(failure, "Subagent profiles") end
        end
        return false
      end,
      on_close = function() active = false end,
      action_list = {
        { id = "previous-value", command = "previous_value", callback = function(context) change_value(context, -1) end },
        { id = "next-value", command = "next_value", callback = function(context) change_value(context, 1) end },
        { id = "add", key = "<C-a>", callback = function() show_profile("") end },
        { id = "delete", key = "<C-x>", callback = function(context)
          local name = context.option and context.option.value
          if not name or saving then return end
          local document = vim.deepcopy(snapshot.document)
          document.profiles[name] = nil
          save_list(document)
        end },
        { id = "reload", key = "<C-r>", callback = function()
          if saving then return end
          saving = true
          client.request_for(session_id, "agent.profiles.get", {}, function(result, failure)
            saving = false
            if not current() then return end
            if failure then notifications.error(failure, "Subagent profiles") return end
            snapshot = result
            show_list()
          end)
        end },
      },
    }
    if picker.is_open("subagent-profiles") then picker.update(spec) else picker.open(spec) end
  end
  client.request_for(session_id, "agent.profiles.get", {}, function(result, failure)
    if not current() then return end
    if failure then notifications.error(failure, "Subagent profiles") return end
    snapshot = result
    if selected_name and not snapshot.document.profiles[selected_name] then
      notifications.error("Unknown subagent profile: " .. selected_name, "Subagent profiles")
    elseif selected_name then show_profile(selected_name)
    else show_list() end
  end)
end

return Profiles
