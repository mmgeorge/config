local SpawnPicker = {}
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")

---@alias ForgeSpawnAccess "read"|"write"

---@class ForgeSpawnRequest
---@field profile string
---@field access? ForgeSpawnAccess
---@field text string

---@class ForgeSpawnPickerOptions
---@field host table
---@field profile? string
---@field access? ForgeSpawnAccess
---@field document { profiles: table<string, { enabled: boolean, description: string }>, default_profile: string }
---@field on_spawn fun(request: ForgeSpawnRequest)
---@field on_close? fun()

--- Select a profile and access mode before composing a fresh child's task.
---@param options ForgeSpawnPickerOptions
---@return boolean
function SpawnPicker.open(options)
  local profile = options.profile
  local function show(spec)
    spec.owner, spec.host, spec.on_close = "spawn", options.host, options.on_close
    if picker.is_open("spawn") then picker.update(spec) else picker.open(spec) end
  end
  ---@param access? ForgeSpawnAccess
  local function task(access)
    local selected_profile = assert(profile)
    show({
      initial_input_kind = "other",
      page_list = { {
        id = "spawn-task", title = "Task for " .. selected_profile,
        column_headers = { "Task", "Details" }, allow_input = true, input_height = 5,
        option_list = { { id = "task", label = "Task message", input_kind = "other",
          detail = "Fresh conversation using this profile's configured provider and model." } },
        footer = "C-s spawn  q close",
      } },
      on_confirm = function(result)
        if not result.text or result.text:match("^%s*$") then return false end
        options.on_spawn({ profile = selected_profile, access = access, text = result.text })
      end,
    })
  end
  local function select_access()
    if options.access then task(options.access) return end
    show({
      page_list = { {
        id = "spawn-access", title = "Subagent access", column_headers = { "Access", "Details" },
        option_list = {
          { id = "inherit", label = "Profile default", detail = "Use the selected profile's access setting." },
          { id = "read", label = "Read", detail = "Run this subagent with write access off.", value = "read" },
          { id = "write", label = "Write", detail = "Require write access from the session and profile.", value = "write" },
        },
        footer = "Enter compose task  q close",
      } },
      on_confirm = function(result) task(result.option.value) return false end,
    })
  end
  if profile then
    local selected = options.document.profiles[profile]
    if not selected or not selected.enabled then
      notifications.warn("Unknown or disabled subagent profile: " .. profile, "Harness agent")
      return false
    end
    select_access()
    return true
  end
  local entries = {}
  for name, entry in pairs(options.document.profiles) do
    if entry.enabled then entries[#entries + 1] = { id = name, label = name, value = name, detail = entry.description } end
  end
  table.sort(entries, function(left, right) return left.label < right.label end)
  local selected_index = 1
  for index, entry in ipairs(entries) do
    if entry.value == options.document.default_profile then selected_index = index end
  end
  show({
    page_list = { { id = "spawn-profile", title = "Spawn Agent", column_headers = { "Profile", "Purpose" },
      option_list = entries, selected_index = selected_index, footer = "Enter select access  q close" } },
    on_confirm = function(result)
      profile = result.option.value
      select_access()
      return false
    end,
  })
  return true
end

return SpawnPicker
