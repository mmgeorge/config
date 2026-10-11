local SpawnPicker = {}
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")

--- Select an enabled profile before composing a fresh child's task.
---@param options table
---@return boolean
function SpawnPicker.open(options)
  local profile = options.profile
  local function show(spec)
    spec.owner, spec.host, spec.on_close = "spawn", options.host, options.on_close
    if picker.is_open("spawn") then picker.update(spec) else picker.open(spec) end
  end
  local function task()
    show({
      initial_input_kind = "other",
      page_list = { {
        id = "spawn-task", title = "Task for " .. profile,
        column_headers = { "Task", "Details" }, allow_input = true, input_height = 5,
        option_list = { { id = "task", label = "Task message", input_kind = "other",
          detail = "Fresh conversation using this profile's configured provider and model." } },
        footer = "C-s spawn  q close",
      } },
      on_confirm = function(result)
        if not result.text or result.text:match("^%s*$") then return false end
        options.on_spawn({ profile = profile, text = result.text })
      end,
    })
  end
  if profile then
    local selected = options.document.profiles[profile]
    if not selected or not selected.enabled then
      notifications.warn("Unknown or disabled subagent profile: " .. profile, "Harness agent")
      return false
    end
    task()
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
      option_list = entries, selected_index = selected_index, footer = "Enter compose task  q close" } },
    on_confirm = function(result)
      profile = result.option.value
      task()
      return false
    end,
  })
  return true
end

return SpawnPicker
