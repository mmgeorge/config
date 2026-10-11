local SpawnPicker = {}
local generation = 0

local config = require("forge.infra.config")
local picker = require("forge.views.picker")
local model_picker = require("forge.views.harness.model_picker")
local notifications = require("forge.infra.notifications")

---@class ForgeSpawnPickerOptions
---@field host table
---@field task_name? string
---@field provider string
---@field model? string
---@field load_models fun(provider: string, callback: function)
---@field on_spawn fun(request: table)
---@field on_close? fun()

---@param options ForgeSpawnPickerOptions
---@return boolean opened
function SpawnPicker.open(options)
  generation = generation + 1
  local opening_generation = generation
  local request = { task_name = options.task_name, provider = options.provider }
  local open_task
  local open_provider
  local function update(spec)
    spec.owner = "spawn"
    spec.host = options.host
    spec.on_close = options.on_close
    if picker.is_open("spawn") then picker.update(spec) else picker.open(spec) end
  end
  local function open_model()
    options.load_models(request.provider, function(model_list, failure)
      if opening_generation ~= generation or not picker.is_open("spawn") then return end
      if failure then notifications.error(failure, "Harness child model") return end
      if type(model_list) ~= "table" or #model_list == 0 then
        notifications.error("The selected backend did not advertise any models", "Harness child model")
        return
      end
      model_picker.open({
        host = options.host,
        backend = request.provider,
        model_list = model_list,
        current_model = request.provider == options.provider and options.model or nil,
        on_confirm = function(selection)
          request.model = selection.model
          request.reasoning_effort = selection.effort and selection.effort ~= "" and selection.effort or nil
          options.on_spawn(request)
        end,
      })
    end)
  end
  open_provider = function()
    local option_list = {}
    local selected_index = 1
    for _, provider in ipairs({ "codex", "copilot", "claude", "antigravity" }) do
      if provider == request.provider then selected_index = #option_list + 1 end
      local backend = config.options.harness.backends[provider] or {}
      option_list[#option_list + 1] = {
        id = provider,
        label = backend.label or provider,
        detail = backend.detail or "Independent child conversation in the shared workspace",
        value = provider,
      }
    end
    update({
      page_list = { {
        id = "spawn-provider", title = "Provider for " .. request.task_name,
        column_headers = { "Provider", "Details" }, option_list = option_list,
        selected_index = selected_index, footer = "↑↓ provider  Enter model and reasoning  q close",
      } },
      on_confirm = function(result)
        request.provider = result.option.value
        open_model()
        return false
      end,
    })
  end
  open_task = function()
    update({
      initial_input_kind = "other",
      page_list = { {
        id = "spawn-task", title = "Task for " .. request.task_name,
        column_headers = { "Task", "Details" }, allow_input = true, input_height = 5,
        option_list = { { id = "task", label = "Task message", value = "task", input_kind = "other",
          detail = "The child receives this task and workspace instructions in a fresh conversation." } },
        footer = "C-s choose provider  Tab options  q close",
      } },
      on_confirm = function(result)
        if not result.text or result.text:match("^%s*$") then return false end
        request.message = result.text
        open_provider()
        return false
      end,
    })
  end
  if request.task_name then
    open_task()
  else
    update({
      initial_input_kind = "other",
      page_list = { {
        id = "spawn-name", title = "Create Child Agent",
        column_headers = { "Name", "Details" }, allow_input = true, input_height = 2,
        option_list = { { id = "name", label = "Task name", value = "name", input_kind = "other",
          detail = "Unique name with letters, digits, underscores, or hyphens. Canonical path: /root/<name>." } },
        footer = "C-s describe task  Tab options  q close",
      } },
      on_confirm = function(result)
        local name = (result.text or ""):match("^%s*(.-)%s*$")
        if name == "" or #name > 64 or not name:match("^[A-Za-z0-9_-]+$") then
          notifications.warn("Use 1 to 64 letters, digits, underscores, or hyphens", "Harness child name")
          return false
        end
        request.task_name = name
        open_task()
        return false
      end,
    })
  end
  return true
end

return SpawnPicker
