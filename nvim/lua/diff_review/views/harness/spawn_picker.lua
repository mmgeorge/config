local SpawnPicker = {}

local config = require("diff_review.infra.config")
local picker = require("diff_review.views.picker")

---@param definition table
---@param options table
local function open_task(definition, options)
  local task_spec = {
    owner = "spawn",
    host = options.host,
    initial_input_kind = "other",
    page_list = {
      {
        id = "spawn-task-" .. definition.name,
        title = "Task for " .. definition.name,
        subtitle = definition.description,
        allow_input = true,
        input_height = 5,
        option_list = {
          {
            id = definition.name,
            label = "Agent task",
            detail = "Describe the bounded work for this child agent.",
            value = definition.name,
            input_kind = "other",
          },
        },
        footer = "C-s spawn  Tab options  C-c clear  q close",
      },
    },
    on_confirm = function(result)
      options.on_spawn(definition.name, result.text)
    end,
    on_close = options.on_close,
  }
  if picker.is_open("spawn") then picker.update(task_spec) else picker.open(task_spec) end
end

---@param definition_list table[]
---@param definition_name string
---@return table?
local function find_definition(definition_list, definition_name)
  for _, definition in ipairs(definition_list) do
    if definition.name == definition_name then return definition end
  end
  return nil
end

---@class DiffReviewSpawnPickerOptions
---@field host table
---@field definition_list table[]
---@field definition_name? string
---@field on_spawn fun(definition: string, task: string)
---@field on_close? fun()

---@param options DiffReviewSpawnPickerOptions
---@return boolean opened
function SpawnPicker.open(options)
  if options.definition_name then
    local definition = find_definition(options.definition_list, options.definition_name)
    if not definition then return false end
    open_task(definition, options)
    return true
  end

  local option_list = {}
  for _, definition in ipairs(options.definition_list) do
    option_list[#option_list + 1] = {
      id = definition.name,
      label = definition.name,
      detail = definition.description,
      search_text = definition.name,
      value = definition,
    }
  end
  picker.open({
    owner = "spawn",
    host = options.host,
    page_list = {
      {
        id = "spawn-agent",
        title = "Spawn child agent",
        subtitle = "Filter the available agent definitions, then describe its task.",
        option_list = option_list,
        empty_text = "No matching agent definitions.",
        search = { choice_keys = config.options.picker.session_keys },
        footer = "Type to filter  ↑↓ select  Enter continue  Esc normal",
      },
    },
    on_confirm = function(result)
      open_task(result.option.value, options)
      return false
    end,
    on_close = options.on_close,
  })
  return true
end

return SpawnPicker
