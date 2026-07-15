local AgentPicker = {}

local config = require("diff_review.infra.config")
local picker = require("diff_review.views.picker")

local function entry_list(agent, selected_run_id)
  local result = {
    { value = { kind = "main" }, label = "Main", detail = "Parent conversation" },
  }
  for _, run in ipairs(agent.run or {}) do
    if run.status == "starting" or run.status == "running" or run.status == "waiting" then
      result[#result + 1] = {
        value = { kind = "run", run = run },
        label = run.nickname or run.definition,
        detail = run.status,
        section = "Active",
      }
    end
  end
  for _, run in ipairs(agent.run or {}) do
    if run.status ~= "starting" and run.status ~= "running" and run.status ~= "waiting" then
      result[#result + 1] = {
        value = { kind = "run", run = run },
        label = run.nickname or run.definition,
        detail = run.status,
        section = "Done",
      }
    end
  end
  for _, definition in ipairs(agent.definition or {}) do
    result[#result + 1] = {
      value = { kind = "definition", definition = definition },
      label = definition.name,
      detail = definition.description,
      section = "Available",
    }
  end
  local initial = 1
  for index, entry in ipairs(result) do
    if entry.value.run and entry.value.run.id == selected_run_id then initial = index end
    entry.key = config.options.picker.choice_keys[index]
  end
  return result, initial
end

---@param options table
function AgentPicker.open(options)
  local entries, initial = entry_list(options.agent or {}, options.selected_run_id)
  local host = options.host
  local function open_launcher()
    picker.open({
      owner = "agent",
      host = host,
      page_list = {
        {
          id = "agents",
          title = "Harness timelines",
          subtitle = "Select a timeline or start another agent.",
          option_list = entries,
          selected_index = initial,
          footer = "↑↓ select  Enter confirm  q close",
        },
      },
      on_confirm = function(result)
        local item = result.option.value
        if item.kind == "definition" then
          local definition = item.definition
          picker.update({
            owner = "agent",
            host = host,
            initial_input_kind = "other",
            page_list = {
              {
                id = "agent-task-" .. definition.name,
                title = "Task for " .. definition.name,
                subtitle = definition.description,
                allow_input = true,
                input_height = 5,
                option_list = {
                  {
                    label = "Agent task",
                    detail = "Describe the bounded work for this child agent.",
                    value = definition.name,
                    input_kind = "other",
                  },
                },
                footer = "C-s spawn  go options  q close",
              },
            },
            on_confirm = function(task_result)
              options.on_spawn(definition.name, task_result.text)
            end,
            on_close = options.on_close,
          })
          return false
        end
        options.on_select(item.run and item.run.id or nil)
      end,
      on_close = options.on_close,
    })
  end
  open_launcher()
end

return AgentPicker
