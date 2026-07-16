local AgentPicker = {}

local agent_catalog = require("diff_review.views.harness.agent_catalog")
local agent_summary = require("diff_review.render.harness.agent_summary")
local config = require("diff_review.infra.config")
local picker = require("diff_review.views.picker")

local active = nil

---@param instance table
local function stop_timer(instance)
  if not instance.timer then return end
  instance.timer:stop()
  instance.timer:close()
  instance.timer = nil
end

---@param state table
---@param selected_run_id string?
---@return table[]
---@return integer
local function option_list(state, selected_run_id)
  local result = {
    { id = "main", value = { kind = "main" }, label = "Main", detail = "Parent conversation" },
  }
  local now_ms = os.time() * 1000
  for _, group in ipairs({
    { title = "Running", run_list = agent_catalog.run_list(state.agent, true) },
    { title = "Done", run_list = agent_catalog.run_list(state.agent, false) },
  }) do
    for _, run in ipairs(group.run_list) do
      result[#result + 1] = {
        id = "run:" .. run.id,
        value = { kind = "run", run = run },
        label = agent_summary.label(run),
        detail = agent_summary.status_detail(
          run,
          agent_catalog.interaction_list(state.agent, state.agent_live, run.id),
          now_ms
        ),
        section = group.title,
      }
    end
  end
  local initial = 1
  for index, entry in ipairs(result) do
    if entry.value.run and entry.value.run.id == selected_run_id then initial = index end
    entry.key = config.options.picker.session_keys[index]
  end
  return result, initial
end

---@param instance table
---@return table
local function build_spec(instance)
  local state = instance.state_provider()
  local entries, initial = option_list(state, state.selected_agent_run_id)
  return {
    owner = "agent",
    host = instance.host,
    page_list = {
      {
        id = "agents",
        title = "Harness timelines",
        subtitle = "Switch between the parent conversation and existing child-agent timelines.",
        option_list = entries,
        selected_index = initial,
        footer = "↑↓ select  Enter switch  q close",
      },
    },
    on_confirm = function(result)
      stop_timer(instance)
      if active == instance then active = nil end
      local item = result.option.value
      instance.on_select(item.run and item.run.id or nil)
    end,
    on_close = function()
      stop_timer(instance)
      if active == instance then active = nil end
      if instance.on_close then instance.on_close() end
    end,
  }
end

---@param instance table
local function refresh(instance)
  if active ~= instance or not picker.is_open("agent") then return end
  picker.update(build_spec(instance))
end

---@class DiffReviewAgentPickerOptions
---@field host table
---@field state_provider fun(): table
---@field on_select fun(run_id: string?)
---@field on_close? fun()

---@param options DiffReviewAgentPickerOptions
function AgentPicker.open(options)
  if active then
    stop_timer(active)
    active = nil
  end
  local instance = {
    host = options.host,
    state_provider = options.state_provider,
    on_select = options.on_select,
    on_close = options.on_close,
    timer = vim.uv.new_timer(),
  }
  active = instance
  picker.open(build_spec(instance))
  instance.timer:start(1000, 1000, function()
    vim.schedule(function() refresh(instance) end)
  end)
end

function AgentPicker.refresh()
  if active then refresh(active) end
end

function AgentPicker.close()
  if active then stop_timer(active) end
  active = nil
  if picker.is_open("agent") then picker.close(true) end
end

function AgentPicker._state_for_test()
  return active
end

return AgentPicker
