local AgentPicker = {}

local agent_catalog = require("forge.views.harness.agent_catalog")
local agent_summary = require("forge.render.harness.agent_summary")
local config = require("forge.infra.config")
local picker = require("forge.views.picker")

local active = nil
local build_spec
local stop_timer

---@param instance table
---@param action string
---@param context table
local function edit_message(instance, action, context)
  local run = context.option and context.option.value.run
  if not run then return end
  instance.editing = true
  picker.update({
    owner = "agent", host = instance.host, initial_input_kind = "other",
    page_list = { {
      id = "agent-message", title = (action == "message" and "Message " or "Follow up with ") .. agent_summary.label(run),
      allow_input = true, input_height = 5, column_headers = { "Action", "Details" },
      option_list = { { id = action, label = action == "message" and "Informational message" or "Follow-up task",
        input_kind = "other", detail = action == "message" and "Retained in the inbox. An idle child remains idle."
          or "Queued for this child's next turn. Retains only its own conversation history." } },
      footer = "C-s send  q close",
    } },
    on_confirm = function(result)
      if not result.text or result.text:match("^%s*$") then return false end
      instance.on_action(action, run.id, result.text)
      instance.editing = false
      picker.update(build_spec(instance))
      return false
    end,
    on_close = function()
      stop_timer(instance)
      if active == instance then active = nil end
    end,
  })
end

---@param instance table
stop_timer = function(instance)
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
    { title = "Running", run_list = agent_catalog.run_list(state, true) },
    { title = "Done", run_list = agent_catalog.run_list(state, false) },
  }) do
    for _, run in ipairs(group.run_list) do
      result[#result + 1] = {
        id = "run:" .. run.id,
        value = { kind = "run", run = run },
        label = agent_summary.label(run),
        detail = agent_summary.status_detail(
          run,
          (state.agent.summary or {})[run.id],
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
build_spec = function(instance)
  local state = instance.state_provider()
  local entries, initial = option_list(state, state.selected_agent_run_id)
  return {
    owner = "agent",
    host = instance.host,
    page_list = {
      {
        id = "agents",
        title = "Agents · model tools " .. (state.subagent_tools == true and "On" or "Off"),
        column_headers = { "Timeline", "Status" },
        option_list = entries,
        selected_index = initial,
        footer = "Enter inspect  C-g message  C-f follow up  C-x interrupt  C-a interrupt all  C-t tools  q close",
      },
    },
    action_list = instance.on_action and {
      { id = "message", key = "<C-g>", desc = "Send informational message", callback = function(context) edit_message(instance,"message",context) end },
      { id = "followup", key = "<C-f>", desc = "Queue follow-up task", callback = function(context) edit_message(instance,"followup",context) end },
      { id = "interrupt", key = "<C-x>", desc = "Interrupt selected child", callback = function(context)
        local run = context.option and context.option.value.run
        if run then instance.on_action("interrupt",run.id) end
      end },
      { id = "interrupt-all", key = "<C-a>", desc = "Interrupt all children", callback = function() instance.on_action("interrupt_all") end },
      { id = "tools", key = "<C-t>", desc = "Toggle root model delegation tools", callback = function() instance.on_action("tools") end },
    } or {},
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
  if active ~= instance or instance.editing or not picker.is_open("agent") then return end
  picker.update(build_spec(instance))
end

---@class ForgeAgentPickerOptions
---@field host table
---@field state_provider fun(): table
---@field on_select fun(run_id: string?)
---@field on_action? fun(action: string, run_id: string?, message: string?)
---@field on_close? fun()

---@param options ForgeAgentPickerOptions
function AgentPicker.open(options)
  if active then
    stop_timer(active)
    active = nil
  end
  local instance = {
    host = options.host,
    state_provider = options.state_provider,
    on_select = options.on_select,
    on_action = options.on_action,
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
