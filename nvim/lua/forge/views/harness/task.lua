local M = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local operation_sequence = 0

---@param state table
---@return table?
function M.current(state)
  local selected = state.session and state.session.current_task_id
  for _, task in ipairs(state.task or {}) do
    if task.id == selected then return task end
  end
end

---@param state table
---@param action table
---@param refresh fun()
---@param settled fun(failure: string?)
function M.transition(state, action, refresh, settled, admitted)
  if state.host_error then notifications.error(state.host_error, "Harness task") return end
  local session_id = state.session.id
  local generation = client.host_generation()
  operation_sequence = operation_sequence + 1
  local identity = ("%s:%s:%s"):format(vim.fn.getpid(), vim.uv.hrtime(), operation_sequence)
  action = vim.deepcopy(action)
  action.operation_id = identity
  local previous = state.task_operation
  local operation = { id = identity, action = action.action, config = action.config, state = "submitting" }
  state.task_operation = operation
  state.execution_notice = nil
  state.queue_suspended = true
  local function current()
    return state.session and state.session.id == session_id and state.task_operation == operation
      and client.host_generation() == generation
  end
  local function receive(result, failure, detail)
    if not current() then return end
    if failure then
      if detail and detail.code ~= "outcome_unknown" then
        state.task_operation = previous
        state.execution_notice = "Task request rejected: " .. failure
        notifications.error(state.execution_notice, "Harness task")
        if previous then
          state.task_config = previous.config
          if previous.poll then previous.poll() end
          refresh()
          return
        end
        settled(state.execution_notice)
        return
      end
      state.execution_notice = "Task outcome unknown: " .. failure
      operation.state = "outcome_unknown"
      refresh()
      return
    end
    if not result or result == vim.NIL then
      operation.state = "outcome_unknown"
      state.execution_notice = "Task acknowledgement unknown. Check task status before retrying."
      refresh()
      return
    end
    local rank = { submitting = 0, accepted = 1, stopping = 2, running = 3 }
    if rank[result.state] and rank[operation.state] and rank[result.state] < rank[operation.state] then return end
    operation.state = result.state
    if not operation.admitted then
      operation.admitted = true
      if admitted then admitted() end
    end
    if result.state == "completed" or result.state == "failed" or result.state == "superseded" or result.state == "outcome_unknown" then
      state.task_operation = nil
      local message = result.error ~= vim.NIL and result.error or nil
      if result.state == "failed" or result.state == "outcome_unknown" then
        message = message or "Task transition failed"
        state.execution_notice = message
        notifications.error(message, "Harness task")
      end
      settled(message)
    else
      refresh()
    end
  end
  operation.receive = receive
  client.request_for(session_id, "task.transition", action, receive)
  local function poll(owner)
    if operation.poll_owner ~= owner then return end
    if not current() or state.host_error then return end
    if not operation.poll_pending then
      operation.poll_pending = true
      client.request_for(session_id, "task.operation", { operation_id = identity }, function(result, failure)
        operation.poll_pending = false
        receive(result, failure)
      end)
    end
    vim.defer_fn(function() poll(owner) end, 2000)
  end
  operation.poll = function()
    operation.poll_owner = {}
    poll(operation.poll_owner)
  end
  vim.defer_fn(function()
    if current() and operation.state == "submitting" then
      state.execution_notice = "Task acknowledgement unknown. Checking the original operation."
      refresh()
    end
    operation.poll()
  end, 10000)
end

---@param state table
---@param payload table
function M.receive(state, payload)
  local operation = state.task_operation
  if operation and payload.id == operation.id then operation.receive(payload) end
end

---@param host table
---@param title string
---@param options table[]
---@param select fun(value: table)
local function choose(host, title, options, select)
  picker.open({ owner = "harness-task", host = host,
    page_list = { { id = "task", title = title, option_list = options, empty_message = "No matching tasks or plans in this conversation." } },
    on_confirm = function(result) select(result.option.value) end,
  })
end

local function inspect(host, task, transition)
  local options = {
    { label = task.kind .. " · " .. task.phase .. " · " .. task.status,
      detail = task.reason or "Task history retained", value = {} },
    { label = "Permission: " .. task.permission,
      detail = "Attempt " .. tostring(task.generation), value = {} },
  }
  if task.plan_id then
    options[#options + 1] = { label = "Fork plan", detail = "Start a new planning task from the latest saved design",
      value = { action = "fork", plan_id = task.plan_id } }
  end
  choose(host, task.title, options, function(action)
    if action.action then transition(action) end
  end)
end

---@param state table
---@param host table
---@param transition fun(action: table)
function M.open(state, host, transition)
  local session_id = state.session.id
  client.request_for(session_id, "task.list", {}, function(result, failure)
    if failure then notifications.error(failure, "Harness tasks") return end
    if not state.session or state.session.id ~= session_id then return end
    local options = {}
    for _, task in ipairs(result or {}) do
      local current = task.id == state.session.current_task_id
      local option = { label = task.title, detail = ("%s · %s · %s%s"):format(task.kind, task.phase, task.status, current and " · current" or ""), value = task }
      if current then table.insert(options, 1, option) else options[#options + 1] = option end
    end
    choose(host, "Tasks", options, function(task)
      if task.status == "completed" or task.status == "cancelled" then
        inspect(host, task, transition)
      else transition({ action = "resume", task_id = task.id, generation = task.generation }) end
    end)
  end)
end

---@param state table
---@param host table
---@param execute boolean
---@param transition fun(action: table)
function M.plans(state, host, execute, transition)
  local session_id = state.session.id
  client.request_for(session_id, "plan.list", {}, function(result, failure)
    if failure then notifications.error(failure, "Harness plans") return end
    if not state.session or state.session.id ~= session_id then return end
    local options = {}
    for _, plan in ipairs(result or {}) do
      if not execute or plan.revision_count > 0 then
        local task = type(plan.task) == "table" and plan.task or nil
        options[#options + 1] = { label = plan.title, detail = task and (task.kind .. " · " .. task.phase .. " · " .. task.status) or plan.state, value = plan }
      end
    end
    choose(host, execute and "Execute Plan" or "Plans", options, function(plan)
      if execute then transition({ action = "execute", plan_id = plan.id, digest = plan.digest }) return end
      local task = type(plan.task) == "table" and plan.task or nil
      local actions = { { label = "Fork plan", detail = "Reassess the latest saved design against this repository", value = { action = "fork", plan_id = plan.id } } }
      if task then
        local terminal = task.status == "completed" or task.status == "cancelled"
        table.insert(actions, 1, { label = terminal and "View task" or "Resume task", detail = task.status, value = terminal and { action = "view", task = task } or { action = "resume", task_id = task.id, generation = task.generation } })
      else
        table.insert(actions, 1, { label = "Start planning task", value = { action = "attach_plan", plan_id = plan.id } })
      end
      choose(host, plan.title, actions, function(action)
        if action.action == "view" then inspect(host, action.task, transition) else transition(action) end
      end)
    end)
  end)
end

---@param host table
---@param select fun(kind: string)
function M.new(host, select)
  choose(host, "New Task", {
    { label = "Plan", value = "plan" }, { label = "Execute", value = "execute" }, { label = "Goal", value = "goal" },
  }, select)
end

return M
