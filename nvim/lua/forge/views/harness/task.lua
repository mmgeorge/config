local M = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local diagnostic = require("forge.views.harness.diagnostic")
local operation_sequence = 0

---@class ForgeHarnessActiveTask
---@field kind 'goal'|'plan'|'execute'
---@field revision string
---@field goal? {objective:string, state:string}
---@field plan? {paused:boolean, artifact:{title:string, state:string}}
---@field execution? {title:string, record:{phase:string, state:string}}

---@param state table
---@return ForgeHarnessActiveTask?
function M.current(state)
  local task = state.session and state.session.active_task
  return task ~= vim.NIL and task or nil
end

---@param task ForgeHarnessActiveTask
---@return string
function M.status(task)
  if task.kind == "plan" then
    if task.plan.paused then return "paused" end
    local status = task.plan.artifact.state
    if status == "awaiting_review" or status == "awaiting_input" or status == "configuring_checks" then return "waiting" end
    if status == "failed" then return "failed" end
    if status == "cancelled" or status == "rejected" then return "cancelled" end
    return status == "accepted" and "completed" or "running"
  end
  local status = task.kind == "goal" and task.goal.state or task.execution.record.state
  return ({ active = "running", paused = "paused", complete = "completed", cleared = "cancelled",
    cancelled = "cancelled" })[status] or "blocked"
end

---@param task ForgeHarnessActiveTask
---@return string
function M.phase(task)
  if task.kind == "goal" then return "continue" end
  if task.kind == "execute" then return task.execution.record.phase end
  return task.plan.artifact.state
end

---@param task ForgeHarnessActiveTask
---@return string
function M.title(task)
  if task.kind == "goal" then return task.goal.objective end
  if task.kind == "execute" then return task.execution.title end
  return task.plan.artifact.title
end

---@param state table
---@param action table
---@param refresh fun()
---@param settled fun(failure: string?, background: boolean?)
---@param admitted? fun()
---@param method? string
function M.transition(state, action, refresh, settled, admitted, method)
  if state.host_error then notifications.error(state.host_error, "Harness task") return end
  local session_id = state.session.id
  local generation = client.host_get_generation()
  operation_sequence = operation_sequence + 1
  local identity = ("%s:%s:%s"):format(vim.fn.getpid(), vim.uv.hrtime(), operation_sequence)
  action = vim.deepcopy(action)
  action.operation_id = identity
  if action.action == "pause" and state.input then state.input.pause_operation_id = identity end
  local scope = session_id .. ":" .. tostring(generation)
  if state.task_operation_scope ~= scope then
    state.task_operation_by_id = {}
    state.task_operation_scope = scope
  end
  local registry = state.task_operation_by_id
  local operation = { id = identity, action = action.action, config = action.config, state = "submitting", sequence = operation_sequence }
  registry[identity] = operation
  if action.action == "configure" and state.configuration_completion then
    operation.configuration_deferred = state.configuration_completion.deferred
  end
  state.task_operation = operation
  state.execution_notice = nil
  state.execution_notice_operation = nil
  local function current()
    return state.session and state.session.id == session_id and registry[identity] == operation
      and client.host_get_generation() == generation
  end
  local function remove()
    registry[identity] = nil
    if state.task_operation ~= operation then return end
    local latest
    for _, pending in pairs(registry) do
      if not latest or pending.sequence > latest.sequence then latest = pending end
    end
    state.task_operation = latest
  end
  local function receive(result, failure, detail)
    if not current() then return end
    if result and type(result.revision) == "number" then
      if operation.server_revision and result.revision < operation.server_revision then return end
      operation.server_revision = result.revision
    end
    if failure and detail and detail.code == "not_admitted" then
      if state.task_operation == operation then
        diagnostic.set(state, "execution_notice", "Waiting to submit task control: " .. failure,
          { owner = identity, category = "admission", severity = "info" })
        refresh()
      end
      vim.defer_fn(function()
        if current() and not operation.admitted then
          client.request({ session_id = session_id, method = method or "session.command.apply", params = action }, receive)
        end
      end, 1000)
      return
    end
    if state.task_operation ~= operation then
      if result then
        operation.state = result.state
        if result.state == "completed" or result.state == "cancelled" or result.state == "failed"
          or result.state == "superseded" or result.state == "outcome_unknown" then
          remove()
          local message = result.error ~= vim.NIL and result.error or nil
          if result.state == "failed" or result.state == "outcome_unknown" then
            message = message or "Task transition failed"
            notifications.error(message, "Harness task")
          end
          settled(message, true)
        end
      elseif detail and detail.code ~= "outcome_unknown" and detail.code ~= "not_admitted" then
        remove()
        notifications.error(failure, "Harness task")
        settled(failure, true)
      end
      return
    end
    if failure then
      if detail and detail.code ~= "outcome_unknown" then
        remove()
        diagnostic.set(state, "execution_notice", "Not submitted: " .. failure,
          { owner = identity, category = "admission", severity = "warning", recovery = "resubmit" })
        notifications.error(state.execution_notice, "Harness task")
        if state.task_operation then
          state.task_config = state.task_operation.config
          if state.task_operation.poll then state.task_operation.poll() end
          settled(state.execution_notice, true)
          refresh()
          return
        end
        settled(state.execution_notice)
        return
      end
      diagnostic.set(state, "execution_notice", "Checking the original task operation: " .. failure,
        { owner = identity, category = "uncertain", severity = "info" })
      operation.state = "outcome_unknown"
      refresh()
      return
    end
    if not result or result == vim.NIL then
      operation.state = "outcome_unknown"
      diagnostic.set(state, "execution_notice", "Task acknowledgement unknown. Check task status before retrying.",
        { owner = identity, category = "uncertain", severity = "info" })
      refresh()
      return
    end
    diagnostic.set(state, "execution_notice", nil)
    operation.state = result.state
    if not operation.admitted then
      operation.admitted = true
      if admitted then admitted() end
    end
    if result.state == "completed" or result.state == "cancelled" or result.state == "failed" or result.state == "superseded" or result.state == "outcome_unknown" then
      remove()
      local message = result.error ~= vim.NIL and result.error or nil
      if result.state == "failed" or result.state == "outcome_unknown" then
        message = message or "Task transition failed"
        diagnostic.set(state, "execution_notice", message, { owner = identity, category = result.state,
          severity = result.state == "failed" and "error" or "warning", recovery = "inspect" })
        state.execution_notice_operation = { id = operation.id, message = message }
        if result.state == "failed" then notifications.error(message, "Harness task") end
      else
        diagnostic.set(state, "execution_notice", nil)
        state.execution_notice_operation = nil
      end
      settled(message, state.task_operation ~= nil)
      if state.task_operation then refresh() end
    else
      refresh()
    end
  end
  operation.receive = receive
  client.request({ session_id = session_id, method = method or "session.command.apply", params = action }, receive)
  local function poll(owner)
    if operation.poll_owner ~= owner then return end
    if not current() or state.host_error then return end
    if not operation.poll_pending then
      operation.poll_pending = true
      client.request({ session_id = session_id, method = "session.command.operation.get", params = { operation_id = identity } }, function(result, failure)
        operation.poll_pending = false
        if not current() then return end
        if failure then
          local notice = "Retrying task status: " .. failure
          operation.poll_error = notice
          diagnostic.set(state, "execution_notice", notice, { owner = identity, category = "poll", severity = "info" })
          refresh()
          return
        end
        if operation.poll_error and state.execution_notice == operation.poll_error then state.execution_notice = nil end
        operation.poll_error = nil
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
      diagnostic.set(state, "execution_notice", "Task acknowledgement unknown. Checking the original operation.",
        { owner = identity, category = "uncertain", severity = "info" })
      refresh()
    end
    operation.poll()
  end, 10000)
end

---@param state table
---@param payload table
function M.receive(state, payload)
  local operation = state.task_operation_by_id and state.task_operation_by_id[payload.id] or state.task_operation
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

---@param state table
---@param host table
---@param transition fun(action: table)
function M.open(state, host, transition)
  local task = M.current(state)
  if not task then
    M.plans(state, host, false, transition)
    return
  end
  local status = M.status(task)
  local options = {}
  if status == "running" or status == "waiting" then
    options[#options + 1] = { label = "Pause task", detail = M.phase(task), value = { action = "pause" } }
  else
    options[#options + 1] = { label = "Resume task", detail = M.phase(task), value = { action = "resume" } }
  end
  options[#options + 1] = { label = "Clear task", detail = M.title(task), value = { action = "clear" } }
  choose(host, M.title(task), options, transition)
end

---@param state table
---@param host table
---@param execute boolean
---@param transition fun(action: table)
function M.plans(state, host, execute, transition)
  local session_id = state.session.id
  client.request({ session_id = session_id, method = "session.plan.list", params = {} }, function(result, failure)
    if failure then notifications.error(failure, "Harness plans") return end
    if not state.session or state.session.id ~= session_id then return end
    local options = {}
    for _, plan in ipairs(result or {}) do
      if not execute or plan.revision_count > 0 then
        options[#options + 1] = { label = plan.title, detail = plan.state, value = plan }
      end
    end
    choose(host, execute and "Execute Plan" or "Plans", options, function(plan)
      if execute then transition({ action = "execute", plan_id = plan.id, digest = plan.digest }) return end
      local actions = {
        { label = "Start planning task", detail = "Use this saved design", value = { action = "attach_plan", plan_id = plan.id } },
        { label = "Fork plan", detail = "Reassess this design against the repository", value = { action = "fork", plan_id = plan.id } },
      }
      choose(host, plan.title, actions, transition)
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
