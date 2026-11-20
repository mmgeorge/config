local M = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local diagnostic = require("forge.views.harness.diagnostic")

---@class ForgeHarnessActiveTask
---@field id string
---@field kind 'goal'|'plan'|'execute'
---@field revision string
---@field phase 'draft'|'review'|'implement'|'check'|'resolve'|'verify'|'work'
---@field status string
---@field auto_accept_plan boolean
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
  return task.status
end

---@param task ForgeHarnessActiveTask
---@return string
function M.phase(task)
  return task.phase
end

---@param task ForgeHarnessActiveTask
---@param turn_pending? boolean a provider turn is running, the host hands off to the next one, or
---  a compaction holds a turn that runs after it (`activity.turn_awaited`)
---@return string
function M.label(task, turn_pending)
  local workflow = task.kind == "execute" and "exec" or task.kind
  local phase = M.phase(task)
  -- A planning turn may resubmit its plan before it ends, so a submission becomes the one to
  -- review only once the turn that made it settles and no successor, such as the execution an
  -- automatic acceptance starts, is about to replace it.
  if task.kind == "plan" and phase == "review" and turn_pending then phase = "draft" end
  local label = workflow .. " · " .. phase
  local status = M.status(task)
  if status ~= "running" and status ~= "waiting" then label = label .. " · " .. status end
  return label
end

---Tell the user how to carry out a plan acceptance the host only recorded.
---
---An acceptance made while the task is paused keeps the pause. Once no review or acceptance
---question remains to answer, the user resumes the task to execute the accepted plan.
---@param state table
---@param elicitation? table the question set still open after the acceptance or its last answer
---@return boolean reported
function M.report_recorded_acceptance(state, elicitation)
  local task = M.current(state)
  if elicitation == vim.NIL then elicitation = nil end
  if not task or task.status ~= "paused" or (elicitation
    and (elicitation.owner == "plan_review" or elicitation.owner == "plan_acceptance")) then return false end
  notifications.info("Plan acceptance recorded. The task is paused; run /task resume to execute.", "ForgePlanReview")
  return true
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
---@param settled fun(failure: string?, background: boolean?, detail: table?) `detail` carries the
---  host's structured error, whose `code` tells a committed change from a rejected one.
---@param method? string
function M.transition(state, action, refresh, settled, method)
  if state.host_error then settled(state.host_error) return end
  local session_id, generation = state.session.id, client.host_get_generation()
  action = vim.deepcopy(action)
  action.operation_id = require("forge.command").identity()
  if action.action == "pause" and state.input then state.input.pause_operation_id = action.operation_id end
  client.request({ session_id = session_id, method = method or "session.command.apply", params = action }, function(result, failure, detail)
    if not state.session or state.session.id ~= session_id or client.host_get_generation() ~= generation then
      settled(failure or "The session changed before command completion", true)
      return
    end
    if not failure and not result then failure = "Task command returned no result" end
    if failure then
      diagnostic.set(state, "execution_notice", failure, { owner = action.operation_id,
        category = detail and detail.code or "command", severity = "error" })
      notifications.error(failure, "Harness task")
    end
    settled(failure, nil, detail)
    refresh()
  end)
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
