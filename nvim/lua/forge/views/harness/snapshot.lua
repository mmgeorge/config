local HarnessSnapshot = {}

local prompt_history = require("forge.views.harness.prompt_history")

---Convert nullable snapshot fields to absent Lua values without changing the wire response.
local function presentation_value(value)
  if value == vim.NIL then return nil end
  if type(value) ~= "table" then return value end
  local projected = {}
  for key, field in pairs(value) do projected[key] = presentation_value(field) end
  return projected
end

---@param state table
---@param result table
function HarnessSnapshot.apply(state, result)
  result = presentation_value(result)
  if state.session and result.session and state.session.id == result.session.id
    and state.runtime_epoch == result.runtime_epoch
    and (result.snapshot_revision or 0) < (state.snapshot_revision or 0) then return false end
  if state.runtime_epoch and state.runtime_epoch ~= result.runtime_epoch then
    state.task_operation, state.busy, state.restore_applying = nil, false, nil
    require("forge.views.harness.timeline_status").stop(state)
  end
  state.runtime_epoch, state.snapshot_revision = result.runtime_epoch, result.snapshot_revision
  if state.host_error then state.host_error, state.execution_notice = nil, nil end
  local operation = result.task_operation
  if operation and (operation.state == "failed" or operation.state == "outcome_unknown") then
    state.execution_notice = operation.error or "Task outcome unknown. Refresh before resuming."
    state.execution_notice_operation = { id = operation.id, message = state.execution_notice }
  elseif state.execution_notice_operation then
    if state.execution_notice == state.execution_notice_operation.message then state.execution_notice = nil end
    state.execution_notice_operation = nil
  end
  local previous_session_id = state.session and state.session.id or nil
  local previous_context_usage = state.session and state.session.context_usage or nil
  state.session = result.session
  if previous_session_id == (result.session and result.session.id or nil)
    and previous_context_usage and not state.session.context_usage
  then
    state.session.context_usage = previous_context_usage
  end
  if previous_session_id ~= (result.session and result.session.id or nil) then
    state.restore_applying = nil
    state.activity_expanded = {}
    require("forge.views.harness.recap").clear(state)
  end
  state.capability = result.capability or {}
  state.timeline_revision = result.timeline_revision or 0
  state.status = result.status or { kind = "idle" }
  state.artifact = vim.deepcopy(result.artifact or {})
  state.no_checkpoint = result.no_checkpoint == true
  state.restore_recovery = result.restore_recovery
  state.task = result.task or {}
  state.goal = result.goal
  state.goal_execution = result.goal_execution
  state.active_plan = result.active_plan
  state.active_elicitation = result.active_elicitation
  state.active_wait = vim.deepcopy(result.active_wait)
  state.approval = vim.deepcopy(result.approval or {})
  state.agent = vim.deepcopy(result.agent or { definition = {}, run = {}, exchange = {} })
  if state.selected_agent_run_id then
    local selected_exists = vim.iter(state.agent.run or {}):any(function(run)
      return run.id == state.selected_agent_run_id
    end)
    if not selected_exists then state.selected_agent_run_id = nil end
  end
  if result.prompt_history then prompt_history.replace(result.prompt_history) end
  return true
end

return HarnessSnapshot
