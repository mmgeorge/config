local HarnessSnapshot = {}

local prompt_history = require("forge.views.harness.prompt_history")
local diagnostic = require("forge.views.harness.diagnostic")

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
  result = assert(presentation_value(result), "Harness snapshot is missing")
  if state.session and result.session and state.session.id == result.session.id
    and state.runtime_epoch == result.runtime_epoch
    and (result.snapshot_revision or 0) < (state.snapshot_revision or 0) then return false end
  if state.runtime_epoch and state.runtime_epoch ~= result.runtime_epoch then
    state.task_operation, state.busy, state.restore_applying = nil, false, nil
    state.check_interrupt_pending = nil
    if state.presentation and state.presentation.transcript then
      require("forge.views.harness.status_hint").clear(state.presentation.transcript.buffer)
    end
  end
  state.runtime_epoch, state.snapshot_revision = result.runtime_epoch, result.snapshot_revision
  if state.host_error then state.host_error, state.execution_notice = nil, nil end
  local operation = result.task_operation
  if operation and (operation.state == "failed" or operation.state == "outcome_unknown") then
    diagnostic.set(state, "execution_notice", operation.error or "Task outcome unknown. Refresh before resuming.",
      { owner = operation.id, category = operation.state, severity = operation.state == "failed" and "error" or "warning",
        recovery = "inspect" })
    state.execution_notice_operation = { id = operation.id, message = state.execution_notice }
  elseif state.execution_notice_operation then
    if state.execution_notice == state.execution_notice_operation.message then diagnostic.set(state, "execution_notice", nil) end
    state.execution_notice_operation = nil
  end
  local previous_session_id = state.session and state.session.id or nil
  local previous_context_usage = state.session and state.session.context_usage or nil
  state.session = result.session
  state.access_abbreviation = result.access_abbreviation
  if state.session and state.session.workspace then
    require("forge.lsp.startup").start(state.session.workspace)
  end
  if previous_session_id == (result.session and result.session.id or nil)
    and previous_context_usage and not state.session.context_usage
  then
    state.session.context_usage = previous_context_usage
  end
  if previous_session_id ~= (result.session and result.session.id or nil) then
    state.check_interrupt_pending = nil
    state.restore_applying = nil
    state.activity_expanded = {}
    require("forge.views.harness.recap").clear(state)
  end
  state.capability = result.capability or {}
  state.subagent_tools = result.subagent_tools == true
  state.lsp_tools = result.lsp_tools == true
  state.session_tools = result.session_tools ~= false
  state.status_thoughts = result.status_thoughts == true
  state.check_editing = result.check_editing == true
  state.checks_running = result.checks_running == true
  state.check_timing = result.check_timing or "after_queue"
  state.automatic_checks = result.automatic_checks ~= false
  state.checks_deferred = result.checks_deferred == true
  state.active_check_run = result.active_check_run
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
