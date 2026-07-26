local HarnessSnapshot = {}

local timeline_cache = require("diff_review.views.harness.timeline_cache")
local prompt_history = require("diff_review.views.harness.prompt_history")

---@param state table
---@param result table
function HarnessSnapshot.apply(state, result)
  local previous_session_id = state.session and state.session.id or nil
  local previous_context_usage = state.session and state.session.context_usage or nil
  state.session = result.session
  if previous_session_id == (result.session and result.session.id or nil)
    and previous_context_usage and not state.session.context_usage
  then
    state.session.context_usage = previous_context_usage
  end
  if previous_session_id ~= (result.session and result.session.id or nil) then state.activity_expanded = {} end
  state.capability = result.capability or {}
  timeline_cache.replace(state, result.timeline or {}, result.timeline_revision or 0)
  state.artifact = vim.deepcopy(result.artifact or {})
  state.no_checkpoint = result.no_checkpoint == true
  state.goal = result.goal
  state.goal_execution = result.goal_execution
  state.active_plan = result.active_plan
  state.active_elicitation = result.active_elicitation
  state.active_wait = vim.deepcopy(result.active_wait)
  state.approval = vim.deepcopy(result.approval or {})
  state.agent = vim.deepcopy(result.agent or { definition = {}, run = {}, turn = {} })
  if state.selected_agent_run_id then
    local selected_exists = vim.iter(state.agent.run or {}):any(function(run)
      return run.id == state.selected_agent_run_id
    end)
    if not selected_exists then state.selected_agent_run_id = nil end
  end
  if result.prompt_history then prompt_history.replace(result.prompt_history) end
end

return HarnessSnapshot
