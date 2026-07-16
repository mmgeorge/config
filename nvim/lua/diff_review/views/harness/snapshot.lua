local HarnessSnapshot = {}

local interaction_state = require("diff_review.views.harness.interaction_state")
local prompt_history = require("diff_review.views.harness.prompt_history")

---@param state table
---@param result table
---@param interaction_mode? "replace"|"reconcile"
function HarnessSnapshot.apply(state, result, interaction_mode)
  local previous_session_id = state.session and state.session.id or nil
  state.session = result.session
  if previous_session_id ~= (result.session and result.session.id or nil) then state.activity_expanded = {} end
  state.capability = result.capability or {}
  if interaction_mode == "reconcile" then
    interaction_state.reconcile_snapshot(state, result.interaction or {})
  else
    interaction_state.replace(state, result.interaction or {})
  end
  state.timeline = vim.deepcopy(result.timeline or {})
  state.artifact = vim.deepcopy(result.artifact or {})
  state.no_checkpoint = result.no_checkpoint == true
  state.goal = result.goal
  state.active_plan = result.active_plan
  state.active_elicitation = result.active_elicitation
  state.approval = vim.deepcopy(result.approval or {})
  state.agent = vim.deepcopy(result.agent or { definition = {}, run = {}, turn = {} })
  state.agent_live = {}
  if state.selected_agent_run_id then
    local selected_exists = vim.iter(state.agent.run or {}):any(function(run)
      return run.id == state.selected_agent_run_id
    end)
    if not selected_exists then state.selected_agent_run_id = nil end
  end
  if result.prompt_history then prompt_history.replace(result.prompt_history) end
end

return HarnessSnapshot
