local M = {}

---@param active_elicitation table?
---@return string?
function M.key(active_elicitation)
  local elicitation = active_elicitation and active_elicitation.elicitation
  local question_set = elicitation and elicitation.question_set
  if not question_set then return nil end
  local owner_id = active_elicitation.plan_id or active_elicitation.interaction_id or "unknown"
  local revision = elicitation.revision or question_set.id or 1
  return table.concat({ active_elicitation.owner or "unknown", owner_id, tostring(revision) }, ":")
end

---@param state DiffReviewHarnessPresentationState
---@return boolean
function M.should_present(state)
  local key = M.key(state.active_elicitation)
  return key ~= nil and key ~= state.presented_question_key
end

---@param state DiffReviewHarnessPresentationState
function M.mark_presented(state)
  state.presented_question_key = M.key(state.active_elicitation)
end

---@param state DiffReviewHarnessPresentationState
function M.reset(state)
  state.presented_question_key = nil
end

return M
