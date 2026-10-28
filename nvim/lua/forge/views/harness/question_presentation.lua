local M = {}

---@param active_elicitation table?
---@return string?
function M.key(active_elicitation)
  local elicitation = active_elicitation and active_elicitation.elicitation
  local question_set = elicitation and elicitation.question_set
  if not (active_elicitation and elicitation and question_set) then return nil end
  local owner_id = active_elicitation.plan_id or active_elicitation.exchange_id or "unknown"
  local revision = elicitation.revision or question_set.id or 1
  return table.concat({ active_elicitation.owner or "unknown", owner_id, question_set.id, tostring(revision) }, ":")
end

---@param state ForgeHarnessPresentationState
---@return boolean
function M.should_present(state)
  local key = M.key(state.active_elicitation)
  return not state.busy and key ~= nil and key ~= state.presented_question_key
end

function M.receive(state, incoming, replace)
  local current = state.active_elicitation
  if not incoming or incoming == vim.NIL then return false end
  if not replace and M.key(current) ~= M.key(incoming) then return false end
  if current and current ~= vim.NIL and current.owner == incoming.owner
    and current.plan_id == incoming.plan_id and current.exchange_id == incoming.exchange_id
    and current.elicitation.question_set.id == incoming.elicitation.question_set.id then
    local before, after = current.elicitation, incoming.elicitation
    if (after.revision or 0) < (before.revision or 0) then return false end
    if after.revision == before.revision and (after.answer_revision or 0) < (before.answer_revision or 0) then return false end
  end
  state.active_elicitation = incoming
  return true
end

---@param state ForgeHarnessPresentationState
function M.mark_presented(state)
  state.presented_question_key = M.key(state.active_elicitation)
end

---@param state ForgeHarnessPresentationState
function M.reset(state)
  state.presented_question_key = nil
end

return M
