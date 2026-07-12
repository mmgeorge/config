local M = {}

---@param state DiffReviewHarnessPresentationState
---@return integer
function M.revision(state)
  return state.transcript_revision or 0
end

---@param state DiffReviewHarnessPresentationState
function M.mark_changed(state)
  state.transcript_revision = M.revision(state) + 1
end

---@param state DiffReviewHarnessPresentationState
---@param event table
function M.append(state, event)
  state.transcript[#state.transcript + 1] = event
  M.mark_changed(state)
end

---@param state DiffReviewHarnessPresentationState
---@param transcript table[]
function M.replace(state, transcript)
  state.transcript = transcript
  M.mark_changed(state)
end

---@param state DiffReviewHarnessPresentationState
---@param snapshot table[]
---@param observed_revision integer
---@param observed_count integer
function M.reconcile_snapshot(state, snapshot, observed_revision, observed_count)
  if M.revision(state) == observed_revision then
    M.replace(state, snapshot)
    return
  end

  local merged = vim.deepcopy(snapshot)
  for event_index = observed_count + 1, #state.transcript do
    merged[#merged + 1] = state.transcript[event_index]
  end
  M.replace(state, merged)
end

return M
