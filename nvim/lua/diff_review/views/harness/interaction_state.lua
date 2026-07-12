local M = {}

local function rebuild_index(state)
  state.interaction_by_id = {}
  for _, interaction in ipairs(state.interaction or {}) do
    if interaction.id then state.interaction_by_id[interaction.id] = interaction end
  end
end

---@param state DiffReviewHarnessPresentationState
---@param interaction_list table[]
function M.replace(state, interaction_list)
  state.interaction = vim.deepcopy(interaction_list or {})
  state.pending_interaction = nil
  rebuild_index(state)
end

---@param state DiffReviewHarnessPresentationState
---@param interaction_list table[]
function M.reconcile_snapshot(state, interaction_list)
  local pending = state.pending_interaction
  M.replace(state, interaction_list)
  if pending then
    state.interaction[#state.interaction + 1] = pending
    state.interaction_by_id[pending.id] = pending
    state.pending_interaction = pending
  end
end

---@param state DiffReviewHarnessPresentationState
---@param prompt string
---@return table
function M.begin(state, prompt)
  local interaction = {
    id = "pending:" .. tostring(vim.uv.hrtime()),
    prompt = prompt,
    state = "running",
    thought = {},
  }
  state.interaction[#state.interaction + 1] = interaction
  state.interaction_by_id[interaction.id] = interaction
  state.pending_interaction = interaction
  return interaction
end

local function resolve(state, interaction_id)
  local interaction = state.interaction_by_id[interaction_id]
  if interaction then return interaction end
  local pending = state.pending_interaction
  if pending then
    state.interaction_by_id[pending.id] = nil
    pending.id = interaction_id
    state.interaction_by_id[interaction_id] = pending
    state.pending_interaction = nil
    return pending
  end
  interaction = { id = interaction_id, prompt = "", state = "running", thought = {} }
  state.interaction[#state.interaction + 1] = interaction
  state.interaction_by_id[interaction_id] = interaction
  return interaction
end

---@param state DiffReviewHarnessPresentationState
---@param update table
function M.apply_active(state, update)
  if type(update) ~= "table" or not update.interaction_id then return end
  local interaction = resolve(state, update.interaction_id)
  local previous = interaction.active
  if previous and (previous.revision or 0) > (update.revision or 0) then return end
  interaction.active = vim.deepcopy(update)
  interaction.state = "running"
end

---@param state DiffReviewHarnessPresentationState
---@param thought table
function M.complete_thought(state, thought)
  if type(thought) ~= "table" or not thought.id then return end
  local interaction_id = thought.id:match("^(.-):thought:%d+$")
  if not interaction_id then return end
  local interaction = resolve(state, interaction_id)
  interaction.thought = interaction.thought or {}
  local replaced = false
  for index, previous in ipairs(interaction.thought) do
    if previous.id == thought.id then
      interaction.thought[index] = vim.deepcopy(thought)
      replaced = true
      break
    end
  end
  if not replaced then interaction.thought[#interaction.thought + 1] = vim.deepcopy(thought) end
  if interaction.active and interaction.active.thought_id == thought.id then interaction.active = nil end
end

---@param state DiffReviewHarnessPresentationState
---@param completed table
function M.complete_interaction(state, completed)
  if type(completed) ~= "table" or not completed.id then return end
  local current = resolve(state, completed.id)
  for key in pairs(current) do current[key] = nil end
  for key, value in pairs(vim.deepcopy(completed)) do current[key] = value end
  current.active = nil
  state.pending_interaction = nil
end

---@param state DiffReviewHarnessPresentationState
---@param message string
function M.fail_pending(state, message)
  local interaction = state.pending_interaction
  if not interaction then return end
  interaction.state = "failed"
  interaction.active = nil
  interaction.response = message
  interaction.completed_at_ms = math.floor(vim.uv.hrtime() / 1000000)
  state.pending_interaction = nil
end

return M
