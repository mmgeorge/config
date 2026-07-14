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
    node_list = {},
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
  interaction = { id = interaction_id, prompt = "", state = "running", node_list = {} }
  state.interaction[#state.interaction + 1] = interaction
  state.interaction_by_id[interaction_id] = interaction
  return interaction
end

---@param state DiffReviewHarnessPresentationState
---@param update table
function M.apply_node(state, update)
  if type(update) ~= "table" or not update.interaction_id then return end
  local node = update.node
  if type(node) ~= "table" then return end
  local interaction = resolve(state, update.interaction_id)
  if node.kind == "steering_prompt" then interaction.active_wait = nil end
  interaction.node_list = interaction.node_list or {}
  local node_id = node.segment and node.segment.id
    or node.agent and node.agent.id
    or node.prompt and node.prompt.id
  if not node_id then return end
  for index, previous in ipairs(interaction.node_list) do
    local previous_id = previous.segment and previous.segment.id
      or previous.agent and previous.agent.id
      or previous.prompt and previous.prompt.id
    if previous_id == node_id then
      interaction.node_list[index] = vim.deepcopy(node)
      interaction.state = "running"
      return
    end
  end
  interaction.node_list[#interaction.node_list + 1] = vim.deepcopy(node)
  interaction.state = "running"
end

---@param state DiffReviewHarnessPresentationState
---@param update table
function M.apply_wait(state, update)
  if type(update) ~= "table" or not update.interaction_id then return end
  local interaction = resolve(state, update.interaction_id)
  interaction.active_wait = type(update.wait) == "table" and vim.deepcopy(update.wait) or nil
end

---@param state DiffReviewHarnessPresentationState
---@param snapshot table
function M.apply_task(state, snapshot)
  if type(snapshot) ~= "table" then return end
  local interaction = state.pending_interaction or state.interaction[#state.interaction]
  if not interaction then return end
  local previous = interaction.task
  if previous and (previous.revision or 0) > (snapshot.revision or 0) then return end
  interaction.task = vim.deepcopy(snapshot)
end

---@param state DiffReviewHarnessPresentationState
---@param thought table
---@param state DiffReviewHarnessPresentationState
---@param completed table
function M.complete_interaction(state, completed)
  if type(completed) ~= "table" or not completed.id then return end
  local current = resolve(state, completed.id)
  for key in pairs(current) do current[key] = nil end
  for key, value in pairs(vim.deepcopy(completed)) do current[key] = value end
  current.active_wait = nil
  state.pending_interaction = nil
end

---@param state DiffReviewHarnessPresentationState
---@param retracted table
function M.retract(state, retracted)
  if type(retracted) ~= "table" or not retracted.interaction_id then return end
  local pending = state.pending_interaction
  local target_id = retracted.interaction_id
  if pending and pending.prompt == retracted.prompt then target_id = pending.id end
  for index, interaction in ipairs(state.interaction or {}) do
    if interaction.id == target_id or interaction.id == retracted.interaction_id then
      table.remove(state.interaction, index)
      break
    end
  end
  if pending and (pending.id == target_id or pending.id == retracted.interaction_id) then
    state.pending_interaction = nil
  end
  rebuild_index(state)
end

---@param state DiffReviewHarnessPresentationState
---@param message string
function M.fail_pending(state, message)
  local interaction = state.pending_interaction
  if not interaction then return end
  interaction.state = "failed"
  interaction.active_wait = nil
  interaction.node_list = interaction.node_list or {}
  interaction.node_list[#interaction.node_list + 1] = {
    kind = "main_segment",
    segment = {
      id = interaction.id .. ":segment:error",
      state = "complete",
      duration_ms = 0,
      thought = {},
      response = message,
    },
  }
  interaction.completed_at_ms = math.floor(vim.uv.hrtime() / 1000000)
  state.pending_interaction = nil
end

return M
