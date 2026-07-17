local M = {}

local function now_ns()
  return vim.uv.hrtime()
end

local function pending_segment(interaction_id)
  return {
    kind = "main_segment",
    segment = {
      id = interaction_id .. ":segment:pending",
      state = "running",
      duration_ms = 0,
      thought = {},
      presentation_placeholder = true,
    },
  }
end

local function apply_presentation_duration(state, interaction)
  local presentation = state.interaction_presentation
    and state.interaction_presentation[interaction.id]
  if not (presentation and presentation.duration_ms) then return end
  for node_index = #(interaction.node_list or {}), 1, -1 do
    local segment = interaction.node_list[node_index].segment
    if segment then
      segment.presentation_duration_ms = presentation.duration_ms
      return
    end
  end
end

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
  state.interaction_presentation = {}
  rebuild_index(state)
end

---@param state DiffReviewHarnessPresentationState
---@param interaction_list table[]
function M.reconcile_snapshot(state, interaction_list)
  local pending = state.pending_interaction
  local presentation = state.interaction_presentation or {}
  M.replace(state, interaction_list)
  state.interaction_presentation = presentation
  for _, interaction in ipairs(state.interaction) do
    apply_presentation_duration(state, interaction)
  end
  if pending then
    state.interaction[#state.interaction + 1] = pending
    state.interaction_by_id[pending.id] = pending
    state.pending_interaction = pending
  end
end

---@param state DiffReviewHarnessPresentationState
---@param prompt string
---@param started_ns? integer
---@return table
function M.begin(state, prompt, started_ns)
  local effective_started_ns = started_ns or now_ns()
  local interaction_id = "pending:" .. tostring(effective_started_ns)
  local interaction = {
    id = interaction_id,
    prompt = prompt,
    state = "running",
    node_list = { pending_segment(interaction_id) },
  }
  state.interaction_presentation = state.interaction_presentation or {}
  state.interaction_presentation[interaction_id] = { started_ns = effective_started_ns }
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
    local presentation = state.interaction_presentation
      and state.interaction_presentation[pending.id]
    state.interaction_by_id[pending.id] = nil
    if state.interaction_presentation then state.interaction_presentation[pending.id] = nil end
    pending.id = interaction_id
    state.interaction_by_id[interaction_id] = pending
    if presentation then state.interaction_presentation[interaction_id] = presentation end
    state.pending_interaction = nil
    return pending
  end
  interaction = { id = interaction_id, prompt = "", state = "running", node_list = {} }
  state.interaction[#state.interaction + 1] = interaction
  state.interaction_by_id[interaction_id] = interaction
  return interaction
end

---@param state DiffReviewHarnessPresentationState
---@param started table
function M.start_interaction(state, started)
  if type(started) ~= "table" or not started.id then return end
  local current = resolve(state, started.id)
  local placeholder_list = current.node_list or {}
  local incoming = vim.deepcopy(started)
  for key, value in pairs(incoming) do current[key] = value end
  if #(current.node_list or {}) == 0 then current.node_list = placeholder_list end
end

---@param state DiffReviewHarnessPresentationState
---@param update table
function M.apply_node(state, update)
  if type(update) ~= "table" or not update.interaction_id then return end
  local node = update.node
  if type(node) ~= "table" then return end
  local interaction = resolve(state, update.interaction_id)
  interaction.node_list = interaction.node_list or {}
  for index = #interaction.node_list, 1, -1 do
    local segment = interaction.node_list[index].segment
    if segment and segment.presentation_placeholder then table.remove(interaction.node_list, index) end
  end
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
  if type(update) ~= "table" then return end
  state.active_wait = type(update.wait) == "table" and vim.deepcopy(update.wait) or nil
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
---@param completed table
---@param completed_ns? integer
function M.complete_interaction(state, completed, completed_ns)
  if type(completed) ~= "table" or not completed.id then return end
  local current = resolve(state, completed.id)
  local presentation = state.interaction_presentation
    and state.interaction_presentation[completed.id]
  if presentation and presentation.started_ns then
    presentation.duration_ms = math.max(0, math.floor(((completed_ns or now_ns()) - presentation.started_ns) / 1000000))
  end
  for key in pairs(current) do current[key] = nil end
  for key, value in pairs(vim.deepcopy(completed)) do current[key] = value end
  apply_presentation_duration(state, current)
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
  interaction.node_list = {}
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
  local completed_ns = now_ns()
  interaction.completed_at_ms = math.floor(completed_ns / 1000000)
  local presentation = state.interaction_presentation
    and state.interaction_presentation[interaction.id]
  if presentation and presentation.started_ns then
    presentation.duration_ms = math.max(0, math.floor((completed_ns - presentation.started_ns) / 1000000))
    apply_presentation_duration(state, interaction)
  end
  state.pending_interaction = nil
end

return M
