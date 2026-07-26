local M = {}

---@class DiffReviewHarnessTimelinePatch
---@field session_id string
---@field base_revision integer
---@field revision integer
---@field operation DiffReviewHarnessTimelineOperation[]

---@class DiffReviewHarnessTimelineOperation
---@field kind "insert"|"replace"|"remove"
---@field index integer
---@field id string?
---@field entry table?

local function trace(state, event, detail)
  local record = vim.tbl_extend("force", {
    event = event,
    timestamp_ms = vim.uv.now(),
    session_id = state.session and state.session.id or nil,
    revision = state.timeline_revision,
  }, detail or {})
  local ok, line = pcall(vim.json.encode, record)
  if not ok then return end
  pcall(
    vim.fn.writefile,
    { line },
    vim.fs.joinpath(vim.fn.stdpath("cache"), "diff-review-harness-timeline-debug.jsonl"),
    "a"
  )
end

local function synchronize_status(state)
  local final_entry = state.timeline[#state.timeline]
  if final_entry and final_entry.kind == "status" then
    state.status = vim.deepcopy(final_entry.status)
  else
    state.status = { kind = "idle" }
  end
end

---@param state DiffReviewHarnessPresentationState
---@param entry_list table[]
---@param revision integer
function M.replace(state, entry_list, revision)
  state.timeline = vim.deepcopy(entry_list)
  state.timeline_revision = revision
  synchronize_status(state)
  trace(state, "timeline_snapshot_applied", {
    entry_count = #state.timeline,
  })
end

---@param state DiffReviewHarnessPresentationState
---@param patch DiffReviewHarnessTimelinePatch
---@return boolean applied
---@return string? error
function M.apply(state, patch)
  local session_id = state.session and state.session.id
  if patch.session_id ~= session_id then
    return false, ("timeline patch belongs to session %s"):format(tostring(patch.session_id))
  end
  if patch.base_revision ~= state.timeline_revision then
    trace(state, "timeline_revision_gap", {
      base_revision = patch.base_revision,
      received_revision = patch.revision,
    })
    return false, ("timeline revision gap: have %s, received base %s")
      :format(tostring(state.timeline_revision), tostring(patch.base_revision))
  end

  local next_timeline = vim.deepcopy(state.timeline or {})
  for _, operation in ipairs(patch.operation or {}) do
    local lua_index = operation.index + 1
    if operation.kind == "insert" then
      if lua_index < 1 or lua_index > #next_timeline + 1 or not operation.entry then
        return false, "invalid timeline insert operation"
      end
      table.insert(next_timeline, lua_index, vim.deepcopy(operation.entry))
    elseif operation.kind == "replace" then
      if lua_index < 1 or lua_index > #next_timeline or not operation.entry then
        return false, "invalid timeline replace operation"
      end
      next_timeline[lua_index] = vim.deepcopy(operation.entry)
    elseif operation.kind == "remove" then
      local existing = next_timeline[lua_index]
      if not existing or existing.id ~= operation.id then
        return false, "timeline remove identity mismatch"
      end
      table.remove(next_timeline, lua_index)
    else
      return false, ("unknown timeline operation: %s"):format(tostring(operation.kind))
    end
  end

  state.timeline = next_timeline
  state.timeline_revision = patch.revision
  synchronize_status(state)
  trace(state, "timeline_patch_applied", {
    base_revision = patch.base_revision,
    operation_count = #(patch.operation or {}),
  })
  return true
end

---@param state DiffReviewHarnessPresentationState
---@return table[]
function M.history(state)
  local entry_list = state.timeline or {}
  if entry_list[#entry_list] and entry_list[#entry_list].kind == "status" then
    return vim.list_slice(entry_list, 1, #entry_list - 1)
  end
  return entry_list
end

local function find_agent(entry_list, run_id)
  for _, entry in ipairs(entry_list or {}) do
    if entry.kind == "agent_lifecycle" then
      if entry.run and entry.run.id == run_id then return entry end
      local nested = find_agent(entry.agent, run_id)
      if nested then return nested end
    end
    for _, attached in pairs(entry.agent_by_id or {}) do
      if attached.run and attached.run.id == run_id then return attached end
      local nested = find_agent(attached.agent, run_id)
      if nested then return nested end
    end
  end
  return nil
end

---@param state DiffReviewHarnessPresentationState
---@return table[]?
function M.selected_agent_history(state)
  if not state.selected_agent_run_id then return nil end
  local agent_entry = find_agent(M.history(state), state.selected_agent_run_id)
  if not agent_entry then return {} end
  local timeline = {}
  for _, interaction in ipairs(agent_entry.interaction or {}) do
    timeline[#timeline + 1] = {
      kind = "interaction",
      id = interaction.id,
      created_at_ms = interaction.created_at_ms,
      interaction = interaction,
      agent_by_id = {},
    }
  end
  return timeline
end

---@param state DiffReviewHarnessPresentationState
---@param run_id string
---@return table[]
function M.agent_interaction_list(state, run_id)
  local agent_entry = find_agent(M.history(state), run_id)
  return agent_entry and agent_entry.interaction or {}
end

return M
