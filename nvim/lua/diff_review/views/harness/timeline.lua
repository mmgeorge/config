local M = {}

---Project durable and live broker state into the main Harness timeline.
---@param state table
---@return table[]
function M.project(state)
  local source = vim.deepcopy(state.timeline or {})
  local interaction_by_id = {}
  for _, interaction in ipairs(state.interaction or {}) do interaction_by_id[interaction.id] = interaction end
  local known_interaction = {}
  for _, entry in ipairs(source) do
    if entry.kind == "interaction" and entry.interaction then
      known_interaction[entry.interaction.id] = true
      entry.interaction = vim.deepcopy(interaction_by_id[entry.interaction.id] or entry.interaction)
    end
  end
  for _, interaction in ipairs(state.interaction or {}) do
    if not known_interaction[interaction.id] then
      source[#source + 1] = { kind = "interaction", interaction = vim.deepcopy(interaction) }
    end
  end

  local timeline = {}
  local lifecycle_by_run = {}
  local interaction_entry_by_id = {}
  for _, entry in ipairs(source) do
    if entry.kind == "agent_lifecycle" and entry.run then
      lifecycle_by_run[entry.run.id] = entry
    else
      timeline[#timeline + 1] = entry
      if entry.kind == "interaction" and entry.interaction then
        interaction_entry_by_id[entry.interaction.id] = entry
      end
    end
  end

  local interaction_by_run = {}
  for _, turn in ipairs((state.agent and state.agent.turn) or {}) do
    interaction_by_run[turn.agent_run_id] = interaction_by_run[turn.agent_run_id] or {}
    interaction_by_run[turn.agent_run_id][#interaction_by_run[turn.agent_run_id] + 1] = turn.interaction
  end
  local child_by_run = {}
  local child_by_thread = {}
  local run_list = (state.agent and state.agent.run) or {}
  for _, run in ipairs(run_list) do
    local child = lifecycle_by_run[run.id] or { kind = "agent_lifecycle", id = run.id }
    child.run = vim.deepcopy(run)
    child.interaction = interaction_by_run[run.id] or {}
    local live = (state.agent_live or {})[run.id]
    if live and live.interaction then
      local interaction = vim.deepcopy(live.interaction)
      interaction.active = vim.deepcopy(live.active)
      local replaced = false
      for index, previous in ipairs(child.interaction) do
        if previous.id == interaction.id then
          child.interaction[index] = interaction
          replaced = true
          break
        end
      end
      if not replaced then child.interaction[#child.interaction + 1] = interaction end
    end
    child_by_run[run.id] = child
    if run.provider_thread_id then child_by_thread[run.provider_thread_id] = child end
  end

  for _, run in ipairs(run_list) do
    local child = child_by_run[run.id]
    local parent_agent = run.parent_thread_id and child_by_thread[run.parent_thread_id] or nil
    if parent_agent and parent_agent ~= child then
      parent_agent.agent = parent_agent.agent or {}
      parent_agent.agent[#parent_agent.agent + 1] = child
    else
      local parent = run.parent_interaction_id and interaction_entry_by_id[run.parent_interaction_id] or nil
      if parent then
        parent.agent_by_id = parent.agent_by_id or {}
        parent.agent_by_id[run.id] = child
      else
        timeline[#timeline + 1] = child
      end
    end
  end
  return timeline
end

return M
