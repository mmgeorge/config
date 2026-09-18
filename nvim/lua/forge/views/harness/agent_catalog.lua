local AgentCatalog = {}

local agent_summary = require("forge.render.harness.agent_summary")
local timeline_cache = require("forge.views.harness.timeline_cache")

---@param left table
---@param right table
---@return boolean
local function run_before(left, right)
  local left_label = agent_summary.label(left):lower()
  local right_label = agent_summary.label(right):lower()
  if left_label ~= right_label then return left_label < right_label end
  if (left.created_at_ms or 0) ~= (right.created_at_ms or 0) then
    return (left.created_at_ms or 0) < (right.created_at_ms or 0)
  end
  return tostring(left.id or "") < tostring(right.id or "")
end

---@param state table
---@param active boolean
---@return table[]
function AgentCatalog.run_list(state, active)
  local result = {}
  local agent = state.agent
  for _, run in ipairs((agent and agent.run) or {}) do
    local exchange_list = timeline_cache.agent_exchange_list(state, run.id)
    if agent_summary.is_active(agent_summary.status(run, exchange_list)) == active then
      result[#result + 1] = run
    end
  end
  table.sort(result, run_before)
  return result
end

---@param state table
---@param selector string
---@return table? run
---@return string? error_message
function AgentCatalog.resolve(state, selector)
  local agent = state.agent
  local active_run_list = AgentCatalog.run_list(state, true)
  local numeric_index = selector:match("^%d+$") and tonumber(selector) or nil
  local letter_index = selector:match("^[a-z]$") and (selector:byte() - string.byte("a")) or nil
  local zero_based_index = numeric_index or letter_index
  if zero_based_index then
    local run = active_run_list[zero_based_index + 1]
    if run then return run end
    return nil, ("No running child agent exists at position %s"):format(selector)
  end

  local match_list = {}
  for _, run in ipairs((agent and agent.run) or {}) do
    if run.id == selector or agent_summary.label(run):lower() == selector:lower() then
      match_list[#match_list + 1] = run
    end
  end
  if #match_list == 1 then return match_list[1] end
  if #match_list > 1 then
    return nil, ("Multiple child agents match %s. Use /agent or a running-agent alias."):format(selector)
  end
  return nil, ("No child agent matches %s"):format(selector)
end

return AgentCatalog
