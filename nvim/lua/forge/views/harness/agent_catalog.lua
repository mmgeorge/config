local AgentCatalog = {}

local agent_summary = require("forge.render.harness.agent_summary")

--- Split a label into text and number segments, so `explorer-10` compares as `explorer-` then 10.
---@param label string
---@return (string|integer)[]
local function label_segments(label)
  local segment_list = {}
  for text, digits in label:gmatch("(%D*)(%d*)") do
    if text ~= "" then segment_list[#segment_list + 1] = text end
    if digits ~= "" then segment_list[#segment_list + 1] = tonumber(digits) end
  end
  return segment_list
end

--- Order labels by their number segments as numbers, so `-2` precedes `-10`.
---@param left_label string
---@param right_label string
---@return boolean?
local function label_before(left_label, right_label)
  local left_segments, right_segments = label_segments(left_label), label_segments(right_label)
  for index = 1, math.min(#left_segments, #right_segments) do
    local left, right = left_segments[index], right_segments[index]
    if left ~= right then
      if type(left) ~= type(right) then return type(left) == "number" end
      return left < right
    end
  end
  if #left_segments ~= #right_segments then return #left_segments < #right_segments end
  return nil
end

---@param left table
---@param right table
---@return boolean
local function run_before(left, right)
  local before = label_before(agent_summary.label(left):lower(), agent_summary.label(right):lower())
  if before ~= nil then return before end
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
    local exchange_list = (state.agent.summary or {})[run.id]
    if run.source ~= "root" and run.definition ~= "primary"
      and agent_summary.is_active(agent_summary.status(run, exchange_list)) == active then
      result[#result + 1] = run
    end
  end
  table.sort(result, run_before)
  return result
end

--- Replace the view's child list unless the incoming list was read before the current one.
---
--- The host delivers child lists in `agent_updated` events and inside whole session snapshots,
--- and a list read earlier can arrive after a later one. Applying it showed a settled child as
--- running again, so an interrupt chosen from that list addressed a child that had already
--- finished. Revisions restart with each host run, so they compare only within one runtime epoch.
---@param state table
---@param agent table
---@return boolean replaced
function AgentCatalog.replace(state, agent)
  local current = state.agent
  if current and type(current.revision) == "number" and type(agent.revision) == "number"
    and current.runtime_epoch == agent.runtime_epoch and agent.revision < current.revision then
    return false
  end
  state.agent = vim.deepcopy(agent)
  return true
end

local interrupt_failure_text = {
  stale_target = "the child's task changed before the interrupt applied",
  rejected = "the child coordinator rejected the interrupt",
  cleanup_failed = "the child's cleanup remains unresolved",
  outcome_uncertain = "the interrupt is still unconfirmed",
  invalid_completion = "the child coordinator returned an invalid result",
  worker_lost = "the interrupt ended without a result",
}

--- Name the first child an interrupt response could not interrupt, with the reason.
---
--- `session.agent.interrupt` and `session.agent.interrupt_all` succeed as a request and carry one
--- `{ target, result }` entry per child, whose `result` is `{ Ok = vim.NIL }` or
--- `{ Err = { failure = <code> } }`.
---@param response table? Host response to an interrupt request.
---@return string? message
function AgentCatalog.interrupt_failure(response)
  if type(response) ~= "table" or type(response.targets) ~= "table" then return nil end
  for _, entry in ipairs(response.targets) do
    local failure = type(entry) == "table" and type(entry.result) == "table" and entry.result.Err
    if failure then
      local code = type(failure) == "table" and failure.failure or tostring(failure)
      return interrupt_failure_text[code] or ("the interrupt failed: " .. tostring(code):gsub("_", " "))
    end
  end
  return nil
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
    if run.source ~= "root" and run.definition ~= "primary"
      and (run.id == selector or run.task_path == selector or agent_summary.label(run):lower() == selector:lower()) then
      match_list[#match_list + 1] = run
    end
  end
  if #match_list == 1 then return match_list[1] end
  if #match_list > 1 then
    return nil, ("Multiple child agents match %s. Use /agents or a running-agent alias."):format(selector)
  end
  return nil, ("No child agent matches %s"):format(selector)
end

return AgentCatalog
