local AgentSummary = {}

local active_status = {
  queued = true,
  starting = true,
  running = true,
  finalizing = true,
  awaiting_input = true,
  closing = true,
}

local function execution_status(run, exchange_list)
  if run.state == "closing" then return "closing" end
  if exchange_list and exchange_list.state then
    return exchange_list.state == "complete" and "completed" or exchange_list.state
  end
  local exchange = exchange_list and exchange_list[#exchange_list] or nil
  if exchange and active_status[exchange.state] then return exchange.state end
  if exchange and exchange.state then return exchange.state == "complete" and "completed" or exchange.state end
  return run.state or "unknown"
end

function AgentSummary.status(run, exchange_list)
  return execution_status(run, exchange_list)
end

--- Checks if an agent status string indicates active execution.
---@param status string? Status identifier string.
---@return boolean active True if status is starting, running, or waiting.
function AgentSummary.is_active(status)
  return active_status[status or ""] == true
end

--- Resolves a human-readable display label for an agent run record.
---@param run table Agent run descriptor table.
---@return string label Display label string.
function AgentSummary.label(run)
  return run.task_name or run.nickname or run.definition or "agent"
end

---@class ForgeAgentRunSummary
---@field state? string Latest task's exchange state.
---@field elapsed_ms? integer Execution time the latest task's exchange clock has accumulated.
---@field running_since_ms? integer Instant from which a running exchange's clock keeps counting.
---@field tool_label? string The status row's tool tally for the latest task.

--- Formats the agents picker's status for one child: state, duration, and tool tally.
---
--- The duration and tally come from the host's summary of the child's latest task exchange, the
--- clock and counter its status row, transcript row, and heading read, so all of them show one
--- number. The editor only keeps a running clock counting between summaries.
---@param run table Agent run descriptor table.
---@param summary ForgeAgentRunSummary? Host summary of the child's latest task.
---@param now_ms integer Current epoch timestamp in milliseconds.
---@return string detail Formatted status description string.
function AgentSummary.status_detail(run, summary, now_ms)
  local status = execution_status(run, summary)
  local elapsed_ms = summary and type(summary.elapsed_ms) == "number" and summary.elapsed_ms or 0
  local running_since_ms = summary and summary.running_since_ms
  if type(running_since_ms) == "number" then elapsed_ms = elapsed_ms + math.max(0, now_ms - running_since_ms) end
  local detail = ("%s %s %ds"):format(
    status:sub(1, 1):upper() .. status:sub(2),
    AgentSummary.is_active(status) and "for" or "in",
    math.floor(elapsed_ms / 1000)
  )
  local tool_label = summary and summary.tool_label
  if type(tool_label) == "string" and tool_label ~= "" then detail = detail .. " · " .. tool_label end
  return detail
end

return AgentSummary
