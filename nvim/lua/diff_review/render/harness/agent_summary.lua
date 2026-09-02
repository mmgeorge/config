local AgentSummary = {}

local tool_render = require("diff_review.render.harness.tool")

local active_status = {
  starting = true,
  running = true,
  waiting = true,
}

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
  return run.nickname or run.definition or "agent"
end

--- Computes total and failed tool invocation counts across interactions.
---@param interaction_list table[]? Array of interaction node tables.
---@return integer tool_count Total count of tool invocations.
---@return integer failed_count Count of failed tool executions.
function AgentSummary.tool_counts(interaction_list)
  local tool_count, failed_count = 0, 0
  for _, interaction in ipairs(interaction_list or {}) do
    for _, node in ipairs(interaction.node_list or {}) do
      local segment = node.segment
      if segment then
        for _, thought in ipairs(segment.thought or {}) do
          tool_count = tool_count + #(thought.tool or {})
          for _, tool in ipairs(thought.tool or {}) do
            if tool_render.failed(tool) then failed_count = failed_count + 1 end
          end
        end
        if segment.active then
          tool_count = tool_count + (segment.active.tool_count or 0)
          failed_count = failed_count + (segment.active.failed_count or 0)
        end
      end
    end
  end
  return tool_count, failed_count
end

--- Formats a status summary string with elapsed duration and tool invocation counts.
---@param run table Agent run descriptor table.
---@param interaction_list table[]? Array of interaction node tables.
---@param now_ms integer Current epoch timestamp in milliseconds.
---@param capitalize? boolean True to capitalize status label prefix.
---@param include_zero_tools? boolean True to include tool count when zero.
---@return string detail Formatted status description string.
function AgentSummary.status_detail(run, interaction_list, now_ms, capitalize, include_zero_tools)
  local status = run.status or "unknown"
  local active = AgentSummary.is_active(status)
  local end_ms = active and now_ms or (run.updated_at_ms or now_ms)
  local duration_seconds = math.floor(math.max(0, end_ms - (run.created_at_ms or end_ms)) / 1000)
  local status_label = capitalize == false and status or (status:sub(1, 1):upper() .. status:sub(2))
  local tool_count, failed_count = AgentSummary.tool_counts(interaction_list)
  local detail = ("%s %s %ds"):format(
    status_label,
    active and "for" or "in",
    duration_seconds
  )
  if tool_count > 0 or include_zero_tools ~= false then
    detail = detail .. (", %d %s called"):format(tool_count, tool_count == 1 and "tool" or "tools")
    if failed_count > 0 then detail = detail .. (" (%d failed)"):format(failed_count) end
  end
  return detail
end

return AgentSummary
