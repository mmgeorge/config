local M = {}

--- Render one terminal accepted-plan outcome with nested deviations and audit evidence.
---@param result table
---@param entry table
function M.append(result, entry)
  if #result.lines > 0 then result.lines[#result.lines + 1] = "" end
  local resolution = entry.resolution or {}
  local label = ({
    completed = "Plan Completed",
    blocked = "Plan Blocked",
    cancelled = "Plan Cancelled",
  })[resolution.kind] or "Plan Resolved"
  local key = "plan_resolution:" .. tostring(entry.id or resolution.id)
  result.lines[#result.lines + 1] = "▸ " .. label
  result.rows[#result.lines] = {
    kind = "plan_resolution",
    resolution = resolution,
    expand_key = key,
    node_id = entry.id,
  }
  result.highlights[#result.highlights + 1] = {
    line = #result.lines,
    first = 2,
    last = 2 + #label,
    group = resolution.kind == "completed" and "DiagnosticOk"
      or resolution.kind == "blocked" and "DiagnosticError"
      or "Comment",
  }
  if not result.expanded[key] then return end

  local task = resolution.task_summary or {}
  local test = resolution.test_summary or {}
  result.lines[#result.lines + 1] = ("  Tasks: %d/%d complete, %d blocked"):format(
    task.completed or 0, task.total or 0, task.blocked or 0)
  result.rows[#result.lines] = { kind = "plan_resolution_summary", resolution = resolution }
  result.lines[#result.lines + 1] = ("  Tests: %d passed, %d failed, %d skipped, %d not run"):format(
    test.passed or 0, test.failed or 0, test.skipped or 0, test.not_run or 0)
  result.rows[#result.lines] = { kind = "plan_resolution_summary", resolution = resolution }

  local deviation_list = entry.deviation or {}
  if #deviation_list > 0 then
    result.lines[#result.lines + 1] = "  Deviations"
    result.rows[#result.lines] = { kind = "plan_resolution_deviations", resolution = resolution }
    for _, deviation in ipairs(deviation_list) do
      result.lines[#result.lines + 1] = ("    %s: %s [%s]"):format(
        deviation.kind or "informational",
        deviation.summary or "",
        deviation.disposition or "recorded")
      result.rows[#result.lines] = { kind = "plan_deviation", deviation = deviation }
      if deviation.reason and deviation.reason ~= "" then
        result.lines[#result.lines + 1] = "      " .. deviation.reason
        result.rows[#result.lines] = { kind = "plan_deviation_reason", deviation = deviation }
      end
    end
  end

  local audit = entry.audit
  if audit then
    result.lines[#result.lines + 1] = "  Audit"
    result.rows[#result.lines] = { kind = "plan_audit", audit = audit }
    result.lines[#result.lines + 1] = ("    %d unplanned paths, %d planned paths unchanged"):format(
      #(audit.unplanned_paths or {}), #(audit.unchanged_planned_paths or {}))
    result.rows[#result.lines] = { kind = "plan_audit_summary", audit = audit }
  end
end

return M
