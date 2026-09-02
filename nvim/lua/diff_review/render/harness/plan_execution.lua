local M = {}

---@class DiffReviewHarnessPlanExecutionHost
---@field append_interaction fun(result: table, interaction: table, options: table)

--- Appends execution stream events including task starts, task completions, and plan deviations.
---@param result table Target render collection table.
---@param entry table Plan execution stream record.
---@param options table Rendering options configuration.
---@param host DiffReviewHarnessPlanExecutionHost Host adapter providing interaction formatting.
function M.append(result, entry, options, host)
  if #result.lines > 0 then result.lines[#result.lines + 1] = "" end
  for _, item in ipairs(entry.item or {}) do
    if item.kind == "interaction" and item.interaction then
      host.append_interaction(result, item.interaction, options)
    elseif item.kind == "task_started" then
      local label = ("Task %d/%d started: %s"):format(
        item.ordinal or 0,
        item.total or 0,
        item.title or ""
      )
      result.lines[#result.lines + 1] = "▸ " .. label
      result.rows[#result.lines] = {
        kind = "plan_task_started",
        task_path = item.task_path,
        node_id = ("%s:%s:started"):format(entry.id or "execution", item.task_path or item.ordinal),
      }
      result.highlights[#result.highlights + 1] = {
        line = #result.lines,
        first = 2,
        last = 2 + #label,
        group = "DiffReviewHarnessThought",
      }
    elseif item.kind == "task_completed" then
      local elapsed_seconds = math.floor(math.max(0, item.elapsed_ms or 0) / 1000)
      local label = ("Task %d/%d completed in %ds"):format(
        item.ordinal or 0,
        item.total or 0,
        elapsed_seconds
      )
      result.lines[#result.lines + 1] = "✓ " .. label
      result.rows[#result.lines] = {
        kind = "plan_task_completed",
        task_path = item.task_path,
        node_id = ("%s:%s:completed"):format(entry.id or "execution", item.task_path or item.ordinal),
      }
      result.highlights[#result.highlights + 1] = {
        line = #result.lines,
        first = 0,
        last = -1,
        group = "DiagnosticOk",
      }
    elseif item.kind == "deviation_recorded" then
      local label = "Plan deviation recorded: " .. (item.summary or "")
      result.lines[#result.lines + 1] = "! " .. label
      result.rows[#result.lines] = {
        kind = "plan_deviation_recorded",
        deviation_id = item.deviation_id,
        node_id = ("%s:deviation:%s"):format(entry.id or "execution", item.deviation_id or #result.lines),
      }
      result.highlights[#result.highlights + 1] = {
        line = #result.lines,
        first = 0,
        last = -1,
        group = "DiagnosticWarn",
      }
    end
  end
end

return M
