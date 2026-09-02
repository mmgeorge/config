local M = {}

--- Maps a task status string to a visual status bullet glyph.
---@param status string? Task status string (`"completed"`, `"in_progress"`, `"superseded"`).
---@return string glyph Display status symbol.
local function marker(status)
  if status == "completed" then return "●" end
  if status == "in_progress" or status == "inProgress" then return "◐" end
  if status == "superseded" then return "⊘" end
  return "○"
end

--- Identifies the unique task identifier currently marked in-progress.
---@param snapshot table Task snapshot state table.
---@return string? task_id Active task identifier if exactly one is running, or nil.
local function active_task_id(snapshot)
  local matching = nil
  for _, task in ipairs(snapshot.current or {}) do
    if task.status == "in_progress" or task.status == "inProgress" then
      if matching then return nil end
      matching = task.id
    end
  end
  return matching
end

---@class DiffReviewHarnessTaskTreeHost
---@field append_completed_thought fun(result: table, interaction: table, thought: table, thought_index: integer, options: table)
---@field append_active_thought fun(result: table, interaction: table, active: table, options: table)

--- Renders task checklist entries for an interaction with nested thoughts and active status.
---@param result table Target render collection table.
---@param interaction table Interaction descriptor table.
---@param options table Render options table.
---@param complete boolean True if interaction has concluded.
---@param host DiffReviewHarnessTaskTreeHost Host callbacks for appending thoughts.
---@return boolean active_owned True if an active thought was assigned to a task row.
function M.append(result, interaction, options, complete, host)
  local snapshot = interaction.task
  if type(snapshot) ~= "table" or #(snapshot.current or {}) == 0 then return false end
  local assigned_active = active_task_id(snapshot)
  for task_index, task in ipairs(snapshot.current or {}) do
    local task_key = ("interaction:%s:task:%s"):format(interaction.id or interaction.ordinal, task.id or task_index)
    local line = #result.lines + 1
    result.lines[line] = ("%s %s"):format(marker(task.status), task.title or "")
    result.rows[line] = { kind = "task", interaction = interaction, task = task }
    if complete and task.status == "completed" then result.rows[line].expand_key = task_key end
    if result.expanded[task_key] then
      for thought_index, thought in ipairs(interaction.thought or {}) do
        if thought.task_id == task.id then
          host.append_completed_thought(result, interaction, thought, thought_index, options)
        end
      end
    end
    if interaction.active and assigned_active == task.id then
      host.append_active_thought(result, interaction, interaction.active, options)
    end
  end
  return assigned_active ~= nil
end

--- Renders superseded tasks when an interaction is expanded.
---@param result table Target render collection table.
---@param interaction table Interaction descriptor table.
function M.append_superseded(result, interaction)
  for _, task in ipairs(interaction.task and interaction.task.superseded or {}) do
    result.lines[#result.lines + 1] = ("%s %s"):format(marker("superseded"), task.title or "")
    result.rows[#result.lines] = { kind = "superseded_task", interaction = interaction, task = task }
  end
end

return M
