local M = {}

local spinner = require("forge.render.spinner")
M.spinner_frame_list = spinner.frame_list
M.frame_at = spinner.frame_at

--- Appends an animated status indicator row to the timeline render target.
---@param result table Target render collection table.
---@param status table? Timeline status descriptor table.
function M.append(result, status)
  if not status then return end
  if #result.lines == 0 or result.lines[#result.lines] ~= "" then result.lines[#result.lines + 1] = "" end
  local line = #result.lines + 1
  result.lines[line] = "  " .. status.text
  result.rows[line] = { kind = "timeline_status", node_id = "timeline-status:" .. status.id }
  result.highlights[#result.highlights + 1] = {
    line = line,
    first = 2,
    last = -1,
    group = "ForgeTimelineStatus",
  }
  result.timeline_status_line = line
end

return M
