local M = {}

M.spinner_frame_list = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }

---@param now_ms integer
---@return string
function M.frame_at(now_ms)
  local index = math.floor((now_ms or 0) / 120) % #M.spinner_frame_list + 1
  return M.spinner_frame_list[index]
end

---@param result table
---@param status table?
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
    group = "DiffReviewTimelineStatus",
  }
  result.timeline_status_line = line
end

return M
