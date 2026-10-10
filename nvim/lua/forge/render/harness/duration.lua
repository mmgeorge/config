local M = {}

--- Formats tool time as milliseconds below one second, then rounded tenths of a second.
---@param milliseconds number Nonnegative observed duration.
---@return string label Duration with its unit.
function M.label(milliseconds)
  if milliseconds < 1000 then return ("%dms"):format(milliseconds) end
  local tenths = math.floor(milliseconds / 100 + 0.5)
  local seconds = math.floor(tenths / 10)
  local fraction = tenths % 10
  if fraction == 0 then return ("%ds"):format(seconds) end
  return ("%d.%ds"):format(seconds, fraction)
end

--- Formats one fixed six-cell duration column for a tool heading.
---@param milliseconds? number Observed duration, or nil when unavailable.
---@return string column Padded duration, using larger units for long-running tools.
function M.tool(milliseconds)
  if milliseconds == nil then return "     —" end
  local label = M.label(milliseconds)
  if #label <= 6 then return ("%6s"):format(label) end
  for _, unit in ipairs({ { "m", 60000 }, { "h", 3600000 }, { "d", 86400000 } }) do
    label = ("%.1f%s"):format(milliseconds / unit[2], unit[1])
    if #label <= 6 then return ("%6s"):format(label) end
  end
  label = ("%.0ed"):format(milliseconds / 86400000):gsub("e%+0?", "e")
  return ("%6s"):format(label)
end

return M
