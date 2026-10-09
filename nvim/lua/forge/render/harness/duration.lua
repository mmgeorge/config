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

return M
