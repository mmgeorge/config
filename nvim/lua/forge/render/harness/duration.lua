local M = {}

local function seconds_label(milliseconds)
  local tenths = math.floor(milliseconds / 100 + 0.5)
  local seconds = math.floor(tenths / 10)
  local fraction = tenths % 10
  if fraction == 0 then return ("%ds"):format(seconds) end
  return ("%d.%ds"):format(seconds, fraction)
end

--- Formats aggregate time with millisecond precision below one second.
---@param milliseconds number Nonnegative observed duration.
---@return string label Duration with its unit.
function M.label(milliseconds)
  if milliseconds < 1000 then return ("%dms"):format(milliseconds) end
  return seconds_label(milliseconds)
end

--- Keeps a five-cell column by switching units when rounded values reach 100.
---@param milliseconds? number Observed duration, or nil when unavailable.
---@return string column Padded duration, using larger units for long-running tools.
function M.tool(milliseconds)
  if milliseconds == nil then return "    —" end
  for _, unit in ipairs({ { "s", 100 }, { "m", 6000 }, { "h", 360000 }, { "d", 8640000 } }) do
    local tenths = math.floor(milliseconds / unit[2] + 0.5)
    if tenths < 1000 then
      local label = tenths % 10 == 0 and ("%d%s"):format(tenths / 10, unit[1])
        or ("%d.%d%s"):format(math.floor(tenths / 10), tenths % 10, unit[1])
      return ("%5s"):format(label)
    end
  end
  local label = ("%.0ed"):format(milliseconds / 86400000):gsub("e%+0?", "e")
  return ("%5s"):format(label)
end

return M
