--- Formats epochs and ISO timestamps into relative/absolute date strings for status
--- and PR/review rows, and locates date spans for highlighting.
---@class DiffReviewDatetimeModule
---@field now_override? fun(): integer test seam for the current time
local M = {}

---@param value any
---@return integer? epoch_seconds
---@return table? date_parts
function M.parse(value)
  local text = vim.trim(tostring(value or ""))
  if text == "" then return nil, nil end

  local year, month, day, hour, minute, second, zone = text:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)T(%d%d):(%d%d):?(%d*)%.?%d*([Zz]?)")
  if not year then
    year, month, day, hour, minute, second = text:match("^(%d%d%d%d)%-(%d%d)%-(%d%d) (%d%d):(%d%d):?(%d*)")
  end
  if not year then return nil, nil end

  local parts = {
    year = tonumber(year),
    month = tonumber(month),
    day = tonumber(day),
    hour = tonumber(hour) or 0,
    min = tonumber(minute) or 0,
    sec = tonumber(second ~= "" and second or "0") or 0,
  }
  if not (parts.year and parts.month and parts.day) then return nil, nil end

  local epoch = os.time(parts)
  if zone and zone:lower() == "z" then
    local now = os.time()
    local utc_now_parts = os.date("!*t", now)
    if type(utc_now_parts) == "table" then
      utc_now_parts.isdst = false
      local local_epoch_for_utc_now = os.time(utc_now_parts)
      if local_epoch_for_utc_now then epoch = epoch + os.difftime(now, local_epoch_for_utc_now) end
    end
  end
  return epoch, parts
end

---@return integer
function M.now()
  if type(M.now_override) == "function" then
    local ok, value = pcall(M.now_override)
    if ok and type(value) == "number" then return value end
  end
  return os.time()
end

---@param count integer
---@param unit string
---@return string
function M.ago(count, unit)
  count = math.max(1, count)
  local suffix = count == 1 and unit or (unit .. "s")
  return ("%d %s ago"):format(count, suffix)
end

---@param epoch integer
---@return string
function M.absolute_date(epoch)
  local parts = os.date("*t", epoch)
  if type(parts) ~= "table" then return "" end
  local months = {
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
  }
  return ("%s %d, %d"):format(months[parts.month] or "", parts.day or 1, parts.year or 1970)
end

---@param epoch integer
---@param now integer
---@return integer
function M.calendar_day_delta(epoch, now)
  local then_parts = os.date("*t", epoch)
  local now_parts = os.date("*t", now)
  if type(then_parts) ~= "table" or type(now_parts) ~= "table" then return 0 end
  local then_midnight = os.time({ year = then_parts.year, month = then_parts.month, day = then_parts.day, hour = 0, min = 0, sec = 0 })
  local now_midnight = os.time({ year = now_parts.year, month = now_parts.month, day = now_parts.day, hour = 0, min = 0, sec = 0 })
  if not (then_midnight and now_midnight) then return 0 end
  return math.floor((os.difftime(now_midnight, then_midnight) / 86400) + 0.5)
end

---@param value any
---@param opts? { yesterday?: boolean }
---@return string
function M.relative(value, opts)
  opts = opts or {}
  local text = vim.trim(tostring(value or ""))
  if text == "" then return "" end
  local epoch = M.parse(text)
  if not epoch then return text end

  local now = M.now()
  local seconds = math.floor(os.difftime(now, epoch))
  if seconds < 60 then return "just now" end
  if seconds < 3600 then return M.ago(math.floor(seconds / 60), "minute") end
  if seconds < 86400 then return M.ago(math.floor(seconds / 3600), "hour") end

  local days = M.calendar_day_delta(epoch, now)
  if days == 1 and opts.yesterday ~= false then return "Yesterday" end
  if days >= 1 and days < 7 then return M.ago(days, "day") end
  if days >= 7 and days < 14 then return "Last week" end
  if days >= 14 and days < 30 then return M.ago(days, "day") end
  if days >= 30 and days < 60 then return "Last month" end
  return M.absolute_date(epoch)
end

---@param user string
---@param action string
---@param value any
---@return string
function M.action_phrase(user, action, value)
  local relative = M.relative(value)
  if relative == "" then return ("%s %s"):format(user, action) end
  return ("%s %s %s"):format(user, action, relative)
end

---@param text string
---@return table[]
function M.date_highlight_ranges(text)
  text = tostring(text or "")
  local ranges = {}
  local patterns = {
    "%f[%w]just now%f[%W]",
    "%f[%w]%d+ minutes? ago%f[%W]",
    "%f[%w]%d+ hours? ago%f[%W]",
    "%f[%w]%d+ days? ago%f[%W]",
    "%f[%w]Yesterday%f[%W]",
    "%f[%w]Last week%f[%W]",
    "%f[%w]Last month%f[%W]",
    "%f[%a]%u%l+ %d%d?, %d%d%d%d%f[%W]",
  }
  for _, pattern in ipairs(patterns) do
    local start_index = 1
    while start_index <= #text do
      local match_start, match_end = text:find(pattern, start_index)
      if not match_start then break end
      ranges[#ranges + 1] = {
        start_col = match_start - 1,
        end_col = match_end,
      }
      start_index = match_end + 1
    end
  end
  table.sort(ranges, function(left, right) return left.start_col < right.start_col end)
  return ranges
end

return M
