local M = {}

---@param token_count number
---@return string
local function format_token_count(token_count)
  if token_count >= 1000000 then
    return ("%.1fM"):format(token_count / 1000000):gsub("%.0M$", "M")
  end
  if token_count >= 1000 then
    return ("%.0fK"):format(token_count / 1000)
  end
  return tostring(token_count)
end

---@param context_usage? table
---@return { text: string, group: string }[]?
function M.segment(context_usage)
  if type(context_usage) ~= "table" then return nil end
  local remaining_percent = tonumber(context_usage.remaining_percent)
  local size = tonumber(context_usage.size)
  if not remaining_percent or not size or size <= 0 then return nil end
  return {
    {
      text = ("%d%% context left (%s)"):format(remaining_percent, format_token_count(size)),
      group = "DiffReviewStatusHint",
    },
  }
end

return M
