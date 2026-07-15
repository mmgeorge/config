local M = {}

---@param current_index integer
---@param count integer
---@param delta integer
---@return integer
function M.cycle(current_index, count, delta)
  if count == 0 then return current_index end
  return ((current_index - 1 + delta) % count) + 1
end

---@param current_index integer
---@param count integer
---@param delta integer
---@return integer
function M.clamp(current_index, count, delta)
  if count == 0 then return current_index end
  return math.max(1, math.min(count, current_index + delta))
end

return M
