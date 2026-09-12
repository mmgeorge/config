local M = {}

M.MAX_BYTES = 16 * 1024 * 1024
local MAX_PART_BYTES = 256 * 1024
local MAX_PARTS = 256

---@class ForgeJsonTransfer
---@field sequence integer
---@field bytes integer
---@field part_count? integer
---@field total_bytes? integer
---@field parts? string[]
---@field finished? boolean

local function counter(value, minimum, maximum)
  return type(value) == "number" and value >= minimum and value <= maximum and value == math.floor(value)
end

---@param part unknown
---@return boolean
function M.valid(part)
  return type(part) == "table" and counter(part.part_count, 1, MAX_PARTS)
    and counter(part.sequence, 0, part.part_count - 1) and counter(part.total_bytes, 1, M.MAX_BYTES)
    and type(part.payload) == "string" and #part.payload > 0 and #part.payload <= MAX_PART_BYTES
end

---@return ForgeJsonTransfer
function M.new()
  return { sequence = 0, bytes = 0, parts = {} }
end

---@param state ForgeJsonTransfer
---@param part table
---@return string? failure
function M.accept(state, part)
  if state.finished or not M.valid(part) or part.sequence ~= state.sequence then return "invalid JSON transfer part or sequence" end
  if state.sequence == 0 then state.part_count, state.total_bytes = part.part_count, part.total_bytes end
  if part.part_count ~= state.part_count or part.total_bytes ~= state.total_bytes
    or state.bytes + #part.payload > state.total_bytes then return "JSON transfer totals differ" end
  state.parts[#state.parts + 1] = part.payload
  state.bytes = state.bytes + #part.payload
  state.sequence = state.sequence + 1
  return nil
end

---@param state ForgeJsonTransfer
---@return string? encoded
---@return string? failure
function M.finish(state)
  if state.finished or not state.parts or state.sequence ~= state.part_count or state.bytes ~= state.total_bytes then
    return nil, "JSON transfer is incomplete"
  end
  local encoded = table.concat(state.parts)
  state.parts = nil
  state.finished = true
  return encoded, nil
end

return M
