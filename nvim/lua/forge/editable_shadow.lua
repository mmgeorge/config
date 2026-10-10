local Sequence = require("forge.block_sequence")
---@class ForgeEditableShadow
---@field sequence ForgeBlockSequence
---@field identity integer
local Shadow = {}
Shadow.__index = Shadow

local function chunks(shadow, text)
  local entries = {}
  for first = 1, #text, 64 do
    local row = {}
    for index = first, math.min(first + 63, #text) do row[#row + 1] = text[index] end
    shadow.identity = shadow.identity + 1
    entries[#entries + 1] = { id = tostring(shadow.identity), entry = { row_count = #row, text = row } }
  end
  return entries
end

--- Retains generated rows in indexed chunks for incremental native callbacks.
---@param text string[]
---@return ForgeEditableShadow
function Shadow.new(text)
  local shadow = setmetatable({ identity = 0 }, Shadow)
  shadow.sequence = Sequence.from(chunks(shadow, text))
  return shadow
end

---@return integer
function Shadow:rows() return self.sequence:rows() end

--- Reads one zero-based row and rejects an index outside the retained text.
---@param index integer
---@return string
function Shadow:row(index)
  local node = assert(self.sequence:locate(index), "shadow row is missing")
  local _, start = self.sequence:position(node.id)
  return node.entry.text[index - start + 1]
end

--- Materializes the complete baseline for explicit restoration.
---@return string[]
function Shadow:text()
  local text = {}
  for index = 0, self.sequence:count() - 1 do
    vim.list_extend(text, self.sequence:at(index).entry.text)
  end
  return text
end

--- Replaces a valid half-open row range without traversing retained suffix rows.
---@param start integer
---@param removed integer
---@param inserted string[]
function Shadow:splice(start, removed, inserted)
  assert(start >= 0 and removed >= 0 and start + removed <= self:rows(), "invalid shadow splice")
  if self:rows() == 0 then
    self.sequence:splice(0, 0, chunks(self, inserted))
    return
  end
  local first = self.sequence:locate(math.min(start, self:rows() - 1))
  local first_index, first_row = self.sequence:position(first.id)
  local last = removed > 0 and self.sequence:locate(start + removed - 1) or first
  local last_index, last_row = self.sequence:position(last.id)
  local text = {}
  for index = 1, start - first_row do text[#text + 1] = first.entry.text[index] end
  vim.list_extend(text, inserted)
  for index = start + removed - last_row + 1, last.entry.row_count do text[#text + 1] = last.entry.text[index] end
  self.sequence:splice(first_index, last_index - first_index + 1, chunks(self, text))
end

return Shadow
