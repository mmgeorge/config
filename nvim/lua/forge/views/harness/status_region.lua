local buffer = require("forge.buffer")

local M = {}
local namespace = vim.api.nvim_create_namespace("ForgeHarnessStatusRegion")

---@class HarnessStatusMetadata
---@field row integer
---@field animated boolean
---@field hint? string

---@class HarnessStatusLocation
---@field revision? integer
---@field sequence ForgeBlockSequence
---@field block? string
---@field offset? integer
---@field status? HarnessStatusMetadata

---@class HarnessStatusRegion
---@field buffer integer
---@field status string
---@field applying? boolean
---@field revision? integer
---@field sequence ForgeBlockSequence
---@field status_hint_location? HarnessStatusLocation

---Resolve the primary status row in the committed native projection.
---@param transcript HarnessStatusRegion
---@return integer? row
---@return HarnessStatusMetadata? status
function M.location(transcript)
  local cached = transcript.status_hint_location
  if not cached or cached.revision ~= transcript.revision or cached.sequence ~= transcript.sequence then
    cached = { revision = transcript.revision, sequence = transcript.sequence }
    transcript.status_hint_location = cached
    for index = transcript.sequence:count() - 1, 0, -1 do
      local node = transcript.sequence:at(index)
      local status = node.entry.metadata.status
      if status and status ~= vim.NIL then
        cached.block, cached.offset, cached.status = node.id, status.row, status
        break
      end
      if node.id ~= "harness:staging" and not node.id:find(":implementation:", 1, true) then break end
    end
  end
  if not cached.status then return nil end
  local _, source_row = transcript.sequence:position(cached.block)
  return buffer.physical_row(transcript, source_row + cached.offset), cached.status
end

---Release presentation spacing without changing native text or document positions.
---@param target_buffer integer
function M.clear(target_buffer)
  if vim.api.nvim_buf_is_valid(target_buffer) then
    vim.api.nvim_buf_clear_namespace(target_buffer, namespace, 0, -1)
  end
end

---Fill unused viewport rows above status while preserving its complete detail and queue rows.
---@param transcript HarnessStatusRegion
---@param window integer
function M.align(transcript, window)
  local target_buffer = transcript.buffer
  M.clear(target_buffer)
  if transcript.status ~= "Applied" or transcript.applying
    or not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= target_buffer then return end
  local row = M.location(transcript)
  if not row then return end
  local height = math.max(1, vim.api.nvim_win_get_height(window) - (vim.wo[window].winbar ~= "" and 1 or 0))
  local used = vim.api.nvim_win_text_height(window, { start_row = 0, max_height = height + 1 }).all
  local count = math.max(0, height - used)
  if count == 0 then return end
  local lines = {}
  for index = 1, count do lines[index] = { { "", "Normal" } } end
  vim.api.nvim_buf_set_extmark(target_buffer, namespace, math.max(0, row - 1), 0, {
    id = 1, virt_lines = lines, virt_lines_above = true,
  })
end

return M
