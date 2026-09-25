local module = {}
local blocks = require("markdown_math.blocks")

---@class MarkdownMathBufferEntry
---@field changedtick integer
---@field regions table<string, MarkdownMathBlock[]>

---@type table<integer, MarkdownMathBufferEntry>
local buffer_store = {}

vim.api.nvim_create_autocmd("BufWipeout", {
  group = vim.api.nvim_create_augroup("MarkdownMathBlockStore", { clear = true }),
  callback = function(event) buffer_store[event.buf] = nil end,
})

---Return borrowed blocks for a valid buffer, refreshing after source edits.
---@param buffer integer
---@param first0 integer?
---@param after0 integer?
---@return MarkdownMathBlock[]
function module.get(buffer, first0, after0)
  if buffer == 0 then buffer = vim.api.nvim_get_current_buf() end
  local changedtick = vim.api.nvim_buf_get_changedtick(buffer)
  local entry = buffer_store[buffer]
  if not entry or entry.changedtick ~= changedtick then
    entry = {
      changedtick = changedtick,
      regions = {},
    }
    buffer_store[buffer] = entry
  end
  first0 = first0 or 0
  after0 = after0 or vim.api.nvim_buf_line_count(buffer)
  local key = first0 .. ":" .. after0
  if not entry.regions[key] then
    local region = blocks.find(vim.api.nvim_buf_get_lines(buffer, first0, after0, false))
    for _, block in ipairs(region) do
      block.start_row = block.start_row + first0
      block.end_row = block.end_row + first0
    end
    entry.regions[key] = region
  end
  return entry.regions[key]
end

return module
