local module = {}
local blocks = require("markdown_math.blocks")

---@class MarkdownMathBufferEntry
---@field changedtick integer
---@field blocks MarkdownMathBlock[]

---@type table<integer, MarkdownMathBufferEntry>
local buffer_store = {}

vim.api.nvim_create_autocmd("BufWipeout", {
  group = vim.api.nvim_create_augroup("MarkdownMathBlockStore", { clear = true }),
  callback = function(event) buffer_store[event.buf] = nil end,
})

---Return borrowed blocks for a valid buffer, refreshing after source edits.
---@param buffer integer
---@return MarkdownMathBlock[]
function module.get(buffer)
  if buffer == 0 then buffer = vim.api.nvim_get_current_buf() end
  local changedtick = vim.api.nvim_buf_get_changedtick(buffer)
  local entry = buffer_store[buffer]
  if not entry or entry.changedtick ~= changedtick then
    entry = {
      changedtick = changedtick,
      blocks = blocks.find(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)),
    }
    buffer_store[buffer] = entry
  end
  return entry.blocks
end

return module
