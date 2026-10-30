local M = {}
local attachment = require("forge.views.harness.attachment")
local input_state = require("forge.views.harness.input_state")

---@class ForgePromptContext
---@field document string
---@field buffer integer
---@field is_current fun(): boolean

---@param state table
---@param prompt {text: string, attachments?: table[], context?: ForgePromptContext, source?: string[], changedtick?: integer, queue_entry?: table}
---@param completed fun(result?: table, failure?: string)
function M.start(state, prompt, completed)
  local context = prompt.context
  if context then require("forge.text").invalidate(context.buffer) end
  local source = context and (prompt.source or vim.api.nvim_buf_get_lines(context.buffer, 0, -1, false)) or nil
  local text = source and table.concat(source, "\n") or prompt.text
  if not text:find("%S") then completed(nil, "Prompt requires nonempty text") return end
  local attachments = prompt.attachments or (context and attachment.capture(context.buffer).attachments) or {}
  local cleared_tick
  if context and context.is_current() and vim.api.nvim_buf_is_valid(context.buffer)
    and (prompt.changedtick == nil or prompt.changedtick == vim.api.nvim_buf_get_changedtick(context.buffer)) then
    vim.api.nvim_buf_set_lines(context.buffer, 0, -1, false, { "" })
    attachment.restore(context.buffer, {})
    cleared_tick = vim.api.nvim_buf_get_changedtick(context.buffer)
  end
  local function restore()
    if context and source and cleared_tick and context.is_current() and vim.api.nvim_buf_is_valid(context.buffer)
      and vim.api.nvim_buf_get_changedtick(context.buffer) == cleared_tick then
      vim.api.nvim_buf_set_lines(context.buffer, 0, -1, false, source)
      attachment.restore(context.buffer, attachments)
      return true
    end
    return false
  end
  input_state.submit(state, text, attachments, "auto", nil, function(failure)
    if failure then restore() end
    completed(failure and nil or {}, failure)
  end, restore)
end

return M
