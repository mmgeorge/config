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
  local restore = function() return false end
  if context and context.is_current() and vim.api.nvim_buf_is_valid(context.buffer)
    and (prompt.changedtick == nil or prompt.changedtick == vim.api.nvim_buf_get_changedtick(context.buffer)) then
    -- An active queue edit shows a queued message's text in the composer, so even an empty edit
    -- belongs to that message rather than to the submitted draft.
    local function composer_owns_draft() return context.is_current() and not state.queue_edit end
    restore = require("forge.views.harness.composer_draft").take(context.buffer, assert(source), attachments,
      composer_owns_draft)
  end
  input_state.submit(state, text, attachments, "auto", nil, function(failure)
    if failure then restore() end
    completed(failure and nil or {}, failure)
  end, restore)
end

return M
