local M = {}
local attachment = require("forge.views.harness.attachment")
local input_state = require("forge.views.harness.input_state")

---@class ForgePromptContext
---@field document string
---@field buffer integer
---@field is_current fun(): boolean

---Remove the empty lines that open a prompt, which the transcript would draw as a prompt marker
---alone above the text. Attachment offsets index the submitted text, so the returned attachments
---are shifted copies; the originals stay with the draft for restoration. Trailing lines and
---indentation are the user's text and are kept.
---@param text string
---@param attachments table[]
---@return string text
---@return table[] attachments
function M.without_leading_blank_lines(text, attachments)
  local blank_prefix = text:match("^%s*\n")
  if not blank_prefix then return text, attachments end
  local removed = #blank_prefix
  local shifted = {}
  for index, item in ipairs(attachments) do
    shifted[index] = vim.tbl_extend("force", {}, item, {
      start = item.start and item.start - removed,
      ["end"] = item["end"] and item["end"] - removed,
    })
  end
  return text:sub(removed + 1), shifted
end

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
  local submitted_text, submitted_attachments = M.without_leading_blank_lines(text, attachments)
  local restore = function() return false end
  if context and context.is_current() and vim.api.nvim_buf_is_valid(context.buffer)
    and (prompt.changedtick == nil or prompt.changedtick == vim.api.nvim_buf_get_changedtick(context.buffer)) then
    -- An active queue edit shows a queued message's text in the composer, so even an empty edit
    -- belongs to that message rather than to the submitted draft.
    local function composer_owns_draft() return context.is_current() and not state.queue_edit end
    restore = require("forge.views.harness.composer_draft").take(context.buffer, assert(source), attachments,
      composer_owns_draft)
  end
  -- Input state returns a rejected prompt to the composer through `restore`, or keeps it as a
  -- failed row when the composer holds newer work.
  input_state.submit(state, submitted_text, submitted_attachments, "auto", nil, function(failure)
    completed(failure and nil or {}, failure)
  end, restore)
end

return M
