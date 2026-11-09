local M = {}
local attachment = require("forge.views.harness.attachment")

---Clear a submitted draft from the composer at once and return its restoration.
---
---The composer empties when the user submits, not when the host answers, because a task command
---can take as long as starting a provider turn to reply and the transcript already reports work
---meanwhile. A rejected submission calls the returned function to put the draft back.
---Restoration never overwrites later composer work: an untouched composer qualifies, and so does
---one that is empty again, because queueing a follow-up types into the composer and then clears it.
---@param buffer integer
---@param source string[] draft lines to restore
---@param attachments table[] draft attachments to restore
---@param is_current fun(): boolean whether the composer still belongs to the submitting view
---@return fun(): boolean restore
function M.take(buffer, source, attachments, is_current)
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "" })
  attachment.restore(buffer, {})
  local cleared_tick = vim.api.nvim_buf_get_changedtick(buffer)
  local function composer_free()
    if vim.api.nvim_buf_get_changedtick(buffer) == cleared_tick then return true end
    local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
    return #lines == 1 and lines[1] == "" and #attachment.capture(buffer).attachments == 0
  end
  return function()
    if not (is_current() and vim.api.nvim_buf_is_valid(buffer) and composer_free()) then return false end
    vim.api.nvim_buf_set_lines(buffer, 0, -1, false, source)
    attachment.restore(buffer, attachments)
    return true
  end
end

return M
