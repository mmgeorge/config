local M = {}
local attachment = require("forge.views.harness.attachment")

---Return whether the composer still holds no work newer than this moment.
---
---Text that arrives asynchronously, such as a rejected draft or a rolled-back prompt, may replace
---the composer only when the user has not written anything since the observation: an untouched
---composer qualifies, and so does one that is empty again, because queueing a follow-up types into
---the composer and then clears it.
---@param buffer integer
---@return fun(): boolean composer_free
function M.observe(buffer)
  local observed_tick = vim.api.nvim_buf_get_changedtick(buffer)
  return function()
    if not vim.api.nvim_buf_is_valid(buffer) then return false end
    if vim.api.nvim_buf_get_changedtick(buffer) == observed_tick then return true end
    local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
    return #lines == 1 and lines[1] == "" and #attachment.capture(buffer).attachments == 0
  end
end

---Clear a submitted draft from the composer at once and return its restoration.
---
---The composer empties when the user submits, not when the host answers, because a task command
---can take as long as starting a provider turn to reply and the transcript already reports work
---meanwhile. A rejected submission calls the returned function to put the draft back.
---Restoration never overwrites later composer work, as `observe` defines it.
---@param buffer integer
---@param source string[] draft lines to restore
---@param attachments table[] draft attachments to restore
---@param is_current fun(): boolean whether the composer still belongs to the submitting view
---@return fun(): boolean restore
function M.take(buffer, source, attachments, is_current)
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "" })
  attachment.restore(buffer, {})
  local composer_free = M.observe(buffer)
  return function()
    if not (is_current() and composer_free()) then return false end
    vim.api.nvim_buf_set_lines(buffer, 0, -1, false, source)
    attachment.restore(buffer, attachments)
    return true
  end
end

---Run a composer action after the input events that preceded its key.
---
---Neovim's terminal UI sends a bracketed paste as a queued `nvim_paste` event, but the keys after
---it through `nvim_input`, which writes the typeahead as soon as the message arrives. A paste and a
---submit key sent in one terminal write therefore reach the mapping before the paste reaches the
---buffer, and the submit sent the draft without the pasted file. The paste event is queued before
---the key is read, so one scheduled step runs every such paste first; the action then sees the
---pasted text and any import it started. A draft changed in any other way during that step, such as
---by keys typed after the submit key, is not sent, so later typing never joins the submitted text.
---@param buffer_of fun(): integer?
---@param action fun()
---@return fun()
function M.after_pending_paste(buffer_of, action)
  return function()
    local buffer = buffer_of()
    if not (buffer and vim.api.nvim_buf_is_valid(buffer)) then action() return end
    local changes = attachment.observe_changes(buffer)
    vim.schedule(function()
      if changes() == "edited" then
        require("forge.infra.notifications").warn("The draft changed while it was being sent. Review it and send it again.",
          "ForgeHarness")
        return
      end
      action()
    end)
  end
end

return M
