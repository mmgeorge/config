local M = {}
local diagnostic = require("forge.views.harness.diagnostic")

---@class HarnessStatusNotice
---@field text string
---@field animated boolean
---@field failed boolean?
---@field waiting boolean?
---@field hint string?
---@param state table
---@return HarnessStatusNotice?
function M.notice(state)
  local notice = state.presentation and diagnostic.get(state.presentation, "failure", "error")
    or diagnostic.get(state, "host_error", "error")
    or diagnostic.get(state, "cleanup_notice", "info")
    or diagnostic.get(state, "queue_notice", "info")
    or diagnostic.get(state, "connection_error", "info")
    or diagnostic.get(state, "sync_error", "info")
    or state.presentation and diagnostic.get(state.presentation, "sync_failure", "info")
  if notice then return { text = notice.text, animated = notice.animated, failed = notice.failed, waiting = notice.waiting } end
  return nil
end

return M
