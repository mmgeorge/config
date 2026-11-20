local M = {}
local diagnostic = require("forge.views.harness.diagnostic")

---@class HarnessStatusNotice
---@field text string
---@field animated boolean
---@field failed boolean?
---@field waiting boolean?
---@field hint string?
---@field outlives_host boolean? The notice explains a view whose host is stopped, switching, or
---  serving another provider, so the status rows show it although that view's presentation is no
---  longer live.
---@param state table
---@return HarnessStatusNotice?
function M.notice(state)
  -- A switch stops the host first, so the view says that it is running rather than the stop.
  if state.switching_provider and state.switch_notice then
    return { text = state.switch_notice, animated = true, outlives_host = true }
  end
  -- Without a host, the way back outranks the presentation failure the stop caused.
  local host_notice = diagnostic.get(state, "host_error", "error")
  if host_notice then
    return { text = host_notice.text, animated = host_notice.animated, failed = host_notice.failed,
      waiting = host_notice.waiting, outlives_host = true }
  end
  local notice = state.presentation and diagnostic.get(state.presentation, "failure", "error")
    -- A failed provider switch left this view on its own session; the notification fades, so
    -- the reason stays here until the next switch.
    or diagnostic.get(state, "switch_failure", "error")
    or diagnostic.get(state, "cleanup_notice", "info")
    or diagnostic.get(state, "queue_notice", "info")
    or diagnostic.get(state, "connection_error", "info")
    or diagnostic.get(state, "sync_error", "info")
    or state.presentation and diagnostic.get(state.presentation, "sync_failure", "info")
  if notice then return { text = notice.text, animated = notice.animated, failed = notice.failed, waiting = notice.waiting } end
  -- A confirmed rollback can wait seconds on the provider's native rewind before the host answers.
  local rollback = state.restore_applying
  if rollback then
    return { text = ({ conversation = "Rolling back the conversation",
      workspace = "Rolling back the conversation and files", files = "Restoring files" })[rollback.scope]
      or "Rolling back", animated = true }
  end
  return nil
end

return M
