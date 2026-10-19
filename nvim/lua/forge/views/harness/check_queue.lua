local client = require("forge.client")
local diagnostic = require("forge.views.harness.diagnostic")
local M = {}

---@class ForgeCheckQueuePublication
---@field identity string
---@field revision integer
---@field pending boolean?
---@field waiting boolean

---Publish queued work before the next execution can observe its completion boundary.
---@param state ForgeHarnessPresentationState
---@param changed fun()
---@return boolean waiting
function M.publish(state, changed)
  if not state.ready or not state.session or not state.runtime_epoch then return false end
  local session_id, epoch = state.session.id, state.runtime_epoch
  local identity = session_id .. ":" .. epoch
  local first = (state.queue or {})[1]
  local pending = first ~= nil and (type(first) ~= "table" or first.task_id == state.session.current_task_id)
  local publication = state.check_queue
  if not publication or publication.identity ~= identity then
    publication = { identity = identity, revision = 0, waiting = false }
    state.check_queue = publication
  end
  if publication.pending == pending then return publication.waiting end
  publication.pending = pending
  publication.revision = publication.revision + 1
  publication.waiting = true
  local revision = publication.revision
  client.request_for(session_id, "queue.update", { runtime_epoch = epoch, revision = revision, pending = pending }, function(_, failure)
    if not state.ready or state.check_queue ~= publication or publication.revision ~= revision
      or not state.session or state.session.id ~= session_id or state.runtime_epoch ~= epoch then return end
    publication.waiting = false
    if failure then
      publication.pending = nil
      diagnostic.set(state, "queue_notice", "Retrying queue synchronization: " .. failure,
        { owner = identity, category = "queue", severity = "info" })
      vim.defer_fn(function()
        if state.check_queue == publication and publication.revision == revision then changed() end
      end, 1000)
      return
    end
    diagnostic.set(state, "queue_notice", nil)
    changed()
  end)
  return true
end

return M
