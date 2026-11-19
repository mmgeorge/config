local M = {}
local diagnostic = require("forge.views.harness.diagnostic")

---Get whether the current host still owes this view a cancellation response.
---@param state table
---@return boolean
function M.pending(state)
  return state.cleanup_request_id ~= nil and state.cleanup_request_epoch == state.runtime_epoch
end

---Get whether the authoritative operation still owns cleanup or recovery.
---@param state table
---@return boolean
function M.active(state)
  return state.cancellation ~= nil and state.cancellation.phase ~= "settled"
    and state.cancellation.phase ~= "failed"
end

---@class ForgeHarnessCleanupProgress
---@field operation_id string
---@field policy "cancellation"|"close"
---@field state "pending"|"timed_out"
---@field message string

---Track the caller identity without predicting host execution state.
---@param state table
---@param operation_id string
function M.begin(state, operation_id)
  state.cleanup_operation_id, state.cleanup_epoch = operation_id, state.runtime_epoch
  state.cleanup_request_id, state.cleanup_request_epoch = operation_id, state.runtime_epoch
end

---Retire only the callback associated with the current host request.
---@param state table
---@param operation_id string
---@param epoch string?
---@return boolean
function M.respond(state, operation_id, epoch)
  if state.runtime_epoch ~= epoch or state.cleanup_request_id ~= operation_id
    or state.cleanup_request_epoch ~= epoch then return false end
  state.cleanup_request_id, state.cleanup_request_epoch = nil, nil
  return true
end

-- Name the boundary a slow cancellation still waits on.
local function phase_notice(state, progress)
  local message = ({ acquiring = "Cancellation pending - acquiring ownership",
    collecting = "Cancellation pending - collecting native work",
    draining = "Cancellation pending - draining host work",
    settling = "Cancellation pending - committing settlement",
    recovering = "Cancellation pending - recovering provider" })[progress.phase]
  if progress.response_timed_out then message = message .. " (response timed out)" end
  diagnostic.set(state, "cleanup_notice", message, { owner = progress.operation_id,
    category = "cleanup", severity = progress.response_timed_out and "warning" or "info",
    recovery = "wait", clear_when = "matching host settlement" })
end

---Render progress published by the host cleanup owner.
---@param state table
---@param progress ForgeHarnessCleanupProgress
function M.receive(state, progress)
  if M.active(state) then
    -- The committed operation knows which boundary is open; the owner's report says only that
    -- the stop is slow, which is what makes that boundary worth a notice.
    if state.cancellation.operation_id == progress.operation_id then
      state.cleanup_reported_id = progress.operation_id
      phase_notice(state, state.cancellation)
    end
    return
  end
  if state.cleanup_epoch == state.runtime_epoch and state.cleanup_operation_id
    and state.cleanup_operation_id ~= progress.operation_id then return end
  state.cleanup_operation_id, state.cleanup_epoch = progress.operation_id, state.runtime_epoch
  state.cleanup_reported_id = progress.operation_id
  diagnostic.set(state, "cleanup_notice", progress.message, {
    owner = progress.operation_id, category = "cleanup",
    severity = progress.state == "timed_out" and "warning" or "info",
    recovery = "wait", clear_when = "matching host settlement",
  })
end

---Project committed operation revisions from incremental and reconnect snapshots.
---@param state table
---@param progress table?
function M.apply(state, progress)
  if not progress then return end
  local previous = state.cancellation
  if previous and progress.revision <= previous.revision then return end
  state.cancellation = progress
  if progress.phase == "settled" or progress.phase == "failed" then
    M.settled(state, progress.operation_id)
    return
  end
  state.cleanup_operation_id, state.cleanup_epoch = progress.operation_id, state.runtime_epoch
  -- The status row reads `Stopping` for the whole operation. A notice for every phase replaced
  -- its text several times within one second, so only a stop already known to be slow gets one.
  if progress.response_timed_out or state.cleanup_reported_id == progress.operation_id then
    phase_notice(state, progress)
  end
end

---Retain the matching timeout notice while the host still owns cleanup.
---@param state table
---@param operation_id string
function M.timeout(state, operation_id)
  if state.cleanup_operation_id ~= operation_id or state.cleanup_epoch ~= state.runtime_epoch then return end
  M.receive(state, { operation_id = operation_id, policy = "cancellation", state = "timed_out",
    message = "Exceeded cancellation timeout" })
end

---Clear a notice only after its matching result or authoritative idle observation.
---@param state table
---@param operation_id? string
function M.settled(state, operation_id)
  if operation_id and state.cleanup_operation_id ~= operation_id then return end
  diagnostic.set(state, "cleanup_notice", nil)
  state.cleanup_operation_id, state.cleanup_epoch, state.cleanup_reported_id = nil, nil, nil
  if not operation_id then state.cleanup_request_id, state.cleanup_request_epoch = nil, nil end
end

return M
