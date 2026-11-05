local M = {}
local diagnostic = require("forge.views.harness.diagnostic")

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
end

---Render progress published by the host cleanup owner.
---@param state table
---@param progress ForgeHarnessCleanupProgress
function M.receive(state, progress)
  M.begin(state, progress.operation_id)
  diagnostic.set(state, "cleanup_notice", progress.message, {
    owner = progress.operation_id, category = "cleanup",
    severity = progress.state == "timed_out" and "warning" or "info",
    recovery = "wait", clear_when = "matching host settlement",
  })
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
  state.cleanup_operation_id, state.cleanup_epoch = nil, nil
end

return M
