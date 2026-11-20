local M = {}

---Read execution activity published by the session owner, never RPC callback state.
---@param state table
---@return boolean
function M.busy(state)
  return not state.host_error and state.activity ~= nil and state.activity.phase == "working"
end

-- Session phases the host reports only while a provider exchange still owns the turn.
local turn_phase = {
  working = true, finalizing = true, compacting = true, stopping = true,
  awaiting_approval = true, waiting_for_agent = true, retrying_provider = true,
}

---Report whether a provider turn is running, from the session phase rather than owner activity.
---
---Owner activity also reads `working` while a queued command is applied, which never starts a
---turn. A `working` phase whose activity is `handing_off` only predicts the next queued turn.
---@param state table
---@return boolean
function M.turn_running(state)
  local status = state.status
  if state.host_error or type(status) ~= "table" or not turn_phase[status.kind] then return false end
  return not (status.kind == "working" and status.activity == "handing_off")
end

---Get the owner's fatal execution failure for status and prompt admission.
---@param state table
---@return string?
function M.unavailable_reason(state)
  if not state.activity or state.activity.phase ~= "unavailable" then return nil end
  return state.activity.unavailable_reason or "Session execution is unavailable; reopen the session to retry"
end

---Adopt one ordered owner projection within its runtime epoch.
---@param state table
---@param incoming table
---@param epoch string
---@return boolean
function M.receive(state, incoming, epoch)
  if epoch ~= state.runtime_epoch or type(incoming) ~= "table" then return false end
  local previous = state.activity
  if previous and incoming.revision < previous.revision then return false end
  state.activity = incoming
  if (previous and previous.phase) ~= incoming.phase then
    require("forge.views.harness.input_state").activity_changed(state)
  end
  local cleanup_progress = require("forge.views.harness.cleanup_progress")
  if incoming.phase == "idle" and not cleanup_progress.active(state) then cleanup_progress.settled(state) end
  return true
end

return M
