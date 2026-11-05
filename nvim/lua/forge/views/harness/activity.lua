local M = {}

---Read execution activity published by the session owner, never RPC callback state.
---@param state table
---@return boolean
function M.busy(state)
  return not state.host_error and state.activity ~= nil and state.activity.phase == "working"
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
  if incoming.phase == "idle" then require("forge.views.harness.cleanup_progress").settled(state) end
  return true
end

return M
