-- Fixture access to Harness execution activity. The session owner publishes activity, and
-- `views/harness/activity.lua` reads it; Lua-side busy flags no longer exist. Unit fixtures that
-- simulate running work therefore publish an owner projection instead of assigning a field.
local activity = require("forge.views.harness.activity")
local M = {}

---@param state table
---@return boolean
function M.busy(state)
  return activity.busy(state)
end

---Publish a working or idle projection newer than the state's current activity.
---@param state table
---@param busy boolean
function M.set_busy(state, busy)
  local revision = (state.activity and state.activity.revision or 0) + 1
  state.activity = { phase = busy and "working" or "idle", revision = revision }
end

return M
