local M = {}
local client = require("forge.client")

---@class HarnessStatusNotice
---@field text string
---@field animated boolean
---@field failed boolean?
---@field waiting boolean?
---@field hint string?
---@param state table
---@return HarnessStatusNotice?
function M.notice(state)
  local failure = state.presentation and state.presentation.failure or state.execution_notice
    or state.host_error or state.connection_error or state.sync_error
    or state.presentation and state.presentation.section_error
  if failure then return { text = failure, animated = false, failed = failure ~= "Paused" } end
  if #(state.approval or {}) > 0 then
    return { text = "Waiting for your approval", animated = false, waiting = true, hint = "permission" }
  end
  return nil
end

---@param state table
---@param refresh fun()
function M.watch(state, refresh)
  if not state.session then return end
  local identity = {}
  local generation = client.host_generation()
  local session_id = state.session.id
  state.health_owner = identity
  local last_response = vim.uv.now()
  local pending = false
  local connection_notice = "Connection unresponsive — task status unknown"
  local function current()
    return state.health_owner == identity and client.host_generation() == generation
      and not state.host_error and state.session and state.session.id == session_id
  end
  local function tick()
    if not current() then return end
    if vim.uv.now() - last_response >= 10000 then
      if not state.connection_error then state.connection_error = connection_notice refresh() end
    end
    if not pending then
      pending = true
      client.request_for(session_id, "health.get", {}, function(_, failure)
        if not current() then return end
        pending = false
        if not failure then
          last_response = vim.uv.now()
          if state.connection_error == connection_notice then state.connection_error = nil refresh() end
        end
      end)
    end
    vim.defer_fn(tick, 2000)
  end
  vim.defer_fn(tick, 2000)
end

return M
