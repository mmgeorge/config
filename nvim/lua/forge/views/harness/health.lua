local M = {}
local client = require("forge.client")

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
  local function tick()
    if state.health_owner ~= identity or client.host_generation() ~= generation or state.host_error
      or not state.session or state.session.id ~= session_id then return end
    if vim.uv.now() - last_response >= 10000 then
      state.connection_error = "Connection unresponsive — task status unknown"
      refresh()
    end
    if state.busy and state.last_provider_progress and vim.uv.now() - state.last_provider_progress >= 30000 then
      state.wait_notice = ("Waiting for provider or tool update (%ds)"):format(math.floor((vim.uv.now() - state.last_provider_progress) / 1000))
      refresh()
    end
    if not pending then
      pending = true
      client.request_for(session_id, "health.get", {}, function(_, failure)
        if state.health_owner ~= identity or client.host_generation() ~= generation then return end
        pending = false
        if not failure then
          last_response = vim.uv.now()
          if state.connection_error then state.connection_error = nil refresh() end
        end
      end)
    end
    vim.defer_fn(tick, 2000)
  end
  vim.defer_fn(tick, 2000)
end

return M
