local M = {}
local client = require("forge.client")
local notifications = require("forge.infra.notifications")

---@class ForgeHarnessRecap
---@field loading boolean
---@field text string?
---@field render fun()

---Invalidate both displayed text and in-flight replies without changing durable exchanges.
---@param state table
function M.clear(state)
  local previous = state.recap
  state.recap = nil
  if previous and previous.render then previous.render() end
end

---Generate a transient recap for the captured session, independently of prompt execution.
---@param state table
---@param render fun()
function M.request(state, render)
  if state.recap and state.recap.loading then return end
  local session_id, generation = state.session.id, client.host_generation()
  local request = { loading = true, render = render }
  state.recap = request
  render()
  client.request_for(session_id, "harness.document", {
    operation = "recap", model = state.session.model or "default",
  }, function(result, failure)
    if state.recap ~= request or not state.session or state.session.id ~= session_id
      or client.host_generation() ~= generation then return end
    if failure then
      state.recap = nil
      notifications.error(failure, "Harness recap")
    elseif not result or type(result.text) ~= "string" or vim.trim(result.text) == "" then
      state.recap = nil
      notifications.error("Recap completed without text", "Harness recap")
    else
      request.loading, request.text = false, result.text
    end
    render()
  end)
end

return M
