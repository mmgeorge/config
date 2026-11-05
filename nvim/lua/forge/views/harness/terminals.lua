local M = {}
local client = require("forge.client")

---@class ForgeTerminalObserverOptions
---@field session_id string
---@field alive fun(): boolean
---@field capability fun(): table? Current normalized provider capability snapshot.
---@field update fun(snapshot: table?)
---@field notice fun(message: string)

---Subscribe to terminal inventory; query once on attachment or explicit refresh.
---@param options ForgeTerminalObserverOptions
---@return {refresh: fun(), close: fun()}
function M.watch(options)
  local closed, pending, last_error = false, false, nil
  local revision = 0
  local unsubscribe
  local observer = {}
  function observer.refresh()
    if closed or pending or not options.alive() then return end
    local capability = options.capability and options.capability()
    if type(capability) == "table" and capability.background_terminals == false then
      options.update({ supported = false, terminal = {} })
      return
    end
    -- Startup can open presentation before its first capability snapshot arrives.
    if type(capability) ~= "table" or capability.background_terminals ~= true then return end
    pending = true
    local requested_revision = revision
    client.request({ session_id = options.session_id, method = "session.document", params = { operation = "background_terminals" } }, function(result, failure)
      pending = false
      if closed or not options.alive() or revision ~= requested_revision then return end
      local current_capability = options.capability and options.capability()
      if type(current_capability) == "table" and current_capability.background_terminals == false then
        options.update({ supported = false, terminal = {} })
        return
      end
      if type(current_capability) ~= "table" or current_capability.background_terminals ~= true then return end
      if failure then
        options.update({ unavailable = true })
        if last_error ~= failure then options.notice("Background terminal status: " .. failure) end
        last_error = failure
      else
        last_error = nil
        if result and result.starting then return end
        options.update(result)
      end
    end)
  end
  function observer.close()
    if closed then return end
    closed = true
    unsubscribe()
  end
  unsubscribe = client.session_events_subscribe(function(event, payload, session_id)
    if closed or session_id ~= options.session_id then return end
    if not options.alive() then observer.close() return end
    if event == "provider_event" and payload.kind == "terminals_changed" then
      revision = revision + 1
      last_error = nil
      options.update(payload.data)
    elseif event == "session_configured" or event == "state_invalidated" then
      observer.refresh()
    end
  end)
  observer.refresh()
  return observer
end

return M
