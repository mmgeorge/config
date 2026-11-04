local M = {}
local client = require("forge.client")

---@class ForgeTerminalObserverOptions
---@field session_id string
---@field alive fun(): boolean
---@field capability fun(): table? Current normalized provider capability snapshot.
---@field update fun(snapshot: table?)
---@field notice fun(message: string)

---Observe provider shell state independently of the current exchange timer.
---@param options ForgeTerminalObserverOptions
---@return {refresh: fun(), close: fun()}
function M.watch(options)
  local closed, pending, last_error = false, false, nil
  local timer = assert(vim.uv.new_timer(), "cannot allocate terminal observer timer")
  local observer = {}
  function observer.refresh()
    if closed or pending or not options.alive() then return end
    local capability = options.capability and options.capability()
    if type(capability) == "table" and capability.background_terminals == false then
      options.update({ supported = false, terminal = {} })
      observer.close()
      return
    end
    -- Startup can open presentation before its first capability snapshot arrives.
    if type(capability) ~= "table" or capability.background_terminals ~= true then return end
    pending = true
    client.request({ session_id = options.session_id, method = "session.document", params = { operation = "background_terminals" } }, function(result, failure)
      pending = false
      if closed or not options.alive() then return end
      local current_capability = options.capability and options.capability()
      if type(current_capability) == "table" and current_capability.background_terminals == false then
        options.update({ supported = false, terminal = {} })
        observer.close()
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
        if result and result.supported == false then observer.close() end
      end
    end)
  end
  function observer.close()
    if closed then return end
    closed = true
    timer:stop()
    timer:close()
  end
  timer:start(0, 2000, vim.schedule_wrap(observer.refresh))
  return observer
end

return M
