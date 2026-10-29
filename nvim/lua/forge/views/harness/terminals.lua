local M = {}
local client = require("forge.client")

---@class ForgeTerminalObserverOptions
---@field session_id string
---@field alive fun(): boolean
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
    pending = true
    client.request({ session_id = options.session_id, method = "session.document", params = { operation = "background_terminals" } }, function(result, failure)
      pending = false
      if closed or not options.alive() then return end
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
