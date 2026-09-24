local M = {}
local client = require("forge.client")

---Observe provider shell state independently of the current exchange timer.
---@param options {session_id: string, alive: fun(): boolean, update: fun(snapshot: table), notice: fun(message: string)}
---@return {refresh: fun(), close: fun()}
function M.watch(options)
  local closed, pending, last_error = false, false, nil
  local timer = vim.uv.new_timer()
  local observer = {}
  function observer.refresh()
    if closed or pending or not options.alive() then return end
    pending = true
    client.request_for(options.session_id, "harness.document", { operation = "background_terminals" }, function(result, failure)
      pending = false
      if closed or not options.alive() then return end
      if failure then
        options.update({ unavailable = true })
        if last_error ~= failure then options.notice("Background terminal status: " .. failure) end
        last_error = failure
      else
        last_error = nil
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
