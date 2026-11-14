-- Deterministic one-shot timers with Neovim's close-before-callback contract.
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local deferred = {}

function deferred.install(delays)
  local pending = {}
  local original
  original = stub(vim, "defer_fn", function(callback, delay)
    if not vim.tbl_contains(delays, delay) then return original(callback, delay) end
    local timer = { callback = callback, delay = delay, closing = false, stopped = false }
    function timer:is_closing() return self.closing end
    function timer:stop() self.stopped = true end
    function timer:close()
      if self.closing then error("timer is already closing") end
      self.closing = true
    end
    pending[#pending + 1] = timer
    return timer
  end)
  return { fire = function(delay)
    local ready = {}
    for _, timer in ipairs(pending) do
      if timer.delay == delay and not timer.closing and not timer.stopped then ready[#ready + 1] = timer end
    end
    for _, timer in ipairs(ready) do timer:close() timer.callback() end
    return #ready
  end }
end

return deferred
