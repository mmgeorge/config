-- An animated status requests one presentation sync per second of synthetic time.
--
-- Rust renders elapsed labels and refreshes them only inside a sync, so a silent tool
-- would freeze every label unless the status timer requests syncs. The fixture drives
-- the spinner timer and clock directly, so no real time passes.
vim.loader.enable(false)
local original_now, original_new_timer = vim.uv.now, vim.uv.new_timer
local now_ms = 0
---@type fun()?
local tick
local success, failure = xpcall(function()
  vim.uv.now = function() return now_ms end
  vim.uv.new_timer = function()
    local timer = original_new_timer()
    return setmetatable({
      start = function(_, _, _, callback) tick = callback end,
    }, { __index = function(_, key)
      return function(_, ...) return timer[key](timer, ...) end
    end })
  end
  local refreshes = 0
  local view = require("forge.views.harness.status_view").new({
    transcript = {}, transcript_window = vim.api.nvim_get_current_win(),
    alive = function() return true end, notice = function() end, recover = function() end,
    refresh = function() refreshes = refreshes + 1 end,
  })
  view.window, view.content = vim.api.nvim_get_current_win(), { animated = 0 }
  view:update_animation()
  assert(tick, "an animated status did not start its timer")
  ---@param expected integer
  ---@param message string
  local function fire(expected, message)
    tick()
    assert(vim.wait(1000, function() return refreshes == expected end, 1), message .. ": " .. refreshes)
  end
  now_ms = 999
  fire(0, "the status requested a sync before one second elapsed")
  now_ms = 1000
  fire(1, "the status did not request a sync after one second")
  now_ms = 1500
  fire(1, "the status requested a second sync within the same second")
  now_ms = 2000
  fire(2, "the status stopped requesting syncs while still animated")
  view.content = { animated = nil }
  view:update_animation()
  view:close()
end, debug.traceback)
vim.uv.now, vim.uv.new_timer = original_now, original_new_timer
assert(success, failure)
print("harness_status_refresh: passed")
