vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
-- A spinner tick scheduled before the status stops animating must not replace the waiting icon.
local StatusView = require("forge.views.harness.status_view")
local transcript_window = vim.api.nvim_get_current_win()
local view = StatusView.new({ transcript = { document = "status-animation", buffer = vim.api.nvim_get_current_buf() },
  transcript_window = transcript_window, alive = function() return true end,
  notice = function(message) error(message) end, recover = function() end })
vim.bo[view.buffer].modifiable = true
vim.api.nvim_buf_set_lines(view.buffer, 0, -1, false, { "  Working" })
vim.bo[view.buffer].modifiable = false
view.window = vim.api.nvim_open_win(view.buffer, false, { split = "above", win = transcript_window, height = 1 })

---@type {tick?: fun(), stopped: boolean}[]
local timers = {}
local original_new_timer = vim.uv.new_timer
vim.uv.new_timer = function()
  local record = { stopped = false }
  timers[#timers + 1] = record
  return {
    start = function(_, _, _, tick) record.tick = tick end,
    stop = function() record.stopped = true end,
    close = function() end,
  }
end
local ok, failure = xpcall(function()
  local namespace = view.animation_namespace
  local function icon()
    local marks = vim.api.nvim_buf_get_extmarks(view.buffer, namespace, 0, -1, { details = true })
    assert(#marks == 1, "status row has " .. #marks .. " animation marks")
    return marks[1][4].virt_text[1][1]
  end
  view.content = { lines = { "  Working" }, animated = 0 }
  view:update_animation()
  assert(icon() ~= "◷", "animated status did not draw a spinner frame")
  -- The libuv callback queues the draw; the state change lands before the queue drains.
  timers[1].tick()
  view.content = { lines = { "  Waiting for plan review" }, waiting = 0 }
  view:update_animation()
  assert(timers[1].stopped, "waiting status left the spinner timer running")
  vim.wait(50, function() return false end)
  assert(icon() == "◷", "a stale spinner tick replaced the waiting icon with " .. icon())
end, debug.traceback)
vim.uv.new_timer = original_new_timer
view.timer = nil
view:close()
if not ok then error(failure, 0) end
print("harness_status_animation: passed")
