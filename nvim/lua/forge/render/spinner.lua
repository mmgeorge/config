local M = {}

M.frame_list = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
--- Target duration of one animation frame in milliseconds (60 frames per second).
M.frame_interval_ms = 1000 / 60

---@param now_ms integer
---@return string
function M.frame_at(now_ms)
  return M.frame_list[math.floor(now_ms / M.frame_interval_ms) % #M.frame_list + 1]
end

---@param current fun(): boolean
---@param update fun(frame: string)
---@return fun()
function M.watch(current, update)
  local timer = assert(vim.uv.new_timer())
  local stopped = false
  local function stop()
    if stopped then return end
    stopped = true
    timer:stop()
    timer:close()
  end
  local function tick()
    if stopped then return end
    if not current() then stop() return end
    update(M.frame_at(vim.uv.now()))
  end
  local interval = math.ceil(M.frame_interval_ms)
  timer:start(interval, interval, vim.schedule_wrap(tick))
  tick()
  return stop
end

return M
