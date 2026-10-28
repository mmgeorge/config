local M = {}
local spinner = require("forge.render.harness.timeline_status")

---@param state {model?: string}
---@param now_ms? integer
---@return string
function M.text(state, now_ms)
  local model = (state.model or "resolving model"):match("[^\r\n]*")
  return spinner.frame_at(now_ms or vim.uv.now()) .. " Generating with " .. model
end

---@param state {state: string, model?: string}
---@param current fun(): boolean
---@param update fun(text: string)
---@return fun()
function M.watch(state, current, update)
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
    if state.state ~= "generating" or not current() then stop() return end
    update(M.text(state))
  end
  timer:start(120, 120, vim.schedule_wrap(tick))
  tick()
  return stop
end

return M
