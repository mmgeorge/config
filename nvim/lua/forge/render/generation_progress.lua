local M = {}
local spinner = require("forge.render.spinner")

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
  return spinner.watch(function() return state.state == "generating" and current() end, function()
    update(M.text(state))
  end)
end

return M
