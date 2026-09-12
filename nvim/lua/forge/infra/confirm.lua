local M = {}
local popup_window = require("forge.infra.popup_window")

--- Open the centered yes/no dialog. Leaving the dialog cancels exactly once.
---@param lines string[]
---@param on_yes fun()
---@param on_no? fun()
---@param options? {title?: string, min_width?: integer}
---@return integer buffer
function M.open(lines, on_yes, on_no, options)
  options = options or {}
  local body = vim.list_extend({}, lines)
  body[#body + 1] = ""
  body[#body + 1] = "  [y] yes    [n] no"
  local width = options.min_width or 32
  for _, line in ipairs(body) do width = math.max(width, vim.fn.strdisplaywidth(line) + 4) end
  local buffer, window = popup_window.open({ relative = "editor", width = width, height = #body,
    title = options.title or "Confirm", filetype = "ForgeConfirm" })
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, body)
  vim.bo[buffer].modifiable = false
  local finished = false
  local function finish(accepted)
    if finished then return end
    finished = true
    popup_window.close(window)
    if accepted then on_yes() elseif on_no then on_no() end
  end
  vim.keymap.set("n", "y", function() finish(true) end, { buffer = buffer, nowait = true, silent = true })
  for _, key in ipairs({ "n", "q", "<Esc>" }) do
    vim.keymap.set("n", key, function() finish(false) end, { buffer = buffer, nowait = true, silent = true })
  end
  vim.api.nvim_create_autocmd("BufLeave", { buffer = buffer, once = true, callback = function() finish(false) end })
  return buffer
end

return M
