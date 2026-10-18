local M = {}
local popup = require("forge.infra.popup_window")

---@class ForgePlanDecisionOptions
---@field title string
---@field parent_win integer

---Collect an optional review comment and complete once, restoring focus on submit or cancellation.
---@param options ForgePlanDecisionOptions
---@param callback fun(comment: string?) Nil cancels the decision, while an empty string submits it.
---@return integer window
function M.open(options, callback)
  local buffer, window = popup.open({
    title = options.title,
    width = math.max(1, math.min(80, vim.o.columns - 4)),
    height = math.max(1, math.min(4, vim.o.lines - 6)),
    filetype = "ForgePlanDecision",
    wrap = true,
    linebreak = true,
  })
  vim.wo[window].winbar = "Enter / Ctrl-S submit · Esc cancel"
  local finished = false
  local parent_closed
  local function complete(value)
    if finished then return end
    finished = true
    if parent_closed then vim.api.nvim_del_autocmd(parent_closed) parent_closed = nil end
    popup.close(window)
    callback(value)
  end
  local function submit()
    if finished then return end
    complete(table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n"))
  end
  local function cancel() complete(nil) end
  for _, key in ipairs({ "<CR>", "<C-s>" }) do
    vim.keymap.set({ "n", "i" }, key, submit, { buffer = buffer, desc = "Submit plan review comment" })
  end
  for _, key in ipairs({ "<Esc>", "<C-c>" }) do
    vim.keymap.set({ "n", "i" }, key, cancel, { buffer = buffer, desc = "Cancel plan review decision" })
  end
  parent_closed = vim.api.nvim_create_autocmd("WinClosed", {
    pattern = tostring(options.parent_win), callback = cancel,
  })
  vim.api.nvim_create_autocmd("BufWipeout", { buffer = buffer, once = true, callback = function()
    if finished then return end
    finished = true
    if parent_closed then vim.api.nvim_del_autocmd(parent_closed) parent_closed = nil end
    callback(nil)
  end })
  vim.cmd("startinsert!")
  return window
end

return M
