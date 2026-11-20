local M = {}
local popup = require("forge.infra.popup_window")
local settle_namespace = vim.api.nvim_create_namespace("ForgePlanDecisionSettle")

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
  -- The popup takes focus as soon as its key is pressed, so keys typed right behind that key, such
  -- as a repeated `oY`, would become comment text or submit it. Keys already waiting in the
  -- typeahead when it opens are read and dropped here. Like the pickers that take focus by
  -- themselves, it then discards every key, except its cancel keys, until the keyboard has been
  -- quiet for the settle interval; each discarded key restarts the interval. A key with no typed
  -- keys, such as one a key-hint plugin feeds back after reading it, is discarded the same way.
  while vim.fn.getchar(1) ~= 0 do vim.fn.getchar(0) end
  local settle_ms = require("forge.views.picker").focus_taking_settle_ms
  vim.uv.update_time()
  local settle_until = vim.uv.now() + settle_ms
  local pass_keys = { [vim.keycode("<Esc>")] = true, [vim.keycode("<C-c>")] = true }
  vim.on_key(function(_, typed)
    if not settle_until or (typed and pass_keys[typed]) then return end
    if vim.api.nvim_get_current_buf() ~= buffer then return end
    vim.uv.update_time()
    local now = vim.uv.now()
    if now >= settle_until then settle_until = nil return end
    settle_until = now + settle_ms
    return ""
  end, settle_namespace)
  local function release_settle() vim.on_key(nil, settle_namespace) end
  local function complete(value)
    if finished then return end
    finished = true
    release_settle()
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
    release_settle()
    if parent_closed then vim.api.nvim_del_autocmd(parent_closed) parent_closed = nil end
    callback(nil)
  end })
  vim.cmd("startinsert!")
  return window
end

return M
