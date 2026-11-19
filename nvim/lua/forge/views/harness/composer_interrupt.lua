local keymaps = require("forge.shared.keymaps")

local M = {}

---@class ForgeInterruptedInsertion
---@field window integer Composer window that held Insert mode when the interrupt arrived.
---@field cursor integer[] One-based row and zero-based byte column of the Insert-mode cursor.

---Resume the Insert mode that the built-in `i_CTRL-C` ended, at the position the user was typing.
---The composer must still be the current buffer in Normal mode, so a cancellation that moved focus
---or opened another mode keeps what it selected.
---@param buffer integer
---@param insertion ForgeInterruptedInsertion
local function resume_insert(buffer, insertion)
  local window = insertion.window
  if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_get_current_win() ~= window
    or vim.api.nvim_win_get_buf(window) ~= buffer or vim.api.nvim_get_mode().mode ~= "n" then
    return
  end
  local row, column = insertion.cursor[1], insertion.cursor[2]
  local line = vim.api.nvim_buf_get_lines(buffer, row - 1, row, false)[1]
  if not line then
    vim.cmd("startinsert")
  elseif column >= #line then
    -- Normal mode cannot rest past the last character, so append instead of inserting before it.
    vim.api.nvim_win_set_cursor(window, { row, math.max(0, #line - 1) })
    vim.cmd("startinsert!")
  else
    vim.api.nvim_win_set_cursor(window, { row, column })
    vim.cmd("startinsert")
  end
end

--- Observe an unmapped interrupt that Neovim delivered while it was busy.
---
--- Neovim then bypasses the composer's Insert-mode mapping and runs the built-in `i_CTRL-C`, which
--- leaves Insert mode. The interrupt cancels the turn exactly as the mapping does, so the composer
--- returns to Insert mode at the same cursor once the cancellation has started.
---@param buffer integer
---@param is_current fun(): boolean
---@param cancel fun()
function M.attach(buffer, is_current, cancel)
  local namespace = vim.api.nvim_create_namespace("ForgeComposerInterrupt" .. buffer)
  local interrupt = vim.api.nvim_replace_termcodes("<C-c>", true, false, true)
  local pending = false
  local alive = true
  vim.on_key(function(key, typed)
    if key ~= interrupt or typed ~= interrupt or pending or not alive then return end
    if vim.api.nvim_get_current_buf() ~= buffer or not is_current() then return end
    local mode = vim.api.nvim_get_mode().mode
    if mode ~= "n" and mode ~= "i" then return end
    local configured = false
    for _, binding in ipairs(keymaps.view_keys_for("harness", "cancel")) do
      if vim.api.nvim_replace_termcodes(binding, true, false, true) == interrupt then configured = true break end
    end
    if not configured then return end
    ---@type ForgeInterruptedInsertion?
    local insertion
    if mode == "i" then
      local window = vim.api.nvim_get_current_win()
      insertion = { window = window, cursor = vim.api.nvim_win_get_cursor(window) }
    end
    pending = true
    vim.schedule(function()
      pending = false
      if not (alive and is_current() and vim.api.nvim_buf_is_valid(buffer)) then return end
      cancel()
      if insertion then resume_insert(buffer, insertion) end
    end)
  end, namespace)
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buffer,
    once = true,
    callback = function()
      alive = false
      vim.on_key(nil, namespace)
    end,
  })
end

return M
