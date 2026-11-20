-- Neovim draws windows when it waits for input in Normal or Insert mode, but not while
-- Command-line mode reads a line: a buffer, extmark, or window title changed by a timer or a host
-- event stays undrawn until the user leaves the `:` prompt. A view whose content keeps changing
-- while the user may sit at that prompt, such as the Harness status row with its countdowns,
-- requests a redraw here after each change. Requests made in one main-loop turn share one flush of
-- the pending screen updates, and outside Command-line mode a request does nothing.
local CommandLineRedraw = {}

local flush_pending = false

---@return boolean
local function in_command_line()
  return vim.api.nvim_get_mode().mode:sub(1, 1) == "c"
end

---Draw the pending screen updates soon when the editor is at a command-line prompt.
function CommandLineRedraw.request()
  if flush_pending or not in_command_line() then return end
  flush_pending = true
  vim.schedule(function()
    flush_pending = false
    if in_command_line() then vim.api.nvim__redraw({ flush = true }) end
  end)
end

return CommandLineRedraw
