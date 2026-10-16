vim.loader.enable(false)
local commands = require("forge.document_commands")
local gutter = require("forge.gutter")
local editable = require("forge.editable")
local original_normalize, original_bounds = gutter.normalize, gutter.bounds
local original_suspend, original_setreg = editable.suspend_generated_text, vim.fn.setreg
local buffer = vim.api.nvim_create_buf(false, true)
local owner
local captured

local ok, failure = xpcall(function()
  local text = string.rep("complete source ", 1200000)
  assert(#text > 16 * 1024 * 1024)
  vim.api.nvim_buf_set_lines(buffer, 0, -1, true, { text, "last row" })
  vim.api.nvim_win_set_buf(0, buffer)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  gutter.normalize = function() end
  gutter.bounds = function() return nil end
  editable.suspend_generated_text = function() return false end
  vim.fn.setreg = function(register, rows, kind)
    assert(register == "+" and kind == "V")
    captured = rows
  end
  owner = commands.attach_selection({ buffer = buffer, status = "Applied", editable = {} }, { normalize = false })
  owner.start()
  vim.api.nvim_win_set_cursor(0, { 2, 0 })
  local mapping = vim.fn.maparg("<Space>l", "x", false, true)
  assert(type(mapping.callback) == "function")
  mapping.callback()
  assert(captured and #captured == 2 and captured[1] == text and captured[2] == "last row")
end, debug.traceback)

if owner then owner.close() end
gutter.normalize, gutter.bounds = original_normalize, original_bounds
editable.suspend_generated_text, vim.fn.setreg = original_suspend, original_setreg
vim.api.nvim_buf_delete(buffer, { force = true })
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
