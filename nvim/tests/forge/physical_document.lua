vim.loader.enable(false)
local replica = require("forge.buffer")
local file = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "# Plan", "original" }, file)
vim.cmd.edit(vim.fn.fnameescape(file))
local buffer = vim.api.nvim_get_current_buf()
local original = { name = vim.api.nvim_buf_get_name(buffer), buftype = vim.bo.buftype, filetype = vim.bo.filetype }
local session = replica.open("plan", { buffer = buffer, physical = true })
local snapshot = { document = "plan", revision = 0, block = {
  { id = "plan", text = { "# Plan", "original" }, metadata = { target = {}, editable_region = {}, decoration = {
    { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 6 } }, capture = "Title", priority = 100 },
  } } },
} }
local ok, failure = xpcall(function()
  local tick = vim.api.nvim_buf_get_changedtick(buffer)
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
  assert(vim.api.nvim_buf_get_changedtick(buffer) == tick, "metadata adoption edited file text")
  assert(vim.bo.modifiable and not vim.bo.modified)
  vim.api.nvim_buf_set_lines(buffer, 1, 2, true, { "typed" })
  assert(replica.apply_snapshot(session, snapshot).kind == "Desynchronized")
  assert(vim.api.nvim_buf_get_lines(buffer, 1, 2, true)[1] == "typed", "projection overwrote unsaved file text")
  assert(vim.bo.modifiable and vim.bo.modified)
  vim.cmd.write()
  assert(vim.fn.readfile(file)[2] == "typed", "native write ownership was replaced")
  replica.close(session)
  assert(vim.api.nvim_buf_is_valid(buffer))
  assert(vim.api.nvim_buf_get_name(buffer) == original.name and vim.bo.buftype == original.buftype and vim.bo.filetype == original.filetype)
end, debug.traceback)
vim.fn.delete(file)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
