vim.opt.runtimepath:prepend("nvim")
local workspace = require("forge.views.harness.workspace")
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(transcript)
local transcript_window = vim.api.nvim_get_current_win()
vim.cmd("split")
vim.api.nvim_set_current_buf(composer)
local state = { transcript_buf = transcript, transcript_win = transcript_window,
  composer_buf = composer, composer_win = vim.api.nvim_get_current_win() }
workspace.attach(state)
if vim.g.test_cancelled_exit then
  vim.api.nvim_exec_autocmds("ExitPre", {})
  vim.wait(10, function() return false end)
  vim.v.errmsg = ""
  vim.cmd("silent! doautocmd BufUnload")
  assert(vim.v.errmsg:find("Harness buffer is locked", 1, true), "cancelled editor exit left Harness unprotected")
end
vim.cmd("qa!")
