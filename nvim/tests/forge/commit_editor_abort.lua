vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
package.loaded["forge.integrations.ai_commit"] = { populate_commit_buffer_when_ready = function() end }
local commit = require("forge.integrations.commit")
vim.notify = function() end
local origin = vim.api.nvim_get_current_buf()
local window = vim.api.nvim_get_current_win()
local target = vim.fn.tempname()
vim.fn.writefile({ "original message" }, target)
local completed = 0
local function open_editor()
  commit._active = { win = window, prev_buf = origin, prev_winbar = "", root = vim.fn.getcwd(),
    console = vim.api.nvim_create_buf(false, true), on_done = function() completed = completed + 1 end }
  commit.editor(target, "invalid-forge-test-editor-socket")
  return vim.api.nvim_win_get_buf(window)
end
local editor = open_editor()
vim.api.nvim_buf_set_lines(editor, 0, -1, false, { "modified unsent message" })
assert(vim.bo[editor].modified)
vim.fn.maparg("q", "n", false, true).callback()
commit._finish(1)
assert(vim.api.nvim_win_get_buf(window) == origin, "modified aborted editor did not restore its borrowed window")
assert(not vim.api.nvim_buf_is_valid(editor), "aborted editor retained its modified scratch message")
assert(vim.fn.readfile(target)[1] == "original message", "abort saved the modified message")
assert(completed == 1 and commit._active == nil)
vim.fn.delete(target)
print("modified commit editor abort lifecycle passed")
