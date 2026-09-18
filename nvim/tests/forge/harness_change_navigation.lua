vim.loader.enable(false)
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local state = session.harness
local workspace = vim.fn.tempname()
vim.fn.mkdir(workspace)
local path = vim.fs.joinpath(workspace, "changed [name].rs")
vim.fn.writefile({ "first", "second", "third" }, path)
local original_notify = vim.notify
local notices = {}
vim.notify = function(message) notices[#notices + 1] = message end
local success, failure = xpcall(function()
  state.transcript_buf = vim.api.nvim_get_current_buf()
  state.session = { workspace = workspace }
  local action = { kind = "file", path = "changed [name].rs", line = 3 }
  state.presentation = {
    activate = function(callback) callback(action, {}) end,
    open_output = function() return false end,
  }
  local original_tab = vim.api.nvim_get_current_tabpage()
  controller.open_timeline_entry()
  assert(vim.api.nvim_get_current_tabpage() ~= original_tab, "source navigation replaced the timeline tab")
  assert(vim.fs.normalize(vim.api.nvim_buf_get_name(0)) == vim.fs.normalize(path))
  assert(vim.api.nvim_win_get_cursor(0)[1] == 3, "source navigation lost the patch line coordinate")
  vim.cmd.tabclose()
  assert(vim.api.nvim_get_current_buf() == state.transcript_buf)
  action.path = "missing.rs"
  local count = #vim.api.nvim_list_tabpages()
  controller.open_timeline_entry()
  assert(#vim.api.nvim_list_tabpages() == count, "missing change created a new empty file tab")
  assert(notices[#notices]:find("no longer available", 1, true))
end, debug.traceback)
vim.notify = original_notify
vim.fn.delete(path)
vim.fn.delete(workspace, "d")
assert(success, failure)
print("harness_change_navigation: passed")
