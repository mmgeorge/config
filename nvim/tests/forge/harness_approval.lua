vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
vim.o.columns = 130
local approval = require("forge.views.harness.approval")
local interrupted, closed = 0, 0
approval.open({ id = "approval", title = "Run command", choice_list = {
  { id = "allow_once", label = "Allow once" },
} }, {
  transcript_win = vim.api.nvim_get_current_win(),
  interrupt = function() interrupted = interrupted + 1 end,
  closed = function() closed = closed + 1 end,
  resolve = function() error("interrupt must not approve or reject the tool") end,
})
assert(approval.is_open())
local mapping = vim.fn.maparg("<C-c>", "n", false, true)
assert(type(mapping.callback) == "function", "approval picker has no interrupt binding")
mapping.callback()
assert(interrupted == 1 and closed == 1 and not approval.is_open())
print("harness_approval: passed")
vim.cmd("qa!")
