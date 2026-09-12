vim.loader.enable(false)
local calls = {}
local opened, refreshed, closed = {}, 0, 0
package.loaded["forge.status"] = {
  open_comparison = function(options) calls[#calls + 1] = options end,
  open = function(options)
    local state = { active = true, replica = { buffer = vim.api.nvim_create_buf(false, true) } }
    opened[#opened + 1] = { options = options, state = state }
    return state
  end,
  refresh = function() refreshed = refreshed + 1 end,
  close = function(state) closed = closed + 1 state.active = false end,
}
package.loaded["forge.git.git_backend"] = { git_root_async = function(callback) callback("D:/config") end }
package.loaded["forge.views.branch_diff"] = setmetatable({}, { __index = function() error("legacy branch rendering was loaded") end })
local commands = require("forge.views.commands")
local ok, failure = xpcall(function()
  commands.open_branch_diff(" HEAD~1 ", { cwd = "D:/config", file = "nvim/init.lua" })
  assert(#calls == 1 and calls[1].reference == "HEAD~1" and calls[1].worktree)
  assert(calls[1].path == "nvim/init.lua" and calls[1].workspace == "D:/config")
  assert(calls[1].name == "ForgeBranchDiff" and calls[1].filetype == "ForgeStatus")
  commands.open_branch_diff("HEAD")
  assert(#calls == 2 and calls[2].path == nil and calls[2].workspace == "D:/config")
  local first = commands.open()
  assert(#opened == 1 and opened[1].options.filetype == "ForgeStatus")
  assert(commands.open() == first and #opened == 1 and refreshed == 1)
  first.active = false
  assert(commands.open() ~= first and #opened == 2 and closed == 0)
end, debug.traceback)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("native_command_facade OK")
vim.cmd("qa!")
