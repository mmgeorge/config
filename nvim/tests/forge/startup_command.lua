vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local captured
local loaded = false
package.loaded.lazy = { load = function(options)
  assert(options.plugins[1] == "forge-local")
  loaded = true
end }
package.loaded.forge = { open = function(started_at)
  assert(loaded, "command entered Forge before loading the plugin")
  captured = started_at
end }
local plugin = require("plugins.forge")[1]
assert(not vim.tbl_contains(plugin.cmd, "ForgeStatus"), "lazy command handler would replace the profiler entry")
plugin.init()
local before = vim.uv.hrtime()
vim.cmd.ForgeStatus()
assert(captured and captured >= before and captured <= vim.uv.hrtime())
print("startup command timing passed")
vim.cmd("qa!")
